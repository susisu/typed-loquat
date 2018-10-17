import * as lq from "../index";

type Term = Var | App | Abs;

// variable
class Var {
    constructor(readonly name: string) {}

    toString(): string {
        return this.name;
    }
}

// application
class App {
    constructor(readonly fun: Term, readonly arg: Term) {}

    toString(): string {
        return `(${this.fun.toString()} ${this.arg.toString()})`;
    }
}

// abstraction
class Abs {
    constructor(readonly paramName: string, readonly body: Term) {}

    toString(): string {
        return `(fun ${this.paramName} -> ${this.body.toString()})`;
    }
}

// P<T> is the type of a parser that yields T as its result
type P<T> = lq.AbstractParser<T>;

// generate a token parser from the language definition
const languageDef = new lq.LanguageDef({
    commentStart  : "(*",
    commentEnd    : "*)",
    commentLine   : "",
    nestedComments: true,
    idStart       : lq.letter(),
    idLetter      : lq.alphaNum().or(lq.char("_")),
    opStart       : lq.oneOf("->"),
    opLetter      : lq.oneOf("->"),
    reservedIds   : ["fun"],
    reservedOps   : ["->"],
    caseSensitive : false,
});
const tp = lq.makeTokenParser(languageDef);

// term ::= app | abs
const term: P<Term> = lq.lazy(() =>
    app.or(abs)
);

// var ::= <identifier>
const _var = tp.identifier.map(name =>
    new Var(name)
);

// aterm ::= var | "(" term ")"
const aterm = _var.or(tp.parens(term));

// app ::= aterm aterm*
const app = aterm.bind(term =>
    aterm.many().map(terms =>
        terms.reduce((fun, arg) => new App(fun, arg), term)
    )
);

// abs ::= "fun" <identifier> "->" term
const fun = tp.reserved("fun");
const arr = tp.reservedOp("->");
const abs = fun.and(tp.identifier).bind(paramName =>
    arr.and(term).map(body =>
        new Abs(paramName, body)
    )
);

const parser: P<Term> = tp.whiteSpace.and(term).skip(lq.eof());

export function parseLambda(src: string): Term {
    const res = parser.parse("", src);
    if (res.success) {
        return res.value;
    }
    else {
        throw new SyntaxError(res.error.toString());
    }
}
