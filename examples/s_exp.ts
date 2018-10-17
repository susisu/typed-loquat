import * as lq from "../index";

type Atom = string | null;
type Cons = { car: SExp, cdr: SExp };
type SExp = Atom | Cons;

// P<T> is the type of a parser that yields T as its result
type P<T> = lq.AbstractParser<T>;

// parser that skips whitespace characters
const spaces = lq.spaces().label("");

// skip trailing whitespace
function lexeme<T>(parser: P<T>): P<T> {
    return parser.skip(spaces);
}

// expr ::= atom | list
const expr: P<SExp> = lq.lazy(() => lq.choice<SExp>([
    atom,
    list,
])).label("expression");

// atom ::= letter atom_tail*
// atom_tail ::= letter | number
const atom = lexeme(lq._do<Atom>(function* () {
    const x  = yield lq.letter();
    const xs = yield lq.alphaNum().manyChars();
    return x + xs;
})).label("atom");

// list ::= "(" expr* ["." expr] ")"
const list = lq._do<SExp>(function* () {
    yield lexeme(lq.char("("));
    const xs: SExp[] = yield expr.many();
    const x: SExp  = yield lq.option(null, lexeme(lq.char(".")).and(expr));
    yield lexeme(lq.char(")"));
    return xs.reduceRight((ys: SExp, y: SExp) => ({ car: y, cdr: ys }), x);
}).label("list");

const parser: P<SExp> = spaces.and(expr).skip(lq.eof());

export function parseSExpr(src: string): SExp {
    const res = parser.parse("", src);
    if (res.success) {
        return res.value;
    }
    else {
        throw new SyntaxError(res.error.toString());
    }
}
