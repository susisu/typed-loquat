import {
    StringParser,
    string,
} from "..";

type Atom = string | null;
type Cons = { car: SExpr; cdr: SExpr };
type SExpr = Atom | Cons;

// define P<T> as the type of a parser that takes input string and yields T as its result
type P<T> = StringParser<T>;

// make parsers
const p = string();

// parser that skips whitespace characters
const spaces = p.spaces.label("");

// skip trailing whitespace
function lexeme<T>(parser: P<T>): P<T> {
    return parser.skip(spaces);
}

// expr ::= atom | list
const expr: P<SExpr> = p.lazy(() => p.choice([
    atom,
    list,
])).label("expression");

// atom ::= letter atom_tail*
// atom_tail ::= letter | number
const atom = lexeme(p.do<Atom>(function* () {
    const x: string = yield p.letter;
    const xs: string = yield p.alphaNum.manyChars();
    return x + xs;
})).label("atom");

// list ::= "(" expr* ["." expr] ")"
const list = p.do<SExpr>(function* () {
    yield lexeme(p.char("("));
    const xs: SExpr[] = yield expr.many();
    const x: SExpr = yield p.option(null, lexeme(p.char(".")).and(expr));
    yield lexeme(p.char(")"));
    return xs.reduceRight((ys: SExpr, y: SExpr) => ({ car: y, cdr: ys }), x);
}).label("list");

const parser: P<SExpr> = spaces.and(expr).skip(p.eof);

export function parseSExpr(src: string): SExpr {
    const res = parser.parse("", src);
    if (res.success) {
        return res.value;
    }
    else {
        throw new SyntaxError(res.error.toString());
    }
}
