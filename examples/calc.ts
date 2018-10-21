import {
    AbstractParser,
    Operator,
    OperatorAssoc,
    string as p,
} from "../index";

// P<T> is the type of a parser that yields T as its result
type P<T> = AbstractParser<T, string>;

// parser that skips whitespace characters
const spaces = p.spaces().label("");

// skips trailing spaces
function lexeme<T>(parser: P<T>): P<T> {
    return parser.skip(spaces);
}

function symbol(str: string): P<string> {
    return lexeme(p.string(str).try());
}

// <number> (not negative)
const numberRegExp = /(0|[1-9]\d*)(\.\d*)?([Ee][+\-]?\d*)?/;
const number = lexeme(p.regexp(numberRegExp)).map(Number).label("number");

// <term> ::= <number> | "(" <expr> ")"
const lparen = symbol("(");
const rparen = symbol(")");
const term: P<number> = p.lazy(() => number.or(expr.between(lparen, rparen)));

// <expr1> ::= "+" <term> | "-" <term> | <term>
// <expr2> ::= <expr1> ** <expr2> | <expr1>
// <expr3> ::= <expr3> "*" <expr2> | <expr3> "/" <expr2> | <expr2>
// <expr>  ::= <expr> "+" <expr3> | <expr> "-" <expr3> | <expr3>
const plus  = symbol("+").return((x: number) => x);
const minus = symbol("-").return((x: number) => -x);
const pow   = symbol("**").return((x: number, y: number) => Math.pow(x, y));
const mul   = symbol("*").return((x: number, y: number) => x * y);
const div   = symbol("/").return((x: number, y: number) => x / y);
const add   = symbol("+").return((x: number, y: number) => x + y);
const sub   = symbol("-").return((x: number, y: number) => x - y);
const expr = p.buildExpressionParser(
    [
        [
            Operator.prefix(plus),
            Operator.prefix(minus),
        ],
        [
            Operator.infix(pow, OperatorAssoc.RIGHT),
        ],
        [
            Operator.infix(mul, OperatorAssoc.LEFT),
            Operator.infix(div, OperatorAssoc.LEFT),
        ],
        [
            Operator.infix(add, OperatorAssoc.LEFT),
            Operator.infix(sub, OperatorAssoc.LEFT),
        ],
    ],
    term
);

const calc: P<number> = spaces.and(expr).left(p.eof());

export function calcExpr(src: string): number {
    const res = calc.parse("", src);
    if (res.success) {
        return res.value;
    }
    else {
        throw new SyntaxError(res.error.toString());
    }
}
