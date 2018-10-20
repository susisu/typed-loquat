import * as lq from "../index";

// P<T> is the type of a parser that yields T as its result
type P<T> = lq.AbstractParser<T>;

// parser that skips whitespace characters
const spaces = lq.spaces().label("");

// skips trailing spaces
function lexeme<T>(parser: P<T>): P<T> {
    return parser.skip(spaces);
}

function symbol(str: string): P<string> {
    return lexeme(lq.string(str).try());
}

// <number> (not negative)
const numberRegExp = /(0|[1-9]\d*)(\.\d*)?([Ee][+\-]?\d*)?/;
const number = lexeme(lq.regexp(numberRegExp)).map(Number).label("number");

// <term> ::= <number> | "(" <expr> ")"
const lparen = symbol("(");
const rparen = symbol(")");
const term: P<number> = lq.lazy(() => number.or(expr.between(lparen, rparen)));

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
const expr = lq.buildExpressionParser(
    [
        [
            lq.Operator.prefix(plus),
            lq.Operator.prefix(minus),
        ],
        [
            lq.Operator.infix(pow, lq.OperatorAssoc.RIGHT),
        ],
        [
            lq.Operator.infix(mul, lq.OperatorAssoc.LEFT),
            lq.Operator.infix(div, lq.OperatorAssoc.LEFT),
        ],
        [
            lq.Operator.infix(add, lq.OperatorAssoc.LEFT),
            lq.Operator.infix(sub, lq.OperatorAssoc.LEFT),
        ],
    ],
    term
);

const calc: P<number> = spaces.and(expr).left(lq.eof());

export function calcExpr(src: string): number {
    const res = calc.parse("", src);
    if (res.success) {
        return res.value;
    }
    else {
        throw new SyntaxError(res.error.toString());
    }
}
