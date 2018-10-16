import * as lq from '../index';

// P<T> is the type of a parser that yields T as its result
type P<T> = lq.AbstractParser<T>;

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
            new lq.PrefixOperator(plus),
            new lq.PrefixOperator(minus)
        ],
        [
            new lq.InfixOperator(pow, lq.OperatorAssoc.RIGHT)
        ],
        [
            new lq.InfixOperator(mul, lq.OperatorAssoc.LEFT),
            new lq.InfixOperator(div, lq.OperatorAssoc.LEFT)
        ],
        [
            new lq.InfixOperator(add, lq.OperatorAssoc.LEFT),
            new lq.InfixOperator(sub, lq.OperatorAssoc.LEFT)
        ]
    ],
    term
);

const calc: P<number> = spaces.and(expr).left(lq.eof());

export function calcExpr(src: string) {
    const res = calc.parse("", src);
    if (res.success) {
        return res.value;
    }
    else {
        throw new SyntaxError(res.error.toString());
    }
}
