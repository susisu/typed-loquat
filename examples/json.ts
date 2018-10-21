import {
    AbstractParser,
    string as p,
} from "../index";

// P<T> is the type of a parser that yields T as its result
type P<T> = AbstractParser<T, string>;

// parser that skips whitespace characters
const spaces = p.spaces().label("");

// skip trailing whitespace
function lexeme<T>(parser: P<T>): P<T> {
    return parser.skip(spaces);
}

// all JSON values
const value: P<{} | null> = p.lazy(() => p.choice([
    object as P<{}>,
    array,
    stringLiteral,
    numberLiteral,
    trueLiteral,
    falseLiteral,
    nullLiteral,
]));

// string literal
const stringRegExp = /"((\\(u[0-9A-Fa-f]{4}|["\\\/bfnrt])|[^\\"\b\f\n\r\t])*?)"/;
const escapeMap = new Map([
    ["b", "\b"],
    ["f", "\f"],
    ["n", "\n"],
    ["r", "\r"],
    ["t", "\t"],
]);
function escape(str: string) {
    return str.replace(/\\(u[0-9A-Fa-f]{4}|[^u])/g, (_, e) => {
        const type = e[0];
        if (type === "u") {
            return String.fromCharCode(parseInt(e.substr(1), 16));
        }
        else if (escapeMap.has(type)) {
            return escapeMap.get(type);
        }
        else {
            return type;
        }
    });
}
const stringLiteral = lexeme(p.regexp(stringRegExp, 1))
    .map(escape)
    .label("string");

// number literal
const numberRegExp = /\-?(0|[1-9]\d*)(\.\d*)?([Ee][+\-]?\d*)?/;
const numberLiteral = lexeme(p.regexp(numberRegExp))
    .map(Number)
    .label("number");

// boolean literals
const trueLiteral = lexeme(p.string("true"))
    .return(true)
    .label("true");

const falseLiteral = lexeme(p.string("false"))
    .return(false)
    .label("false");

// null literal
const nullLiteral = lexeme(p.string("null"))
    .return(null)
    .label("null");

// object and array
const lbrace   = lexeme(p.char("{"));
const rbrace   = lexeme(p.char("}"));
const lbracket = lexeme(p.char("["));
const rbracket = lexeme(p.char("]"));
const colon    = lexeme(p.char(":"));
const comma    = lexeme(p.char(","));

const keyValue = stringLiteral.bind(key =>
    colon.and(value).bind(val =>
        p.pure<[string, {} | null]>([key, val])
    )
);
const object = keyValue
    .sepBy(comma)
    .between(lbrace, rbrace)
    .map(kvs => {
        const obj: { [k: string]: {} | null } = {};
        for (const kv of kvs) {
            obj[kv[0]] = kv[1];
        }
        return obj;
    })
    .label("object");

const array = value
    .sepBy(comma)
    .between(lbracket, rbracket)
    .label("array");

const json: P<{} | null> = spaces
    .and(value)
    .skip(p.eof());

export function parseJson(src: string): {} | null {
    const res = json.parse("", src);
    if (res.success) {
        return res.value;
    }
    else {
        throw new SyntaxError(res.error.toString());
    }
}
