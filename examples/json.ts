import * as lq from "../index";

// P<T> is the type of a parser that yields T as its result
type P<T> = lq.AbstractParser<T>;

// parser that skips whitespace characters
const spaces = lq.spaces().label("");

// skip trailing whitespace
function lexeme<T>(parser: P<T>): P<T> {
    return parser.skip(spaces);
}

// all JSON values
const value: P<{} | null> = lq.lazy(() => lq.choice<{} | null>([
    object,
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
const stringLiteral = lexeme(lq.regexp(stringRegExp, 1))
    .map(escape)
    .label("string");

// number literal
const numberRegExp = /\-?(0|[1-9]\d*)(\.\d*)?([Ee][+\-]?\d*)?/;
const numberLiteral = lexeme(lq.regexp(numberRegExp))
    .map(Number)
    .label("number");

// boolean literals
const trueLiteral = lexeme(lq.string("true"))
    .return(true)
    .label("true");

const falseLiteral = lexeme(lq.string("false"))
    .return(false)
    .label("false");

// null literal
const nullLiteral = lexeme(lq.string("null"))
    .return(null)
    .label("null");

// object and array
const lbrace   = lexeme(lq.char("{"));
const rbrace   = lexeme(lq.char("}"));
const lbracket = lexeme(lq.char("["));
const rbracket = lexeme(lq.char("]"));
const colon    = lexeme(lq.char(":"));
const comma    = lexeme(lq.char(","));

const keyValue = stringLiteral.bind(key =>
    colon.and(value).bind(val =>
        lq.pure<[string, {} | null]>([key, val])
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
    .skip(lq.eof());

export function parseJson(src: string): {} | null {
    const res = json.parse("", src);
    if (res.success) {
        return res.value;
    }
    else {
        throw new SyntaxError(res.error.toString());
    }
}
