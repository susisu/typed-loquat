// # common types
/**
 * `Maybe<A>` represents a value that can be empty or of type `A`.
 */
export type Maybe<A> = { empty: true } | { empty: false; value: A };
/**
 * AssocValueOf<T> computes the union type of the values associated to `T`.
 */
type AssocValueOf<T> = T[keyof T];

/**
 * `Unconsed<T, S>` represents a result of `uncons` that can be empty or a pair of head `T` and tail
 * `S`.
 */
export type Unconsed<T, S> = { empty: true } | { empty: false; head: T; tail: S };

// # from "core"
// ## from "core/utils"
/**
 * Pretty-prints a value to display.
 */
export declare function show<T>(value: T): string;
/**
 * Unconses a string. When `unicode` is `true`, head will be the initial code point of the string;
 * otherwise head will be the initial code unit.
 */
export declare function unconsString(str: string, unicode: boolean): Unconsed<string, string>;

// ## from "core/stream"
export declare function uncons(input: string, unicode: boolean): Unconsed<string, string>;
export declare function uncons<T>(input: T[], unicode: boolean): Unconsed<T, T[]>;
export declare function uncons<T, S extends { uncons(): Unconsed<T, S> }>(
    input: S,
    unicode: boolean
): Unconsed<T, S>;
export declare class ArrayStream<T> {
    public readonly arr: T[];
    public readonly index: number;
    constructor(arr: T[], index: number);
    public uncons(): Unconsed<T, ArrayStream<T>>;
}

// ## from "core/pos"
/**
 * `SourcePos` represents a position in source.
 */
export class SourcePos {
    /**
     * Creates an initial position i.e. line 1 and column 1.
     */
    public static init(name: string): SourcePos;
    /**
     * Tests if two positions are equal.
     */
    public static equal(posA: SourcePos, posB: SourcePos): boolean;
    /**
     * Compares two positions.
     */
    public static compare(posA: SourcePos, posB: SourcePos): -1 | 0 | 1;
    /**
     * Name of the source.
     */
    public readonly name: string;
    /**
     * Line number.
     */
    public readonly line: number;
    /**
     * Column number.
     */
    public readonly column: number;
    /**
     * Creates a new `SourcePos` instance.
     */
    constructor(name: string, line: number, column: number);
    /**
     * Pretty-prints the position.
     */
    public toString(): string;
    /**
     * Creates a copy with `name` updated.
     */
    public setName(name: string): SourcePos;
    /**
     * Creates a copy with `line` updated.
     */
    public setLine(line: number): SourcePos;
    /**
     * Creates a copy with `column` updated.
     */
    public setColumn(column: number): SourcePos;
    /**
     * Computes a new position advanced by the given character.
     */
    public addChar(char: string, tabWidth: number): SourcePos;
    /**
     * Computes a new position advanced by the given string.
     */
    public addString(str: string, tabWidth: number, unicode: boolean): SourcePos;
}

// ## from "core/error"
/**
 * `ErrorMessageType` represents types of error messages.
 */
export type ErrorMessageType = AssocValueOf<typeof ErrorMessageType>;
export declare const ErrorMessageType: Readonly<{
    SYSTEM_UNEXPECT: "systemUnexpect";
    UNEXPECT: "unexpect";
    EXPECT: "expect";
    MESSAGE: "message";
}>;
/**
 * `ErrorMessage` represents a single error message in parse error.
 */
export class ErrorMessage {
    /**
     * Tests if two error messages are equal.
     */
    public static equal(msgA: ErrorMessage, msgB: ErrorMessage): boolean;
    /**
     * Pretty-prints multiple messages.
     */
    public static messagesToString(msgs: ErrorMessage[]): string;
    /**
     * Tests if two message arrays are equal.
     */
    public static messagesEqual(msgsA: ErrorMessage[], msgsB: ErrorMessage[]): boolean;
    /**
     * Type of the message.
     */
    public readonly type: ErrorMessageType;
    /**
     * Content of the message.
     */
    public readonly msgStr: string;
    /**
     * Creates a new `ErrorMessage` instance.
     */
    constructor(type: ErrorMessageType, msgStr: string);
}
/**
 * `AbstractParseError` represents a parse error raised at some position in source.
 */
export abstract class AbstractParseError {
    /**
     * Position where the parse error is raised.
     */
    public readonly pos: SourcePos;
    /**
     * Error messages of the parse error.
     */
    public readonly msgs: ErrorMessage[];
    /**
     * Pretty-prints the error.
     */
    public toString(): string;
    /**
     * Tests if the error is unknown i.e. it contains no messages.
     */
    public isUnknown(): boolean;
    /**
     * Creates a new error object with `pos` updated.
     */
    public setPosition(pos: SourcePos): AbstractParseError;
    /**
     * Creates a new error object with `msgs` updated.
     */
    public setMessages(msgs: ErrorMessage[]): AbstractParseError;
    /**
     * Creates a new error object with the given messages added to the original message.
     */
    public addMessages(msgs: ErrorMessage[]): AbstractParseError;
    /**
     * Creates a new error object with all messages of the given type updated.
     */
    public setSpecificTypeMessages(type: ErrorMessageType, msgStrs: string[]): AbstractParseError;
}
/**
 * `ParseError` represents a standard parse error.
 */
export class ParseError extends AbstractParseError {
    /**
     * Creates an unknown parse error with no messages.
     */
    public static unknown(pos: SourcePos): ParseError;
    /**
     * Tests if two errors are equal.
     */
    public static equal(errA: AbstractParseError, errB: AbstractParseError): boolean;
    /**
     * Merges two errors.
     */
    public static merge(errA: AbstractParseError, errB: AbstractParseError): AbstractParseError;
    /**
     * Creates a new `ParseError` instance.
     */
    constructor(pos: SourcePos, msgs: ErrorMessage[]);
}
/**
 * `LazyParseError` is a lazy evaluated version of `ParseError`.
 */
export class LazyParseError extends AbstractParseError {
    /**
     * Creates a new `LazyParseError` instance.
     */
    constructor(thunk: () => AbstractParseError);
    /**
     * Forces evaluation.
     */
    public eval(): ParseError;
}

// ## from "core/parser"
/**
 * Argument to the `Config` constructor.
 */
export type ConfigOptions = { tabWidth?: number; unicode?: boolean };
/**
 * `Config` contains configuration information of parser.
 */
export class Config {
    /**
     * Tests if two configurations are equal.
     */
    public static equal(configA: Config, configB: Config): boolean;
    /**
     * Creates a new `Config` instance.
     */
    constructor(opts: ConfigOptions);
    /**
     * Width of a tab character (default = `8`).
     */
    public readonly tabWidth: number;
    /**
     * Whether various unicode features should be enabled (default = `false`).
     */
    public readonly unicode: boolean;
}
/**
 * `State<S, U>` represents state of parsers of stream type `S` and user defined state type `U`.
 */
export class State<S, U = undefined> {
    /**
     * Tests if two states are equal.
     */
    public static equal<S, U>(
        stateA: State<S, U>,
        stateB: State<S, U>,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    /**
     * Current configuration.
     */
    public readonly config: Config;
    /**
     * Current input.
     */
    public readonly input: S;
    /**
     * Current position.
     */
    public readonly pos: SourcePos;
    /**
     * Current user defined state.
     */
    public readonly userState: U;
    /**
     * Creates a new `State` instance.
     */
    constructor(config: Config, input: S, pos: SourcePos, userState: U);
    /**
     * Creates a copy of the state with `config` updated.
     */
    public setConfig(config: Config): State<S, U>;
    /**
     * Creates a copy of the state with `input` updated.
     */
    public setInput<S2>(input: S2): State<S2, U>;
    /**
     * Creates a copy of the state with `pos` updated.
     */
    public setPosition(pos: SourcePos): State<S, U>;
    /**
     * Creates a copy of the state with `userState` updated.
     */
    public setUserState<U2>(userState: U2): State<S, U2>;
}
/**
 * `Result<A, S, U>` represents a result of a parser of stream type `S` and user defined state
 * type `U`. It can be failure, or success with a value of type `A`.
 */
export type Result<A, S, U = undefined> = Failure | Success<A, S, U>;
export declare const Result: {
    /**
     * Tests if two results are equal.
     */
    equal<A, S, U>(
        resA: Result<A, S, U>,
        resB: Result<A, S, U>,
        valEqual: (valA: A, valB: A) => boolean,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    /**
     * Creates a successful result marked as the parser consumed some tokens from input.
     */
    csuc<A, S, U>(
        err: AbstractParseError,
        val: A,
        state: State<S, U>
    ): Success<A, S, U>;
    /**
     * Creates a unsuccessful result marked as the parser consumed some tokens from input.
     */
    cerr(err: AbstractParseError): Failure;
    /**
     * Creates a successful result marked as the parser did not consumed any tokens from input.
     */
    esuc<A, S, U>(
        err: AbstractParseError,
        val: A,
        state: State<S, U>
    ): Success<A, S, U>;
    /**
     * Creates a unsuccessful result marked as the parser did not consumed any tokens from
     * input.
     */
    eerr(err: AbstractParseError): Failure;
};
/**
 * `Failure` represents a unsuccessful result.
 */
export type Failure = {
    /**
     * Whether the parser consumed some tokens from input or not.
     */
    readonly consumed: boolean;
    /**
     * `false`.
     */
    readonly success: false;
    /**
     * Parse error that will explain why the parser failed.
     */
    readonly err: AbstractParseError;
};
/**
 * `Success` represents a successful result.
 */
export type Success<A, S, U> = {
    /**
     * Whether the parser consumed some tokens from input or not.
     */
    readonly consumed: boolean;
    /**
     * `true`.
     */
    readonly success: true;
    /**
     * Parse error. This may contribute to making better error messages raised from succeeding
     * parsers.
     */
    readonly err: AbstractParseError;
    /**
     * Resultant value.
     */
    readonly val: A;
    /**
     * Fianl state of the parser.
     */
    readonly state: State<S, U>;
};
/**
 * `AbstractParser<A, S, U>` represents a parser of stream type `S` and user defined state type `U`
 * that yield a value of type `A` when it successes.
 */
export abstract class AbstractParser<A, S, U = undefined> {
    // # from "core"
    /**
     * Runs a parser with the initial state and returns a result. This is a primitive operation
     * and users may use `parse` insted.
     */
    public run(state: State<S, U>): Result<A, S, U>;
    /**
     * Parses the given input and returns a result.
     */
    public parse: MethodParse<A, S, U>;

    // # from "prim"
    public map<B>(func: (val: A) => B): AbstractParser<B, S, U>;
    public return<B>(val: B): AbstractParser<B, S, U>;
    public ap: MethodAp<A, S, U>;
    public left<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;
    public skip<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;
    public right<B>(parser: AbstractParser<B, S, U>): AbstractParser<B, S, U>;
    public bind<B>(func: (val: A) => AbstractParser<B, S, U>): AbstractParser<B, S, U>;
    public and<B>(parser: AbstractParser<B, S, U>): AbstractParser<B, S, U>;
    public fail(msgStr: string): AbstractParser<never, S, U>;
    public done(): AbstractParser<TailRecDone<A>, S, U>;
    public cont(): AbstractParser<TailRecCont<A>, S, U>;
    public or<B>(parser: AbstractParser<B, S, U>): AbstractParser<A | B, S, U>;
    public label(labelStr: string): AbstractParser<A, S, U>;
    public hidden(): AbstractParser<A, S, U>;
    public try(): AbstractParser<A, S, U>;
    public lookAhead(): AbstractParser<A, S, U>;
    public reduceMany<B>(callback: (accum: B, val: A) => B, initVal: B): AbstractParser<B, S, U>;
    public many(): AbstractParser<A[], S, U>;
    public skipMany(): AbstractParser<undefined, S, U>;
    public skipMany<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;

    // # from "char"
    public manyChars: MethodManyChar<A, S, U>;
    public manyChars1: MethodManyChar<A, S, U>;

    // # from "combinators"
    public option<B>(val: B): AbstractParser<A | B, S, U>;
    public optionMaybe(): AbstractParser<Maybe<A>, S, U>;
    public optional(): AbstractParser<undefined, S, U>;
    public between<B, C>(
        open: AbstractParser<B, S, U>,
        close: AbstractParser<C, S, U>
    ): AbstractParser<A, S, U>;
    public many1(): AbstractParser<A[], S, U>;
    public skipMany1(): AbstractParser<undefined, S, U>;
    public skipMany1<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;
    public sepBy<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    public sepBy1<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    public sepEndBy<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    public sepEndBy1<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    public endBy<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    public endBy1<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    public count(num: number): AbstractParser<A[], S, U>;
    public notFollowedBy(): AbstractParser<undefined, S, U>;
    public notFollowedBy<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;
    public reduceManyTill<B, C>(
        end: AbstractParser<B, S, U>,
        callback: (accum: C, val: A) => C,
        initVal: C
    ): AbstractParser<C, S, U>;
    public manyTill<B>(end: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    public skipManyTill<B>(end: AbstractParser<B, S, U>): AbstractParser<undefined, S, U>;
    public skipManyTill<B, C>(
        parser: AbstractParser<B, S, U>,
        end: AbstractParser<C, S, U>
    ): AbstractParser<A, S, U>;

    // # from "monad"
    public forever(): AbstractParser<never, S, U>;
    public discard(): AbstractParser<undefined, S, U>;
    public void(): AbstractParser<undefined, S, U>;
    public join: MethodJoin<A, S, U>;
    public when(cond: boolean): AbstractParser<A, S, U>;
    public unless(cond: boolean): AbstractParser<A, S, U>;
    public filter(test: (val: A) => boolean): AbstractParser<A, S, U>;
}
/**
 * `ParseResult<A>` represents a result that can be failure with a parse error, or success with a
 * value of type `A`.
 */
export type ParseResult<A> =
      { success: false; error: AbstractParseError }
    | { success: true; value: A };
type MethodParse<A, S, U> = U extends undefined
    ? (name: string, input: S, userState?: U, opts?: ConfigOptions) => ParseResult<A>
    : (name: string, input: S, userState: U, opts?: ConfigOptions) => ParseResult<A>;
type MethodAp<A, S, U> = A extends (val: infer B) => infer C
    ? (parser: AbstractParser<B, S, U>) => AbstractParser<C, S, U>
    : unknown;
type MethodManyChar<A, S, U> = A extends string
    ? () => AbstractParser<string, S, U>
    : unknown;
type MethodJoin<A, S, U> = A extends AbstractParser <infer B, S, U>
    ? () => AbstractParser<B, S, U>
    : unknown;
/**
 * `Parser<A, S, U>` represents a standard parser. A parser can be considered a function that takes
 * the initial state and returns the final state and a resultant value when successful.
 */
export class Parser<A, S, U = undefined> extends AbstractParser<A, S, U> {
    constructor(func: (state: State<S, U>) => Result<A, S, U>);
}
/**
 * `LazyParser<A, S, U>` is a lazy evaluated version of `Parser<A, S, U>`.
 */
export class LazyParser<A, S, U = undefined> extends AbstractParser<A, S, U> {
    constructor(thunk: () => AbstractParser<A, S, U>);
    /**
     * Forces evaluation.
     */
    public eval(): Parser<A, S, U>;
}
export declare function parse<A, S>(
    parser: AbstractParser<A, S, undefined>,
    name: string,
    input: S,
    userState?: undefined,
    opts?: ConfigOptions
): ParseResult<A>;
export declare function parse<A, S, U>(
    parser: AbstractParser<A, S, U>,
    name: string,
    input: S,
    userState: U,
    opts?: ConfigOptions
): ParseResult<A>;
/**
 * Tests if a value is a parser.
 */
export declare function isParserr<T>(val: T): boolean;
/**
 * Asserts that a value is a parser.
 */
export declare function assertParser<T>(val: T): undefined;

// # from "prim"
/**
 * `TailRec<A, B>` is used in return values of the `tailRecM` function and represents a parsing
 * should be continued with an accumulated value of type `A` or done with a final value of type `B`.
 */
export type TailRec<A, B> = TailRecCont<A> | TailRecDone<B>;
/**
 * `TailRecCont<A>` represents a parsing should be continued.
 */
export type TailRecCont<A> = { done: false; value: A };
/**
 * `TailRecCont<A>` represents a parsing should be done.
 */
export type TailRecDone<A> = { done: true; value: A };

// # from "expr"
/**
 * `OperatorType` represents type of operators.
 */
export type OperatorType = AssocValueOf<typeof OperatorType>;
export declare const OperatorType: Readonly<{
    INFIX: "infix";
    PREFIX: "prefix";
    POSTFIX: "postfix";
}>;
/**
 * `OperatorAssoc` represents associativity of infix operators.
 */
export type OperatorAssoc = AssocValueOf<typeof OperatorAssoc>;
export declare const OperatorAssoc: Readonly<{
    NONE: "none";
    LEFT: "left";
    RIGHT: "right";
}>;
/**
 * `Operator<A, S, U>` represents an operator that operates on `A`.
 */
export type Operator<A, S, U = undefined> =
      InfixOperator<A, S, U>
    | PrefixOperator<A, S, U>
    | PostfixOperator<A, S, U>;
export declare const Operator: {
    /**
     * Creates an infix operator.
     */
    infix<A, S, U>(
        parser: AbstractParser<(valA: A, valB: A) => A, S, U>,
        assoc: OperatorAssoc
    ): InfixOperator<A, S, U>;
    /**
     * Creates a prefix operator.
     */
    prefix<A, S, U>(parser: AbstractParser<(val: A) => A, S, U>): PrefixOperator<A, S, U>;
    /**
     * Creates a postfix operator.
     */
    postfix<A, S, U>(parser: AbstractParser<(val: A) => A, S, U>): PostfixOperator<A, S, U>;
};
/**
 * `InfixOperator<A, S, U>` represents an infix operator that operates on `A`.
 */
export type InfixOperator<A, S, U = undefined> = {
    /**
     * `"infix"`.
     */
    readonly type: "infix";
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<(valA: A, valB: A) => A, S, U>;
    /**
     * Associativity of the operator.
     */
    readonly assoc: OperatorAssoc;
};
/**
 * `PrefixOperator<A, S, U>` represents a prefix operator that operates on `A`.
 */
export type PrefixOperator<A, S, U = undefined> = {
    /**
     * `"prefix"`.
     */
    readonly type: "prefix";
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<(val: A) => A, S, U>;
};
/**
 * `PostfixOperator<A, S, U>` represents a postfix operator that operates on `A`.
 */
export type PostfixOperator<A, S, U = undefined> = {
    /**
     * `"postfix"`.
     */
    readonly type: "postfix";
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<(val: A) => A, S, U>;
};

// # from "token"
/**
 * Argument to the `LanguageDef` constructor.
 */
export type LanguageDefObj<S, U = undefined> = {
    commentStart?: string;
    commentEnd?: string;
    commentLine?: string;
    nestedComments?: boolean;
    idStart: AbstractParser<string, S, U>;
    idLetter: AbstractParser<string, S, U>;
    opStart: AbstractParser<string, S, U>;
    opLetter: AbstractParser<string, S, U>;
    reservedIds?: string[];
    reservedOps?: string[];
    caseSensitive?: boolean;
};
/**
 * `LanguageDef<S, U>` defines a language.
 */
export class LanguageDef<S, U = undefined> {
    constructor(obj: LanguageDefObj<S, U>);
    /**
     * A string that marks the start of a multiline comment. The language has no multiline comments
     * when it is an empty string or undefined.
     */
    public commentStart: string | undefined;
    /**
     * A string that marks the end of a multiline comment. The language has no multiline comments
     * when it is an empty string or undefined.
     */
    public commentEnd: string | undefined;
    /**
     * A string that marks the start of a line comment. The language has no line comments when it is
     * an empty string or undefined.
     */
    public commentLine: string | undefined;
    /**
     * Whether nested multiline comments are allowed or not (default = `true`).
     */
    public nestedComments: boolean;
    /**
     * A parser that parses a start letter of identifiers.
     */
    public idStart: AbstractParser<string, S, U>;
    /**
     * A parser that parses a non-start letter of identifiers.
     */
    public idLetter: AbstractParser<string, S, U>;
    /**
     * A parser that parses a start letter of operators.
     */
    public opStart: AbstractParser<string, S, U>;
    /**
     * A parser that parses a non-start letter of operators.
     */
    public opLetter: AbstractParser<string, S, U>;
    /**
     * An array of reserved words.
     */
    public reservedIds: string[];
    /**
     * An array of reserved operators.
     */
    public reservedOps: string[];
    /**
     * Whether the reserved words are case-sensitive or not (default = `true`).
     */
    public caseSensitive: boolean;
    /**
     * Creates a copy of the language definition.
     */
    public clone(): LanguageDef<S, U>;
}
/**
 * `TokenParser<S, U>` represents a set of token parsers.
 */
export type TokenParser<S, U> = {
    whiteSpace: AbstractParser<undefined, S, U>;
    lexeme: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>;
    symbol: (name: string) => AbstractParser<string, S, U>;
    parens: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>;
    braces: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>;
    angles: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>;
    brackets: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>;
    semi: AbstractParser<string, S, U>;
    comma: AbstractParser<string, S, U>;
    colon: AbstractParser<string, S, U>;
    dot: AbstractParser<string, S, U>;
    semiSep: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A[], S, U>;
    semiSep1: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A[], S, U>;
    commaSep: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A[], S, U>;
    commaSep1: <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A[], S, U>;
    decimal: AbstractParser<number, S, U>;
    hexadecimal: AbstractParser<number, S, U>;
    octal: AbstractParser<number, S, U>;
    natural: AbstractParser<number, S, U>;
    integer: AbstractParser<number, S, U>;
    float: AbstractParser<number, S, U>;
    naturalOrFloat: AbstractParser<NaturalOrFloat, S, U>;
    charLiteral: AbstractParser<string, S, U>;
    stringLiteral: AbstractParser<string, S, U>;
    identifier: AbstractParser<string, S, U>;
    reserved: (name: string) => AbstractParser<undefined, S, U>;
    operator: AbstractParser<string, S, U>;
    reservedOp: (name: string) => AbstractParser<undefined, S, U>;
};
/**
 * `NaturalOrFloat` represents a natural (integer) value or a float value.
 */
export type NaturalOrFloat =
      { type: "natural"; value: number }
    | { type: "float"; value: number };

// # parser modules
/**
 * Set of generic parsers i.e. parsers that does not depend on any stream implementation.
 */
type GenericsParsers<S, U> = Readonly<{
    // # from "core"
    lazy<A>(thunk: () => AbstractParser<A, S, U>): AbstractParser<A, S, U>;

    // # from "prim"
    map<A, B>(parser: AbstractParser<A, S, U>, func: (val: A) => B): AbstractParser<B, S, U>;
    fmap<A, B>(func: (val: A) => B): (parser: AbstractParser<A, S, U>) => AbstractParser<B, S, U>;
    pure<A>(val: A): AbstractParser<A, S, U>;
    return<A>(val: A): AbstractParser<A, S, U>;
    ap<A, B>(
        parserA: AbstractParser<(val: A) => B, S, U>,
        parserB: AbstractParser<A, S, U>
    ): AbstractParser<B, S, U>;
    left<A, B>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<A, S, U>;
    right<A, B>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<B, S, U>;
    bind<A, B>(
        parser: AbstractParser<A, S, U>,
        func: (val: A) => AbstractParser<B, S, U>
    ): AbstractParser<B, S, U>;
    then<A, B>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<B, S, U>;
    and<A, B>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<B, S, U>;
    fail(msgStr: string): AbstractParser<never, S, U>;
    tailRecM<A, B>(
        initVal: A,
        func: (val: A) => AbstractParser<TailRec<A, B>, S, U>
    ): AbstractParser<B, S, U>;
    ftailRecM<A, B>(
        func: (val: A) => AbstractParser<TailRec<A, B>, S, U>
    ): (initVal: A) => AbstractParser<B, S, U>;
    mzero: AbstractParser<never, S, U>;
    mplus<A, B>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<A | B, S, U>;
    or<A, B>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<A | B, S, U>;
    label<A>(
        parser: AbstractParser<A, S, U>,
        labelStr: string
    ): AbstractParser<A, S, U>;
    labels<A>(
        parser: AbstractParser<A, S, U>,
        labelStrs: string[]
    ): AbstractParser<A, S, U>;
    hidden<A>(parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>;
    unexpected(msgStr: string): AbstractParser<never, S, U>;
    tryParse<A>(parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>;
    try<A>(parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>;
    lookAhead<A>(parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>;
    reduceMany<A, B>(
        parser: AbstractParser<A, S, U>,
        callback: (accum: B, val: A) => B,
        initVal: B
    ): AbstractParser<B, S, U>;
    many<A>(parser: AbstractParser<A, S, U>): AbstractParser<A[], S, U>;
    skipMany<A>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>;
    getParserState: AbstractParser<State<S, U>, S, U>;
    setParserState(state: State<S, U>): AbstractParser<State<S, U>, S, U>;
    updateParserState(
        func: (state: State<S, U>) => State<S, U>
    ): AbstractParser<State<S, U>, S, U>;
    getConfig: AbstractParser<Config, S, U>;
    setConfig(config: Config): AbstractParser<undefined, S, U>;
    getInput: AbstractParser<S, S, U>;
    setInput(input: S): AbstractParser<undefined, S, U>;
    getPosition: AbstractParser<SourcePos, S, U>;
    setPosition(pos: SourcePos): AbstractParser<undefined, S, U>;
    getState: AbstractParser<U, S, U>;
    setState(userState: U): AbstractParser<undefined, S, U>;

    // # from "char"
    manyChars(parser: AbstractParser<string, S, U>): AbstractParser<string, S, U>;
    manyChars1(parser: AbstractParser<string, S, U>): AbstractParser<string, S, U>;

    // # from "combinators"
    choice<A>(parsers: AbstractParser<A, S, U>[]): AbstractParser<A, S, U>;
    option<A, B>(val: B, parser: AbstractParser<A, S, U>): AbstractParser<A | B, S, U>;
    optionMaybe<A>(parser: AbstractParser<A, S, U>): AbstractParser<Maybe<A>, S, U>;
    optional<A>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>;
    between<A, Open, Close>(
        open: AbstractParser<Open, S, U>,
        close: AbstractParser<Close, S, U>,
        parser: AbstractParser<A, S, U>
    ): AbstractParser<A, S, U>;
    many1<A>(parser: AbstractParser<A, S, U>): AbstractParser<A[], S, U>;
    skipMany1<A>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>;
    sepBy<A, Sep>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>;
    sepBy1<A, Sep>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>;
    sepEndBy<A, Sep>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>;
    sepEndBy1<A, Sep>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>;
    endBy<A, Sep>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>;
    endBy1<A, Sep>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>;
    count<A>(num: number, parser: AbstractParser<A, S, U>): AbstractParser<A[], S, U>;
    chainl<A>(
        term: AbstractParser<A, S, U>,
        op: AbstractParser<(accum: A, val: A) => A, S, U>,
        defaultVal: A
    ): AbstractParser<A, S, U>;
    chainl1<A>(
        term: AbstractParser<A, S, U>,
        op: AbstractParser<(accum: A, val: A) => A, S, U>
    ): AbstractParser<A, S, U>;
    chainr<A>(
        term: AbstractParser<A, S, U>,
        op: AbstractParser<(val: A, accum: A) => A, S, U>,
        defaultVal: A
    ): AbstractParser<A, S, U>;
    chainr1<A>(
        term: AbstractParser<A, S, U>,
        op: AbstractParser<(val: A, accum: A) => A, S, U>
    ): AbstractParser<A, S, U>;
    reduceManyTill<A, B, C>(
        parser: AbstractParser<A, S, U>,
        end: AbstractParser<B, S, U>,
        callback: (accum: C, val: A) => C,
        initVal: C
    ): AbstractParser<C, S, U>;
    manyTill<A, End>(
        parser: AbstractParser<A, S, U>,
        end: AbstractParser<End, S, U>
    ): AbstractParser<A[], S, U>;
    skipManyTill<A, End>(
        parser: AbstractParser<A, S, U>,
        end: AbstractParser<End, S, U>
    ): AbstractParser<undefined, S, U>;

    // # from "monad"
    forever<A>(parser: AbstractParser<A, S, U>): AbstractParser<never, S, U>;
    discard<A>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>;
    void<A>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>;
    join<A>(parser: AbstractParser<AbstractParser<A, S, U>, S, U>): AbstractParser<A, S, U>;
    when(cond: boolean, parser: AbstractParser<undefined, S, U>): AbstractParser<undefined, S, U>;
    unless(cond: boolean, parser: AbstractParser<undefined, S, U>): AbstractParser<undefined, S, U>;
    liftM<A, B>(func: (val: A) => B): (
        parser: AbstractParser<A, S, U>
    ) => AbstractParser<B, S, U>;
    liftM2<A, B, C>(func: (valA: A, valB: B) => C): (
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ) => AbstractParser<C, S, U>;
    liftM3<A, B, C, D>(func: (valA: A, valB: B, valC: C) => D): (
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>,
        parserC: AbstractParser<C, S, U>
    ) => AbstractParser<D, S, U>;
    liftM4<A, B, C, D, E>(func: (valA: A, valB: B, valC: C, valD: D) => E): (
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>,
        parserC: AbstractParser<C, S, U>,
        parserD: AbstractParser<D, S, U>
    ) => AbstractParser<E, S, U>;
    liftM5<A, B, C, D, E, F>(func: (valA: A, valB: B, valC: C, valD: D, valE: E) => F): (
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>,
        parserC: AbstractParser<C, S, U>,
        parserD: AbstractParser<D, S, U>,
        parserE: AbstractParser<E, S, U>
    ) => AbstractParser<F, S, U>;
    ltor<A, B, C>(
        funcA: (val: A) => AbstractParser<B, S, U>,
        funcB: (val: B) => AbstractParser<C, S, U>
    ): (val: A) => AbstractParser<C, S, U>;
    rtol<A, B, C>(
        funcA: (val: B) => AbstractParser<C, S, U>,
        funcB: (val: A) => AbstractParser<B, S, U>
    ): (val: A) => AbstractParser<C, S, U>;
    sequence<A>(parsers: AbstractParser<A, S, U>[]): AbstractParser<A[], S, U>;
    sequence_<A>(parsers: AbstractParser<A, S, U>[]): AbstractParser<undefined, S, U>;
    mapM<A, B>(func: (val: A) => AbstractParser<B, S, U>, arr: A[]): AbstractParser<B[], S, U>;
    mapM_<A, B>(
        func: (val: A) => AbstractParser<B, S, U>,
        arr: A[]
    ): AbstractParser<undefined, S, U>;
    forM<A, B>(arr: A[], func: (val: A) => AbstractParser<B, S, U>): AbstractParser<B[], S, U>;
    forM_<A, B>(
        arr: A[],
        func: (val: A) => AbstractParser<B, S, U>
    ): AbstractParser<undefined, S, U>;
    filterM<A>(test: (val: A) => AbstractParser<boolean, S, U>, arr: A[]): AbstractParser<A, S, U>;
    zipWithM<A, B, C>(
        func: (valA: A, valB: B) => AbstractParser<C, S, U>,
        arrA: A[],
        arrB: B[]
    ): AbstractParser<C[], S, U>;
    zipWithM_<A, B, C>(
        func: (valA: A, valB: B) => AbstractParser<C, S, U>,
        arrA: A[],
        arrB: B[]
    ): AbstractParser<undefined, S, U>;
    foldM<A, B>(
        func: (accum: B, val: A) => AbstractParser<B, S, U>,
        initVal: B,
        arr: A[]
    ): AbstractParser<B, S, U>;
    foldM_<A, B>(
        func: (accum: B, val: A) => AbstractParser<B, S, U>,
        initVal: B,
        arr: A[]
    ): AbstractParser<undefined, S, U>;
    replicateM<A>(num: number, parser: AbstractParser<A, S, U>): AbstractParser<A[], S, U>;
    replicateM_<A>(num: number, parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>;
    guard(cond: boolean): AbstractParser<undefined, S, U>;
    msum<A>(parsers: AbstractParser<A, S, U>[]): AbstractParser<A, S, U>;
    mfilter<A>(test: (val: A) => boolean, parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>;

    // # from "qo"
    qo<A>(genFunc: () => IterableIterator<any>): AbstractParser<A, S, U>;
    do<A>(genFunc: () => IterableIterator<any>): AbstractParser<A, S, U>;

    // # from "expr"
    buildExpressionParser<A>(
        opTable: Operator<A, S, U>[][],
        atom: AbstractParser<A, S, U>
    ): AbstractParser<A, S, U>;
}>;
/**
 * Set of parsers that depends on some stream implementation.
 */
type TokenStreamParsers<S, T, U> = Readonly<{
    // # from "prim"
    tokens(
        expectTokens: T[],
        tokenEqual: (tokenA: T, tokenB: T) => boolean,
        tokensToString: (tokens: T[]) => string,
        calcNextPos: (pos: SourcePos, tokens: T[], config: Config) => SourcePos
    ): AbstractParser<T[], S, U>;
    token<A>(
        calcValue: (token: T, config: Config) => Maybe<A>,
        tokenToString: (token: T) => string,
        calcPos: (token: T, config: Config) => SourcePos
    ): AbstractParser<A, S, U>;
    tokenPrim<A>(
        calcValue: (token: T, config: Config) => Maybe<A>,
        tokenToString: (token: T) => string,
        calcNextPos: (
            pos: SourcePos,
            head: T,
            tail: S,
            config: Config
        ) => SourcePos,
        calcNextUserState?: (
            userState: U,
            pos: SourcePos,
            head: T,
            tail: S,
            config: Config
        ) => U
    ): AbstractParser<A, S, U>;

    // # from "combinators"
    anyToken: AbstractParser<T, S, U>;
    notFollowedBy<A>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>;
    eof: AbstractParser<undefined, S, U>;
}>;
/**
 * Set of parsers that depends on some stream that yields string.
 */
type StringStreamParsers<S, U> = Readonly<{
    // # from "char"
    string(str: string): AbstractParser<string, S, U>;
    satisfy(test: (char: string, config: Config) => boolean): AbstractParser<string, S, U>;
    oneOf(str: string): AbstractParser<string, S, U>;
    noneOf(str: string): AbstractParser<string, S, U>;
    char(expectChar: string): AbstractParser<string, S, U>;
    anyChar: AbstractParser<string, S, U>;
    space: AbstractParser<string, S, U>;
    spaces: AbstractParser<undefined, S, U>;
    newline: AbstractParser<string, S, U>;
    tab: AbstractParser<string, S, U>;
    upper: AbstractParser<string, S, U>;
    lower: AbstractParser<string, S, U>;
    letter: AbstractParser<string, S, U>;
    digit: AbstractParser<string, S, U>;
    alphaNum: AbstractParser<string, S, U>;
    octDigit: AbstractParser<string, S, U>;
    hexDigit: AbstractParser<string, S, U>;

    // # from "token"
    makeTokenParser(def: LanguageDef<S, U>): TokenParser<S, U>;
}>;
/**
 * Set of parsers that depends on the predefined string stream implementation.
 */
type StringParsers<U> = Readonly<{
    // # from "char"
    regexp(re: RegExp, groupId?: number): AbstractParser<string, string, U>;
}>;
/**
 * Set of parsers of stream type `S` and token type `T`.
 */
type Parsers<S, T, U> =
      GenericsParsers<S, U>
    & TokenStreamParsers<S, T, U>
    & (T extends string ? StringStreamParsers<S, U> : unknown);
/**
 * String parsers.
 */
export declare function string<U = undefined>(): Parsers<string, string, U> & StringParsers<U>;
/**
 * Array parsers.
 */
export declare function array<T, U = undefined>(): Parsers<T[], T, U>;
/**
 * Stream parsers. A stream must implement `uncons` method.
 */
export declare function stream<T, S extends { uncons(): Unconsed<T, S> }, U = undefined>(
): Parsers<S, T, U>;
