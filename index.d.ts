// # common types
/**
 * `Maybe<A>` represents a value that can be empty or of type `A`.
 */
export declare type Maybe<A> = { empty: true } | { empty: false; value: A };
/**
 * AssocValueOf<T> computes the union type of the values associated to `T`.
 */
declare type AssocValueOf<T> = T[keyof T];

/**
 * `Unconsed<T, S>` represents a result of `uncons` that can be empty or a pair of head `T` and tail
 * `S`.
 */
export declare type Unconsed<T, S> = { empty: true } | { empty: false; head: T; tail: S };

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
export declare class SourcePos {
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
export declare type ErrorMessageType = AssocValueOf<typeof ErrorMessageType>;
export declare const ErrorMessageType: Readonly<{
    SYSTEM_UNEXPECT: "systemUnexpect";
    UNEXPECT: "unexpect";
    EXPECT: "expect";
    MESSAGE: "message";
}>;
/**
 * `ErrorMessage` represents a single error message in parse error.
 */
export declare class ErrorMessage {
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
export declare abstract class AbstractParseError {
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
export declare class ParseError extends AbstractParseError {
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
export declare class LazyParseError extends AbstractParseError {
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
export declare type ConfigOptions = { tabWidth?: number; unicode?: boolean };
/**
 * `Config` contains configuration information of parser.
 */
export declare class Config {
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
export declare class State<S, U = undefined> {
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
 * `Result<S, A, U>` represents a result of a parser of stream type `S` and user defined state
 * type `U`. It can be failure, or success with a value of type `A`.
 */
export declare type Result<S, A, U = undefined> = Failure | Success<S, A, U>;
export declare const Result: {
    /**
     * Tests if two results are equal.
     */
    equal<S, A, U>(
        resA: Result<S, A, U>,
        resB: Result<S, A, U>,
        valEqual: (valA: A, valB: A) => boolean,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    /**
     * Creates a successful result marked as the parser consumed some tokens from input.
     */
    csuc<S, A, U>(
        err: AbstractParseError,
        val: A,
        state: State<S, U>
    ): Success<S, A, U>;
    /**
     * Creates a unsuccessful result marked as the parser consumed some tokens from input.
     */
    cerr(err: AbstractParseError): Failure;
    /**
     * Creates a successful result marked as the parser did not consumed any tokens from input.
     */
    esuc<S, A, U>(
        err: AbstractParseError,
        val: A,
        state: State<S, U>
    ): Success<S, A, U>;
    /**
     * Creates a unsuccessful result marked as the parser did not consumed any tokens from
     * input.
     */
    eerr(err: AbstractParseError): Failure;
};
/**
 * `Failure` represents a unsuccessful result.
 */
export declare type Failure = {
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
export declare type Success<S, A, U> = {
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
 * `AbstractParser<S, A, U>` represents a parser of stream type `S` and user defined state type `U`
 * that yield a value of type `A` when it successes.
 */
export declare abstract class AbstractParser<S, A, U = undefined> {
    // # from "core"
    /**
     * Runs a parser with the initial state and returns a result. This is a primitive operation
     * and users may use `parse` insted.
     */
    public run(state: State<S, U>): Result<S, A, U>;
    /**
     * Parses the given input and returns a result.
     */
    public parse: MethodParse<S, A, U>;

    // # from "prim"
    public map<B>(func: (val: A) => B): AbstractParser<S, B, U>;
    public return<B>(val: B): AbstractParser<S, B, U>;
    public ap: MethodAp<S, A, U>;
    public left<B>(parser: AbstractParser<S, B, U>): AbstractParser<S, A, U>;
    public skip<B>(parser: AbstractParser<S, B, U>): AbstractParser<S, A, U>;
    public right<B>(parser: AbstractParser<S, B, U>): AbstractParser<S, B, U>;
    public bind<B>(func: (val: A) => AbstractParser<S, B, U>): AbstractParser<S, B, U>;
    public and<B>(parser: AbstractParser<S, B, U>): AbstractParser<S, B, U>;
    public fail(msgStr: string): AbstractParser<S, never, U>;
    public done(): AbstractParser<S, TailRecDone<A>, U>;
    public cont(): AbstractParser<S, TailRecCont<A>, U>;
    public or<B>(parser: AbstractParser<S, B, U>): AbstractParser<S, A | B, U>;
    public label(labelStr: string): AbstractParser<S, A, U>;
    public hidden(): AbstractParser<S, A, U>;
    public try(): AbstractParser<S, A, U>;
    public lookAhead(): AbstractParser<S, A, U>;
    public reduceMany<B>(callback: (accum: B, val: A) => B, initVal: B): AbstractParser<S, B, U>;
    public many(): AbstractParser<S, A[], U>;
    public skipMany(): AbstractParser<S, undefined, U>;
    public skipMany<B>(parser: AbstractParser<S, B, U>): AbstractParser<S, A, U>;

    // # from "char"
    public manyChars: MethodManyChar<S, A, U>;
    public manyChars1: MethodManyChar<S, A, U>;

    // # from "combinators"
    public option<B>(val: B): AbstractParser<S, A | B, U>;
    public optionMaybe(): AbstractParser<S, Maybe<A>, U>;
    public optional(): AbstractParser<S, undefined, U>;
    public between<Open, Close>(
        open: AbstractParser<S, Open, U>,
        close: AbstractParser<S, Close, U>
    ): AbstractParser<S, A, U>;
    public many1(): AbstractParser<S, A[], U>;
    public skipMany1(): AbstractParser<S, undefined, U>;
    public skipMany1<B>(parser: AbstractParser<S, B, U>): AbstractParser<S, A, U>;
    public sepBy<Sep>(sep: AbstractParser<S, Sep, U>): AbstractParser<S, A[], U>;
    public sepBy1<Sep>(sep: AbstractParser<S, Sep, U>): AbstractParser<S, A[], U>;
    public sepEndBy<Sep>(sep: AbstractParser<S, Sep, U>): AbstractParser<S, A[], U>;
    public sepEndBy1<Sep>(sep: AbstractParser<S, Sep, U>): AbstractParser<S, A[], U>;
    public endBy<Sep>(sep: AbstractParser<S, Sep, U>): AbstractParser<S, A[], U>;
    public endBy1<Sep>(sep: AbstractParser<S, Sep, U>): AbstractParser<S, A[], U>;
    public count(num: number): AbstractParser<S, A[], U>;
    public notFollowedBy(): AbstractParser<S, undefined, U>;
    public notFollowedBy<B>(parser: AbstractParser<S, B, U>): AbstractParser<S, A, U>;
    public reduceManyTill<B, End>(
        end: AbstractParser<S, End, U>,
        callback: (accum: B, val: A) => B,
        initVal: B
    ): AbstractParser<S, B, U>;
    public manyTill<End>(end: AbstractParser<S, End, U>): AbstractParser<S, A[], U>;
    public skipManyTill<End>(end: AbstractParser<S, End, U>): AbstractParser<S, undefined, U>;
    public skipManyTill<B, End>(
        parser: AbstractParser<S, B, U>,
        end: AbstractParser<S, End, U>
    ): AbstractParser<S, A, U>;

    // # from "monad"
    public forever(): AbstractParser<S, never, U>;
    public discard(): AbstractParser<S, undefined, U>;
    public void(): AbstractParser<S, undefined, U>;
    public join: MethodJoin<S, A, U>;
    public when(cond: boolean): AbstractParser<S, A, U>;
    public unless(cond: boolean): AbstractParser<S, A, U>;
    public filter(test: (val: A) => boolean): AbstractParser<S, A, U>;
}
/**
 * `ParseResult<A>` represents a result that can be failure with a parse error, or success with a
 * value of type `A`.
 */
export declare type ParseResult<A> =
      { success: false; error: AbstractParseError }
    | { success: true; value: A };
declare type MethodParse<S, A, U> = U extends undefined
    ? (name: string, input: S, userState?: U, opts?: ConfigOptions) => ParseResult<A>
    : (name: string, input: S, userState: U, opts?: ConfigOptions) => ParseResult<A>;
declare type MethodAp<S, A, U> = A extends (val: infer B) => infer C
    ? (parser: AbstractParser<S, B, U>) => AbstractParser<S, C, U>
    : unknown;
declare type MethodManyChar<S, A, U> = A extends string
    ? () => AbstractParser<S, string, U>
    : unknown;
declare type MethodJoin<S, A, U> = A extends AbstractParser<S, infer B, U>
    ? () => AbstractParser<S, B, U>
    : unknown;
/**
 * `Parser<S, A, U>` represents a standard parser. A parser can be considered a function that takes
 * the initial state and returns the final state and a resultant value when successful.
 */
export declare class Parser<S, A, U = undefined> extends AbstractParser<S, A, U> {
    constructor(func: (state: State<S, U>) => Result<S, A, U>);
}
/**
 * `LazyParser<S, A, U>` is a lazy evaluated version of `Parser<S, A, U>`.
 */
export declare class LazyParser<S, A, U = undefined> extends AbstractParser<S, A, U> {
    constructor(thunk: () => AbstractParser<S, A, U>);
    /**
     * Forces evaluation.
     */
    public eval(): Parser<S, A, U>;
}
export declare type StringParser<A, U = undefined> = AbstractParser<string, A, U>;
export declare type ArrayParser<T, A, U = undefined> = AbstractParser<T[], A, U>;
export declare type StreamParser<T, S extends { uncons(): Unconsed<T, S> }, A, U = undefined> =
    AbstractParser<S, A, U>;
export declare function parse<S, A>(
    parser: AbstractParser<S, A, undefined>,
    name: string,
    input: S,
    userState?: undefined,
    opts?: ConfigOptions
): ParseResult<A>;
export declare function parse<S, A, U>(
    parser: AbstractParser<S, A, U>,
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
export declare type TailRec<A, B> = TailRecCont<A> | TailRecDone<B>;
/**
 * `TailRecCont<A>` represents a parsing should be continued.
 */
export declare type TailRecCont<A> = { done: false; value: A };
/**
 * `TailRecCont<A>` represents a parsing should be done.
 */
export declare type TailRecDone<A> = { done: true; value: A };

// # from "expr"
/**
 * `OperatorType` represents type of operators.
 */
export declare type OperatorType = AssocValueOf<typeof OperatorType>;
export declare const OperatorType: Readonly<{
    INFIX: "infix";
    PREFIX: "prefix";
    POSTFIX: "postfix";
}>;
/**
 * `OperatorAssoc` represents associativity of infix operators.
 */
export declare type OperatorAssoc = AssocValueOf<typeof OperatorAssoc>;
export declare const OperatorAssoc: Readonly<{
    NONE: "none";
    LEFT: "left";
    RIGHT: "right";
}>;
/**
 * `Operator<S, A, U>` represents an operator that operates on `A`.
 */
export declare type Operator<S, A, U = undefined> =
      InfixOperator<S, A, U>
    | PrefixOperator<S, A, U>
    | PostfixOperator<S, A, U>;
export declare const Operator: {
    /**
     * Creates an infix operator.
     */
    infix<S, A, U>(
        parser: AbstractParser<S, (valA: A, valB: A) => A, U>,
        assoc: OperatorAssoc
    ): InfixOperator<S, A, U>;
    /**
     * Creates a prefix operator.
     */
    prefix<S, A, U>(parser: AbstractParser<S, (val: A) => A, U>): PrefixOperator<S, A, U>;
    /**
     * Creates a postfix operator.
     */
    postfix<S, A, U>(parser: AbstractParser<S, (val: A) => A, U>): PostfixOperator<S, A, U>;
};
/**
 * `InfixOperator<S, A, U>` represents an infix operator that operates on `A`.
 */
export declare type InfixOperator<S, A, U = undefined> = {
    /**
     * `"infix"`.
     */
    readonly type: "infix";
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<S, (valA: A, valB: A) => A, U>;
    /**
     * Associativity of the operator.
     */
    readonly assoc: OperatorAssoc;
};
/**
 * `PrefixOperator<S, A, U>` represents a prefix operator that operates on `A`.
 */
export declare type PrefixOperator<S, A, U = undefined> = {
    /**
     * `"prefix"`.
     */
    readonly type: "prefix";
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<S, (val: A) => A, U>;
};
/**
 * `PostfixOperator<S, A, U>` represents a postfix operator that operates on `A`.
 */
export declare type PostfixOperator<S, A, U = undefined> = {
    /**
     * `"postfix"`.
     */
    readonly type: "postfix";
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<S, (val: A) => A, U>;
};

// # from "token"
/**
 * Argument to the `LanguageDef` constructor.
 */
export declare type LanguageDefObj<S, U = undefined> = {
    commentStart?: string;
    commentEnd?: string;
    commentLine?: string;
    nestedComments?: boolean;
    idStart: AbstractParser<S, string, U>;
    idLetter: AbstractParser<S, string, U>;
    opStart: AbstractParser<S, string, U>;
    opLetter: AbstractParser<S, string, U>;
    reservedIds?: string[];
    reservedOps?: string[];
    caseSensitive?: boolean;
};
/**
 * `LanguageDef<S, U>` defines a language.
 */
export declare class LanguageDef<S, U = undefined> {
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
    public idStart: AbstractParser<S, string, U>;
    /**
     * A parser that parses a non-start letter of identifiers.
     */
    public idLetter: AbstractParser<S, string, U>;
    /**
     * A parser that parses a start letter of operators.
     */
    public opStart: AbstractParser<S, string, U>;
    /**
     * A parser that parses a non-start letter of operators.
     */
    public opLetter: AbstractParser<S, string, U>;
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
export declare type TokenParser<S, U> = {
    whiteSpace: AbstractParser<S, undefined, U>;
    lexeme: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A, U>;
    symbol: (name: string) => AbstractParser<S, string, U>;
    parens: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A, U>;
    braces: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A, U>;
    angles: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A, U>;
    brackets: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A, U>;
    semi: AbstractParser<S, string, U>;
    comma: AbstractParser<S, string, U>;
    colon: AbstractParser<S, string, U>;
    dot: AbstractParser<S, string, U>;
    semiSep: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A[], U>;
    semiSep1: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A[], U>;
    commaSep: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A[], U>;
    commaSep1: <A>(parser: AbstractParser<S, A, U>) => AbstractParser<S, A[], U>;
    decimal: AbstractParser<S, number, U>;
    hexadecimal: AbstractParser<S, number, U>;
    octal: AbstractParser<S, number, U>;
    natural: AbstractParser<S, number, U>;
    integer: AbstractParser<S, number, U>;
    float: AbstractParser<S, number, U>;
    naturalOrFloat: AbstractParser<S, NaturalOrFloat, U>;
    charLiteral: AbstractParser<S, string, U>;
    stringLiteral: AbstractParser<S, string, U>;
    identifier: AbstractParser<S, string, U>;
    reserved: (name: string) => AbstractParser<S, undefined, U>;
    operator: AbstractParser<S, string, U>;
    reservedOp: (name: string) => AbstractParser<S, undefined, U>;
};
/**
 * `NaturalOrFloat` represents a natural (integer) value or a float value.
 */
export declare type NaturalOrFloat =
      { type: "natural"; value: number }
    | { type: "float"; value: number };

// # parser modules
/**
 * Set of generic parsers i.e. parsers that does not depend on any stream implementation.
 */
declare type GenericsParsers<S, U> = Readonly<{
    // # from "core"
    lazy<A>(thunk: () => AbstractParser<S, A, U>): AbstractParser<S, A, U>;

    // # from "prim"
    map<A, B>(parser: AbstractParser<S, A, U>, func: (val: A) => B): AbstractParser<S, B, U>;
    fmap<A, B>(func: (val: A) => B): (parser: AbstractParser<S, A, U>) => AbstractParser<S, B, U>;
    pure<A>(val: A): AbstractParser<S, A, U>;
    return<A>(val: A): AbstractParser<S, A, U>;
    ap<A, B>(
        parserA: AbstractParser<S, (val: A) => B, U>,
        parserB: AbstractParser<S, A, U>
    ): AbstractParser<S, B, U>;
    left<A, B>(
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>
    ): AbstractParser<S, A, U>;
    right<A, B>(
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>
    ): AbstractParser<S, B, U>;
    bind<A, B>(
        parser: AbstractParser<S, A, U>,
        func: (val: A) => AbstractParser<S, B, U>
    ): AbstractParser<S, B, U>;
    then<A, B>(
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>
    ): AbstractParser<S, B, U>;
    and<A, B>(
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>
    ): AbstractParser<S, B, U>;
    fail(msgStr: string): AbstractParser<S, never, U>;
    tailRecM<A, B>(
        initVal: A,
        func: (val: A) => AbstractParser<S, TailRec<A, B>, U>
    ): AbstractParser<S, B, U>;
    ftailRecM<A, B>(
        func: (val: A) => AbstractParser<S, TailRec<A, B>, U>
    ): (initVal: A) => AbstractParser<S, B, U>;
    mzero: AbstractParser<S, never, U>;
    mplus<A, B>(
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>
    ): AbstractParser<S, A | B, U>;
    or<A, B>(
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>
    ): AbstractParser<S, A | B, U>;
    label<A>(
        parser: AbstractParser<S, A, U>,
        labelStr: string
    ): AbstractParser<S, A, U>;
    labels<A>(
        parser: AbstractParser<S, A, U>,
        labelStrs: string[]
    ): AbstractParser<S, A, U>;
    hidden<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, A, U>;
    unexpected(msgStr: string): AbstractParser<S, never, U>;
    tryParse<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, A, U>;
    try<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, A, U>;
    lookAhead<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, A, U>;
    reduceMany<A, B>(
        parser: AbstractParser<S, A, U>,
        callback: (accum: B, val: A) => B,
        initVal: B
    ): AbstractParser<S, B, U>;
    many<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, A[], U>;
    skipMany<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, undefined, U>;
    getParserState: AbstractParser<S, State<S, U>, U>;
    setParserState(state: State<S, U>): AbstractParser<S, State<S, U>, U>;
    updateParserState(
        func: (state: State<S, U>) => State<S, U>
    ): AbstractParser<S, State<S, U>, U>;
    getConfig: AbstractParser<S, Config, U>;
    setConfig(config: Config): AbstractParser<S, undefined, U>;
    getInput: AbstractParser<S, S, U>;
    setInput(input: S): AbstractParser<S, undefined, U>;
    getPosition: AbstractParser<S, SourcePos, U>;
    setPosition(pos: SourcePos): AbstractParser<S, undefined, U>;
    getState: AbstractParser<S, U, U>;
    setState(userState: U): AbstractParser<S, undefined, U>;

    // # from "char"
    manyChars(parser: AbstractParser<S, string, U>): AbstractParser<S, string, U>;
    manyChars1(parser: AbstractParser<S, string, U>): AbstractParser<S, string, U>;

    // # from "combinators"
    choice<A>(parsers: AbstractParser<S, A, U>[]): AbstractParser<S, A, U>;
    option<A, B>(val: B, parser: AbstractParser<S, A, U>): AbstractParser<S, A | B, U>;
    optionMaybe<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, Maybe<A>, U>;
    optional<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, undefined, U>;
    between<A, Open, Close>(
        open: AbstractParser<S, Open, U>,
        close: AbstractParser<S, Close, U>,
        parser: AbstractParser<S, A, U>
    ): AbstractParser<S, A, U>;
    many1<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, A[], U>;
    skipMany1<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, undefined, U>;
    sepBy<A, Sep>(
        parser: AbstractParser<S, A, U>,
        sep: AbstractParser<S, Sep, U>
    ): AbstractParser<S, A[], U>;
    sepBy1<A, Sep>(
        parser: AbstractParser<S, A, U>,
        sep: AbstractParser<S, Sep, U>
    ): AbstractParser<S, A[], U>;
    sepEndBy<A, Sep>(
        parser: AbstractParser<S, A, U>,
        sep: AbstractParser<S, Sep, U>
    ): AbstractParser<S, A[], U>;
    sepEndBy1<A, Sep>(
        parser: AbstractParser<S, A, U>,
        sep: AbstractParser<S, Sep, U>
    ): AbstractParser<S, A[], U>;
    endBy<A, Sep>(
        parser: AbstractParser<S, A, U>,
        sep: AbstractParser<S, Sep, U>
    ): AbstractParser<S, A[], U>;
    endBy1<A, Sep>(
        parser: AbstractParser<S, A, U>,
        sep: AbstractParser<S, Sep, U>
    ): AbstractParser<S, A[], U>;
    count<A>(num: number, parser: AbstractParser<S, A, U>): AbstractParser<S, A[], U>;
    chainl<A>(
        term: AbstractParser<S, A, U>,
        op: AbstractParser<S, (accum: A, val: A) => A, U>,
        defaultVal: A
    ): AbstractParser<S, A, U>;
    chainl1<A>(
        term: AbstractParser<S, A, U>,
        op: AbstractParser<S, (accum: A, val: A) => A, U>
    ): AbstractParser<S, A, U>;
    chainr<A>(
        term: AbstractParser<S, A, U>,
        op: AbstractParser<S, (val: A, accum: A) => A, U>,
        defaultVal: A
    ): AbstractParser<S, A, U>;
    chainr1<A>(
        term: AbstractParser<S, A, U>,
        op: AbstractParser<S, (val: A, accum: A) => A, U>
    ): AbstractParser<S, A, U>;
    reduceManyTill<A, B, End>(
        parser: AbstractParser<S, A, U>,
        end: AbstractParser<S, End, U>,
        callback: (accum: B, val: A) => B,
        initVal: B
    ): AbstractParser<S, B, U>;
    manyTill<A, End>(
        parser: AbstractParser<S, A, U>,
        end: AbstractParser<S, End, U>
    ): AbstractParser<S, A[], U>;
    skipManyTill<A, End>(
        parser: AbstractParser<S, A, U>,
        end: AbstractParser<S, End, U>
    ): AbstractParser<S, undefined, U>;

    // # from "monad"
    forever<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, never, U>;
    discard<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, undefined, U>;
    void<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, undefined, U>;
    join<A>(parser: AbstractParser<S, AbstractParser<S, A, U>, U>): AbstractParser<S, A, U>;
    when(cond: boolean, parser: AbstractParser<S, undefined, U>): AbstractParser<S, undefined, U>;
    unless(cond: boolean, parser: AbstractParser<S, undefined, U>): AbstractParser<S, undefined, U>;
    liftM<A, B>(func: (val: A) => B): (
        parser: AbstractParser<S, A, U>
    ) => AbstractParser<S, B, U>;
    liftM2<A, B, C>(func: (valA: A, valB: B) => C): (
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>
    ) => AbstractParser<S, C, U>;
    liftM3<A, B, C, D>(func: (valA: A, valB: B, valC: C) => D): (
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>,
        parserC: AbstractParser<S, C, U>
    ) => AbstractParser<S, D, U>;
    liftM4<A, B, C, D, E>(func: (valA: A, valB: B, valC: C, valD: D) => E): (
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>,
        parserC: AbstractParser<S, C, U>,
        parserD: AbstractParser<S, D, U>
    ) => AbstractParser<S, E, U>;
    liftM5<A, B, C, D, E, F>(func: (valA: A, valB: B, valC: C, valD: D, valE: E) => F): (
        parserA: AbstractParser<S, A, U>,
        parserB: AbstractParser<S, B, U>,
        parserC: AbstractParser<S, C, U>,
        parserD: AbstractParser<S, D, U>,
        parserE: AbstractParser<S, E, U>
    ) => AbstractParser<S, F, U>;
    ltor<A, B, C>(
        funcA: (val: A) => AbstractParser<S, B, U>,
        funcB: (val: B) => AbstractParser<S, C, U>
    ): (val: A) => AbstractParser<S, C, U>;
    rtol<A, B, C>(
        funcA: (val: B) => AbstractParser<S, C, U>,
        funcB: (val: A) => AbstractParser<S, B, U>
    ): (val: A) => AbstractParser<S, C, U>;
    sequence<A>(parsers: AbstractParser<S, A, U>[]): AbstractParser<S, A[], U>;
    sequence_<A>(parsers: AbstractParser<S, A, U>[]): AbstractParser<S, undefined, U>;
    mapM<A, B>(func: (val: A) => AbstractParser<S, B, U>, arr: A[]): AbstractParser<S, B[], U>;
    mapM_<A, B>(
        func: (val: A) => AbstractParser<S, B, U>,
        arr: A[]
    ): AbstractParser<S, undefined, U>;
    forM<A, B>(arr: A[], func: (val: A) => AbstractParser<S, B, U>): AbstractParser<S, B[], U>;
    forM_<A, B>(
        arr: A[],
        func: (val: A) => AbstractParser<S, B, U>
    ): AbstractParser<S, undefined, U>;
    filterM<A>(test: (val: A) => AbstractParser<S, boolean, U>, arr: A[]): AbstractParser<S, A, U>;
    zipWithM<A, B, C>(
        func: (valA: A, valB: B) => AbstractParser<S, C, U>,
        arrA: A[],
        arrB: B[]
    ): AbstractParser<S, C[], U>;
    zipWithM_<A, B, C>(
        func: (valA: A, valB: B) => AbstractParser<S, C, U>,
        arrA: A[],
        arrB: B[]
    ): AbstractParser<S, undefined, U>;
    foldM<A, B>(
        func: (accum: B, val: A) => AbstractParser<S, B, U>,
        initVal: B,
        arr: A[]
    ): AbstractParser<S, B, U>;
    foldM_<A, B>(
        func: (accum: B, val: A) => AbstractParser<S, B, U>,
        initVal: B,
        arr: A[]
    ): AbstractParser<S, undefined, U>;
    replicateM<A>(num: number, parser: AbstractParser<S, A, U>): AbstractParser<S, A[], U>;
    replicateM_<A>(num: number, parser: AbstractParser<S, A, U>): AbstractParser<S, undefined, U>;
    guard(cond: boolean): AbstractParser<S, undefined, U>;
    msum<A>(parsers: AbstractParser<S, A, U>[]): AbstractParser<S, A, U>;
    mfilter<A>(test: (val: A) => boolean, parser: AbstractParser<S, A, U>): AbstractParser<S, A, U>;

    // # from "qo"
    qo<A>(genFunc: () => IterableIterator<any>): AbstractParser<S, A, U>;
    do<A>(genFunc: () => IterableIterator<any>): AbstractParser<S, A, U>;

    // # from "expr"
    buildExpressionParser<A>(
        opTable: Operator<S, A, U>[][],
        atom: AbstractParser<S, A, U>
    ): AbstractParser<S, A, U>;
}>;
/**
 * Set of parsers that depends on some stream implementation.
 */
declare type TokenStreamParsers<S, T, U> = Readonly<{
    // # from "prim"
    tokens(
        expectTokens: T[],
        tokenEqual: (tokenA: T, tokenB: T) => boolean,
        tokensToString: (tokens: T[]) => string,
        calcNextPos: (pos: SourcePos, tokens: T[], config: Config) => SourcePos
    ): AbstractParser<S, T[], U>;
    token<A>(
        calcValue: (token: T, config: Config) => Maybe<A>,
        tokenToString: (token: T) => string,
        calcPos: (token: T, config: Config) => SourcePos
    ): AbstractParser<S, A, U>;
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
    ): AbstractParser<S, A, U>;

    // # from "combinators"
    anyToken: AbstractParser<S, T, U>;
    notFollowedBy<A>(parser: AbstractParser<S, A, U>): AbstractParser<S, undefined, U>;
    eof: AbstractParser<S, undefined, U>;
}>;
/**
 * Set of parsers that depends on some stream that yields string.
 */
declare type StringStreamParsers<S, U> = Readonly<{
    // # from "char"
    string(str: string): AbstractParser<S, string, U>;
    satisfy(test: (char: string, config: Config) => boolean): AbstractParser<S, string, U>;
    oneOf(str: string): AbstractParser<S, string, U>;
    noneOf(str: string): AbstractParser<S, string, U>;
    char(expectChar: string): AbstractParser<S, string, U>;
    anyChar: AbstractParser<S, string, U>;
    space: AbstractParser<S, string, U>;
    spaces: AbstractParser<S, undefined, U>;
    newline: AbstractParser<S, string, U>;
    tab: AbstractParser<S, string, U>;
    upper: AbstractParser<S, string, U>;
    lower: AbstractParser<S, string, U>;
    letter: AbstractParser<S, string, U>;
    digit: AbstractParser<S, string, U>;
    alphaNum: AbstractParser<S, string, U>;
    octDigit: AbstractParser<S, string, U>;
    hexDigit: AbstractParser<S, string, U>;

    // # from "token"
    makeTokenParser(def: LanguageDef<S, U>): TokenParser<S, U>;
}>;
/**
 * Set of parsers that depends on the predefined string stream implementation.
 */
declare type StringParsers<U> = Readonly<{
    // # from "char"
    regexp(re: RegExp, groupId?: number): AbstractParser<string, string, U>;
}>;
/**
 * Set of parsers of stream type `S` and token type `T`.
 */
declare type Parsers<S, T, U> =
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
