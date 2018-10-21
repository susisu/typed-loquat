// # common types
/**
 * `Maybe<A>` represents a value that can be empty or of type `A`.
 */
export declare type Maybe<A> = { empty: true } | { empty: false, value: A };
/**
 * AssocValueOf<T> computes the union type of the values associated to `T`.
 */
declare type AssocValueOf<T> = T[keyof T];

/**
 * `Unconsed<T, S>` represents a result of `uncons` that can be empty or a pair of head `T` and tail
 * `S`.
 */
export declare type Unconsed<T, S> = { empty: true } | { empty: false, head: T, tail: S };

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

// ## from "core/pos"
/**
 * `SourcePos` represents a position in source.
 */
export declare class SourcePos {
    constructor(name: string, line: number, column: number);
    /**
     * Creates an initial position i.e. line 1 and column 1.
     */
    static init(name: string): SourcePos;
    /**
     * Tests if two positions are equal.
     */
    static equal(posA: SourcePos, posB: SourcePos): boolean;
    /**
     * Compares two positions.
     */
    static compare(posA: SourcePos, posB: SourcePos): -1 | 0 | 1;
    /**
     * Name of the source.
     */
    readonly name: string;
    /**
     * Line number.
     */
    readonly line: number;
    /**
     * Column number.
     */
    readonly column: number;
    /**
     * Pretty-prints the position.
     */
    toString(): string;
    /**
     * Creates a copy with `name` updated.
     */
    setName(name: string): SourcePos;
    /**
     * Creates a copy with `line` updated.
     */
    setLine(line: number): SourcePos;
    /**
     * Creates a copy with `column` updated.
     */
    setColumn(column: number): SourcePos;
    /**
     * Computes a new position advanced by the given character.
     */
    addChar(char: string, tabWidth: number): SourcePos;
    /**
     * Computes a new position advanced by the given string.
     */
    addString(str: string, tabWidth: number, unicode: boolean): SourcePos;
}

// ## from "core/error"
/**
 * `ErrorMessageType` represents types of error messages.
 */
export declare type ErrorMessageType = AssocValueOf<typeof ErrorMessageType>;
export declare const ErrorMessageType: Readonly<{
    SYSTEM_UNEXPECT: "systemUnexpect",
    UNEXPECT       : "unexpect",
    EXPECT         : "expect",
    MESSAGE        : "message",
}>;
/**
 * `ErrorMessage` represents a single error message in parse error.
 */
export declare class ErrorMessage {
    constructor(type: ErrorMessageType, msgStr: string);
    /**
     * Tests if two error messages are equal.
     */
    static equal(msgA: ErrorMessage, msgB: ErrorMessage): boolean;
    /**
     * Pretty-prints multiple messages.
     */
    static messagesToString(msgs: ErrorMessage[]): string;
    /**
     * Tests if two message arrays are equal.
     */
    static messagesEqual(msgsA: ErrorMessage[], msgsB: ErrorMessage[]): boolean;
    /**
     * Type of the message.
     */
    readonly type: ErrorMessageType;
    /**
     * Content of the message.
     */
    readonly msgStr: string;
}
/**
 * `AbstractParseError` represents a parse error raised at some position in source.
 */
export declare abstract class AbstractParseError {
    /**
     * Position where the parse error is raised.
     */
    readonly pos: SourcePos;
    /**
     * Error messages of the parse error.
     */
    readonly msgs: ErrorMessage[];
    /**
     * Pretty-prints the error.
     */
    toString(): string;
    /**
     * Tests if the error is unknown i.e. it contains no messages.
     */
    isUnknown(): boolean;
    /**
     * Creates a new error object with `pos` updated.
     */
    setPosition(pos: SourcePos): AbstractParseError;
    /**
     * Creates a new error object with `msgs` updated.
     */
    setMessages(msgs: ErrorMessage[]): AbstractParseError;
    /**
     * Creates a new error object with the given messages added to the original message.
     */
    addMessages(msgs: ErrorMessage[]): AbstractParseError;
    /**
     * Creates a new error object with all messages of the given type updated.
     */
    setSpecificTypeMessages(type: ErrorMessageType, msgStrs: string[]): AbstractParseError;
}
/**
 * `ParseError` represents a standard parse error.
 */
export declare class ParseError extends AbstractParseError {
    constructor(pos: SourcePos, msgs: ErrorMessage[]);
    /**
     * Creates an unknown parse error with no messages.
     */
    static unknown(pos: SourcePos): ParseError;
    /**
     * Tests if two errors are equal.
     */
    static equal(errA: AbstractParseError, errB: AbstractParseError): boolean;
    /**
     * Merges two errors.
     */
    static merge(errA: AbstractParseError, errB: AbstractParseError): AbstractParseError;
}
/**
 * `LazyParseError` is a lazy evaluated version of `ParseError`.
 */
export declare class LazyParseError extends AbstractParseError {
    constructor(thunk: () => AbstractParseError);
    /**
     * Forces evaluation.
     */
    eval(): ParseError;
}

// ## from "core/parser"
/**
 * Argument to the `Config` constructor.
 */
export declare type ConfigOptions = { tabWidth?: number, unicode?: boolean };
/**
 * `Config` contains configuration information of parser.
 */
export declare class Config {
    constructor(opts: ConfigOptions);
    /**
     * Tests if two configurations are equal.
     */
    static equal(configA: Config, configB: Config): boolean;
    /**
     * Width of a tab character (default = `8`).
     */
    readonly tabWidth: number;
    /**
     * Whether various unicode features should be enabled (default = `false`).
     */
    readonly unicode: boolean;
}
/**
 * `State<S, U>` represents state of parsers of stream type `S` and user defined state type `U`.
 */
export declare class State<S = string, U = undefined> {
    constructor(config: Config, input: S, pos: SourcePos, userState: U);
    /**
     * Tests if two states are equal.
     */
    static equal<S, U = undefined>(
        stateA: State<S, U>,
        stateB: State<S, U>,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    /**
     * Current configuration.
     */
    readonly config: Config;
    /**
     * Current input.
     */
    readonly input: S;
    /**
     * Current position.
     */
    readonly pos: SourcePos;
    /**
     * Current user defined state.
     */
    readonly userState: U;
    /**
     * Creates a copy of the state with `config` updated.
     */
    setConfig(config: Config): State<S, U>;
    /**
     * Creates a copy of the state with `input` updated.
     */
    setInput<S2>(input: S2): State<S2, U>;
    /**
     * Creates a copy of the state with `pos` updated.
     */
    setPosition(pos: SourcePos): State<S, U>;
    /**
     * Creates a copy of the state with `userState` updated.
     */
    setUserState<U2>(userState: U2): State<S, U2>;
}
/**
 * `Result<A, S, U>` represents a result of a parser of stream type `S` and user defined state
 * type `U`. It can be failure, or success with a value of type `A`.
 */
export declare type Result<A, S, U = undefined> = Failure | Success<A, S, U>;
export declare const Result: Readonly<{
    /**
     * Tests if two results are equal.
     */
    equal<A, S, U = undefined>(
        resA: Result<A, S, U>,
        resB: Result<A, S, U>,
        valEqual: (valA: A, valB: A) => boolean,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    /**
     * Creates a successful result marked as the parser consumed some tokens from input.
     */
    csuc<A, S, U = undefined>(
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
    esuc<A, S, U = undefined>(
        err: AbstractParseError,
        val: A,
        state: State<S, U>
    ): Success<A, S, U>;
    /**
     * Creates a unsuccessful result marked as the parser did not consumed any tokens from input.
     */
    eerr(err: AbstractParseError): Failure;
}>;
/**
 * `Failure` represents a unsuccessful result.
 */
export declare type Failure = {
    /**
     * Whether the parser consumed some tokens from input or not.
     */
    readonly consumed: boolean,
    /**
     * `false`.
     */
    readonly success: false,
    /**
     * Parse error that will explain why the parser failed.
     */
    readonly err: AbstractParseError,
};
/**
 * `Success` represents a successful result.
 */
export declare type Success<A, S, U> = {
    /**
     * Whether the parser consumed some tokens from input or not.
     */
    readonly consumed: boolean,
    /**
     * `true`.
     */
    readonly success: true,
    /**
     * Parse error. This may contribute to making better error messages raised from succeeding
     * parsers.
     */
    readonly err: AbstractParseError,
    /**
     * Resultant value.
     */
    readonly val: A,
    /**
     * Fianl state of the parser.
     */
    readonly state: State<S, U>,
};
/**
 * `AbstractParser<A, S, U>` represents a parser of stream type `S` and user defined state type `U`
 * that yield a value of type `A` when it successes.
 */
export declare abstract class AbstractParser<A, S, U = undefined> {
    // # from "core"
    /**
     * Runs a parser with the initial state and returns a result. This is a primitive operation
     * and users may use `parse` insted.
     */
    run(state: State<S, U>): Result<A, S, U>;
    /**
     * Parses the given input and returns a result.
     */
    parse: MethodParse<A, S, U>;

    // # from "prim"
    map<B>(func: (val: A) => B): AbstractParser<B, S, U>;
    return<B>(val: B): AbstractParser<B, S, U>;
    ap: MethodAp<A, S, U>;
    left<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;
    skip<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;
    right<B>(parser: AbstractParser<B, S, U>): AbstractParser<B, S, U>;
    bind<B>(func: (val: A) => AbstractParser<B, S, U>): AbstractParser<B, S, U>;
    and<B>(parser: AbstractParser<B, S, U>): AbstractParser<B, S, U>;
    fail(msgStr: string): AbstractParser<never, S, U>;
    done(): AbstractParser<TailRecDone<A>, S, U>;
    cont(): AbstractParser<TailRecCont<A>, S, U>;
    or<B>(parser: AbstractParser<B, S, U>): AbstractParser<A | B, S, U>;
    label(labelStr: string): AbstractParser<A, S, U>;
    hidden(): AbstractParser<A, S, U>;
    try(): AbstractParser<A, S, U>;
    lookAhead(): AbstractParser<A, S, U>;
    reduceMany<B>(callback: (accum: B, val: A) => B, initVal: B): AbstractParser<B, S, U>;
    many(): AbstractParser<A[], S, U>;
    skipMany(): AbstractParser<undefined, S, U>;
    skipMany<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;

    // # from "char"
    manyChars: MethodManyChar<A, S, U>;
    manyChars1: MethodManyChar<A, S, U>;

    // # from "combinators"
    option<B>(val: B): AbstractParser<A | B, S, U>;
    optionMaybe(): AbstractParser<Maybe<A>, S, U>;
    optional(): AbstractParser<undefined, S, U>;
    between<B, C>(
        open: AbstractParser<B, S, U>,
        close: AbstractParser<C, S, U>
    ): AbstractParser<A, S, U>;
    many1(): AbstractParser<A[], S, U>;
    skipMany1(): AbstractParser<undefined, S, U>;
    skipMany1<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;
    sepBy<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    sepBy1<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    sepEndBy<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    sepEndBy1<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    endBy<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    endBy1<B>(sep: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    count(num: number): AbstractParser<A[], S, U>;
    notFollowedBy(): AbstractParser<undefined, S, U>;
    notFollowedBy<B>(parser: AbstractParser<B, S, U>): AbstractParser<A, S, U>;
    reduceManyTill<B, C>(
        end: AbstractParser<B, S, U>,
        callback: (accum: C, val: A) => C,
        initVal: C
    ): AbstractParser<C, S, U>;
    manyTill<B>(end: AbstractParser<B, S, U>): AbstractParser<A[], S, U>;
    skipManyTill<B>(end: AbstractParser<B, S, U>): AbstractParser<undefined, S, U>;
    skipManyTill<B, C>(
        parser: AbstractParser<B, S, U>,
        end: AbstractParser<C, S, U>
    ): AbstractParser<A, S, U>;

    // # from "monad"
    forever(): AbstractParser<never, S, U>;
    discard(): AbstractParser<undefined, S, U>;
    void(): AbstractParser<undefined, S, U>;
    join: MethodJoin<A, S, U>;
    when(cond: boolean): AbstractParser<A, S, U>;
    unless(cond: boolean): AbstractParser<A, S, U>;
    filter(test: (val: A) => boolean): AbstractParser<A, S, U>;
}
/**
 * `ParseResult<A>` represents a result that can be failure with a parse error, or success with a
 * value of type `A`.
 */
export declare type ParseResult<A> =
      { success: false, error: AbstractParseError }
    | { success: true, value: A };
declare type MethodParse<A, S, U> = U extends undefined
    ? (name: string, input: S, userState?: U, opts?: ConfigOptions) => ParseResult<A>
    : (name: string, input: S, userState: U, opts?: ConfigOptions) => ParseResult<A>;
declare type MethodAp<A, S, U> = A extends (val: infer B) => infer C
    ? (parser: AbstractParser<B, S, U>) => AbstractParser<C, S, U>
    : unknown;
declare type MethodManyChar<A, S, U> = A extends string
    ? () => AbstractParser<string, S, U>
    : unknown;
declare type MethodJoin<A, S, U> = A extends AbstractParser<infer B, S, U>
    ? () => AbstractParser<B, S, U>
    : unknown;
/**
 * `Parser<A, S, U>` represents a standard parser. A parser can be considered a function that takes
 * the initial state and returns the final state and a resultant value when successful.
 */
export declare class Parser<A, S, U = undefined> extends AbstractParser<A, S, U> {
    constructor(func: (state: State<S, U>) => Result<A, S, U>);
}
/**
 * `LazyParser<A, S, U>` is a lazy evaluated version of `Parser<A, S, U>`.
 */
export declare class LazyParser<A, S, U = undefined> extends AbstractParser<A, S, U> {
    constructor(thunk: () => AbstractParser<A, S, U>);
    /**
     * Forces evaluation.
     */
    eval(): Parser<A, S, U>;
}
/**
 * Tests if a value is a parser.
 */
export declare function isParser<T>(val: T): boolean;
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
export declare type TailRecCont<A> = { done: false, value: A };
/**
 * `TailRecCont<A>` represents a parsing should be done.
 */
export declare type TailRecDone<A> = { done: true, value: A };

// # from "expr"
/**
 * `OperatorType` represents type of operators.
 */
export declare type OperatorType = AssocValueOf<typeof OperatorType>;
export declare const OperatorType: Readonly<{
    INFIX  : "infix",
    PREFIX : "prefix",
    POSTFIX: "postfix",
}>;
/**
 * `OperatorAssoc` represents associativity of infix operators.
 */
export declare type OperatorAssoc = AssocValueOf<typeof OperatorAssoc>;
export declare const OperatorAssoc: Readonly<{
    NONE : "none",
    LEFT : "left",
    RIGHT: "right",
}>;
/**
 * `Operator<A, S, U>` represents an operator that operates on `A`.
 */
export declare type Operator<A, S, U = undefined> =
      InfixOperator<A, S, U>
    | PrefixOperator<A, S, U>
    | PostfixOperator<A, S, U>;
export declare const Operator: Readonly<{
    /**
     * Creates an infix operator.
     */
    infix<A, S, U = undefined>(
        parser: AbstractParser<(valA: A, valB: A) => A, S, U>,
        assoc: OperatorAssoc
    ): InfixOperator<A, S, U>,
    /**
     * Creates a prefix operator.
     */
    prefix<A, S, U = undefined>(
        parser: AbstractParser<(val: A) => A, S, U>
    ): PrefixOperator<A, S, U>,
    /**
     * Creates a postfix operator.
     */
    postfix<A, S, U = undefined>(
        parser: AbstractParser<(val: A) => A, S, U>
    ): PostfixOperator<A, S, U>,
}>;
/**
 * `InfixOperator<A, S, U>` represents an infix operator that operates on `A`.
 */
export declare type InfixOperator<A, S, U = undefined> = {
    /**
     * `"infix"`.
     */
    readonly type: "infix",
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<(valA: A, valB: A) => A, S, U>,
    /**
     * Associativity of the operator.
     */
    readonly assoc: OperatorAssoc,
};
/**
 * `PrefixOperator<A, S, U>` represents a prefix operator that operates on `A`.
 */
export declare type PrefixOperator<A, S, U = undefined> = {
    /**
     * `"prefix"`.
     */
    readonly type: "prefix",
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<(val: A) => A, S, U>,
};
/**
 * `PostfixOperator<A, S, U>` represents a postfix operator that operates on `A`.
 */
export declare type PostfixOperator<A, S, U = undefined> = {
    /**
     * `"postfix"`.
     */
    readonly type: "postfix",
    /**
     * Parser that parses an operator symbol and yields an operator function.
     */
    readonly parser: AbstractParser<(val: A) => A, S, U>,
};

// # from "token"
/**
 * Argument to the `LanguageDef` constructor.
 */
export declare type LanguageDefObj<S, U = undefined> = {
    commentStart?  : string,
    commentEnd?    : string,
    commentLine?   : string,
    nestedComments?: boolean,
    idStart        : AbstractParser<string, S, U>,
    idLetter       : AbstractParser<string, S, U>,
    opStart        : AbstractParser<string, S, U>,
    opLetter       : AbstractParser<string, S, U>,
    reservedIds?   : string[],
    reservedOps?   : string[],
    caseSensitive? : boolean,
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
    commentStart: string | undefined;
    /**
     * A string that marks the end of a multiline comment. The language has no multiline comments
     * when it is an empty string or undefined.
     */
    commentEnd: string | undefined;
    /**
     * A string that marks the start of a line comment. The language has no line comments when it is
     * an empty string or undefined.
     */
    commentLine: string | undefined;
    /**
     * Whether nested multiline comments are allowed or not (default = `true`).
     */
    nestedComments: boolean;
    /**
     * A parser that parses a start letter of identifiers.
     */
    idStart: AbstractParser<string, S, U>;
    /**
     * A parser that parses a non-start letter of identifiers.
     */
    idLetter: AbstractParser<string, S, U>;
    /**
     * A parser that parses a start letter of operators.
     */
    opStart: AbstractParser<string, S, U>;
    /**
     * A parser that parses a non-start letter of operators.
     */
    opLetter: AbstractParser<string, S, U>;
    /**
     * An array of reserved words.
     */
    reservedIds: string[];
    /**
     * An array of reserved operators.
     */
    reservedOps: string[];
    /**
     * Whether the reserved words are case-sensitive or not (default = `true`).
     */
    caseSensitive: boolean;
    /**
     * Creates a copy of the language definition.
     */
    clone(): LanguageDef<S, U>;
}
/**
 * `TokenParser<S, U>` represents a set of token parsers.
 */
export declare type TokenParser<S, U> = {
    whiteSpace    : AbstractParser<undefined, S, U>,
    lexeme        : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>,
    symbol        : (name: string) => AbstractParser<string, S, U>,
    parens        : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>,
    braces        : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>,
    angles        : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>,
    brackets      : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A, S, U>,
    semi          : AbstractParser<string, S, U>,
    comma         : AbstractParser<string, S, U>,
    colon         : AbstractParser<string, S, U>,
    dot           : AbstractParser<string, S, U>,
    semiSep       : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A[], S, U>,
    semiSep1      : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A[], S, U>,
    commaSep      : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A[], S, U>,
    commaSep1     : <A>(parser: AbstractParser<A, S, U>) => AbstractParser<A[], S, U>,
    decimal       : AbstractParser<number, S, U>,
    hexadecimal   : AbstractParser<number, S, U>,
    octal         : AbstractParser<number, S, U>,
    natural       : AbstractParser<number, S, U>,
    integer       : AbstractParser<number, S, U>,
    float         : AbstractParser<number, S, U>,
    naturalOrFloat: AbstractParser<NaturalOrFloat, S, U>,
    charLiteral   : AbstractParser<string, S, U>,
    stringLiteral : AbstractParser<string, S, U>,
    identifier    : AbstractParser<string, S, U>,
    reserved      : (name: string) => AbstractParser<undefined, S, U>,
    operator      : AbstractParser<string, S, U>,
    reservedOp    : (name: string) => AbstractParser<undefined, S, U>,
};
/**
 * `NaturalOrFloat` represents a natural (integer) value or a float value.
 */
export declare type NaturalOrFloat =
      { type: "natural", value: number }
    | { type: "float", value: number };

// # parser modules
/**
 * Set of generic parsers i.e. parsers that does not depend on any stream implementation.
 */
declare type GenericParserSet<S> = Readonly<{
    // # from "core"
    lazy<A, U = undefined>(
        thunk: () => AbstractParser<A, S, U>
    ): LazyParser<A, S, U>;
    parse<A, U = undefined>(
        parser: AbstractParser<A, S, U>,
        name: string,
        input: S,
        userState: U,
        opts?: ConfigOptions
    ): ParseResult<A>;
    parse<A>(
        parser: AbstractParser<A, S, undefined>,
        name: string,
        input: S,
        userState?: undefined,
        opts?: ConfigOptions
    ): ParseResult<A>;

    // # from "prim"
    map<A, B, U = undefined>(
        parser: AbstractParser<A, S, U>,
        func: (val: A) => B
    ): AbstractParser<B, S, U>,
    fmap<A, B>(
        func: (val: A) => B
    ): <U = undefined>(parser: AbstractParser<A, S, U>) => AbstractParser<B, S, U>,
    pure<A, U = undefined>(val: A): AbstractParser<A, S, U>,
    return<A, U = undefined>(val: A): AbstractParser<A, S, U>,
    ap<A, B, U = undefined>(
        parserA: AbstractParser<(val: A) => B, S, U>,
        parserB: AbstractParser<A, S, U>
    ): AbstractParser<B, S, U>,
    left<A, B, U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<A, S, U>,
    right<A, B, U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<B, S, U>,
    bind<A, B, U = undefined>(
        parser: AbstractParser<A, S, U>,
        func: (val: A) => AbstractParser<B, S, U>
    ): AbstractParser<B, S, U>,
    then<A, B, U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<B, S, U>,
    and<A, B, U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<B, S, U>,
    fail<U = undefined>(msgStr: string): AbstractParser<never, S, U>,
    tailRecM<A, B, U = undefined>(
        initVal: A,
        func: (val: A) => AbstractParser<TailRec<A, B>, S, U>
    ): AbstractParser<B, S, U>,
    ftailRecM<A, B, U = undefined>(
        func: (val: A) => AbstractParser<TailRec<A, B>, S, U>
    ): (initVal: A) => AbstractParser<B, S, U>,
    unsafeMzero: AbstractParser<never, S, any>,
    mzero<U = undefined>(): AbstractParser<never, S, U>,
    mplus<A, B, U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<A | B, S, U>,
    or<A, B, U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ): AbstractParser<A | B, S, U>,
    label<A, U = undefined>(
        parser: AbstractParser<A, S, U>,
        labelStr: string
    ): AbstractParser<A, S, U>,
    labels<A, U = undefined>(
        parser: AbstractParser<A, S, U>,
        labelStrs: string[]
    ): AbstractParser<A, S, U>,
    hidden<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>,
    unexpected<U = undefined>(msgStr: string): AbstractParser<never, S, U>,
    tryParse<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>,
    try<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>,
    lookAhead<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<A, S, U>,
    reduceMany<A, B, U = undefined>(
        parser: AbstractParser<A, S, U>,
        callback: (accum: B, val: A) => B,
        initVal: B
    ): AbstractParser<B, S, U>,
    many<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<A[], S, U>,
    skipMany<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>,
    unsafeGetParserState: AbstractParser<State<S, any>, S, any>,
    getParserState<U = undefined>(): AbstractParser<State<S, U>, S, U>,
    setParserState<U = undefined>(state: State<S, U>): AbstractParser<State<S, U>, S, U>,
    updateParserState<U = undefined>(
        func: (state: State<S, U>) => State<S, U>
    ): AbstractParser<State<S, U>, S, U>,
    unsafeGetConfig: AbstractParser<Config, S, any>,
    getConfig<U = undefined>(): AbstractParser<Config, S, U>,
    setConfig<U = undefined>(config: Config): AbstractParser<undefined, S, U>,
    unsafeGetInput: AbstractParser<S, S, any>,
    getInput<U = undefined>(): AbstractParser<S, S, U>,
    setInput<U = undefined>(input: S): AbstractParser<undefined, S, U>,
    unsafeGetPosition: AbstractParser<SourcePos, S, any>,
    getPosition<U = undefined>(): AbstractParser<SourcePos, S, U>,
    setPosition<U = undefined>(pos: SourcePos): AbstractParser<undefined, S, U>,
    unsafeGetState: AbstractParser<any, S, any>,
    getState<U = undefined>(): AbstractParser<U, S, U>,
    setState<U = undefined>(userState: U): AbstractParser<undefined, S, U>,

    // # from "char"
    manyChars<U = undefined>(parser: AbstractParser<string, S, U>): AbstractParser<string, S, U>,
    manyChars1<U = undefined>(parser: AbstractParser<string, S, U>): AbstractParser<string, S, U>,

    // # from "combinators"
    choice<A, U = undefined>(parsers: AbstractParser<A, S, U>[]): AbstractParser<A, S, U>,
    option<A, B, U = undefined>(
        val: B,
        parser: AbstractParser<A, S, U>
    ): AbstractParser<A | B, S, U>,
    optionMaybe<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<Maybe<A>, S, U>,
    optional<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>,
    between<A, Open, Close, U = undefined>(
        open: AbstractParser<Open, S, U>,
        close: AbstractParser<Close, S, U>,
        parser: AbstractParser<A, S, U>
    ): AbstractParser<A, S, U>,
    many1<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<A[], S, U>,
    skipMany1<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>,
    sepBy<A, Sep, U = undefined>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>,
    sepBy1<A, Sep, U = undefined>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>,
    sepEndBy<A, Sep, U = undefined>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>,
    sepEndBy1<A, Sep, U = undefined>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>,
    endBy<A, Sep, U = undefined>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>,
    endBy1<A, Sep, U = undefined>(
        parser: AbstractParser<A, S, U>,
        sep: AbstractParser<Sep, S, U>
    ): AbstractParser<A[], S, U>,
    count<A, U = undefined>(
        num: number,
        parser: AbstractParser<A, S, U>
    ): AbstractParser<A[], S, U>,
    chainl<A, U = undefined>(
        term: AbstractParser<A, S, U>,
        op: AbstractParser<(accum: A, val: A) => A, S, U>,
        defaultVal: A
    ): AbstractParser<A, S, U>,
    chainl1<A, U = undefined>(
        term: AbstractParser<A, S, U>,
        op: AbstractParser<(accum: A, val: A) => A, S, U>,
    ): AbstractParser<A, S, U>,
    chainr<A, U = undefined>(
        term: AbstractParser<A, S, U>,
        op: AbstractParser<(val: A, accum: A) => A, S, U>,
        defaultVal: A
    ): AbstractParser<A, S, U>,
    chainr1<A, U = undefined>(
        term: AbstractParser<A, S, U>,
        op: AbstractParser<(val: A, accum: A) => A, S, U>,
    ): AbstractParser<A, S, U>,
    reduceManyTill<A, B, C, U = undefined>(
        parser: AbstractParser<A, S, U>,
        end: AbstractParser<B, S, U>,
        callback: (accum: C, val: A) => C,
        initVal: C
    ): AbstractParser<C, S, U>,
    manyTill<A, End, U = undefined>(
        parser: AbstractParser<A, S, U>,
        end: AbstractParser<End, S, U>
    ): AbstractParser<A[], S, U>,
    skipManyTill<A, End, U = undefined>(
        parser: AbstractParser<A, S, U>,
        end: AbstractParser<End, S, U>
    ): AbstractParser<undefined, S, U>,

    // # from "monad"
    forever<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<never, S, U>,
    discard<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>,
    void<A, U = undefined>(parser: AbstractParser<A, S, U>): AbstractParser<undefined, S, U>,
    join<A, U = undefined>(
        parser: AbstractParser<AbstractParser<A, S, U>, S, U>
    ): AbstractParser<A, S, U>,
    when<U = undefined>(
        cond: boolean,
        parser: AbstractParser<undefined, S, U>
    ): AbstractParser<undefined, S, U>,
    unless<U = undefined>(
        cond: boolean,
        parser: AbstractParser<undefined, S, U>
    ): AbstractParser<undefined, S, U>,
    liftM<A, B>(func: (val: A) => B): <U = undefined>(
        parser: AbstractParser<A, S, U>
    ) => AbstractParser<B, S, U>,
    liftM2<A, B, C>(func: (valA: A, valB: B) => C): <U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>
    ) => AbstractParser<C, S, U>,
    liftM3<A, B, C, D>(func: (valA: A, valB: B, valC: C) => D): <U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>,
        parserC: AbstractParser<C, S, U>
    ) => AbstractParser<D, S, U>,
    liftM4<A, B, C, D, E>(func: (valA: A, valB: B, valC: C, valD: D) => E): <U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>,
        parserC: AbstractParser<C, S, U>,
        parserD: AbstractParser<D, S, U>
    ) => AbstractParser<E, S, U>,
    liftM5<A, B, C, D, E, F>(
        func: (valA: A, valB: B, valC: C, valD: D, valE: E) => F
    ): <U = undefined>(
        parserA: AbstractParser<A, S, U>,
        parserB: AbstractParser<B, S, U>,
        parserC: AbstractParser<C, S, U>,
        parserD: AbstractParser<D, S, U>,
        parserE: AbstractParser<E, S, U>
    ) => AbstractParser<F, S, U>,
    ltor<A, B, C, U = undefined>(
        funcA: (val: A) => AbstractParser<B, S, U>,
        funcB: (val: B) => AbstractParser<C, S, U>
    ): (val: A) => AbstractParser<C, S, U>,
    rtol<A, B, C, U = undefined>(
        funcA: (val: B) => AbstractParser<C, S, U>,
        funcB: (val: A) => AbstractParser<B, S, U>
    ): (val: A) => AbstractParser<C, S, U>,
    sequence<A, U = undefined>(parsers: AbstractParser<A, S, U>[]): AbstractParser<A[], S, U>,
    sequence_<A, U = undefined>(
        parsers: AbstractParser<A, S, U>[]
    ): AbstractParser<undefined, S, U>,
    mapM<A, B, U = undefined>(
        func: (val: A) => AbstractParser<B, S, U>,
        arr: A[]
    ): AbstractParser<B[], S, U>,
    mapM_<A, B, U = undefined>(
        func: (val: A) => AbstractParser<B, S, U>,
        arr: A[]
    ): AbstractParser<undefined, S, U>,
    forM<A, B, U = undefined>(
        arr: A[],
        func: (val: A) => AbstractParser<B, S, U>
    ): AbstractParser<B[], S, U>,
    forM_<A, B, U = undefined>(
        arr: A[],
        func: (val: A) => AbstractParser<B, S, U>
    ): AbstractParser<undefined, S, U>,
    filterM<A, U = undefined>(
        test: (val: A) => AbstractParser<boolean, S, U>,
        arr: A[]
    ): AbstractParser<A, S, U>,
    zipWithM<A, B, C, U = undefined>(
        func: (valA: A, valB: B) => AbstractParser<C, S, U>,
        arrA: A[],
        arrB: B[]
    ): AbstractParser<C[], S, U>,
    zipWithM_<A, B, C, U = undefined>(
        func: (valA: A, valB: B) => AbstractParser<C, S, U>,
        arrA: A[],
        arrB: B[]
    ): AbstractParser<undefined, S, U>,
    foldM<A, B, U = undefined>(
        func: (accum: B, val: A) => AbstractParser<B, S, U>,
        initVal: B,
        arr: A[]
    ): AbstractParser<B, S, U>,
    foldM_<A, B, U = undefined>(
        func: (accum: B, val: A) => AbstractParser<B, S, U>,
        initVal: B,
        arr: A[]
    ): AbstractParser<undefined, S, U>,
    replicateM<A, U = undefined>(
        num: number,
        parser: AbstractParser<A, S, U>
    ): AbstractParser<A[], S, U>,
    replicateM_<A, U = undefined>(
        num: number,
        parser: AbstractParser<A, S, U>
    ): AbstractParser<undefined, S, U>,
    guard<U = undefined>(cond: boolean): AbstractParser<undefined, S, U>,
    msum<A, U = undefined>(parsers: AbstractParser<A, S, U>[]): AbstractParser<A, S, U>,
    mfilter<A, U = undefined>(
        test: (val: A) => boolean,
        parser: AbstractParser<A, S, U>
    ): AbstractParser<A, S, U>,

    // # from "qo"
    qo<T, U = undefined>(genFunc: () => IterableIterator<any>): AbstractParser<T, S, U>,
    do<T, U = undefined>(genFunc: () => IterableIterator<any>): AbstractParser<T, S, U>,

    // # from "expr"
    buildExpressionParser<A, U = undefined>(
        opTable: Operator<A, S, U>[][],
        atom: AbstractParser<A, S, U>
    ): AbstractParser<A, S, U>;
}>;

/**
 * Set of parsers that depends on some stream implementation.
 */
declare type TokenStreamParserSet<S, T> = Readonly<{
    // # from "core"
    uncons(input: S, unicode: boolean): Unconsed<T, S>,

    // # from "prim"
    tokens<U = undefined>(
        expectTokens: T[],
        tokenEqual: (tokenA: T, tokenB: T) => boolean,
        tokensToString: (tokens: T[]) => string,
        calcNextPos: (pos: SourcePos, tokens: T[], config: Config) => SourcePos
    ): AbstractParser<T[], S, U>,
    token<A, U = undefined>(
        calcValue: (token: T, config: Config) => Maybe<A>,
        tokenToString: (token: T) => string,
        calcPos: (token: T, config: Config) => SourcePos
    ): AbstractParser<A, S, U>,
    tokenPrim<A, U = undefined>(
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
    ): AbstractParser<A, S, U>,

    // # from "combinators"
    unsafeAnyToken: AbstractParser<T, S, any>,
    anyToken<U = undefined>(): AbstractParser<T, S, U>,
    notFollowedBy<A, U = undefined>(
        parser: AbstractParser<A, S, U>
    ): AbstractParser<undefined, S, U>,
    unsafeEof: AbstractParser<undefined, S, any>,
    eof<U = undefined>(): AbstractParser<undefined, S, U>,
}>;

/**
 * Set of parsers that depends on some string stream implementation.
 */
declare type StringStreamParserSet<S> = Readonly<{
    // # from "char"
    string<U = undefined>(str: string): AbstractParser<string, S, U>,
    satisfy<U = undefined>(
        test: (char: string, config: Config) => boolean
    ): AbstractParser<string, S, U>,
    oneOf<U = undefined>(str: string): AbstractParser<string, S, U>,
    noneOf<U = undefined>(str: string): AbstractParser<string, S, U>,
    char<U = undefined>(expectChar: string): AbstractParser<string, S, U>,
    unsafeAnyChar: AbstractParser<string, S, any>,
    anyChar<U = undefined>(): AbstractParser<string, S, U>,
    unsafeSpace: AbstractParser<string, S, any>,
    space<U = undefined>(): AbstractParser<string, S, U>,
    unsafeSpaces: AbstractParser<undefined, S, any>,
    spaces<U = undefined>(): AbstractParser<undefined, S, U>,
    unsafeNewline: AbstractParser<string, S, any>,
    newline<U = undefined>(): AbstractParser<string, S, U>,
    unsafeTab: AbstractParser<string, S, any>,
    tab<U = undefined>(): AbstractParser<string, S, U>,
    unsafeUpper: AbstractParser<string, S, any>,
    upper<U = undefined>(): AbstractParser<string, S, U>,
    unsafeLower: AbstractParser<string, S, any>,
    lower<U = undefined>(): AbstractParser<string, S, U>,
    unsafeLetter: AbstractParser<string, S, any>,
    letter<U = undefined>(): AbstractParser<string, S, U>,
    unsafeDigit: AbstractParser<string, S, any>,
    digit<U = undefined>(): AbstractParser<string, S, U>,
    unsafeAlphaNum: AbstractParser<string, S, any>,
    alphaNum<U = undefined>(): AbstractParser<string, S, U>,
    unsafeOctDigit: AbstractParser<string, S, any>,
    octDigit<U = undefined>(): AbstractParser<string, S, U>,
    unsafeHexDigit: AbstractParser<string, S, any>,
    hexDigit<U = undefined>(): AbstractParser<string, S, U>,

    // # from "token"
    makeTokenParser<S, U = undefined>(def: LanguageDef<S, U>): TokenParser<S, U>;
}>;

/**
 * Set of parsers that depends on the predefined string stream and only available in the `string`
 * module.
 */
declare type PredefStringStreamParserSet = Readonly<{
    // # from "char"
    regexp<U = undefined>(re: RegExp, groupId?: number): AbstractParser<string, string, U>,
}>;

/**
 * `Stream<S, T>` is a type class (or a module signature). Its instances mut define a function
 * `uncons`, that reads the input stream of `S` and returns its head (token) of type `T` and
 * tail (rest stream) of `S`.
 */
export declare type Stream<S, T> = {
    uncons: (input: S, unicode: boolean) => Unconsed<T, S>,
};

/**
 * Creates a new parser module.
 */
export declare function make<S, T>(
    stream: Stream<S, T>
): ParserModule<S, T>;

/**
 * Type of standard parser module of stream type `S` and token type `T`.
 */
declare type ParserModule<S, T> =
      GenericParserSet<S>
    & TokenStreamParserSet<S, T>
    & (T extends string ? StringStreamParserSet<S> : unknown);

/**
 * Type of the predefined `string` module.
 */
declare type PredefStringParserModule = ParserModule<string, string> & PredefStringStreamParserSet;
/**
 * Standard string parser module.
 */
export declare const string: PredefStringParserModule;

/**
 * Type of array parser module.
 */
declare type ArrayParserModule<T> = ParserModule<T[], T>;
/**
 * Creates an array parser module.
 */
export declare function array<T>(): ArrayParserModule<T>;

/**
 * Creates a stream parser module. A stream must implement `uncons` method that follows the
 * definition of `Stream<S, T>`.
 */
export declare function stream<T, S extends { uncons(): Unconsed<T, S> }>(): ParserModule<S, T>;
