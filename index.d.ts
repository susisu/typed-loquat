export declare type Maybe<A> = { empty: true } | { empty: false, value: A };
declare type AssocValueOf<T> = T[keyof T];

// # core
export declare type Unconsed<T, S> = EmptyUnconsed | NonEmptyUnconsed<T, S>;
declare type EmptyUnconsed = { empty: true };
declare type NonEmptyUnconsed<T, S> = { empty: false, head: T, tail: S };

// ## core.utils
export declare function show<T>(value: T): string;
export declare function unconsString(str: string, unicode: boolean): Unconsed<string, string>;

// ## core.pos
export declare class SourcePos {
    constructor(name: string, line: number, column: number);
    static init(name: string): SourcePos;
    static equal(posA: SourcePos, posB: SourcePos): boolean;
    static compare(posA: SourcePos, posB: SourcePos): -1 | 0 | 1;
    readonly name: string;
    readonly line: number;
    readonly column: number;
    toString(): string;
    setName(name: string): SourcePos;
    setLine(line: number): SourcePos;
    setColumn(column: number): SourcePos;
    addChar(char: string, tabWidth: number): SourcePos;
    addString(str: string, tabWidth: number, unicode: boolean): SourcePos;
}

// ## core.error
export declare type ErrorMessageType = AssocValueOf<typeof ErrorMessageType>;
export declare const ErrorMessageType: Readonly<{
    SYSTEM_UNEXPECT: "systemUnexpect",
    UNEXPECT       : "unexpect",
    EXPECT         : "expect",
    MESSAGE        : "message",
}>;
export declare class ErrorMessage {
    constructor(type: ErrorMessageType, msgStr: string);
    static equal(msgA: ErrorMessage, msgB: ErrorMessage): boolean;
    static messagesToString(msgs: ErrorMessage[]): string;
    static messagesEqual(msgsA: ErrorMessage[], msgsB: ErrorMessage[]): boolean;
    readonly type: ErrorMessageType;
    readonly msgStr: string;
}
export declare abstract class AbstractParseError {
    readonly pos: SourcePos;
    readonly msgs: ErrorMessage[];
    toString(): string;
    isUnknown(): boolean;
    setPosition(pos: SourcePos): AbstractParseError;
    setMessages(msgs: ErrorMessage[]): AbstractParseError;
    addMessages(msgs: ErrorMessage[]): AbstractParseError;
    setSpecificTypeMessages(type: ErrorMessageType, msgStrs: string[]): AbstractParseError;
}
export declare class ParseError extends AbstractParseError {
    constructor(pos: SourcePos, msgs: ErrorMessage[]);
    static unknown(pos: SourcePos): ParseError;
    static equal(errA: AbstractParseError, errB: AbstractParseError): boolean;
    static merge(errA: AbstractParseError, errB: AbstractParseError): AbstractParseError;
}
export declare class LazyParseError extends AbstractParseError {
    constructor(thunk: () => AbstractParseError);
    eval(): ParseError;
}

// ## core.stream
export declare interface IStream<T> {
    uncons(): Unconsed<T, IStream<T>>;
}
declare interface EmptyStream {
    uncons(): EmptyUnconsed;
}
export declare type Stream<T> = (string extends T ? string : never) | T[] | IStream<T>;
export declare type AbstractStream<S> =
      (S extends string ? string : never)
    | (S extends (infer T)[] ? T[] : never)
    | AbstractIStream<S>;
declare type AbstractIStream<S> = S extends IStream<infer T>
    ? (S extends EmptyStream ? S : ValidateIStream<S, UnconsedTail<ReturnType<S["uncons"]>>>)
    : never;
declare type UnconsedTail<U> = U extends NonEmptyUnconsed<unknown, infer S> ? S : never;
declare type ValidateIStream<S, SS> = S extends SS ? SS : never;
export declare type Token<S extends Stream<unknown>> =
      (S extends string ? string : never)
    | (S extends (infer T)[] ? T : never)
    | IStreamToken<S>;
declare type IStreamToken<S> = S extends IStream<infer T>
    ? (S extends EmptyStream ? never : T)
    : never;
export declare function uncons<S extends Stream<unknown>>(
    input: AbstractStream<S>,
    unicode: boolean
): Unconsed<Token<S>, AbstractStream<S>>;
export declare class ArrayStream<T> implements IStream<T> {
    constructor(arr: T[], index: number);
    readonly arr: T[];
    readonly index: number;
    uncons(): Unconsed<T, ArrayStream<T>>;
}

// ## core.parser
export declare type ConfigOptions = { tabWidth: number, unicode: boolean };
export declare class Config {
    constructor(opts: ConfigOptions);
    static equal(configA: Config, configB: Config): boolean;
    readonly tabWidth: number;
    readonly unicode: boolean;
}
export declare class State<S = string, U = undefined> {
    constructor(config: Config, input: S, pos: SourcePos, userState: U);
    static equal<S = string, U = undefined>(
        stateA: State<S, U>,
        stateB: State<S, U>,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    readonly config: Config;
    readonly input: S;
    readonly pos: SourcePos;
    readonly userState: U;
    setConfig(config: Config): State<S, U>;
    setInput<S2>(input: S2): State<S2, U>;
    setPosition(pos: SourcePos): State<S, U>;
    setUserState<U2>(userState: U2): State<S, U2>;
}
export declare type Result<A, S = string, U = undefined> = Failure | Success<A, S, U>;
export declare const Result: Readonly<{
    equal<A, S = string, U = undefined>(
        resA: Result<A, S, U>,
        resB: Result<A, S, U>,
        valEqual: (valA: A, valB: A) => boolean,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    csuc<A, S = string, U = undefined>(
        err: AbstractParseError,
        val: A,
        state: State<S, U>
    ): Success<A, S, U>;
    cerr(err: AbstractParseError): Failure;
    esuc<A, S = string, U = undefined>(
        err: AbstractParseError,
        val: A,
        state: State<S, U>
    ): Success<A, S, U>;
    eerr(err: AbstractParseError): Failure;
}>;
export declare type Failure = {
   readonly consumed: boolean,
   readonly success: false,
   readonly err: AbstractParseError,
};
export declare type Success<A, S = string, U = undefined> = {
   readonly consumed: boolean,
   readonly success: true,
   readonly err: AbstractParseError,
   readonly val: A,
   readonly state: State<S, U>,
};
export declare type ParseResult<A> =
      { success: false, error: AbstractParseError }
    | { success: true, value: A };
export declare abstract class AbstractParser<A, S = string, U = undefined> {
    run(state: State<S, U>): Result<A, S, U>;
    parse: MethodParse<A, S, U>;
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
    manyChars: MethodManyChar<A, S, U>;
    manyChars1: MethodManyChar<A, S, U>;
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
    forever(): AbstractParser<never, S, U>;
    discard(): AbstractParser<undefined, S, U>;
    void(): AbstractParser<undefined, S, U>;
    join: MethodJoin<A, S, U>;
    when(cond: boolean): AbstractParser<A, S, U>;
    unless(cond: boolean): AbstractParser<A, S, U>;
    filter(test: (val: A) => boolean): AbstractParser<A, S, U>;
}
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
export declare class Parser<A, S = string, U = undefined> extends AbstractParser<A, S, U> {
    constructor(func: (state: State<S, U>) => Result<A, S, U>);
}
export declare class LazyParser<A, S = string, U = undefined> extends AbstractParser<A, S, U> {
    constructor(thunk: () => AbstractParser<A, S, U>);
    eval(): Parser<A, S, U>;
}
export declare function lazy<A, S = string, U = undefined>(
    thunk: () => AbstractParser<A, S, U>
): LazyParser<A, S, U>;
export declare function parse<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    name: string,
    input: S,
    userState: U,
    opts?: ConfigOptions
): ParseResult<A>;
export declare function parse<A, S = string>(
    parser: AbstractParser<A, S, undefined>,
    name: string,
    input: S,
    userState?: undefined,
    opts?: ConfigOptions
): ParseResult<A>;
export declare function isParser(val: unknown): boolean;
export declare function assertParser(val: unknown): undefined;

// # prim
export declare function map<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    func: (val: A) => B
): AbstractParser<B, S, U>;
export declare function fmap<A, B>(
    func: (val: A) => B
): <S = string, U = undefined>(parser: AbstractParser<A, S, U>) => AbstractParser<B, S, U>;
export declare function pure<A, S = string, U = undefined>(val: A): AbstractParser<A, S, U>;
export declare function _return<A, S = string, U = undefined>(val: A): AbstractParser<A, S, U>;
export declare function ap<A, B, S = string, U = undefined>(
    parserA: AbstractParser<(val: A) => B, S, U>,
    parserB: AbstractParser<A, S, U>
): AbstractParser<B, S, U>;
export declare function left<A, B, S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>
): AbstractParser<A, S, U>;
export declare function right<A, B, S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>
): AbstractParser<B, S, U>;
export declare function bind<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    func: (val: A) => AbstractParser<B, S, U>
): AbstractParser<B, S, U>;
export declare function then<A, B, S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>
): AbstractParser<B, S, U>;
export declare function and<A, B, S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>
): AbstractParser<B, S, U>;
export declare function fail<S = string, U = undefined>(
    msgStr: string
): AbstractParser<never, S, U>;
export declare type TailRec<A, B> = TailRecCont<A> | TailRecDone<B>;
export declare type TailRecCont<A> = { done: false, value: A };
export declare type TailRecDone<A> = { done: true, value: A };
export declare function tailRecM<A, B, S = string, U = undefined>(
    initVal: A,
    func: (val: A) => AbstractParser<TailRec<A, B>, S, U>
): AbstractParser<B, S, U>;
export declare function ftailRecM<A, B, S = string, U = undefined>(
    func: (val: A) => AbstractParser<TailRec<A, B>, S, U>
): (initVal: A) => AbstractParser<B, S, U>;
export declare const unsafeMzero: AbstractParser<never, any, any>;
export declare function mzero<S = string, U = undefined>(): AbstractParser<never, S, U>;
export declare function mplus<A, B, S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>
): AbstractParser<A | B, S, U>;
export declare function or<A, B, S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>
): AbstractParser<A | B, S, U>;
export declare function label<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    labelStr: string
): AbstractParser<A, S, U>;
export declare function labels<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    labelStrs: string[]
): AbstractParser<A, S, U>;
export declare function hidden<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<A, S, U>;
export declare function unexpected<S = string, U = undefined>(
    msgStr: string
): AbstractParser<never, S, U>;
export declare function tryParse<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<A, S, U>;
export declare function _try<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<A, S, U>;
export declare function lookAhead<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<A, S, U>;
export declare function reduceMany<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    callback: (accum: B, val: A) => B,
    initVal: B
): AbstractParser<B, S, U>;
export declare function many<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<A[], S, U>;
export declare function skipMany<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<undefined, S, U>;
export declare function tokens<S extends Stream<unknown> = string, U = undefined>(
    expectTokens: Token<S>[],
    tokenEqual: (tokenA: Token<S>, tokenB: Token<S>) => boolean,
    tokensToString: (tokens: Token<S>[]) => string,
    calcNextPos: (pos: SourcePos, tokens: Token<S>[], config: Config) => SourcePos
): AbstractParser<Token<S>[], AbstractStream<S>, U>;
export declare function token<A, S extends Stream<unknown> = string, U = undefined>(
    calcValue: (token: Token<S>, config: Config) => Maybe<A>,
    tokenToString: (token: Token<S>) => string,
    calcPos: (token: Token<S>, config: Config) => SourcePos
): AbstractParser<A, AbstractStream<S>, U>;
export declare function tokenPrim<A, S extends Stream<unknown> = string, U = undefined>(
    calcValue: (token: Token<S>, config: Config) => Maybe<A>,
    tokenToString: (token: Token<S>) => string,
    calcNextPos: (
        pos: SourcePos,
        head: Token<S>,
        tail: AbstractStream<S>,
        config: Config
    ) => SourcePos,
    calcNextUserState?: (
        userState: U,
        pos: SourcePos,
        head: Token<S>,
        tail: AbstractStream<S>,
        config: Config
    ) => U
): AbstractParser<A, AbstractStream<S>, U>;
export declare const unsafeGetParserState: AbstractParser<State<any, any>, any, any>;
export declare function getParserState<S = string, U = undefined>(
): AbstractParser<State<S, U>, S, U>;
export declare function setParserState<S = string, U = undefined>(
    state: State<S, U>
): AbstractParser<State<S, U>, S, U>;
export declare function updateParserState<S = string, U = undefined>(
    func: (state: State<S, U>) => State<S, U>
): AbstractParser<State<S, U>, S, U>;
export declare const unsafeGetConfig: AbstractParser<Config, any, any>;
export declare function getConfig<S = string, U = undefined>(): AbstractParser<Config, S, U>;
export declare function setConfig<S = string, U = undefined>(
    config: Config
): AbstractParser<undefined, S, U>;
export declare const unsafeGetInput: AbstractParser<any, any, any>;
export declare function getInput<S = string, U = undefined>(): AbstractParser<S, S, U>;
export declare function setInput<S = string, U = undefined>(
    input: S
): AbstractParser<undefined, S, U>;
export declare const unsafeGetPosition: AbstractParser<SourcePos, any, any>;
export declare function getPosition<S = string, U = undefined>(): AbstractParser<SourcePos, S, U>;
export declare function setPosition<S = string, U = undefined>(
    pos: SourcePos
): AbstractParser<undefined, S, U>;
export declare const unsafeGetState: AbstractParser<any, any, any>;
export declare function getState<S = string, U = undefined>(): AbstractParser<U, S, U>;
export declare function setState<S = string, U = undefined>(
    userState: U
): AbstractParser<undefined, S, U>;

// # char
export declare function string<S extends Stream<string> = string, U = undefined>(
    str: string
): AbstractParser<string, AbstractStream<S>, U>;
export declare function satisfy<S extends Stream<string> = string, U = undefined>(
    test: (char: string, config: Config) => boolean
): AbstractParser<string, AbstractStream<S>, U>;
export declare function oneOf<S extends Stream<string> = string, U = undefined>(
    str: string
): AbstractParser<string, AbstractStream<S>, U>;
export declare function noneOf<S extends Stream<string> = string, U = undefined>(
    str: string
): AbstractParser<string, AbstractStream<S>, U>;
export declare function char<S extends Stream<string> = string, U = undefined>(
    expectChar: string
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeAnyChar: AbstractParser<string, any, any>;
export declare function anyChar<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeSpace: AbstractParser<string, any, any>;
export declare function space<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeSpaces: AbstractParser<undefined, any, any>;
export declare function spaces<S extends Stream<string> = string, U = undefined>(
): AbstractParser<undefined, AbstractStream<S>, U>;
export declare const unsafeNewline: AbstractParser<string, any, any>;
export declare function newline<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeTab: AbstractParser<string, any, any>;
export declare function tab<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeUpper: AbstractParser<string, any, any>;
export declare function upper<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeLower: AbstractParser<string, any, any>;
export declare function lower<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeLetter: AbstractParser<string, any, any>;
export declare function letter<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeDigit: AbstractParser<string, any, any>;
export declare function digit<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeAlphaNum: AbstractParser<string, any, any>;
export declare function alphaNum<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeOctDigit: AbstractParser<string, any, any>;
export declare function octDigit<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare const unsafeHexDigit: AbstractParser<string, any, any>;
export declare function hexDigit<S extends Stream<string> = string, U = undefined>(
): AbstractParser<string, AbstractStream<S>, U>;
export declare function manyChars<S = string, U = undefined>(
    parser: AbstractParser<string, S, U>
): AbstractParser<string, S, U>;
export declare function manyChars1<S = string, U = undefined>(
    parser: AbstractParser<string, S, U>
): AbstractParser<string, S, U>;
export declare function regexp<U = undefined>(
    re: RegExp,
    groupId?: number
): AbstractParser<string, string, U>;

// # combinators
export declare function choice<A, S = string, U = undefined>(
    parsers: AbstractParser<A, S, U>[]
): AbstractParser<A, S, U>;
export declare function option<A, B, S = string, U = undefined>(
    val: B,
    parser: AbstractParser<A, S, U>
): AbstractParser<A | B, S, U>;
export declare function optionMaybe<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<Maybe<A>, S, U>;
export declare function optional<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<undefined, S, U>;
export declare function between<A, B, C, S = string, U = undefined>(
    open: AbstractParser<A, S, U>,
    close: AbstractParser<B, S, U>,
    parser: AbstractParser<C, S, U>
): AbstractParser<C, S, U>;
export declare function many1<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<A[], S, U>;
export declare function skipMany1<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<undefined, S, U>;
export declare function sepBy<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    sep: AbstractParser<B, S, U>
): AbstractParser<A[], S, U>;
export declare function sepBy1<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    sep: AbstractParser<B, S, U>
): AbstractParser<A[], S, U>;
export declare function sepEndBy<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    sep: AbstractParser<B, S, U>
): AbstractParser<A[], S, U>;
export declare function sepEndBy1<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    sep: AbstractParser<B, S, U>
): AbstractParser<A[], S, U>;
export declare function endBy<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    sep: AbstractParser<B, S, U>
): AbstractParser<A[], S, U>;
export declare function endBy1<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    sep: AbstractParser<B, S, U>
): AbstractParser<A[], S, U>;
export declare function count<A, S = string, U = undefined>(
    num: number,
    parser: AbstractParser<A, S, U>
): AbstractParser<A[], S, U>;
export declare function chainl<A, S = string, U = undefined>(
    term: AbstractParser<A, S, U>,
    op: AbstractParser<(accum: A, val: A) => A, S, U>,
    defaultVal: A
): AbstractParser<A, S, U>;
export declare function chainl1<A, S = string, U = undefined>(
    term: AbstractParser<A, S, U>,
    op: AbstractParser<(accum: A, val: A) => A, S, U>,
): AbstractParser<A, S, U>;
export declare function chainr<A, S = string, U = undefined>(
    term: AbstractParser<A, S, U>,
    op: AbstractParser<(val: A, accum: A) => A, S, U>,
    defaultVal: A
): AbstractParser<A, S, U>;
export declare function chainr1<A, S = string, U = undefined>(
    term: AbstractParser<A, S, U>,
    op: AbstractParser<(val: A, accum: A) => A, S, U>,
): AbstractParser<A, S, U>;
export declare const unsafeAnyToken: AbstractParser<any, any, any>;
export declare function anyToken<S extends Stream<unknown> = string, U = undefined>(
): AbstractParser<Token<S>, AbstractStream<S>, U>;
export declare function notFollowedBy<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<undefined, S, U>;
export declare const unsafeEof: AbstractParser<undefined, any, any>;
export declare function eof<S extends Stream<unknown> = string, U = undefined>(
): AbstractParser<undefined, AbstractStream<S>, U>;
export declare function reduceManyTill<A, B, C, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    end: AbstractParser<B, S, U>,
    callback: (accum: C, val: A) => C,
    initVal: C
): AbstractParser<C, S, U>;
export declare function manyTill<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    end: AbstractParser<B, S, U>
): AbstractParser<A[], S, U>;
export declare function skipManyTill<A, B, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>,
    end: AbstractParser<B, S, U>
): AbstractParser<undefined, S, U>;

// # monad
export declare function forever<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<never, S, U>;
export declare function discard<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<undefined, S, U>;
export declare function _void<A, S = string, U = undefined>(
    parser: AbstractParser<A, S, U>
): AbstractParser<undefined, S, U>;
export declare function join<A, S = string, U = undefined>(
    parser: AbstractParser<AbstractParser<A, S, U>, S, U>
): AbstractParser<A, S, U>;
export declare function when<S = string, U = undefined>(
    cond: boolean,
    parser: AbstractParser<undefined, S, U>
): AbstractParser<undefined, S, U>;
export declare function unless<S = string, U = undefined>(
    cond: boolean,
    parser: AbstractParser<undefined, S, U>
): AbstractParser<undefined, S, U>;
export declare function liftM<A, B>(
    func: (val: A) => B
): <S = string, U = undefined>(parser: AbstractParser<A, S, U>) => AbstractParser<B, S, U>;
export declare function liftM2<A, B, C>(
    func: (valA: A, valB: B) => C
): <S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>
) => AbstractParser<C, S, U>;
export declare function liftM3<A, B, C, D>(
    func: (valA: A, valB: B, valC: C) => D
): <S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>,
    parserC: AbstractParser<C, S, U>
) => AbstractParser<D, S, U>;
export declare function liftM4<A, B, C, D, E>(
    func: (valA: A, valB: B, valC: C, valD: D) => E
): <S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>,
    parserC: AbstractParser<C, S, U>,
    parserD: AbstractParser<D, S, U>
) => AbstractParser<E, S, U>;
export declare function liftM5<A, B, C, D, E, F>(
    func: (valA: A, valB: B, valC: C, valD: D, valE: E) => F
): <S = string, U = undefined>(
    parserA: AbstractParser<A, S, U>,
    parserB: AbstractParser<B, S, U>,
    parserC: AbstractParser<C, S, U>,
    parserD: AbstractParser<D, S, U>,
    parserE: AbstractParser<E, S, U>
) => AbstractParser<F, S, U>;
export declare function ltor<A, B, C, S = string, U = undefined>(
    funcA: (val: A) => AbstractParser<B, S, U>,
    funcB: (val: B) => AbstractParser<C, S, U>
): (val: A) => AbstractParser<C, S, U>;
export declare function rtol<A, B, C, S = string, U = undefined>(
    funcA: (val: B) => AbstractParser<C, S, U>,
    funcB: (val: A) => AbstractParser<B, S, U>
): (val: A) => AbstractParser<C, S, U>;
export declare function sequence<A, S = string, U = undefined>(
    parsers: AbstractParser<A, S, U>[]
): AbstractParser<A[], S, U>;
export declare function sequence_<A, S = string, U = undefined>(
    parsers: AbstractParser<A, S, U>[]
): AbstractParser<undefined, S, U>;
export declare function mapM<A, B, S = string, U = undefined>(
    func: (val: A) => AbstractParser<B, S, U>,
    arr: A[]
): AbstractParser<B[], S, U>;
export declare function mapM_<A, B, S = string, U = undefined>(
    func: (val: A) => AbstractParser<B, S, U>,
    arr: A[]
): AbstractParser<undefined, S, U>;
export declare function forM<A, B, S = string, U = undefined>(
    arr: A[],
    func: (val: A) => AbstractParser<B, S, U>
): AbstractParser<B[], S, U>;
export declare function forM_<A, B, S = string, U = undefined>(
    arr: A[],
    func: (val: A) => AbstractParser<B, S, U>
): AbstractParser<undefined, S, U>;
export declare function filterM<A, S = string, U = undefined>(
    test: (val: A) => AbstractParser<boolean, S, U>,
    arr: A[]
): AbstractParser<A, S, U>;
export declare function zipWithM<A, B, C, S = string, U = undefined>(
    func: (valA: A, valB: B) => AbstractParser<C, S, U>,
    arrA: A[],
    arrB: B[]
): AbstractParser<C[], S, U>;
export declare function zipWithM_<A, B, C, S = string, U = undefined>(
    func: (valA: A, valB: B) => AbstractParser<C, S, U>,
    arrA: A[],
    arrB: B[]
): AbstractParser<undefined, S, U>;
export declare function foldM<A, B, S = string, U = undefined>(
    func: (accum: B, val: A) => AbstractParser<B, S, U>,
    initVal: B,
    arr: A[]
): AbstractParser<B, S, U>;
export declare function foldM_<A, B, S = string, U = undefined>(
    func: (accum: B, val: A) => AbstractParser<B, S, U>,
    initVal: B,
    arr: A[]
): AbstractParser<undefined, S, U>;
export declare function replicateM<A, S = string, U = undefined>(
    num: number,
    parser: AbstractParser<A, S, U>
): AbstractParser<A[], S, U>;
export declare function replicateM_<A, S = string, U = undefined>(
    num: number,
    parser: AbstractParser<A, S, U>
): AbstractParser<undefined, S, U>;
export declare function guard<S = string, U = undefined>(
    cond: boolean
): AbstractParser<undefined, S, U>;
export declare function msum<A, S = string, U = undefined>(
    parsers: AbstractParser<A, S, U>[]
): AbstractParser<A, S, U>;
export declare function mfilter<A, S = string, U = undefined>(
    test: (val: A) => boolean,
    parser: AbstractParser<A, S, U>
): AbstractParser<A, S, U>;

// # qo
export declare function qo<T, S = string, U = undefined>(
    genFunc: () => IterableIterator<any>
): AbstractParser<T, S, U>;
export declare function _do<T, S = string, U = undefined>(
    genFunc: () => IterableIterator<any>
): AbstractParser<T, S, U>;

// # expr
export declare type OperatorType = AssocValueOf<typeof OperatorType>;
export declare const OperatorType: Readonly<{
    INFIX  : "infix",
    PREFIX : "prefix",
    POSTFIX: "postfix",
}>;
export declare type OperatorAssoc = AssocValueOf<typeof OperatorAssoc>;
export declare const OperatorAssoc: Readonly<{
    NONE : "none",
    LEFT : "left",
    RIGHT: "right",
}>;
export declare type Operator<A, S = string, U = undefined> =
      InfixOperator<A, S, U>
    | PrefixOperator<A, S, U>
    | PostfixOperator<A, S, U>;
export declare const Operator: Readonly<{
    infix<A, S = string, U = undefined>(
        parser: AbstractParser<(valA: A, valB: A) => A, S, U>,
        assoc: OperatorAssoc
    ): InfixOperator<A, S, U>,
    prefix<A, S = string, U = undefined>(
        parser: AbstractParser<(val: A) => A, S, U>
    ): PrefixOperator<A, S, U>,
    postfix<A, S = string, U = undefined>(
        parser: AbstractParser<(val: A) => A, S, U>
    ): PostfixOperator<A, S, U>,
}>;
export declare type InfixOperator<A, S = string, U = undefined> = {
    readonly type: "infix",
    readonly parser: AbstractParser<(valA: A, valB: A) => A, S, U>,
    readonly assoc: OperatorAssoc,
};
export declare type PrefixOperator<A, S = string, U = undefined> = {
    readonly type: "prefix",
    readonly parser: AbstractParser<(val: A) => A, S, U>,
};
export declare type PostfixOperator<A, S = string, U = undefined> = {
    readonly type: "postfix",
    readonly parser: AbstractParser<(val: A) => A, S, U>,
};
export declare function buildExpressionParser<A, S = string, U = undefined>(
    opTable: Operator<A, S, U>[][],
    atom: AbstractParser<A, S, U>
): AbstractParser<A, S, U>;

// # token
export declare type LanguageDefObj<S = string, U = undefined> = {
    commentStart?  : string,
    commentEnd?    : string,
    commentLine?   : string,
    nestedComments?: boolean,
    idStart?       : AbstractParser<string, S, U>,
    idLetter?      : AbstractParser<string, S, U>,
    opStart?       : AbstractParser<string, S, U>,
    opLetter?      : AbstractParser<string, S, U>,
    reservedIds?   : string[],
    reservedOps?   : string[],
    caseSensitive? : boolean,
};
export declare class LanguageDef<S = string, U = undefined> {
    constructor(obj: LanguageDefObj<S, U>);
    clone(): LanguageDef<S, U>;
}
export declare type NaturalOrFloat =
      { type: "natural", value: number }
    | { type: "float", value: number };
export declare type TokenParser<S = string, U = undefined> = {
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
export declare function makeTokenParser<S extends Stream<string> = string, U = undefined>(
    def: LanguageDef<AbstractStream<S>, U>
): TokenParser<AbstractStream<S>, U>;
