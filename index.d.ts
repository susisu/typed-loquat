export declare type Maybe<A> = { empty: true } | { empty: false, value: A };
declare type AssocValueOf<T> = T[keyof T];

// core
export declare type Unconsed<T, S> = EmptyUnconsed | NonEmptyUnconsed<T, S>;
declare type EmptyUnconsed = { empty: true };
declare type NonEmptyUnconsed<T, S> = { empty: false, head: T, tail: S };

// core.utils
export declare function show<T>(value: T): string;
export declare function unconsString(str: string, unicode: boolean): Unconsed<string, string>;

// core.pos
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

// core.error
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

// core.stream
export declare interface IStream<T> {
    uncons(): Unconsed<T, IStream<T>>;
}
export declare type Stream<T> = string extends T
    ? string | string[] | IStream<string>
    : T[] | IStream<T>;
export declare type Token<S extends Stream<unknown>> =
      S extends string           ? string
    : S extends (infer T)[]      ? T
    : S extends IStream<infer T> ? T
    : never;
export declare type Tail<S extends Stream<unknown>> =
      S extends string           ? string
    : S extends (infer T)[]      ? T[]
    : S extends IStream<unknown> ? UnconsedTail<ReturnType<S["uncons"]>>
    : never;
declare type UnconsedTail<U> = U extends Unconsed<unknown, infer S> ? S : never;
export declare function uncons<S extends Stream<unknown>>(
    input: S,
    unicode: boolean
): Unconsed<Token<S>, Tail<S>>;
export declare class ArrayStream<T> implements IStream<T> {
    constructor(arr: T[], index: number);
    readonly arr: T[];
    readonly index: number;
    uncons(): Unconsed<T, ArrayStream<T>>;
}

// core.parser
export declare type ConfigOptions = { tabWidth: number, unicode: boolean };
export declare class Config {
    constructor(opts: ConfigOptions);
    static equal(configA: Config, configB: Config): boolean;
    readonly tabWidth: number;
    readonly unicode: boolean;
}
export declare class State<S, U> {
    constructor(config: Config, input: S, pos: SourcePos, userState: U);
    static equal<S, U>(
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
    setPosition(pos: SourcePos): State<S, U>
    setUserState<U2>(userState: U2): State<S, U2>;
}
export declare class Result<S, U, A> {
    constructor(
        consumed: boolean,
        success: boolean,
        err: AbstractParseError,
        val?: A | undefined,
        state?: State<S, U> | undefined
    );
    static equal<S, U, A>(
        resA: Result<S, U, A>,
        resB: Result<S, U, A>,
        valEqual: (valA: A, valB: A) => boolean,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    static csuc<S, U, A>(err: AbstractParseError, val: A, state: State<S, U>): Result<S, U, A>;
    static cerr<S, U, A>(err: AbstractParseError): Result<S, U, A>;
    static esuc<S, U, A>(err: AbstractParseError, val: A, state: State<S, U>): Result<S, U, A>;
    static eerr<S, U, A>(err: AbstractParseError): Result<S, U, A>;
    readonly consumed: boolean;
    readonly success: boolean;
    readonly err: AbstractParseError;
    readonly val: A | undefined;
    readonly state: State<S, U> | undefined;
}
export declare type ParseResult<A> =
      { success: true, value: A }
    | { success: false, err: AbstractParseError };
export declare abstract class AbstractParser<S, U, A> {
    run(state: State<S, U>): Result<S, U, A>;
    parse(name: string, input: S, userState: U, opts: ConfigOptions): ParseResult<A>;
    map<B>(func: (val: A) => B): AbstractParser<S, U, B>;
    return<B>(val: B): AbstractParser<S, U, B>;
    ap(parser: MethodApArg<S, U, A>): MethodApRet<S, U, A>;
    left<B>(parser: AbstractParser<S, U, B>): AbstractParser<S, U, A>;
    skip<B>(parser: AbstractParser<S, U, B>): AbstractParser<S, U, A>;
    right<B>(parser: AbstractParser<S, U, B>): AbstractParser<S, U, B>;
    bind<B>(func: (val: A) => AbstractParser<S, U, B>): AbstractParser<S, U, B>;
    and<B>(parser: AbstractParser<S, U, B>): AbstractParser<S, U, B>;
    fail(msgStr: string): AbstractParser<S, U, never>;
    done(): AbstractParser<S, U, TailRecDone<A>>;
    cont(): AbstractParser<S, U, TailRecCont<A>>;
    or<B>(parser: AbstractParser<S, U, B>): AbstractParser<S, U, A | B>;
    label(labelStr: string): AbstractParser<S, U, A>;
    hidden(): AbstractParser<S, U, A>;
    try(): AbstractParser<S, U, A>;
    lookAhead(): AbstractParser<S, U, A>;
    reduceMany<B>(callback: (accum: B, val: A) => B, initVal: B): AbstractParser<S, U, B>;
    many(): AbstractParser<S, U, A[]>;
    skipMany(): AbstractParser<S, U, undefined>;
    skipMany<B>(parser: AbstractParser<S, U, B>): AbstractParser<S, U, A>;
    manyChars(): MethodManyChaRet<S, U, A>;
    manyChars1(): MethodManyChaRet<S, U, A>;
    option<B>(val: B): AbstractParser<S, U, A | B>;
    optionMaybe(): AbstractParser<S, U, Maybe<A>>;
    optional(): AbstractParser<S, U, undefined>;
    between<B, C>(
        open: AbstractParser<S, U, B>,
        close: AbstractParser<S, U, C>
    ): AbstractParser<S, U, A>;
    many1(): AbstractParser<S, U, A[]>;
    skipMany1(): AbstractParser<S, U, undefined>;
    skipMany1<B>(parser: AbstractParser<S, U, B>): AbstractParser<S, U, A>;
    sepBy<B>(sep: AbstractParser<S, U, B>): AbstractParser<S, U, A[]>;
    sepBy1<B>(sep: AbstractParser<S, U, B>): AbstractParser<S, U, A[]>;
    sepEndBy<B>(sep: AbstractParser<S, U, B>): AbstractParser<S, U, A[]>;
    sepEndBy1<B>(sep: AbstractParser<S, U, B>): AbstractParser<S, U, A[]>;
    endBy<B>(sep: AbstractParser<S, U, B>): AbstractParser<S, U, A[]>;
    endBy1<B>(sep: AbstractParser<S, U, B>): AbstractParser<S, U, A[]>;
    count(num: number): AbstractParser<S, U, A[]>;
    notFollowedBy(): AbstractParser<S, U, undefined>;
    notFollowedBy<B>(parser: AbstractParser<S, U, B>): AbstractParser<S, U, A>;
    reduceManyTill<B, C>(
        end: AbstractParser<S, U, B>,
        callback: (accum: C, val: A) => C,
        initVal: C
    ): AbstractParser<S, U, C>;
    manyTill<B>(end: AbstractParser<S, U, B>): AbstractParser<S, U, A[]>;
    skipManyTill<B>(end: AbstractParser<S, U, B>): AbstractParser<S, U, undefined>;
    skipManyTill<B, C>(
        parser: AbstractParser<S, U, B>,
        end: AbstractParser<S, U, C>
    ): AbstractParser<S, U, A>;
    forever(): AbstractParser<S, U, never>;
    discard(): AbstractParser<S, U, undefined>;
    void(): AbstractParser<S, U, undefined>;
    join(): MethodJoinRet<S, U, A>;
    when(cond: boolean): AbstractParser<S, U, A>;
    unless(cond: boolean): AbstractParser<S, U, A>;
    filter(test: (val: A) => boolean): AbstractParser<S, U, A>;
}
declare type AsFunction<T> = T extends (arg: infer A) => infer R
    ? (arg: A) => R
    : unknown;
declare type MethodApArg<S, U, A> = AsFunction<A> extends (arg: infer B) => unknown
    ? AbstractParser<S, U, B>
    : never;
declare type MethodApRet<S, U, A> = AsFunction<A> extends (arg: never) => infer B
    ? AbstractParser<S, U, B>
    : unknown;
declare type MethodManyChaRet<S, U, A> = A extends string
    ? AbstractParser<S, U, string>
    : unknown;
declare type MethodJoinRet<S, U, A> = A extends AbstractParser<S, U, infer B>
    ? AbstractParser<S, U, B>
    : unknown;
export declare class Parser<S, U, A> extends AbstractParser<S, U, A> {
    constructor(func: (state: State<S, U>) => Result<S, U, A>);
}
export declare class LazyParser<S, U, A> extends AbstractParser<S, U, A> {
    constructor(thunk: () => AbstractParser<S, U, A>);
    eval(): Parser<S, U, A>;
}
export declare function lazy<S, U, A>(thunk: () => AbstractParser<S, U, A>): LazyParser<S, U, A>;
export declare function parse<S, U, A>(
    parser: AbstractParser<S, U, A>,
    name: string,
    input: S,
    userState: U,
    opts: ConfigOptions
): ParseResult<A>;
export declare function isParser(val: unknown): boolean;
export declare function assertParser(val: unknown): boolean;

// prim
export declare function map<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    func: (val: A) => B
): AbstractParser<S, U, B>;
export declare function fmap<A, B>(
    func: (val: A) => B
): <S, U>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, B>;
export declare function pure<S, U, A>(val: A): AbstractParser<S, U, A>;
export declare function ap<S, U, A, B>(
    parserA: AbstractParser<S, U, (val: A) => B>,
    parserB: AbstractParser<S, U, A>
): AbstractParser<S, U, B>;
export declare function left<S, U, A, B>(
    parserA: AbstractParser<S, U, A>,
    parserB: AbstractParser<S, U, B>
): AbstractParser<S, U, A>;
export declare function right<S, U, A, B>(
    parserA: AbstractParser<S, U, A>,
    parserB: AbstractParser<S, U, B>
): AbstractParser<S, U, B>;
export declare function bind<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    func: (val: A) => AbstractParser<S, U, B>
): AbstractParser<S, U, B>;
export declare function then<S, U, A, B>(
    parserA: AbstractParser<S, U, A>,
    parserB: AbstractParser<S, U, B>
): AbstractParser<S, U, B>;
export declare function fail<S, U>(msgStr: string): AbstractParser<S, U, never>;
export declare type TailRec<A, B> = TailRecCont<A> | TailRecDone<B>;
export declare type TailRecCont<A> = { done: false, value: A };
export declare type TailRecDone<A> = { done: true, value: A };
export declare function tailRecM<S, U, A, B>(
    initVal: A,
    func: (val: A) => AbstractParser<S, U, TailRec<A, B>>
): AbstractParser<S, U, B>;
export declare function ftailRecM<S, U, A, B>(
    func: (val: A) => AbstractParser<S, U, TailRec<A, B>>
): (initVal: A) => AbstractParser<S, U, B>;
// TODO
export declare function mzero<S, U>(): AbstractParser<S, U, never>;
export declare function mplus<S, U, A, B>(
    parserA: AbstractParser<S, U, A>,
    parserB: AbstractParser<S, U, B>
): AbstractParser<S, U, A | B>;
export declare function label<S, U, A>(
    parser: AbstractParser<S, U, A>,
    labelStr: string
): AbstractParser<S, U, A>;
export declare function labels<S, U, A>(
    parser: AbstractParser<S, U, A>,
    labelStrs: string[]
): AbstractParser<S, U, A>;
export declare function hidden<S, U, A>(parser: AbstractParser<S, U, A>): AbstractParser<S, U, A>;
export declare function unexpected<S, U>(msgStr: string): AbstractParser<S, U, never>;
export declare function tryParse<S, U, A>(parser: AbstractParser<S, U, A>): AbstractParser<S, U, A>;
export declare function lookAhead<S, U, A>(
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, A>;
export declare function reduceMany<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    callback: (accum: B, val: A) => B,
    initVal: B
): AbstractParser<S, U, B>;
export declare function many<S, U, A>(parser: AbstractParser<S, U, A>): AbstractParser<S, U, A[]>;
export declare function skipMany<S, U, A>(
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, undefined>;
export declare function tokens<S extends Stream<unknown>, U>(
    expectTokens: Token<S>[],
    tokenEqual: (tokenA: Token<S>, tokenB: Token<S>) => boolean,
    tokensToString: (tokens: Token<S>[]) => string,
    calcNextPos: (pos: SourcePos, tokens: Token<S>[], config: Config) => SourcePos
): AbstractParser<S, U, Token<S>[]>;
export declare function token<S extends Stream<unknown>, U, A>(
    calcValue: (token: Token<S>, config: Config) => Maybe<A>,
    tokenToString: (token: Token<S>) => string,
    calcPos: (token: Token<S>, config: Config) => SourcePos
): AbstractParser<S, U, A>;
export declare function tokenPrim<S extends Stream<unknown>, U, A>(
    calcValue: (token: Token<S>, config: Config) => Maybe<A>,
    tokenToString: (token: Token<S>) => string,
    calcNextPos: (pos: SourcePos, head: Token<S>, tail: Tail<S>, config: Config) => SourcePos,
    calcNextUserState?: (
        userState: U,
        pos: SourcePos,
        head: Token<S>,
        tail: Tail<S>,
        config: Config
    ) => U
): AbstractParser<S, U, A>;
// TODO
export declare function getParserState<S, U>(): AbstractParser<S, U, State<S, U>>;
export declare function setParserState<S, U>(state: State<S, U>): AbstractParser<S, U, State<S, U>>;
export declare function updateParserState<S, U>(
    func: (state: State<S, U>) => State<S, U>
): AbstractParser<S, U, State<S, U>>;
// TODO
export declare function getConfig<S, U>(): AbstractParser<S, U, Config>;
export declare function setConfig<S, U>(config: Config): AbstractParser<S, U, undefined>;
// TODO
export declare function getInput<S, U>(): AbstractParser<S, U, S>;
export declare function setInput<S, U>(input: S): AbstractParser<S, U, undefined>;
// TODO
export declare function getPosition<S, U>(): AbstractParser<S, U, SourcePos>;
export declare function setPosition<S, U>(pos: SourcePos): AbstractParser<S, U, undefined>;
// TODO
export declare function getState<S, U>(): AbstractParser<S, U, U>;
export declare function setState<S, U>(userState: U): AbstractParser<S, U, undefined>;

// char
export declare function string<S extends Stream<string>, U>(
    str: string
): AbstractParser<S, U, string>;
export declare function satisfy<S extends Stream<string>, U>(
    test: (char: string, config: Config) => boolean
): AbstractParser<S, U, string>;
export declare function oneOf<S extends Stream<string>, U>(
    str: string
): AbstractParser<S, U, string>;
export declare function noneOf<S extends Stream<string>, U>(
    str: string
): AbstractParser<S, U, string>;
export declare function char<S extends Stream<string>, U>(
    expectChar: string
): AbstractParser<S, U, string>;
// TODO
export declare function anyChar<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function space<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function spaces<S extends Stream<string>, U>(): AbstractParser<S, U, undefined>;
// TODO
export declare function newline<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function tab<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function upper<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function lower<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function letter<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function digit<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function alphaNum<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function octDigit<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
// TODO
export declare function hexDigit<S extends Stream<string>, U>(): AbstractParser<S, U, string>;
export declare function manyChars<S, U>(
    parser: AbstractParser<S, U, string>
): AbstractParser<S, U, string>;
export declare function manyChars1<S, U>(
    parser: AbstractParser<S, U, string>
): AbstractParser<S, U, string>;
export declare function regexp<U>(re: RegExp, groupId?: number): AbstractParser<string, U, string>;

// combinators
export declare function choice<S, U, A>(parsers: AbstractParser<S, U, A>[]): AbstractParser<S, U, A>;
export declare function option<S, U, A, B>(
    val: B,
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, A | B>;
export declare function optionMaybe<S, U, A>(
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, Maybe<A>>;
export declare function optional<S, U, A>(
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, undefined>;
export declare function between<S, U, A, B, C>(
    open: AbstractParser<S, U, A>,
    close: AbstractParser<S, U, B>,
    parser: AbstractParser<S, U, C>
): AbstractParser<S, U, C>;
export declare function many1<S, U, A>(parser: AbstractParser<S, U, A>): AbstractParser<S, U, A[]>;
export declare function skipMany1<S, U, A>(
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, undefined>;
export declare function sepBy<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    sep: AbstractParser<S, U, B>
): AbstractParser<S, U, A[]>;
export declare function sepBy1<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    sep: AbstractParser<S, U, B>
): AbstractParser<S, U, A[]>;
export declare function sepEndBy<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    sep: AbstractParser<S, U, B>
): AbstractParser<S, U, A[]>;
export declare function sepEndBy1<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    sep: AbstractParser<S, U, B>
): AbstractParser<S, U, A[]>;
export declare function endBy<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    sep: AbstractParser<S, U, B>
): AbstractParser<S, U, A[]>;
export declare function endBy1<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    sep: AbstractParser<S, U, B>
): AbstractParser<S, U, A[]>;
export declare function count<S, U, A>(
    num: number,
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, A[]>;
export declare function chainl<S, U, A>(
    term: AbstractParser<S, U, A>,
    op: AbstractParser<S, U, (accum: A, val: A) => A>,
    defaultVal: A
): AbstractParser<S, U, A>;
export declare function chainl1<S, U, A>(
    term: AbstractParser<S, U, A>,
    op: AbstractParser<S, U, (accum: A, val: A) => A>,
): AbstractParser<S, U, A>;
export declare function chainr<S, U, A>(
    term: AbstractParser<S, U, A>,
    op: AbstractParser<S, U, (val: A, accum: A) => A>,
    defaultVal: A
): AbstractParser<S, U, A>;
export declare function chainr1<S, U, A>(
    term: AbstractParser<S, U, A>,
    op: AbstractParser<S, U, (val: A, accum: A) => A>,
): AbstractParser<S, U, A>;
// TODO
export declare function anyToken<S extends Stream<unknown>, U>(): AbstractParser<S, U, Token<S>>;
export declare function notFollowedBy<S, U, A>(
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, undefined>;
// TODO
export declare function eof<S extends Stream<unknown>, U>(): AbstractParser<S, U, undefined>;
export declare function reduceManyTill<S, U, A, B, C>(
    parser: AbstractParser<S, U, A>,
    end: AbstractParser<S, U, B>,
    callback: (accum: C, val: A) => C,
    initVal: C
): AbstractParser<S, U, C>
export declare function manyTill<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    end: AbstractParser<S, U, B>
): AbstractParser<S, U, A[]>;
export declare function skipManyTill<S, U, A, B>(
    parser: AbstractParser<S, U, A>,
    end: AbstractParser<S, U, B>
): AbstractParser<S, U, undefined>;

// monad
export declare function forever<S, U, A>(
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, never>;
export declare function discard<S, U, A>(
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, undefined>;
export declare function join<S, U, A>(
    parser: AbstractParser<S, U, AbstractParser<S, U, A>>
): AbstractParser<S, U, A>;
export declare function when<S, U>(
    cond: boolean,
    parser: AbstractParser<S, U, undefined>
): AbstractParser<S, U, undefined>;
export declare function unless<S, U>(
    cond: boolean,
    parser: AbstractParser<S, U, undefined>
): AbstractParser<S, U, undefined>;
export declare function liftM<A, B>(
    func: (val: A) => B
): <S, U>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, B>;
export declare function liftM2<A, B, C>(
    func: (valA: A, valB: B) => C
): <S, U>(
    parserA: AbstractParser<S, U, A>,
    parserB: AbstractParser<S, U, B>
) => AbstractParser<S, U, C>;
export declare function liftM3<A, B, C, D>(
    func: (valA: A, valB: B, valC: C) => D
): <S, U>(
    parserA: AbstractParser<S, U, A>,
    parserB: AbstractParser<S, U, B>,
    parserC: AbstractParser<S, U, C>
) => AbstractParser<S, U, D>;
export declare function liftM4<A, B, C, D, E>(
    func: (valA: A, valB: B, valC: C, valD: D) => E
): <S, U>(
    parserA: AbstractParser<S, U, A>,
    parserB: AbstractParser<S, U, B>,
    parserC: AbstractParser<S, U, C>,
    parserD: AbstractParser<S, U, D>
) => AbstractParser<S, U, E>;
export declare function liftM5<A, B, C, D, E, F>(
    func: (valA: A, valB: B, valC: C, valD: D, valE: E) => F
): <S, U>(
    parserA: AbstractParser<S, U, A>,
    parserB: AbstractParser<S, U, B>,
    parserC: AbstractParser<S, U, C>,
    parserD: AbstractParser<S, U, D>,
    parserE: AbstractParser<S, U, E>
) => AbstractParser<S, U, F>;
export declare function ltor<S, U, A, B, C>(
    funcA: (val: A) => AbstractParser<S, U, B>,
    funcB: (val: B) => AbstractParser<S, U, C>
): (val: A) => AbstractParser<S, U, C>;
export declare function rtol<S, U, A, B, C>(
    funcA: (val: B) => AbstractParser<S, U, C>,
    funcB: (val: A) => AbstractParser<S, U, B>
): (val: A) => AbstractParser<S, U, C>;
export declare function sequence<S, U, A>(
    parsers: AbstractParser<S, U, A>[]
): AbstractParser<S, U, A[]>;
export declare function sequence_<S, U, A>(
    parsers: AbstractParser<S, U, A>[]
): AbstractParser<S, U, undefined>;
export declare function mapM<S, U, A, B>(
    func: (val: A) => AbstractParser<S, U, B>,
    arr: A[]
): AbstractParser<S, U, B[]>;
export declare function mapM_<S, U, A, B>(
    func: (val: A) => AbstractParser<S, U, B>,
    arr: A[]
): AbstractParser<S, U, undefined>;
export declare function forM<S, U, A, B>(
    arr: A[],
    func: (val: A) => AbstractParser<S, U, B>
): AbstractParser<S, U, B[]>;
export declare function forM_<S, U, A, B>(
    arr: A[],
    func: (val: A) => AbstractParser<S, U, B>
): AbstractParser<S, U, undefined>;
export declare function filterM<S, U, A>(
    test: (val: A) => AbstractParser<S, U, boolean>,
    arr: A[]
): AbstractParser<S, U, A>;
export declare function zipWithM<S, U, A, B, C>(
    func: (valA: A, valB: B) => AbstractParser<S, U, C>,
    arrA: A[],
    arrB: B[]
): AbstractParser<S, U, C[]>;
export declare function zipWithM_<S, U, A, B, C>(
    func: (valA: A, valB: B) => AbstractParser<S, U, C>,
    arrA: A[],
    arrB: B[]
): AbstractParser<S, U, undefined>;
export declare function foldM<S, U, A, B>(
    func: (accum: B, val: A) => AbstractParser<S, U, B>,
    initVal: B,
    arr: A[]
): AbstractParser<S, U, B>;
export declare function foldM_<S, U, A, B>(
    func: (accum: B, val: A) => AbstractParser<S, U, B>,
    initVal: B,
    arr: A[]
): AbstractParser<S, U, undefined>;
export declare function replicateM<S, U, A>(
    num: number,
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, A[]>;
export declare function replicateM_<S, U, A>(
    num: number,
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, undefined>;
export declare function guard<S, U>(cond: boolean): AbstractParser<S, U, undefined>;
export declare function msum<S, U, A>(parsers: AbstractParser<S, U, A>[]): AbstractParser<S, U, A>;
export declare function mfilter<S, U, A>(
    test: (val: A) => boolean,
    parser: AbstractParser<S, U, A>
): AbstractParser<S, U, A>;

// qo
export declare function qo<S, U>(
    genFunc: () => IterableIterator<AbstractParser<S, U, any>>
): AbstractParser<S, U, any>;

// expr
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
declare type OperatorFunc<T extends OperatorType, A> = T extends "infix"
    ? (valA: A, valB: A) => A
    : (val: A) => A;
export declare class Operator<T extends OperatorType, S, U, A> {
    constructor(type: T, parser: AbstractParser<S, U, OperatorFunc<T, A>>, assoc?: OperatorAssoc);
    readonly type: OperatorType;
    readonly parser: AbstractParser<S, U, OperatorFunc<T, A>>;
    readonly assoc: OperatorAssoc | undefined;
}
export declare function buildExpressionParser<S, U, A>(
    opTable: Operator<OperatorType, S, U, A>[][],
    atom: AbstractParser<S, U, A>
): AbstractParser<S, U, A>

// token
export declare type LanguageDefObj<S, U> = {
    commentStart?  : string,
    commentEnd?    : string,
    commentLine?   : string,
    nestedComments?: boolean,
    idStart?       : AbstractParser<S, U, string>,
    idLetter?      : AbstractParser<S, U, string>,
    opStart?       : AbstractParser<S, U, string>,
    opLetter?      : AbstractParser<S, U, string>,
    reservedIds?   : string[],
    reservedOps?   : string[],
    caseSensitive? : boolean,
};
export declare class LanguageDef<S, U> {
    constructor(obj: LanguageDefObj<S, U>);
    clone(): LanguageDef<S, U>;
}
export declare type NaturalOrFloat =
      { type: "natural", value: number }
    | { type: "float", value: number };
export declare type TokenParser<S, U> = {
    whiteSpace    : AbstractParser<S, U, undefined>,
    lexeme        : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A>,
    symbol        : (name: string) => AbstractParser<S, U, string>,
    parens        : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A>,
    braces        : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A>,
    angles        : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A>,
    brackets      : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A>,
    semi          : AbstractParser<S, U, string>,
    comma         : AbstractParser<S, U, string>,
    colon         : AbstractParser<S, U, string>,
    dot           : AbstractParser<S, U, string>,
    semiSep       : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A[]>,
    semiSep1      : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A[]>,
    commaSep      : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A[]>,
    commaSep1     : <A>(parser: AbstractParser<S, U, A>) => AbstractParser<S, U, A[]>,
    decimal       : AbstractParser<S, U, number>,
    hexadecimal   : AbstractParser<S, U, number>,
    octal         : AbstractParser<S, U, number>,
    natural       : AbstractParser<S, U, number>,
    integer       : AbstractParser<S, U, number>,
    float         : AbstractParser<S, U, number>,
    naturalOrFloat: AbstractParser<S, U, NaturalOrFloat>,
    charLiteral   : AbstractParser<S, U, string>,
    stringLiteral : AbstractParser<S, U, string>,
    identifier    : AbstractParser<S, U, string>,
    reserved      : (name: string) => AbstractParser<S, U, undefined>,
    operator      : AbstractParser<S, U, string>,
    reservedOp    : (name: string) => AbstractParser<S, U, undefined>,
};
export declare function makeTokenParser<S extends Stream<string>, U>(
    def: LanguageDef<S, U>
): TokenParser<S, U>;
