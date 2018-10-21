export declare type Maybe<A> = { empty: true } | { empty: false, value: A };
declare type AssocValueOf<T> = T[keyof T];

export declare type Unconsed<T, S> = EmptyUnconsed | NonEmptyUnconsed<T, S>;
declare type EmptyUnconsed = { empty: true };
declare type NonEmptyUnconsed<T, S> = { empty: false, head: T, tail: S };

// # from "core"
// ## from "core/utils"
export declare function show<T>(value: T): string;
export declare function unconsString(str: string, unicode: boolean): Unconsed<string, string>;

// ## from "core/pos"
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

// ## from "core/error"
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

// ## from "core/parser"
export declare type ConfigOptions = { tabWidth?: number, unicode?: boolean };
export declare class Config {
    constructor(opts: ConfigOptions);
    static equal(configA: Config, configB: Config): boolean;
    readonly tabWidth: number;
    readonly unicode: boolean;
}
export declare class State<S = string, U = undefined> {
    constructor(config: Config, input: S, pos: SourcePos, userState: U);
    static equal<S, U = undefined>(
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
export declare type Result<A, S, U = undefined> = Failure | Success<A, S, U>;
export declare const Result: Readonly<{
    equal<A, S, U = undefined>(
        resA: Result<A, S, U>,
        resB: Result<A, S, U>,
        valEqual: (valA: A, valB: A) => boolean,
        inputEqual: (inputA: S, inputB: S) => boolean,
        userStateEqual: (userStateA: U, userStateB: U) => boolean
    ): boolean;
    csuc<A, S, U = undefined>(
        err: AbstractParseError,
        val: A,
        state: State<S, U>
    ): Success<A, S, U>;
    cerr(err: AbstractParseError): Failure;
    esuc<A, S, U = undefined>(
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
export declare type Success<A, S, U> = {
   readonly consumed: boolean,
   readonly success: true,
   readonly err: AbstractParseError,
   readonly val: A,
   readonly state: State<S, U>,
};
export declare type ParseResult<A> =
      { success: false, error: AbstractParseError }
    | { success: true, value: A };
export declare abstract class AbstractParser<A, S, U = undefined> {
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
export declare class Parser<A, S, U = undefined> extends AbstractParser<A, S, U> {
    constructor(func: (state: State<S, U>) => Result<A, S, U>);
}
export declare class LazyParser<A, S, U = undefined> extends AbstractParser<A, S, U> {
    constructor(thunk: () => AbstractParser<A, S, U>);
    eval(): Parser<A, S, U>;
}
export declare function isParser<T>(val: T): boolean;
export declare function assertParser<T>(val: T): undefined;

// # from "prim"
export declare type TailRec<A, B> = TailRecCont<A> | TailRecDone<B>;
export declare type TailRecCont<A> = { done: false, value: A };
export declare type TailRecDone<A> = { done: true, value: A };

// # from "expr"
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
export declare type Operator<A, S, U = undefined> =
      InfixOperator<A, S, U>
    | PrefixOperator<A, S, U>
    | PostfixOperator<A, S, U>;
export declare const Operator: Readonly<{
    infix<A, S, U = undefined>(
        parser: AbstractParser<(valA: A, valB: A) => A, S, U>,
        assoc: OperatorAssoc
    ): InfixOperator<A, S, U>,
    prefix<A, S, U = undefined>(
        parser: AbstractParser<(val: A) => A, S, U>
    ): PrefixOperator<A, S, U>,
    postfix<A, S, U = undefined>(
        parser: AbstractParser<(val: A) => A, S, U>
    ): PostfixOperator<A, S, U>,
}>;
export declare type InfixOperator<A, S, U = undefined> = {
    readonly type: "infix",
    readonly parser: AbstractParser<(valA: A, valB: A) => A, S, U>,
    readonly assoc: OperatorAssoc,
};
export declare type PrefixOperator<A, S, U = undefined> = {
    readonly type: "prefix",
    readonly parser: AbstractParser<(val: A) => A, S, U>,
};
export declare type PostfixOperator<A, S, U = undefined> = {
    readonly type: "postfix",
    readonly parser: AbstractParser<(val: A) => A, S, U>,
};

// # from "token"
export declare type LanguageDefObj<S, U = undefined> = {
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
export declare class LanguageDef<S, U = undefined> {
    constructor(obj: LanguageDefObj<S, U>);
    clone(): LanguageDef<S, U>;
}
export declare type NaturalOrFloat =
      { type: "natural", value: number }
    | { type: "float", value: number };
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

declare type GenericParsers<S> = Readonly<{
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

declare type TokenStreamParsers<S, T> = Readonly<{
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

declare type StringStreamParsers<S> = Readonly<{
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

declare type PredefStringStreamParsers = Readonly<{
    // # from "char"
    regexp<U = undefined>(re: RegExp, groupId?: number): AbstractParser<string, string, U>,
}>;

export declare type Stream<S, T> = {
    uncons: (input: S, unicode: boolean) => Unconsed<T, S>,
};

declare type StreamParsers<S, T> =
      GenericParsers<S>
    & TokenStreamParsers<S, T>
    & (T extends string ? StringStreamParsers<S> : unknown);

export declare function make<S, T>(
    stream: Stream<S, T>
): StreamParsers<S, T>;

declare type StringParsers = StreamParsers<string, string> & PredefStringStreamParsers;
export declare const string: StringParsers;

declare type ArrayParsers<T> = StreamParsers<T[], T>;
export declare function array<T>(): ArrayParsers<T>;

export declare function stream<T, S extends { uncons(): Unconsed<T, S> }>(): StreamParsers<S, T>;
