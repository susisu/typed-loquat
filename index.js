"use strict";

const core = require("loquat-core")();
const lq = Object.assign(
    {},
    core,
    require("loquat-prim")(core, { sugar: true }),
    require("loquat-char")(core, { sugar: true }),
    require("loquat-combinators")(core, { sugar: true }),
    require("loquat-monad")(core, { sugar: true }),
    require("loquat-expr")(core),
    require("loquat-qo")(core),
    require("loquat-token")(core)
);

const Result = Object.freeze({
    equal(resA, resB, valEqual, inputEqual, userStateEqual) {
        return lq.Result.equal(resA, resB, valEqual, inputEqual, userStateEqual);
    },
    csuc(err, val, state) {
        return lq.Result.csuc(err, val, state);
    },
    cerr(err) {
        return lq.Result.cerr(err);
    },
    esuc(err, val, state) {
        return lq.Result.esuc(err, val, state);
    },
    eerr(err) {
        return lq.Result.eerr(err);
    },
});

const Operator = Object.freeze({
    infix(parser, assoc) {
        return new lq.Operator("infix", parser, assoc);
    },
    prefix(parser) {
        return new lq.Operator("prefix", parser);
    },
    postfix(parser) {
        return new lq.Operator("postfix", parser);
    },
});

const parsers = Object.freeze({
    // # from "core"
    lazy: lq.lazy,

    // # from "prim"
    map              : lq.map,
    fmap             : lq.fmap,
    pure             : lq.pure,
    return           : lq.return,
    ap               : lq.ap,
    left             : lq.left,
    right            : lq.right,
    bind             : lq.bind,
    then             : lq.then,
    and              : lq.and,
    fail             : lq.fail,
    tailRecM         : lq.tailRecM,
    ftailRecM        : lq.ftailRecM,
    mzero            : lq.mzero,
    mplus            : lq.mplus,
    or               : lq.or,
    label            : lq.label,
    labels           : lq.labels,
    hidden           : lq.hidden,
    unexpected       : lq.unexpected,
    tryParse         : lq.tryParse,
    try              : lq.try,
    lookAhead        : lq.lookAhead,
    reduceMany       : lq.reduceMany,
    many             : lq.many,
    skipMany         : lq.skipMany,
    tokens           : lq.tokens,
    token            : lq.token,
    tokenPrim        : lq.tokenPrim,
    getParserState   : lq.getParserState,
    setParserState   : lq.setParserState,
    updateParserState: lq.updateParserState,
    getConfig        : lq.getConfig,
    setConfig        : lq.setConfig,
    getInput         : lq.getInput,
    setInput         : lq.setInput,
    getPosition      : lq.getPosition,
    setPosition      : lq.setPosition,
    getState         : lq.getState,
    setState         : lq.setState,

    // # from "char"
    string    : lq.string,
    satisfy   : lq.satisfy,
    oneOf     : lq.oneOf,
    noneOf    : lq.noneOf,
    char      : lq.char,
    anyChar   : lq.anyChar,
    space     : lq.space,
    spaces    : lq.spaces,
    newline   : lq.newline,
    tab       : lq.tab,
    upper     : lq.upper,
    lower     : lq.lower,
    letter    : lq.letter,
    digit     : lq.digit,
    alphaNum  : lq.alphaNum,
    octDigit  : lq.octDigit,
    hexDigit  : lq.hexDigit,
    manyChars : lq.manyChars,
    manyChars1: lq.manyChars1,
    regexp    : lq.regexp,

    // # from "combinators"
    choice        : lq.choice,
    option        : lq.option,
    optionMaybe   : lq.optionMaybe,
    optional      : lq.optional,
    between       : lq.between,
    many1         : lq.many1,
    skipMany1     : lq.skipMany1,
    sepBy         : lq.sepBy,
    sepBy1        : lq.sepBy1,
    sepEndBy      : lq.sepEndBy,
    sepEndBy1     : lq.sepEndBy1,
    endBy         : lq.endBy,
    endBy1        : lq.endBy1,
    count         : lq.count,
    chainl        : lq.chainl,
    chainl1       : lq.chainl1,
    chainr        : lq.chainr,
    chainr1       : lq.chainr1,
    anyToken      : lq.anyToken,
    notFollowedBy : lq.notFollowedBy,
    eof           : lq.eof,
    reduceManyTill: lq.reduceManyTill,
    manyTill      : lq.manyTill,
    skipManyTill  : lq.skipManyTill,

    // # from "monad"
    forever    : lq.forever,
    discard    : lq.discard,
    void       : lq.void,
    join       : lq.join,
    when       : lq.when,
    unless     : lq.unless,
    liftM      : lq.liftM,
    liftM2     : lq.liftM2,
    liftM3     : lq.liftM3,
    liftM4     : lq.liftM4,
    liftM5     : lq.liftM5,
    ltor       : lq.ltor,
    rtol       : lq.rtol,
    sequence   : lq.sequence,
    sequence_  : lq.sequence_,
    mapM       : lq.mapM,
    mapM_      : lq.mapM_,
    forM       : lq.forM,
    forM_      : lq.forM_,
    filterM    : lq.filterM,
    zipWithM   : lq.zipWithM,
    zipWithM_  : lq.zipWithM_,
    foldM      : lq.foldM,
    foldM_     : lq.foldM_,
    replicateM : lq.replicateM,
    replicateM_: lq.replicateM_,
    guard      : lq.guard,
    msum       : lq.msum,
    mfilter    : lq.mfilter,

    // # from "expr"
    buildExpressionParser: lq.buildExpressionParser,

    // # from "qo"
    qo: lq.qo,
    do: lq.do,

    // # from "token"
    makeTokenParser: lq.makeTokenParser,
});

module.exports = Object.freeze({
    // # from "core"
    // ## from "core/utils",
    show              : lq.show,
    unconsString      : lq.unconsString,
    // ## from "core/stream"
    ArrayStream       : lq.ArrayStream,
    // ## from "core/pos"
    SourcePos         : lq.SourcePos,
    // ## from "core/error"
    ErrorMessageType  : lq.ErrorMessageType,
    ErrorMessage      : lq.ErrorMessage,
    AbstractParseError: lq.AbstractParseError,
    ParseError        : lq.ParseError,
    LazyParseError    : lq.LazyParseError,
    // ## from "core/parser"
    Config            : lq.Config,
    State             : lq.State,
    Result            : Result,
    AbstractParser    : lq.AbstractParser,
    Parser            : lq.Parser,
    LazyParser        : lq.LazyParser,
    parse             : lq.parse,
    isParser          : lq.isParser,
    assertParser      : lq.assertParser,

    // # from "expr"
    OperatorType : lq.OperatorType,
    OperatorAssoc: lq.OperatorAssoc,
    Operator     : Operator,

    // # from "token"
    LanguageDef: lq.LanguageDef,

    // # parsers
    string: () => parsers,
    array : () => parsers,
    stream: () => parsers,
});
