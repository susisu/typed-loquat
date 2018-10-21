"use strict";

const _core        = require("loquat-core");
const _prim        = require("loquat-prim/lib/prim.js");
const _char        = require("loquat-char/lib/char.js");
const _combinators = require("loquat-combinators/lib/combinators.js");
const _monad       = require("loquat-monad/lib/monad.js");
const _expr        = require("loquat-expr/lib/expr.js");
const _qo          = require("loquat-qo/lib/qo.js");
const _language    = require("loquat-token/lib/language.js");
const _token       = require("loquat-token/lib/token.js");

const _primSugar        = require("loquat-prim/lib/sugar.js");
const _charSugar        = require("loquat-char/lib/sugar.js");
const _combinatorsSugar = require("loquat-combinators/lib/sugar.js");
const _monadSugar       = require("loquat-monad/lib/sugar.js");

const coreUtils  = require("loquat-core/lib/utils.js")();
const coreStream = require("loquat-core/lib/stream.js")(coreUtils);

function make(stream) {
  const core        = Object.assign({}, _core(), { uncons: stream.uncons });
  const prim        = _prim(core);
  const char        = _char(core, prim);
  const combinators = _combinators(core, prim);
  const monad       = _monad(core, prim);
  const expr        = _expr(core, prim, combinators);
  const qo          = _qo(core);
  const language    = _language();
  const token       = _token(core, prim, char, combinators);

  core.extendParser(_primSugar(core, prim));
  core.extendParser(_charSugar(core, char));
  core.extendParser(_combinatorsSugar(core, prim, combinators));
  core.extendParser(_monadSugar(core, monad));

  const Result = Object.freeze({
    equal(resA, resB, valEqual, inputEqual, userStateEqual) {
      return core.Result.equal(resA, resB, valEqual, inputEqual, userStateEqual);
    },
    csuc(err, val, state) {
      return core.Result.csuc(err, val, state);
    },
    cerr(err) {
      return core.Result.cerr(err);
    },
    esuc(err, val, state) {
      return core.Result.esuc(err, val, state);
    },
    eerr(err) {
      return core.Result.eerr(err);
    },
  });

  const Operator = Object.freeze({
    infix(parser, assoc) {
      return new expr.Operator("infix", parser, assoc);
    },
    prefix(parser) {
      return new expr.Operator("prefix", parser);
    },
    postfix(parser) {
      return new expr.Operator("postfix", parser);
    },
  });

  return Object.freeze({
    // # from "core"
    // ## from "core/pos"
    SourcePos         : core.SourcePos,
    // ## from "core/error"
    ErrorMessageType  : core.ErrorMessageType,
    ErrorMessage      : core.ErrorMessage,
    AbstractParseError: core.AbstractParseError,
    ParseError        : core.ParseError,
    LazyParseError    : core.LazyParseError,
    // ## from "core/stream"
    ArrayStream       : core.ArrayStream,
    // ## from "core/parser"
    Config            : core.Config,
    State             : core.State,
    Result            : Result,
    AbstractParser    : core.AbstractParser,
    Parser            : core.Parser,
    LazyParser        : core.LazyParser,
    lazy              : core.lazy,
    parse             : core.parse,
    isParser          : core.isParser,
    assertParser      : core.assertParser,

    // # from "prim"
    map                 : prim.map,
    fmap                : prim.fmap,
    pure                : prim.pure,
    return              : prim.return,
    ap                  : prim.ap,
    left                : prim.left,
    right               : prim.right,
    bind                : prim.bind,
    then                : prim.then,
    and                 : prim.and,
    fail                : prim.fail,
    tailRecM            : prim.tailRecM,
    ftailRecM           : prim.ftailRecM,
    unsafeMzero         : prim.mzero,
    mzero               : () => prim.mzero,
    mplus               : prim.mplus,
    or                  : prim.or,
    label               : prim.label,
    labels              : prim.labels,
    hidden              : prim.hidden,
    unexpected          : prim.unexpected,
    tryParse            : prim.tryParse,
    try                 : prim.try,
    lookAhead           : prim.lookAhead,
    reduceMany          : prim.reduceMany,
    many                : prim.many,
    skipMany            : prim.skipMany,
    tokens              : prim.tokens,
    token               : prim.token,
    tokenPrim           : prim.tokenPrim,
    unsafeGetParserState: prim.getParserState,
    getParserState      : () => prim.getParserState,
    setParserState      : prim.setParserState,
    updateParserState   : prim.updateParserState,
    unsafeGetConfig     : prim.getConfig,
    getConfig           : () => prim.getConfig,
    setConfig           : prim.setConfig,
    unsafeGetInput      : prim.getInput,
    getInput            : () => prim.getInput,
    setInput            : prim.setInput,
    unsafeGetPosition   : prim.getPosition,
    getPosition         : () => prim.getPosition,
    setPosition         : prim.setPosition,
    unsafeGetState      : prim.getState,
    getState            : () => prim.getState,
    setState            : prim.setState,

    // # from "char"
    string        : char.string,
    satisfy       : char.satisfy,
    oneOf         : char.oneOf,
    noneOf        : char.noneOf,
    char          : char.char,
    unsafeAnyChar : char.anyChar,
    anyChar       : () => char.anyChar,
    unsafeSpace   : char.space,
    space         : () => char.space,
    unsafeSpaces  : char.spaces,
    spaces        : () => char.spaces,
    unsafeNewline : char.newline,
    newline       : () => char.newline,
    unsafeTab     : char.tab,
    tab           : () => char.tab,
    unsafeUpper   : char.upper,
    upper         : () => char.upper,
    unsafeLower   : char.lower,
    lower         : () => char.lower,
    unsafeLetter  : char.letter,
    letter        : () => char.letter,
    unsafeDigit   : char.digit,
    digit         : () => char.digit,
    unsafeAlphaNum: char.alphaNum,
    alphaNum      : () => char.alphaNum,
    unsafeOctDigit: char.octDigit,
    octDigit      : () => char.octDigit,
    unsafeHexDigit: char.hexDigit,
    hexDigit      : () => char.hexDigit,
    manyChars     : char.manyChars,
    manyChars1    : char.manyChars1,
    regexp        : char.regexp,

    // # from "combinators"
    choice        : combinators.choice,
    option        : combinators.option,
    optionMaybe   : combinators.optionMaybe,
    optional      : combinators.optional,
    between       : combinators.between,
    many1         : combinators.many1,
    skipMany1     : combinators.skipMany1,
    sepBy         : combinators.sepBy,
    sepBy1        : combinators.sepBy1,
    sepEndBy      : combinators.sepEndBy,
    sepEndBy1     : combinators.sepEndBy1,
    endBy         : combinators.endBy,
    endBy1        : combinators.endBy1,
    count         : combinators.count,
    chainl        : combinators.chainl,
    chainl1       : combinators.chainl1,
    chainr        : combinators.chainr,
    chainr1       : combinators.chainr1,
    unsafeAnyToken: combinators.anyToken,
    anyToken      : () => combinators.anyToken,
    notFollowedBy : combinators.notFollowedBy,
    unsafeEof     : combinators.eof,
    eof           : () => combinators.eof,
    reduceManyTill: combinators.reduceManyTill,
    manyTill      : combinators.manyTill,
    skipManyTill  : combinators.skipManyTill,

    // # from "monad"
    forever    : monad.forever,
    discard    : monad.discard,
    void       : monad.void,
    join       : monad.join,
    when       : monad.when,
    unless     : monad.unless,
    liftM      : monad.liftM,
    liftM2     : monad.liftM2,
    liftM3     : monad.liftM3,
    liftM4     : monad.liftM4,
    liftM5     : monad.liftM5,
    ltor       : monad.ltor,
    rtol       : monad.rtol,
    sequence   : monad.sequence,
    sequence_  : monad.sequence_,
    mapM       : monad.mapM,
    mapM_      : monad.mapM_,
    forM       : monad.forM,
    forM_      : monad.forM_,
    filterM    : monad.filterM,
    zipWithM   : monad.zipWithM,
    zipWithM_  : monad.zipWithM_,
    foldM      : monad.foldM,
    foldM_     : monad.foldM_,
    replicateM : monad.replicateM,
    replicateM_: monad.replicateM_,
    guard      : monad.guard,
    msum       : monad.msum,
    mfilter    : monad.mfilter,

    // # from "expr"
    OperatorType         : expr.OperatorType,
    OperatorAssoc        : expr.OperatorAssoc,
    Operator             : Operator,
    buildExpressionParser: expr.buildExpressionParser,

    // # from "qo"
    qo: qo.qo,
    do: qo.do,

    // # from "token"
    LanguageDef    : language.LanguageDef,
    makeTokenParser: token.makeTokenParser,
  });
}

const string = make({ uncons: coreUtils.unconsString });

function array() {
  return make({ uncons: coreStream.uncons });
}

function stream() {
  return make({ uncons: coreStream.uncons });
}

module.exports = Object.freeze({
  // # from "core"
  // ## from "core/utils",
  show        : coreUtils.show,
  unconsString: coreUtils.unconsString,
  // ## from "core/stream"
  ArrayStream : coreStream.ArrayStream,

  // functors
  make  : make,
  string: string,
  array : array,
  stream: stream,
});
