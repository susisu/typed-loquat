"use strict";

const lq = require("loquat")();
lq.use(require("loquat-token"));

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

class InfixOperator extends lq.Operator {
  constructor(parser, assoc) {
    super("infix", parser, assoc);
  }
}

class PrefixOperator extends lq.Operator {
  constructor(parser) {
    super("prefix", parser);
  }
}

class PostfixOperator extends lq.Operator {
  constructor(parser) {
    super("postfix", parser);
  }
}

module.exports = Object.freeze({
  // # core
  // ## core.utils
  show              : lq.show,
  unconsString      : lq.unconsString,
  // ## core.pos
  SourcePos         : lq.SourcePos,
  // ## core.error
  ErrorMessageType  : lq.ErrorMessageType,
  ErrorMessage      : lq.ErrorMessage,
  AbstractParseError: lq.AbstractParseError,
  ParseError        : lq.ParseError,
  LazyParseError    : lq.LazyParseError,
  // ## core.stream
  uncons            : lq.uncons,
  ArrayStream       : lq.ArrayStream,
  // ## core.parser
  Config            : lq.Config,
  State             : lq.State,
  Result            : Result,
  AbstractParser    : lq.AbstractParser,
  Parser            : lq.Parser,
  LazyParser        : lq.LazyParser,
  lazy              : lq.lazy,
  parse             : lq.parse,
  isParser          : lq.isParser,
  assertParser      : lq.assertParser,

  // # prim
  map                 : lq.map,
  fmap                : lq.fmap,
  pure                : lq.pure,
  _return             : lq.return,
  ap                  : lq.ap,
  left                : lq.left,
  right               : lq.right,
  bind                : lq.bind,
  then                : lq.then,
  and                 : lq.and,
  fail                : lq.fail,
  tailRecM            : lq.tailRecM,
  ftailRecM           : lq.ftailRecM,
  unsafeMzero         : lq.mzero,
  mzero               : () => lq.mzero,
  mplus               : lq.mplus,
  or                  : lq.or,
  label               : lq.label,
  labels              : lq.labels,
  hidden              : lq.hidden,
  unexpected          : lq.unexpected,
  tryParse            : lq.tryParse,
  _try                : lq.try,
  lookAhead           : lq.lookAhead,
  reduceMany          : lq.reduceMany,
  many                : lq.many,
  skipMany            : lq.skipMany,
  tokens              : lq.tokens,
  token               : lq.token,
  tokenPrim           : lq.tokenPrim,
  unsafeGetParserState: lq.getParserState,
  getParserState      : () => lq.getParserState,
  setParserState      : lq.setParserState,
  updateParserState   : lq.updateParserState,
  unsafeGetConfig     : lq.getConfig,
  getConfig           : () => lq.getConfig,
  setConfig           : lq.setConfig,
  unsafeGetInput      : lq.getInput,
  getInput            : () => lq.getInput,
  setInput            : lq.setInput,
  unsafeGetPosition   : lq.getPosition,
  getPosition         : () => lq.getPosition,
  setPosition         : lq.setPosition,
  unsafeGetState      : lq.getState,
  getState            : () => lq.getState,
  setState            : lq.setState,

  // # char
  string        : lq.string,
  satisfy       : lq.satisfy,
  oneOf         : lq.oneOf,
  noneOf        : lq.noneOf,
  char          : lq.char,
  unsafeAnyChar : lq.anyChar,
  anyChar       : () => lq.anyChar,
  unsafeSpace   : lq.space,
  space         : () => lq.space,
  unsafeSpaces  : lq.spaces,
  spaces        : () => lq.spaces,
  unsafeNewline : lq.newline,
  newline       : () => lq.newline,
  unsafeTab     : lq.tab,
  tab           : () => lq.tab,
  unsafeUpper   : lq.upper,
  upper         : () => lq.upper,
  unsafeLower   : lq.lower,
  lower         : () => lq.lower,
  unsafeLetter  : lq.letter,
  letter        : () => lq.letter,
  unsafeDigit   : lq.digit,
  digit         : () => lq.digit,
  unsafeAlphaNum: lq.alphaNum,
  alphaNum      : () => lq.alphaNum,
  unsafeOctDigit: lq.octDigit,
  octDigit      : () => lq.octDigit,
  unsafeHexDigit: lq.hexDigit,
  hexDigit      : () => lq.hexDigit,
  manyChars     : lq.manyChars,
  manyChars1    : lq.manyChars1,
  regexp        : lq.regexp,

  // # combinators
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
  unsafeAnyToken: lq.anyToken,
  anyToken      : () => lq.anyToken,
  notFollowedBy : lq.notFollowedBy,
  unsafeEof     : lq.eof,
  eof           : () => lq.eof,
  reduceManyTill: lq.reduceManyTill,
  manyTill      : lq.manyTill,
  skipManyTill  : lq.skipManyTill,

  // # monad
  forever    : lq.forever,
  discard    : lq.discard,
  _void      : lq.void,
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

  // # qo
  qo : lq.qo,
  _do: lq.do,

  // # expr
  OperatorType         : lq.OperatorType,
  OperatorAssoc        : lq.OperatorAssoc,
  InfixOperator        : InfixOperator,
  PrefixOperator       : PrefixOperator,
  PostfixOperator      : PostfixOperator,
  buildExpressionParser: lq.buildExpressionParser,

  // # token
  LanguageDef    : lq.LanguageDef,
  makeTokenParser: lq.makeTokenParser,
});
