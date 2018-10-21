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

function make(stream) {
  const {
    show,
    ErrorMessage,
    ErrorMessageType,
    ParseError,
    State,
    Result,
    Parser,
    label,
    skipMany,
  } = lq;

  function tokens(expectTokens, tokenEqual, tokensToString, calcNextPos) {
    function eofError(pos) {
      return new ParseError(
        pos,
        [
          new ErrorMessage(ErrorMessageType.SYSTEM_UNEXPECT, ""),
          new ErrorMessage(ErrorMessageType.EXPECT, tokensToString(expectTokens))
        ]
      );
    }
    function expectError(pos, token) {
      return new ParseError(
        pos,
        [
          new ErrorMessage(ErrorMessageType.SYSTEM_UNEXPECT, tokensToString([token])),
          new ErrorMessage(ErrorMessageType.EXPECT, tokensToString(expectTokens))
        ]
      );
    }
    return new Parser(state => {
      const len = expectTokens.length;
      if (len === 0) {
        return Result.esuc(ParseError.unknown(state.pos), [], state);
      }
      let rest = state.input;
      for (let i = 0; i < len; i++) {
        const unconsed = stream.uncons(rest, state.config.unicode);
        if (unconsed.empty) {
          return i === 0
            ? Result.eerr(eofError(state.pos))
            : Result.cerr(eofError(state.pos));
        }
        else {
          if (tokenEqual(expectTokens[i], unconsed.head)) {
            rest = unconsed.tail;
          }
          else {
            return i === 0
              ? Result.eerr(expectError(state.pos, unconsed.head))
              : Result.cerr(expectError(state.pos, unconsed.head));
          }
        }
      }
      const newPos = calcNextPos(state.pos, expectTokens, state.config);
      return Result.csuc(
        ParseError.unknown(newPos),
        expectTokens,
        new State(state.config, rest, newPos, state.userState)
      );
    });
  }

  function token(calcValue, tokenToString, calcPos) {
    function calcNextPos(pos, token, rest, config) {
      const unconsed = stream.uncons(rest, config.unicode);
      return unconsed.empty
        ? calcPos(token, config)
        : calcPos(unconsed.head, config);
    }
    return tokenPrim(calcValue, tokenToString, calcNextPos);
  }

  function tokenPrim(calcValue, tokenToString, calcNextPos, calcNextUserState) {
    function systemUnexpectError(pos, str) {
      return new ParseError(
        pos,
        [new ErrorMessage(ErrorMessageType.SYSTEM_UNEXPECT, str)]
      );
    }
    return new Parser(state => {
      const unconsed = stream.uncons(state.input, state.config.unicode);
      if (unconsed.empty) {
        return Result.eerr(systemUnexpectError(state.pos, ""));
      }
      else {
        const maybeVal = calcValue(unconsed.head, state.config);
        if (maybeVal.empty) {
          return Result.eerr(systemUnexpectError(state.pos, tokenToString(unconsed.head)));
        }
        else {
          const newPos = calcNextPos(state.pos, unconsed.head, unconsed.tail, state.config);
          const newUserState = calcNextUserState === undefined
            ? state.userState
            : calcNextUserState(
              state.userState,
              state.pos,
              unconsed.head,
              unconsed.tail,
              state.config
            );
          return Result.csuc(
            ParseError.unknown(newPos),
            maybeVal.value,
            new State(state.config, unconsed.tail, newPos, newUserState)
          );
        }
      }
    });
  }

  function string(str) {
    function eofError(pos) {
      return new ParseError(
        pos,
        [
          new ErrorMessage(ErrorMessageType.SYSTEM_UNEXPECT, ""),
          new ErrorMessage(ErrorMessageType.EXPECT, show(str))
        ]
      );
    }
    function expectError(pos, char) {
      return new ParseError(
        pos,
        [
          new ErrorMessage(ErrorMessageType.SYSTEM_UNEXPECT, show(char)),
          new ErrorMessage(ErrorMessageType.EXPECT, show(str))
        ]
      );
    }
    return new Parser(state => {
      const len = str.length;
      if (len === 0) {
        return Result.esuc(ParseError.unknown(state.pos), "", state);
      }
      const tabWidth = state.config.tabWidth;
      const unicode  = state.config.unicode;
      let rest = state.input;
      if (unicode) {
        let consumed = false;
        for (const char of str) {
          const unconsed = stream.uncons(rest, unicode);
          if (unconsed.empty) {
            return !consumed
              ? Result.eerr(eofError(state.pos))
              : Result.cerr(eofError(state.pos));
          }
          else {
            if (char === unconsed.head) {
              rest     = unconsed.tail;
              consumed = true;
            }
            else {
              return !consumed
                ? Result.eerr(expectError(state.pos, unconsed.head))
                : Result.cerr(expectError(state.pos, unconsed.head));
            }
          }
        }
      }
      else {
        for (let i = 0; i < len; i++) {
          const unconsed = stream.uncons(rest, unicode);
          if (unconsed.empty) {
            return i === 0
              ? Result.eerr(eofError(state.pos))
              : Result.cerr(eofError(state.pos));
          }
          else {
            if (str[i] === unconsed.head) {
              rest = unconsed.tail;
            }
            else {
              return i === 0
                ? Result.eerr(expectError(state.pos, unconsed.head))
                : Result.cerr(expectError(state.pos, unconsed.head));
            }
          }
        }
      }
      const newPos = state.pos.addString(str, tabWidth, unicode);
      return Result.csuc(
        ParseError.unknown(newPos),
        str,
        new State(state.config, rest, newPos, state.userState)
      );
    });
  }

  function satisfy(test) {
    function systemUnexpectError(pos, str) {
      return new ParseError(
        pos,
        [new ErrorMessage(ErrorMessageType.SYSTEM_UNEXPECT, str)]
      );
    }
    return new Parser(state => {
      const unconsed = stream.uncons(state.input, state.config.unicode);
      if (unconsed.empty) {
        return Result.eerr(systemUnexpectError(state.pos, ""));
      }
      else {
        if (test(unconsed.head, state.config)) {
          const newPos = state.pos.addChar(unconsed.head, state.config.tabWidth);
          return Result.csuc(
            ParseError.unknown(newPos),
            unconsed.head,
            new State(state.config, unconsed.tail, newPos, state.userState)
          );
        }
        else {
          return Result.eerr(systemUnexpectError(state.pos, show(unconsed.head)));
        }
      }
    });
  }

  function oneOf(str) {
    const cpChars = new Set(str);
    const chars   = new Set();
    for (let i = 0; i < str.length; i++) {
      chars.add(str[i]);
    }
    return satisfy((char, config) => config.unicode ? cpChars.has(char) : chars.has(char));
  }

  function noneOf(str) {
    const cpChars = new Set(str);
    const chars   = new Set();
    for (let i = 0; i < str.length; i++) {
      chars.add(str[i]);
    }
    return satisfy((char, config) => config.unicode ? !cpChars.has(char) : !chars.has(char));
  }

  function char(expectChar) {
    return label(satisfy(char => char === expectChar), show(expectChar));
  }

  const anyChar = satisfy(() => true);

  const spaceChars    = new Set(" \f\n\r\t\v");
  const upperChars    = new Set("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  const lowerChars    = new Set("abcdefghijklmnopqrstuvwxyz");
  const letterChars   = new Set("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
  const digitChars    = new Set("0123456789");
  const alphaNumChars = new Set("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
  const octDigitChars = new Set("01234567");
  const hexDigitChars = new Set("0123456789ABCDEFabcdef");

  const space = label(satisfy(char => spaceChars.has(char)), "space");
  const spaces = label(skipMany(space), "white space");
  const newline = label(char("\n"), "new-line");
  const tab = label(char("\t"), "tab");
  const upper = label(satisfy(char => upperChars.has(char)), "uppercase letter");
  const lower = label(satisfy(char => lowerChars.has(char)), "lowercase letter");
  const letter = label(satisfy(char => letterChars.has(char)), "letter");
  const digit = label(satisfy(char => digitChars.has(char)), "digit");
  const alphaNum = label(satisfy(char => alphaNumChars.has(char)), "letter or digit");
  const octDigit = label(satisfy(char => octDigitChars.has(char)), "octal digit");
  const hexDigit = label(satisfy(char => hexDigitChars.has(char)), "hexadecimal digit");

  return Object.freeze({
    // # from "core"
    uncons: stream.uncons,
    lazy  : lq.lazy,
    parse : lq.parse,

    // # from "prim"
    map                 : lq.map,
    fmap                : lq.fmap,
    pure                : lq.pure,
    return              : lq.return,
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
    try                 : lq.try,
    lookAhead           : lq.lookAhead,
    reduceMany          : lq.reduceMany,
    many                : lq.many,
    skipMany            : lq.skipMany,
    tokens              : tokens,
    token               : token,
    tokenPrim           : tokenPrim,
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

    // # from "char"
    string        : string,
    satisfy       : satisfy,
    oneOf         : oneOf,
    noneOf        : noneOf,
    char          : char,
    unsafeAnyChar : anyChar,
    anyChar       : () => anyChar,
    unsafeSpace   : space,
    space         : () => space,
    unsafeSpaces  : spaces,
    spaces        : () => spaces,
    unsafeNewline : newline,
    newline       : () => newline,
    unsafeTab     : tab,
    tab           : () => tab,
    unsafeUpper   : upper,
    upper         : () => upper,
    unsafeLower   : lower,
    lower         : () => lower,
    unsafeLetter  : letter,
    letter        : () => letter,
    unsafeDigit   : digit,
    digit         : () => digit,
    unsafeAlphaNum: alphaNum,
    alphaNum      : () => alphaNum,
    unsafeOctDigit: octDigit,
    octDigit      : () => octDigit,
    unsafeHexDigit: hexDigit,
    hexDigit      : () => hexDigit,
    manyChars     : lq.manyChars,
    manyChars1    : lq.manyChars1,
    regexp        : lq.regexp,

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
    unsafeAnyToken: lq.anyToken,
    anyToken      : () => lq.anyToken,
    notFollowedBy : lq.notFollowedBy,
    unsafeEof     : lq.eof,
    eof           : () => lq.eof,
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

    // # from "qo"
    qo: lq.qo,
    do: lq.do,

    // # from "expr"
    buildExpressionParser: lq.buildExpressionParser,

    // # from "token"
    makeTokenParser: lq.makeTokenParser,
  });
}

const string = make({ uncons: lq.unconsString });

function array() {
  return make({ uncons: lq.uncons });
}

function stream() {
  return make({ uncons: lq.uncons });
}

module.exports = Object.freeze({
  // # from "core"
  // ## from "core/utils"
  show              : lq.show,
  unconsString      : lq.unconsString,
  // ## from "core/pos"
  SourcePos         : lq.SourcePos,
  // ## from "core/error"
  ErrorMessageType  : lq.ErrorMessageType,
  ErrorMessage      : lq.ErrorMessage,
  AbstractParseError: lq.AbstractParseError,
  ParseError        : lq.ParseError,
  LazyParseError    : lq.LazyParseError,
  // ## from "core/stream"
  uncons            : lq.uncons,
  ArrayStream       : lq.ArrayStream,
  // ## from "core/parser"
  Config            : lq.Config,
  State             : lq.State,
  Result            : Result,
  AbstractParser    : lq.AbstractParser,
  Parser            : lq.Parser,
  LazyParser        : lq.LazyParser,
  isParser          : lq.isParser,
  assertParser      : lq.assertParser,

  // # from "expr"
  OperatorType : lq.OperatorType,
  OperatorAssoc: lq.OperatorAssoc,
  Operator     : Operator,

  // # from "token"
  LanguageDef: lq.LanguageDef,

  make,
  string,
  array,
  stream,
});
