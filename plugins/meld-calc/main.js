'use strict';

var obsidian = require('obsidian');

/*! *****************************************************************************
Copyright (c) Microsoft Corporation.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
***************************************************************************** */

function __awaiter(thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
}

var commonjsGlobal = typeof globalThis !== 'undefined' ? globalThis : typeof window !== 'undefined' ? window : typeof global !== 'undefined' ? global : typeof self !== 'undefined' ? self : {};

function createCommonjsModule(fn, basedir, module) {
	return module = {
		path: basedir,
		exports: {},
		require: function (path, base) {
			return commonjsRequire(path, (base === undefined || base === null) ? module.path : base);
		}
	}, fn(module, module.exports), module.exports;
}

function commonjsRequire () {
	throw new Error('Dynamic requires are not currently supported by @rollup/plugin-commonjs');
}

var decimal = createCommonjsModule(function (module) {
(function (globalScope) {


  /*
   *  decimal.js v10.2.1
   *  An arbitrary-precision Decimal type for JavaScript.
   *  https://github.com/MikeMcl/decimal.js
   *  Copyright (c) 2020 Michael Mclaughlin <M8ch88l@gmail.com>
   *  MIT Licence
   */


  // -----------------------------------  EDITABLE DEFAULTS  ------------------------------------ //


    // The maximum exponent magnitude.
    // The limit on the value of `toExpNeg`, `toExpPos`, `minE` and `maxE`.
  var EXP_LIMIT = 9e15,                      // 0 to 9e15

    // The limit on the value of `precision`, and on the value of the first argument to
    // `toDecimalPlaces`, `toExponential`, `toFixed`, `toPrecision` and `toSignificantDigits`.
    MAX_DIGITS = 1e9,                        // 0 to 1e9

    // Base conversion alphabet.
    NUMERALS = '0123456789abcdef',

    // The natural logarithm of 10 (1025 digits).
    LN10 = '2.3025850929940456840179914546843642076011014886287729760333279009675726096773524802359972050895982983419677840422862486334095254650828067566662873690987816894829072083255546808437998948262331985283935053089653777326288461633662222876982198867465436674744042432743651550489343149393914796194044002221051017141748003688084012647080685567743216228355220114804663715659121373450747856947683463616792101806445070648000277502684916746550586856935673420670581136429224554405758925724208241314695689016758940256776311356919292033376587141660230105703089634572075440370847469940168269282808481184289314848524948644871927809676271275775397027668605952496716674183485704422507197965004714951050492214776567636938662976979522110718264549734772662425709429322582798502585509785265383207606726317164309505995087807523710333101197857547331541421808427543863591778117054309827482385045648019095610299291824318237525357709750539565187697510374970888692180205189339507238539205144634197265287286965110862571492198849978748873771345686209167058',

    // Pi (1025 digits).
    PI = '3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989380952572010654858632789',


    // The initial configuration properties of the Decimal constructor.
    DEFAULTS = {

      // These values must be integers within the stated ranges (inclusive).
      // Most of these values can be changed at run-time using the `Decimal.config` method.

      // The maximum number of significant digits of the result of a calculation or base conversion.
      // E.g. `Decimal.config({ precision: 20 });`
      precision: 20,                         // 1 to MAX_DIGITS

      // The rounding mode used when rounding to `precision`.
      //
      // ROUND_UP         0 Away from zero.
      // ROUND_DOWN       1 Towards zero.
      // ROUND_CEIL       2 Towards +Infinity.
      // ROUND_FLOOR      3 Towards -Infinity.
      // ROUND_HALF_UP    4 Towards nearest neighbour. If equidistant, up.
      // ROUND_HALF_DOWN  5 Towards nearest neighbour. If equidistant, down.
      // ROUND_HALF_EVEN  6 Towards nearest neighbour. If equidistant, towards even neighbour.
      // ROUND_HALF_CEIL  7 Towards nearest neighbour. If equidistant, towards +Infinity.
      // ROUND_HALF_FLOOR 8 Towards nearest neighbour. If equidistant, towards -Infinity.
      //
      // E.g.
      // `Decimal.rounding = 4;`
      // `Decimal.rounding = Decimal.ROUND_HALF_UP;`
      rounding: 4,                           // 0 to 8

      // The modulo mode used when calculating the modulus: a mod n.
      // The quotient (q = a / n) is calculated according to the corresponding rounding mode.
      // The remainder (r) is calculated as: r = a - n * q.
      //
      // UP         0 The remainder is positive if the dividend is negative, else is negative.
      // DOWN       1 The remainder has the same sign as the dividend (JavaScript %).
      // FLOOR      3 The remainder has the same sign as the divisor (Python %).
      // HALF_EVEN  6 The IEEE 754 remainder function.
      // EUCLID     9 Euclidian division. q = sign(n) * floor(a / abs(n)). Always positive.
      //
      // Truncated division (1), floored division (3), the IEEE 754 remainder (6), and Euclidian
      // division (9) are commonly used for the modulus operation. The other rounding modes can also
      // be used, but they may not give useful results.
      modulo: 1,                             // 0 to 9

      // The exponent value at and beneath which `toString` returns exponential notation.
      // JavaScript numbers: -7
      toExpNeg: -7,                          // 0 to -EXP_LIMIT

      // The exponent value at and above which `toString` returns exponential notation.
      // JavaScript numbers: 21
      toExpPos:  21,                         // 0 to EXP_LIMIT

      // The minimum exponent value, beneath which underflow to zero occurs.
      // JavaScript numbers: -324  (5e-324)
      minE: -EXP_LIMIT,                      // -1 to -EXP_LIMIT

      // The maximum exponent value, above which overflow to Infinity occurs.
      // JavaScript numbers: 308  (1.7976931348623157e+308)
      maxE: EXP_LIMIT,                       // 1 to EXP_LIMIT

      // Whether to use cryptographically-secure random number generation, if available.
      crypto: false                          // true/false
    },


  // ----------------------------------- END OF EDITABLE DEFAULTS ------------------------------- //


    Decimal, inexact, noConflict, quadrant,
    external = true,

    decimalError = '[DecimalError] ',
    invalidArgument = decimalError + 'Invalid argument: ',
    precisionLimitExceeded = decimalError + 'Precision limit exceeded',
    cryptoUnavailable = decimalError + 'crypto unavailable',

    mathfloor = Math.floor,
    mathpow = Math.pow,

    isBinary = /^0b([01]+(\.[01]*)?|\.[01]+)(p[+-]?\d+)?$/i,
    isHex = /^0x([0-9a-f]+(\.[0-9a-f]*)?|\.[0-9a-f]+)(p[+-]?\d+)?$/i,
    isOctal = /^0o([0-7]+(\.[0-7]*)?|\.[0-7]+)(p[+-]?\d+)?$/i,
    isDecimal = /^(\d+(\.\d*)?|\.\d+)(e[+-]?\d+)?$/i,

    BASE = 1e7,
    LOG_BASE = 7,
    MAX_SAFE_INTEGER = 9007199254740991,

    LN10_PRECISION = LN10.length - 1,
    PI_PRECISION = PI.length - 1,

    // Decimal.prototype object
    P = { name: '[object Decimal]' };


  // Decimal prototype methods


  /*
   *  absoluteValue             abs
   *  ceil
   *  comparedTo                cmp
   *  cosine                    cos
   *  cubeRoot                  cbrt
   *  decimalPlaces             dp
   *  dividedBy                 div
   *  dividedToIntegerBy        divToInt
   *  equals                    eq
   *  floor
   *  greaterThan               gt
   *  greaterThanOrEqualTo      gte
   *  hyperbolicCosine          cosh
   *  hyperbolicSine            sinh
   *  hyperbolicTangent         tanh
   *  inverseCosine             acos
   *  inverseHyperbolicCosine   acosh
   *  inverseHyperbolicSine     asinh
   *  inverseHyperbolicTangent  atanh
   *  inverseSine               asin
   *  inverseTangent            atan
   *  isFinite
   *  isInteger                 isInt
   *  isNaN
   *  isNegative                isNeg
   *  isPositive                isPos
   *  isZero
   *  lessThan                  lt
   *  lessThanOrEqualTo         lte
   *  logarithm                 log
   *  [maximum]                 [max]
   *  [minimum]                 [min]
   *  minus                     sub
   *  modulo                    mod
   *  naturalExponential        exp
   *  naturalLogarithm          ln
   *  negated                   neg
   *  plus                      add
   *  precision                 sd
   *  round
   *  sine                      sin
   *  squareRoot                sqrt
   *  tangent                   tan
   *  times                     mul
   *  toBinary
   *  toDecimalPlaces           toDP
   *  toExponential
   *  toFixed
   *  toFraction
   *  toHexadecimal             toHex
   *  toNearest
   *  toNumber
   *  toOctal
   *  toPower                   pow
   *  toPrecision
   *  toSignificantDigits       toSD
   *  toString
   *  truncated                 trunc
   *  valueOf                   toJSON
   */


  /*
   * Return a new Decimal whose value is the absolute value of this Decimal.
   *
   */
  P.absoluteValue = P.abs = function () {
    var x = new this.constructor(this);
    if (x.s < 0) x.s = 1;
    return finalise(x);
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a whole number in the
   * direction of positive Infinity.
   *
   */
  P.ceil = function () {
    return finalise(new this.constructor(this), this.e + 1, 2);
  };


  /*
   * Return
   *   1    if the value of this Decimal is greater than the value of `y`,
   *  -1    if the value of this Decimal is less than the value of `y`,
   *   0    if they have the same value,
   *   NaN  if the value of either Decimal is NaN.
   *
   */
  P.comparedTo = P.cmp = function (y) {
    var i, j, xdL, ydL,
      x = this,
      xd = x.d,
      yd = (y = new x.constructor(y)).d,
      xs = x.s,
      ys = y.s;

    // Either NaN or ±Infinity?
    if (!xd || !yd) {
      return !xs || !ys ? NaN : xs !== ys ? xs : xd === yd ? 0 : !xd ^ xs < 0 ? 1 : -1;
    }

    // Either zero?
    if (!xd[0] || !yd[0]) return xd[0] ? xs : yd[0] ? -ys : 0;

    // Signs differ?
    if (xs !== ys) return xs;

    // Compare exponents.
    if (x.e !== y.e) return x.e > y.e ^ xs < 0 ? 1 : -1;

    xdL = xd.length;
    ydL = yd.length;

    // Compare digit by digit.
    for (i = 0, j = xdL < ydL ? xdL : ydL; i < j; ++i) {
      if (xd[i] !== yd[i]) return xd[i] > yd[i] ^ xs < 0 ? 1 : -1;
    }

    // Compare lengths.
    return xdL === ydL ? 0 : xdL > ydL ^ xs < 0 ? 1 : -1;
  };


  /*
   * Return a new Decimal whose value is the cosine of the value in radians of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-1, 1]
   *
   * cos(0)         = 1
   * cos(-0)        = 1
   * cos(Infinity)  = NaN
   * cos(-Infinity) = NaN
   * cos(NaN)       = NaN
   *
   */
  P.cosine = P.cos = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.d) return new Ctor(NaN);

    // cos(0) = cos(-0) = 1
    if (!x.d[0]) return new Ctor(1);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(x.e, x.sd()) + LOG_BASE;
    Ctor.rounding = 1;

    x = cosine(Ctor, toLessThanHalfPi(Ctor, x));

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return finalise(quadrant == 2 || quadrant == 3 ? x.neg() : x, pr, rm, true);
  };


  /*
   *
   * Return a new Decimal whose value is the cube root of the value of this Decimal, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   *  cbrt(0)  =  0
   *  cbrt(-0) = -0
   *  cbrt(1)  =  1
   *  cbrt(-1) = -1
   *  cbrt(N)  =  N
   *  cbrt(-I) = -I
   *  cbrt(I)  =  I
   *
   * Math.cbrt(x) = (x < 0 ? -Math.pow(-x, 1/3) : Math.pow(x, 1/3))
   *
   */
  P.cubeRoot = P.cbrt = function () {
    var e, m, n, r, rep, s, sd, t, t3, t3plusx,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite() || x.isZero()) return new Ctor(x);
    external = false;

    // Initial estimate.
    s = x.s * mathpow(x.s * x, 1 / 3);

     // Math.cbrt underflow/overflow?
     // Pass x to Math.pow as integer, then adjust the exponent of the result.
    if (!s || Math.abs(s) == 1 / 0) {
      n = digitsToString(x.d);
      e = x.e;

      // Adjust n exponent so it is a multiple of 3 away from x exponent.
      if (s = (e - n.length + 1) % 3) n += (s == 1 || s == -2 ? '0' : '00');
      s = mathpow(n, 1 / 3);

      // Rarely, e may be one less than the result exponent value.
      e = mathfloor((e + 1) / 3) - (e % 3 == (e < 0 ? -1 : 2));

      if (s == 1 / 0) {
        n = '5e' + e;
      } else {
        n = s.toExponential();
        n = n.slice(0, n.indexOf('e') + 1) + e;
      }

      r = new Ctor(n);
      r.s = x.s;
    } else {
      r = new Ctor(s.toString());
    }

    sd = (e = Ctor.precision) + 3;

    // Halley's method.
    // TODO? Compare Newton's method.
    for (;;) {
      t = r;
      t3 = t.times(t).times(t);
      t3plusx = t3.plus(x);
      r = divide(t3plusx.plus(x).times(t), t3plusx.plus(t3), sd + 2, 1);

      // TODO? Replace with for-loop and checkRoundingDigits.
      if (digitsToString(t.d).slice(0, sd) === (n = digitsToString(r.d)).slice(0, sd)) {
        n = n.slice(sd - 3, sd + 1);

        // The 4th rounding digit may be in error by -1 so if the 4 rounding digits are 9999 or 4999
        // , i.e. approaching a rounding boundary, continue the iteration.
        if (n == '9999' || !rep && n == '4999') {

          // On the first iteration only, check to see if rounding up gives the exact result as the
          // nines may infinitely repeat.
          if (!rep) {
            finalise(t, e + 1, 0);

            if (t.times(t).times(t).eq(x)) {
              r = t;
              break;
            }
          }

          sd += 4;
          rep = 1;
        } else {

          // If the rounding digits are null, 0{0,4} or 50{0,3}, check for an exact result.
          // If not, then there are further digits and m will be truthy.
          if (!+n || !+n.slice(1) && n.charAt(0) == '5') {

            // Truncate to the first rounding digit.
            finalise(r, e + 1, 1);
            m = !r.times(r).times(r).eq(x);
          }

          break;
        }
      }
    }

    external = true;

    return finalise(r, e, Ctor.rounding, m);
  };


  /*
   * Return the number of decimal places of the value of this Decimal.
   *
   */
  P.decimalPlaces = P.dp = function () {
    var w,
      d = this.d,
      n = NaN;

    if (d) {
      w = d.length - 1;
      n = (w - mathfloor(this.e / LOG_BASE)) * LOG_BASE;

      // Subtract the number of trailing zeros of the last word.
      w = d[w];
      if (w) for (; w % 10 == 0; w /= 10) n--;
      if (n < 0) n = 0;
    }

    return n;
  };


  /*
   *  n / 0 = I
   *  n / N = N
   *  n / I = 0
   *  0 / n = 0
   *  0 / 0 = N
   *  0 / N = N
   *  0 / I = 0
   *  N / n = N
   *  N / 0 = N
   *  N / N = N
   *  N / I = N
   *  I / n = I
   *  I / 0 = I
   *  I / N = N
   *  I / I = N
   *
   * Return a new Decimal whose value is the value of this Decimal divided by `y`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   */
  P.dividedBy = P.div = function (y) {
    return divide(this, new this.constructor(y));
  };


  /*
   * Return a new Decimal whose value is the integer part of dividing the value of this Decimal
   * by the value of `y`, rounded to `precision` significant digits using rounding mode `rounding`.
   *
   */
  P.dividedToIntegerBy = P.divToInt = function (y) {
    var x = this,
      Ctor = x.constructor;
    return finalise(divide(x, new Ctor(y), 0, 1, 1), Ctor.precision, Ctor.rounding);
  };


  /*
   * Return true if the value of this Decimal is equal to the value of `y`, otherwise return false.
   *
   */
  P.equals = P.eq = function (y) {
    return this.cmp(y) === 0;
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a whole number in the
   * direction of negative Infinity.
   *
   */
  P.floor = function () {
    return finalise(new this.constructor(this), this.e + 1, 3);
  };


  /*
   * Return true if the value of this Decimal is greater than the value of `y`, otherwise return
   * false.
   *
   */
  P.greaterThan = P.gt = function (y) {
    return this.cmp(y) > 0;
  };


  /*
   * Return true if the value of this Decimal is greater than or equal to the value of `y`,
   * otherwise return false.
   *
   */
  P.greaterThanOrEqualTo = P.gte = function (y) {
    var k = this.cmp(y);
    return k == 1 || k === 0;
  };


  /*
   * Return a new Decimal whose value is the hyperbolic cosine of the value in radians of this
   * Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [1, Infinity]
   *
   * cosh(x) = 1 + x^2/2! + x^4/4! + x^6/6! + ...
   *
   * cosh(0)         = 1
   * cosh(-0)        = 1
   * cosh(Infinity)  = Infinity
   * cosh(-Infinity) = Infinity
   * cosh(NaN)       = NaN
   *
   *  x        time taken (ms)   result
   * 1000      9                 9.8503555700852349694e+433
   * 10000     25                4.4034091128314607936e+4342
   * 100000    171               1.4033316802130615897e+43429
   * 1000000   3817              1.5166076984010437725e+434294
   * 10000000  abandoned after 2 minute wait
   *
   * TODO? Compare performance of cosh(x) = 0.5 * (exp(x) + exp(-x))
   *
   */
  P.hyperbolicCosine = P.cosh = function () {
    var k, n, pr, rm, len,
      x = this,
      Ctor = x.constructor,
      one = new Ctor(1);

    if (!x.isFinite()) return new Ctor(x.s ? 1 / 0 : NaN);
    if (x.isZero()) return one;

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(x.e, x.sd()) + 4;
    Ctor.rounding = 1;
    len = x.d.length;

    // Argument reduction: cos(4x) = 1 - 8cos^2(x) + 8cos^4(x) + 1
    // i.e. cos(x) = 1 - cos^2(x/4)(8 - 8cos^2(x/4))

    // Estimate the optimum number of times to use the argument reduction.
    // TODO? Estimation reused from cosine() and may not be optimal here.
    if (len < 32) {
      k = Math.ceil(len / 3);
      n = (1 / tinyPow(4, k)).toString();
    } else {
      k = 16;
      n = '2.3283064365386962890625e-10';
    }

    x = taylorSeries(Ctor, 1, x.times(n), new Ctor(1), true);

    // Reverse argument reduction
    var cosh2_x,
      i = k,
      d8 = new Ctor(8);
    for (; i--;) {
      cosh2_x = x.times(x);
      x = one.minus(cosh2_x.times(d8.minus(cosh2_x.times(d8))));
    }

    return finalise(x, Ctor.precision = pr, Ctor.rounding = rm, true);
  };


  /*
   * Return a new Decimal whose value is the hyperbolic sine of the value in radians of this
   * Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-Infinity, Infinity]
   *
   * sinh(x) = x + x^3/3! + x^5/5! + x^7/7! + ...
   *
   * sinh(0)         = 0
   * sinh(-0)        = -0
   * sinh(Infinity)  = Infinity
   * sinh(-Infinity) = -Infinity
   * sinh(NaN)       = NaN
   *
   * x        time taken (ms)
   * 10       2 ms
   * 100      5 ms
   * 1000     14 ms
   * 10000    82 ms
   * 100000   886 ms            1.4033316802130615897e+43429
   * 200000   2613 ms
   * 300000   5407 ms
   * 400000   8824 ms
   * 500000   13026 ms          8.7080643612718084129e+217146
   * 1000000  48543 ms
   *
   * TODO? Compare performance of sinh(x) = 0.5 * (exp(x) - exp(-x))
   *
   */
  P.hyperbolicSine = P.sinh = function () {
    var k, pr, rm, len,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite() || x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(x.e, x.sd()) + 4;
    Ctor.rounding = 1;
    len = x.d.length;

    if (len < 3) {
      x = taylorSeries(Ctor, 2, x, x, true);
    } else {

      // Alternative argument reduction: sinh(3x) = sinh(x)(3 + 4sinh^2(x))
      // i.e. sinh(x) = sinh(x/3)(3 + 4sinh^2(x/3))
      // 3 multiplications and 1 addition

      // Argument reduction: sinh(5x) = sinh(x)(5 + sinh^2(x)(20 + 16sinh^2(x)))
      // i.e. sinh(x) = sinh(x/5)(5 + sinh^2(x/5)(20 + 16sinh^2(x/5)))
      // 4 multiplications and 2 additions

      // Estimate the optimum number of times to use the argument reduction.
      k = 1.4 * Math.sqrt(len);
      k = k > 16 ? 16 : k | 0;

      x = x.times(1 / tinyPow(5, k));
      x = taylorSeries(Ctor, 2, x, x, true);

      // Reverse argument reduction
      var sinh2_x,
        d5 = new Ctor(5),
        d16 = new Ctor(16),
        d20 = new Ctor(20);
      for (; k--;) {
        sinh2_x = x.times(x);
        x = x.times(d5.plus(sinh2_x.times(d16.times(sinh2_x).plus(d20))));
      }
    }

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return finalise(x, pr, rm, true);
  };


  /*
   * Return a new Decimal whose value is the hyperbolic tangent of the value in radians of this
   * Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-1, 1]
   *
   * tanh(x) = sinh(x) / cosh(x)
   *
   * tanh(0)         = 0
   * tanh(-0)        = -0
   * tanh(Infinity)  = 1
   * tanh(-Infinity) = -1
   * tanh(NaN)       = NaN
   *
   */
  P.hyperbolicTangent = P.tanh = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite()) return new Ctor(x.s);
    if (x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + 7;
    Ctor.rounding = 1;

    return divide(x.sinh(), x.cosh(), Ctor.precision = pr, Ctor.rounding = rm);
  };


  /*
   * Return a new Decimal whose value is the arccosine (inverse cosine) in radians of the value of
   * this Decimal.
   *
   * Domain: [-1, 1]
   * Range: [0, pi]
   *
   * acos(x) = pi/2 - asin(x)
   *
   * acos(0)       = pi/2
   * acos(-0)      = pi/2
   * acos(1)       = 0
   * acos(-1)      = pi
   * acos(1/2)     = pi/3
   * acos(-1/2)    = 2*pi/3
   * acos(|x| > 1) = NaN
   * acos(NaN)     = NaN
   *
   */
  P.inverseCosine = P.acos = function () {
    var halfPi,
      x = this,
      Ctor = x.constructor,
      k = x.abs().cmp(1),
      pr = Ctor.precision,
      rm = Ctor.rounding;

    if (k !== -1) {
      return k === 0
        // |x| is 1
        ? x.isNeg() ? getPi(Ctor, pr, rm) : new Ctor(0)
        // |x| > 1 or x is NaN
        : new Ctor(NaN);
    }

    if (x.isZero()) return getPi(Ctor, pr + 4, rm).times(0.5);

    // TODO? Special case acos(0.5) = pi/3 and acos(-0.5) = 2*pi/3

    Ctor.precision = pr + 6;
    Ctor.rounding = 1;

    x = x.asin();
    halfPi = getPi(Ctor, pr + 4, rm).times(0.5);

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return halfPi.minus(x);
  };


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic cosine in radians of the
   * value of this Decimal.
   *
   * Domain: [1, Infinity]
   * Range: [0, Infinity]
   *
   * acosh(x) = ln(x + sqrt(x^2 - 1))
   *
   * acosh(x < 1)     = NaN
   * acosh(NaN)       = NaN
   * acosh(Infinity)  = Infinity
   * acosh(-Infinity) = NaN
   * acosh(0)         = NaN
   * acosh(-0)        = NaN
   * acosh(1)         = 0
   * acosh(-1)        = NaN
   *
   */
  P.inverseHyperbolicCosine = P.acosh = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (x.lte(1)) return new Ctor(x.eq(1) ? 0 : NaN);
    if (!x.isFinite()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(Math.abs(x.e), x.sd()) + 4;
    Ctor.rounding = 1;
    external = false;

    x = x.times(x).minus(1).sqrt().plus(x);

    external = true;
    Ctor.precision = pr;
    Ctor.rounding = rm;

    return x.ln();
  };


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic sine in radians of the value
   * of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-Infinity, Infinity]
   *
   * asinh(x) = ln(x + sqrt(x^2 + 1))
   *
   * asinh(NaN)       = NaN
   * asinh(Infinity)  = Infinity
   * asinh(-Infinity) = -Infinity
   * asinh(0)         = 0
   * asinh(-0)        = -0
   *
   */
  P.inverseHyperbolicSine = P.asinh = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite() || x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + 2 * Math.max(Math.abs(x.e), x.sd()) + 6;
    Ctor.rounding = 1;
    external = false;

    x = x.times(x).plus(1).sqrt().plus(x);

    external = true;
    Ctor.precision = pr;
    Ctor.rounding = rm;

    return x.ln();
  };


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic tangent in radians of the
   * value of this Decimal.
   *
   * Domain: [-1, 1]
   * Range: [-Infinity, Infinity]
   *
   * atanh(x) = 0.5 * ln((1 + x) / (1 - x))
   *
   * atanh(|x| > 1)   = NaN
   * atanh(NaN)       = NaN
   * atanh(Infinity)  = NaN
   * atanh(-Infinity) = NaN
   * atanh(0)         = 0
   * atanh(-0)        = -0
   * atanh(1)         = Infinity
   * atanh(-1)        = -Infinity
   *
   */
  P.inverseHyperbolicTangent = P.atanh = function () {
    var pr, rm, wpr, xsd,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite()) return new Ctor(NaN);
    if (x.e >= 0) return new Ctor(x.abs().eq(1) ? x.s / 0 : x.isZero() ? x : NaN);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    xsd = x.sd();

    if (Math.max(xsd, pr) < 2 * -x.e - 1) return finalise(new Ctor(x), pr, rm, true);

    Ctor.precision = wpr = xsd - x.e;

    x = divide(x.plus(1), new Ctor(1).minus(x), wpr + pr, 1);

    Ctor.precision = pr + 4;
    Ctor.rounding = 1;

    x = x.ln();

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return x.times(0.5);
  };


  /*
   * Return a new Decimal whose value is the arcsine (inverse sine) in radians of the value of this
   * Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-pi/2, pi/2]
   *
   * asin(x) = 2*atan(x/(1 + sqrt(1 - x^2)))
   *
   * asin(0)       = 0
   * asin(-0)      = -0
   * asin(1/2)     = pi/6
   * asin(-1/2)    = -pi/6
   * asin(1)       = pi/2
   * asin(-1)      = -pi/2
   * asin(|x| > 1) = NaN
   * asin(NaN)     = NaN
   *
   * TODO? Compare performance of Taylor series.
   *
   */
  P.inverseSine = P.asin = function () {
    var halfPi, k,
      pr, rm,
      x = this,
      Ctor = x.constructor;

    if (x.isZero()) return new Ctor(x);

    k = x.abs().cmp(1);
    pr = Ctor.precision;
    rm = Ctor.rounding;

    if (k !== -1) {

      // |x| is 1
      if (k === 0) {
        halfPi = getPi(Ctor, pr + 4, rm).times(0.5);
        halfPi.s = x.s;
        return halfPi;
      }

      // |x| > 1 or x is NaN
      return new Ctor(NaN);
    }

    // TODO? Special case asin(1/2) = pi/6 and asin(-1/2) = -pi/6

    Ctor.precision = pr + 6;
    Ctor.rounding = 1;

    x = x.div(new Ctor(1).minus(x.times(x)).sqrt().plus(1)).atan();

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return x.times(2);
  };


  /*
   * Return a new Decimal whose value is the arctangent (inverse tangent) in radians of the value
   * of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-pi/2, pi/2]
   *
   * atan(x) = x - x^3/3 + x^5/5 - x^7/7 + ...
   *
   * atan(0)         = 0
   * atan(-0)        = -0
   * atan(1)         = pi/4
   * atan(-1)        = -pi/4
   * atan(Infinity)  = pi/2
   * atan(-Infinity) = -pi/2
   * atan(NaN)       = NaN
   *
   */
  P.inverseTangent = P.atan = function () {
    var i, j, k, n, px, t, r, wpr, x2,
      x = this,
      Ctor = x.constructor,
      pr = Ctor.precision,
      rm = Ctor.rounding;

    if (!x.isFinite()) {
      if (!x.s) return new Ctor(NaN);
      if (pr + 4 <= PI_PRECISION) {
        r = getPi(Ctor, pr + 4, rm).times(0.5);
        r.s = x.s;
        return r;
      }
    } else if (x.isZero()) {
      return new Ctor(x);
    } else if (x.abs().eq(1) && pr + 4 <= PI_PRECISION) {
      r = getPi(Ctor, pr + 4, rm).times(0.25);
      r.s = x.s;
      return r;
    }

    Ctor.precision = wpr = pr + 10;
    Ctor.rounding = 1;

    // TODO? if (x >= 1 && pr <= PI_PRECISION) atan(x) = halfPi * x.s - atan(1 / x);

    // Argument reduction
    // Ensure |x| < 0.42
    // atan(x) = 2 * atan(x / (1 + sqrt(1 + x^2)))

    k = Math.min(28, wpr / LOG_BASE + 2 | 0);

    for (i = k; i; --i) x = x.div(x.times(x).plus(1).sqrt().plus(1));

    external = false;

    j = Math.ceil(wpr / LOG_BASE);
    n = 1;
    x2 = x.times(x);
    r = new Ctor(x);
    px = x;

    // atan(x) = x - x^3/3 + x^5/5 - x^7/7 + ...
    for (; i !== -1;) {
      px = px.times(x2);
      t = r.minus(px.div(n += 2));

      px = px.times(x2);
      r = t.plus(px.div(n += 2));

      if (r.d[j] !== void 0) for (i = j; r.d[i] === t.d[i] && i--;);
    }

    if (k) r = r.times(2 << (k - 1));

    external = true;

    return finalise(r, Ctor.precision = pr, Ctor.rounding = rm, true);
  };


  /*
   * Return true if the value of this Decimal is a finite number, otherwise return false.
   *
   */
  P.isFinite = function () {
    return !!this.d;
  };


  /*
   * Return true if the value of this Decimal is an integer, otherwise return false.
   *
   */
  P.isInteger = P.isInt = function () {
    return !!this.d && mathfloor(this.e / LOG_BASE) > this.d.length - 2;
  };


  /*
   * Return true if the value of this Decimal is NaN, otherwise return false.
   *
   */
  P.isNaN = function () {
    return !this.s;
  };


  /*
   * Return true if the value of this Decimal is negative, otherwise return false.
   *
   */
  P.isNegative = P.isNeg = function () {
    return this.s < 0;
  };


  /*
   * Return true if the value of this Decimal is positive, otherwise return false.
   *
   */
  P.isPositive = P.isPos = function () {
    return this.s > 0;
  };


  /*
   * Return true if the value of this Decimal is 0 or -0, otherwise return false.
   *
   */
  P.isZero = function () {
    return !!this.d && this.d[0] === 0;
  };


  /*
   * Return true if the value of this Decimal is less than `y`, otherwise return false.
   *
   */
  P.lessThan = P.lt = function (y) {
    return this.cmp(y) < 0;
  };


  /*
   * Return true if the value of this Decimal is less than or equal to `y`, otherwise return false.
   *
   */
  P.lessThanOrEqualTo = P.lte = function (y) {
    return this.cmp(y) < 1;
  };


  /*
   * Return the logarithm of the value of this Decimal to the specified base, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * If no base is specified, return log[10](arg).
   *
   * log[base](arg) = ln(arg) / ln(base)
   *
   * The result will always be correctly rounded if the base of the log is 10, and 'almost always'
   * otherwise:
   *
   * Depending on the rounding mode, the result may be incorrectly rounded if the first fifteen
   * rounding digits are [49]99999999999999 or [50]00000000000000. In that case, the maximum error
   * between the result and the correctly rounded result will be one ulp (unit in the last place).
   *
   * log[-b](a)       = NaN
   * log[0](a)        = NaN
   * log[1](a)        = NaN
   * log[NaN](a)      = NaN
   * log[Infinity](a) = NaN
   * log[b](0)        = -Infinity
   * log[b](-0)       = -Infinity
   * log[b](-a)       = NaN
   * log[b](1)        = 0
   * log[b](Infinity) = Infinity
   * log[b](NaN)      = NaN
   *
   * [base] {number|string|Decimal} The base of the logarithm.
   *
   */
  P.logarithm = P.log = function (base) {
    var isBase10, d, denominator, k, inf, num, sd, r,
      arg = this,
      Ctor = arg.constructor,
      pr = Ctor.precision,
      rm = Ctor.rounding,
      guard = 5;

    // Default base is 10.
    if (base == null) {
      base = new Ctor(10);
      isBase10 = true;
    } else {
      base = new Ctor(base);
      d = base.d;

      // Return NaN if base is negative, or non-finite, or is 0 or 1.
      if (base.s < 0 || !d || !d[0] || base.eq(1)) return new Ctor(NaN);

      isBase10 = base.eq(10);
    }

    d = arg.d;

    // Is arg negative, non-finite, 0 or 1?
    if (arg.s < 0 || !d || !d[0] || arg.eq(1)) {
      return new Ctor(d && !d[0] ? -1 / 0 : arg.s != 1 ? NaN : d ? 0 : 1 / 0);
    }

    // The result will have a non-terminating decimal expansion if base is 10 and arg is not an
    // integer power of 10.
    if (isBase10) {
      if (d.length > 1) {
        inf = true;
      } else {
        for (k = d[0]; k % 10 === 0;) k /= 10;
        inf = k !== 1;
      }
    }

    external = false;
    sd = pr + guard;
    num = naturalLogarithm(arg, sd);
    denominator = isBase10 ? getLn10(Ctor, sd + 10) : naturalLogarithm(base, sd);

    // The result will have 5 rounding digits.
    r = divide(num, denominator, sd, 1);

    // If at a rounding boundary, i.e. the result's rounding digits are [49]9999 or [50]0000,
    // calculate 10 further digits.
    //
    // If the result is known to have an infinite decimal expansion, repeat this until it is clear
    // that the result is above or below the boundary. Otherwise, if after calculating the 10
    // further digits, the last 14 are nines, round up and assume the result is exact.
    // Also assume the result is exact if the last 14 are zero.
    //
    // Example of a result that will be incorrectly rounded:
    // log[1048576](4503599627370502) = 2.60000000000000009610279511444746...
    // The above result correctly rounded using ROUND_CEIL to 1 decimal place should be 2.7, but it
    // will be given as 2.6 as there are 15 zeros immediately after the requested decimal place, so
    // the exact result would be assumed to be 2.6, which rounded using ROUND_CEIL to 1 decimal
    // place is still 2.6.
    if (checkRoundingDigits(r.d, k = pr, rm)) {

      do {
        sd += 10;
        num = naturalLogarithm(arg, sd);
        denominator = isBase10 ? getLn10(Ctor, sd + 10) : naturalLogarithm(base, sd);
        r = divide(num, denominator, sd, 1);

        if (!inf) {

          // Check for 14 nines from the 2nd rounding digit, as the first may be 4.
          if (+digitsToString(r.d).slice(k + 1, k + 15) + 1 == 1e14) {
            r = finalise(r, pr + 1, 0);
          }

          break;
        }
      } while (checkRoundingDigits(r.d, k += 10, rm));
    }

    external = true;

    return finalise(r, pr, rm);
  };


  /*
   * Return a new Decimal whose value is the maximum of the arguments and the value of this Decimal.
   *
   * arguments {number|string|Decimal}
   *
  P.max = function () {
    Array.prototype.push.call(arguments, this);
    return maxOrMin(this.constructor, arguments, 'lt');
  };
   */


  /*
   * Return a new Decimal whose value is the minimum of the arguments and the value of this Decimal.
   *
   * arguments {number|string|Decimal}
   *
  P.min = function () {
    Array.prototype.push.call(arguments, this);
    return maxOrMin(this.constructor, arguments, 'gt');
  };
   */


  /*
   *  n - 0 = n
   *  n - N = N
   *  n - I = -I
   *  0 - n = -n
   *  0 - 0 = 0
   *  0 - N = N
   *  0 - I = -I
   *  N - n = N
   *  N - 0 = N
   *  N - N = N
   *  N - I = N
   *  I - n = I
   *  I - 0 = I
   *  I - N = N
   *  I - I = N
   *
   * Return a new Decimal whose value is the value of this Decimal minus `y`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   */
  P.minus = P.sub = function (y) {
    var d, e, i, j, k, len, pr, rm, xd, xe, xLTy, yd,
      x = this,
      Ctor = x.constructor;

    y = new Ctor(y);

    // If either is not finite...
    if (!x.d || !y.d) {

      // Return NaN if either is NaN.
      if (!x.s || !y.s) y = new Ctor(NaN);

      // Return y negated if x is finite and y is ±Infinity.
      else if (x.d) y.s = -y.s;

      // Return x if y is finite and x is ±Infinity.
      // Return x if both are ±Infinity with different signs.
      // Return NaN if both are ±Infinity with the same sign.
      else y = new Ctor(y.d || x.s !== y.s ? x : NaN);

      return y;
    }

    // If signs differ...
    if (x.s != y.s) {
      y.s = -y.s;
      return x.plus(y);
    }

    xd = x.d;
    yd = y.d;
    pr = Ctor.precision;
    rm = Ctor.rounding;

    // If either is zero...
    if (!xd[0] || !yd[0]) {

      // Return y negated if x is zero and y is non-zero.
      if (yd[0]) y.s = -y.s;

      // Return x if y is zero and x is non-zero.
      else if (xd[0]) y = new Ctor(x);

      // Return zero if both are zero.
      // From IEEE 754 (2008) 6.3: 0 - 0 = -0 - -0 = -0 when rounding to -Infinity.
      else return new Ctor(rm === 3 ? -0 : 0);

      return external ? finalise(y, pr, rm) : y;
    }

    // x and y are finite, non-zero numbers with the same sign.

    // Calculate base 1e7 exponents.
    e = mathfloor(y.e / LOG_BASE);
    xe = mathfloor(x.e / LOG_BASE);

    xd = xd.slice();
    k = xe - e;

    // If base 1e7 exponents differ...
    if (k) {
      xLTy = k < 0;

      if (xLTy) {
        d = xd;
        k = -k;
        len = yd.length;
      } else {
        d = yd;
        e = xe;
        len = xd.length;
      }

      // Numbers with massively different exponents would result in a very high number of
      // zeros needing to be prepended, but this can be avoided while still ensuring correct
      // rounding by limiting the number of zeros to `Math.ceil(pr / LOG_BASE) + 2`.
      i = Math.max(Math.ceil(pr / LOG_BASE), len) + 2;

      if (k > i) {
        k = i;
        d.length = 1;
      }

      // Prepend zeros to equalise exponents.
      d.reverse();
      for (i = k; i--;) d.push(0);
      d.reverse();

    // Base 1e7 exponents equal.
    } else {

      // Check digits to determine which is the bigger number.

      i = xd.length;
      len = yd.length;
      xLTy = i < len;
      if (xLTy) len = i;

      for (i = 0; i < len; i++) {
        if (xd[i] != yd[i]) {
          xLTy = xd[i] < yd[i];
          break;
        }
      }

      k = 0;
    }

    if (xLTy) {
      d = xd;
      xd = yd;
      yd = d;
      y.s = -y.s;
    }

    len = xd.length;

    // Append zeros to `xd` if shorter.
    // Don't add zeros to `yd` if shorter as subtraction only needs to start at `yd` length.
    for (i = yd.length - len; i > 0; --i) xd[len++] = 0;

    // Subtract yd from xd.
    for (i = yd.length; i > k;) {

      if (xd[--i] < yd[i]) {
        for (j = i; j && xd[--j] === 0;) xd[j] = BASE - 1;
        --xd[j];
        xd[i] += BASE;
      }

      xd[i] -= yd[i];
    }

    // Remove trailing zeros.
    for (; xd[--len] === 0;) xd.pop();

    // Remove leading zeros and adjust exponent accordingly.
    for (; xd[0] === 0; xd.shift()) --e;

    // Zero?
    if (!xd[0]) return new Ctor(rm === 3 ? -0 : 0);

    y.d = xd;
    y.e = getBase10Exponent(xd, e);

    return external ? finalise(y, pr, rm) : y;
  };


  /*
   *   n % 0 =  N
   *   n % N =  N
   *   n % I =  n
   *   0 % n =  0
   *  -0 % n = -0
   *   0 % 0 =  N
   *   0 % N =  N
   *   0 % I =  0
   *   N % n =  N
   *   N % 0 =  N
   *   N % N =  N
   *   N % I =  N
   *   I % n =  N
   *   I % 0 =  N
   *   I % N =  N
   *   I % I =  N
   *
   * Return a new Decimal whose value is the value of this Decimal modulo `y`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   * The result depends on the modulo mode.
   *
   */
  P.modulo = P.mod = function (y) {
    var q,
      x = this,
      Ctor = x.constructor;

    y = new Ctor(y);

    // Return NaN if x is ±Infinity or NaN, or y is NaN or ±0.
    if (!x.d || !y.s || y.d && !y.d[0]) return new Ctor(NaN);

    // Return x if y is ±Infinity or x is ±0.
    if (!y.d || x.d && !x.d[0]) {
      return finalise(new Ctor(x), Ctor.precision, Ctor.rounding);
    }

    // Prevent rounding of intermediate calculations.
    external = false;

    if (Ctor.modulo == 9) {

      // Euclidian division: q = sign(y) * floor(x / abs(y))
      // result = x - q * y    where  0 <= result < abs(y)
      q = divide(x, y.abs(), 0, 3, 1);
      q.s *= y.s;
    } else {
      q = divide(x, y, 0, Ctor.modulo, 1);
    }

    q = q.times(y);

    external = true;

    return x.minus(q);
  };


  /*
   * Return a new Decimal whose value is the natural exponential of the value of this Decimal,
   * i.e. the base e raised to the power the value of this Decimal, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   */
  P.naturalExponential = P.exp = function () {
    return naturalExponential(this);
  };


  /*
   * Return a new Decimal whose value is the natural logarithm of the value of this Decimal,
   * rounded to `precision` significant digits using rounding mode `rounding`.
   *
   */
  P.naturalLogarithm = P.ln = function () {
    return naturalLogarithm(this);
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal negated, i.e. as if multiplied by
   * -1.
   *
   */
  P.negated = P.neg = function () {
    var x = new this.constructor(this);
    x.s = -x.s;
    return finalise(x);
  };


  /*
   *  n + 0 = n
   *  n + N = N
   *  n + I = I
   *  0 + n = n
   *  0 + 0 = 0
   *  0 + N = N
   *  0 + I = I
   *  N + n = N
   *  N + 0 = N
   *  N + N = N
   *  N + I = N
   *  I + n = I
   *  I + 0 = I
   *  I + N = N
   *  I + I = I
   *
   * Return a new Decimal whose value is the value of this Decimal plus `y`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   */
  P.plus = P.add = function (y) {
    var carry, d, e, i, k, len, pr, rm, xd, yd,
      x = this,
      Ctor = x.constructor;

    y = new Ctor(y);

    // If either is not finite...
    if (!x.d || !y.d) {

      // Return NaN if either is NaN.
      if (!x.s || !y.s) y = new Ctor(NaN);

      // Return x if y is finite and x is ±Infinity.
      // Return x if both are ±Infinity with the same sign.
      // Return NaN if both are ±Infinity with different signs.
      // Return y if x is finite and y is ±Infinity.
      else if (!x.d) y = new Ctor(y.d || x.s === y.s ? x : NaN);

      return y;
    }

     // If signs differ...
    if (x.s != y.s) {
      y.s = -y.s;
      return x.minus(y);
    }

    xd = x.d;
    yd = y.d;
    pr = Ctor.precision;
    rm = Ctor.rounding;

    // If either is zero...
    if (!xd[0] || !yd[0]) {

      // Return x if y is zero.
      // Return y if y is non-zero.
      if (!yd[0]) y = new Ctor(x);

      return external ? finalise(y, pr, rm) : y;
    }

    // x and y are finite, non-zero numbers with the same sign.

    // Calculate base 1e7 exponents.
    k = mathfloor(x.e / LOG_BASE);
    e = mathfloor(y.e / LOG_BASE);

    xd = xd.slice();
    i = k - e;

    // If base 1e7 exponents differ...
    if (i) {

      if (i < 0) {
        d = xd;
        i = -i;
        len = yd.length;
      } else {
        d = yd;
        e = k;
        len = xd.length;
      }

      // Limit number of zeros prepended to max(ceil(pr / LOG_BASE), len) + 1.
      k = Math.ceil(pr / LOG_BASE);
      len = k > len ? k + 1 : len + 1;

      if (i > len) {
        i = len;
        d.length = 1;
      }

      // Prepend zeros to equalise exponents. Note: Faster to use reverse then do unshifts.
      d.reverse();
      for (; i--;) d.push(0);
      d.reverse();
    }

    len = xd.length;
    i = yd.length;

    // If yd is longer than xd, swap xd and yd so xd points to the longer array.
    if (len - i < 0) {
      i = len;
      d = yd;
      yd = xd;
      xd = d;
    }

    // Only start adding at yd.length - 1 as the further digits of xd can be left as they are.
    for (carry = 0; i;) {
      carry = (xd[--i] = xd[i] + yd[i] + carry) / BASE | 0;
      xd[i] %= BASE;
    }

    if (carry) {
      xd.unshift(carry);
      ++e;
    }

    // Remove trailing zeros.
    // No need to check for zero, as +x + +y != 0 && -x + -y != 0
    for (len = xd.length; xd[--len] == 0;) xd.pop();

    y.d = xd;
    y.e = getBase10Exponent(xd, e);

    return external ? finalise(y, pr, rm) : y;
  };


  /*
   * Return the number of significant digits of the value of this Decimal.
   *
   * [z] {boolean|number} Whether to count integer-part trailing zeros: true, false, 1 or 0.
   *
   */
  P.precision = P.sd = function (z) {
    var k,
      x = this;

    if (z !== void 0 && z !== !!z && z !== 1 && z !== 0) throw Error(invalidArgument + z);

    if (x.d) {
      k = getPrecision(x.d);
      if (z && x.e + 1 > k) k = x.e + 1;
    } else {
      k = NaN;
    }

    return k;
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a whole number using
   * rounding mode `rounding`.
   *
   */
  P.round = function () {
    var x = this,
      Ctor = x.constructor;

    return finalise(new Ctor(x), x.e + 1, Ctor.rounding);
  };


  /*
   * Return a new Decimal whose value is the sine of the value in radians of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-1, 1]
   *
   * sin(x) = x - x^3/3! + x^5/5! - ...
   *
   * sin(0)         = 0
   * sin(-0)        = -0
   * sin(Infinity)  = NaN
   * sin(-Infinity) = NaN
   * sin(NaN)       = NaN
   *
   */
  P.sine = P.sin = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite()) return new Ctor(NaN);
    if (x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(x.e, x.sd()) + LOG_BASE;
    Ctor.rounding = 1;

    x = sine(Ctor, toLessThanHalfPi(Ctor, x));

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return finalise(quadrant > 2 ? x.neg() : x, pr, rm, true);
  };


  /*
   * Return a new Decimal whose value is the square root of this Decimal, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   *  sqrt(-n) =  N
   *  sqrt(N)  =  N
   *  sqrt(-I) =  N
   *  sqrt(I)  =  I
   *  sqrt(0)  =  0
   *  sqrt(-0) = -0
   *
   */
  P.squareRoot = P.sqrt = function () {
    var m, n, sd, r, rep, t,
      x = this,
      d = x.d,
      e = x.e,
      s = x.s,
      Ctor = x.constructor;

    // Negative/NaN/Infinity/zero?
    if (s !== 1 || !d || !d[0]) {
      return new Ctor(!s || s < 0 && (!d || d[0]) ? NaN : d ? x : 1 / 0);
    }

    external = false;

    // Initial estimate.
    s = Math.sqrt(+x);

    // Math.sqrt underflow/overflow?
    // Pass x to Math.sqrt as integer, then adjust the exponent of the result.
    if (s == 0 || s == 1 / 0) {
      n = digitsToString(d);

      if ((n.length + e) % 2 == 0) n += '0';
      s = Math.sqrt(n);
      e = mathfloor((e + 1) / 2) - (e < 0 || e % 2);

      if (s == 1 / 0) {
        n = '5e' + e;
      } else {
        n = s.toExponential();
        n = n.slice(0, n.indexOf('e') + 1) + e;
      }

      r = new Ctor(n);
    } else {
      r = new Ctor(s.toString());
    }

    sd = (e = Ctor.precision) + 3;

    // Newton-Raphson iteration.
    for (;;) {
      t = r;
      r = t.plus(divide(x, t, sd + 2, 1)).times(0.5);

      // TODO? Replace with for-loop and checkRoundingDigits.
      if (digitsToString(t.d).slice(0, sd) === (n = digitsToString(r.d)).slice(0, sd)) {
        n = n.slice(sd - 3, sd + 1);

        // The 4th rounding digit may be in error by -1 so if the 4 rounding digits are 9999 or
        // 4999, i.e. approaching a rounding boundary, continue the iteration.
        if (n == '9999' || !rep && n == '4999') {

          // On the first iteration only, check to see if rounding up gives the exact result as the
          // nines may infinitely repeat.
          if (!rep) {
            finalise(t, e + 1, 0);

            if (t.times(t).eq(x)) {
              r = t;
              break;
            }
          }

          sd += 4;
          rep = 1;
        } else {

          // If the rounding digits are null, 0{0,4} or 50{0,3}, check for an exact result.
          // If not, then there are further digits and m will be truthy.
          if (!+n || !+n.slice(1) && n.charAt(0) == '5') {

            // Truncate to the first rounding digit.
            finalise(r, e + 1, 1);
            m = !r.times(r).eq(x);
          }

          break;
        }
      }
    }

    external = true;

    return finalise(r, e, Ctor.rounding, m);
  };


  /*
   * Return a new Decimal whose value is the tangent of the value in radians of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-Infinity, Infinity]
   *
   * tan(0)         = 0
   * tan(-0)        = -0
   * tan(Infinity)  = NaN
   * tan(-Infinity) = NaN
   * tan(NaN)       = NaN
   *
   */
  P.tangent = P.tan = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite()) return new Ctor(NaN);
    if (x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + 10;
    Ctor.rounding = 1;

    x = x.sin();
    x.s = 1;
    x = divide(x, new Ctor(1).minus(x.times(x)).sqrt(), pr + 10, 0);

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return finalise(quadrant == 2 || quadrant == 4 ? x.neg() : x, pr, rm, true);
  };


  /*
   *  n * 0 = 0
   *  n * N = N
   *  n * I = I
   *  0 * n = 0
   *  0 * 0 = 0
   *  0 * N = N
   *  0 * I = N
   *  N * n = N
   *  N * 0 = N
   *  N * N = N
   *  N * I = N
   *  I * n = I
   *  I * 0 = N
   *  I * N = N
   *  I * I = I
   *
   * Return a new Decimal whose value is this Decimal times `y`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   */
  P.times = P.mul = function (y) {
    var carry, e, i, k, r, rL, t, xdL, ydL,
      x = this,
      Ctor = x.constructor,
      xd = x.d,
      yd = (y = new Ctor(y)).d;

    y.s *= x.s;

     // If either is NaN, ±Infinity or ±0...
    if (!xd || !xd[0] || !yd || !yd[0]) {

      return new Ctor(!y.s || xd && !xd[0] && !yd || yd && !yd[0] && !xd

        // Return NaN if either is NaN.
        // Return NaN if x is ±0 and y is ±Infinity, or y is ±0 and x is ±Infinity.
        ? NaN

        // Return ±Infinity if either is ±Infinity.
        // Return ±0 if either is ±0.
        : !xd || !yd ? y.s / 0 : y.s * 0);
    }

    e = mathfloor(x.e / LOG_BASE) + mathfloor(y.e / LOG_BASE);
    xdL = xd.length;
    ydL = yd.length;

    // Ensure xd points to the longer array.
    if (xdL < ydL) {
      r = xd;
      xd = yd;
      yd = r;
      rL = xdL;
      xdL = ydL;
      ydL = rL;
    }

    // Initialise the result array with zeros.
    r = [];
    rL = xdL + ydL;
    for (i = rL; i--;) r.push(0);

    // Multiply!
    for (i = ydL; --i >= 0;) {
      carry = 0;
      for (k = xdL + i; k > i;) {
        t = r[k] + yd[i] * xd[k - i - 1] + carry;
        r[k--] = t % BASE | 0;
        carry = t / BASE | 0;
      }

      r[k] = (r[k] + carry) % BASE | 0;
    }

    // Remove trailing zeros.
    for (; !r[--rL];) r.pop();

    if (carry) ++e;
    else r.shift();

    y.d = r;
    y.e = getBase10Exponent(r, e);

    return external ? finalise(y, Ctor.precision, Ctor.rounding) : y;
  };


  /*
   * Return a string representing the value of this Decimal in base 2, round to `sd` significant
   * digits using rounding mode `rm`.
   *
   * If the optional `sd` argument is present then return binary exponential notation.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toBinary = function (sd, rm) {
    return toStringBinary(this, 2, sd, rm);
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a maximum of `dp`
   * decimal places using rounding mode `rm` or `rounding` if `rm` is omitted.
   *
   * If `dp` is omitted, return a new Decimal whose value is the value of this Decimal.
   *
   * [dp] {number} Decimal places. Integer, 0 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toDecimalPlaces = P.toDP = function (dp, rm) {
    var x = this,
      Ctor = x.constructor;

    x = new Ctor(x);
    if (dp === void 0) return x;

    checkInt32(dp, 0, MAX_DIGITS);

    if (rm === void 0) rm = Ctor.rounding;
    else checkInt32(rm, 0, 8);

    return finalise(x, dp + x.e + 1, rm);
  };


  /*
   * Return a string representing the value of this Decimal in exponential notation rounded to
   * `dp` fixed decimal places using rounding mode `rounding`.
   *
   * [dp] {number} Decimal places. Integer, 0 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toExponential = function (dp, rm) {
    var str,
      x = this,
      Ctor = x.constructor;

    if (dp === void 0) {
      str = finiteToString(x, true);
    } else {
      checkInt32(dp, 0, MAX_DIGITS);

      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);

      x = finalise(new Ctor(x), dp + 1, rm);
      str = finiteToString(x, true, dp + 1);
    }

    return x.isNeg() && !x.isZero() ? '-' + str : str;
  };


  /*
   * Return a string representing the value of this Decimal in normal (fixed-point) notation to
   * `dp` fixed decimal places and rounded using rounding mode `rm` or `rounding` if `rm` is
   * omitted.
   *
   * As with JavaScript numbers, (-0).toFixed(0) is '0', but e.g. (-0.00001).toFixed(0) is '-0'.
   *
   * [dp] {number} Decimal places. Integer, 0 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   * (-0).toFixed(0) is '0', but (-0.1).toFixed(0) is '-0'.
   * (-0).toFixed(1) is '0.0', but (-0.01).toFixed(1) is '-0.0'.
   * (-0).toFixed(3) is '0.000'.
   * (-0.5).toFixed(0) is '-0'.
   *
   */
  P.toFixed = function (dp, rm) {
    var str, y,
      x = this,
      Ctor = x.constructor;

    if (dp === void 0) {
      str = finiteToString(x);
    } else {
      checkInt32(dp, 0, MAX_DIGITS);

      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);

      y = finalise(new Ctor(x), dp + x.e + 1, rm);
      str = finiteToString(y, false, dp + y.e + 1);
    }

    // To determine whether to add the minus sign look at the value before it was rounded,
    // i.e. look at `x` rather than `y`.
    return x.isNeg() && !x.isZero() ? '-' + str : str;
  };


  /*
   * Return an array representing the value of this Decimal as a simple fraction with an integer
   * numerator and an integer denominator.
   *
   * The denominator will be a positive non-zero value less than or equal to the specified maximum
   * denominator. If a maximum denominator is not specified, the denominator will be the lowest
   * value necessary to represent the number exactly.
   *
   * [maxD] {number|string|Decimal} Maximum denominator. Integer >= 1 and < Infinity.
   *
   */
  P.toFraction = function (maxD) {
    var d, d0, d1, d2, e, k, n, n0, n1, pr, q, r,
      x = this,
      xd = x.d,
      Ctor = x.constructor;

    if (!xd) return new Ctor(x);

    n1 = d0 = new Ctor(1);
    d1 = n0 = new Ctor(0);

    d = new Ctor(d1);
    e = d.e = getPrecision(xd) - x.e - 1;
    k = e % LOG_BASE;
    d.d[0] = mathpow(10, k < 0 ? LOG_BASE + k : k);

    if (maxD == null) {

      // d is 10**e, the minimum max-denominator needed.
      maxD = e > 0 ? d : n1;
    } else {
      n = new Ctor(maxD);
      if (!n.isInt() || n.lt(n1)) throw Error(invalidArgument + n);
      maxD = n.gt(d) ? (e > 0 ? d : n1) : n;
    }

    external = false;
    n = new Ctor(digitsToString(xd));
    pr = Ctor.precision;
    Ctor.precision = e = xd.length * LOG_BASE * 2;

    for (;;)  {
      q = divide(n, d, 0, 1, 1);
      d2 = d0.plus(q.times(d1));
      if (d2.cmp(maxD) == 1) break;
      d0 = d1;
      d1 = d2;
      d2 = n1;
      n1 = n0.plus(q.times(d2));
      n0 = d2;
      d2 = d;
      d = n.minus(q.times(d2));
      n = d2;
    }

    d2 = divide(maxD.minus(d0), d1, 0, 1, 1);
    n0 = n0.plus(d2.times(n1));
    d0 = d0.plus(d2.times(d1));
    n0.s = n1.s = x.s;

    // Determine which fraction is closer to x, n0/d0 or n1/d1?
    r = divide(n1, d1, e, 1).minus(x).abs().cmp(divide(n0, d0, e, 1).minus(x).abs()) < 1
        ? [n1, d1] : [n0, d0];

    Ctor.precision = pr;
    external = true;

    return r;
  };


  /*
   * Return a string representing the value of this Decimal in base 16, round to `sd` significant
   * digits using rounding mode `rm`.
   *
   * If the optional `sd` argument is present then return binary exponential notation.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toHexadecimal = P.toHex = function (sd, rm) {
    return toStringBinary(this, 16, sd, rm);
  };


  /*
   * Returns a new Decimal whose value is the nearest multiple of `y` in the direction of rounding
   * mode `rm`, or `Decimal.rounding` if `rm` is omitted, to the value of this Decimal.
   *
   * The return value will always have the same sign as this Decimal, unless either this Decimal
   * or `y` is NaN, in which case the return value will be also be NaN.
   *
   * The return value is not affected by the value of `precision`.
   *
   * y {number|string|Decimal} The magnitude to round to a multiple of.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   * 'toNearest() rounding mode not an integer: {rm}'
   * 'toNearest() rounding mode out of range: {rm}'
   *
   */
  P.toNearest = function (y, rm) {
    var x = this,
      Ctor = x.constructor;

    x = new Ctor(x);

    if (y == null) {

      // If x is not finite, return x.
      if (!x.d) return x;

      y = new Ctor(1);
      rm = Ctor.rounding;
    } else {
      y = new Ctor(y);
      if (rm === void 0) {
        rm = Ctor.rounding;
      } else {
        checkInt32(rm, 0, 8);
      }

      // If x is not finite, return x if y is not NaN, else NaN.
      if (!x.d) return y.s ? x : y;

      // If y is not finite, return Infinity with the sign of x if y is Infinity, else NaN.
      if (!y.d) {
        if (y.s) y.s = x.s;
        return y;
      }
    }

    // If y is not zero, calculate the nearest multiple of y to x.
    if (y.d[0]) {
      external = false;
      x = divide(x, y, 0, rm, 1).times(y);
      external = true;
      finalise(x);

    // If y is zero, return zero with the sign of x.
    } else {
      y.s = x.s;
      x = y;
    }

    return x;
  };


  /*
   * Return the value of this Decimal converted to a number primitive.
   * Zero keeps its sign.
   *
   */
  P.toNumber = function () {
    return +this;
  };


  /*
   * Return a string representing the value of this Decimal in base 8, round to `sd` significant
   * digits using rounding mode `rm`.
   *
   * If the optional `sd` argument is present then return binary exponential notation.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toOctal = function (sd, rm) {
    return toStringBinary(this, 8, sd, rm);
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal raised to the power `y`, rounded
   * to `precision` significant digits using rounding mode `rounding`.
   *
   * ECMAScript compliant.
   *
   *   pow(x, NaN)                           = NaN
   *   pow(x, ±0)                            = 1

   *   pow(NaN, non-zero)                    = NaN
   *   pow(abs(x) > 1, +Infinity)            = +Infinity
   *   pow(abs(x) > 1, -Infinity)            = +0
   *   pow(abs(x) == 1, ±Infinity)           = NaN
   *   pow(abs(x) < 1, +Infinity)            = +0
   *   pow(abs(x) < 1, -Infinity)            = +Infinity
   *   pow(+Infinity, y > 0)                 = +Infinity
   *   pow(+Infinity, y < 0)                 = +0
   *   pow(-Infinity, odd integer > 0)       = -Infinity
   *   pow(-Infinity, even integer > 0)      = +Infinity
   *   pow(-Infinity, odd integer < 0)       = -0
   *   pow(-Infinity, even integer < 0)      = +0
   *   pow(+0, y > 0)                        = +0
   *   pow(+0, y < 0)                        = +Infinity
   *   pow(-0, odd integer > 0)              = -0
   *   pow(-0, even integer > 0)             = +0
   *   pow(-0, odd integer < 0)              = -Infinity
   *   pow(-0, even integer < 0)             = +Infinity
   *   pow(finite x < 0, finite non-integer) = NaN
   *
   * For non-integer or very large exponents pow(x, y) is calculated using
   *
   *   x^y = exp(y*ln(x))
   *
   * Assuming the first 15 rounding digits are each equally likely to be any digit 0-9, the
   * probability of an incorrectly rounded result
   * P([49]9{14} | [50]0{14}) = 2 * 0.2 * 10^-14 = 4e-15 = 1/2.5e+14
   * i.e. 1 in 250,000,000,000,000
   *
   * If a result is incorrectly rounded the maximum error will be 1 ulp (unit in last place).
   *
   * y {number|string|Decimal} The power to which to raise this Decimal.
   *
   */
  P.toPower = P.pow = function (y) {
    var e, k, pr, r, rm, s,
      x = this,
      Ctor = x.constructor,
      yn = +(y = new Ctor(y));

    // Either ±Infinity, NaN or ±0?
    if (!x.d || !y.d || !x.d[0] || !y.d[0]) return new Ctor(mathpow(+x, yn));

    x = new Ctor(x);

    if (x.eq(1)) return x;

    pr = Ctor.precision;
    rm = Ctor.rounding;

    if (y.eq(1)) return finalise(x, pr, rm);

    // y exponent
    e = mathfloor(y.e / LOG_BASE);

    // If y is a small integer use the 'exponentiation by squaring' algorithm.
    if (e >= y.d.length - 1 && (k = yn < 0 ? -yn : yn) <= MAX_SAFE_INTEGER) {
      r = intPow(Ctor, x, k, pr);
      return y.s < 0 ? new Ctor(1).div(r) : finalise(r, pr, rm);
    }

    s = x.s;

    // if x is negative
    if (s < 0) {

      // if y is not an integer
      if (e < y.d.length - 1) return new Ctor(NaN);

      // Result is positive if x is negative and the last digit of integer y is even.
      if ((y.d[e] & 1) == 0) s = 1;

      // if x.eq(-1)
      if (x.e == 0 && x.d[0] == 1 && x.d.length == 1) {
        x.s = s;
        return x;
      }
    }

    // Estimate result exponent.
    // x^y = 10^e,  where e = y * log10(x)
    // log10(x) = log10(x_significand) + x_exponent
    // log10(x_significand) = ln(x_significand) / ln(10)
    k = mathpow(+x, yn);
    e = k == 0 || !isFinite(k)
      ? mathfloor(yn * (Math.log('0.' + digitsToString(x.d)) / Math.LN10 + x.e + 1))
      : new Ctor(k + '').e;

    // Exponent estimate may be incorrect e.g. x: 0.999999999999999999, y: 2.29, e: 0, r.e: -1.

    // Overflow/underflow?
    if (e > Ctor.maxE + 1 || e < Ctor.minE - 1) return new Ctor(e > 0 ? s / 0 : 0);

    external = false;
    Ctor.rounding = x.s = 1;

    // Estimate the extra guard digits needed to ensure five correct rounding digits from
    // naturalLogarithm(x). Example of failure without these extra digits (precision: 10):
    // new Decimal(2.32456).pow('2087987436534566.46411')
    // should be 1.162377823e+764914905173815, but is 1.162355823e+764914905173815
    k = Math.min(12, (e + '').length);

    // r = x^y = exp(y*ln(x))
    r = naturalExponential(y.times(naturalLogarithm(x, pr + k)), pr);

    // r may be Infinity, e.g. (0.9999999999999999).pow(-1e+40)
    if (r.d) {

      // Truncate to the required precision plus five rounding digits.
      r = finalise(r, pr + 5, 1);

      // If the rounding digits are [49]9999 or [50]0000 increase the precision by 10 and recalculate
      // the result.
      if (checkRoundingDigits(r.d, pr, rm)) {
        e = pr + 10;

        // Truncate to the increased precision plus five rounding digits.
        r = finalise(naturalExponential(y.times(naturalLogarithm(x, e + k)), e), e + 5, 1);

        // Check for 14 nines from the 2nd rounding digit (the first rounding digit may be 4 or 9).
        if (+digitsToString(r.d).slice(pr + 1, pr + 15) + 1 == 1e14) {
          r = finalise(r, pr + 1, 0);
        }
      }
    }

    r.s = s;
    external = true;
    Ctor.rounding = rm;

    return finalise(r, pr, rm);
  };


  /*
   * Return a string representing the value of this Decimal rounded to `sd` significant digits
   * using rounding mode `rounding`.
   *
   * Return exponential notation if `sd` is less than the number of digits necessary to represent
   * the integer part of the value in normal notation.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toPrecision = function (sd, rm) {
    var str,
      x = this,
      Ctor = x.constructor;

    if (sd === void 0) {
      str = finiteToString(x, x.e <= Ctor.toExpNeg || x.e >= Ctor.toExpPos);
    } else {
      checkInt32(sd, 1, MAX_DIGITS);

      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);

      x = finalise(new Ctor(x), sd, rm);
      str = finiteToString(x, sd <= x.e || x.e <= Ctor.toExpNeg, sd);
    }

    return x.isNeg() && !x.isZero() ? '-' + str : str;
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a maximum of `sd`
   * significant digits using rounding mode `rm`, or to `precision` and `rounding` respectively if
   * omitted.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   * 'toSD() digits out of range: {sd}'
   * 'toSD() digits not an integer: {sd}'
   * 'toSD() rounding mode not an integer: {rm}'
   * 'toSD() rounding mode out of range: {rm}'
   *
   */
  P.toSignificantDigits = P.toSD = function (sd, rm) {
    var x = this,
      Ctor = x.constructor;

    if (sd === void 0) {
      sd = Ctor.precision;
      rm = Ctor.rounding;
    } else {
      checkInt32(sd, 1, MAX_DIGITS);

      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);
    }

    return finalise(new Ctor(x), sd, rm);
  };


  /*
   * Return a string representing the value of this Decimal.
   *
   * Return exponential notation if this Decimal has a positive exponent equal to or greater than
   * `toExpPos`, or a negative exponent equal to or less than `toExpNeg`.
   *
   */
  P.toString = function () {
    var x = this,
      Ctor = x.constructor,
      str = finiteToString(x, x.e <= Ctor.toExpNeg || x.e >= Ctor.toExpPos);

    return x.isNeg() && !x.isZero() ? '-' + str : str;
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal truncated to a whole number.
   *
   */
  P.truncated = P.trunc = function () {
    return finalise(new this.constructor(this), this.e + 1, 1);
  };


  /*
   * Return a string representing the value of this Decimal.
   * Unlike `toString`, negative zero will include the minus sign.
   *
   */
  P.valueOf = P.toJSON = function () {
    var x = this,
      Ctor = x.constructor,
      str = finiteToString(x, x.e <= Ctor.toExpNeg || x.e >= Ctor.toExpPos);

    return x.isNeg() ? '-' + str : str;
  };


  /*
  // Add aliases to match BigDecimal method names.
  // P.add = P.plus;
  P.subtract = P.minus;
  P.multiply = P.times;
  P.divide = P.div;
  P.remainder = P.mod;
  P.compareTo = P.cmp;
  P.negate = P.neg;
   */


  // Helper functions for Decimal.prototype (P) and/or Decimal methods, and their callers.


  /*
   *  digitsToString           P.cubeRoot, P.logarithm, P.squareRoot, P.toFraction, P.toPower,
   *                           finiteToString, naturalExponential, naturalLogarithm
   *  checkInt32               P.toDecimalPlaces, P.toExponential, P.toFixed, P.toNearest,
   *                           P.toPrecision, P.toSignificantDigits, toStringBinary, random
   *  checkRoundingDigits      P.logarithm, P.toPower, naturalExponential, naturalLogarithm
   *  convertBase              toStringBinary, parseOther
   *  cos                      P.cos
   *  divide                   P.atanh, P.cubeRoot, P.dividedBy, P.dividedToIntegerBy,
   *                           P.logarithm, P.modulo, P.squareRoot, P.tan, P.tanh, P.toFraction,
   *                           P.toNearest, toStringBinary, naturalExponential, naturalLogarithm,
   *                           taylorSeries, atan2, parseOther
   *  finalise                 P.absoluteValue, P.atan, P.atanh, P.ceil, P.cos, P.cosh,
   *                           P.cubeRoot, P.dividedToIntegerBy, P.floor, P.logarithm, P.minus,
   *                           P.modulo, P.negated, P.plus, P.round, P.sin, P.sinh, P.squareRoot,
   *                           P.tan, P.times, P.toDecimalPlaces, P.toExponential, P.toFixed,
   *                           P.toNearest, P.toPower, P.toPrecision, P.toSignificantDigits,
   *                           P.truncated, divide, getLn10, getPi, naturalExponential,
   *                           naturalLogarithm, ceil, floor, round, trunc
   *  finiteToString           P.toExponential, P.toFixed, P.toPrecision, P.toString, P.valueOf,
   *                           toStringBinary
   *  getBase10Exponent        P.minus, P.plus, P.times, parseOther
   *  getLn10                  P.logarithm, naturalLogarithm
   *  getPi                    P.acos, P.asin, P.atan, toLessThanHalfPi, atan2
   *  getPrecision             P.precision, P.toFraction
   *  getZeroString            digitsToString, finiteToString
   *  intPow                   P.toPower, parseOther
   *  isOdd                    toLessThanHalfPi
   *  maxOrMin                 max, min
   *  naturalExponential       P.naturalExponential, P.toPower
   *  naturalLogarithm         P.acosh, P.asinh, P.atanh, P.logarithm, P.naturalLogarithm,
   *                           P.toPower, naturalExponential
   *  nonFiniteToString        finiteToString, toStringBinary
   *  parseDecimal             Decimal
   *  parseOther               Decimal
   *  sin                      P.sin
   *  taylorSeries             P.cosh, P.sinh, cos, sin
   *  toLessThanHalfPi         P.cos, P.sin
   *  toStringBinary           P.toBinary, P.toHexadecimal, P.toOctal
   *  truncate                 intPow
   *
   *  Throws:                  P.logarithm, P.precision, P.toFraction, checkInt32, getLn10, getPi,
   *                           naturalLogarithm, config, parseOther, random, Decimal
   */


  function digitsToString(d) {
    var i, k, ws,
      indexOfLastWord = d.length - 1,
      str = '',
      w = d[0];

    if (indexOfLastWord > 0) {
      str += w;
      for (i = 1; i < indexOfLastWord; i++) {
        ws = d[i] + '';
        k = LOG_BASE - ws.length;
        if (k) str += getZeroString(k);
        str += ws;
      }

      w = d[i];
      ws = w + '';
      k = LOG_BASE - ws.length;
      if (k) str += getZeroString(k);
    } else if (w === 0) {
      return '0';
    }

    // Remove trailing zeros of last w.
    for (; w % 10 === 0;) w /= 10;

    return str + w;
  }


  function checkInt32(i, min, max) {
    if (i !== ~~i || i < min || i > max) {
      throw Error(invalidArgument + i);
    }
  }


  /*
   * Check 5 rounding digits if `repeating` is null, 4 otherwise.
   * `repeating == null` if caller is `log` or `pow`,
   * `repeating != null` if caller is `naturalLogarithm` or `naturalExponential`.
   */
  function checkRoundingDigits(d, i, rm, repeating) {
    var di, k, r, rd;

    // Get the length of the first word of the array d.
    for (k = d[0]; k >= 10; k /= 10) --i;

    // Is the rounding digit in the first word of d?
    if (--i < 0) {
      i += LOG_BASE;
      di = 0;
    } else {
      di = Math.ceil((i + 1) / LOG_BASE);
      i %= LOG_BASE;
    }

    // i is the index (0 - 6) of the rounding digit.
    // E.g. if within the word 3487563 the first rounding digit is 5,
    // then i = 4, k = 1000, rd = 3487563 % 1000 = 563
    k = mathpow(10, LOG_BASE - i);
    rd = d[di] % k | 0;

    if (repeating == null) {
      if (i < 3) {
        if (i == 0) rd = rd / 100 | 0;
        else if (i == 1) rd = rd / 10 | 0;
        r = rm < 4 && rd == 99999 || rm > 3 && rd == 49999 || rd == 50000 || rd == 0;
      } else {
        r = (rm < 4 && rd + 1 == k || rm > 3 && rd + 1 == k / 2) &&
          (d[di + 1] / k / 100 | 0) == mathpow(10, i - 2) - 1 ||
            (rd == k / 2 || rd == 0) && (d[di + 1] / k / 100 | 0) == 0;
      }
    } else {
      if (i < 4) {
        if (i == 0) rd = rd / 1000 | 0;
        else if (i == 1) rd = rd / 100 | 0;
        else if (i == 2) rd = rd / 10 | 0;
        r = (repeating || rm < 4) && rd == 9999 || !repeating && rm > 3 && rd == 4999;
      } else {
        r = ((repeating || rm < 4) && rd + 1 == k ||
        (!repeating && rm > 3) && rd + 1 == k / 2) &&
          (d[di + 1] / k / 1000 | 0) == mathpow(10, i - 3) - 1;
      }
    }

    return r;
  }


  // Convert string of `baseIn` to an array of numbers of `baseOut`.
  // Eg. convertBase('255', 10, 16) returns [15, 15].
  // Eg. convertBase('ff', 16, 10) returns [2, 5, 5].
  function convertBase(str, baseIn, baseOut) {
    var j,
      arr = [0],
      arrL,
      i = 0,
      strL = str.length;

    for (; i < strL;) {
      for (arrL = arr.length; arrL--;) arr[arrL] *= baseIn;
      arr[0] += NUMERALS.indexOf(str.charAt(i++));
      for (j = 0; j < arr.length; j++) {
        if (arr[j] > baseOut - 1) {
          if (arr[j + 1] === void 0) arr[j + 1] = 0;
          arr[j + 1] += arr[j] / baseOut | 0;
          arr[j] %= baseOut;
        }
      }
    }

    return arr.reverse();
  }


  /*
   * cos(x) = 1 - x^2/2! + x^4/4! - ...
   * |x| < pi/2
   *
   */
  function cosine(Ctor, x) {
    var k, y,
      len = x.d.length;

    // Argument reduction: cos(4x) = 8*(cos^4(x) - cos^2(x)) + 1
    // i.e. cos(x) = 8*(cos^4(x/4) - cos^2(x/4)) + 1

    // Estimate the optimum number of times to use the argument reduction.
    if (len < 32) {
      k = Math.ceil(len / 3);
      y = (1 / tinyPow(4, k)).toString();
    } else {
      k = 16;
      y = '2.3283064365386962890625e-10';
    }

    Ctor.precision += k;

    x = taylorSeries(Ctor, 1, x.times(y), new Ctor(1));

    // Reverse argument reduction
    for (var i = k; i--;) {
      var cos2x = x.times(x);
      x = cos2x.times(cos2x).minus(cos2x).times(8).plus(1);
    }

    Ctor.precision -= k;

    return x;
  }


  /*
   * Perform division in the specified base.
   */
  var divide = (function () {

    // Assumes non-zero x and k, and hence non-zero result.
    function multiplyInteger(x, k, base) {
      var temp,
        carry = 0,
        i = x.length;

      for (x = x.slice(); i--;) {
        temp = x[i] * k + carry;
        x[i] = temp % base | 0;
        carry = temp / base | 0;
      }

      if (carry) x.unshift(carry);

      return x;
    }

    function compare(a, b, aL, bL) {
      var i, r;

      if (aL != bL) {
        r = aL > bL ? 1 : -1;
      } else {
        for (i = r = 0; i < aL; i++) {
          if (a[i] != b[i]) {
            r = a[i] > b[i] ? 1 : -1;
            break;
          }
        }
      }

      return r;
    }

    function subtract(a, b, aL, base) {
      var i = 0;

      // Subtract b from a.
      for (; aL--;) {
        a[aL] -= i;
        i = a[aL] < b[aL] ? 1 : 0;
        a[aL] = i * base + a[aL] - b[aL];
      }

      // Remove leading zeros.
      for (; !a[0] && a.length > 1;) a.shift();
    }

    return function (x, y, pr, rm, dp, base) {
      var cmp, e, i, k, logBase, more, prod, prodL, q, qd, rem, remL, rem0, sd, t, xi, xL, yd0,
        yL, yz,
        Ctor = x.constructor,
        sign = x.s == y.s ? 1 : -1,
        xd = x.d,
        yd = y.d;

      // Either NaN, Infinity or 0?
      if (!xd || !xd[0] || !yd || !yd[0]) {

        return new Ctor(// Return NaN if either NaN, or both Infinity or 0.
          !x.s || !y.s || (xd ? yd && xd[0] == yd[0] : !yd) ? NaN :

          // Return ±0 if x is 0 or y is ±Infinity, or return ±Infinity as y is 0.
          xd && xd[0] == 0 || !yd ? sign * 0 : sign / 0);
      }

      if (base) {
        logBase = 1;
        e = x.e - y.e;
      } else {
        base = BASE;
        logBase = LOG_BASE;
        e = mathfloor(x.e / logBase) - mathfloor(y.e / logBase);
      }

      yL = yd.length;
      xL = xd.length;
      q = new Ctor(sign);
      qd = q.d = [];

      // Result exponent may be one less than e.
      // The digit array of a Decimal from toStringBinary may have trailing zeros.
      for (i = 0; yd[i] == (xd[i] || 0); i++);

      if (yd[i] > (xd[i] || 0)) e--;

      if (pr == null) {
        sd = pr = Ctor.precision;
        rm = Ctor.rounding;
      } else if (dp) {
        sd = pr + (x.e - y.e) + 1;
      } else {
        sd = pr;
      }

      if (sd < 0) {
        qd.push(1);
        more = true;
      } else {

        // Convert precision in number of base 10 digits to base 1e7 digits.
        sd = sd / logBase + 2 | 0;
        i = 0;

        // divisor < 1e7
        if (yL == 1) {
          k = 0;
          yd = yd[0];
          sd++;

          // k is the carry.
          for (; (i < xL || k) && sd--; i++) {
            t = k * base + (xd[i] || 0);
            qd[i] = t / yd | 0;
            k = t % yd | 0;
          }

          more = k || i < xL;

        // divisor >= 1e7
        } else {

          // Normalise xd and yd so highest order digit of yd is >= base/2
          k = base / (yd[0] + 1) | 0;

          if (k > 1) {
            yd = multiplyInteger(yd, k, base);
            xd = multiplyInteger(xd, k, base);
            yL = yd.length;
            xL = xd.length;
          }

          xi = yL;
          rem = xd.slice(0, yL);
          remL = rem.length;

          // Add zeros to make remainder as long as divisor.
          for (; remL < yL;) rem[remL++] = 0;

          yz = yd.slice();
          yz.unshift(0);
          yd0 = yd[0];

          if (yd[1] >= base / 2) ++yd0;

          do {
            k = 0;

            // Compare divisor and remainder.
            cmp = compare(yd, rem, yL, remL);

            // If divisor < remainder.
            if (cmp < 0) {

              // Calculate trial digit, k.
              rem0 = rem[0];
              if (yL != remL) rem0 = rem0 * base + (rem[1] || 0);

              // k will be how many times the divisor goes into the current remainder.
              k = rem0 / yd0 | 0;

              //  Algorithm:
              //  1. product = divisor * trial digit (k)
              //  2. if product > remainder: product -= divisor, k--
              //  3. remainder -= product
              //  4. if product was < remainder at 2:
              //    5. compare new remainder and divisor
              //    6. If remainder > divisor: remainder -= divisor, k++

              if (k > 1) {
                if (k >= base) k = base - 1;

                // product = divisor * trial digit.
                prod = multiplyInteger(yd, k, base);
                prodL = prod.length;
                remL = rem.length;

                // Compare product and remainder.
                cmp = compare(prod, rem, prodL, remL);

                // product > remainder.
                if (cmp == 1) {
                  k--;

                  // Subtract divisor from product.
                  subtract(prod, yL < prodL ? yz : yd, prodL, base);
                }
              } else {

                // cmp is -1.
                // If k is 0, there is no need to compare yd and rem again below, so change cmp to 1
                // to avoid it. If k is 1 there is a need to compare yd and rem again below.
                if (k == 0) cmp = k = 1;
                prod = yd.slice();
              }

              prodL = prod.length;
              if (prodL < remL) prod.unshift(0);

              // Subtract product from remainder.
              subtract(rem, prod, remL, base);

              // If product was < previous remainder.
              if (cmp == -1) {
                remL = rem.length;

                // Compare divisor and new remainder.
                cmp = compare(yd, rem, yL, remL);

                // If divisor < new remainder, subtract divisor from remainder.
                if (cmp < 1) {
                  k++;

                  // Subtract divisor from remainder.
                  subtract(rem, yL < remL ? yz : yd, remL, base);
                }
              }

              remL = rem.length;
            } else if (cmp === 0) {
              k++;
              rem = [0];
            }    // if cmp === 1, k will be 0

            // Add the next digit, k, to the result array.
            qd[i++] = k;

            // Update the remainder.
            if (cmp && rem[0]) {
              rem[remL++] = xd[xi] || 0;
            } else {
              rem = [xd[xi]];
              remL = 1;
            }

          } while ((xi++ < xL || rem[0] !== void 0) && sd--);

          more = rem[0] !== void 0;
        }

        // Leading zero?
        if (!qd[0]) qd.shift();
      }

      // logBase is 1 when divide is being used for base conversion.
      if (logBase == 1) {
        q.e = e;
        inexact = more;
      } else {

        // To calculate q.e, first get the number of digits of qd[0].
        for (i = 1, k = qd[0]; k >= 10; k /= 10) i++;
        q.e = i + e * logBase - 1;

        finalise(q, dp ? pr + q.e + 1 : pr, rm, more);
      }

      return q;
    };
  })();


  /*
   * Round `x` to `sd` significant digits using rounding mode `rm`.
   * Check for over/under-flow.
   */
   function finalise(x, sd, rm, isTruncated) {
    var digits, i, j, k, rd, roundUp, w, xd, xdi,
      Ctor = x.constructor;

    // Don't round if sd is null or undefined.
    out: if (sd != null) {
      xd = x.d;

      // Infinity/NaN.
      if (!xd) return x;

      // rd: the rounding digit, i.e. the digit after the digit that may be rounded up.
      // w: the word of xd containing rd, a base 1e7 number.
      // xdi: the index of w within xd.
      // digits: the number of digits of w.
      // i: what would be the index of rd within w if all the numbers were 7 digits long (i.e. if
      // they had leading zeros)
      // j: if > 0, the actual index of rd within w (if < 0, rd is a leading zero).

      // Get the length of the first word of the digits array xd.
      for (digits = 1, k = xd[0]; k >= 10; k /= 10) digits++;
      i = sd - digits;

      // Is the rounding digit in the first word of xd?
      if (i < 0) {
        i += LOG_BASE;
        j = sd;
        w = xd[xdi = 0];

        // Get the rounding digit at index j of w.
        rd = w / mathpow(10, digits - j - 1) % 10 | 0;
      } else {
        xdi = Math.ceil((i + 1) / LOG_BASE);
        k = xd.length;
        if (xdi >= k) {
          if (isTruncated) {

            // Needed by `naturalExponential`, `naturalLogarithm` and `squareRoot`.
            for (; k++ <= xdi;) xd.push(0);
            w = rd = 0;
            digits = 1;
            i %= LOG_BASE;
            j = i - LOG_BASE + 1;
          } else {
            break out;
          }
        } else {
          w = k = xd[xdi];

          // Get the number of digits of w.
          for (digits = 1; k >= 10; k /= 10) digits++;

          // Get the index of rd within w.
          i %= LOG_BASE;

          // Get the index of rd within w, adjusted for leading zeros.
          // The number of leading zeros of w is given by LOG_BASE - digits.
          j = i - LOG_BASE + digits;

          // Get the rounding digit at index j of w.
          rd = j < 0 ? 0 : w / mathpow(10, digits - j - 1) % 10 | 0;
        }
      }

      // Are there any non-zero digits after the rounding digit?
      isTruncated = isTruncated || sd < 0 ||
        xd[xdi + 1] !== void 0 || (j < 0 ? w : w % mathpow(10, digits - j - 1));

      // The expression `w % mathpow(10, digits - j - 1)` returns all the digits of w to the right
      // of the digit at (left-to-right) index j, e.g. if w is 908714 and j is 2, the expression
      // will give 714.

      roundUp = rm < 4
        ? (rd || isTruncated) && (rm == 0 || rm == (x.s < 0 ? 3 : 2))
        : rd > 5 || rd == 5 && (rm == 4 || isTruncated || rm == 6 &&

          // Check whether the digit to the left of the rounding digit is odd.
          ((i > 0 ? j > 0 ? w / mathpow(10, digits - j) : 0 : xd[xdi - 1]) % 10) & 1 ||
            rm == (x.s < 0 ? 8 : 7));

      if (sd < 1 || !xd[0]) {
        xd.length = 0;
        if (roundUp) {

          // Convert sd to decimal places.
          sd -= x.e + 1;

          // 1, 0.1, 0.01, 0.001, 0.0001 etc.
          xd[0] = mathpow(10, (LOG_BASE - sd % LOG_BASE) % LOG_BASE);
          x.e = -sd || 0;
        } else {

          // Zero.
          xd[0] = x.e = 0;
        }

        return x;
      }

      // Remove excess digits.
      if (i == 0) {
        xd.length = xdi;
        k = 1;
        xdi--;
      } else {
        xd.length = xdi + 1;
        k = mathpow(10, LOG_BASE - i);

        // E.g. 56700 becomes 56000 if 7 is the rounding digit.
        // j > 0 means i > number of leading zeros of w.
        xd[xdi] = j > 0 ? (w / mathpow(10, digits - j) % mathpow(10, j) | 0) * k : 0;
      }

      if (roundUp) {
        for (;;) {

          // Is the digit to be rounded up in the first word of xd?
          if (xdi == 0) {

            // i will be the length of xd[0] before k is added.
            for (i = 1, j = xd[0]; j >= 10; j /= 10) i++;
            j = xd[0] += k;
            for (k = 1; j >= 10; j /= 10) k++;

            // if i != k the length has increased.
            if (i != k) {
              x.e++;
              if (xd[0] == BASE) xd[0] = 1;
            }

            break;
          } else {
            xd[xdi] += k;
            if (xd[xdi] != BASE) break;
            xd[xdi--] = 0;
            k = 1;
          }
        }
      }

      // Remove trailing zeros.
      for (i = xd.length; xd[--i] === 0;) xd.pop();
    }

    if (external) {

      // Overflow?
      if (x.e > Ctor.maxE) {

        // Infinity.
        x.d = null;
        x.e = NaN;

      // Underflow?
      } else if (x.e < Ctor.minE) {

        // Zero.
        x.e = 0;
        x.d = [0];
        // Ctor.underflow = true;
      } // else Ctor.underflow = false;
    }

    return x;
  }


  function finiteToString(x, isExp, sd) {
    if (!x.isFinite()) return nonFiniteToString(x);
    var k,
      e = x.e,
      str = digitsToString(x.d),
      len = str.length;

    if (isExp) {
      if (sd && (k = sd - len) > 0) {
        str = str.charAt(0) + '.' + str.slice(1) + getZeroString(k);
      } else if (len > 1) {
        str = str.charAt(0) + '.' + str.slice(1);
      }

      str = str + (x.e < 0 ? 'e' : 'e+') + x.e;
    } else if (e < 0) {
      str = '0.' + getZeroString(-e - 1) + str;
      if (sd && (k = sd - len) > 0) str += getZeroString(k);
    } else if (e >= len) {
      str += getZeroString(e + 1 - len);
      if (sd && (k = sd - e - 1) > 0) str = str + '.' + getZeroString(k);
    } else {
      if ((k = e + 1) < len) str = str.slice(0, k) + '.' + str.slice(k);
      if (sd && (k = sd - len) > 0) {
        if (e + 1 === len) str += '.';
        str += getZeroString(k);
      }
    }

    return str;
  }


  // Calculate the base 10 exponent from the base 1e7 exponent.
  function getBase10Exponent(digits, e) {
    var w = digits[0];

    // Add the number of digits of the first word of the digits array.
    for ( e *= LOG_BASE; w >= 10; w /= 10) e++;
    return e;
  }


  function getLn10(Ctor, sd, pr) {
    if (sd > LN10_PRECISION) {

      // Reset global state in case the exception is caught.
      external = true;
      if (pr) Ctor.precision = pr;
      throw Error(precisionLimitExceeded);
    }
    return finalise(new Ctor(LN10), sd, 1, true);
  }


  function getPi(Ctor, sd, rm) {
    if (sd > PI_PRECISION) throw Error(precisionLimitExceeded);
    return finalise(new Ctor(PI), sd, rm, true);
  }


  function getPrecision(digits) {
    var w = digits.length - 1,
      len = w * LOG_BASE + 1;

    w = digits[w];

    // If non-zero...
    if (w) {

      // Subtract the number of trailing zeros of the last word.
      for (; w % 10 == 0; w /= 10) len--;

      // Add the number of digits of the first word.
      for (w = digits[0]; w >= 10; w /= 10) len++;
    }

    return len;
  }


  function getZeroString(k) {
    var zs = '';
    for (; k--;) zs += '0';
    return zs;
  }


  /*
   * Return a new Decimal whose value is the value of Decimal `x` to the power `n`, where `n` is an
   * integer of type number.
   *
   * Implements 'exponentiation by squaring'. Called by `pow` and `parseOther`.
   *
   */
  function intPow(Ctor, x, n, pr) {
    var isTruncated,
      r = new Ctor(1),

      // Max n of 9007199254740991 takes 53 loop iterations.
      // Maximum digits array length; leaves [28, 34] guard digits.
      k = Math.ceil(pr / LOG_BASE + 4);

    external = false;

    for (;;) {
      if (n % 2) {
        r = r.times(x);
        if (truncate(r.d, k)) isTruncated = true;
      }

      n = mathfloor(n / 2);
      if (n === 0) {

        // To ensure correct rounding when r.d is truncated, increment the last word if it is zero.
        n = r.d.length - 1;
        if (isTruncated && r.d[n] === 0) ++r.d[n];
        break;
      }

      x = x.times(x);
      truncate(x.d, k);
    }

    external = true;

    return r;
  }


  function isOdd(n) {
    return n.d[n.d.length - 1] & 1;
  }


  /*
   * Handle `max` and `min`. `ltgt` is 'lt' or 'gt'.
   */
  function maxOrMin(Ctor, args, ltgt) {
    var y,
      x = new Ctor(args[0]),
      i = 0;

    for (; ++i < args.length;) {
      y = new Ctor(args[i]);
      if (!y.s) {
        x = y;
        break;
      } else if (x[ltgt](y)) {
        x = y;
      }
    }

    return x;
  }


  /*
   * Return a new Decimal whose value is the natural exponential of `x` rounded to `sd` significant
   * digits.
   *
   * Taylor/Maclaurin series.
   *
   * exp(x) = x^0/0! + x^1/1! + x^2/2! + x^3/3! + ...
   *
   * Argument reduction:
   *   Repeat x = x / 32, k += 5, until |x| < 0.1
   *   exp(x) = exp(x / 2^k)^(2^k)
   *
   * Previously, the argument was initially reduced by
   * exp(x) = exp(r) * 10^k  where r = x - k * ln10, k = floor(x / ln10)
   * to first put r in the range [0, ln10], before dividing by 32 until |x| < 0.1, but this was
   * found to be slower than just dividing repeatedly by 32 as above.
   *
   * Max integer argument: exp('20723265836946413') = 6.3e+9000000000000000
   * Min integer argument: exp('-20723265836946411') = 1.2e-9000000000000000
   * (Math object integer min/max: Math.exp(709) = 8.2e+307, Math.exp(-745) = 5e-324)
   *
   *  exp(Infinity)  = Infinity
   *  exp(-Infinity) = 0
   *  exp(NaN)       = NaN
   *  exp(±0)        = 1
   *
   *  exp(x) is non-terminating for any finite, non-zero x.
   *
   *  The result will always be correctly rounded.
   *
   */
  function naturalExponential(x, sd) {
    var denominator, guard, j, pow, sum, t, wpr,
      rep = 0,
      i = 0,
      k = 0,
      Ctor = x.constructor,
      rm = Ctor.rounding,
      pr = Ctor.precision;

    // 0/NaN/Infinity?
    if (!x.d || !x.d[0] || x.e > 17) {

      return new Ctor(x.d
        ? !x.d[0] ? 1 : x.s < 0 ? 0 : 1 / 0
        : x.s ? x.s < 0 ? 0 : x : 0 / 0);
    }

    if (sd == null) {
      external = false;
      wpr = pr;
    } else {
      wpr = sd;
    }

    t = new Ctor(0.03125);

    // while abs(x) >= 0.1
    while (x.e > -2) {

      // x = x / 2^5
      x = x.times(t);
      k += 5;
    }

    // Use 2 * log10(2^k) + 5 (empirically derived) to estimate the increase in precision
    // necessary to ensure the first 4 rounding digits are correct.
    guard = Math.log(mathpow(2, k)) / Math.LN10 * 2 + 5 | 0;
    wpr += guard;
    denominator = pow = sum = new Ctor(1);
    Ctor.precision = wpr;

    for (;;) {
      pow = finalise(pow.times(x), wpr, 1);
      denominator = denominator.times(++i);
      t = sum.plus(divide(pow, denominator, wpr, 1));

      if (digitsToString(t.d).slice(0, wpr) === digitsToString(sum.d).slice(0, wpr)) {
        j = k;
        while (j--) sum = finalise(sum.times(sum), wpr, 1);

        // Check to see if the first 4 rounding digits are [49]999.
        // If so, repeat the summation with a higher precision, otherwise
        // e.g. with precision: 18, rounding: 1
        // exp(18.404272462595034083567793919843761) = 98372560.1229999999 (should be 98372560.123)
        // `wpr - guard` is the index of first rounding digit.
        if (sd == null) {

          if (rep < 3 && checkRoundingDigits(sum.d, wpr - guard, rm, rep)) {
            Ctor.precision = wpr += 10;
            denominator = pow = t = new Ctor(1);
            i = 0;
            rep++;
          } else {
            return finalise(sum, Ctor.precision = pr, rm, external = true);
          }
        } else {
          Ctor.precision = pr;
          return sum;
        }
      }

      sum = t;
    }
  }


  /*
   * Return a new Decimal whose value is the natural logarithm of `x` rounded to `sd` significant
   * digits.
   *
   *  ln(-n)        = NaN
   *  ln(0)         = -Infinity
   *  ln(-0)        = -Infinity
   *  ln(1)         = 0
   *  ln(Infinity)  = Infinity
   *  ln(-Infinity) = NaN
   *  ln(NaN)       = NaN
   *
   *  ln(n) (n != 1) is non-terminating.
   *
   */
  function naturalLogarithm(y, sd) {
    var c, c0, denominator, e, numerator, rep, sum, t, wpr, x1, x2,
      n = 1,
      guard = 10,
      x = y,
      xd = x.d,
      Ctor = x.constructor,
      rm = Ctor.rounding,
      pr = Ctor.precision;

    // Is x negative or Infinity, NaN, 0 or 1?
    if (x.s < 0 || !xd || !xd[0] || !x.e && xd[0] == 1 && xd.length == 1) {
      return new Ctor(xd && !xd[0] ? -1 / 0 : x.s != 1 ? NaN : xd ? 0 : x);
    }

    if (sd == null) {
      external = false;
      wpr = pr;
    } else {
      wpr = sd;
    }

    Ctor.precision = wpr += guard;
    c = digitsToString(xd);
    c0 = c.charAt(0);

    if (Math.abs(e = x.e) < 1.5e15) {

      // Argument reduction.
      // The series converges faster the closer the argument is to 1, so using
      // ln(a^b) = b * ln(a),   ln(a) = ln(a^b) / b
      // multiply the argument by itself until the leading digits of the significand are 7, 8, 9,
      // 10, 11, 12 or 13, recording the number of multiplications so the sum of the series can
      // later be divided by this number, then separate out the power of 10 using
      // ln(a*10^b) = ln(a) + b*ln(10).

      // max n is 21 (gives 0.9, 1.0 or 1.1) (9e15 / 21 = 4.2e14).
      //while (c0 < 9 && c0 != 1 || c0 == 1 && c.charAt(1) > 1) {
      // max n is 6 (gives 0.7 - 1.3)
      while (c0 < 7 && c0 != 1 || c0 == 1 && c.charAt(1) > 3) {
        x = x.times(y);
        c = digitsToString(x.d);
        c0 = c.charAt(0);
        n++;
      }

      e = x.e;

      if (c0 > 1) {
        x = new Ctor('0.' + c);
        e++;
      } else {
        x = new Ctor(c0 + '.' + c.slice(1));
      }
    } else {

      // The argument reduction method above may result in overflow if the argument y is a massive
      // number with exponent >= 1500000000000000 (9e15 / 6 = 1.5e15), so instead recall this
      // function using ln(x*10^e) = ln(x) + e*ln(10).
      t = getLn10(Ctor, wpr + 2, pr).times(e + '');
      x = naturalLogarithm(new Ctor(c0 + '.' + c.slice(1)), wpr - guard).plus(t);
      Ctor.precision = pr;

      return sd == null ? finalise(x, pr, rm, external = true) : x;
    }

    // x1 is x reduced to a value near 1.
    x1 = x;

    // Taylor series.
    // ln(y) = ln((1 + x)/(1 - x)) = 2(x + x^3/3 + x^5/5 + x^7/7 + ...)
    // where x = (y - 1)/(y + 1)    (|x| < 1)
    sum = numerator = x = divide(x.minus(1), x.plus(1), wpr, 1);
    x2 = finalise(x.times(x), wpr, 1);
    denominator = 3;

    for (;;) {
      numerator = finalise(numerator.times(x2), wpr, 1);
      t = sum.plus(divide(numerator, new Ctor(denominator), wpr, 1));

      if (digitsToString(t.d).slice(0, wpr) === digitsToString(sum.d).slice(0, wpr)) {
        sum = sum.times(2);

        // Reverse the argument reduction. Check that e is not 0 because, besides preventing an
        // unnecessary calculation, -0 + 0 = +0 and to ensure correct rounding -0 needs to stay -0.
        if (e !== 0) sum = sum.plus(getLn10(Ctor, wpr + 2, pr).times(e + ''));
        sum = divide(sum, new Ctor(n), wpr, 1);

        // Is rm > 3 and the first 4 rounding digits 4999, or rm < 4 (or the summation has
        // been repeated previously) and the first 4 rounding digits 9999?
        // If so, restart the summation with a higher precision, otherwise
        // e.g. with precision: 12, rounding: 1
        // ln(135520028.6126091714265381533) = 18.7246299999 when it should be 18.72463.
        // `wpr - guard` is the index of first rounding digit.
        if (sd == null) {
          if (checkRoundingDigits(sum.d, wpr - guard, rm, rep)) {
            Ctor.precision = wpr += guard;
            t = numerator = x = divide(x1.minus(1), x1.plus(1), wpr, 1);
            x2 = finalise(x.times(x), wpr, 1);
            denominator = rep = 1;
          } else {
            return finalise(sum, Ctor.precision = pr, rm, external = true);
          }
        } else {
          Ctor.precision = pr;
          return sum;
        }
      }

      sum = t;
      denominator += 2;
    }
  }


  // ±Infinity, NaN.
  function nonFiniteToString(x) {
    // Unsigned.
    return String(x.s * x.s / 0);
  }


  /*
   * Parse the value of a new Decimal `x` from string `str`.
   */
  function parseDecimal(x, str) {
    var e, i, len;

    // Decimal point?
    if ((e = str.indexOf('.')) > -1) str = str.replace('.', '');

    // Exponential form?
    if ((i = str.search(/e/i)) > 0) {

      // Determine exponent.
      if (e < 0) e = i;
      e += +str.slice(i + 1);
      str = str.substring(0, i);
    } else if (e < 0) {

      // Integer.
      e = str.length;
    }

    // Determine leading zeros.
    for (i = 0; str.charCodeAt(i) === 48; i++);

    // Determine trailing zeros.
    for (len = str.length; str.charCodeAt(len - 1) === 48; --len);
    str = str.slice(i, len);

    if (str) {
      len -= i;
      x.e = e = e - i - 1;
      x.d = [];

      // Transform base

      // e is the base 10 exponent.
      // i is where to slice str to get the first word of the digits array.
      i = (e + 1) % LOG_BASE;
      if (e < 0) i += LOG_BASE;

      if (i < len) {
        if (i) x.d.push(+str.slice(0, i));
        for (len -= LOG_BASE; i < len;) x.d.push(+str.slice(i, i += LOG_BASE));
        str = str.slice(i);
        i = LOG_BASE - str.length;
      } else {
        i -= len;
      }

      for (; i--;) str += '0';
      x.d.push(+str);

      if (external) {

        // Overflow?
        if (x.e > x.constructor.maxE) {

          // Infinity.
          x.d = null;
          x.e = NaN;

        // Underflow?
        } else if (x.e < x.constructor.minE) {

          // Zero.
          x.e = 0;
          x.d = [0];
          // x.constructor.underflow = true;
        } // else x.constructor.underflow = false;
      }
    } else {

      // Zero.
      x.e = 0;
      x.d = [0];
    }

    return x;
  }


  /*
   * Parse the value of a new Decimal `x` from a string `str`, which is not a decimal value.
   */
  function parseOther(x, str) {
    var base, Ctor, divisor, i, isFloat, len, p, xd, xe;

    if (str === 'Infinity' || str === 'NaN') {
      if (!+str) x.s = NaN;
      x.e = NaN;
      x.d = null;
      return x;
    }

    if (isHex.test(str))  {
      base = 16;
      str = str.toLowerCase();
    } else if (isBinary.test(str))  {
      base = 2;
    } else if (isOctal.test(str))  {
      base = 8;
    } else {
      throw Error(invalidArgument + str);
    }

    // Is there a binary exponent part?
    i = str.search(/p/i);

    if (i > 0) {
      p = +str.slice(i + 1);
      str = str.substring(2, i);
    } else {
      str = str.slice(2);
    }

    // Convert `str` as an integer then divide the result by `base` raised to a power such that the
    // fraction part will be restored.
    i = str.indexOf('.');
    isFloat = i >= 0;
    Ctor = x.constructor;

    if (isFloat) {
      str = str.replace('.', '');
      len = str.length;
      i = len - i;

      // log[10](16) = 1.2041... , log[10](88) = 1.9444....
      divisor = intPow(Ctor, new Ctor(base), i, i * 2);
    }

    xd = convertBase(str, base, BASE);
    xe = xd.length - 1;

    // Remove trailing zeros.
    for (i = xe; xd[i] === 0; --i) xd.pop();
    if (i < 0) return new Ctor(x.s * 0);
    x.e = getBase10Exponent(xd, xe);
    x.d = xd;
    external = false;

    // At what precision to perform the division to ensure exact conversion?
    // maxDecimalIntegerPartDigitCount = ceil(log[10](b) * otherBaseIntegerPartDigitCount)
    // log[10](2) = 0.30103, log[10](8) = 0.90309, log[10](16) = 1.20412
    // E.g. ceil(1.2 * 3) = 4, so up to 4 decimal digits are needed to represent 3 hex int digits.
    // maxDecimalFractionPartDigitCount = {Hex:4|Oct:3|Bin:1} * otherBaseFractionPartDigitCount
    // Therefore using 4 * the number of digits of str will always be enough.
    if (isFloat) x = divide(x, divisor, len * 4);

    // Multiply by the binary exponent part if present.
    if (p) x = x.times(Math.abs(p) < 54 ? mathpow(2, p) : Decimal.pow(2, p));
    external = true;

    return x;
  }


  /*
   * sin(x) = x - x^3/3! + x^5/5! - ...
   * |x| < pi/2
   *
   */
  function sine(Ctor, x) {
    var k,
      len = x.d.length;

    if (len < 3) return taylorSeries(Ctor, 2, x, x);

    // Argument reduction: sin(5x) = 16*sin^5(x) - 20*sin^3(x) + 5*sin(x)
    // i.e. sin(x) = 16*sin^5(x/5) - 20*sin^3(x/5) + 5*sin(x/5)
    // and  sin(x) = sin(x/5)(5 + sin^2(x/5)(16sin^2(x/5) - 20))

    // Estimate the optimum number of times to use the argument reduction.
    k = 1.4 * Math.sqrt(len);
    k = k > 16 ? 16 : k | 0;

    x = x.times(1 / tinyPow(5, k));
    x = taylorSeries(Ctor, 2, x, x);

    // Reverse argument reduction
    var sin2_x,
      d5 = new Ctor(5),
      d16 = new Ctor(16),
      d20 = new Ctor(20);
    for (; k--;) {
      sin2_x = x.times(x);
      x = x.times(d5.plus(sin2_x.times(d16.times(sin2_x).minus(d20))));
    }

    return x;
  }


  // Calculate Taylor series for `cos`, `cosh`, `sin` and `sinh`.
  function taylorSeries(Ctor, n, x, y, isHyperbolic) {
    var j, t, u, x2,
      pr = Ctor.precision,
      k = Math.ceil(pr / LOG_BASE);

    external = false;
    x2 = x.times(x);
    u = new Ctor(y);

    for (;;) {
      t = divide(u.times(x2), new Ctor(n++ * n++), pr, 1);
      u = isHyperbolic ? y.plus(t) : y.minus(t);
      y = divide(t.times(x2), new Ctor(n++ * n++), pr, 1);
      t = u.plus(y);

      if (t.d[k] !== void 0) {
        for (j = k; t.d[j] === u.d[j] && j--;);
        if (j == -1) break;
      }

      j = u;
      u = y;
      y = t;
      t = j;
    }

    external = true;
    t.d.length = k + 1;

    return t;
  }


  // Exponent e must be positive and non-zero.
  function tinyPow(b, e) {
    var n = b;
    while (--e) n *= b;
    return n;
  }


  // Return the absolute value of `x` reduced to less than or equal to half pi.
  function toLessThanHalfPi(Ctor, x) {
    var t,
      isNeg = x.s < 0,
      pi = getPi(Ctor, Ctor.precision, 1),
      halfPi = pi.times(0.5);

    x = x.abs();

    if (x.lte(halfPi)) {
      quadrant = isNeg ? 4 : 1;
      return x;
    }

    t = x.divToInt(pi);

    if (t.isZero()) {
      quadrant = isNeg ? 3 : 2;
    } else {
      x = x.minus(t.times(pi));

      // 0 <= x < pi
      if (x.lte(halfPi)) {
        quadrant = isOdd(t) ? (isNeg ? 2 : 3) : (isNeg ? 4 : 1);
        return x;
      }

      quadrant = isOdd(t) ? (isNeg ? 1 : 4) : (isNeg ? 3 : 2);
    }

    return x.minus(pi).abs();
  }


  /*
   * Return the value of Decimal `x` as a string in base `baseOut`.
   *
   * If the optional `sd` argument is present include a binary exponent suffix.
   */
  function toStringBinary(x, baseOut, sd, rm) {
    var base, e, i, k, len, roundUp, str, xd, y,
      Ctor = x.constructor,
      isExp = sd !== void 0;

    if (isExp) {
      checkInt32(sd, 1, MAX_DIGITS);
      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);
    } else {
      sd = Ctor.precision;
      rm = Ctor.rounding;
    }

    if (!x.isFinite()) {
      str = nonFiniteToString(x);
    } else {
      str = finiteToString(x);
      i = str.indexOf('.');

      // Use exponential notation according to `toExpPos` and `toExpNeg`? No, but if required:
      // maxBinaryExponent = floor((decimalExponent + 1) * log[2](10))
      // minBinaryExponent = floor(decimalExponent * log[2](10))
      // log[2](10) = 3.321928094887362347870319429489390175864

      if (isExp) {
        base = 2;
        if (baseOut == 16) {
          sd = sd * 4 - 3;
        } else if (baseOut == 8) {
          sd = sd * 3 - 2;
        }
      } else {
        base = baseOut;
      }

      // Convert the number as an integer then divide the result by its base raised to a power such
      // that the fraction part will be restored.

      // Non-integer.
      if (i >= 0) {
        str = str.replace('.', '');
        y = new Ctor(1);
        y.e = str.length - i;
        y.d = convertBase(finiteToString(y), 10, base);
        y.e = y.d.length;
      }

      xd = convertBase(str, 10, base);
      e = len = xd.length;

      // Remove trailing zeros.
      for (; xd[--len] == 0;) xd.pop();

      if (!xd[0]) {
        str = isExp ? '0p+0' : '0';
      } else {
        if (i < 0) {
          e--;
        } else {
          x = new Ctor(x);
          x.d = xd;
          x.e = e;
          x = divide(x, y, sd, rm, 0, base);
          xd = x.d;
          e = x.e;
          roundUp = inexact;
        }

        // The rounding digit, i.e. the digit after the digit that may be rounded up.
        i = xd[sd];
        k = base / 2;
        roundUp = roundUp || xd[sd + 1] !== void 0;

        roundUp = rm < 4
          ? (i !== void 0 || roundUp) && (rm === 0 || rm === (x.s < 0 ? 3 : 2))
          : i > k || i === k && (rm === 4 || roundUp || rm === 6 && xd[sd - 1] & 1 ||
            rm === (x.s < 0 ? 8 : 7));

        xd.length = sd;

        if (roundUp) {

          // Rounding up may mean the previous digit has to be rounded up and so on.
          for (; ++xd[--sd] > base - 1;) {
            xd[sd] = 0;
            if (!sd) {
              ++e;
              xd.unshift(1);
            }
          }
        }

        // Determine trailing zeros.
        for (len = xd.length; !xd[len - 1]; --len);

        // E.g. [4, 11, 15] becomes 4bf.
        for (i = 0, str = ''; i < len; i++) str += NUMERALS.charAt(xd[i]);

        // Add binary exponent suffix?
        if (isExp) {
          if (len > 1) {
            if (baseOut == 16 || baseOut == 8) {
              i = baseOut == 16 ? 4 : 3;
              for (--len; len % i; len++) str += '0';
              xd = convertBase(str, base, baseOut);
              for (len = xd.length; !xd[len - 1]; --len);

              // xd[0] will always be be 1
              for (i = 1, str = '1.'; i < len; i++) str += NUMERALS.charAt(xd[i]);
            } else {
              str = str.charAt(0) + '.' + str.slice(1);
            }
          }

          str =  str + (e < 0 ? 'p' : 'p+') + e;
        } else if (e < 0) {
          for (; ++e;) str = '0' + str;
          str = '0.' + str;
        } else {
          if (++e > len) for (e -= len; e-- ;) str += '0';
          else if (e < len) str = str.slice(0, e) + '.' + str.slice(e);
        }
      }

      str = (baseOut == 16 ? '0x' : baseOut == 2 ? '0b' : baseOut == 8 ? '0o' : '') + str;
    }

    return x.s < 0 ? '-' + str : str;
  }


  // Does not strip trailing zeros.
  function truncate(arr, len) {
    if (arr.length > len) {
      arr.length = len;
      return true;
    }
  }


  // Decimal methods


  /*
   *  abs
   *  acos
   *  acosh
   *  add
   *  asin
   *  asinh
   *  atan
   *  atanh
   *  atan2
   *  cbrt
   *  ceil
   *  clone
   *  config
   *  cos
   *  cosh
   *  div
   *  exp
   *  floor
   *  hypot
   *  ln
   *  log
   *  log2
   *  log10
   *  max
   *  min
   *  mod
   *  mul
   *  pow
   *  random
   *  round
   *  set
   *  sign
   *  sin
   *  sinh
   *  sqrt
   *  sub
   *  tan
   *  tanh
   *  trunc
   */


  /*
   * Return a new Decimal whose value is the absolute value of `x`.
   *
   * x {number|string|Decimal}
   *
   */
  function abs(x) {
    return new this(x).abs();
  }


  /*
   * Return a new Decimal whose value is the arccosine in radians of `x`.
   *
   * x {number|string|Decimal}
   *
   */
  function acos(x) {
    return new this(x).acos();
  }


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic cosine of `x`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function acosh(x) {
    return new this(x).acosh();
  }


  /*
   * Return a new Decimal whose value is the sum of `x` and `y`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function add(x, y) {
    return new this(x).plus(y);
  }


  /*
   * Return a new Decimal whose value is the arcsine in radians of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function asin(x) {
    return new this(x).asin();
  }


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic sine of `x`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function asinh(x) {
    return new this(x).asinh();
  }


  /*
   * Return a new Decimal whose value is the arctangent in radians of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function atan(x) {
    return new this(x).atan();
  }


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic tangent of `x`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function atanh(x) {
    return new this(x).atanh();
  }


  /*
   * Return a new Decimal whose value is the arctangent in radians of `y/x` in the range -pi to pi
   * (inclusive), rounded to `precision` significant digits using rounding mode `rounding`.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-pi, pi]
   *
   * y {number|string|Decimal} The y-coordinate.
   * x {number|string|Decimal} The x-coordinate.
   *
   * atan2(±0, -0)               = ±pi
   * atan2(±0, +0)               = ±0
   * atan2(±0, -x)               = ±pi for x > 0
   * atan2(±0, x)                = ±0 for x > 0
   * atan2(-y, ±0)               = -pi/2 for y > 0
   * atan2(y, ±0)                = pi/2 for y > 0
   * atan2(±y, -Infinity)        = ±pi for finite y > 0
   * atan2(±y, +Infinity)        = ±0 for finite y > 0
   * atan2(±Infinity, x)         = ±pi/2 for finite x
   * atan2(±Infinity, -Infinity) = ±3*pi/4
   * atan2(±Infinity, +Infinity) = ±pi/4
   * atan2(NaN, x) = NaN
   * atan2(y, NaN) = NaN
   *
   */
  function atan2(y, x) {
    y = new this(y);
    x = new this(x);
    var r,
      pr = this.precision,
      rm = this.rounding,
      wpr = pr + 4;

    // Either NaN
    if (!y.s || !x.s) {
      r = new this(NaN);

    // Both ±Infinity
    } else if (!y.d && !x.d) {
      r = getPi(this, wpr, 1).times(x.s > 0 ? 0.25 : 0.75);
      r.s = y.s;

    // x is ±Infinity or y is ±0
    } else if (!x.d || y.isZero()) {
      r = x.s < 0 ? getPi(this, pr, rm) : new this(0);
      r.s = y.s;

    // y is ±Infinity or x is ±0
    } else if (!y.d || x.isZero()) {
      r = getPi(this, wpr, 1).times(0.5);
      r.s = y.s;

    // Both non-zero and finite
    } else if (x.s < 0) {
      this.precision = wpr;
      this.rounding = 1;
      r = this.atan(divide(y, x, wpr, 1));
      x = getPi(this, wpr, 1);
      this.precision = pr;
      this.rounding = rm;
      r = y.s < 0 ? r.minus(x) : r.plus(x);
    } else {
      r = this.atan(divide(y, x, wpr, 1));
    }

    return r;
  }


  /*
   * Return a new Decimal whose value is the cube root of `x`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function cbrt(x) {
    return new this(x).cbrt();
  }


  /*
   * Return a new Decimal whose value is `x` rounded to an integer using `ROUND_CEIL`.
   *
   * x {number|string|Decimal}
   *
   */
  function ceil(x) {
    return finalise(x = new this(x), x.e + 1, 2);
  }


  /*
   * Configure global settings for a Decimal constructor.
   *
   * `obj` is an object with one or more of the following properties,
   *
   *   precision  {number}
   *   rounding   {number}
   *   toExpNeg   {number}
   *   toExpPos   {number}
   *   maxE       {number}
   *   minE       {number}
   *   modulo     {number}
   *   crypto     {boolean|number}
   *   defaults   {true}
   *
   * E.g. Decimal.config({ precision: 20, rounding: 4 })
   *
   */
  function config(obj) {
    if (!obj || typeof obj !== 'object') throw Error(decimalError + 'Object expected');
    var i, p, v,
      useDefaults = obj.defaults === true,
      ps = [
        'precision', 1, MAX_DIGITS,
        'rounding', 0, 8,
        'toExpNeg', -EXP_LIMIT, 0,
        'toExpPos', 0, EXP_LIMIT,
        'maxE', 0, EXP_LIMIT,
        'minE', -EXP_LIMIT, 0,
        'modulo', 0, 9
      ];

    for (i = 0; i < ps.length; i += 3) {
      if (p = ps[i], useDefaults) this[p] = DEFAULTS[p];
      if ((v = obj[p]) !== void 0) {
        if (mathfloor(v) === v && v >= ps[i + 1] && v <= ps[i + 2]) this[p] = v;
        else throw Error(invalidArgument + p + ': ' + v);
      }
    }

    if (p = 'crypto', useDefaults) this[p] = DEFAULTS[p];
    if ((v = obj[p]) !== void 0) {
      if (v === true || v === false || v === 0 || v === 1) {
        if (v) {
          if (typeof crypto != 'undefined' && crypto &&
            (crypto.getRandomValues || crypto.randomBytes)) {
            this[p] = true;
          } else {
            throw Error(cryptoUnavailable);
          }
        } else {
          this[p] = false;
        }
      } else {
        throw Error(invalidArgument + p + ': ' + v);
      }
    }

    return this;
  }


  /*
   * Return a new Decimal whose value is the cosine of `x`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function cos(x) {
    return new this(x).cos();
  }


  /*
   * Return a new Decimal whose value is the hyperbolic cosine of `x`, rounded to precision
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function cosh(x) {
    return new this(x).cosh();
  }


  /*
   * Create and return a Decimal constructor with the same configuration properties as this Decimal
   * constructor.
   *
   */
  function clone(obj) {
    var i, p, ps;

    /*
     * The Decimal constructor and exported function.
     * Return a new Decimal instance.
     *
     * v {number|string|Decimal} A numeric value.
     *
     */
    function Decimal(v) {
      var e, i, t,
        x = this;

      // Decimal called without new.
      if (!(x instanceof Decimal)) return new Decimal(v);

      // Retain a reference to this Decimal constructor, and shadow Decimal.prototype.constructor
      // which points to Object.
      x.constructor = Decimal;

      // Duplicate.
      if (v instanceof Decimal) {
        x.s = v.s;

        if (external) {
          if (!v.d || v.e > Decimal.maxE) {

            // Infinity.
            x.e = NaN;
            x.d = null;
          } else if (v.e < Decimal.minE) {

            // Zero.
            x.e = 0;
            x.d = [0];
          } else {
            x.e = v.e;
            x.d = v.d.slice();
          }
        } else {
          x.e = v.e;
          x.d = v.d ? v.d.slice() : v.d;
        }

        return;
      }

      t = typeof v;

      if (t === 'number') {
        if (v === 0) {
          x.s = 1 / v < 0 ? -1 : 1;
          x.e = 0;
          x.d = [0];
          return;
        }

        if (v < 0) {
          v = -v;
          x.s = -1;
        } else {
          x.s = 1;
        }

        // Fast path for small integers.
        if (v === ~~v && v < 1e7) {
          for (e = 0, i = v; i >= 10; i /= 10) e++;

          if (external) {
            if (e > Decimal.maxE) {
              x.e = NaN;
              x.d = null;
            } else if (e < Decimal.minE) {
              x.e = 0;
              x.d = [0];
            } else {
              x.e = e;
              x.d = [v];
            }
          } else {
            x.e = e;
            x.d = [v];
          }

          return;

        // Infinity, NaN.
        } else if (v * 0 !== 0) {
          if (!v) x.s = NaN;
          x.e = NaN;
          x.d = null;
          return;
        }

        return parseDecimal(x, v.toString());

      } else if (t !== 'string') {
        throw Error(invalidArgument + v);
      }

      // Minus sign?
      if ((i = v.charCodeAt(0)) === 45) {
        v = v.slice(1);
        x.s = -1;
      } else {
        // Plus sign?
        if (i === 43) v = v.slice(1);
        x.s = 1;
      }

      return isDecimal.test(v) ? parseDecimal(x, v) : parseOther(x, v);
    }

    Decimal.prototype = P;

    Decimal.ROUND_UP = 0;
    Decimal.ROUND_DOWN = 1;
    Decimal.ROUND_CEIL = 2;
    Decimal.ROUND_FLOOR = 3;
    Decimal.ROUND_HALF_UP = 4;
    Decimal.ROUND_HALF_DOWN = 5;
    Decimal.ROUND_HALF_EVEN = 6;
    Decimal.ROUND_HALF_CEIL = 7;
    Decimal.ROUND_HALF_FLOOR = 8;
    Decimal.EUCLID = 9;

    Decimal.config = Decimal.set = config;
    Decimal.clone = clone;
    Decimal.isDecimal = isDecimalInstance;

    Decimal.abs = abs;
    Decimal.acos = acos;
    Decimal.acosh = acosh;        // ES6
    Decimal.add = add;
    Decimal.asin = asin;
    Decimal.asinh = asinh;        // ES6
    Decimal.atan = atan;
    Decimal.atanh = atanh;        // ES6
    Decimal.atan2 = atan2;
    Decimal.cbrt = cbrt;          // ES6
    Decimal.ceil = ceil;
    Decimal.cos = cos;
    Decimal.cosh = cosh;          // ES6
    Decimal.div = div;
    Decimal.exp = exp;
    Decimal.floor = floor;
    Decimal.hypot = hypot;        // ES6
    Decimal.ln = ln;
    Decimal.log = log;
    Decimal.log10 = log10;        // ES6
    Decimal.log2 = log2;          // ES6
    Decimal.max = max;
    Decimal.min = min;
    Decimal.mod = mod;
    Decimal.mul = mul;
    Decimal.pow = pow;
    Decimal.random = random;
    Decimal.round = round;
    Decimal.sign = sign;          // ES6
    Decimal.sin = sin;
    Decimal.sinh = sinh;          // ES6
    Decimal.sqrt = sqrt;
    Decimal.sub = sub;
    Decimal.tan = tan;
    Decimal.tanh = tanh;          // ES6
    Decimal.trunc = trunc;        // ES6

    if (obj === void 0) obj = {};
    if (obj) {
      if (obj.defaults !== true) {
        ps = ['precision', 'rounding', 'toExpNeg', 'toExpPos', 'maxE', 'minE', 'modulo', 'crypto'];
        for (i = 0; i < ps.length;) if (!obj.hasOwnProperty(p = ps[i++])) obj[p] = this[p];
      }
    }

    Decimal.config(obj);

    return Decimal;
  }


  /*
   * Return a new Decimal whose value is `x` divided by `y`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function div(x, y) {
    return new this(x).div(y);
  }


  /*
   * Return a new Decimal whose value is the natural exponential of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} The power to which to raise the base of the natural log.
   *
   */
  function exp(x) {
    return new this(x).exp();
  }


  /*
   * Return a new Decimal whose value is `x` round to an integer using `ROUND_FLOOR`.
   *
   * x {number|string|Decimal}
   *
   */
  function floor(x) {
    return finalise(x = new this(x), x.e + 1, 3);
  }


  /*
   * Return a new Decimal whose value is the square root of the sum of the squares of the arguments,
   * rounded to `precision` significant digits using rounding mode `rounding`.
   *
   * hypot(a, b, ...) = sqrt(a^2 + b^2 + ...)
   *
   * arguments {number|string|Decimal}
   *
   */
  function hypot() {
    var i, n,
      t = new this(0);

    external = false;

    for (i = 0; i < arguments.length;) {
      n = new this(arguments[i++]);
      if (!n.d) {
        if (n.s) {
          external = true;
          return new this(1 / 0);
        }
        t = n;
      } else if (t.d) {
        t = t.plus(n.times(n));
      }
    }

    external = true;

    return t.sqrt();
  }


  /*
   * Return true if object is a Decimal instance (where Decimal is any Decimal constructor),
   * otherwise return false.
   *
   */
  function isDecimalInstance(obj) {
    return obj instanceof Decimal || obj && obj.name === '[object Decimal]' || false;
  }


  /*
   * Return a new Decimal whose value is the natural logarithm of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function ln(x) {
    return new this(x).ln();
  }


  /*
   * Return a new Decimal whose value is the log of `x` to the base `y`, or to base 10 if no base
   * is specified, rounded to `precision` significant digits using rounding mode `rounding`.
   *
   * log[y](x)
   *
   * x {number|string|Decimal} The argument of the logarithm.
   * y {number|string|Decimal} The base of the logarithm.
   *
   */
  function log(x, y) {
    return new this(x).log(y);
  }


  /*
   * Return a new Decimal whose value is the base 2 logarithm of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function log2(x) {
    return new this(x).log(2);
  }


  /*
   * Return a new Decimal whose value is the base 10 logarithm of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function log10(x) {
    return new this(x).log(10);
  }


  /*
   * Return a new Decimal whose value is the maximum of the arguments.
   *
   * arguments {number|string|Decimal}
   *
   */
  function max() {
    return maxOrMin(this, arguments, 'lt');
  }


  /*
   * Return a new Decimal whose value is the minimum of the arguments.
   *
   * arguments {number|string|Decimal}
   *
   */
  function min() {
    return maxOrMin(this, arguments, 'gt');
  }


  /*
   * Return a new Decimal whose value is `x` modulo `y`, rounded to `precision` significant digits
   * using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function mod(x, y) {
    return new this(x).mod(y);
  }


  /*
   * Return a new Decimal whose value is `x` multiplied by `y`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function mul(x, y) {
    return new this(x).mul(y);
  }


  /*
   * Return a new Decimal whose value is `x` raised to the power `y`, rounded to precision
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} The base.
   * y {number|string|Decimal} The exponent.
   *
   */
  function pow(x, y) {
    return new this(x).pow(y);
  }


  /*
   * Returns a new Decimal with a random value equal to or greater than 0 and less than 1, and with
   * `sd`, or `Decimal.precision` if `sd` is omitted, significant digits (or less if trailing zeros
   * are produced).
   *
   * [sd] {number} Significant digits. Integer, 0 to MAX_DIGITS inclusive.
   *
   */
  function random(sd) {
    var d, e, k, n,
      i = 0,
      r = new this(1),
      rd = [];

    if (sd === void 0) sd = this.precision;
    else checkInt32(sd, 1, MAX_DIGITS);

    k = Math.ceil(sd / LOG_BASE);

    if (!this.crypto) {
      for (; i < k;) rd[i++] = Math.random() * 1e7 | 0;

    // Browsers supporting crypto.getRandomValues.
    } else if (crypto.getRandomValues) {
      d = crypto.getRandomValues(new Uint32Array(k));

      for (; i < k;) {
        n = d[i];

        // 0 <= n < 4294967296
        // Probability n >= 4.29e9, is 4967296 / 4294967296 = 0.00116 (1 in 865).
        if (n >= 4.29e9) {
          d[i] = crypto.getRandomValues(new Uint32Array(1))[0];
        } else {

          // 0 <= n <= 4289999999
          // 0 <= (n % 1e7) <= 9999999
          rd[i++] = n % 1e7;
        }
      }

    // Node.js supporting crypto.randomBytes.
    } else if (crypto.randomBytes) {

      // buffer
      d = crypto.randomBytes(k *= 4);

      for (; i < k;) {

        // 0 <= n < 2147483648
        n = d[i] + (d[i + 1] << 8) + (d[i + 2] << 16) + ((d[i + 3] & 0x7f) << 24);

        // Probability n >= 2.14e9, is 7483648 / 2147483648 = 0.0035 (1 in 286).
        if (n >= 2.14e9) {
          crypto.randomBytes(4).copy(d, i);
        } else {

          // 0 <= n <= 2139999999
          // 0 <= (n % 1e7) <= 9999999
          rd.push(n % 1e7);
          i += 4;
        }
      }

      i = k / 4;
    } else {
      throw Error(cryptoUnavailable);
    }

    k = rd[--i];
    sd %= LOG_BASE;

    // Convert trailing digits to zeros according to sd.
    if (k && sd) {
      n = mathpow(10, LOG_BASE - sd);
      rd[i] = (k / n | 0) * n;
    }

    // Remove trailing words which are zero.
    for (; rd[i] === 0; i--) rd.pop();

    // Zero?
    if (i < 0) {
      e = 0;
      rd = [0];
    } else {
      e = -1;

      // Remove leading words which are zero and adjust exponent accordingly.
      for (; rd[0] === 0; e -= LOG_BASE) rd.shift();

      // Count the digits of the first word of rd to determine leading zeros.
      for (k = 1, n = rd[0]; n >= 10; n /= 10) k++;

      // Adjust the exponent for leading zeros of the first word of rd.
      if (k < LOG_BASE) e -= LOG_BASE - k;
    }

    r.e = e;
    r.d = rd;

    return r;
  }


  /*
   * Return a new Decimal whose value is `x` rounded to an integer using rounding mode `rounding`.
   *
   * To emulate `Math.round`, set rounding to 7 (ROUND_HALF_CEIL).
   *
   * x {number|string|Decimal}
   *
   */
  function round(x) {
    return finalise(x = new this(x), x.e + 1, this.rounding);
  }


  /*
   * Return
   *   1    if x > 0,
   *  -1    if x < 0,
   *   0    if x is 0,
   *  -0    if x is -0,
   *   NaN  otherwise
   *
   * x {number|string|Decimal}
   *
   */
  function sign(x) {
    x = new this(x);
    return x.d ? (x.d[0] ? x.s : 0 * x.s) : x.s || NaN;
  }


  /*
   * Return a new Decimal whose value is the sine of `x`, rounded to `precision` significant digits
   * using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function sin(x) {
    return new this(x).sin();
  }


  /*
   * Return a new Decimal whose value is the hyperbolic sine of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function sinh(x) {
    return new this(x).sinh();
  }


  /*
   * Return a new Decimal whose value is the square root of `x`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function sqrt(x) {
    return new this(x).sqrt();
  }


  /*
   * Return a new Decimal whose value is `x` minus `y`, rounded to `precision` significant digits
   * using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function sub(x, y) {
    return new this(x).sub(y);
  }


  /*
   * Return a new Decimal whose value is the tangent of `x`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function tan(x) {
    return new this(x).tan();
  }


  /*
   * Return a new Decimal whose value is the hyperbolic tangent of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function tanh(x) {
    return new this(x).tanh();
  }


  /*
   * Return a new Decimal whose value is `x` truncated to an integer.
   *
   * x {number|string|Decimal}
   *
   */
  function trunc(x) {
    return finalise(x = new this(x), x.e + 1, 1);
  }


  // Create and configure initial Decimal constructor.
  Decimal = clone(DEFAULTS);

  Decimal['default'] = Decimal.Decimal = Decimal;

  // Create the internal constants from their string values.
  LN10 = new Decimal(LN10);
  PI = new Decimal(PI);


  // Export.


  // AMD.
  if (module.exports) {
    if (typeof Symbol == 'function' && typeof Symbol.iterator == 'symbol') {
      P[Symbol.for('nodejs.util.inspect.custom')] = P.toString;
      P[Symbol.toStringTag] = 'Decimal';
    }

    module.exports = Decimal;

  // Browser.
  } else {
    if (!globalScope) {
      globalScope = typeof self != 'undefined' && self && self.self == self ? self : window;
    }

    noConflict = globalScope.Decimal;
    Decimal.noConflict = function () {
      globalScope.Decimal = noConflict;
      return Decimal;
    };

    globalScope.Decimal = Decimal;
  }
})(commonjsGlobal);
});

var numberSystem = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.NumberSystem = void 0;
class NumberSystem {
    constructor(name, to) {
        this.to = to;
        this.name = name;
    }
    static get(ns) {
        return NumberSystem.ns[ns];
    }
}
exports.NumberSystem = NumberSystem;
NumberSystem.dec = new NumberSystem('Decimal', (num) => {
    return num.add(0).toString();
});
NumberSystem.hex = new NumberSystem('HexaDecimal', (num) => {
    return num.toHexadecimal();
});
NumberSystem.bin = new NumberSystem('Binary', (num) => {
    return num.toBinary();
});
NumberSystem.oct = new NumberSystem('Octal', (num) => {
    return num.toOctal();
});
NumberSystem.ns = {
    bin: NumberSystem.bin,
    binary: NumberSystem.bin,
    dec: NumberSystem.dec,
    decimal: NumberSystem.dec,
    hex: NumberSystem.hex,
    hexadecimal: NumberSystem.hex,
    oct: NumberSystem.oct,
    octal: NumberSystem.oct,
};
});

var toFormat_1 = createCommonjsModule(function (module) {
/*
 *  toFormat v2.0.0
 *  Adds a toFormat instance method to big.js or decimal.js
 *  Copyright (c) 2017 Michael Mclaughlin
 *  MIT Licence
 */

 /*
 * Adds a `toFormat` method to `Ctor.prototype` and a `format` object to `Ctor`, where `Ctor` is
 * a big number constructor such as `Decimal` (decimal.js) or `Big` (big.js).
 */
function toFormat(Ctor) {

  /*
   *  Returns a string representing the value of this big number in fixed-point notation to `dp`
   *  decimal places using rounding mode `rm`, and formatted according to the properties of the
   * `fmt`, `this.format` and `this.constructor.format` objects, in that order of precedence.
   *
   *  Example:
   *
   *  x = new Decimal('123456789.987654321')
   *
   *  // Add a format object to the constructor...
   *  Decimal.format = {
   *    decimalSeparator: '.',
   *    groupSeparator: ',',
   *    groupSize: 3,
   *    secondaryGroupSize: 0,
   *    fractionGroupSeparator: '',     // '\xA0' non-breaking space
   *    fractionGroupSize : 0
   *  }
   *
   *  x.toFormat();                // 123,456,789.987654321
   *  x.toFormat(2, 1);            // 123,456,789.98
   *
   *  // And/or add a format object to the big number itself...
   *  x.format = {
   *    decimalSeparator: ',',
   *    groupSeparator: '',
   *  }
   *
   *  x.toFormat();                // 123456789,987654321
   *
   *  format = {
   *    decimalSeparator: '.',
   *    groupSeparator: ' ',
   *    groupSize: 3,
   *    fractionGroupSeparator: ' ',     // '\xA0' non-breaking space
   *    fractionGroupSize : 5
   *  }

   *  // And/or pass a format object to the method call.
   *  x.toFormat(format);          // 123 456 789.98765 4321
   *  x.toFormat(4, format);       // 123 456 789.9877
   *  x.toFormat(2, 1, format);    // 123 456 789.98
   *
   *  [dp] {number} Decimal places. Integer.
   *  [rm] {number} Rounding mode. Integer, 0 to 8. (Ignored if using big.js.)
   *  [fmt] {Object} A format object.
   *
   */
  Ctor.prototype.toFormat = function toFormat(dp, rm, fmt) {

    if (!this.e && this.e !== 0) return this.toString();   // Infinity/NaN

    var arr, g1, g2, i,
      u,                             // undefined
      nd,                            // number of integer digits
      intd,                          // integer digits
      intp,                          // integer part
      fracp,                         // fraction part
      dsep,                          // decimalSeparator
      gsep,                          // groupSeparator
      gsize,                         // groupSize
      sgsize,                        // secondaryGroupSize
      fgsep,                         // fractionGroupSeparator
      fgsize,                        // fractionGroupSize
      tfmt = this.format || {},
      cfmt = this.constructor.format || {};

    if (dp != u) {
      if (typeof dp == 'object') {
        fmt = dp;
        dp = u;
      } else if (rm != u) {
        if (typeof rm == 'object') {
          fmt = rm;
          rm = u;
        } else if (typeof fmt != 'object') {
          fmt = {};
        }
      } else {
        fmt = {};
      }
    } else {
      fmt = {};
    }

    arr = this.toFixed(dp, rm).split('.');
    intp = arr[0];
    fracp = arr[1];
    intd = this.s < 0 ? intp.slice(1) : intp;
    nd = intd.length;

    dsep = fmt.decimalSeparator;
    if (dsep == u) {
      dsep = tfmt.decimalSeparator;
      if (dsep == u) {
        dsep = cfmt.decimalSeparator;
        if (dsep == u) dsep = '.';
      }
    }

    gsep = fmt.groupSeparator;
    if (gsep == u) {
      gsep = tfmt.groupSeparator;
      if (gsep == u) gsep = cfmt.groupSeparator;
    }

    if (gsep) {
      gsize = fmt.groupSize;
      if (gsize == u) {
        gsize = tfmt.groupSize;
        if (gsize == u) {
          gsize = cfmt.groupSize;
          if (gsize == u) gsize = 0;
        }
      }

      sgsize = fmt.secondaryGroupSize;
      if (sgsize == u) {
        sgsize = tfmt.secondaryGroupSize;
        if (sgsize == u) {
          sgsize = cfmt.secondaryGroupSize;
          if (sgsize == u) sgsize = 0;
        }
      }

      if (sgsize) {
        g1 = +sgsize;
        g2 = +gsize;
        nd -= g2;
      } else {
        g1 = +gsize;
        g2 = +sgsize;
      }

      if (g1 > 0 && nd > 0) {
        i = nd % g1 || g1;
        intp = intd.substr(0, i);
        for (; i < nd; i += g1) intp += gsep + intd.substr(i, g1);
        if (g2 > 0) intp += gsep + intd.slice(i);
        if (this.s < 0) intp = '-' + intp;
      }
    }

    if (fracp) {
      fgsep = fmt.fractionGroupSeparator;
      if (fgsep == u) {
        fgsep = tfmt.fractionGroupSeparator;
        if (fgsep == u) fgsep = cfmt.fractionGroupSeparator;
      }

      if (fgsep) {
        fgsize = fmt.fractionGroupSize;
        if (fgsize == u) {
          fgsize = tfmt.fractionGroupSize;
          if (fgsize == u) {
            fgsize = cfmt.fractionGroupSize;
            if (fgsize == u) fgsize = 0;
          }
        }

        fgsize = +fgsize;

        if (fgsize) {
          fracp = fracp.replace(new RegExp('\\d{' + fgsize + '}\\B', 'g'), '$&' + fgsep);
        }
      }

      return intp + dsep + fracp;
    } else {

      return intp;
    }
  };

  Ctor.format = {
    decimalSeparator: '.',
    groupSeparator: ',',
    groupSize: 3,
    secondaryGroupSize: 0,
    fractionGroupSeparator: '',
    fractionGroupSize: 0
  };

  return Ctor;
}

if (module.exports) module.exports = toFormat;
});

var datatype = createCommonjsModule(function (module, exports) {
var __importDefault = (commonjsGlobal && commonjsGlobal.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.Type = exports.TYPE_RANK = exports.DATATYPE = void 0;



const toformat_1 = __importDefault(toFormat_1);
toformat_1.default(decimal.Decimal);
var DATATYPE;
(function (DATATYPE) {
    DATATYPE[DATATYPE["NUMBER"] = 0] = "NUMBER";
    DATATYPE[DATATYPE["UNIT"] = 1] = "UNIT";
    DATATYPE[DATATYPE["PERCENTAGE"] = 2] = "PERCENTAGE";
})(DATATYPE = exports.DATATYPE || (exports.DATATYPE = {}));
var TYPE_RANK;
(function (TYPE_RANK) {
    TYPE_RANK[TYPE_RANK["PERCENTAGE"] = 0] = "PERCENTAGE";
    TYPE_RANK[TYPE_RANK["NUMBER"] = 1] = "NUMBER";
    TYPE_RANK[TYPE_RANK["UNIT"] = 2] = "UNIT";
})(TYPE_RANK = exports.TYPE_RANK || (exports.TYPE_RANK = {}));
class Type {
    toString() {
        return this.print();
    }
}
exports.Type = Type;
Type.typeVsStr = { 0: 'number', 1: 'unit', 2: 'percentage' };
/**
 * Represents a type of variable or value
 */
(function (Type) {
    class Numeric extends Type {
        constructor(value) {
            super();
            if (value instanceof decimal.Decimal) {
                this.n = value;
            }
            else {
                this.n = new decimal.Decimal(value);
            }
            this.ns = numberSystem.NumberSystem.dec;
            this.lf = false;
        }
        format() {
            return this.n.toFormat();
        }
        setSystem(numberSys) {
            this.ns = numberSys;
            return this;
        }
        toNumericString() {
            return this.ns.to(this.n);
        }
        print() {
            return this.toNumericString();
        }
        GT(value) {
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                return this.gt(value);
            }
            return value.gt(this);
        }
        GTE(value) {
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                return this.gte(value);
            }
            return value.gte(this);
        }
        LT(value) {
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                return this.lt(value);
            }
            return value.lt(this);
        }
        LTE(value) {
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                return this.lte(value);
            }
            return value.lte(this);
        }
        EQ(value) {
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                return this.eq(value);
            }
            return value.eq(this);
        }
        NEQ(value) {
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                return this.nEq(value);
            }
            return value.nEq(this);
        }
        Add(value) {
            if (!this.n.isFinite() && !value.n.isFinite()) {
                if (!((this.n.isNegative() && value.n.isNegative()) || (this.n.isPositive() && value.n.isPositive()))) {
                    // console.log(left.number, right.number);
                    throw new fcal.FcalError('Subtraction between Infinity is indeterminate');
                }
            }
            // check type to see which datatype operation
            // if both type is same na right variable operation
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                // check type rank to see which will be the return type
                if (this.TYPE_RANK <= value.TYPE_RANK) {
                    return value.New(this.plus(value).n);
                }
                return this.plus(value);
            }
            if (value.TYPE_RANK >= this.TYPE_RANK) {
                return value.plus(this);
            }
            return this.New(value.plus(this).n);
        }
        Sub(value) {
            return this.Add(value.negated());
        }
        times(value) {
            // check type to see which datatype operation
            // if both type is same na right variable operation
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                // check type rank to see which will be the return type
                if (this.TYPE_RANK <= value.TYPE_RANK) {
                    return value.New(this.mul(value).n);
                }
                return this.mul(value);
            }
            if (value.TYPE_RANK >= this.TYPE_RANK) {
                return value.mul(this);
            }
            return this.New(value.mul(this).n);
        }
        divide(value) {
            if (!this.n.isFinite() && !value.n.isFinite()) {
                throw new fcal.FcalError('Division between Infinity is indeterminate');
            }
            // check type to see which datatype operation
            // if both type is same na right variable operation
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                // check type rank to see which will be the return type
                if (this.TYPE_RANK <= value.TYPE_RANK) {
                    if (this.TYPE_RANK === value.TYPE_RANK) {
                        return this.div(value);
                    }
                    return value.New(this.div(value).n);
                }
                return this.div(value);
            }
            if (value.TYPE_RANK >= this.TYPE_RANK) {
                return value.div(this);
            }
            return this.New(value.div(this).n);
        }
        power(value) {
            if (this.isNegative()) {
                if (!value.n.isInt()) {
                    throw new fcal.FcalError(`Pow of operation results in complex number and complex number is not supported yet`);
                }
            }
            // console.log(`CAP ${this.number.toString()} ${value.number.toString()}`);
            // check type to see which datatype operation
            // if both type is same na right variable operation
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                // check type rank to see which will be the return type
                if (this.TYPE_RANK <= value.TYPE_RANK) {
                    if (this.TYPE_RANK === value.TYPE_RANK) {
                        return this.New(this.pow(value).n);
                    }
                    return value.New(this.pow(value).n);
                }
                return this.pow(value);
            }
            if (value.TYPE_RANK >= this.TYPE_RANK) {
                return value.pow(this);
            }
            return this.New(value.pow(this).n);
        }
        modulo(value) {
            if (!this.n.isFinite()) {
                throw new fcal.FcalError('Modulus with Infinity is indeterminate');
            }
            if (value.isZero()) {
                return new Type.BNumber('Infinity');
            }
            // check type to see which datatype operation
            // if both type is same na right variable operation
            this.lf = true;
            if (this.TYPE >= value.TYPE) {
                // check type rank to see which will be the return type
                if (this.TYPE_RANK <= value.TYPE_RANK) {
                    if (this.TYPE_RANK === value.TYPE_RANK) {
                        return this.New(this.mod(value).n);
                    }
                    return value.New(this.mod(value).n);
                }
                return this.mod(value);
            }
            if (value.TYPE_RANK >= this.TYPE_RANK) {
                return value.mod(this);
            }
            return this.New(value.mod(this).n);
        }
        toNumber() {
            return this.n.toNumber();
        }
        trusty() {
            return !this.n.isZero();
        }
        not() {
            return new FcalBoolean(this.n).not();
        }
    }
    Type.Numeric = Numeric;
    /**
     * Basic Number type
     */
    class BNumber extends Numeric {
        constructor(value) {
            super(value);
            this.TYPE = DATATYPE.NUMBER;
            this.TYPE_RANK = TYPE_RANK.NUMBER;
        }
        toFormat() {
            return this.format();
        }
        static New(value) {
            return new BNumber(value);
        }
        gt(value) {
            return new FcalBoolean(this.n.gt(value.n));
        }
        gte(value) {
            return new FcalBoolean(this.n.gte(value.n));
        }
        lt(value) {
            return new FcalBoolean(this.n.lt(value.n));
        }
        lte(value) {
            return new FcalBoolean(this.n.lte(value.n));
        }
        eq(value) {
            return new FcalBoolean(this.n.eq(value.n));
        }
        nEq(value) {
            return this.eq(value).not();
        }
        isZero() {
            return this.n.isZero();
        }
        isNegative() {
            return this.n.isNegative();
        }
        negated() {
            return BNumber.New(this.n.negated());
        }
        div(value) {
            return BNumber.New(this.n.div(value.n));
        }
        pow(value) {
            return BNumber.New(this.n.pow(value.n));
        }
        mod(value) {
            return BNumber.New(this.n.modulo(value.n));
        }
        mul(value) {
            return BNumber.New(this.n.mul(value.n));
        }
        plus(value) {
            return BNumber.New(this.n.plus(value.n));
        }
        New(value) {
            return BNumber.New(value);
        }
    }
    BNumber.ZERO = BNumber.New(new decimal.Decimal(0));
    Type.BNumber = BNumber;
    /**
     * Percentage type
     */
    class Percentage extends Numeric {
        constructor(value) {
            super(value);
            this.TYPE = DATATYPE.PERCENTAGE;
            this.TYPE_RANK = TYPE_RANK.PERCENTAGE;
        }
        toFormat() {
            return `% ${this.format()}`;
        }
        static New(value) {
            return new Percentage(value);
        }
        gt(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return new FcalBoolean(this.n.gt(value.n));
            }
            if (value.lf) {
                return new FcalBoolean(value.n.gt(this.percentageValue(value.n)));
            }
            return new FcalBoolean(this.percentageValue(value.n).gt(value.n));
        }
        gte(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return new FcalBoolean(this.n.gte(value.n));
            }
            if (value.lf) {
                return new FcalBoolean(value.n.gte(this.percentageValue(value.n)));
            }
            return new FcalBoolean(this.percentageValue(value.n).gte(value.n));
        }
        lt(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return new FcalBoolean(this.n.lt(value.n));
            }
            if (value.lf) {
                return new FcalBoolean(value.n.lt(this.percentageValue(value.n)));
            }
            return new FcalBoolean(this.percentageValue(value.n).lt(value.n));
        }
        lte(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return new FcalBoolean(this.n.lte(value.n));
            }
            if (value.lf) {
                return new FcalBoolean(value.n.lte(this.percentageValue(value.n)));
            }
            return new FcalBoolean(this.percentageValue(value.n).lte(value.n));
        }
        eq(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return new FcalBoolean(this.n.eq(value.n));
            }
            return new FcalBoolean(value.n.eq(this.percentageValue(value.n)));
        }
        nEq(value) {
            return this.eq(value).not();
        }
        isZero() {
            return this.n.isZero();
        }
        isNegative() {
            return this.n.isNegative();
        }
        negated() {
            return Percentage.New(this.n.negated());
        }
        plus(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return Percentage.New(this.n.plus(value.n));
            }
            return Percentage.New(value.n.plus(this.percentageValue(value.n)));
        }
        mul(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return Percentage.New(this.n.mul(value.n));
            }
            return Percentage.New(value.n.mul(this.percentageValue(value.n)));
        }
        div(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return Percentage.New(this.n.div(value.n));
            }
            if (value.lf) {
                return Percentage.New(value.n.div(this.percentageValue(value.n)));
            }
            return Percentage.New(this.percentageValue(value.n).div(value.n));
        }
        pow(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return Percentage.New(this.n.pow(value.n));
            }
            if (value.lf) {
                return Percentage.New(value.n.pow(this.percentageValue(value.n)));
            }
            return Percentage.New(this.percentageValue(value.n).pow(value.n));
        }
        mod(value) {
            if (value.TYPE === DATATYPE.PERCENTAGE) {
                return Percentage.New(this.n.mod(value.n));
            }
            if (value.lf) {
                return Percentage.New(value.n.mod(this.percentageValue(value.n)));
            }
            return Percentage.New(this.percentageValue(value.n).mod(value.n));
        }
        percentageValue(value) {
            return value.mul(this.n.div(Percentage.base));
        }
        print() {
            return `% ${this.toNumericString()}`;
        }
        New(value) {
            return Percentage.New(value);
        }
    }
    Percentage.base = new decimal.Decimal(100);
    Type.Percentage = Percentage;
    /**
     * Number with unit
     */
    class UnitNumber extends Numeric {
        constructor(value, unit) {
            super(value);
            this.unit = unit;
            this.TYPE = DATATYPE.UNIT;
            this.TYPE_RANK = TYPE_RANK.UNIT;
        }
        toFormat() {
            if (this.n.lessThanOrEqualTo(1) && !this.n.isNegative()) {
                return `${this.format()} ${this.unit.singular}`;
            }
            return `${this.format()} ${this.unit.plural}`;
        }
        static New(value, unit) {
            return new UnitNumber(value, unit);
        }
        static convertToUnit(value, unit) {
            if (value instanceof UnitNumber) {
                const value2 = value;
                if (value2.unit.id === unit.id && value2.unit.unitType !== unit.unitType) {
                    return UnitNumber.New(value2.convert(unit.ratio, unit.bias), unit).setSystem(value.ns);
                }
            }
            return UnitNumber.New(value.n, unit).setSystem(value.ns);
        }
        New(value) {
            return new UnitNumber(value, this.unit);
        }
        isZero() {
            return this.n.isZero();
        }
        isNegative() {
            return this.n.isNegative();
        }
        negated() {
            return this.New(this.n.negated());
        }
        gt(value) {
            let left;
            let right;
            [left, right] = this.lf ? [this, value] : [value, this];
            if (value instanceof UnitNumber) {
                const left1 = left;
                const right1 = right;
                if (left1.unit.id === right1.unit.id) {
                    return new FcalBoolean(left1.convert(right1.ratio(), right1.bias()).gt(right1.n));
                }
            }
            return new FcalBoolean(left.n.gt(right.n));
        }
        gte(value) {
            let left;
            let right;
            [left, right] = this.lf ? [this, value] : [value, this];
            if (value instanceof UnitNumber) {
                const left1 = left;
                const right1 = right;
                if (left1.unit.id === right1.unit.id) {
                    return new FcalBoolean(left1.convert(right1.ratio(), right1.bias()).gte(right1.n));
                }
            }
            return new FcalBoolean(left.n.gte(right.n));
        }
        lt(value) {
            let left;
            let right;
            [left, right] = this.lf ? [this, value] : [value, this];
            if (value instanceof UnitNumber) {
                const left1 = left;
                const right1 = right;
                if (left1.unit.id === right1.unit.id) {
                    return new FcalBoolean(left1.convert(right1.ratio(), right1.bias()).lt(right1.n));
                }
            }
            return new FcalBoolean(left.n.lt(right.n));
        }
        lte(value) {
            let left;
            let right;
            [left, right] = this.lf ? [this, value] : [value, this];
            if (value instanceof UnitNumber) {
                const left1 = left;
                const right1 = right;
                if (left1.unit.id === right1.unit.id) {
                    return new FcalBoolean(left1.convert(right1.ratio(), right1.bias()).lte(right1.n));
                }
            }
            return new FcalBoolean(left.n.lte(right.n));
        }
        eq(value) {
            let left;
            let right;
            [left, right] = this.lf ? [this, value] : [value, this];
            if (value instanceof UnitNumber) {
                const left1 = left;
                const right1 = right;
                if (left1.unit.id === right1.unit.id) {
                    return new FcalBoolean(left1.convert(right1.ratio(), right1.bias()).eq(right1.n));
                }
            }
            return new FcalBoolean(left.n.eq(right.n));
        }
        nEq(value) {
            return this.eq(value).not();
        }
        plus(value) {
            if (value instanceof UnitNumber) {
                const right = value;
                if (this.unit.id === value.unit.id) {
                    return right.New(this.convert(right.ratio(), right.bias()).add(right.n));
                }
                return value.New(this.n.plus(value.n));
            }
            return this.New(this.n.plus(value.n));
        }
        mul(value) {
            if (value instanceof UnitNumber) {
                const right = value;
                if (this.unit.id === value.unit.id) {
                    return right.New(this.convert(right.ratio(), right.bias()).mul(right.n));
                }
                return value.New(this.n.mul(value.n));
            }
            return this.New(this.n.mul(value.n));
        }
        div(value) {
            let left;
            let right;
            [left, right] = this.lf ? [this, value] : [value, this];
            if (value instanceof UnitNumber) {
                const left1 = left;
                const right1 = right;
                if (left1.unit.unitType === right1.unit.unitType) {
                    return new Type.BNumber(left1.n.div(right1.n));
                }
                if (left1.unit.id !== right1.unit.id) {
                    return left1.New(left1.n.div(right.n));
                }
                return new Type.BNumber(left1.n.div(right1.convert(left1.ratio(), left1.bias())));
            }
            return this.New(left.n.div(right.n));
        }
        pow(value) {
            let left;
            let right;
            [left, right] = this.lf ? [this, value] : [value, this];
            if (value instanceof UnitNumber) {
                const left1 = left;
                const right1 = right;
                if (left1.unit.unitType === right1.unit.unitType) {
                    return left1.New(left1.n.pow(right1.n));
                }
                if (left1.unit.id !== right1.unit.id) {
                    return left1.New(left1.n.pow(right.n));
                }
                return left1.New(left1.n.pow(right1.convert(left1.ratio(), left1.bias())));
            }
            return this.New(left.n.pow(right.n));
        }
        mod(value) {
            let left;
            let right;
            [left, right] = this.lf ? [this, value] : [value, this];
            if (value instanceof UnitNumber) {
                const left1 = left;
                const right1 = right;
                if (left1.unit.id !== right1.unit.id) {
                    return left1.New(left1.n.mod(right1.n));
                }
                if (left1.unit.unitType === right1.unit.unitType) {
                    return left1.New(left1.n.mod(right1.n));
                }
                return left1.New(left1.n.mod(right1.convert(left1.ratio(), left1.bias())));
            }
            return this.New(left.n.mod(right.n));
        }
        convert(ratio, bias) {
            return this.n
                .mul(this.ratio())
                .add(this.bias())
                .minus(bias)
                .div(ratio);
        }
        ratio() {
            return this.unit.ratio;
        }
        bias() {
            return this.unit.bias;
        }
        print() {
            if (this.n.lessThanOrEqualTo(1) && !this.n.isNegative()) {
                return `${this.toNumericString()} ${this.unit.singular}`;
            }
            return `${this.toNumericString()} ${this.unit.plural}`;
        }
    }
    Type.UnitNumber = UnitNumber;
    class FcalBoolean extends BNumber {
        constructor(value) {
            if (typeof value === 'boolean') {
                super(value ? 1 : 0);
                this.v = value;
                return;
            }
            super(value);
            this.v = !this.n.isZero();
        }
        toFormat() {
            throw new Error('Method not implemented.');
        }
        print() {
            return this.v + '';
        }
        not() {
            return this.v ? FcalBoolean.FALSE : FcalBoolean.TRUE;
        }
    }
    FcalBoolean.TRUE = new FcalBoolean(1);
    FcalBoolean.FALSE = new FcalBoolean(0);
    Type.FcalBoolean = FcalBoolean;
})(Type || (Type = {}));
exports.Type = Type;
});

var functions = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDefaultFunctions = void 0;

function getDefaultFunctions() {
    const functions = [
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.abs());
            },
            name: 'abs',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.sqrt());
            },
            name: 'sqrt',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.cbrt());
            },
            name: 'cbrt',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.log());
            },
            name: 'log',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.ln());
            },
            name: 'ln',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.round());
            },
            name: 'round',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.floor());
            },
            name: 'floor',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.ceil());
            },
            name: 'ceil',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.cos());
            },
            name: 'cos',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.acos());
            },
            name: 'acos',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.cosh());
            },
            name: 'cosh',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.acosh());
            },
            name: 'acosh',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.sin());
            },
            name: 'sin',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.asin());
            },
            name: 'asin',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.sinh());
            },
            name: 'sinh',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.asinh());
            },
            name: 'asinh',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.tan());
            },
            name: 'tan',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.atan());
            },
            name: 'atan',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.tanh());
            },
            name: 'tanh',
        },
        {
            arity: 1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const value = args[0];
                return value.New(value.n.atanh());
            },
            name: 'atanh',
        },
        {
            arity: 2,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                const start = args[0];
                const end = args[1];
                start.n = start.n.minus(1);
                return end.n
                    .mul(end.n.plus(1))
                    .div(2)
                    .sub(start.n.mul(start.n.plus(1)).div(2));
            },
            name: 'sigma',
        },
        {
            arity: -1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                if (args.length > 0) {
                    let max = args[0];
                    for (let index = 1; index < args.length; index++) {
                        const element = args[index];
                        if (element.n.gt(max.n)) {
                            max = element;
                        }
                    }
                    return max;
                }
                return datatype.Type.BNumber.New(0);
            },
            name: 'max',
        },
        {
            arity: -1,
            // tslint:disable-next-line: variable-name
            func: (_env, args) => {
                if (args.length > 0) {
                    let min = args[0];
                    for (let index = 1; index < args.length; index++) {
                        const element = args[index];
                        if (element.n.lt(min.n)) {
                            min = element;
                        }
                    }
                    return min;
                }
                return datatype.Type.BNumber.New(0);
            },
            name: 'min',
        },
    ];
    return functions;
}
exports.getDefaultFunctions = getDefaultFunctions;
});

var symboltable = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.SymbolTable = exports.Entity = void 0;

/**
 * SymbolTable maintains registry of words with its types
 */
class SymbolTable {
    /**
     * Create new symbol table
     * @param {SymbolTable | undefined}parent parent of the symbol table
     */
    constructor(parent) {
        if (parent) {
            this.registry = new Map();
            this.parent = parent;
            return;
        }
        this.registry = new Map();
        this.registry.set('bin', Entity.NS);
        this.registry.set('binary', Entity.NS);
        this.registry.set('dec', Entity.NS);
        this.registry.set('decimal', Entity.NS);
        this.registry.set('hex', Entity.NS);
        this.registry.set('hexadecimal', Entity.NS);
        this.registry.set('oct', Entity.NS);
        this.registry.set('octal', Entity.NS);
        this.registry.set('_', Entity.VARIABLE);
    }
    /**
     * Register new phrase or word in symbol table
     * @param {string} phrase phrase
     * @param {Entity} entity type of the phrase
     * @throws {FcalError} if word is already registered
     */
    set(phrase, entity) {
        const c = this.get(phrase);
        if (c) {
            throw new fcal.FcalError(`${phrase} is already used in ${c.toLowerCase()}`);
        }
        this.registry.set(phrase, entity);
    }
    /**
     * search symbol table whether phrase is already registered
     * @param {string} phrase phrase or word
     * @returns {Entity} entity or type of the phrase
     */
    get(phrase) {
        var _a;
        const value = this.registry.get(phrase);
        if (value) {
            return value;
        }
        return (_a = this.parent) === null || _a === void 0 ? void 0 : _a.get(phrase);
    }
}
exports.SymbolTable = SymbolTable;
var Entity;
(function (Entity) {
    Entity["FUNCTION"] = "FUNCTION";
    Entity["VARIABLE"] = "VARIABLE";
    Entity["CONSTANT"] = "CONSTANT";
    Entity["OPERATION_PHRASE"] = "OPERATION PHRASE";
    Entity["NS"] = "NUMBER SYSTEM";
    Entity["UNIT"] = "UNIT";
    Entity["CONVERTER"] = "CONVERTER";
    Entity["SCALE"] = "SCALE";
})(Entity || (Entity = {}));
exports.Entity = Entity;
});

var units$1 = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.UnitMeta = exports.Unit = void 0;


class UnitMeta {
    constructor(id, ratio, unitType) {
        this.id = id;
        this.r = ratio;
        this.b = new decimal.Decimal(0);
        this.unitType = unitType;
        this.plural = unitType;
        this.singular = unitType;
    }
    get ratio() {
        if (this.r instanceof decimal.Decimal) {
            return this.r;
        }
        const value = this.r();
        if (value instanceof decimal.Decimal) {
            return value;
        }
        return new decimal.Decimal(value);
    }
    get bias() {
        if (this.b instanceof decimal.Decimal) {
            return this.b;
        }
        const value = this.b();
        if (value instanceof decimal.Decimal) {
            return value;
        }
        return new decimal.Decimal(value);
    }
    setBias(value) {
        this.b = value;
    }
    setPlural(value) {
        this.plural = value;
    }
    setSingular(value) {
        this.singular = value;
    }
}
exports.UnitMeta = UnitMeta;
/**
 * Represents unit with info
 */
class Unit {
    constructor(id, ratio, unitType, phrases) {
        this.phrases = phrases;
        if (ratio instanceof decimal.Decimal || typeof ratio === 'function') {
            this.meta = new UnitMeta(id, ratio, unitType);
            return;
        }
        this.meta = new UnitMeta(id, new decimal.Decimal(ratio), unitType);
    }
    setBias(value) {
        if (value instanceof decimal.Decimal) {
            this.meta.setBias(value);
            return this;
        }
        if (typeof value === 'function') {
            this.meta.setBias(value);
            return this;
        }
        this.meta.setBias(new decimal.Decimal(value));
        return this;
    }
    Plural(value) {
        this.meta.setPlural(value);
        return this;
    }
    Singular(value) {
        this.meta.setSingular(value);
        return this;
    }
}
exports.Unit = Unit;
// tslint:disable-next-line:no-namespace
(function (Unit) {
    Unit.LENGTH_ID = 'LENGTH';
    Unit.SPEED_ID = 'SPEED';
    Unit.TIME_ID = 'TIME';
    Unit.TEMPERATURE_ID = 'TEMPERATURE';
    Unit.MASS_ID = 'MASS';
    Unit.DIGITAL_ID = 'DIGITAL STORAGE';
    /**
     * List of {Unit} sunits
     */
    class List {
        constructor(symbolTable) {
            this.symbolTable = symbolTable;
            this.units = new Map();
        }
        /**
         * Add a new unit
         * @param {Unit} unit
         * @throws {FcalError} Error if phrases already exists
         */
        push(unit) {
            for (const phrase1 of unit.phrases) {
                this.symbolTable.set(phrase1, symboltable.Entity.UNIT);
                this.units.set(phrase1, unit);
            }
        }
        /**
         * get the unit by its phrase
         * @param {string} phrase
         * @returns {UnitMeta | null }
         */
        get(phrase) {
            const c = this.units.get(phrase);
            if (c) {
                return c.meta;
            }
            return null;
        }
    }
    Unit.List = List;
})(Unit || (Unit = {}));
exports.Unit = Unit;
});

var units = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDefaultUnits = void 0;

function getDefaultUnits() {
    const units = new Array();
    setDistanceUnits(units);
    setSpeedUnits(units);
    setTimeUnits(units);
    setTemperatureUnits(units);
    setMassUnits(units);
    setDigitalStorageUnits(units);
    return units;
}
exports.getDefaultUnits = getDefaultUnits;
function setDistanceUnits(units) {
    units.push(...[
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['cm', 'centimeter', 'centimeters'],
            plural: 'Centimeters',
            ratio: 1,
            singular: 'Centimeter',
            type: 'cm',
        },
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['m', 'meter', 'meters'],
            plural: 'Meters',
            ratio: 100,
            singular: 'Meter',
            type: 'm',
        },
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['mm', 'millimeter', 'millimeters'],
            plural: 'Millimeters',
            ratio: 0.1,
            singular: 'Millimeter',
            type: 'mm',
        },
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['km', 'kilometer', 'kilometers'],
            plural: 'Kilometers',
            ratio: 100000,
            singular: 'Kilometer',
            type: 'km',
        },
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['inch', 'inches'],
            plural: 'Inches',
            ratio: 2.54,
            singular: 'Inch',
            type: 'inch',
        },
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['ft', 'feet', 'foot'],
            plural: 'Feet',
            ratio: 30.48,
            singular: 'Foot',
            type: 'foot/feet',
        },
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['yd', 'yard', 'yards'],
            plural: 'Yards',
            ratio: 91.44,
            singular: 'Yard',
            type: 'yard',
        },
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['mi', 'mile', 'miles'],
            plural: 'Miles',
            ratio: 160934.4,
            singular: 'Mile',
            type: 'mile',
        },
        {
            id: units$1.Unit.LENGTH_ID,
            phrases: ['nmi'],
            ratio: 185200,
            type: 'nautical mile (nmi)',
        },
    ]);
}
function setSpeedUnits(units) {
    units.push(...[
        {
            id: units$1.Unit.SPEED_ID,
            phrases: ['kmh', 'kmph', 'khm', 'kph'],
            ratio: 1,
            type: 'km/h',
        },
        {
            id: units$1.Unit.SPEED_ID,
            phrases: ['mph'],
            ratio: 1.609344,
            type: 'miles/h',
        },
        {
            id: units$1.Unit.SPEED_ID,
            phrases: ['mps'],
            ratio: 3.6,
            type: 'm/s',
        },
        {
            id: units$1.Unit.SPEED_ID,
            phrases: ['fps'],
            ratio: 1.097,
            type: 'ft/s',
        },
        {
            id: units$1.Unit.SPEED_ID,
            phrases: ['kts', 'knots'],
            ratio: 1.852,
            type: 'kt',
        },
    ]);
}
function setTimeUnits(units) {
    units.push(...[
        {
            id: units$1.Unit.TIME_ID,
            phrases: ['nsec', 'nanosecond', 'nanoseconds'],
            plural: 'Nanoseconds',
            ratio: 1e-9,
            singular: 'Nanosecond',
            type: 'nsec',
        },
        {
            id: units$1.Unit.TIME_ID,
            phrases: ['msec', 'microsecond', 'microseconds'],
            plural: 'Microseconds',
            ratio: 1e-6,
            singular: 'Microsecond',
            type: 'msec',
        },
        {
            id: units$1.Unit.TIME_ID,
            phrases: ['ms', 'millisecond', 'milliseconds'],
            plural: 'Milliseconds',
            ratio: 1e-3,
            singular: 'Millisecond',
            type: 'ms',
        },
        {
            id: units$1.Unit.TIME_ID,
            phrases: ['sec', 'second', 'seconds'],
            plural: 'Seconds',
            ratio: 1,
            singular: 'Second',
            type: 'second',
        },
        {
            id: units$1.Unit.TIME_ID,
            phrases: ['minute', 'minutes'],
            plural: 'Minutes',
            ratio: 60,
            singular: 'Minute',
            type: 'minute',
        },
        {
            id: units$1.Unit.TIME_ID,
            phrases: ['hr', 'hour', 'hours'],
            plural: 'Hours',
            ratio: 3600,
            singular: 'Hour',
            type: 'hour',
        },
        {
            id: units$1.Unit.TIME_ID,
            phrases: ['day', 'days'],
            plural: 'Days',
            ratio: 86400,
            singular: 'Day',
            type: 'day',
        },
        {
            id: units$1.Unit.TIME_ID,
            phrases: ['week', 'weeks'],
            plural: 'Weeks',
            ratio: 604800,
            singular: 'Week',
            type: 'week',
        },
    ]);
}
function setTemperatureUnits(units) {
    units.push(...[
        {
            id: units$1.Unit.TEMPERATURE_ID,
            phrases: ['K', 'kelvin'],
            ratio: 1,
            type: 'K',
        },
        {
            bias: '255.3722222222222',
            id: units$1.Unit.TEMPERATURE_ID,
            phrases: ['°F', 'F', 'fahrenheit'],
            ratio: '0.55555555555555555556',
            type: '°F',
        },
        {
            bias: 273.15,
            id: units$1.Unit.TEMPERATURE_ID,
            phrases: ['°C', 'C', 'celsius'],
            ratio: 1,
            type: '°C',
        },
    ]);
}
function setMassUnits(units) {
    units.push(...[
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['gram', 'g', 'grams'],
            ratio: 1,
            type: 'gram',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['tonne', 'tonnes'],
            ratio: 1e6,
            type: 'tonne',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['kg', 'kilogram', 'kilograms'],
            ratio: 1000,
            type: 'kilogram',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['milligram', 'mg', 'milligrams'],
            ratio: 0.001,
            type: 'milligram',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['microgram', 'micrograms'],
            ratio: 1e-6,
            type: 'microgram',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['imperialton'],
            ratio: '1.016e+6',
            type: 'imperialton',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['uston'],
            ratio: '907185',
            type: 'uston',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['stone', 'stones'],
            ratio: '6350.29',
            type: 'stone',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['pound', 'pounds'],
            ratio: '453.592',
            type: 'pound',
        },
        {
            id: units$1.Unit.MASS_ID,
            phrases: ['ounce'],
            ratio: '28.3495',
            type: 'ounce',
        },
    ]);
}
function setDigitalStorageUnits(units) {
    units.push(...[
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['bit'],
            ratio: 1,
            type: 'bit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['kilobit', 'kB'],
            ratio: 1000,
            type: 'kilobit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['kibibit', 'kiB'],
            ratio: 1024,
            type: 'kibibit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['megabit', 'mB'],
            ratio: 1e6,
            type: 'megabit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['mebibit', 'miB'],
            ratio: '1.049e+6',
            type: 'mebibit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['gigabit', 'gB'],
            ratio: 1e9,
            type: 'gigabit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['gibibit', 'giB'],
            ratio: '1.074e+9',
            type: 'gibibit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['terabit', 'tB'],
            ratio: 1e12,
            type: 'terabit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['tebibit', 'tiB'],
            ratio: '1.1e+12',
            type: 'tebibit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['petabit', 'pB'],
            ratio: 1e15,
            type: 'petabit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['pebibit', 'piB'],
            ratio: '1.126e+15',
            type: 'pebibit',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['byte', 'b'],
            ratio: 8,
            type: 'byte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['kilobyte', 'kb'],
            ratio: 8000,
            type: 'kilobyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['kibibyte', 'kib'],
            ratio: 8192,
            type: 'kibibyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['megabyte', 'mb'],
            ratio: 8e6,
            type: 'megabyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['mebibyte', 'mib'],
            ratio: '8.389e+6',
            type: 'mebibyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['gigabyte', 'gb'],
            ratio: 8e9,
            type: 'gigabyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['gibibyte', 'gib'],
            ratio: '8.59e+9',
            type: 'gibibyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['terabyte', 'tb'],
            ratio: 8e12,
            type: 'terabyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['tebibyte', 'tib'],
            ratio: '8.796e+12',
            type: 'tebibyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['petabyte', 'pb'],
            ratio: 8e15,
            type: 'petabyte',
        },
        {
            id: units$1.Unit.DIGITAL_ID,
            phrases: ['pebibyte', 'pib'],
            ratio: '9.007e+15',
            type: 'pebibyte',
        },
    ]);
}
});

var constants = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Constant = void 0;


class Constant {
    constructor(symbolTable) {
        this.values = new Map();
        this.symbolTable = symbolTable;
    }
    get(key) {
        return this.values.get(key);
    }
    /**
     * create or assign a constant with value
     * @param {string} key constants name
     * @param  {Type | Big.Decimal | number | string} value value
     */
    set(key, value) {
        this.symbolTable.set(key, symboltable.Entity.CONSTANT);
        if (value instanceof datatype.Type) {
            this.values.set(key, value);
            return;
        }
        this.values.set(key, datatype.Type.BNumber.New(value));
    }
    /**
     * import values from Object or map into constants
     * @param {Object | Map} values
     */
    use(values) {
        if (values instanceof Map) {
            values.forEach((value, key) => {
                this.set(key, value);
            });
            return;
        }
        for (const key in values) {
            if (values.hasOwnProperty(key)) {
                const element = values[key];
                this.set(key, element);
            }
        }
    }
}
exports.Constant = Constant;
});

var converter = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Converter = void 0;

/**
 * Converter converts one value into another
 */
class Converter {
    /**
     * Create new converter register
     * @param {SymbolTable} st symbol table
     */
    constructor(st) {
        this.st = st;
        this.c = new Map();
    }
    /**
     * Get the converter by its ID or phrase
     * @param {string} id id of the converter or phrase
     * @returns {converterFuncFmt | undefined} converter function
     */
    get(id) {
        return this.c.get(id);
    }
    /**
     * Register new converter function
     * @param id string
     * @param func converter function
     */
    set(id, func) {
        this.st.set(id, symboltable.Entity.CONVERTER);
        this.c.set(id, func);
    }
}
exports.Converter = Converter;
});

var environment = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Environment = void 0;



/**
 * Represents runtime variable environment
 * It represents state of fcal
 */
class Environment {
    /**
     * Creates new environment
     * @param {FcalFunction.List}functions list of functions
     * @param {SymbolTable} symbolTable symbol table
     * @param {Constant} constants constants
     */
    constructor(functions, symbolTable, constants) {
        this.values = new Map();
        this.functions = functions;
        this.symbolTable = symbolTable;
        this.constants = constants;
        this.values.set('_', new datatype.Type.BNumber(0));
    }
    /**
     * Get the value of variable
     * @param {String} key variable name
     * @throws {FcalError} Error if variable is not available
     */
    get(key, start, end) {
        const v = this.values.get(key) || this.constants.get(key);
        if (v) {
            return v;
        }
        throw new fcal.FcalError(`Undefined variable ${key}`, start, end);
    }
    /**
     * create or assign a variable with value
     * @param {string} key variable name
     * @param {ValInputType} value value
     */
    set(key, value) {
        const en = this.symbolTable.get(key);
        if (en && en === symboltable.Entity.CONSTANT) {
            throw new fcal.FcalError(`Can't reassign constant ${key}`);
        }
        if (!this.values.has(key)) {
            this.symbolTable.set(key, symboltable.Entity.VARIABLE);
        }
        if (value instanceof datatype.Type) {
            this.values.set(key, value);
            return;
        }
        this.values.set(key, datatype.Type.BNumber.New(value));
    }
    /**
     * import values from  Object or Map
     * @param {Object | Map} values
     */
    use(values) {
        if (values instanceof Map) {
            values.forEach((value, key) => {
                this.set(key, value);
            });
            return;
        }
        for (const key in values) {
            if (values.hasOwnProperty(key)) {
                const element = values[key];
                this.set(key, element);
            }
        }
    }
}
exports.Environment = Environment;
});

var toJSON = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.JSON_TYPES = exports.ToJSON = void 0;


var JSON_TYPES;
(function (JSON_TYPES) {
    JSON_TYPES["BINARY"] = "binary";
    JSON_TYPES["GROUP"] = "group";
    JSON_TYPES["LITERAL"] = "literal";
    JSON_TYPES["UNARY"] = "unary";
    JSON_TYPES["PERCENTAGE"] = "percentage";
    JSON_TYPES["UNIT"] = "unit";
    JSON_TYPES["CONVERSION"] = "conversion";
    JSON_TYPES["ASSIGN"] = "assign";
    JSON_TYPES["VARIABLE"] = "variable";
    JSON_TYPES["CALL"] = "call";
    JSON_TYPES["LOGICAL"] = "logical";
    JSON_TYPES["TERNARY"] = "ternary";
})(JSON_TYPES || (JSON_TYPES = {}));
exports.JSON_TYPES = JSON_TYPES;
class ToJSON {
    constructor(ast) {
        this.ast = ast;
    }
    toJSON() {
        const astObj = this.toObj();
        return JSON.stringify(astObj);
    }
    toObj() {
        return this.evaluate(this.ast);
    }
    visitBinaryExpr(expr) {
        const right = this.evaluate(expr.right);
        const left = this.evaluate(expr.left);
        const operator = expr.operator;
        return { type: JSON_TYPES.BINARY, right, left, operator };
    }
    visitGroupingExpr(expr) {
        return { type: JSON_TYPES.GROUP, value: this.evaluate(expr.expression) };
    }
    visitLiteralExpr(expr) {
        return { type: JSON_TYPES.LITERAL, value: expr.value.print() };
    }
    visitUnaryExpr(expr) {
        return { type: JSON_TYPES.UNARY, operator: expr.operator, value: this.evaluate(expr.right) };
    }
    visitPercentageExpr(expr) {
        return { type: JSON_TYPES.PERCENTAGE, value: this.evaluate(expr.expression) };
    }
    visitUnitExpr(expr) {
        return { type: JSON_TYPES.UNIT, phrase: expr.phrase, value: this.evaluate(expr.expression) };
    }
    visitConversionExpr(expr) {
        const value = this.evaluate(expr.expression);
        if (expr.to instanceof units$1.UnitMeta) {
            return { type: JSON_TYPES.CONVERSION, unit: expr.name, value };
        }
        if (expr.to instanceof numberSystem.NumberSystem) {
            return { type: JSON_TYPES.CONVERSION, ns: expr.name, value };
        }
        return { type: JSON_TYPES.CONVERSION, converter: expr.name, value };
    }
    visitAssignExpr(expr) {
        return { type: JSON_TYPES.ASSIGN, variable: expr.name, value: this.evaluate(expr.value) };
    }
    visitVariableExpr(expr) {
        return { type: JSON_TYPES.VARIABLE, name: expr.name };
    }
    visitCallExpr(expr) {
        const args = Array();
        for (const arg of expr.argument) {
            args.push(this.evaluate(arg));
        }
        return { type: JSON_TYPES.CALL, name: expr.name, args };
    }
    visitLogicalExpr(expr) {
        const right = this.evaluate(expr.left);
        const left = this.evaluate(expr.left);
        const operator = expr.operator;
        return { type: JSON_TYPES.LOGICAL, right, left, operator };
    }
    visitTernaryExpr(expr) {
        const trueExpr = this.evaluate(expr.trueExpr);
        const falseExpr = this.evaluate(expr.falseExpr);
        const main = this.evaluate(expr.main);
        return { type: JSON_TYPES.TERNARY, main, trueExpr, falseExpr };
    }
    evaluate(expr) {
        const ast = expr.accept(this);
        return Object.assign({ start: expr.start, end: expr.end }, ast);
    }
}
exports.ToJSON = ToJSON;
});

var token = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Token = exports.TT = void 0;
var TT;
(function (TT) {
    TT["PLUS"] = "+";
    TT["MINUS"] = "-";
    TT["TIMES"] = "*";
    TT["MOD"] = "mod";
    TT["SLASH"] = "/";
    TT["Number"] = "number";
    TT["OPEN_PAREN"] = "(";
    TT["CLOSE_PAREN"] = ")";
    TT["NEWLINE"] = "\n";
    TT["EOL"] = "EOL";
    TT["IN"] = "in";
    TT["NAME"] = "name";
    TT["EQUAL"] = "=";
    TT["COMMA"] = ",";
    TT["PERCENTAGE"] = "%";
    TT["OF"] = "of";
    TT["UNIT"] = "unit";
    TT["CAP"] = "^";
    TT["NS"] = "ns";
    TT["DOUBLE_COLON"] = ":";
    TT["FLOOR_DIVIDE"] = "//";
    TT["LESS_EQUAL"] = "<=";
    TT["GREATER_EQUAL"] = ">=";
    TT["LESS_EQUAL_EQUAL"] = "<==";
    TT["GREATER_EQUAL_EQUAL"] = ">==";
    TT["LESS"] = "<";
    TT["GREATER"] = ">";
    TT["EQUAL_EQUAL"] = "==";
    TT["EQUAL_EQUAL_EQUAL"] = "===";
    TT["NOT_EQUAL"] = "!=";
    TT["NOT_EQUAL_EQUAL"] = "!==";
    TT["NOT"] = "!";
    TT["AND"] = "&&";
    TT["OR"] = "||";
    TT["Q"] = "?";
    TT["CC"] = "cc";
    TT["PLUS_EQUAL"] = "+=";
    TT["MINUS_EQUAL"] = "-=";
    TT["DIVIDE_EQUAL"] = "/=";
    TT["FLOOR_DIVIDE_EQUAL"] = "//=";
    TT["MULTIPLY_EQUAL"] = "*=";
    TT["POWER_EQUAL"] = "^=";
    TT["SCALE"] = "scale";
})(TT || (TT = {}));
exports.TT = TT;
class Token {
    constructor(type, lexeme, literal, start, end) {
        this.type = type;
        this.lexeme = lexeme;
        this.start = start;
        this.end = end;
        this.literal = literal;
    }
    static EOL(end) {
        return new Token(TT.EOL, 'EOL', null, end, end);
    }
}
exports.Token = Token;
});

var astPrinter = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.ASTPrinter = void 0;


class ASTPrinter {
    constructor() {
        this.depth = 0;
    }
    static createPrefix(depth, type) {
        return `${this.prefixChar}${'-'.repeat(depth * this.tab)} (${depth / this.tab})${type}`;
    }
    visitTernaryExpr(expr) {
        this.depth += ASTPrinter.tab;
        const main = this.evaluate(expr.main);
        const trueExpr = this.evaluate(expr.trueExpr);
        const falseExpr = this.evaluate(expr.falseExpr);
        this.depth -= ASTPrinter.tab;
        return `${ASTPrinter.createPrefix(this.depth, 'TERNARY')}\n|\n${main}${trueExpr}${falseExpr}`;
    }
    visitCallExpr(expr) {
        let str = `${ASTPrinter.createPrefix(this.depth, 'FUNCTION')} ==> ${expr.name} `;
        this.depth += ASTPrinter.tab;
        for (const arg of expr.argument) {
            str = `${str} \n|\n${this.evaluate(arg)}`;
        }
        this.depth -= ASTPrinter.tab;
        return str;
    }
    visitAssignExpr(expr) {
        this.depth += ASTPrinter.tab;
        const value = this.evaluate(expr.value);
        this.depth -= ASTPrinter.tab;
        return `${ASTPrinter.createPrefix(this.depth, 'ASSIGN')} ${expr.name} \n|\n${value}`;
    }
    visitVariableExpr(expr) {
        return `${ASTPrinter.createPrefix(this.depth, 'VARIABLE')} ${expr.name}\n|\n`;
    }
    visitUnitExpr(expr) {
        this.depth += ASTPrinter.tab;
        const expression = this.evaluate(expr.expression);
        this.depth -= ASTPrinter.tab;
        return `${ASTPrinter.createPrefix(this.depth, 'UNIT')} ${expr.unit.unitType} \n|\n${expression}`;
    }
    visitConversionExpr(expr) {
        this.depth += ASTPrinter.tab;
        const expression = this.evaluate(expr.expression);
        this.depth -= ASTPrinter.tab;
        if (expr.to instanceof units$1.UnitMeta) {
            return `${ASTPrinter.createPrefix(this.depth, 'UNIT CONVERT')} ${expr.name} \n|\n${expression}`;
        }
        if (expr.to instanceof numberSystem.NumberSystem) {
            return `${ASTPrinter.createPrefix(this.depth, 'NUMERICAL SYSTEM')} ${expr.name} \n|\n${expression}`;
        }
        return `${ASTPrinter.createPrefix(this.depth, 'CONVERTER')} ${expr.name} \n|\n${expression}`;
    }
    visitLogicalExpr(expr) {
        this.depth += ASTPrinter.tab;
        const left = this.evaluate(expr.left);
        const right = this.evaluate(expr.right);
        this.depth -= ASTPrinter.tab;
        return `${ASTPrinter.createPrefix(this.depth, 'LOGICAL')}  ${expr.operator.type} \n|\n${left}${right}`;
    }
    visitBinaryExpr(expr) {
        this.depth += ASTPrinter.tab;
        const left = this.evaluate(expr.left);
        const right = this.evaluate(expr.right);
        this.depth -= ASTPrinter.tab;
        return `${ASTPrinter.createPrefix(this.depth, 'BINARY')}  ${expr.operator.type} \n|\n${left}${right}`;
    }
    visitGroupingExpr(expr) {
        this.depth += ASTPrinter.tab;
        const expression = this.evaluate(expr.expression);
        this.depth -= ASTPrinter.tab;
        return `${ASTPrinter.createPrefix(this.depth, 'GROUPING')} \n|\n${expression}`;
    }
    visitLiteralExpr(expr) {
        return `${ASTPrinter.createPrefix(this.depth, 'LITERAL')} ${expr.value.print()}\n|\n`;
    }
    visitUnaryExpr(expr) {
        this.depth += ASTPrinter.tab;
        const expression = this.evaluate(expr.right);
        this.depth -= ASTPrinter.tab;
        return `${ASTPrinter.createPrefix(this.depth, 'UNARY')} ${expr.operator.type} \n|\n${expression}`;
    }
    visitPercentageExpr(expr) {
        this.depth += ASTPrinter.tab;
        const expression = this.evaluate(expr.expression);
        this.depth -= ASTPrinter.tab;
        return `${ASTPrinter.createPrefix(this.depth, 'PERCENTAGE')} \n|\n${expression}`;
    }
    print(expr) {
        return this.evaluate(expr);
    }
    evaluate(expr) {
        const ast = expr.accept(this);
        return ast;
    }
}
exports.ASTPrinter = ASTPrinter;
ASTPrinter.tab = 2;
ASTPrinter.prefixChar = '+';
});

var expr = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Expr = void 0;


class Expr {
    constructor(start, end) {
        this.start = start;
        this.end = end;
    }
    toString() {
        const res = new astPrinter.ASTPrinter().print(this);
        return res.substring(0, res.length - 2);
    }
    eval(visitor) {
        try {
            return this.accept(visitor);
        }
        catch (e) {
            if (e instanceof fcal.FcalError) {
                if (e.start === undefined) {
                    e.start = this.start;
                }
                if (e.end === undefined) {
                    e.end = this.end;
                }
            }
            throw e;
        }
    }
}
exports.Expr = Expr;
(function (Expr) {
    class Binary extends Expr {
        constructor(left, operator, right, start, end) {
            super(start, end);
            this.left = left;
            this.operator = operator;
            this.right = right;
        }
        accept(visitor) {
            return visitor.visitBinaryExpr(this);
        }
    }
    Expr.Binary = Binary;
    class Ternary extends Expr {
        constructor(main, trueExpr, falseExpr, start, end) {
            super(start, end);
            this.main = main;
            this.trueExpr = trueExpr;
            this.falseExpr = falseExpr;
        }
        accept(visitor) {
            return visitor.visitTernaryExpr(this);
        }
    }
    Expr.Ternary = Ternary;
    class Logical extends Expr {
        constructor(left, operator, right, start, end) {
            super(start, end);
            this.left = left;
            this.operator = operator;
            this.right = right;
        }
        accept(visitor) {
            return visitor.visitLogicalExpr(this);
        }
    }
    Expr.Logical = Logical;
    class Grouping extends Expr {
        constructor(expression, start, end) {
            super(start, end);
            this.expression = expression;
        }
        accept(visitor) {
            return visitor.visitGroupingExpr(this);
        }
    }
    Expr.Grouping = Grouping;
    class Assign extends Expr {
        constructor(name, value, start, end) {
            super(start, end);
            this.name = name;
            this.value = value;
        }
        accept(visitor) {
            return visitor.visitAssignExpr(this);
        }
    }
    Expr.Assign = Assign;
    class Variable extends Expr {
        constructor(name, start, end) {
            super(start, end);
            this.name = name;
        }
        accept(visitor) {
            return visitor.visitVariableExpr(this);
        }
    }
    Expr.Variable = Variable;
    class Call extends Expr {
        constructor(name, argument, start, end) {
            super(start, end);
            this.name = name;
            this.argument = argument;
        }
        accept(visitor) {
            return visitor.visitCallExpr(this);
        }
    }
    Expr.Call = Call;
    class Literal extends Expr {
        constructor(value, start, end) {
            super(start, end);
            this.value = value;
        }
        accept(visitor) {
            return visitor.visitLiteralExpr(this);
        }
    }
    Expr.Literal = Literal;
    class Percentage extends Expr {
        constructor(expression, start, end) {
            super(start, end);
            this.expression = expression;
        }
        accept(visitor) {
            return visitor.visitPercentageExpr(this);
        }
    }
    Expr.Percentage = Percentage;
    class UnitExpr extends Expr {
        constructor(expression, phrase, unit, start, end) {
            super(start, end);
            this.unit = unit;
            this.phrase = phrase;
            this.expression = expression;
        }
        accept(visitor) {
            return visitor.visitUnitExpr(this);
        }
    }
    Expr.UnitExpr = UnitExpr;
    class ConversionExpr extends Expr {
        constructor(expression, to, name, start, end) {
            super(start, end);
            this.to = to;
            this.name = name;
            this.expression = expression;
        }
        accept(visitor) {
            return visitor.visitConversionExpr(this);
        }
    }
    Expr.ConversionExpr = ConversionExpr;
    class Unary extends Expr {
        constructor(operator, right, start, end) {
            super(start, end);
            this.operator = operator;
            this.right = right;
        }
        accept(visitor) {
            return visitor.visitUnaryExpr(this);
        }
    }
    Expr.Unary = Unary;
})(Expr || (Expr = {}));
exports.Expr = Expr;
});

var lex = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Lexer = void 0;




class Lexer {
    constructor(source, phrases, units, cc, scale) {
        // Removing the space around expression
        this.source = source.replace(/[ \t]+$/, '');
        this.start = 0;
        this.current = 0;
        this.tokens = Array();
        this.phrases = phrases;
        this.units = units;
        this.cc = cc;
        this.scale = scale;
    }
    static isDigit(char) {
        return char >= '0' && char <= '9';
    }
    static isAlpha(char) {
        return (!Lexer.isDigit(char) && !this.isSpace(char) && char !== '\0' && char !== '\n' && !Lexer.notAlpha.includes(char));
    }
    static isSpace(char) {
        return char === '\t' || char === ' ';
    }
    static isBinaryDigit(char) {
        return char === '0' || char === '1';
    }
    static isOctalDigit(char) {
        return char >= '0' && char <= '8';
    }
    static isHexDigit(char) {
        return (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F');
    }
    Next() {
        if (this.isAtEnd()) {
            return token.Token.EOL(this.current);
        }
        return this.scan();
    }
    getTokens() {
        const tokens = [];
        let token$1;
        while ((token$1 = this.Next()).type !== token.TT.EOL) {
            tokens.push(token$1);
        }
        return tokens;
    }
    scan() {
        const char = this.space();
        switch (char) {
            case token.TT.PLUS:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    return this.TT(token.TT.PLUS_EQUAL);
                }
                return this.TT(token.TT.PLUS);
            case token.TT.MINUS:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    return this.TT(token.TT.MINUS_EQUAL);
                }
                return this.TT(token.TT.MINUS);
            case token.TT.TIMES:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    return this.TT(token.TT.MULTIPLY_EQUAL);
                }
                if (this.peek(0) === token.TT.TIMES) {
                    this.eat();
                    if (this.peek(0) === token.TT.EQUAL) {
                        this.eat();
                        return this.TT(token.TT.POWER_EQUAL);
                    }
                    return this.TT(token.TT.CAP);
                }
                return this.TT(token.TT.TIMES);
            case token.TT.SLASH:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    return this.TT(token.TT.DIVIDE_EQUAL);
                }
                if (this.peek(0) === token.TT.SLASH) {
                    this.eat();
                    if (this.peek(0) === token.TT.EQUAL) {
                        this.eat();
                        return this.TT(token.TT.FLOOR_DIVIDE_EQUAL);
                    }
                    return this.TT(token.TT.FLOOR_DIVIDE);
                }
                return this.TT(token.TT.SLASH);
            case token.TT.EQUAL:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    if (this.peek(0) === token.TT.EQUAL) {
                        this.eat();
                        return this.TT(token.TT.EQUAL_EQUAL_EQUAL);
                    }
                    return this.TT(token.TT.EQUAL_EQUAL);
                }
                return this.TT(token.TT.EQUAL);
            case token.TT.NOT:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    if (this.peek(0) === token.TT.EQUAL) {
                        this.eat();
                        return this.TT(token.TT.NOT_EQUAL_EQUAL);
                    }
                    return this.TT(token.TT.NOT_EQUAL);
                }
                return this.TT(token.TT.NOT);
            case token.TT.GREATER:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    if (this.peek(0) === token.TT.EQUAL) {
                        this.eat();
                        return this.TT(token.TT.GREATER_EQUAL_EQUAL);
                    }
                    return this.TT(token.TT.GREATER_EQUAL);
                }
                return this.TT(token.TT.GREATER);
            case token.TT.LESS:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    if (this.peek(0) === token.TT.EQUAL) {
                        this.eat();
                        return this.TT(token.TT.LESS_EQUAL_EQUAL);
                    }
                    return this.TT(token.TT.LESS_EQUAL);
                }
                return this.TT(token.TT.LESS);
            case '&':
                if (this.peek(0) === '&') {
                    this.eat();
                    return this.TT(token.TT.AND);
                }
                throw new fcal.FcalError('Unexpected character &', this.current);
            case '|':
                if (this.peek(0) === '|') {
                    this.eat();
                    return this.TT(token.TT.OR);
                }
                throw new fcal.FcalError('Unexpected character |', this.current);
            case token.TT.COMMA:
                return this.TT(token.TT.COMMA);
            case token.TT.DOUBLE_COLON:
                return this.TT(token.TT.DOUBLE_COLON);
            case token.TT.OPEN_PAREN:
                return this.TT(token.TT.OPEN_PAREN);
            case token.TT.CLOSE_PAREN:
                return this.TT(token.TT.CLOSE_PAREN);
            case token.TT.CAP:
                if (this.peek(0) === token.TT.EQUAL) {
                    this.eat();
                    return this.TT(token.TT.POWER_EQUAL);
                }
                return this.TT(token.TT.CAP);
            case token.TT.Q:
                return this.TT(token.TT.Q);
            case token.TT.PERCENTAGE:
                return this.TT(token.TT.PERCENTAGE);
            case token.TT.NEWLINE:
                return this.TT(token.TT.NEWLINE);
            default:
                if (Lexer.isDigit(char)) {
                    return this.number();
                }
                return this.string();
        }
    }
    isAtEnd() {
        return this.current >= this.source.length;
    }
    eat() {
        this.current++;
        return this.source.charAt(this.current - 1);
    }
    peek(n) {
        if (this.current + n >= this.source.length) {
            return '\0';
        }
        return this.source.charAt(this.current + n);
    }
    string() {
        while (Lexer.isAlpha(this.peek(0)) || Lexer.isDigit(this.peek(0))) {
            this.eat();
        }
        const text = this.lexeme();
        let type;
        if (text === 'Infinity') {
            return this.TTWithLiteral(token.TT.Number, new datatype.Type.BNumber(text));
        }
        type = this.phrases.get(text);
        if (type) {
            return this.TT(type);
        }
        const s = this.scale.get(text);
        if (s) {
            return this.TTWithLiteral(token.TT.SCALE, text);
        }
        const unit = this.units.get(text);
        if (unit) {
            return this.TTWithLiteral(token.TT.UNIT, text);
        }
        const ns = numberSystem.NumberSystem.get(text);
        if (ns) {
            return this.TTWithLiteral(token.TT.NS, text);
        }
        const cc = this.cc.get(text);
        if (cc) {
            return this.TTWithLiteral(token.TT.CC, text);
        }
        return this.TT(token.TT.NAME);
    }
    number() {
        if (this.previous() === '0' && (this.peek(0) === 'b' || this.peek(0) === 'B')) {
            this.eat();
            while (Lexer.isDigit(this.peek(0))) {
                if (!Lexer.isBinaryDigit(this.peek(0))) {
                    throw new fcal.FcalError(`Unexpected '${this.peek(0)}' in binary number`, this.current);
                }
                this.eat();
                if (this.peek(0) === '_' && Lexer.isBinaryDigit(this.peek(1))) {
                    this.eat();
                }
            }
            const value = new datatype.Type.BNumber(this.lexeme().replace(/_/g, ''));
            value.setSystem(numberSystem.NumberSystem.bin);
            return this.TTWithLiteral(token.TT.Number, value);
        }
        if (this.previous() === '0' && (this.peek(0) === 'o' || this.peek(0) === 'O')) {
            this.eat();
            while (Lexer.isDigit(this.peek(0))) {
                if (!Lexer.isOctalDigit(this.peek(0))) {
                    throw new fcal.FcalError(`Unexpected '${this.peek(0)}' in Octal number`, this.current);
                }
                this.eat();
                if (this.peek(0) === '_' && Lexer.isOctalDigit(this.peek(1))) {
                    this.eat();
                }
            }
            const value = new datatype.Type.BNumber(this.lexeme().replace(/_/g, ''));
            value.setSystem(numberSystem.NumberSystem.oct);
            return this.TTWithLiteral(token.TT.Number, value);
        }
        if (this.previous() === '0' && (this.peek(0) === 'x' || this.peek(0) === 'X')) {
            this.eat();
            if (!Lexer.isHexDigit(this.peek(0))) {
                throw new fcal.FcalError(`Unexpected '${this.peek(0)}' in Hexadecimal`, this.current);
            }
            while (Lexer.isHexDigit(this.peek(0))) {
                this.eat();
                if (this.peek(0) === '_' && Lexer.isHexDigit(this.peek(1))) {
                    this.eat();
                }
            }
            const value = new datatype.Type.BNumber(this.lexeme().replace(/_/g, ''));
            value.setSystem(numberSystem.NumberSystem.hex);
            return this.TTWithLiteral(token.TT.Number, value);
        }
        if (this.peek(0) === '_') {
            this.eat();
        }
        while (Lexer.isDigit(this.peek(0))) {
            this.eat();
            if (this.peek(0) === '_' && Lexer.isDigit(this.peek(1))) {
                this.eat();
            }
        }
        if (this.peek(0) === '.' && Lexer.isDigit(this.peek(1))) {
            this.eat();
            while (Lexer.isDigit(this.peek(0))) {
                this.eat();
                if (this.peek(0) === '_' && Lexer.isDigit(this.peek(1))) {
                    this.eat();
                }
            }
        }
        if (this.peek(0) === 'E' || this.peek(0) === 'e') {
            let c = this.peek(0);
            this.eat();
            if (this.peek(0) === '+' || this.peek(0) === '-') {
                c = this.peek(0);
                this.eat();
            }
            if (!Lexer.isDigit(this.peek(0))) {
                let peekValue = this.peek(0);
                if (peekValue === '\n') {
                    peekValue = 'EOL';
                }
                throw new fcal.FcalError(`Expecting number after ${c} but got '${peekValue}'`, this.start, this.current);
            }
            while (Lexer.isDigit(this.peek(0))) {
                this.eat();
                if (this.peek(0) === '_' && Lexer.isDigit(this.peek(1))) {
                    this.eat();
                }
            }
        }
        return this.TTWithLiteral(token.TT.Number, new datatype.Type.BNumber(this.lexeme().replace(/_/g, '')));
    }
    TT(type) {
        return this.TTWithLiteral(type, null);
    }
    TTWithLiteral(type, literal) {
        const token$1 = new token.Token(type, this.lexeme(), literal, this.start, this.current);
        this.start = this.current;
        this.tokens.push(token$1);
        return token$1;
    }
    lexeme() {
        return this.source.substring(this.start, this.current);
    }
    space() {
        let char = this.eat();
        while (Lexer.isSpace(char)) {
            this.start = this.current;
            char = this.eat();
        }
        return char;
    }
    previous() {
        if (this.current > 0) {
            return this.source.charAt(this.current - 1);
        }
        return '\0';
    }
}
exports.Lexer = Lexer;
Lexer.notAlpha = [
    token.TT.PLUS,
    token.TT.MINUS,
    token.TT.TIMES,
    token.TT.SLASH,
    token.TT.OPEN_PAREN,
    token.TT.CLOSE_PAREN,
    token.TT.CAP,
    token.TT.PERCENTAGE,
    token.TT.EQUAL,
    token.TT.COMMA,
    token.TT.DOUBLE_COLON,
    token.TT.NEWLINE,
    '&',
    '|',
    token.TT.LESS,
    token.TT.GREATER,
    '!',
    token.TT.Q,
];
});

var parser = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Parser = void 0;





class Parser {
    constructor(source, phrases, units, cc, scale, symbolTable) {
        this.source = source;
        this.lexer = new lex.Lexer(this.source, phrases, units, cc, scale);
        this.n = 0;
        this.tokens = [];
        this.c = cc;
        this.scale = scale;
        this.symbolTable = symbolTable;
    }
    parse() {
        try {
            const expr = this.Stmt();
            return expr;
        }
        catch (E) {
            if (E instanceof fcal.FcalError) {
                E.source = this.source;
            }
            throw E;
        }
    }
    getScannedTokens() {
        return this.tokens;
    }
    Stmt() {
        const expr = this.assignment();
        if (this.match([token.TT.NEWLINE])) {
            return expr;
        }
        if (this.peek().type === token.TT.EOL) {
            throw new fcal.FcalError('Expecting EOL', this.peek().end);
        }
        throw new fcal.FcalError(`Unexpected token ${this.peek().lexeme}`, this.peek().start, this.peek().end);
    }
    expression() {
        return this.assignment();
    }
    assignment() {
        const expr$1 = this.ternary();
        if (this.match([token.TT.EQUAL, token.TT.DOUBLE_COLON])) {
            const leftExpr = this.assignment();
            if (expr$1 instanceof expr.Expr.Variable) {
                const name = expr$1.name;
                return new expr.Expr.Assign(name, leftExpr, expr$1.start, leftExpr.end);
            }
            throw new fcal.FcalError('Expecting variable in left side of assignment', expr$1.start, expr$1.end);
        }
        if (this.match([
            token.TT.PLUS_EQUAL,
            token.TT.MINUS_EQUAL,
            token.TT.MULTIPLY_EQUAL,
            token.TT.DIVIDE_EQUAL,
            token.TT.FLOOR_DIVIDE_EQUAL,
            token.TT.POWER_EQUAL,
        ])) {
            const operator = this.previous();
            const leftExpr = this.assignment();
            if (expr$1 instanceof expr.Expr.Variable) {
                let tt;
                switch (operator.type) {
                    case token.TT.PLUS_EQUAL:
                        tt = token.TT.PLUS;
                        break;
                    case token.TT.MINUS_EQUAL:
                        tt = token.TT.MINUS;
                        break;
                    case token.TT.MULTIPLY_EQUAL:
                        tt = token.TT.TIMES;
                        break;
                    case token.TT.DIVIDE_EQUAL:
                        tt = token.TT.SLASH;
                        break;
                    case token.TT.FLOOR_DIVIDE_EQUAL:
                        tt = token.TT.FLOOR_DIVIDE;
                        break;
                    default:
                        tt = token.TT.CAP;
                        break;
                }
                return new expr.Expr.Assign(expr$1.name, new expr.Expr.Binary(expr$1, new token.Token(tt, operator.lexeme, operator.literal, operator.start, operator.start), leftExpr, expr$1.start, leftExpr.end));
            }
            throw new fcal.FcalError('Expecting variable in left side of assignment', expr$1.start, expr$1.end);
        }
        return expr$1;
    }
    ternary() {
        let expr$1 = this.logical();
        if (this.match([token.TT.Q])) {
            const trueExpr = this.ternary();
            this.consume(token.TT.DOUBLE_COLON, `Expecting ':' in ternary operation but found ${this.peek().type === '\n' ? 'EOL' : this.peek().type}`);
            const falseExpr = this.ternary();
            expr$1 = new expr.Expr.Ternary(expr$1, trueExpr, falseExpr, expr$1.start, falseExpr.end);
        }
        return expr$1;
    }
    logical() {
        let expr$1 = this.equality();
        while (this.match([token.TT.OR, token.TT.AND])) {
            const operator = this.previous();
            const right = this.equality();
            expr$1 = new expr.Expr.Logical(expr$1, operator, right, expr$1.start, right.end);
        }
        return expr$1;
    }
    equality() {
        let expr$1 = this.comparison();
        while (this.match([token.TT.EQUAL_EQUAL, token.TT.EQUAL_EQUAL_EQUAL, token.TT.NOT_EQUAL, token.TT.NOT_EQUAL_EQUAL])) {
            const operator = this.previous();
            const right = this.comparison();
            expr$1 = new expr.Expr.Binary(expr$1, operator, right, expr$1.start, right.end);
        }
        return expr$1;
    }
    comparison() {
        let expr$1 = this.addition();
        while (this.match([token.TT.GREATER, token.TT.GREATER_EQUAL, token.TT.GREATER_EQUAL_EQUAL, token.TT.LESS, token.TT.LESS_EQUAL, token.TT.LESS_EQUAL_EQUAL])) {
            const operator = this.previous();
            const right = this.addition();
            expr$1 = new expr.Expr.Binary(expr$1, operator, right, expr$1.start, right.end);
        }
        return expr$1;
    }
    addition() {
        let expr$1 = this.multiply();
        while (this.match([token.TT.PLUS, token.TT.MINUS])) {
            const operator = this.previous();
            const right = this.multiply();
            expr$1 = new expr.Expr.Binary(expr$1, operator, right, expr$1.start, right.end);
        }
        return expr$1;
    }
    multiply() {
        let expr$1 = this.unitConvert();
        while (this.match([token.TT.TIMES, token.TT.SLASH, token.TT.MOD, token.TT.OF, token.TT.FLOOR_DIVIDE])) {
            const operator = this.previous();
            const right = this.unitConvert();
            expr$1 = new expr.Expr.Binary(expr$1, operator, right, expr$1.start, right.end);
        }
        return expr$1;
    }
    unitConvert() {
        const expr$1 = this.unary();
        if (this.match([token.TT.IN])) {
            if (this.match([token.TT.UNIT])) {
                const unit = this.previous();
                const unit2 = this.lexer.units.get(unit.lexeme);
                if (unit2) {
                    return new expr.Expr.ConversionExpr(expr$1, unit2, unit.lexeme, expr$1.start, unit.end);
                }
            }
            if (this.match([token.TT.NS])) {
                const token = this.previous();
                const ns = numberSystem.NumberSystem.get(token.lexeme);
                if (ns) {
                    return new expr.Expr.ConversionExpr(expr$1, ns, token.lexeme, expr$1.start, token.end);
                }
            }
            if (this.match([token.TT.CC])) {
                const token = this.previous();
                const c = this.c.get(token.lexeme);
                if (c) {
                    return new expr.Expr.ConversionExpr(expr$1, c, token.lexeme, expr$1.start, token.end);
                }
            }
            throw new fcal.FcalError('Expecting unit after in');
        }
        return expr$1;
    }
    unary() {
        if (this.match([token.TT.PLUS, token.TT.MINUS, token.TT.NOT])) {
            const operator = this.previous();
            const right = this.unary();
            return new expr.Expr.Unary(operator, right, operator.start, right.end);
        }
        return this.exponent();
    }
    exponent() {
        let expr$1 = this.suffix();
        while (this.match([token.TT.CAP])) {
            const operator = this.previous();
            const right = this.unary();
            expr$1 = new expr.Expr.Binary(expr$1, operator, right, expr$1.start, right.end);
        }
        return expr$1;
    }
    suffix() {
        const expr$1 = this.call();
        if (this.match([token.TT.PERCENTAGE])) {
            const operator = this.previous();
            return new expr.Expr.Percentage(expr$1, expr$1.start, operator.end);
        }
        if (this.match([token.TT.UNIT])) {
            const unit = this.previous();
            let unit2;
            unit2 = this.lexer.units.get(unit.lexeme);
            if (unit2) {
                return new expr.Expr.UnitExpr(expr$1, unit.lexeme, unit2, expr$1.start, unit.end);
            }
        }
        return expr$1;
    }
    call() {
        const expr$1 = this.term();
        if (this.match([token.TT.OPEN_PAREN])) {
            if (expr$1 instanceof expr.Expr.Variable) {
                const argument = Array();
                if (this.peek().type !== token.TT.CLOSE_PAREN) {
                    do {
                        argument.push(this.expression());
                    } while (this.match([token.TT.COMMA]));
                }
                this.consume(token.TT.CLOSE_PAREN, "Expect ')' after the arguments");
                return new expr.Expr.Call(expr$1.name, argument, expr$1.start, this.previous().end);
            }
            throw new fcal.FcalError(`Not callable`, expr$1.start, this.previous().end);
        }
        return expr$1;
    }
    term() {
        if (this.match([token.TT.Number])) {
            const numToken = this.previous();
            const num = numToken.literal;
            if (this.match([token.TT.SCALE])) {
                const s = this.previous().literal;
                const scaleC = this.scale.get(s);
                if (s) {
                    num.n = num.n.mul(scaleC.n);
                    return new expr.Expr.Literal(num, numToken.start, this.previous().end);
                }
            }
            return new expr.Expr.Literal(num, numToken.start, numToken.end);
        }
        if (this.match([token.TT.OPEN_PAREN])) {
            const start = this.previous();
            const expr$1 = this.expression();
            this.consume(token.TT.CLOSE_PAREN, `Expect ')' after expression but found ${this.peek().lexeme}`);
            return new expr.Expr.Grouping(expr$1, start.start, this.previous().end);
        }
        if (this.match([token.TT.NAME])) {
            return new expr.Expr.Variable(this.previous().lexeme, this.previous().start, this.previous().end);
        }
        const lexeme = this.peek().lexeme;
        const entity = this.symbolTable.get(lexeme);
        if (entity) {
            throw new fcal.FcalError(`Expect expression but found ${lexeme} [${entity.toLowerCase()}]`, this.peek().start, this.peek().end);
        }
        throw new fcal.FcalError(`Expect expression but found ${lexeme === '\n' ? 'EOL' : lexeme}`, this.peek().start, this.peek().end);
    }
    match(types) {
        for (const type of types) {
            if (this.check(type)) {
                this.incr();
                return true;
            }
        }
        return false;
    }
    consume(type, message) {
        if (this.check(type)) {
            this.incr();
            return;
        }
        throw new fcal.FcalError(message, this.peek().start, this.peek().end);
    }
    check(type) {
        if (this.isAtEnd()) {
            return false;
        }
        return this.peek().type === type;
    }
    isAtEnd() {
        const token$1 = this.nextToken();
        return token$1.type === token.TT.EOL;
    }
    nextToken() {
        if (this.n < this.tokens.length) {
            return this.tokens[this.n];
        }
        return this.getToken();
    }
    getToken() {
        const token$1 = this.lexer.Next();
        if (token$1.type !== token.TT.EOL) {
            this.tokens.push(token$1);
        }
        return token$1;
    }
    previous() {
        return this.tokens[this.n - 1];
    }
    peek() {
        return this.nextToken();
    }
    incr() {
        this.n++;
    }
}
exports.Parser = Parser;
});

var evaluator = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Evaluator = void 0;







class Evaluator {
    constructor(source, phrases, units, environment, c, scale, strict) {
        this.environment = environment;
        this.strict = strict;
        if (typeof source === 'string') {
            const parser$1 = new parser.Parser(source, phrases, units, c, scale, environment.symbolTable);
            this.parser = parser$1;
            this.ast = parser$1.parse();
            this.source = source;
            return;
        }
        this.ast = source;
    }
    getAST() {
        return this.ast.toString();
    }
    toJSON() {
        return new toJSON.ToJSON(this.ast).toJSON();
    }
    toObj() {
        return new toJSON.ToJSON(this.ast).toObj();
    }
    getScannedTokens() {
        var _a;
        return (_a = this.parser) === null || _a === void 0 ? void 0 : _a.getScannedTokens();
    }
    visitCallExpr(expr) {
        const name = expr.name;
        let call;
        call = this.environment.functions.get(name);
        if (call) {
            if (call.arity !== -1) {
                if (call.arity !== expr.argument.length) {
                    throw new fcal.FcalError(`function ${name} expected ${call.arity} args but got ${expr.argument.length}`, expr.start, expr.end);
                }
            }
            const argument = Array();
            for (const param of expr.argument) {
                argument.push(this.evaluate(param));
            }
            return call.call(this.environment, argument);
        }
        throw new fcal.FcalError(`${name} is not callable`, expr.start, expr.end);
    }
    visitAssignExpr(expr) {
        const value = this.evaluate(expr.value);
        this.environment.set(expr.name, value);
        return value;
    }
    visitVariableExpr(expr) {
        return this.environment.get(expr.name, expr.start, expr.end);
    }
    evaluateExpression() {
        try {
            const value = this.evaluate(this.ast);
            this.environment.set('_', value);
            return value;
        }
        catch (e) {
            if (e instanceof fcal.FcalError) {
                e.source = this.source;
            }
            throw e;
        }
    }
    visitConversionExpr(expr) {
        const value = this.evaluate(expr.expression);
        if (value instanceof datatype.Type.Numeric) {
            if (expr.to instanceof units$1.UnitMeta) {
                return datatype.Type.UnitNumber.convertToUnit(value, expr.to).setSystem(value.ns);
            }
            if (expr.to instanceof numberSystem.NumberSystem) {
                return value.New(value.n).setSystem(expr.to);
            }
            return expr.to(value);
        }
        throw new fcal.FcalError('Expecting numeric value before in', expr.start, expr.end);
    }
    visitUnitExpr(expr) {
        const value = this.evaluate(expr.expression);
        if (value instanceof datatype.Type.Numeric) {
            return datatype.Type.UnitNumber.New(value.n, expr.unit).setSystem(value.ns);
        }
        throw new fcal.FcalError('Expecting numeric value before unit', expr.start, expr.end);
    }
    visitTernaryExpr(expr) {
        const main = this.evaluate(expr.main);
        if (main.trusty()) {
            return this.evaluate(expr.trueExpr);
        }
        return this.evaluate(expr.falseExpr);
    }
    visitLogicalExpr(expr) {
        const left = this.evaluate(expr.left);
        if (expr.operator.type === token.TT.AND) {
            return left.trusty() ? this.evaluate(expr.right) : left;
        }
        return left.trusty() ? left : this.evaluate(expr.right);
    }
    visitBinaryExpr(expr) {
        const left = this.evaluate(expr.left);
        const right = this.evaluate(expr.right);
        if (this.strict) {
            this.checkInvalidOperation(expr.operator.type, [left, right]);
        }
        switch (expr.operator.type) {
            case token.TT.EQUAL_EQUAL:
                return left.EQ(right);
            case token.TT.EQUAL_EQUAL_EQUAL:
                return new datatype.Type.FcalBoolean(left.n.eq(right.n));
            case token.TT.NOT_EQUAL:
                return left.NEQ(right);
            case token.TT.NOT_EQUAL_EQUAL:
                return new datatype.Type.FcalBoolean(!left.n.eq(right.n));
            case token.TT.GREATER:
                return left.GT(right);
            case token.TT.GREATER_EQUAL:
                return left.GTE(right);
            case token.TT.GREATER_EQUAL_EQUAL:
                return new datatype.Type.FcalBoolean(left.n.gte(right.n));
            case token.TT.LESS:
                return left.LT(right);
            case token.TT.LESS_EQUAL:
                return left.LTE(right);
            case token.TT.LESS_EQUAL_EQUAL:
                return new datatype.Type.FcalBoolean(left.n.lte(right.n));
            case token.TT.PLUS:
                return left.Add(right);
            case token.TT.MINUS:
                return left.Sub(right);
            case token.TT.TIMES:
                return left.times(right);
            case token.TT.FLOOR_DIVIDE:
                const v = left.divide(right);
                v.n = v.n.floor();
                return v;
            case token.TT.SLASH:
                return left.divide(right);
            case token.TT.MOD:
                return left.modulo(right);
            case token.TT.CAP:
                return left.power(right);
            case token.TT.OF:
                // check whether boolean involved in percentage operation
                if (left instanceof datatype.Type.FcalBoolean || right instanceof datatype.Type.FcalBoolean) {
                    throw new fcal.FcalError('Unexpected Boolean in percentage operation');
                }
                if ((left instanceof datatype.Type.BNumber && right instanceof datatype.Type.BNumber) ||
                    (left instanceof datatype.Type.Percentage && right instanceof datatype.Type.Percentage)) {
                    return new datatype.Type.Percentage(left.n.div(right.n).mul(100));
                }
                if (left instanceof datatype.Type.UnitNumber && right instanceof datatype.Type.UnitNumber) {
                    if (left.unit.id === right.unit.id) {
                        return new datatype.Type.Percentage(datatype.Type.UnitNumber.convertToUnit(left, right.unit)
                            .n.div(right.n)
                            .mul(100));
                    }
                    return new datatype.Type.Percentage(left.n.div(right.n).mul(100));
                }
                if (left instanceof datatype.Type.Percentage) {
                    const per = left;
                    return right.New(per.percentageValue(right.n));
                }
                throw new fcal.FcalError(`Expecting Percentage type in left side of percentage operation but got (${datatype.Type.typeVsStr[left.TYPE]}, ${datatype.Type.typeVsStr[right.TYPE]})`);
            default:
                return datatype.Type.BNumber.ZERO;
        }
    }
    visitGroupingExpr(expr) {
        return this.evaluate(expr.expression);
    }
    visitLiteralExpr(expr) {
        return expr.value;
    }
    visitUnaryExpr(expr) {
        const right = this.evaluate(expr.right);
        if (expr.operator.type === token.TT.MINUS) {
            return right.negated();
        }
        if (expr.operator.type === token.TT.NOT) {
            return right.not();
        }
        return right;
    }
    visitPercentageExpr(expr) {
        const value = this.evaluate(expr.expression);
        if (value instanceof datatype.Type.Numeric) {
            return datatype.Type.Percentage.New(value.n);
        }
        throw new fcal.FcalError('Expecting numeric value in percentage', expr.start, expr.end);
    }
    evaluate(expr) {
        const ast = expr.eval(this);
        return ast;
    }
    checkInvalidOperation(operation, values) {
        let checkValue;
        for (const value of values) {
            if (value instanceof datatype.Type.Percentage) {
                continue;
            }
            if (!checkValue) {
                checkValue = value;
                continue;
            }
            if (checkValue.TYPE !== value.TYPE) {
                switch (operation) {
                    case token.TT.TIMES:
                    case token.TT.SLASH:
                    case token.TT.FLOOR_DIVIDE:
                    case token.TT.MOD:
                    case token.TT.PERCENTAGE:
                    case token.TT.CAP:
                    case token.TT.LESS_EQUAL_EQUAL:
                    case token.TT.GREATER_EQUAL_EQUAL:
                    case token.TT.EQUAL_EQUAL_EQUAL:
                    case token.TT.NOT_EQUAL_EQUAL:
                        continue;
                    default:
                        throw new fcal.FcalError(`Unexpected '${operation}' operation between different types (${datatype.Type.typeVsStr[checkValue.TYPE]}, ${datatype.Type.typeVsStr[value.TYPE]})`);
                }
            }
            if (checkValue instanceof datatype.Type.UnitNumber && value instanceof datatype.Type.UnitNumber) {
                if (checkValue.unit.id !== value.unit.id) {
                    this.throwUnexpectedUnits(operation, checkValue.unit.id, value.unit.id);
                }
            }
        }
    }
    throwUnexpectedUnits(operation, leftID, rightID) {
        throw new fcal.FcalError(`Unexpected '${operation}' operation between different units (${leftID}, ${rightID})`);
    }
}
exports.Evaluator = Evaluator;
});

var _function = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.FcalFunction = void 0;



/**
 * FcalFunction represents function in fcal
 */
class FcalFunction {
    /**
     * Create new Fcal function
     * @param name name of the function
     * @param arity number of arguments function can expect, -1 for any number of functions
     * @param func function implementation
     */
    constructor(name, arity, func) {
        this.arity = arity;
        this.function = func;
        this.name = name;
    }
    /**
     * call the function
     * @param {Environment} environment state of fcal
     * @param {Array<Type>} argument arguments of the function
     * @returns {Type} function result
     * @throws {FcalError} Error if function return invalid return type
     */
    call(environment, argument) {
        const value = this.function(environment, argument);
        if (!value) {
            // if function does not return no value then
            // Assign basic 0 number
            return datatype.Type.BNumber.New(0);
        }
        if (typeof value === 'number' || value instanceof decimal.Decimal) {
            return datatype.Type.BNumber.New(value);
        }
        if (!(value instanceof datatype.Type)) {
            throw new fcal.FcalError(`${this.name} Function Invalid return type,  Expecting Fcal.Type but got ${typeof value}`);
        }
        return value;
    }
}
exports.FcalFunction = FcalFunction;
/**
 * List of fcal functions
 */
(function (FcalFunction) {
    class List {
        constructor() {
            this.functions = new Map();
        }
        /**
         * Add new fcal function
         * @param {FcalFunction} fcalFunction
         * @throws {FcalError} Error if function name is already exists
         */
        push(ff) {
            if (ff.arity < -1) {
                throw new fcal.FcalError(`Can not register ${ff.name}, arity should be greater than or equal to -1 but got ${ff.arity}`);
            }
            if (ff.arity >= 255) {
                throw new fcal.FcalError(`Can not register ${ff.name}, function cannot have more than 254 arguments`);
            }
            if (ff.arity % 1 !== 0) {
                throw new fcal.FcalError(`Can not register ${ff.name}, arity should be Integer`);
            }
            this.functions.set(ff.name, ff);
        }
        /**
         * Call a function by its name
         * @param {string} name name of the function
         * @param {Environment} environment state of fcal
         * @param {Array<Type>} argument arguments for the function
         * @param {Type} Type result of the function
         * @throws {FcalError} Error if function is not found
         */
        call(name, environment, argument) {
            const fcalFunc = this.get(name);
            if (fcalFunc) {
                return fcalFunc.function(environment, argument);
            }
            throw new fcal.FcalError(`Function ${name} is not found`);
        }
        /**
         * Get function implementation by its function name
         * @param {string} name function name
         * @returns {FcalFunction | undefined} function
         */
        get(name) {
            return this.functions.get(name);
        }
    }
    FcalFunction.List = List;
})(FcalFunction || (FcalFunction = {}));
exports.FcalFunction = FcalFunction;
});

var scale = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Scale = void 0;


/**
 * Scale is used to define scale of number literal
 */
class Scale {
    /**
     * Create scale register
     * @param symbolTable symbol table
     */
    constructor(symbolTable) {
        this.values = new Map();
        this.symbolTable = symbolTable;
    }
    /**
     * Get the Scale value by its phrase
     * @param {string} key scale phrase or id
     * @returns {Type | undefined} scale value
     */
    get(key) {
        return this.values.get(key);
    }
    /**
     * create new scale
     * @param {string} key scale name
     * @param  {Type | Big.Decimal | number | string} value value
     */
    set(key, value) {
        this.symbolTable.set(key, symboltable.Entity.SCALE);
        if (value instanceof datatype.Type) {
            this.values.set(key, value);
            return;
        }
        this.values.set(key, datatype.Type.BNumber.New(value));
    }
    /**
     * import values from Object or map into scale
     * @param {Object | Map} values
     */
    use(values) {
        if (values instanceof Map) {
            values.forEach((value, key) => {
                this.set(key, value);
            });
            return;
        }
        for (const key in values) {
            if (values.hasOwnProperty(key)) {
                const element = values[key];
                this.set(key, element);
            }
        }
    }
}
exports.Scale = Scale;
});

var JSONParser_1 = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.JSONParser = void 0;





class JSONParser {
    constructor(astJSON, units, c) {
        this.units = units;
        this.c = c;
        this.ast = JSON.parse(astJSON);
    }
    parse() {
        return this.createExpr(this.ast);
    }
    createExpr(ast) {
        const type = ast.type;
        switch (type) {
            case toJSON.JSON_TYPES.BINARY:
                if (ast.right && ast.left && ast.operator) {
                    const left = this.createExpr(ast.left);
                    const right = this.createExpr(ast.right);
                    return new expr.Expr.Binary(left, ast.operator, right, ast.start, ast.end);
                }
                break;
            case toJSON.JSON_TYPES.GROUP:
                if (ast.value && typeof ast.value !== 'string') {
                    const expr$1 = this.createExpr(ast.value);
                    return new expr.Expr.Grouping(expr$1, ast.start, ast.end);
                }
                break;
            case toJSON.JSON_TYPES.LITERAL:
                if (ast.value && typeof ast.value === 'string') {
                    return new expr.Expr.Literal(new datatype.Type.BNumber(ast.value), ast.start, ast.end);
                }
                break;
            case toJSON.JSON_TYPES.UNARY:
                if (ast.operator && ast.value && typeof ast.value !== 'string') {
                    const expr$1 = this.createExpr(ast.value);
                    return new expr.Expr.Unary(ast.operator, expr$1, ast.start, ast.end);
                }
                break;
            case toJSON.JSON_TYPES.PERCENTAGE:
                if (ast.value && typeof ast.value !== 'string') {
                    const expr$1 = this.createExpr(ast.value);
                    return new expr.Expr.Percentage(expr$1, ast.start, ast.end);
                }
                break;
            case toJSON.JSON_TYPES.UNIT:
                if (ast.phrase && ast.value && typeof ast.value !== 'string') {
                    const unitMeta = this.units.get(ast.phrase);
                    if (unitMeta) {
                        const expr$1 = this.createExpr(ast.value);
                        return new expr.Expr.UnitExpr(expr$1, ast.phrase, unitMeta, ast.start, ast.end);
                    }
                }
                break;
            case toJSON.JSON_TYPES.CONVERSION:
                if (ast.value && typeof ast.value !== 'string') {
                    const value = this.createExpr(ast.value);
                    if (ast.unit) {
                        const unitMeta = this.units.get(ast.unit);
                        if (unitMeta) {
                            return new expr.Expr.ConversionExpr(value, unitMeta, ast.unit, ast.start, ast.end);
                        }
                    }
                    if (ast.ns) {
                        const ns = numberSystem.NumberSystem.get(ast.ns);
                        if (ns) {
                            return new expr.Expr.ConversionExpr(value, ns, ast.ns, ast.start, ast.end);
                        }
                    }
                    if (ast.converter) {
                        const cov = this.c.get(ast.converter);
                        if (cov) {
                            return new expr.Expr.ConversionExpr(value, cov, ast.converter, ast.start, ast.end);
                        }
                    }
                }
                break;
            case toJSON.JSON_TYPES.ASSIGN:
                if (ast.value && typeof ast.value !== 'string') {
                    const value = this.createExpr(ast.value);
                    if (ast.variable) {
                        return new expr.Expr.Assign(ast.variable, value, ast.start, ast.end);
                    }
                }
                break;
            case toJSON.JSON_TYPES.VARIABLE:
                if (ast.name) {
                    return new expr.Expr.Variable(ast.name, ast.start, ast.end);
                }
                break;
            case toJSON.JSON_TYPES.CALL:
                if (ast.name) {
                    const exprs = Array();
                    if (ast.args) {
                        for (const arg of ast.args) {
                            exprs.push(this.createExpr(arg));
                        }
                        return new expr.Expr.Call(ast.name, exprs, ast.start, ast.end);
                    }
                }
                break;
            case toJSON.JSON_TYPES.LOGICAL:
                if (ast.right && ast.left && ast.operator) {
                    const left = this.createExpr(ast.left);
                    const right = this.createExpr(ast.right);
                    return new expr.Expr.Logical(left, ast.operator, right, ast.start, ast.end);
                }
                break;
            case toJSON.JSON_TYPES.TERNARY:
                if (ast.main && ast.trueExpr && ast.falseExpr) {
                    const main = this.createExpr(ast.main);
                    const trueExpr = this.createExpr(ast.trueExpr);
                    const falseExpr = this.createExpr(ast.falseExpr);
                    return new expr.Expr.Ternary(main, trueExpr, falseExpr, ast.start, ast.end);
                }
                break;
        }
        throw new fcal.FcalError(`Invalid JSON ${ast}`);
    }
}
exports.JSONParser = JSONParser;
});

var phrase_1 = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Phrases = void 0;

class Phrases {
    constructor(symbolTable) {
        this.symbolTable = symbolTable;
        this.phrases = new Map();
    }
    push(key, phrases) {
        for (const phrase of phrases) {
            this.symbolTable.set(phrase.toUpperCase(), symboltable.Entity.OPERATION_PHRASE);
            this.phrases.set(phrase.toUpperCase(), key);
        }
    }
    get(key) {
        return this.phrases.get(key.toUpperCase());
    }
}
exports.Phrases = Phrases;
});

var fcal = createCommonjsModule(function (module, exports) {
Object.defineProperty(exports, "__esModule", { value: true });
exports.Decimal = exports.Type = exports.Unit = exports.Environment = exports.FcalFunction = exports.Expression = exports.FcalError = exports.Fcal = void 0;

Object.defineProperty(exports, "Decimal", { enumerable: true, get: function () { return decimal.Decimal; } });





Object.defineProperty(exports, "Environment", { enumerable: true, get: function () { return environment.Environment; } });


Object.defineProperty(exports, "FcalFunction", { enumerable: true, get: function () { return _function.FcalFunction; } });





Object.defineProperty(exports, "Type", { enumerable: true, get: function () { return datatype.Type; } });


Object.defineProperty(exports, "Unit", { enumerable: true, get: function () { return units$1.Unit; } });

/**
 * Math expression evaluator.
 * It evaluates various arithmetic operations, percentage operations,
 * variables and functions with units
 */
class Fcal {
    constructor() {
        this.lst = new symboltable.SymbolTable(Fcal.gst);
        this.strict = false;
        this.environment = new environment.Environment(Fcal.functions, this.lst, Fcal.constants);
    }
    /**
     * Quick math expression evaluator
     * @param {string} source expression
     * @returns {Type} result
     */
    static eval(source) {
        return new Fcal().evaluate(source);
    }
    /**
     * register new fcal Functions
     * @param {Array<FcalFunction | Object>} functions list of fcal function definitions
     */
    static UseFunctions(functions) {
        for (const func of functions) {
            this.UseFunction(func);
        }
    }
    /**
     * Register new Fcal function
     * @param {FcalFunction | Object} function fcal function definitions
     */
    static UseFunction(func) {
        Fcal.gst.set(func.name, symboltable.Entity.FUNCTION);
        if (func instanceof _function.FcalFunction) {
            this.functions.push(func);
            return;
        }
        this.functions.push(new _function.FcalFunction(func.name, func.arity, func.func));
    }
    /**
     * Register new units
     * @param {Array<Unit | Object>} units
     */
    static UseUnits(units) {
        for (const unit of units) {
            this.UseUnit(unit);
        }
    }
    /**
     * Register new unit
     * @param {Unit | Object} unit
     */
    static UseUnit(unit) {
        if (unit instanceof units$1.Unit) {
            return this.units.push(unit);
        }
        const u = new units$1.Unit(unit.id, unit.ratio, unit.type, unit.phrases);
        if (unit.bias) {
            u.setBias(unit.bias);
        }
        if (unit.plural) {
            u.Plural(unit.plural);
        }
        if (unit.singular) {
            u.Singular(unit.singular);
        }
        this.units.push(u);
    }
    /**
     * Get unit meta by its phrase
     * @param {string} unit phrase
     * @returns {UnitMeta | null}
     */
    static getUnit(unit) {
        return this.units.get(unit);
    }
    /**
     * useConstants set the constants in fcal
     * @param { { [index: string]: Type | Decimal | number | string } } constants
     */
    static useConstants(constants) {
        this.constants.use(constants);
    }
    /**
     * useScales register new scale in fcal
     * @param { { [index: string]: Type | Decimal | number | string } } scales
     */
    static useScales(scales) {
        this.scales.use(scales);
    }
    /**
     * Register new converter function
     * @param {string}id id of the converter function
     * @param {converterFuncFmt}f function
     */
    static useConverter(id, f) {
        this.converters.set(id, f);
    }
    /**
     * Get the units list
     * @returns {Unit.List} units
     */
    static getUnits() {
        return this.units;
    }
    /**
     * Get the constants
     * @returns {Constant} constants
     */
    static getConstants() {
        return this.constants;
    }
    /**
     * Get the functions
     * @returns {FcalFunction.List} functions
     */
    static getFunctions() {
        return this.functions;
    }
    /**
     * Get the scales
     * @returns {Scale} scales
     */
    static getScales() {
        return this.scales;
    }
    /**
     * Get the converters
     * @returns {Converter} converters
     */
    static getConverters() {
        return this.converters;
    }
    /**
     * Scan the math expression and  gets array of tokens
     * @param {string} expression math expression
     * @returns {Token[]} array of tokens
     */
    static getTokensForExpression(expression) {
        const lexer = new lex.Lexer(expression, this.phrases, this.units, this.converters, this.scales);
        return lexer.getTokens();
    }
    static initialize() {
        if (!this.gst) {
            this.gst = new symboltable.SymbolTable();
        }
        if (!this.phrases) {
            this.phrases = this.getDefaultPhrases();
        }
        if (!this.units) {
            this.units = new units$1.Unit.List(Fcal.gst);
            this.setDefaultUnits();
        }
        if (!this.functions) {
            this.functions = new _function.FcalFunction.List();
            this.setDefaultFunctions();
        }
        if (!this.constants) {
            this.constants = new constants.Constant(this.gst);
            this.setDefaultConstants();
        }
        if (!this.converters) {
            this.converters = new converter.Converter(this.gst);
            this.setDefaultConverter();
        }
        if (!this.scales) {
            this.scales = new scale.Scale(this.gst);
            this.setDefaultScales();
        }
    }
    static getDefaultPhrases() {
        const phrases = new phrase_1.Phrases(this.gst);
        phrases.push(token.TT.PLUS, ['PLUS', 'WITH', 'ADD']);
        phrases.push(token.TT.MINUS, ['MINUS', 'SUBTRACT', 'WITHOUT']);
        phrases.push(token.TT.TIMES, ['TIMES', 'MULTIPLIEDBY', 'mul']);
        phrases.push(token.TT.SLASH, ['DIVIDE', 'DIVIDEBY']);
        phrases.push(token.TT.CAP, ['POW']);
        phrases.push(token.TT.MOD, ['mod']);
        phrases.push(token.TT.OF, ['of']);
        phrases.push(token.TT.IN, ['in', 'as', 'to']);
        phrases.push(token.TT.AND, ['and']);
        phrases.push(token.TT.OR, ['or']);
        phrases.push(token.TT.NOT, ['not']);
        return phrases;
    }
    static setDefaultFunctions() {
        this.UseFunctions(functions.getDefaultFunctions());
    }
    static setDefaultUnits() {
        this.UseUnits(units.getDefaultUnits());
    }
    static setDefaultConstants() {
        this.useConstants({
            E: datatype.Type.BNumber.New('2.718281828459045235360287'),
            PI: datatype.Type.BNumber.New('3.141592653589793238462645'),
            PI2: datatype.Type.BNumber.New('6.2831853071795864769'),
            false: datatype.Type.FcalBoolean.FALSE,
            true: datatype.Type.FcalBoolean.TRUE,
        });
    }
    static setDefaultScales() {
        const thousand = 1000;
        const million = 1000000;
        const billion = 10000000;
        this.useScales({ k: thousand, M: million, B: billion, thousand, million, billion });
    }
    static setDefaultConverter() {
        const num = (v) => {
            return datatype.Type.BNumber.New(v.n);
        };
        const per = (v) => {
            return datatype.Type.Percentage.New(v.n);
        };
        this.useConverter('number', num);
        this.useConverter('num', num);
        this.useConverter('percentage', per);
        this.useConverter('percent', per);
    }
    /**
     * Evaluates given expression
     * it appends new line character if not present
     * @param {string} expression Math expression
     * @returns {Type} result of expression
     */
    evaluate(source) {
        source = prefixNewLIne(source);
        return this.rawEvaluate(source);
    }
    /**
     * rawEvaluates given expression
     * it does not appends new line character if not present
     * @param {string} expression Math expression
     * @returns {Type} result of expression
     */
    rawEvaluate(source) {
        return new evaluator.Evaluator(source /*expression */, Fcal.phrases, Fcal.units, this.environment, Fcal.converters, Fcal.scales, this.strict).evaluateExpression();
    }
    /**
     * Create new expression with copy of Fcal.Environment
     * @param {string} source Math  expression
     * @returns {Expression} Expression with parsed AST
     */
    expression(source) {
        // Cloning fcal session
        const symbolTable = new symboltable.SymbolTable(this.lst);
        // Creating new environment
        const env = new environment.Environment(Fcal.functions, symbolTable, Fcal.constants);
        // coping values from fcal
        env.values = new Map(this.environment.values);
        source = prefixNewLIne(source);
        return new Expression(new evaluator.Evaluator(source /* expression */, Fcal.phrases, Fcal.units, env /* environment */, Fcal.converters /* converters */, Fcal.scales, this.strict));
    }
    /**
     * Create new  Expression in sync with Fcal.Environment
     * @param {string} source Math expression
     * @returns {Expression} Expression with parsed AST
     */
    expressionSync(source) {
        source = prefixNewLIne(source);
        return new Expression(new evaluator.Evaluator(source /* expression */, Fcal.phrases /* environment */, Fcal.units, this.environment, Fcal.converters /* converters */, Fcal.scales, this.strict));
    }
    /**
     * create a new variable with value or assign value to variable
     * @param {Object | EnvInputType} values variables
     */
    setValues(values) {
        this.environment.use(values);
    }
    /**
     * Get the environment of this fcal session
     * @returns {Environment} env
     */
    getEnvironment() {
        return this.environment;
    }
    /**
     * Import expression from JSON
     * @param {string} source json
     * @returns {Expression}
     */
    fromJSON(source) {
        const parser = new JSONParser_1.JSONParser(source, Fcal.units, Fcal.converters);
        const symbolTable = new symboltable.SymbolTable(this.lst);
        const env = new environment.Environment(Fcal.functions, symbolTable, Fcal.constants);
        env.values = new Map(this.environment.values);
        source = prefixNewLIne(source);
        return new Expression(new evaluator.Evaluator(parser.parse(), Fcal.phrases, Fcal.units, env, Fcal.converters, Fcal.scales, this.strict));
    }
    /**
     * Set strict mode
     * @param v
     */
    setStrict(v) {
        this.strict = v;
    }
}
exports.Fcal = Fcal;
function prefixNewLIne(source) {
    if (source.endsWith('\n')) {
        return source;
    }
    return source + '\n';
}
/**
 * Expression takes AST created from Parser and
 * evaluate AST with its state
 */
class Expression {
    constructor(evaluator) {
        this.evaluator = evaluator;
    }
    /**
     * Evaluate AST of Math expression
     * @returns {Type}  result of Math expression
     */
    evaluate() {
        return this.evaluator.evaluateExpression();
    }
    /**
     * Change state of variables
     * if variable is not found,  it will create a new variable
     * @param {Object | Map} values variables
     */
    setValues(values) {
        this.evaluator.environment.use(values);
    }
    /**
     * Get the environment of this expression
     * @returns {Environment} environment
     */
    getValues() {
        return this.evaluator.environment;
    }
    /**
     * Get the AST tree view of the formula expression
     * @returns {string}  AST tree view
     */
    getAST() {
        return this.evaluator.getAST();
    }
    /**
     * Convert the expression into JSON
     * @returns {string} JSON
     */
    toJSON() {
        return this.evaluator.toJSON();
    }
    /**
     * Convert the expression into an Object
     */
    toObj() {
        return this.evaluator.toObj();
    }
    /**
     * Get scanned tokens
     * @returns {Token[] | undefined} tokens
     */
    getScannedTokens() {
        return this.evaluator.getScannedTokens();
    }
    toString() {
        return this.getAST();
    }
}
exports.Expression = Expression;
/**
 * FcalError represents Error in Fcal
 */
class FcalError extends Error {
    constructor(message, start, end) {
        super(message);
        this.start = start;
        this.end = end;
        this.message = message;
        if (!start) {
            this.name = 'FcalError';
            return;
        }
        if (!end) {
            this.end = start;
        }
        this.name = `FcalError [${this.start}, ${this.end}]`;
    }
    static mark(start, end) {
        return '^'.repeat(start === end ? 1 : end - start).padStart(end, '.');
    }
    /**
     * info gets more information about FcalError
     */
    info() {
        const values = Array();
        values.push(`err: ${this.message}\n`);
        if (this.source !== undefined && this.start !== undefined && this.end !== undefined) {
            values.push(`| ${this.source}`);
            values.push(`| ${FcalError.mark(this.start, this.end)}\n`);
        }
        return values.join('');
    }
}
exports.FcalError = FcalError;
/***************************************************************/
Fcal.initialize();
});

class MeldCalcPlugin extends obsidian.Plugin {
    constructor() {
        super(...arguments);
        this.fcal = new fcal.Fcal();
    }
    onload() {
        return __awaiter(this, void 0, void 0, function* () {
            this.addCommand({
                id: 'encrypt-calc',
                name: 'Evaluate',
                editorCheckCallback: (checking, editor, view) => this.processEvaluateCommand_fcal(checking, editor, view)
            });
        });
    }
    processEvaluateCommand_fcal(checking, editor, view) {
        const selection = editor.getSelection();
        let evalText = selection;
        if (evalText.length === 0) {
            return false;
        }
        if (checking) {
            return true;
        }
        // split in to array of lines
        const lines = evalText.split('\n');
        let evaluatedLines = [];
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const lastLine = (i === lines.length - 1);
            evaluatedLines.push(this.evaluateLine(line, lastLine));
        }
        const formatedResult = evaluatedLines.join('\n');
        editor.replaceSelection(formatedResult);
        return true;
    }
    evaluateLine(line, isLastLine) {
        let appendResult = false;
        let evalLine = line.trim();
        if (evalLine.endsWith('=')) {
            appendResult = true;
            evalLine = evalLine.slice(0, -1); // remove '='
        }
        // replace escaped multiplication
        evalLine = evalLine.replace('\\*', '*');
        // trim it down
        evalLine = evalLine.trim();
        try {
            const rawResult = this.fcal.evaluate(evalLine);
            const formatedResult = rawResult.toString();
            if (appendResult) {
                return `${line}${formatedResult}`;
            }
            else {
                if (isLastLine) {
                    navigator.clipboard.writeText(formatedResult).then(() => {
                        new obsidian.Notice(`${formatedResult} (copied)`, 5000);
                    });
                }
                return line;
            }
        }
        catch (ex) {
            if (ex instanceof fcal.FcalError) {
                console.error(ex.message);
                new obsidian.Notice(ex.message, 5000);
            }
            else {
                console.error(ex);
                new obsidian.Notice(ex, 5000);
            }
            return line;
        }
    }
}

module.exports = MeldCalcPlugin;


/* nosourcemap */