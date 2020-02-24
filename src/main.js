(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_enqueueEffects(managers, result.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


// BYTES

function _Bytes_width(bytes)
{
	return bytes.byteLength;
}

var _Bytes_getHostEndianness = F2(function(le, be)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(new Uint8Array(new Uint32Array([1]))[0] === 1 ? le : be));
	});
});


// ENCODERS

function _Bytes_encode(encoder)
{
	var mutableBytes = new DataView(new ArrayBuffer($elm$bytes$Bytes$Encode$getWidth(encoder)));
	$elm$bytes$Bytes$Encode$write(encoder)(mutableBytes)(0);
	return mutableBytes;
}


// SIGNED INTEGERS

var _Bytes_write_i8  = F3(function(mb, i, n) { mb.setInt8(i, n); return i + 1; });
var _Bytes_write_i16 = F4(function(mb, i, n, isLE) { mb.setInt16(i, n, isLE); return i + 2; });
var _Bytes_write_i32 = F4(function(mb, i, n, isLE) { mb.setInt32(i, n, isLE); return i + 4; });


// UNSIGNED INTEGERS

var _Bytes_write_u8  = F3(function(mb, i, n) { mb.setUint8(i, n); return i + 1 ;});
var _Bytes_write_u16 = F4(function(mb, i, n, isLE) { mb.setUint16(i, n, isLE); return i + 2; });
var _Bytes_write_u32 = F4(function(mb, i, n, isLE) { mb.setUint32(i, n, isLE); return i + 4; });


// FLOATS

var _Bytes_write_f32 = F4(function(mb, i, n, isLE) { mb.setFloat32(i, n, isLE); return i + 4; });
var _Bytes_write_f64 = F4(function(mb, i, n, isLE) { mb.setFloat64(i, n, isLE); return i + 8; });


// BYTES

var _Bytes_write_bytes = F3(function(mb, offset, bytes)
{
	for (var i = 0, len = bytes.byteLength, limit = len - 4; i <= limit; i += 4)
	{
		mb.setUint32(offset + i, bytes.getUint32(i));
	}
	for (; i < len; i++)
	{
		mb.setUint8(offset + i, bytes.getUint8(i));
	}
	return offset + len;
});


// STRINGS

function _Bytes_getStringWidth(string)
{
	for (var width = 0, i = 0; i < string.length; i++)
	{
		var code = string.charCodeAt(i);
		width +=
			(code < 0x80) ? 1 :
			(code < 0x800) ? 2 :
			(code < 0xD800 || 0xDBFF < code) ? 3 : (i++, 4);
	}
	return width;
}

var _Bytes_write_string = F3(function(mb, offset, string)
{
	for (var i = 0; i < string.length; i++)
	{
		var code = string.charCodeAt(i);
		offset +=
			(code < 0x80)
				? (mb.setUint8(offset, code)
				, 1
				)
				:
			(code < 0x800)
				? (mb.setUint16(offset, 0xC080 /* 0b1100000010000000 */
					| (code >>> 6 & 0x1F /* 0b00011111 */) << 8
					| code & 0x3F /* 0b00111111 */)
				, 2
				)
				:
			(code < 0xD800 || 0xDBFF < code)
				? (mb.setUint16(offset, 0xE080 /* 0b1110000010000000 */
					| (code >>> 12 & 0xF /* 0b00001111 */) << 8
					| code >>> 6 & 0x3F /* 0b00111111 */)
				, mb.setUint8(offset + 2, 0x80 /* 0b10000000 */
					| code & 0x3F /* 0b00111111 */)
				, 3
				)
				:
			(code = (code - 0xD800) * 0x400 + string.charCodeAt(++i) - 0xDC00 + 0x10000
			, mb.setUint32(offset, 0xF0808080 /* 0b11110000100000001000000010000000 */
				| (code >>> 18 & 0x7 /* 0b00000111 */) << 24
				| (code >>> 12 & 0x3F /* 0b00111111 */) << 16
				| (code >>> 6 & 0x3F /* 0b00111111 */) << 8
				| code & 0x3F /* 0b00111111 */)
			, 4
			);
	}
	return offset;
});


// DECODER

var _Bytes_decode = F2(function(decoder, bytes)
{
	try {
		return $elm$core$Maybe$Just(A2(decoder, bytes, 0).b);
	} catch(e) {
		return $elm$core$Maybe$Nothing;
	}
});

var _Bytes_read_i8  = F2(function(      bytes, offset) { return _Utils_Tuple2(offset + 1, bytes.getInt8(offset)); });
var _Bytes_read_i16 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 2, bytes.getInt16(offset, isLE)); });
var _Bytes_read_i32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getInt32(offset, isLE)); });
var _Bytes_read_u8  = F2(function(      bytes, offset) { return _Utils_Tuple2(offset + 1, bytes.getUint8(offset)); });
var _Bytes_read_u16 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 2, bytes.getUint16(offset, isLE)); });
var _Bytes_read_u32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getUint32(offset, isLE)); });
var _Bytes_read_f32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getFloat32(offset, isLE)); });
var _Bytes_read_f64 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 8, bytes.getFloat64(offset, isLE)); });

var _Bytes_read_bytes = F3(function(len, bytes, offset)
{
	return _Utils_Tuple2(offset + len, new DataView(bytes.buffer, bytes.byteOffset + offset, len));
});

var _Bytes_read_string = F3(function(len, bytes, offset)
{
	var string = '';
	var end = offset + len;
	for (; offset < end;)
	{
		var byte = bytes.getUint8(offset++);
		string +=
			(byte < 128)
				? String.fromCharCode(byte)
				:
			((byte & 0xE0 /* 0b11100000 */) === 0xC0 /* 0b11000000 */)
				? String.fromCharCode((byte & 0x1F /* 0b00011111 */) << 6 | bytes.getUint8(offset++) & 0x3F /* 0b00111111 */)
				:
			((byte & 0xF0 /* 0b11110000 */) === 0xE0 /* 0b11100000 */)
				? String.fromCharCode(
					(byte & 0xF /* 0b00001111 */) << 12
					| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
					| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
				)
				:
				(byte =
					((byte & 0x7 /* 0b00000111 */) << 18
						| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 12
						| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
						| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
					) - 0x10000
				, String.fromCharCode(Math.floor(byte / 0x400) + 0xD800, byte % 0x400 + 0xDC00)
				);
	}
	return _Utils_Tuple2(offset, string);
});

var _Bytes_decodeFailure = F2(function() { throw 0; });



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}

var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$HoldNothing = {$: 'HoldNothing'};
var $author$project$Main$MousePosition = F4(
	function (x, y, dx, dy) {
		return {dx: dx, dy: dy, x: x, y: y};
	});
var $author$project$Main$SelectNothing = {$: 'SelectNothing'};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$askImageSize = _Platform_outgoingPort('askImageSize', $elm$json$Json$Encode$string);
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$BBox$BBox = F5(
	function (x, y, width, height, hold) {
		return {height: height, hold: hold, width: width, x: x, y: y};
	});
var $author$project$BBox$None = {$: 'None'};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$BBoxies$fromList = function (xs) {
	var len = $elm$core$List$length(xs);
	return {
		entities: $elm$core$Dict$fromList(
			A3(
				$elm$core$List$map2,
				$elm$core$Tuple$pair,
				A2($elm$core$List$range, 0, len - 1),
				xs)),
		nextId: len
	};
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$Main$initialBoxies = $author$project$BBoxies$fromList(
	_List_fromArray(
		[
			A5($author$project$BBox$BBox, 0, -0.68, 201, 46.8, $author$project$BBox$None),
			A5($author$project$BBox$BBox, 6, 188.32, 308, 65.58, $author$project$BBox$None),
			A5($author$project$BBox$BBox, 50, 352, 146, 135, $author$project$BBox$None)
		]));
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Main$init = function (jsVal) {
	var src = A2(
		$elm$core$Result$withDefault,
		'',
		A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$string, jsVal));
	return _Utils_Tuple2(
		{
			boxies: $author$project$Main$initialBoxies,
			clippedImages: _List_Nil,
			hold: $author$project$Main$HoldNothing,
			imgHeight: 10,
			imgSrc: src,
			imgWidth: 10,
			mousePosition: A4($author$project$Main$MousePosition, 0, 0, 0, 0),
			select: $author$project$Main$SelectNothing
		},
		$author$project$Main$askImageSize(src));
};
var $author$project$Main$FailedToLoadImage = {$: 'FailedToLoadImage'};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$failedToLoadImage = _Platform_incomingPort('failedToLoadImage', $elm$json$Json$Decode$value);
var $author$project$Main$Holding = F2(
	function (a, b) {
		return {$: 'Holding', a: a, b: b};
	});
var $author$project$Main$MouseMove = F2(
	function (a, b) {
		return {$: 'MouseMove', a: a, b: b};
	});
var $author$project$Main$MouseUp = function (a) {
	return {$: 'MouseUp', a: a};
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onMouseMove = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mousemove');
var $elm$browser$Browser$Events$onMouseUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mouseup');
var $author$project$Main$holdSub = function (model) {
	var mousemove = $elm$browser$Browser$Events$onMouseMove(
		A3(
			$elm$json$Json$Decode$map2,
			$author$project$Main$MouseMove,
			A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float)));
	var _v0 = model.hold;
	if (_v0.$ === 'HoldNothing') {
		return mousemove;
	} else {
		var id = _v0.a;
		var pos = _v0.b;
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					mousemove,
					$elm$browser$Browser$Events$onMouseMove(
					$elm$json$Json$Decode$succeed(
						A2($author$project$Main$Holding, id, pos))),
					$elm$browser$Browser$Events$onMouseUp(
					$elm$json$Json$Decode$succeed(
						$author$project$Main$MouseUp(id)))
				]));
	}
};
var $author$project$Main$ClippedImagesReceived = function (a) {
	return {$: 'ClippedImagesReceived', a: a};
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Main$receiveClippedImages = _Platform_incomingPort('receiveClippedImages', $elm$json$Json$Decode$value);
var $author$project$Main$receiveClippedImagesSub = function (model) {
	return $author$project$Main$receiveClippedImages(
		function (v) {
			return A2(
				$elm$core$Result$withDefault,
				$author$project$Main$ClippedImagesReceived(_List_Nil),
				A2(
					$elm$json$Json$Decode$decodeValue,
					A2(
						$elm$json$Json$Decode$map,
						$author$project$Main$ClippedImagesReceived,
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
					v));
		});
};
var $author$project$Main$ImageSizeReceived = F2(
	function (a, b) {
		return {$: 'ImageSizeReceived', a: a, b: b};
	});
var $author$project$Main$receiveImageSize = _Platform_incomingPort('receiveImageSize', $elm$json$Json$Decode$value);
var $author$project$Main$receiveImageSizeSub = function (model) {
	return $author$project$Main$receiveImageSize(
		function (v) {
			return A2(
				$elm$core$Result$withDefault,
				A2($author$project$Main$ImageSizeReceived, 0, 0),
				A2(
					$elm$json$Json$Decode$decodeValue,
					A3(
						$elm$json$Json$Decode$map2,
						$author$project$Main$ImageSizeReceived,
						A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float),
						A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float)),
					v));
		});
};
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $author$project$Main$KeyDelete = function (a) {
	return {$: 'KeyDelete', a: a};
};
var $author$project$Main$KeyDowned = function (a) {
	return {$: 'KeyDowned', a: a};
};
var $author$project$Main$KeyOthers = function (a) {
	return {$: 'KeyOthers', a: a};
};
var $author$project$Main$toKey = F2(
	function (id, key) {
		if (key === 'Delete') {
			return $author$project$Main$KeyDowned(
				$author$project$Main$KeyDelete(id));
		} else {
			var c = key;
			return $author$project$Main$KeyDowned(
				$author$project$Main$KeyOthers(c));
		}
	});
var $author$project$Main$selectSub = function (model) {
	var _v0 = model.select;
	if (_v0.$ === 'SelectNothing') {
		return $elm$core$Platform$Sub$none;
	} else {
		var id = _v0.a;
		return $elm$browser$Browser$Events$onKeyDown(
			A2(
				$elm$json$Json$Decode$map,
				$author$project$Main$toKey(id),
				A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string)));
	}
};
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Main$holdSub(model),
				$author$project$Main$selectSub(model),
				$author$project$Main$receiveImageSizeSub(model),
				$author$project$Main$receiveClippedImagesSub(model),
				$author$project$Main$failedToLoadImage(
				function (_v0) {
					return $author$project$Main$FailedToLoadImage;
				})
			]));
};
var $author$project$Main$HoldBBox = F2(
	function (a, b) {
		return {$: 'HoldBBox', a: a, b: b};
	});
var $author$project$Main$ImageSelected = function (a) {
	return {$: 'ImageSelected', a: a};
};
var $author$project$Main$ImageUrlReceived = function (a) {
	return {$: 'ImageUrlReceived', a: a};
};
var $author$project$Main$SelectBBox = function (a) {
	return {$: 'SelectBBox', a: a};
};
var $author$project$BBoxies$add = F2(
	function (entity, _v0) {
		var entities = _v0.entities;
		var nextId = _v0.nextId;
		return {
			entities: A3($elm$core$Dict$insert, nextId, entity, entities),
			nextId: nextId + 1
		};
	});
var $author$project$Main$askClippedImages = _Platform_outgoingPort('askClippedImages', $elm$core$Basics$identity);
var $elm$json$Json$Encode$float = _Json_wrap;
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$BBoxies$map = F2(
	function (f, bboxies) {
		return _Utils_update(
			bboxies,
			{
				entities: A2($elm$core$Dict$map, f, bboxies.entities)
			});
	});
var $author$project$BBox$scale = F2(
	function (ratio, b) {
		var _v0 = ratio;
		var rw = _v0.a;
		var rh = _v0.b;
		return _Utils_update(
			b,
			{height: b.height * rh, width: b.width * rw, x: b.x * rw, y: b.y * rh});
	});
var $author$project$BBoxies$scaleAll = F2(
	function (ratio, bboxies) {
		return A2(
			$author$project$BBoxies$map,
			F2(
				function (i, e) {
					return A2($author$project$BBox$scale, ratio, e);
				}),
			bboxies);
	});
var $author$project$Main$svgSizeWith = F2(
	function (width, height) {
		return _Utils_Tuple2(500, (500 * height) / width);
	});
var $author$project$Main$svgSizeRatio = F2(
	function (width, height) {
		var _v0 = A2($author$project$Main$svgSizeWith, width, height);
		var w = _v0.a;
		var h = _v0.b;
		return _Utils_Tuple2(width / w, height / h);
	});
var $author$project$BBoxies$toList = function (_v0) {
	var entities = _v0.entities;
	return $elm$core$Dict$toList(entities);
};
var $author$project$Main$clipImageCommand = function (model) {
	var boxies = A2(
		$author$project$BBoxies$scaleAll,
		A2($author$project$Main$svgSizeRatio, model.imgWidth, model.imgHeight),
		model.boxies);
	return $author$project$Main$askClippedImages(
		A2(
			$elm$json$Json$Encode$list,
			function (e) {
				var _v0 = e;
				var id = _v0.a;
				var b = _v0.b;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'id',
							$elm$json$Json$Encode$int(id)),
							_Utils_Tuple2(
							'x',
							$elm$json$Json$Encode$float(b.x)),
							_Utils_Tuple2(
							'y',
							$elm$json$Json$Encode$float(b.y)),
							_Utils_Tuple2(
							'width',
							$elm$json$Json$Encode$float(b.width)),
							_Utils_Tuple2(
							'height',
							$elm$json$Json$Encode$float(b.height))
						]));
			},
			$author$project$BBoxies$toList(boxies)));
};
var $jxxcarlson$elm_tar$Tar$BinaryData = function (a) {
	return {$: 'BinaryData', a: a};
};
var $elm$bytes$Bytes$Encode$getWidth = function (builder) {
	switch (builder.$) {
		case 'I8':
			return 1;
		case 'I16':
			return 2;
		case 'I32':
			return 4;
		case 'U8':
			return 1;
		case 'U16':
			return 2;
		case 'U32':
			return 4;
		case 'F32':
			return 4;
		case 'F64':
			return 8;
		case 'Seq':
			var w = builder.a;
			return w;
		case 'Utf8':
			var w = builder.a;
			return w;
		default:
			var bs = builder.a;
			return _Bytes_width(bs);
	}
};
var $elm$bytes$Bytes$LE = {$: 'LE'};
var $elm$bytes$Bytes$Encode$write = F3(
	function (builder, mb, offset) {
		switch (builder.$) {
			case 'I8':
				var n = builder.a;
				return A3(_Bytes_write_i8, mb, offset, n);
			case 'I16':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_i16,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'I32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_i32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'U8':
				var n = builder.a;
				return A3(_Bytes_write_u8, mb, offset, n);
			case 'U16':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_u16,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'U32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_u32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'F32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_f32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'F64':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_f64,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'Seq':
				var bs = builder.b;
				return A3($elm$bytes$Bytes$Encode$writeSequence, bs, mb, offset);
			case 'Utf8':
				var s = builder.b;
				return A3(_Bytes_write_string, mb, offset, s);
			default:
				var bs = builder.a;
				return A3(_Bytes_write_bytes, mb, offset, bs);
		}
	});
var $elm$bytes$Bytes$Encode$writeSequence = F3(
	function (builders, mb, offset) {
		writeSequence:
		while (true) {
			if (!builders.b) {
				return offset;
			} else {
				var b = builders.a;
				var bs = builders.b;
				var $temp$builders = bs,
					$temp$mb = mb,
					$temp$offset = A3($elm$bytes$Bytes$Encode$write, b, mb, offset);
				builders = $temp$builders;
				mb = $temp$mb;
				offset = $temp$offset;
				continue writeSequence;
			}
		}
	});
var $elm$bytes$Bytes$Encode$encode = _Bytes_encode;
var $elm$bytes$Bytes$Encode$Bytes = function (a) {
	return {$: 'Bytes', a: a};
};
var $elm$bytes$Bytes$Encode$bytes = $elm$bytes$Bytes$Encode$Bytes;
var $elm$core$Basics$modBy = _Basics_modBy;
var $jxxcarlson$elm_tar$Octal$octalDigitsHelp = F2(
	function (n, accum) {
		octalDigitsHelp:
		while (true) {
			if (n < 8) {
				return A2($elm$core$List$cons, n, accum);
			} else {
				var lo = A2($elm$core$Basics$modBy, 8, n);
				var hi = (n / 8) | 0;
				var $temp$n = hi,
					$temp$accum = A2($elm$core$List$cons, lo, accum);
				n = $temp$n;
				accum = $temp$accum;
				continue octalDigitsHelp;
			}
		}
	});
var $jxxcarlson$elm_tar$Octal$digits = function (n) {
	return A2($jxxcarlson$elm_tar$Octal$octalDigitsHelp, n, _List_Nil);
};
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm$bytes$Bytes$Encode$Seq = F2(
	function (a, b) {
		return {$: 'Seq', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$getWidths = F2(
	function (width, builders) {
		getWidths:
		while (true) {
			if (!builders.b) {
				return width;
			} else {
				var b = builders.a;
				var bs = builders.b;
				var $temp$width = width + $elm$bytes$Bytes$Encode$getWidth(b),
					$temp$builders = bs;
				width = $temp$width;
				builders = $temp$builders;
				continue getWidths;
			}
		}
	});
var $elm$bytes$Bytes$Encode$sequence = function (builders) {
	return A2(
		$elm$bytes$Bytes$Encode$Seq,
		A2($elm$bytes$Bytes$Encode$getWidths, 0, builders),
		builders);
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm$bytes$Bytes$Encode$U8 = function (a) {
	return {$: 'U8', a: a};
};
var $elm$bytes$Bytes$Encode$unsignedInt8 = $elm$bytes$Bytes$Encode$U8;
var $jxxcarlson$elm_tar$Octal$encode = F2(
	function (width, n) {
		var octalDigits = A2(
			$elm$core$List$map,
			function (x) {
				return $elm$bytes$Bytes$Encode$unsignedInt8(x + 48);
			},
			A2(
				$elm$core$List$take,
				n - 1,
				$jxxcarlson$elm_tar$Octal$digits(n)));
		var padding = A2(
			$elm$core$List$repeat,
			(width - $elm$core$List$length(octalDigits)) - 1,
			$elm$bytes$Bytes$Encode$unsignedInt8(48));
		return $elm$bytes$Bytes$Encode$sequence(
			_Utils_ap(
				padding,
				_Utils_ap(
					octalDigits,
					_List_fromArray(
						[
							$elm$bytes$Bytes$Encode$unsignedInt8(0)
						]))));
	});
var $jxxcarlson$elm_tar$Tar$modeBits = {
	gid: 1024,
	group: {execute: 8, read: 32, write: 16},
	other: {execute: 1, read: 4, write: 2},
	owner: {execute: 64, read: 256, write: 128},
	reserved: 512,
	uid: 2048
};
var $elm$core$Bitwise$or = _Bitwise_or;
var $jxxcarlson$elm_tar$Tar$encodeMode = function (mode) {
	var isSet = F2(
		function (test, bit) {
			return test ? bit : 0;
		});
	var bitflags = A2(isSet, mode.setGroupID, $jxxcarlson$elm_tar$Tar$modeBits.gid) | (A2(isSet, mode.setUserID, $jxxcarlson$elm_tar$Tar$modeBits.uid) | (A2(isSet, mode.other.execute, $jxxcarlson$elm_tar$Tar$modeBits.other.execute) | (A2(isSet, mode.other.write, $jxxcarlson$elm_tar$Tar$modeBits.other.write) | (A2(isSet, mode.other.read, $jxxcarlson$elm_tar$Tar$modeBits.other.read) | (A2(isSet, mode.group.execute, $jxxcarlson$elm_tar$Tar$modeBits.group.execute) | (A2(isSet, mode.group.write, $jxxcarlson$elm_tar$Tar$modeBits.group.write) | (A2(isSet, mode.group.read, $jxxcarlson$elm_tar$Tar$modeBits.group.read) | (A2(isSet, mode.owner.execute, $jxxcarlson$elm_tar$Tar$modeBits.owner.execute) | (A2(isSet, mode.owner.write, $jxxcarlson$elm_tar$Tar$modeBits.owner.write) | (A2(isSet, mode.owner.read, $jxxcarlson$elm_tar$Tar$modeBits.owner.read) | 0))))))))));
	return A2($jxxcarlson$elm_tar$Octal$encode, 8, bitflags);
};
var $elm$bytes$Bytes$Encode$Utf8 = F2(
	function (a, b) {
		return {$: 'Utf8', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$string = function (str) {
	return A2(
		$elm$bytes$Bytes$Encode$Utf8,
		_Bytes_getStringWidth(str),
		str);
};
var $jxxcarlson$elm_tar$Tar$linkEncoder = function (link) {
	switch (link.$) {
		case 'NormalFile':
			return $elm$bytes$Bytes$Encode$string('0');
		case 'HardLink':
			return $elm$bytes$Bytes$Encode$string('1');
		default:
			return $elm$bytes$Bytes$Encode$string('2');
	}
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$concat = $elm$core$String$concat;
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$sliceLeft = F2(
	function (from, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		if (!from) {
			return array;
		} else {
			if (_Utils_cmp(
				from,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					len - from,
					$elm$core$Array$shiftStep,
					$elm$core$Elm$JsArray$empty,
					A3(
						$elm$core$Elm$JsArray$slice,
						from - $elm$core$Array$tailIndex(len),
						$elm$core$Elm$JsArray$length(tail),
						tail));
			} else {
				var skipNodes = (from / $elm$core$Array$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						if (node.$ === 'SubTree') {
							var subTree = node.a;
							return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
						} else {
							var leaf = node.a;
							return A2($elm$core$List$cons, leaf, acc);
						}
					});
				var leafNodes = A3(
					$elm$core$Elm$JsArray$foldr,
					helper,
					_List_fromArray(
						[tail]),
					tree);
				var nodesToInsert = A2($elm$core$List$drop, skipNodes, leafNodes);
				if (!nodesToInsert.b) {
					return $elm$core$Array$empty;
				} else {
					var head = nodesToInsert.a;
					var rest = nodesToInsert.b;
					var firstSlice = from - (skipNodes * $elm$core$Array$branchFactor);
					var initialBuilder = {
						nodeList: _List_Nil,
						nodeListSize: 0,
						tail: A3(
							$elm$core$Elm$JsArray$slice,
							firstSlice,
							$elm$core$Elm$JsArray$length(head),
							head)
					};
					return A2(
						$elm$core$Array$builderToArray,
						true,
						A3($elm$core$List$foldl, $elm$core$Array$appendHelpBuilder, initialBuilder, rest));
				}
			}
		}
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = $elm$core$Array$bitMask & (treeEnd >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var sub = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$end = end,
					$temp$treeEnd = treeEnd,
					$temp$tree = sub;
				shift = $temp$shift;
				end = $temp$end;
				treeEnd = $temp$treeEnd;
				tree = $temp$tree;
				continue fetchNewTail;
			} else {
				var values = _v0.a;
				return A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, values);
			}
		}
	});
var $elm$core$Array$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if ((_Utils_cmp(oldShift, newShift) < 1) || (!$elm$core$Elm$JsArray$length(tree))) {
				return tree;
			} else {
				var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, 0, tree);
				if (_v0.$ === 'SubTree') {
					var sub = _v0.a;
					var $temp$oldShift = oldShift - $elm$core$Array$shiftStep,
						$temp$newShift = newShift,
						$temp$tree = sub;
					oldShift = $temp$oldShift;
					newShift = $temp$newShift;
					tree = $temp$tree;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = $elm$core$Array$bitMask & (endIdx >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, lastPos, tree);
		if (_v0.$ === 'SubTree') {
			var sub = _v0.a;
			var newSub = A3($elm$core$Array$sliceTree, shift - $elm$core$Array$shiftStep, endIdx, sub);
			return (!$elm$core$Elm$JsArray$length(newSub)) ? A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree) : A3(
				$elm$core$Elm$JsArray$unsafeSet,
				lastPos,
				$elm$core$Array$SubTree(newSub),
				A3($elm$core$Elm$JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree);
		}
	});
var $elm$core$Array$sliceRight = F2(
	function (end, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		if (_Utils_eq(end, len)) {
			return array;
		} else {
			if (_Utils_cmp(
				end,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					startShift,
					tree,
					A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, tail));
			} else {
				var endIdx = $elm$core$Array$tailIndex(end);
				var depth = $elm$core$Basics$floor(
					A2(
						$elm$core$Basics$logBase,
						$elm$core$Array$branchFactor,
						A2($elm$core$Basics$max, 1, endIdx - 1)));
				var newShift = A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep);
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					newShift,
					A3(
						$elm$core$Array$hoistTree,
						startShift,
						newShift,
						A3($elm$core$Array$sliceTree, startShift, endIdx, tree)),
					A4($elm$core$Array$fetchNewTail, startShift, end, endIdx, tree));
			}
		}
	});
var $elm$core$Array$translateIndex = F2(
	function (index, _v0) {
		var len = _v0.a;
		var posIndex = (index < 0) ? (len + index) : index;
		return (posIndex < 0) ? 0 : ((_Utils_cmp(posIndex, len) > 0) ? len : posIndex);
	});
var $elm$core$Array$slice = F3(
	function (from, to, array) {
		var correctTo = A2($elm$core$Array$translateIndex, to, array);
		var correctFrom = A2($elm$core$Array$translateIndex, from, array);
		return (_Utils_cmp(correctFrom, correctTo) > 0) ? $elm$core$Array$empty : A2(
			$elm$core$Array$sliceLeft,
			correctFrom,
			A2($elm$core$Array$sliceRight, correctTo, array));
	});
var $elm$core$String$fromList = _String_fromList;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$CR = {$: 'CR'};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Empty = {$: 'Empty'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty = $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Empty;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Empty = {$: 'Empty'};
var $elm$core$String$foldl = _String_foldl;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Error = function (a) {
	return {$: 'Error', a: a};
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$One = {$: 'One'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Two = function (a) {
	return {$: 'Two', a: a};
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Branch = F5(
	function (a, b, c, d, e) {
		return {$: 'Branch', a: a, b: b, c: c, d: d, e: e};
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height = function (rangeDict) {
	if (rangeDict.$ === 'Empty') {
		return 0;
	} else {
		var height_ = rangeDict.a;
		return height_;
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch = F4(
	function (range, value, lt, gt) {
		return A5(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Branch,
			A2(
				$elm$core$Basics$max,
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height(lt),
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height(gt)) + 1,
			range,
			value,
			lt,
			gt);
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff = function (rangeDict) {
	if (rangeDict.$ === 'Empty') {
		return 0;
	} else {
		var lt = rangeDict.d;
		var gt = rangeDict.e;
		return $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height(gt) - $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$height(lt);
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateLeft = function (rangeDict) {
	if ((rangeDict.$ === 'Branch') && (rangeDict.e.$ === 'Branch')) {
		var head = rangeDict.b;
		var value = rangeDict.c;
		var lessThans = rangeDict.d;
		var _v1 = rangeDict.e;
		var subHead = _v1.b;
		var subValue = _v1.c;
		var betweens = _v1.d;
		var greaterThans = _v1.e;
		return A4(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
			subHead,
			subValue,
			A4($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch, head, value, lessThans, betweens),
			greaterThans);
	} else {
		return rangeDict;
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateRight = function (rangeDict) {
	if ((rangeDict.$ === 'Branch') && (rangeDict.d.$ === 'Branch')) {
		var head = rangeDict.b;
		var value = rangeDict.c;
		var _v1 = rangeDict.d;
		var subHead = _v1.b;
		var subValue = _v1.c;
		var lessThans = _v1.d;
		var betweens = _v1.e;
		var greaterThans = rangeDict.e;
		return A4(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
			subHead,
			subValue,
			lessThans,
			A4($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch, head, value, betweens, greaterThans));
	} else {
		return rangeDict;
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$balance = function (rangeDict) {
	if (rangeDict.$ === 'Empty') {
		return rangeDict;
	} else {
		var here = rangeDict.b;
		var value = rangeDict.c;
		var lt = rangeDict.d;
		var gt = rangeDict.e;
		return (_Utils_eq(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(rangeDict),
			-2) && ($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(lt) === 1)) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateRight(
			A4(
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
				here,
				value,
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateLeft(lt),
				gt)) : ((_Utils_cmp(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(rangeDict),
			-1) < 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateRight(rangeDict) : ((($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(rangeDict) === 2) && _Utils_eq(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(gt),
			-1)) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateLeft(
			A4(
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
				here,
				value,
				lt,
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateRight(gt))) : (($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$heightDiff(rangeDict) > 1) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$rotateLeft(rangeDict) : rangeDict)));
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range = F2(
	function (a, b) {
		return {$: 'Range', a: a, b: b};
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Point = function (a) {
	return {$: 'Point', a: a};
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$range = F2(
	function (a, b) {
		return _Utils_eq(a, b) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Point(a) : A2(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range,
			A2($elm$core$Basics$min, a, b),
			A2($elm$core$Basics$max, a, b));
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$combine = F2(
	function (a, b) {
		var _v0 = _Utils_Tuple2(a, b);
		if (_v0.a.$ === 'Point') {
			if (_v0.b.$ === 'Point') {
				var x = _v0.a.a;
				var y = _v0.b.a;
				return A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$range, x, y);
			} else {
				var x = _v0.a.a;
				var _v1 = _v0.b;
				var low = _v1.a;
				var high = _v1.b;
				return A2(
					$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range,
					A2($elm$core$Basics$min, x, low),
					A2($elm$core$Basics$max, x, high));
			}
		} else {
			if (_v0.b.$ === 'Point') {
				var _v2 = _v0.a;
				var low = _v2.a;
				var high = _v2.b;
				var x = _v0.b.a;
				return A2(
					$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range,
					A2($elm$core$Basics$min, x, low),
					A2($elm$core$Basics$max, x, high));
			} else {
				var _v3 = _v0.a;
				var low1 = _v3.a;
				var high1 = _v3.b;
				var _v4 = _v0.b;
				var low2 = _v4.a;
				var high2 = _v4.b;
				return A2(
					$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Range,
					A2($elm$core$Basics$min, low1, low2),
					A2($elm$core$Basics$max, high1, high2));
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$EQ = {$: 'EQ'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$GT = {$: 'GT'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$LT = {$: 'LT'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Overlapping = {$: 'Overlapping'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$compare = F2(
	function (a, b) {
		var _v0 = _Utils_Tuple2(a, b);
		if (_v0.a.$ === 'Point') {
			if (_v0.b.$ === 'Point') {
				var x = _v0.a.a;
				var y = _v0.b.a;
				return (_Utils_cmp(x, y) > 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$GT : ((_Utils_cmp(x, y) < 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$LT : $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$EQ);
			} else {
				var x = _v0.a.a;
				var _v1 = _v0.b;
				var low = _v1.a;
				var high = _v1.b;
				return (_Utils_cmp(x, low) < 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$LT : ((_Utils_cmp(x, high) > 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$GT : $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Overlapping);
			}
		} else {
			if (_v0.b.$ === 'Point') {
				var _v2 = _v0.a;
				var low = _v2.a;
				var high = _v2.b;
				var x = _v0.b.a;
				return (_Utils_cmp(x, low) < 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$GT : ((_Utils_cmp(x, high) > 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$LT : $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Overlapping);
			} else {
				var _v3 = _v0.a;
				var low1 = _v3.a;
				var high1 = _v3.b;
				var _v4 = _v0.b;
				var low2 = _v4.a;
				var high2 = _v4.b;
				return (_Utils_cmp(high1, low2) < 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$LT : ((_Utils_cmp(low1, high2) > 0) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$GT : ((_Utils_eq(low1, low2) && _Utils_eq(high1, high2)) ? $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$EQ : $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Overlapping));
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert = F3(
	function (range, value, set) {
		if (set.$ === 'Empty') {
			return A4($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch, range, value, $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Empty, $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Empty);
		} else {
			var height_ = set.a;
			var here = set.b;
			var hereValue = set.c;
			var lt = set.d;
			var gt = set.e;
			var _v1 = A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$compare, here, range);
			switch (_v1.$) {
				case 'LT':
					return $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$balance(
						A4(
							$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
							here,
							hereValue,
							A3($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert, range, value, lt),
							gt));
				case 'GT':
					return $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$balance(
						A4(
							$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch,
							here,
							hereValue,
							lt,
							A3($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert, range, value, gt)));
				case 'EQ':
					return set;
				default:
					var combined = A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$combine, range, here);
					return A4($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$branch, combined, value, lt, gt);
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$point = $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$Point;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$handleChar = F3(
	function (value, _char, _v0) {
		var parserState = _v0.a;
		var rangeDict = _v0.b;
		var _v1 = _Utils_Tuple2(parserState, _char);
		switch (_v1.a.$) {
			case 'Error':
				return _Utils_Tuple2(parserState, rangeDict);
			case 'Empty':
				switch (_v1.b.valueOf()) {
					case '1':
						var _v2 = _v1.a;
						return _Utils_Tuple2($BrianHicks$elm_string_graphemes$String$Graphemes$Data$One, rangeDict);
					case '2':
						var _v3 = _v1.a;
						return _Utils_Tuple2(
							$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Two($elm$core$Maybe$Nothing),
							rangeDict);
					default:
						var _v4 = _v1.a;
						return _Utils_Tuple2(
							$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Error('expected to see a parsing directive like \'1\' or \'2\''),
							rangeDict);
				}
			case 'One':
				var _v5 = _v1.a;
				return _Utils_Tuple2(
					$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Empty,
					A3(
						$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert,
						$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$point(_char),
						value,
						rangeDict));
			default:
				if (_v1.a.a.$ === 'Nothing') {
					var _v6 = _v1.a.a;
					return _Utils_Tuple2(
						$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Two(
							$elm$core$Maybe$Just(_char)),
						rangeDict);
				} else {
					var low = _v1.a.a.a;
					return _Utils_Tuple2(
						$BrianHicks$elm_string_graphemes$String$Graphemes$Data$Empty,
						A3(
							$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert,
							A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$range, low, _char),
							value,
							rangeDict));
				}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser = F2(
	function (value, source) {
		var _v0 = A3(
			$elm$core$String$foldl,
			$BrianHicks$elm_string_graphemes$String$Graphemes$Data$handleChar(value),
			_Utils_Tuple2($BrianHicks$elm_string_graphemes$String$Graphemes$Data$Empty, $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
			source);
		switch (_v0.a.$) {
			case 'Empty':
				var _v1 = _v0.a;
				var out = _v0.b;
				return $elm$core$Result$Ok(out);
			case 'Error':
				var err = _v0.a.a;
				return $elm$core$Result$Err(err);
			case 'One':
				var _v2 = _v0.a;
				return $elm$core$Result$Err('ended with an empty One');
			default:
				return $elm$core$Result$Err('ended with an empty Two');
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$CR$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$CR))('1\r');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Control = {$: 'Control'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Control$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$Control))('2\u0000\u00092\u000B\u000C2\u000E\u001F2\u007F\u009F1\u00AD1\u061C1\u180E1\u200B2\u200E\u200F1\u20281\u20292\u202A\u202E2\u2060\u20641\u20652\u2066\u206F1\uFEFF2\uFFF0\uFFF82\uFFF9\uFFFB2\uD80D\uDC30\uD80D\uDC382\uD82F\uDCA0\uD82F\uDCA32\uD834\uDD73\uD834\uDD7A1\uDB40\uDC001\uDB40\uDC012\uDB40\uDC02\uDB40\uDC1F2\uDB40\uDC80\uDB40\uDCFF2\uDB40\uDDF0\uDB43\uDFFF');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extend = {$: 'Extend'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extend$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extend))('2̀ͯ2҃҇2҈҉2ֽ֑1ֿ2ׁׂ2ׅׄ1ׇ2ؚؐ2ًٟ1ٰ2ۖۜ2۟ۤ2ۧۨ2۪ۭ1ܑ2ܰ݊2ަް2߫߳1߽2ࠖ࠙2ࠛࠣ2ࠥࠧ2ࠩ࠭2࡙࡛2࣓࣡2ࣣं1ऺ1़2ुै1्2॑ॗ2ॢॣ1ঁ1়1া2ুৄ1্1ৗ2ৢৣ1৾2ਁਂ1਼2ੁੂ2ੇੈ2ੋ੍1ੑ2ੰੱ1ੵ2ઁં1઼2ુૅ2ેૈ1્2ૢૣ2ૺ૿1ଁ1଼1ା1ି2ୁୄ1୍1ୖ1ୗ2ୢୣ1ஂ1ா1ீ1்1ௗ1ఀ1ఄ2ాీ2ెై2ొ్2ౕౖ2ౢౣ1ಁ1಼1ಿ1ೂ1ೆ2ೌ್2ೕೖ2ೢೣ2ഀഁ2഻഼1ാ2ുൄ1്1ൗ2ൢൣ1්1ා2ිු1ූ1ෟ1ั2ิฺ2็๎1ັ2ິຼ2່ໍ2༘༙1༵1༷1༹2ཱཾ2྄ྀ2྆྇2ྍྗ2ྙྼ1࿆2ိူ2ဲ့2္်2ွှ2ၘၙ2ၞၠ2ၱၴ1ႂ2ႅႆ1ႍ1ႝ2፝፟2ᜒ᜔2ᜲ᜴2ᝒᝓ2ᝲᝳ2឴឵2ិួ1ំ2៉៓1៝2᠋᠍2ᢅᢆ1ᢩ2ᤠᤢ2ᤧᤨ1ᤲ2᤻᤹2ᨘᨗ1ᨛ1ᩖ2ᩘᩞ1᩠1ᩢ2ᩥᩬ2ᩳ᩼1᩿2᪽᪰1᪾2ᬀᬃ1᬴1ᬵ2ᬶᬺ1ᬼ1ᭂ2᭫᭳2ᮀᮁ2ᮢᮥ2ᮨᮩ2᮫ᮭ1᯦2ᯨᯩ1ᯭ2ᯯᯱ2ᰬᰳ2ᰶ᰷2᳐᳒2᳔᳠2᳢᳨1᳭1᳴2᳸᳹2᷹᷀2᷿᷻1‌2⃐⃜2⃝⃠1⃡2⃢⃤2⃥⃰2⳯⳱1⵿2ⷠⷿ2〪〭2〮〯2゙゚1꙯2꙰꙲2ꙴ꙽2ꚞꚟ2꛰꛱1ꠂ1꠆1ꠋ2ꠥꠦ2꣄ꣅ2꣠꣱1ꣿ2ꤦ꤭2ꥇꥑ2ꦀꦂ1꦳2ꦶꦹ2ꦼꦽ1ꧥ2ꨩꨮ2ꨱꨲ2ꨵꨶ1ꩃ1ꩌ1ꩼ1ꪰ2ꪴꪲ2ꪷꪸ2ꪾ꪿1꫁2ꫬꫭ1꫶1ꯥ1ꯨ1꯭1ﬞ2︀️2︠︯2ﾞﾟ1𐇽1𐋠2𐍶𐍺2𐨁𐨃2𐨅𐨆2𐨌𐨏2𐨺𐨸1𐨿2𐫦𐫥2𐴤𐴧2𐽆𐽐1𑀁2𑀸𑁆2𑁿𑂁2𑂳𑂶2𑂺𑂹2𑄀𑄂2𑄧𑄫2𑄭𑄴1𑅳2𑆀𑆁2𑆶𑆾2𑇉𑇌2𑈯𑈱1𑈴2𑈶𑈷1𑈾1𑋟2𑋣𑋪2𑌀𑌁2𑌻𑌼1𑌾1𑍀1𑍗2𑍦𑍬2𑍰𑍴2𑐸𑐿2𑑂𑑄1𑑆1𑑞1𑒰2𑒳𑒸1𑒺1𑒽2𑒿𑓀2𑓃𑓂1𑖯2𑖲𑖵2𑖼𑖽2𑗀𑖿2𑗜𑗝2𑘳𑘺1𑘽2𑘿𑙀1𑚫1𑚭2𑚰𑚵1𑚷2𑜝𑜟2𑜢𑜥2𑜧𑜫2𑠯𑠷2𑠺𑠹2𑧔𑧗2𑧚𑧛1𑧠2𑨁𑨊2𑨳𑨸2𑨻𑨾1𑩇2𑩑𑩖2𑩙𑩛2𑪊𑪖2𑪘𑪙2𑰰𑰶2𑰸𑰽1𑰿2𑲒𑲧2𑲪𑲰2𑲲𑲳2𑲵𑲶2𑴱𑴶1𑴺2𑴼𑴽2𑴿𑵅1𑵇2𑶐𑶑1𑶕1𑶗2𑻳𑻴2𖫰𖫴2𖬰𖬶1𖽏2𖾏𖾒2𛲝𛲞1𝅥2𝅧𝅩2𝅮𝅲2𝅻𝆂2𝆋𝆅2𝆪𝆭2𝉂𝉄2𝨀𝨶2𝨻𝩬1𝩵1𝪄2𝪛𝪟2𝪡𝪯2𞀀𞀆2𞀈𞀘2𞀛𞀡2𞀣𞀤2𞀦𞀪2𞄰𞄶2𞋬𞋯2𞣐𞣖2𞥊𞥄2🏻🏿2󠀠󠁿2󠄀󠇯');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$ExtendedPictographic = {$: 'ExtendedPictographic'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extended_Pictographic$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$ExtendedPictographic))('1©1®1‼1⁉1™1ℹ2↔↙2↩↪2⌚⌛1⌨1⎈1⏏2⏩⏳2⏸⏺1Ⓜ2▪▫1▶1◀2◻◾2☀☄1★2☇☍1☎2☏☐1☑1☒2☔☕2☖☗1☘2☙☜1☝2☞☟1☠1☡2☢☣2☤☥1☦2☧☩1☪2☫☭2☮☯2☰☷2☸☺2☻☿1♀1♁1♂2♃♇2♈♓2♔♞1♟1♠2♡♢1♣1♤2♥♦1♧1♨2♩♺1♻2♼♽1♾1♿2⚀⚅2⚐⚑2⚒⚔1⚕2⚖⚗1⚘1⚙1⚚2⚛⚜2⚝⚟2⚠⚡2⚢⚩2⚪⚫2⚬⚯2⚰⚱2⚲⚼2⚽⚾2⚿⛃2⛄⛅2⛆⛇1⛈2⛉⛍2⛎⛏1⛐1⛑1⛒2⛓⛔2⛕⛨2⛩⛪2⛫⛯2⛰⛵1⛶2⛷⛺2⛻⛼1⛽2⛾✁1✂2✃✄1✅2✈✍1✎1✏2✐✑1✒1✔1✖1✝1✡1✨2✳✴1❄1❇1❌1❎2❓❕1❗2❣❤2❥❧2➕➗1➡1➰1➿2⤴⤵2⬅⬇2⬛⬜1⭐1⭕1〰1〽1㊗1㊙2🀀🀃1🀄2🀅🃎1🃏2🃐🃿2🄍🄏1🄯2🅬🅯2🅰🅱2🅾🅿1🆎2🆑🆚2🆭🇥2🈁🈂2🈃🈏1🈚1🈯2🈲🈺2🈼🈿2🉉🉏2🉐🉑2🉒🋿2🌀🌡2🌢🌣2🌤🎓2🎔🎕2🎖🎗1🎘2🎙🎛2🎜🎝2🎞🏰2🏱🏲2🏳🏵1🏶2🏷🏺2🐀📽1📾2📿🔽2🕆🕈2🕉🕎1🕏2🕐🕧2🕨🕮2🕯🕰2🕱🕲2🕳🕹1🕺2🕻🖆1🖇2🖈🖉2🖊🖍2🖎🖏1🖐2🖑🖔2🖕🖖2🖗🖣1🖤1🖥2🖦🖧1🖨2🖩🖰2🖱🖲2🖳🖻1🖼2🖽🗁2🗂🗄2🗅🗐2🗑🗓2🗔🗛2🗜🗞2🗟🗠1🗡1🗢1🗣2🗤🗧1🗨2🗩🗮1🗯2🗰🗲1🗳2🗴🗹2🗺🙏2🚀🛅2🛆🛊2🛋🛐2🛑🛒2🛓🛔1🛕2🛖🛟2🛠🛥2🛦🛨1🛩1🛪2🛫🛬2🛭🛯1🛰2🛱🛲1🛳2🛴🛶2🛷🛸1🛹1🛺2🛻🛿2🝴🝿2🟕🟟2🟠🟫2🟬🟿2🠌🠏2🡈🡏2🡚🡟2🢈🢏2🢮🣿1🤌2🤍🤏2🤐🤘2🤙🤞1🤟2🤠🤧2🤨🤯1🤰2🤱🤲2🤳🤺2🤼🤾1🤿2🥀🥅2🥇🥋1🥌2🥍🥏2🥐🥞2🥟🥫2🥬🥰1🥱1🥲2🥳🥶2🥷🥹1🥺1🥻2🥼🥿2🦀🦄2🦅🦑2🦒🦗2🦘🦢2🦣🦤2🦥🦪2🦫🦭2🦮🦯2🦰🦹2🦺🦿1🧀2🧁🧂2🧃🧊2🧋🧌2🧍🧏2🧐🧦2🧧🧿2🨀🩯2🩰🩳2🩴🩷2🩸🩺2🩻🩿2🪀🪂2🪃🪏2🪐🪕2🪖🿽');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$L = {$: 'L'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$L$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$L))('2ᄀᅟ2ꥠꥼ');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LF = {$: 'LF'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LF$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$LF))('1\n');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LV = {$: 'LV'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LV$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$LV))('1가1개1갸1걔1거1게1겨1계1고1과1괘1괴1교1구1궈1궤1귀1규1그1긔1기1까1깨1꺄1꺠1꺼1께1껴1꼐1꼬1꽈1꽤1꾀1꾜1꾸1꿔1꿰1뀌1뀨1끄1끠1끼1나1내1냐1냬1너1네1녀1녜1노1놔1놰1뇌1뇨1누1눠1눼1뉘1뉴1느1늬1니1다1대1댜1댸1더1데1뎌1뎨1도1돠1돼1되1됴1두1둬1뒈1뒤1듀1드1듸1디1따1때1땨1떄1떠1떼1뗘1뗴1또1똬1뙈1뙤1뚀1뚜1뚸1뛔1뛰1뜌1뜨1띄1띠1라1래1랴1럐1러1레1려1례1로1롸1뢔1뢰1료1루1뤄1뤠1뤼1류1르1릐1리1마1매1먀1먜1머1메1며1몌1모1뫄1뫠1뫼1묘1무1뭐1뭬1뮈1뮤1므1믜1미1바1배1뱌1뱨1버1베1벼1볘1보1봐1봬1뵈1뵤1부1붜1붸1뷔1뷰1브1븨1비1빠1빼1뺘1뺴1뻐1뻬1뼈1뼤1뽀1뽜1뽸1뾔1뾰1뿌1뿨1쀄1쀠1쀼1쁘1쁴1삐1사1새1샤1섀1서1세1셔1셰1소1솨1쇄1쇠1쇼1수1숴1쉐1쉬1슈1스1싀1시1싸1쌔1쌰1썌1써1쎄1쎠1쎼1쏘1쏴1쐐1쐬1쑈1쑤1쒀1쒜1쒸1쓔1쓰1씌1씨1아1애1야1얘1어1에1여1예1오1와1왜1외1요1우1워1웨1위1유1으1의1이1자1재1쟈1쟤1저1제1져1졔1조1좌1좨1죄1죠1주1줘1줴1쥐1쥬1즈1즤1지1짜1째1쨔1쨰1쩌1쩨1쪄1쪠1쪼1쫘1쫴1쬐1쬬1쭈1쭤1쮀1쮜1쮸1쯔1쯰1찌1차1채1챠1챼1처1체1쳐1쳬1초1촤1쵀1최1쵸1추1춰1췌1취1츄1츠1츼1치1카1캐1캬1컈1커1케1켜1켸1코1콰1쾌1쾨1쿄1쿠1쿼1퀘1퀴1큐1크1킈1키1타1태1탸1턔1터1테1텨1톄1토1톼1퇘1퇴1툐1투1퉈1퉤1튀1튜1트1틔1티1파1패1퍄1퍠1퍼1페1펴1폐1포1퐈1퐤1푀1표1푸1풔1풰1퓌1퓨1프1픠1피1하1해1햐1햬1허1헤1혀1혜1호1화1홰1회1효1후1훠1훼1휘1휴1흐1희1히');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LVT = {$: 'LVT'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LVT$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$LVT))('2각갛2객갷2갹걓2걕걯2걱겋2겍겧2격곃2곅곟2곡곻2곽괗2괙괳2괵굏2굑굫2국궇2궉궣2궥궿2귁귛2귝귷2극긓2긕긯2긱깋2깍깧2깩꺃2꺅꺟2꺡꺻2꺽껗2껙껳2껵꼏2꼑꼫2꼭꽇2꽉꽣2꽥꽿2꾁꾛2꾝꾷2꾹꿓2꿕꿯2꿱뀋2뀍뀧2뀩끃2끅끟2끡끻2끽낗2낙낳2낵냏2냑냫2냭넇2넉넣2넥넿2녁녛2녝녷2녹놓2놕놯2놱뇋2뇍뇧2뇩눃2눅눟2눡눻2눽뉗2뉙뉳2뉵늏2늑늫2늭닇2닉닣2닥닿2댁댛2댝댷2댹덓2덕덯2덱뎋2뎍뎧2뎩돃2독돟2돡돻2돽됗2됙됳2됵둏2둑둫2둭뒇2뒉뒣2뒥뒿2듁듛2득듷2듹딓2딕딯2딱땋2땍땧2땩떃2떅떟2떡떻2떽뗗2뗙뗳2뗵똏2똑똫2똭뙇2뙉뙣2뙥뙿2뚁뚛2뚝뚷2뚹뛓2뛕뛯2뛱뜋2뜍뜧2뜩띃2띅띟2띡띻2락랗2랙랳2략럏2럑럫2럭렇2렉렣2력렿2롁롛2록롷2롹뢓2뢕뢯2뢱룋2룍룧2룩뤃2뤅뤟2뤡뤻2뤽륗2륙륳2륵릏2릑릫2릭맇2막맣2맥맿2먁먛2먝먷2먹멓2멕멯2멱몋2몍몧2목뫃2뫅뫟2뫡뫻2뫽묗2묙묳2묵뭏2뭑뭫2뭭뮇2뮉뮣2뮥뮿2믁믛2믝믷2믹밓2박밯2백뱋2뱍뱧2뱩벃2벅벟2벡벻2벽볗2볙볳2복봏2봑봫2봭뵇2뵉뵣2뵥뵿2북붛2붝붷2붹뷓2뷕뷯2뷱븋2븍븧2븩빃2빅빟2빡빻2빽뺗2뺙뺳2뺵뻏2뻑뻫2뻭뼇2뼉뼣2뼥뼿2뽁뽛2뽝뽷2뽹뾓2뾕뾯2뾱뿋2뿍뿧2뿩쀃2쀅쀟2쀡쀻2쀽쁗2쁙쁳2쁵삏2삑삫2삭샇2색샣2샥샿2섁섛2석섷2섹셓2셕셯2셱솋2속솧2솩쇃2쇅쇟2쇡쇻2쇽숗2숙숳2숵쉏2쉑쉫2쉭슇2슉슣2슥슿2싁싛2식싷2싹쌓2쌕쌯2쌱썋2썍썧2썩쎃2쎅쎟2쎡쎻2쎽쏗2쏙쏳2쏵쐏2쐑쐫2쐭쑇2쑉쑣2쑥쑿2쒁쒛2쒝쒷2쒹쓓2쓕쓯2쓱씋2씍씧2씩앃2악앟2액앻2약얗2얙얳2억엏2엑엫2역옇2옉옣2옥옿2왁왛2왝왷2왹욓2욕욯2욱웋2웍웧2웩윃2윅윟2육윻2윽읗2읙읳2익잏2작잫2잭쟇2쟉쟣2쟥쟿2적젛2젝젷2젹졓2졕졯2족좋2좍좧2좩죃2죅죟2죡죻2죽줗2줙줳2줵쥏2쥑쥫2쥭즇2즉즣2즥즿2직짛2짝짷2짹쨓2쨕쨯2쨱쩋2쩍쩧2쩩쪃2쪅쪟2쪡쪻2쪽쫗2쫙쫳2쫵쬏2쬑쬫2쬭쭇2쭉쭣2쭥쭿2쮁쮛2쮝쮷2쮹쯓2쯕쯯2쯱찋2찍찧2착챃2책챟2챡챻2챽첗2척첳2첵쳏2쳑쳫2쳭촇2촉촣2촥촿2쵁쵛2쵝쵷2쵹춓2축춯2춱췋2췍췧2췩츃2츅츟2측츻2츽칗2칙칳2칵캏2캑캫2캭컇2컉컣2컥컿2켁켛2켝켷2켹콓2콕콯2콱쾋2쾍쾧2쾩쿃2쿅쿟2쿡쿻2쿽퀗2퀙퀳2퀵큏2큑큫2큭킇2킉킣2킥킿2탁탛2택탷2탹턓2턕턯2턱텋2텍텧2텩톃2톅톟2톡톻2톽퇗2퇙퇳2퇵툏2툑툫2툭퉇2퉉퉣2퉥퉿2튁튛2튝튷2특틓2틕틯2틱팋2팍팧2팩퍃2퍅퍟2퍡퍻2퍽펗2펙펳2펵폏2폑폫2폭퐇2퐉퐣2퐥퐿2푁푛2푝푷2푹풓2풕풯2풱퓋2퓍퓧2퓩픃2픅픟2픡픻2픽핗2학핳2핵햏2햑햫2햭헇2헉헣2헥헿2혁혛2혝혷2혹홓2확홯2홱횋2획횧2횩훃2훅훟2훡훻2훽휗2휙휳2휵흏2흑흫2흭힇2힉힣');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Prepend = {$: 'Prepend'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Prepend$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$Prepend))('2؀؅1۝1܏1࣢1ൎ1𑂽1𑃍2𑇂𑇃1𑨺2𑪄𑪉1𑵆');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$RegionalIndicator = {$: 'RegionalIndicator'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Regional_Indicator$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$RegionalIndicator))('2🇦🇿');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$SpacingMark = {$: 'SpacingMark'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$SpacingMark$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$SpacingMark))('1ः1ऻ2ाी2ॉौ2ॎॏ2ংঃ2িী2েৈ2োৌ1ਃ2ਾੀ1ઃ2ાી1ૉ2ોૌ2ଂଃ1ୀ2େୈ2ୋୌ1ி2ுூ2ெை2ொௌ2ఁః2ుౄ2ಂಃ1ಾ2ೀು2ೃೄ2ೇೈ2ೊೋ2ംഃ2ിീ2െൈ2ൊൌ2ංඃ2ැෑ2ෘෞ2ෲෳ1ำ1ຳ2༾༿1ཿ1ေ2ျြ2ၖၗ1ႄ1ា2ើៅ2ះៈ2ᤣᤦ2ᤩᤫ2ᤰᤱ2ᤳᤸ2ᨙᨚ1ᩕ1ᩗ2ᩭᩲ1ᬄ1ᬻ2ᬽᭁ2ᭃ᭄1ᮂ1ᮡ2ᮦᮧ1᮪1ᯧ2ᯪᯬ1ᯮ2᯲᯳2ᰤᰫ2ᰴᰵ1᳡1᳷2ꠣꠤ1ꠧ2ꢀꢁ2ꢴꣃ2ꥒ꥓1ꦃ2ꦴꦵ2ꦺꦻ2ꦾ꧀2ꨯꨰ2ꨳꨴ1ꩍ1ꫫ2ꫮꫯ1ꫵ2ꯣꯤ2ꯦꯧ2ꯩꯪ1꯬1𑀀1𑀂1𑂂2𑂰𑂲2𑂷𑂸1𑄬2𑅅𑅆1𑆂2𑆳𑆵2𑆿𑇀2𑈬𑈮2𑈲𑈳1𑈵2𑋠𑋢2𑌂𑌃1𑌿2𑍁𑍄2𑍇𑍈2𑍋𑍍2𑍢𑍣2𑐵𑐷2𑑀𑑁1𑑅2𑒱𑒲1𑒹2𑒻𑒼1𑒾1𑓁2𑖰𑖱2𑖸𑖻1𑖾2𑘰𑘲2𑘻𑘼1𑘾1𑚬2𑚮𑚯1𑚶2𑜠𑜡1𑜦2𑠬𑠮1𑠸2𑧑𑧓2𑧜𑧟1𑧤1𑨹2𑩗𑩘1𑪗1𑰯1𑰾1𑲩1𑲱1𑲴2𑶊𑶎2𑶓𑶔1𑶖2𑻵𑻶2𖽑𖾇1𝅦1𝅭');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$T = {$: 'T'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$T$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$T))('2ᆨᇿ2ퟋퟻ');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$V = {$: 'V'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$V$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$V))('2ᅠᆧ2ힰퟆ');
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$ZWJ = {$: 'ZWJ'};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Data$ZWJ$chars = A2(
	$elm$core$Basics$composeL,
	$elm$core$Result$withDefault($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty),
	$BrianHicks$elm_string_graphemes$String$Graphemes$Data$parser($BrianHicks$elm_string_graphemes$String$Graphemes$Data$ZWJ))('1‍');
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$toList = function (rangeDict) {
	if (rangeDict.$ === 'Empty') {
		return _List_Nil;
	} else {
		var here = rangeDict.b;
		var value = rangeDict.c;
		var lt = rangeDict.d;
		var gt = rangeDict.e;
		return _Utils_ap(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$toList(lt),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(here, value),
				$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$toList(gt)));
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$union = F2(
	function (a, b) {
		return A3(
			$elm$core$List$foldl,
			function (_v0) {
				var range_ = _v0.a;
				var value = _v0.b;
				return A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$insert, range_, value);
			},
			b,
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$toList(a));
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$classes = A3(
	$elm$core$List$foldl,
	$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$union,
	$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$empty,
	_List_fromArray(
		[$BrianHicks$elm_string_graphemes$String$Graphemes$Data$CR$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LF$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Control$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extend$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Regional_Indicator$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Prepend$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$SpacingMark$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$L$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$V$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$T$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LV$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$LVT$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$Extended_Pictographic$chars, $BrianHicks$elm_string_graphemes$String$Graphemes$Data$ZWJ$chars]));
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$getHelp = F2(
	function (range, rangeDict) {
		getHelp:
		while (true) {
			if (rangeDict.$ === 'Empty') {
				return $elm$core$Maybe$Nothing;
			} else {
				var height_ = rangeDict.a;
				var here = rangeDict.b;
				var value = rangeDict.c;
				var lt = rangeDict.d;
				var gt = rangeDict.e;
				var _v1 = A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$compare, range, here);
				switch (_v1.$) {
					case 'LT':
						var $temp$range = range,
							$temp$rangeDict = gt;
						range = $temp$range;
						rangeDict = $temp$rangeDict;
						continue getHelp;
					case 'GT':
						var $temp$range = range,
							$temp$rangeDict = lt;
						range = $temp$range;
						rangeDict = $temp$rangeDict;
						continue getHelp;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						return $elm$core$Maybe$Just(value);
				}
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$get = F2(
	function (what, rangeDict) {
		return A2(
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$getHelp,
			$BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$Range$point(what),
			rangeDict);
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Basics$not = _Basics_not;
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakForRule11 = function (classes_) {
	shouldBreakForRule11:
	while (true) {
		_v0$2:
		while (true) {
			if (classes_.b && (classes_.a.$ === 'Just')) {
				switch (classes_.a.a.$) {
					case 'Extend':
						var _v1 = classes_.a.a;
						var rest = classes_.b;
						var $temp$classes_ = rest;
						classes_ = $temp$classes_;
						continue shouldBreakForRule11;
					case 'ExtendedPictographic':
						if (!classes_.b.b) {
							var _v2 = classes_.a.a;
							return false;
						} else {
							break _v0$2;
						}
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return true;
	}
};
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakBefore = F3(
	function (lastChar, restChars, nextChar) {
		var _v0 = _Utils_Tuple2(
			A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$get, lastChar, $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$classes),
			A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$get, nextChar, $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$classes));
		_v0$1:
		while (true) {
			_v0$8:
			while (true) {
				_v0$20:
				while (true) {
					_v0$21:
					while (true) {
						_v0$22:
						while (true) {
							_v0$23:
							while (true) {
								if (_v0.a.$ === 'Just') {
									switch (_v0.a.a.$) {
										case 'CR':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'LF':
														var _v1 = _v0.a.a;
														var _v2 = _v0.b.a;
														return false;
													case 'ZWJ':
														break _v0$1;
													case 'SpacingMark':
														break _v0$1;
													case 'Extend':
														break _v0$1;
													default:
														break _v0$1;
												}
											} else {
												break _v0$1;
											}
										case 'LF':
											var _v4 = _v0.a.a;
											return true;
										case 'Control':
											var _v5 = _v0.a.a;
											return true;
										case 'RegionalIndicator':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'RegionalIndicator':
														var _v6 = _v0.a.a;
														var _v7 = _v0.b.a;
														return !$elm$core$List$isEmpty(restChars);
													case 'ZWJ':
														break _v0$20;
													case 'SpacingMark':
														break _v0$21;
													case 'Extend':
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 'Prepend':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'CR':
														var _v8 = _v0.a.a;
														var _v9 = _v0.b.a;
														return true;
													case 'LF':
														var _v10 = _v0.a.a;
														var _v11 = _v0.b.a;
														return true;
													case 'Control':
														var _v12 = _v0.a.a;
														var _v13 = _v0.b.a;
														return true;
													case 'ZWJ':
														break _v0$8;
													case 'SpacingMark':
														break _v0$8;
													case 'Extend':
														break _v0$8;
													default:
														break _v0$8;
												}
											} else {
												break _v0$8;
											}
										case 'L':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'L':
														var _v15 = _v0.a.a;
														var _v16 = _v0.b.a;
														return false;
													case 'V':
														var _v17 = _v0.a.a;
														var _v18 = _v0.b.a;
														return false;
													case 'LV':
														var _v19 = _v0.a.a;
														var _v20 = _v0.b.a;
														return false;
													case 'LVT':
														var _v21 = _v0.a.a;
														var _v22 = _v0.b.a;
														return false;
													case 'ZWJ':
														break _v0$20;
													case 'SpacingMark':
														break _v0$21;
													case 'Extend':
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 'V':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'V':
														var _v23 = _v0.a.a;
														var _v24 = _v0.b.a;
														return false;
													case 'T':
														var _v25 = _v0.a.a;
														var _v26 = _v0.b.a;
														return false;
													case 'ZWJ':
														break _v0$20;
													case 'SpacingMark':
														break _v0$21;
													case 'Extend':
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 'T':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'T':
														var _v27 = _v0.a.a;
														var _v28 = _v0.b.a;
														return false;
													case 'ZWJ':
														break _v0$20;
													case 'SpacingMark':
														break _v0$21;
													case 'Extend':
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 'LV':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'V':
														var _v29 = _v0.a.a;
														var _v30 = _v0.b.a;
														return false;
													case 'T':
														var _v31 = _v0.a.a;
														var _v32 = _v0.b.a;
														return false;
													case 'ZWJ':
														break _v0$20;
													case 'SpacingMark':
														break _v0$21;
													case 'Extend':
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 'LVT':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'T':
														var _v33 = _v0.a.a;
														var _v34 = _v0.b.a;
														return false;
													case 'ZWJ':
														break _v0$20;
													case 'SpacingMark':
														break _v0$21;
													case 'Extend':
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										case 'ZWJ':
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'ExtendedPictographic':
														var _v35 = _v0.a.a;
														var _v36 = _v0.b.a;
														return $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakForRule11(
															A2(
																$elm$core$List$map,
																function (c) {
																	return A2($BrianHicks$elm_string_graphemes$String$Graphemes$RangeDict$get, c, $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$classes);
																},
																restChars));
													case 'ZWJ':
														break _v0$20;
													case 'SpacingMark':
														break _v0$21;
													case 'Extend':
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
										default:
											if (_v0.b.$ === 'Just') {
												switch (_v0.b.a.$) {
													case 'ZWJ':
														break _v0$20;
													case 'SpacingMark':
														break _v0$21;
													case 'Extend':
														break _v0$22;
													default:
														break _v0$23;
												}
											} else {
												break _v0$23;
											}
									}
								} else {
									if (_v0.b.$ === 'Just') {
										switch (_v0.b.a.$) {
											case 'ZWJ':
												break _v0$20;
											case 'SpacingMark':
												break _v0$21;
											case 'Extend':
												break _v0$22;
											default:
												break _v0$23;
										}
									} else {
										break _v0$23;
									}
								}
							}
							return true;
						}
						var _v39 = _v0.b.a;
						return false;
					}
					var _v38 = _v0.b.a;
					return false;
				}
				var _v37 = _v0.b.a;
				return false;
			}
			var _v14 = _v0.a.a;
			return false;
		}
		var _v3 = _v0.a.a;
		return true;
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$unconsHelp = F2(
	function (str, chars) {
		unconsHelp:
		while (true) {
			var _v0 = _Utils_Tuple2(
				$elm$core$String$uncons(str),
				chars);
			if (_v0.a.$ === 'Nothing') {
				if (!_v0.b.b) {
					var _v1 = _v0.a;
					if (A2($elm$core$String$left, 1, str) === '\u0000') {
						var $temp$str = A2($elm$core$String$dropLeft, 1, str),
							$temp$chars = _List_fromArray(
							[
								_Utils_chr('\u0000')
							]);
						str = $temp$str;
						chars = $temp$chars;
						continue unconsHelp;
					} else {
						return _Utils_Tuple2(chars, '');
					}
				} else {
					var _v2 = _v0.a;
					var _v3 = _v0.b;
					var last = _v3.a;
					var rest = _v3.b;
					if (A2($elm$core$String$left, 1, str) === '\u0000') {
						if (A3(
							$BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakBefore,
							last,
							rest,
							_Utils_chr('\u0000'))) {
							return _Utils_Tuple2(chars, str);
						} else {
							var $temp$str = A2($elm$core$String$dropLeft, 1, str),
								$temp$chars = A2(
								$elm$core$List$cons,
								_Utils_chr('\u0000'),
								chars);
							str = $temp$str;
							chars = $temp$chars;
							continue unconsHelp;
						}
					} else {
						return _Utils_Tuple2(chars, '');
					}
				}
			} else {
				if (!_v0.b.b) {
					var _v4 = _v0.a.a;
					var _char = _v4.a;
					var strTail = _v4.b;
					var $temp$str = strTail,
						$temp$chars = _List_fromArray(
						[_char]);
					str = $temp$str;
					chars = $temp$chars;
					continue unconsHelp;
				} else {
					var _v5 = _v0.a.a;
					var _char = _v5.a;
					var strTail = _v5.b;
					var _v6 = _v0.b;
					var last = _v6.a;
					var rest = _v6.b;
					if (A3($BrianHicks$elm_string_graphemes$String$Graphemes$Parser$shouldBreakBefore, last, rest, _char)) {
						return _Utils_Tuple2(chars, str);
					} else {
						var $temp$str = strTail,
							$temp$chars = A2($elm$core$List$cons, _char, chars);
						str = $temp$str;
						chars = $temp$chars;
						continue unconsHelp;
					}
				}
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$foldl = F3(
	function (fn, initial, string) {
		foldl:
		while (true) {
			if (string === '') {
				return initial;
			} else {
				var _v1 = A2($BrianHicks$elm_string_graphemes$String$Graphemes$Parser$unconsHelp, string, _List_Nil);
				var chars = _v1.a;
				var remaining = _v1.b;
				var $temp$fn = fn,
					$temp$initial = A2(
					fn,
					$elm$core$String$fromList(
						$elm$core$List$reverse(chars)),
					initial),
					$temp$string = remaining;
				fn = $temp$fn;
				initial = $temp$initial;
				string = $temp$string;
				continue foldl;
			}
		}
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$foldl = $BrianHicks$elm_string_graphemes$String$Graphemes$Parser$foldl;
var $BrianHicks$elm_string_graphemes$String$Graphemes$foldr = F3(
	function (fn, state, string) {
		return A3(
			$elm$core$List$foldl,
			fn,
			state,
			A3($BrianHicks$elm_string_graphemes$String$Graphemes$foldl, $elm$core$List$cons, _List_Nil, string));
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$toList = A2($BrianHicks$elm_string_graphemes$String$Graphemes$foldr, $elm$core$List$cons, _List_Nil);
var $BrianHicks$elm_string_graphemes$String$Graphemes$slice = F3(
	function (start, end, string) {
		return $BrianHicks$elm_string_graphemes$String$Graphemes$concat(
			$elm$core$Array$toList(
				A3(
					$elm$core$Array$slice,
					start,
					end,
					$elm$core$Array$fromList(
						$BrianHicks$elm_string_graphemes$String$Graphemes$toList(string)))));
	});
var $BrianHicks$elm_string_graphemes$String$Graphemes$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($BrianHicks$elm_string_graphemes$String$Graphemes$slice, 0, -n, string);
	});
var $elm$bytes$Bytes$Encode$getStringWidth = _Bytes_getStringWidth;
var $jxxcarlson$elm_tar$Tar$dropRightLoop = F2(
	function (desiredLength, str) {
		dropRightLoop:
		while (true) {
			if (_Utils_cmp(
				$elm$bytes$Bytes$Encode$getStringWidth(str),
				desiredLength) > 0) {
				var $temp$desiredLength = desiredLength,
					$temp$str = A2($BrianHicks$elm_string_graphemes$String$Graphemes$dropRight, 1, str);
				desiredLength = $temp$desiredLength;
				str = $temp$str;
				continue dropRightLoop;
			} else {
				return str;
			}
		}
	});
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $jxxcarlson$elm_tar$Tar$normalizeString = F2(
	function (desiredLength, str) {
		if (!desiredLength) {
			return '';
		} else {
			var dropped = A2(
				$jxxcarlson$elm_tar$Tar$dropRightLoop,
				desiredLength - 1,
				A2($elm$core$String$left, desiredLength, str));
			var paddingSize = desiredLength - $elm$bytes$Bytes$Encode$getStringWidth(dropped);
			return _Utils_ap(
				dropped,
				A2($elm$core$String$repeat, paddingSize, '\u0000'));
		}
	});
var $elm$bytes$Bytes$Decode$decode = F2(
	function (_v0, bs) {
		var decoder = _v0.a;
		return A2(_Bytes_decode, decoder, bs);
	});
var $elm$bytes$Bytes$Decode$Decoder = function (a) {
	return {$: 'Decoder', a: a};
};
var $elm$bytes$Bytes$Decode$loopHelp = F4(
	function (state, callback, bites, offset) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var decoder = _v0.a;
			var _v1 = A2(decoder, bites, offset);
			var newOffset = _v1.a;
			var step = _v1.b;
			if (step.$ === 'Loop') {
				var newState = step.a;
				var $temp$state = newState,
					$temp$callback = callback,
					$temp$bites = bites,
					$temp$offset = newOffset;
				state = $temp$state;
				callback = $temp$callback;
				bites = $temp$bites;
				offset = $temp$offset;
				continue loopHelp;
			} else {
				var result = step.a;
				return _Utils_Tuple2(newOffset, result);
			}
		}
	});
var $elm$bytes$Bytes$Decode$loop = F2(
	function (state, callback) {
		return $elm$bytes$Bytes$Decode$Decoder(
			A2($elm$bytes$Bytes$Decode$loopHelp, state, callback));
	});
var $elm$bytes$Bytes$BE = {$: 'BE'};
var $elm$bytes$Bytes$Decode$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$bytes$Bytes$Decode$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$bytes$Bytes$Decode$map = F2(
	function (func, _v0) {
		var decodeA = _v0.a;
		return $elm$bytes$Bytes$Decode$Decoder(
			F2(
				function (bites, offset) {
					var _v1 = A2(decodeA, bites, offset);
					var aOffset = _v1.a;
					var a = _v1.b;
					return _Utils_Tuple2(
						aOffset,
						func(a));
				}));
	});
var $elm$bytes$Bytes$Decode$map4 = F5(
	function (func, _v0, _v1, _v2, _v3) {
		var decodeA = _v0.a;
		var decodeB = _v1.a;
		var decodeC = _v2.a;
		var decodeD = _v3.a;
		return $elm$bytes$Bytes$Decode$Decoder(
			F2(
				function (bites, offset) {
					var _v4 = A2(decodeA, bites, offset);
					var aOffset = _v4.a;
					var a = _v4.b;
					var _v5 = A2(decodeB, bites, aOffset);
					var bOffset = _v5.a;
					var b = _v5.b;
					var _v6 = A2(decodeC, bites, bOffset);
					var cOffset = _v6.a;
					var c = _v6.b;
					var _v7 = A2(decodeD, bites, cOffset);
					var dOffset = _v7.a;
					var d = _v7.b;
					return _Utils_Tuple2(
						dOffset,
						A4(func, a, b, c, d));
				}));
	});
var $elm$bytes$Bytes$Decode$succeed = function (a) {
	return $elm$bytes$Bytes$Decode$Decoder(
		F2(
			function (_v0, offset) {
				return _Utils_Tuple2(offset, a);
			}));
};
var $elm$bytes$Bytes$Decode$unsignedInt32 = function (endianness) {
	return $elm$bytes$Bytes$Decode$Decoder(
		_Bytes_read_u32(
			_Utils_eq(endianness, $elm$bytes$Bytes$LE)));
};
var $elm$bytes$Bytes$Decode$unsignedInt8 = $elm$bytes$Bytes$Decode$Decoder(_Bytes_read_u8);
var $jxxcarlson$elm_tar$Tar$sumBytesHelp = function (_v0) {
	var remaining = _v0.remaining;
	var accum = _v0.accum;
	return (remaining >= 16) ? A5(
		$elm$bytes$Bytes$Decode$map4,
		F4(
			function (word1, word2, word3, word4) {
				return $elm$bytes$Bytes$Decode$Loop(
					{accum: (255 & word4) + ((255 & (word4 >>> 8)) + ((255 & (word4 >>> 16)) + ((255 & (word4 >>> 24)) + ((255 & word3) + ((255 & (word3 >>> 8)) + ((255 & (word3 >>> 16)) + ((255 & (word3 >>> 24)) + ((255 & word2) + ((255 & (word2 >>> 8)) + ((255 & (word2 >>> 16)) + ((255 & (word2 >>> 24)) + ((255 & word1) + ((255 & (word1 >>> 8)) + ((255 & (word1 >>> 16)) + ((255 & (word1 >>> 24)) + accum))))))))))))))), remaining: remaining - 16});
			}),
		$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE),
		$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE),
		$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE),
		$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE)) : ((remaining > 0) ? A2(
		$elm$bytes$Bytes$Decode$map,
		function (_new) {
			return $elm$bytes$Bytes$Decode$Loop(
				{accum: _new + accum, remaining: remaining - 1});
		},
		$elm$bytes$Bytes$Decode$unsignedInt8) : $elm$bytes$Bytes$Decode$succeed(
		$elm$bytes$Bytes$Decode$Done(accum)));
};
var $elm$bytes$Bytes$width = _Bytes_width;
var $jxxcarlson$elm_tar$Tar$sumBytes = function (bytes) {
	var decoder = A2(
		$elm$bytes$Bytes$Decode$loop,
		{
			accum: 0,
			remaining: $elm$bytes$Bytes$width(bytes)
		},
		$jxxcarlson$elm_tar$Tar$sumBytesHelp);
	var _v0 = A2($elm$bytes$Bytes$Decode$decode, decoder, bytes);
	if (_v0.$ === 'Just') {
		var v = _v0.a;
		return v;
	} else {
		return 0;
	}
};
var $jxxcarlson$elm_tar$Tar$encodeMetadata = function (metadata) {
	var metaDataTop = $elm$bytes$Bytes$Encode$encode(
		$elm$bytes$Bytes$Encode$sequence(
			_List_fromArray(
				[
					$elm$bytes$Bytes$Encode$string(
					A2($jxxcarlson$elm_tar$Tar$normalizeString, 100, metadata.filename)),
					$jxxcarlson$elm_tar$Tar$encodeMode(metadata.mode),
					$elm$bytes$Bytes$Encode$sequence(
					_List_fromArray(
						[
							A2($jxxcarlson$elm_tar$Octal$encode, 8, metadata.ownerID)
						])),
					$elm$bytes$Bytes$Encode$sequence(
					_List_fromArray(
						[
							A2($jxxcarlson$elm_tar$Octal$encode, 8, metadata.groupID)
						])),
					$elm$bytes$Bytes$Encode$sequence(
					_List_fromArray(
						[
							A2($jxxcarlson$elm_tar$Octal$encode, 12, metadata.fileSize)
						])),
					$elm$bytes$Bytes$Encode$sequence(
					_List_fromArray(
						[
							A2($jxxcarlson$elm_tar$Octal$encode, 12, metadata.lastModificationTime)
						]))
				])));
	var metaDataBottom = $elm$bytes$Bytes$Encode$encode(
		$elm$bytes$Bytes$Encode$sequence(
			_List_fromArray(
				[
					$jxxcarlson$elm_tar$Tar$linkEncoder(metadata.linkIndicator),
					$elm$bytes$Bytes$Encode$string(
					A2($jxxcarlson$elm_tar$Tar$normalizeString, 100, metadata.linkedFileName)),
					$elm$bytes$Bytes$Encode$string('ustar\u0000'),
					$elm$bytes$Bytes$Encode$string('00'),
					$elm$bytes$Bytes$Encode$string(
					A2($jxxcarlson$elm_tar$Tar$normalizeString, 32, metadata.userName)),
					$elm$bytes$Bytes$Encode$string(
					A2($jxxcarlson$elm_tar$Tar$normalizeString, 32, metadata.groupName)),
					$elm$bytes$Bytes$Encode$sequence(
					_List_fromArray(
						[
							A2($jxxcarlson$elm_tar$Octal$encode, 8, 0)
						])),
					$elm$bytes$Bytes$Encode$sequence(
					_List_fromArray(
						[
							A2($jxxcarlson$elm_tar$Octal$encode, 8, 0)
						])),
					$elm$bytes$Bytes$Encode$string(
					A2($jxxcarlson$elm_tar$Tar$normalizeString, 167, metadata.fileNamePrefix))
				])));
	var preliminary = _List_fromArray(
		[
			$elm$bytes$Bytes$Encode$bytes(metaDataTop),
			$elm$bytes$Bytes$Encode$string(
			A2($elm$core$String$repeat, 8, ' ')),
			$elm$bytes$Bytes$Encode$bytes(metaDataBottom)
		]);
	var checksum = $jxxcarlson$elm_tar$Tar$sumBytes(
		$elm$bytes$Bytes$Encode$encode(
			$elm$bytes$Bytes$Encode$sequence(preliminary)));
	return $elm$bytes$Bytes$Encode$sequence(
		_List_fromArray(
			[
				$elm$bytes$Bytes$Encode$bytes(metaDataTop),
				A2($jxxcarlson$elm_tar$Octal$encode, 7, checksum),
				$elm$bytes$Bytes$Encode$string(' '),
				$elm$bytes$Bytes$Encode$bytes(metaDataBottom)
			]));
};
var $elm$bytes$Bytes$Encode$U32 = F2(
	function (a, b) {
		return {$: 'U32', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$unsignedInt32 = $elm$bytes$Bytes$Encode$U32;
var $jxxcarlson$elm_tar$Tar$nullBlock = $elm$bytes$Bytes$Encode$encode(
	$elm$bytes$Bytes$Encode$sequence(
		A2(
			$elm$core$List$repeat,
			(512 / 4) | 0,
			A2($elm$bytes$Bytes$Encode$unsignedInt32, $elm$bytes$Bytes$BE, 0))));
var $elm$bytes$Bytes$Decode$bytes = function (n) {
	return $elm$bytes$Bytes$Decode$Decoder(
		_Bytes_read_bytes(n));
};
var $jxxcarlson$elm_tar$Tar$takeBytes = F2(
	function (k, bytes) {
		var _v0 = A2(
			$elm$bytes$Bytes$Decode$decode,
			$elm$bytes$Bytes$Decode$bytes(k),
			bytes);
		if (_v0.$ === 'Just') {
			var v = _v0.a;
			return v;
		} else {
			return bytes;
		}
	});
var $jxxcarlson$elm_tar$Tar$encodePaddedBytes = function (bytes) {
	var paddingWidth = function (x) {
		return 512 - x;
	}(
		A2(
			$elm$core$Basics$modBy,
			512,
			$elm$bytes$Bytes$width(bytes)));
	return $elm$bytes$Bytes$Encode$sequence(
		_List_fromArray(
			[
				$elm$bytes$Bytes$Encode$bytes(bytes),
				$elm$bytes$Bytes$Encode$bytes(
				A2($jxxcarlson$elm_tar$Tar$takeBytes, paddingWidth, $jxxcarlson$elm_tar$Tar$nullBlock))
			]));
};
var $jxxcarlson$elm_tar$Tar$encodeBinaryFile = F2(
	function (metaData, bytes) {
		var width = $elm$bytes$Bytes$width(bytes);
		if (!width) {
			return $jxxcarlson$elm_tar$Tar$encodeMetadata(
				_Utils_update(
					metaData,
					{fileSize: width}));
		} else {
			return $elm$bytes$Bytes$Encode$sequence(
				_List_fromArray(
					[
						$jxxcarlson$elm_tar$Tar$encodeMetadata(
						_Utils_update(
							metaData,
							{fileSize: width})),
						$jxxcarlson$elm_tar$Tar$encodePaddedBytes(bytes)
					]));
		}
	});
var $jxxcarlson$elm_tar$Tar$encodeTextFile = F2(
	function (metaData, contents) {
		return A2(
			$jxxcarlson$elm_tar$Tar$encodeBinaryFile,
			metaData,
			$elm$bytes$Bytes$Encode$encode(
				$elm$bytes$Bytes$Encode$string(contents)));
	});
var $jxxcarlson$elm_tar$Tar$encodeFile = F2(
	function (metaData, data) {
		if (data.$ === 'StringData') {
			var contents = data.a;
			return A2($jxxcarlson$elm_tar$Tar$encodeTextFile, metaData, contents);
		} else {
			var bytes = data.a;
			return A2($jxxcarlson$elm_tar$Tar$encodeBinaryFile, metaData, bytes);
		}
	});
var $jxxcarlson$elm_tar$Tar$endOfFileMarker = $elm$bytes$Bytes$Encode$string(
	A2($elm$core$String$repeat, 1024, '\u0000'));
var $jxxcarlson$elm_tar$Tar$encodeFiles = function (fileList) {
	var folder = F2(
		function (_v0, accum) {
			var metadata = _v0.a;
			var string = _v0.b;
			return A2(
				$elm$core$List$cons,
				A2($jxxcarlson$elm_tar$Tar$encodeFile, metadata, string),
				accum);
		});
	return $elm$bytes$Bytes$Encode$sequence(
		A3(
			$elm$core$List$foldr,
			folder,
			_List_fromArray(
				[$jxxcarlson$elm_tar$Tar$endOfFileMarker]),
			fileList));
};
var $jxxcarlson$elm_tar$Tar$createArchive = function (dataList) {
	return $elm$bytes$Bytes$Encode$encode(
		$jxxcarlson$elm_tar$Tar$encodeFiles(dataList));
};
var $jxxcarlson$elm_tar$Tar$NormalFile = {$: 'NormalFile'};
var $jxxcarlson$elm_tar$Tar$defaultMode = {
	group: {execute: false, read: true, write: true},
	other: {execute: false, read: true, write: false},
	owner: {execute: true, read: true, write: true},
	setGroupID: false,
	setUserID: false
};
var $jxxcarlson$elm_tar$Tar$defaultMetadata = {fileNamePrefix: 'abc', fileSize: 20, filename: 'test.txt', groupID: 123, groupName: 'staff', lastModificationTime: 1542665285, linkIndicator: $jxxcarlson$elm_tar$Tar$NormalFile, linkedFileName: 'bar.txt', mode: $jxxcarlson$elm_tar$Tar$defaultMode, ownerID: 501, userName: 'anonymous'};
var $author$project$Main$metadataWithPngName = function (name) {
	return _Utils_update(
		$jxxcarlson$elm_tar$Tar$defaultMetadata,
		{fileNamePrefix: 'images', filename: name});
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $danfishgold$base64_bytes$Encode$isValidChar = function (c) {
	if ($elm$core$Char$isAlphaNum(c)) {
		return true;
	} else {
		switch (c.valueOf()) {
			case '+':
				return true;
			case '/':
				return true;
			default:
				return false;
		}
	}
};
var $danfishgold$base64_bytes$Encode$unsafeConvertChar = function (_char) {
	var key = $elm$core$Char$toCode(_char);
	if ((key >= 65) && (key <= 90)) {
		return key - 65;
	} else {
		if ((key >= 97) && (key <= 122)) {
			return (key - 97) + 26;
		} else {
			if ((key >= 48) && (key <= 57)) {
				return ((key - 48) + 26) + 26;
			} else {
				switch (_char.valueOf()) {
					case '+':
						return 62;
					case '/':
						return 63;
					default:
						return -1;
				}
			}
		}
	}
};
var $elm$bytes$Bytes$Encode$U16 = F2(
	function (a, b) {
		return {$: 'U16', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$unsignedInt16 = $elm$bytes$Bytes$Encode$U16;
var $danfishgold$base64_bytes$Encode$encodeCharacters = F4(
	function (a, b, c, d) {
		if ($danfishgold$base64_bytes$Encode$isValidChar(a) && $danfishgold$base64_bytes$Encode$isValidChar(b)) {
			var n2 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(b);
			var n1 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(a);
			if ('=' === d.valueOf()) {
				if ('=' === c.valueOf()) {
					var n = (n1 << 18) | (n2 << 12);
					var b1 = n >> 16;
					return $elm$core$Maybe$Just(
						$elm$bytes$Bytes$Encode$unsignedInt8(b1));
				} else {
					if ($danfishgold$base64_bytes$Encode$isValidChar(c)) {
						var n3 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(c);
						var n = ((n1 << 18) | (n2 << 12)) | (n3 << 6);
						var combined = n >> 8;
						return $elm$core$Maybe$Just(
							A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$BE, combined));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}
			} else {
				if ($danfishgold$base64_bytes$Encode$isValidChar(c) && $danfishgold$base64_bytes$Encode$isValidChar(d)) {
					var n4 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(d);
					var n3 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(c);
					var n = ((n1 << 18) | (n2 << 12)) | ((n3 << 6) | n4);
					var combined = n >> 8;
					var b3 = n;
					return $elm$core$Maybe$Just(
						$elm$bytes$Bytes$Encode$sequence(
							_List_fromArray(
								[
									A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$BE, combined),
									$elm$bytes$Bytes$Encode$unsignedInt8(b3)
								])));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $danfishgold$base64_bytes$Encode$encodeChunks = F2(
	function (input, accum) {
		encodeChunks:
		while (true) {
			var _v0 = $elm$core$String$toList(
				A2($elm$core$String$left, 4, input));
			_v0$4:
			while (true) {
				if (!_v0.b) {
					return $elm$core$Maybe$Just(accum);
				} else {
					if (_v0.b.b) {
						if (_v0.b.b.b) {
							if (_v0.b.b.b.b) {
								if (!_v0.b.b.b.b.b) {
									var a = _v0.a;
									var _v1 = _v0.b;
									var b = _v1.a;
									var _v2 = _v1.b;
									var c = _v2.a;
									var _v3 = _v2.b;
									var d = _v3.a;
									var _v4 = A4($danfishgold$base64_bytes$Encode$encodeCharacters, a, b, c, d);
									if (_v4.$ === 'Just') {
										var enc = _v4.a;
										var $temp$input = A2($elm$core$String$dropLeft, 4, input),
											$temp$accum = A2($elm$core$List$cons, enc, accum);
										input = $temp$input;
										accum = $temp$accum;
										continue encodeChunks;
									} else {
										return $elm$core$Maybe$Nothing;
									}
								} else {
									break _v0$4;
								}
							} else {
								var a = _v0.a;
								var _v5 = _v0.b;
								var b = _v5.a;
								var _v6 = _v5.b;
								var c = _v6.a;
								var _v7 = A4(
									$danfishgold$base64_bytes$Encode$encodeCharacters,
									a,
									b,
									c,
									_Utils_chr('='));
								if (_v7.$ === 'Nothing') {
									return $elm$core$Maybe$Nothing;
								} else {
									var enc = _v7.a;
									return $elm$core$Maybe$Just(
										A2($elm$core$List$cons, enc, accum));
								}
							}
						} else {
							var a = _v0.a;
							var _v8 = _v0.b;
							var b = _v8.a;
							var _v9 = A4(
								$danfishgold$base64_bytes$Encode$encodeCharacters,
								a,
								b,
								_Utils_chr('='),
								_Utils_chr('='));
							if (_v9.$ === 'Nothing') {
								return $elm$core$Maybe$Nothing;
							} else {
								var enc = _v9.a;
								return $elm$core$Maybe$Just(
									A2($elm$core$List$cons, enc, accum));
							}
						}
					} else {
						break _v0$4;
					}
				}
			}
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $danfishgold$base64_bytes$Encode$encoder = function (string) {
	return A2(
		$elm$core$Maybe$map,
		A2($elm$core$Basics$composeR, $elm$core$List$reverse, $elm$bytes$Bytes$Encode$sequence),
		A2($danfishgold$base64_bytes$Encode$encodeChunks, string, _List_Nil));
};
var $danfishgold$base64_bytes$Encode$toBytes = function (string) {
	return A2(
		$elm$core$Maybe$map,
		$elm$bytes$Bytes$Encode$encode,
		$danfishgold$base64_bytes$Encode$encoder(string));
};
var $danfishgold$base64_bytes$Base64$toBytes = $danfishgold$base64_bytes$Encode$toBytes;
var $author$project$Main$base64sToTar = function (list) {
	return $jxxcarlson$elm_tar$Tar$createArchive(
		A3(
			$elm$core$List$foldr,
			F2(
				function (_v0, acc) {
					var name = _v0.name;
					var url = _v0.url;
					var _v1 = $danfishgold$base64_bytes$Base64$toBytes(
						A2($elm$core$String$dropLeft, 22, url));
					if (_v1.$ === 'Nothing') {
						return acc;
					} else {
						var _byte = _v1.a;
						var entry = _Utils_Tuple2(
							$author$project$Main$metadataWithPngName(name + '.png'),
							$jxxcarlson$elm_tar$Tar$BinaryData(_byte));
						return A2($elm$core$List$cons, entry, acc);
					}
				}),
			_List_Nil,
			list));
};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$file$File$Download$bytes = F3(
	function (name, mime, content) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$never,
			A3(
				_File_download,
				name,
				mime,
				_File_makeBytesSafeForInternetExplorer(content)));
	});
var $author$project$Main$downloadBase64Tar = function (list) {
	return A3(
		$elm$file$File$Download$bytes,
		'images.tar',
		'application/x-tar',
		$author$project$Main$base64sToTar(list));
};
var $author$project$BBoxies$empty = {entities: $elm$core$Dict$empty, nextId: 0};
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $author$project$BBoxies$get = F2(
	function (i, _v0) {
		var entities = _v0.entities;
		return A2($elm$core$Dict$get, i, entities);
	});
var $author$project$BBoxies$insert = F3(
	function (i, entity, bboxies) {
		return _Utils_update(
			bboxies,
			{
				entities: A3($elm$core$Dict$insert, i, entity, bboxies.entities)
			});
	});
var $elm$core$Debug$log = _Debug_log;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $author$project$BBox$normalize = function (_v0) {
	var x = _v0.x;
	var y = _v0.y;
	var width = _v0.width;
	var height = _v0.height;
	var hold = _v0.hold;
	var list = $elm$core$List$sort(
		_List_fromArray(
			[
				_Utils_Tuple2(x, y),
				_Utils_Tuple2(x + width, y),
				_Utils_Tuple2(x, y + height),
				_Utils_Tuple2(x + width, y + height)
			]));
	if ((((list.b && list.b.b) && list.b.b.b) && list.b.b.b.b) && (!list.b.b.b.b.b)) {
		var a = list.a;
		var _v2 = list.b;
		var b = _v2.a;
		var _v3 = _v2.b;
		var c = _v3.a;
		var _v4 = _v3.b;
		var d = _v4.a;
		var _v5 = d;
		var x1 = _v5.a;
		var y1 = _v5.b;
		var _v6 = a;
		var x0 = _v6.a;
		var y0 = _v6.b;
		var _v7 = _Utils_Tuple2(x1 - x0, y1 - y0);
		var newW = _v7.a;
		var newH = _v7.b;
		return A5($author$project$BBox$BBox, x0, y0, newW, newH, hold);
	} else {
		return A5($author$project$BBox$BBox, x, y, width, height, hold);
	}
};
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$BBoxies$remove = F2(
	function (i, bboxies) {
		return _Utils_update(
			bboxies,
			{
				entities: A2($elm$core$Dict$remove, i, bboxies.entities)
			});
	});
var $elm$file$File$toUrl = _File_toUrl;
var $author$project$BBox$transform = F3(
	function (_v0, pos, bbox) {
		var x = _v0.x;
		var y = _v0.y;
		var dx = _v0.dx;
		var dy = _v0.dy;
		var _v1 = _Utils_Tuple2(bbox.x, bbox.y);
		var sx = _v1.a;
		var sy = _v1.b;
		var _v2 = _Utils_Tuple2(sx + bbox.width, sy + bbox.height);
		var ex = _v2.a;
		var ey = _v2.b;
		var _new = function () {
			switch (pos.$) {
				case 'Center':
					return {ex: ex + dx, ey: ey + dy, sx: sx + dx, sy: sy + dy};
				case 'Above':
					return {ex: ex, ey: ey, sx: sx, sy: y};
				case 'Right':
					return {ex: x, ey: ey, sx: sx, sy: sy};
				case 'Bottom':
					return {ex: ex, ey: y, sx: sx, sy: sy};
				case 'Left':
					return {ex: ex, ey: ey, sx: x, sy: sy};
				case 'AboveLeft':
					return {ex: ex, ey: ey, sx: x, sy: y};
				case 'AboveRight':
					return {ex: x, ey: ey, sx: sx, sy: y};
				case 'BottomRight':
					return {ex: x, ey: y, sx: sx, sy: sy};
				case 'BottomLeft':
					return {ex: ex, ey: y, sx: x, sy: sy};
				default:
					return {ex: ex, ey: ey, sx: sx, sy: sy};
			}
		}();
		return _Utils_update(
			bbox,
			{height: _new.ey - _new.sy, width: _new.ex - _new.sx, x: _new.sx, y: _new.sy});
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Hold':
				var id = msg.a;
				var pos = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							hold: A2($author$project$Main$HoldBBox, id, pos),
							select: $author$project$Main$SelectBBox(id)
						}),
					$elm$core$Platform$Cmd$none);
			case 'MouseMove':
				var x = msg.a;
				var y = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							mousePosition: {dx: x - model.mousePosition.x, dy: y - model.mousePosition.y, x: x, y: y}
						}),
					$elm$core$Platform$Cmd$none);
			case 'Holding':
				var id = msg.a;
				var pos = msg.b;
				var _v1 = A2($author$project$BBoxies$get, id, model.boxies);
				if (_v1.$ === 'Nothing') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var box = _v1.a;
					var newBox = A3($author$project$BBox$transform, model.mousePosition, pos, box);
					var _v2 = A2($elm$core$Debug$log, 'box', newBox);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								boxies: A3($author$project$BBoxies$insert, id, newBox, model.boxies)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'MouseUp':
				var id = msg.a;
				var _v3 = A2($author$project$BBoxies$get, id, model.boxies);
				if (_v3.$ === 'Nothing') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var box = _v3.a;
					var newBox = $author$project$BBox$normalize(box);
					var newBoxies = A3($author$project$BBoxies$insert, id, newBox, model.boxies);
					var newModel = _Utils_update(
						model,
						{boxies: newBoxies, hold: $author$project$Main$HoldNothing});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$clipImageCommand(newModel));
				}
			case 'KeyDowned':
				var key = msg.a;
				if (key.$ === 'KeyDelete') {
					var id = key.a;
					var newBoxies = A2($author$project$BBoxies$remove, id, model.boxies);
					var newModel = _Utils_update(
						model,
						{boxies: newBoxies});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$clipImageCommand(newModel));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'AddBox':
				var _v5 = A2($author$project$Main$svgSizeWith, model.imgWidth, model.imgHeight);
				var svgWidth = _v5.a;
				var svgHeight = _v5.b;
				var _v6 = _Utils_Tuple2(100, 100);
				var w = _v6.a;
				var h = _v6.b;
				var newBox = A5($author$project$BBox$BBox, (svgWidth - w) / 2, (svgHeight - h) / 2, w, h, $author$project$BBox$None);
				var newBoxies = A2($author$project$BBoxies$add, newBox, model.boxies);
				var newModel = _Utils_update(
					model,
					{boxies: newBoxies});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$clipImageCommand(newModel));
			case 'CopyBox':
				var _v7 = model.select;
				if (_v7.$ === 'SelectNothing') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var id = _v7.a;
					var _v8 = A2($author$project$BBoxies$get, id, model.boxies);
					if (_v8.$ === 'Nothing') {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						var box = _v8.a;
						var newBoxies = A2($author$project$BBoxies$add, box, model.boxies);
						var newModel = _Utils_update(
							model,
							{boxies: newBoxies});
						return _Utils_Tuple2(
							newModel,
							$author$project$Main$clipImageCommand(newModel));
					}
				}
			case 'ImageRequested':
				return _Utils_Tuple2(
					model,
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['image/png', 'image/jpeg', 'image/gif', 'image/bmp']),
						$author$project$Main$ImageSelected));
			case 'ImageSelected':
				var file = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$core$Task$perform,
						$author$project$Main$ImageUrlReceived,
						$elm$file$File$toUrl(file)));
			case 'ImageUrlReceived':
				var url = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{boxies: $author$project$BBoxies$empty, imgSrc: url}),
					$elm$core$Platform$Cmd$none);
			case 'ImageLoaded':
				return _Utils_Tuple2(
					model,
					$author$project$Main$askImageSize(model.imgSrc));
			case 'ImageSizeReceived':
				var w = msg.a;
				var h = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{imgHeight: h, imgWidth: w}),
					$author$project$Main$clipImageCommand(model));
			case 'ClippedImagesReceived':
				var list = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							clippedImages: A2(
								$elm$core$List$indexedMap,
								F2(
									function (i, x) {
										return {
											name: $elm$core$String$fromInt(i),
											url: x
										};
									}),
								list)
						}),
					$elm$core$Platform$Cmd$none);
			case 'DownloadTar':
				return _Utils_Tuple2(
					model,
					$author$project$Main$downloadBase64Tar(model.clippedImages));
			case 'ClippedImageNameChanged':
				var i = msg.a;
				var name = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							clippedImages: A2(
								$elm$core$List$indexedMap,
								F2(
									function (j, e) {
										return _Utils_eq(i, j) ? _Utils_update(
											e,
											{name: name}) : e;
									}),
								model.clippedImages)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{imgSrc: ''}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$Main$ImageLoaded = {$: 'ImageLoaded'};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$image = $elm$svg$Svg$trustedNode('image');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$svg$Svg$Events$on = $elm$html$Html$Events$on;
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$BBoxies$toMappedList = F2(
	function (f, _v0) {
		var entities = _v0.entities;
		return $elm$core$Dict$values(
			A2($elm$core$Dict$map, f, entities));
	});
var $author$project$BBox$AboveLeft = {$: 'AboveLeft'};
var $author$project$BBox$AboveRight = {$: 'AboveRight'};
var $author$project$Main$BBoxCorner = F4(
	function (id, point, len, pos) {
		return {id: id, len: len, point: point, pos: pos};
	});
var $author$project$BBox$BottomLeft = {$: 'BottomLeft'};
var $author$project$BBox$BottomRight = {$: 'BottomRight'};
var $author$project$Main$edgeW = 5;
var $author$project$Main$cornersOf = F2(
	function (id, _v0) {
		var width = _v0.width;
		var height = _v0.height;
		var p3 = {x: -$author$project$Main$edgeW, y: height};
		var p2 = {x: width, y: height};
		var p1 = {x: width, y: -$author$project$Main$edgeW};
		var p0 = {x: -$author$project$Main$edgeW, y: -$author$project$Main$edgeW};
		return _List_fromArray(
			[
				A4($author$project$Main$BBoxCorner, id, p0, $author$project$Main$edgeW, $author$project$BBox$AboveLeft),
				A4($author$project$Main$BBoxCorner, id, p1, $author$project$Main$edgeW, $author$project$BBox$AboveRight),
				A4($author$project$Main$BBoxCorner, id, p2, $author$project$Main$edgeW, $author$project$BBox$BottomRight),
				A4($author$project$Main$BBoxCorner, id, p3, $author$project$Main$edgeW, $author$project$BBox$BottomLeft)
			]);
	});
var $author$project$BBox$Above = {$: 'Above'};
var $author$project$Main$BBoxEdge = F4(
	function (id, from, to, pos) {
		return {from: from, id: id, pos: pos, to: to};
	});
var $author$project$BBox$Bottom = {$: 'Bottom'};
var $author$project$BBox$Left = {$: 'Left'};
var $author$project$BBox$Right = {$: 'Right'};
var $author$project$Main$edgesOf = F2(
	function (id, _v0) {
		var width = _v0.width;
		var height = _v0.height;
		var _v1 = _Utils_Tuple2(
			{x: (-$author$project$Main$edgeW) / 2, y: height},
			{x: (-$author$project$Main$edgeW) / 2, y: 0});
		var p30 = _v1.a;
		var p31 = _v1.b;
		var _v2 = _Utils_Tuple2(
			{x: width, y: height + ($author$project$Main$edgeW / 2)},
			{x: 0, y: height + ($author$project$Main$edgeW / 2)});
		var p20 = _v2.a;
		var p21 = _v2.b;
		var _v3 = _Utils_Tuple2(
			{x: width + ($author$project$Main$edgeW / 2), y: 0},
			{x: width + ($author$project$Main$edgeW / 2), y: height});
		var p10 = _v3.a;
		var p11 = _v3.b;
		var _v4 = _Utils_Tuple2(
			{x: 0, y: (-$author$project$Main$edgeW) / 2},
			{x: width, y: (-$author$project$Main$edgeW) / 2});
		var p00 = _v4.a;
		var p01 = _v4.b;
		return _List_fromArray(
			[
				A4($author$project$Main$BBoxEdge, id, p00, p01, $author$project$BBox$Above),
				A4($author$project$Main$BBoxEdge, id, p10, p11, $author$project$BBox$Right),
				A4($author$project$Main$BBoxEdge, id, p20, p21, $author$project$BBox$Bottom),
				A4($author$project$Main$BBoxEdge, id, p30, p31, $author$project$BBox$Left)
			]);
	});
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $author$project$Main$translate = F2(
	function (x, y) {
		return 'translate(' + ($elm$core$String$fromFloat(x) + (',' + ($elm$core$String$fromFloat(y) + ')')));
	});
var $author$project$BBox$Center = {$: 'Center'};
var $author$project$Main$Hold = F2(
	function (a, b) {
		return {$: 'Hold', a: a, b: b};
	});
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$svg$Svg$Attributes$opacity = _VirtualDom_attribute('opacity');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var $author$project$Main$styleCursor = function (pos) {
	switch (pos.$) {
		case 'Above':
			return $elm$svg$Svg$Attributes$style('cursor: ns-resize;');
		case 'Left':
			return $elm$svg$Svg$Attributes$style('cursor: ew-resize;');
		case 'Bottom':
			return $elm$svg$Svg$Attributes$style('cursor: ns-resize;');
		case 'Right':
			return $elm$svg$Svg$Attributes$style('cursor: ew-resize;');
		case 'AboveLeft':
			return $elm$svg$Svg$Attributes$style('cursor: nwse-resize;');
		case 'AboveRight':
			return $elm$svg$Svg$Attributes$style('cursor: nesw-resize;');
		case 'BottomRight':
			return $elm$svg$Svg$Attributes$style('cursor: nwse-resize;');
		case 'BottomLeft':
			return $elm$svg$Svg$Attributes$style('cursor: nesw-resize;');
		case 'Center':
			return $elm$svg$Svg$Attributes$style('cursor: move;');
		default:
			return $elm$svg$Svg$Attributes$style('cursor: default;');
	}
};
var $author$project$Main$viewBBoxArea = F3(
	function (id, width, height) {
		return A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[
								'M' + ($elm$core$String$fromFloat(0) + (',' + $elm$core$String$fromFloat(0))),
								'L' + ($elm$core$String$fromFloat(width) + (',' + $elm$core$String$fromFloat(0))),
								'L' + ($elm$core$String$fromFloat(width) + (',' + $elm$core$String$fromFloat(height))),
								'L' + ($elm$core$String$fromFloat(0) + (',' + $elm$core$String$fromFloat(height))),
								'Z'
							]))),
					$elm$svg$Svg$Attributes$fill('#000'),
					$elm$svg$Svg$Attributes$opacity('0.2'),
					$elm$svg$Svg$Events$onMouseDown(
					A2($author$project$Main$Hold, id, $author$project$BBox$Center)),
					$author$project$Main$styleCursor($author$project$BBox$Center)
				]),
			_List_Nil);
	});
var $author$project$Main$bboxColor = F2(
	function (state, id) {
		if (state.$ === 'SelectNothing') {
			return '#333';
		} else {
			var sid = state.a;
			return _Utils_eq(sid, id) ? '#f80' : '#333';
		}
	});
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$Main$viewBBoxCorner = F2(
	function (model, _v0) {
		var point = _v0.point;
		var id = _v0.id;
		var len = _v0.len;
		var pos = _v0.pos;
		return A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromFloat(point.x)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromFloat(point.y)),
					$elm$svg$Svg$Attributes$fill(
					A2($author$project$Main$bboxColor, model.select, id)),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromFloat(len)),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromFloat(len)),
					$elm$svg$Svg$Events$onMouseDown(
					A2($author$project$Main$Hold, id, pos)),
					$author$project$Main$styleCursor(pos)
				]),
			_List_Nil);
	});
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $author$project$Main$viewBBoxEdge = F2(
	function (model, _v0) {
		var from = _v0.from;
		var to = _v0.to;
		var id = _v0.id;
		var pos = _v0.pos;
		return A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[
								'M' + ($elm$core$String$fromFloat(from.x) + (',' + $elm$core$String$fromFloat(from.y))),
								'L' + ($elm$core$String$fromFloat(to.x) + (',' + $elm$core$String$fromFloat(to.y)))
							]))),
					$elm$svg$Svg$Attributes$stroke(
					A2($author$project$Main$bboxColor, model.select, id)),
					$elm$svg$Svg$Attributes$strokeWidth(
					$elm$core$String$fromFloat($author$project$Main$edgeW)),
					$elm$svg$Svg$Events$onMouseDown(
					A2($author$project$Main$Hold, id, pos)),
					$author$project$Main$styleCursor(pos)
				]),
			_List_Nil);
	});
var $elm$svg$Svg$Attributes$dominantBaseline = _VirtualDom_attribute('dominant-baseline');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $author$project$Main$viewBBoxLabel = function (id) {
	return A2(
		$elm$svg$Svg$g,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$transform(
				A2($author$project$Main$translate, -10, -10))
			]),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$text_,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$style('user-select: none'),
						$elm$svg$Svg$Attributes$dominantBaseline('central'),
						$elm$svg$Svg$Attributes$textAnchor('middle')
					]),
				_List_fromArray(
					[
						$elm$svg$Svg$text(
						$elm$core$String$fromInt(id))
					]))
			]));
};
var $author$project$Main$viewBBox = F3(
	function (model, id, box) {
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$transform(
					A2($author$project$Main$translate, box.x, box.y))
				]),
			_List_fromArray(
				[
					$author$project$Main$viewBBoxLabel(id),
					A3($author$project$Main$viewBBoxArea, id, box.width, box.height),
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('bbox-edges')
						]),
					A2(
						$elm$core$List$map,
						$author$project$Main$viewBBoxEdge(model),
						A2($author$project$Main$edgesOf, id, box))),
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('bbox-corners')
						]),
					A2(
						$elm$core$List$map,
						$author$project$Main$viewBBoxCorner(model),
						A2($author$project$Main$cornersOf, id, box)))
				]));
	});
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $author$project$Main$viewMain = function (model) {
	var _v0 = A2($author$project$Main$svgSizeWith, model.imgWidth, model.imgHeight);
	var svgWidth = _v0.a;
	var svgHeight = _v0.b;
	return A2(
		$elm$svg$Svg$svg,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$class('main'),
				A2($elm$html$Html$Attributes$style, 'border', '1px solid #000'),
				A2(
				$elm$html$Html$Attributes$style,
				'width',
				$elm$core$String$fromFloat(svgWidth)),
				A2(
				$elm$html$Html$Attributes$style,
				'height',
				$elm$core$String$fromFloat(svgHeight)),
				A2($elm$html$Html$Attributes$attribute, 'xmlns', 'http://www.w3.org/2000/svg'),
				A2($elm$html$Html$Attributes$attribute, 'xmlns:xlink', 'http://www.w3.org/1999/xlink')
			]),
		$elm$core$List$concat(
			_List_fromArray(
				[
					_List_fromArray(
					[
						A2(
						$elm$svg$Svg$image,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id('image_input'),
								$elm$svg$Svg$Attributes$xlinkHref(model.imgSrc),
								$elm$svg$Svg$Attributes$x('0'),
								$elm$svg$Svg$Attributes$y('0'),
								$elm$svg$Svg$Attributes$width(
								$elm$core$String$fromFloat(svgWidth)),
								$elm$svg$Svg$Attributes$height(
								$elm$core$String$fromFloat(svgHeight)),
								A2(
								$elm$svg$Svg$Events$on,
								'load',
								$elm$json$Json$Decode$succeed($author$project$Main$ImageLoaded))
							]),
						_List_Nil)
					]),
					A2(
					$author$project$BBoxies$toMappedList,
					$author$project$Main$viewBBox(model),
					model.boxies)
				])));
};
var $author$project$Main$AddBox = {$: 'AddBox'};
var $author$project$Main$CopyBox = {$: 'CopyBox'};
var $author$project$Main$DownloadTar = {$: 'DownloadTar'};
var $author$project$Main$ImageRequested = {$: 'ImageRequested'};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Main$ClippedImageNameChanged = F2(
	function (a, b) {
		return {$: 'ClippedImageNameChanged', a: a, b: b};
	});
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$viewClippedImage = F2(
	function (i, _v0) {
		var name = _v0.name;
		var url = _v0.url;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('clipped-image')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(url)
						]),
					_List_Nil),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Events$onInput(
									$author$project$Main$ClippedImageNameChanged(i)),
									$elm$html$Html$Attributes$value(name),
									$elm$html$Html$Attributes$placeholder(
									$elm$core$String$fromInt(i))
								]),
							_List_Nil),
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('.png')
								]))
						]))
				]));
	});
var $author$project$Main$viewSide = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('side')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$ImageRequested)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Load image')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$AddBox)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Add')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$CopyBox)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Copy')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('clipped-images')
					]),
				A2($elm$core$List$indexedMap, $author$project$Main$viewClippedImage, model.clippedImages)),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$DownloadTar)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Download')
					]))
			]));
};
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('wrapper')
			]),
		_List_fromArray(
			[
				$author$project$Main$viewMain(model),
				$author$project$Main$viewSide(model)
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));