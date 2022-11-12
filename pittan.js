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
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

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
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
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
var $author$project$Pittan$Model = function (conf) {
	return function (candidates) {
		return function (board) {
			return function (startedAt) {
				return function (nowAt) {
					return function (moving) {
						return function (cursor) {
							return function (newWordsAt) {
								return function (foundWords) {
									return function (completed) {
										return function (gameId) {
											return {board: board, candidates: candidates, completed: completed, conf: conf, cursor: cursor, foundWords: foundWords, gameId: gameId, moving: moving, newWordsAt: newWordsAt, nowAt: nowAt, startedAt: startedAt};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Pittan$init = function (_v0) {
	return _Utils_Tuple2(
		$author$project$Pittan$Model(_List_Nil)(_List_Nil)(_List_Nil)(
			{x: 0, y: 0})(
			{x: 0, y: 0})($elm$core$Maybe$Nothing)(0)(_List_Nil)(_List_Nil)(false)(-1),
		$elm$core$Platform$Cmd$none);
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Pittan$subscriptions = function (model) {
	return $elm$core$Platform$Sub$none;
};
var $author$project$Pittan$Cell = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $author$project$Pittan$Piece = F5(
	function (id, x, y, c, used) {
		return {c: c, id: id, used: used, x: x, y: y};
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $author$project$Pittan$boards = _List_fromArray(
	[
		_List_fromArray(
		[
			{x: 3, y: 2},
			{x: 3, y: 3},
			{x: 3, y: 4},
			{x: 3, y: 5},
			{x: 4, y: 1},
			{x: 4, y: 2},
			{x: 4, y: 3},
			{x: 4, y: 4},
			{x: 4, y: 5},
			{x: 4, y: 6},
			{x: 5, y: 1},
			{x: 5, y: 2},
			{x: 5, y: 3},
			{x: 5, y: 6},
			{x: 6, y: 2},
			{x: 6, y: 3},
			{x: 6, y: 6},
			{x: 6, y: 7},
			{x: 7, y: 3},
			{x: 7, y: 6},
			{x: 7, y: 7},
			{x: 7, y: 8},
			{x: 8, y: 2},
			{x: 8, y: 3},
			{x: 8, y: 6},
			{x: 8, y: 7},
			{x: 9, y: 1},
			{x: 9, y: 2},
			{x: 9, y: 3},
			{x: 9, y: 6},
			{x: 10, y: 1},
			{x: 10, y: 2},
			{x: 10, y: 3},
			{x: 10, y: 4},
			{x: 10, y: 5},
			{x: 10, y: 6},
			{x: 11, y: 2},
			{x: 11, y: 3},
			{x: 11, y: 4},
			{x: 11, y: 5}
		]),
		_List_fromArray(
		[
			{x: 3, y: 1},
			{x: 3, y: 2},
			{x: 4, y: 2},
			{x: 4, y: 3},
			{x: 4, y: 4},
			{x: 5, y: 3},
			{x: 5, y: 4},
			{x: 5, y: 5},
			{x: 6, y: 5},
			{x: 6, y: 6},
			{x: 7, y: 4},
			{x: 7, y: 5},
			{x: 9, y: 1},
			{x: 9, y: 2},
			{x: 10, y: 2},
			{x: 10, y: 3},
			{x: 11, y: 3},
			{x: 11, y: 4},
			{x: 11, y: 5}
		]),
		_List_fromArray(
		[
			{x: 3, y: 6},
			{x: 3, y: 7},
			{x: 4, y: 4},
			{x: 4, y: 5},
			{x: 4, y: 6},
			{x: 5, y: 2},
			{x: 5, y: 3},
			{x: 5, y: 4},
			{x: 5, y: 5},
			{x: 6, y: 1},
			{x: 6, y: 2},
			{x: 6, y: 5},
			{x: 7, y: 2},
			{x: 7, y: 3},
			{x: 7, y: 4},
			{x: 7, y: 5},
			{x: 8, y: 4},
			{x: 8, y: 5},
			{x: 8, y: 6},
			{x: 9, y: 6},
			{x: 9, y: 7}
		])
	]);
var $elm$json$Json$Encode$bool = _Json_wrap;
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
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$Basics$compare = _Utils_compare;
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
var $author$project$Dictionary$joukyu = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2('あ', 'あ'),
			_Utils_Tuple2('ああ', 'ああ'),
			_Utils_Tuple2('あい', '愛'),
			_Utils_Tuple2('あいさつ', '挨拶'),
			_Utils_Tuple2('あいす', 'アイス'),
			_Utils_Tuple2('あいすくりーむ', 'アイスクリーム'),
			_Utils_Tuple2('あいすこーひー', 'アイスコーヒー'),
			_Utils_Tuple2('あいすすけーと', 'アイススケート'),
			_Utils_Tuple2('あいだ', '間'),
			_Utils_Tuple2('あいてぃー', 'ＩＴ'),
			_Utils_Tuple2('あいろん', 'アイロン'),
			_Utils_Tuple2('あう', '会う'),
			_Utils_Tuple2('あう', '合う'),
			_Utils_Tuple2('あお', '青'),
			_Utils_Tuple2('あおい', '青い'),
			_Utils_Tuple2('あおいろ', '青色'),
			_Utils_Tuple2('あおしんごう', '青信号'),
			_Utils_Tuple2('あか', '赤'),
			_Utils_Tuple2('あかい', '赤い'),
			_Utils_Tuple2('あかいろ', '赤色'),
			_Utils_Tuple2('あかしんごう', '赤信号'),
			_Utils_Tuple2('あかちゃん', '赤ちゃん'),
			_Utils_Tuple2('あがる', '上がる'),
			_Utils_Tuple2('あかるい', '明るい'),
			_Utils_Tuple2('あき', '秋'),
			_Utils_Tuple2('あく', '開く'),
			_Utils_Tuple2('あける', '開ける'),
			_Utils_Tuple2('あげる', '上げる'),
			_Utils_Tuple2('あさ', '朝'),
			_Utils_Tuple2('あさごはん', '朝御飯'),
			_Utils_Tuple2('あさって', 'あさって'),
			_Utils_Tuple2('あし', '足'),
			_Utils_Tuple2('あじ', '味'),
			_Utils_Tuple2('あじあ', 'アジア'),
			_Utils_Tuple2('あした', '明日'),
			_Utils_Tuple2('あそこ', 'あそこ'),
			_Utils_Tuple2('あそび', '遊び'),
			_Utils_Tuple2('あそぶ', '遊ぶ'),
			_Utils_Tuple2('あたたかい', '暖かい'),
			_Utils_Tuple2('あたたかい', '温かい'),
			_Utils_Tuple2('あたま', '頭'),
			_Utils_Tuple2('あたらしい', '新しい'),
			_Utils_Tuple2('あたり', '辺り'),
			_Utils_Tuple2('あちら', 'あちら'),
			_Utils_Tuple2('あっ', 'あっ'),
			_Utils_Tuple2('あつい', '暑い'),
			_Utils_Tuple2('あつい', '熱い'),
			_Utils_Tuple2('あつい', '厚い'),
			_Utils_Tuple2('あつまる', '集まる'),
			_Utils_Tuple2('あと', '後'),
			_Utils_Tuple2('あなた', 'あなた'),
			_Utils_Tuple2('あに', '兄'),
			_Utils_Tuple2('あにめ', 'アニメ'),
			_Utils_Tuple2('あにめーしょん', 'アニメーション'),
			_Utils_Tuple2('あね', '姉'),
			_Utils_Tuple2('あの', 'あの'),
			_Utils_Tuple2('あぱーと', 'アパート'),
			_Utils_Tuple2('あびる', '浴びる'),
			_Utils_Tuple2('あぶない', '危ない'),
			_Utils_Tuple2('あふりか', 'アフリカ'),
			_Utils_Tuple2('あまい', '甘い'),
			_Utils_Tuple2('あまり', '余り'),
			_Utils_Tuple2('あめ', '雨'),
			_Utils_Tuple2('あめ', 'あめ'),
			_Utils_Tuple2('あめりか', 'アメリカ'),
			_Utils_Tuple2('あめりかがっしゅうこく', 'アメリカ合衆国'),
			_Utils_Tuple2('あらう', '洗う'),
			_Utils_Tuple2('ありがとう', 'ありがとう'),
			_Utils_Tuple2('ある', 'ある'),
			_Utils_Tuple2('あるく', '歩く'),
			_Utils_Tuple2('あるばいと', 'アルバイト'),
			_Utils_Tuple2('あれ', 'あれ'),
			_Utils_Tuple2('あれ', 'あれ'),
			_Utils_Tuple2('あんしん', '安心'),
			_Utils_Tuple2('あんな', 'あんな'),
			_Utils_Tuple2('あんない', '案内'),
			_Utils_Tuple2('いいえ', 'いいえ'),
			_Utils_Tuple2('いう', '言う'),
			_Utils_Tuple2('いえ', '家'),
			_Utils_Tuple2('いえ', 'いえ'),
			_Utils_Tuple2('いき', '行き'),
			_Utils_Tuple2('いぎりす', 'イギリス'),
			_Utils_Tuple2('いきる', '生きる'),
			_Utils_Tuple2('いく', '行く'),
			_Utils_Tuple2('いくつ', '幾つ'),
			_Utils_Tuple2('いくら', '幾ら'),
			_Utils_Tuple2('いくら', '幾ら'),
			_Utils_Tuple2('いくら', '幾ら'),
			_Utils_Tuple2('いけ', '池'),
			_Utils_Tuple2('いざかや', '居酒屋'),
			_Utils_Tuple2('いしゃ', '医者'),
			_Utils_Tuple2('いす', 'いす'),
			_Utils_Tuple2('いそがしい', '忙しい'),
			_Utils_Tuple2('いたい', '痛い'),
			_Utils_Tuple2('いたりあ', 'イタリア'),
			_Utils_Tuple2('いち', '一'),
			_Utils_Tuple2('いちがつ', '一月'),
			_Utils_Tuple2('いちご', 'いちご'),
			_Utils_Tuple2('いちつ', '一つ'),
			_Utils_Tuple2('いちど', '一度'),
			_Utils_Tuple2('いちにち', '一日'),
			_Utils_Tuple2('いちねん', '一年'),
			_Utils_Tuple2('いちはい', '一杯'),
			_Utils_Tuple2('いちばん', '一番'),
			_Utils_Tuple2('いちばん', '一番'),
			_Utils_Tuple2('いちまい', '一枚'),
			_Utils_Tuple2('いつ', 'いつ'),
			_Utils_Tuple2('いつか', '五日'),
			_Utils_Tuple2('いっしょ', '一緒'),
			_Utils_Tuple2('いっしょうけんめい', '一生懸命'),
			_Utils_Tuple2('いつつ', '五つ'),
			_Utils_Tuple2('いつでも', '何時でも'),
			_Utils_Tuple2('いっぱい', '一杯'),
			_Utils_Tuple2('いっぱい', '一杯'),
			_Utils_Tuple2('いつまで', 'いつまで'),
			_Utils_Tuple2('いつも', 'いつも'),
			_Utils_Tuple2('いぬ', '犬'),
			_Utils_Tuple2('いま', '今'),
			_Utils_Tuple2('いみ', '意味'),
			_Utils_Tuple2('いもうと', '妹'),
			_Utils_Tuple2('いや', '嫌'),
			_Utils_Tuple2('いやりんぐ', 'イヤリング'),
			_Utils_Tuple2('いらっしゃいませ', 'いらっしゃいませ'),
			_Utils_Tuple2('いらっしゃる', 'いらっしゃる'),
			_Utils_Tuple2('いりぐち', '入り口'),
			_Utils_Tuple2('いる', 'いる'),
			_Utils_Tuple2('いる', '要る'),
			_Utils_Tuple2('いれる', '入れる'),
			_Utils_Tuple2('いろ', '色'),
			_Utils_Tuple2('いろいろ', '色々'),
			_Utils_Tuple2('いろいろ', '色々'),
			_Utils_Tuple2('いんたーねっと', 'インターネット'),
			_Utils_Tuple2('いんど', 'インド'),
			_Utils_Tuple2('いんどねしあ', 'インドネシア'),
			_Utils_Tuple2('ういすきー', 'ウイスキー'),
			_Utils_Tuple2('うーろんちゃ', 'ウーロン茶'),
			_Utils_Tuple2('ううん', 'ううん'),
			_Utils_Tuple2('うえ', '上'),
			_Utils_Tuple2('うさぎ', 'うさぎ'),
			_Utils_Tuple2('うし', '牛'),
			_Utils_Tuple2('うしろ', '後ろ'),
			_Utils_Tuple2('うすい', '薄い'),
			_Utils_Tuple2('うた', '歌'),
			_Utils_Tuple2('うたう', '歌う'),
			_Utils_Tuple2('うで', '腕'),
			_Utils_Tuple2('うでどけい', '腕時計'),
			_Utils_Tuple2('うどん', 'うどん'),
			_Utils_Tuple2('うま', '馬'),
			_Utils_Tuple2('うまい', 'うまい'),
			_Utils_Tuple2('うまれる', '生まれる'),
			_Utils_Tuple2('うみ', '海'),
			_Utils_Tuple2('うりば', '売り場'),
			_Utils_Tuple2('うる', '売る'),
			_Utils_Tuple2('うるさい', 'うるさい'),
			_Utils_Tuple2('うれしい', 'うれしい'),
			_Utils_Tuple2('うん', 'うん'),
			_Utils_Tuple2('うんうん', 'うんうん'),
			_Utils_Tuple2('うんどう', '運動'),
			_Utils_Tuple2('うんどうじょう', '運動場'),
			_Utils_Tuple2('え', '絵'),
			_Utils_Tuple2('え', 'え'),
			_Utils_Tuple2('えあこん', 'エアコン'),
			_Utils_Tuple2('えいが', '映画'),
			_Utils_Tuple2('えいがかん', '映画館'),
			_Utils_Tuple2('えいご', '英語'),
			_Utils_Tuple2('ええ', 'ええ'),
			_Utils_Tuple2('えーえむ', 'ＡＭ'),
			_Utils_Tuple2('ええと', 'ええと'),
			_Utils_Tuple2('えき', '駅'),
			_Utils_Tuple2('えきいん', '駅員'),
			_Utils_Tuple2('えじぷと', 'エジプト'),
			_Utils_Tuple2('えすかれーたー', 'エスカレーター'),
			_Utils_Tuple2('えすさいず', 'Ｓサイズ'),
			_Utils_Tuple2('えっ', 'えっ'),
			_Utils_Tuple2('えらぶ', '選ぶ'),
			_Utils_Tuple2('えれべーた', 'エレベーター'),
			_Utils_Tuple2('えれべーたー', 'エレベーター'),
			_Utils_Tuple2('えん', '円'),
			_Utils_Tuple2('えんぴつ', '鉛筆'),
			_Utils_Tuple2('おい', 'おい'),
			_Utils_Tuple2('おいしい', 'おいしい'),
			_Utils_Tuple2('おおい', '多い'),
			_Utils_Tuple2('おおきい', '大きい'),
			_Utils_Tuple2('おおきな', '大きな'),
			_Utils_Tuple2('おおさか', '大阪'),
			_Utils_Tuple2('おーすとらりあ', 'オーストラリア'),
			_Utils_Tuple2('おーとばい', 'オートバイ'),
			_Utils_Tuple2('おかね', 'お金'),
			_Utils_Tuple2('おきゃくさん', 'お客さん'),
			_Utils_Tuple2('おきる', '起きる'),
			_Utils_Tuple2('おく', '置く'),
			_Utils_Tuple2('おく', '奥'),
			_Utils_Tuple2('おく', '億'),
			_Utils_Tuple2('おくさま', '奥様'),
			_Utils_Tuple2('おくさん', '奥さん'),
			_Utils_Tuple2('おくる', '送る'),
			_Utils_Tuple2('おくれる', '遅れる'),
			_Utils_Tuple2('おじ', '伯父'),
			_Utils_Tuple2('おじいさん', 'おじいさん'),
			_Utils_Tuple2('おじいちゃん', 'おじいちゃん'),
			_Utils_Tuple2('おしえる', '教える'),
			_Utils_Tuple2('おじさん', '伯父さん'),
			_Utils_Tuple2('おす', '押す'),
			_Utils_Tuple2('おそい', '遅い'),
			_Utils_Tuple2('おちる', '落ちる'),
			_Utils_Tuple2('おっと', '夫'),
			_Utils_Tuple2('おてあらい', 'お手洗い'),
			_Utils_Tuple2('おと', '音'),
			_Utils_Tuple2('おとうさん', 'お父さん'),
			_Utils_Tuple2('おとうと', '弟'),
			_Utils_Tuple2('おとこ', '男'),
			_Utils_Tuple2('おとこのこ', '男の子'),
			_Utils_Tuple2('おとこのひと', '男の人'),
			_Utils_Tuple2('おとす', '落とす'),
			_Utils_Tuple2('おととい', 'おととい'),
			_Utils_Tuple2('おととし', 'おととし'),
			_Utils_Tuple2('おとな', '大人'),
			_Utils_Tuple2('おなか', 'おなか'),
			_Utils_Tuple2('おなじ', '同じ'),
			_Utils_Tuple2('おなじ', '同じ'),
			_Utils_Tuple2('おにいさん', 'お兄さん'),
			_Utils_Tuple2('おにぎり', 'お握り'),
			_Utils_Tuple2('おねえさん', 'お姉さん'),
			_Utils_Tuple2('おねがい', 'おねがい'),
			_Utils_Tuple2('おねがいします', 'お願いします'),
			_Utils_Tuple2('おば', '伯母'),
			_Utils_Tuple2('おばあさん', 'おばあさん'),
			_Utils_Tuple2('おばあちゃん', 'おばあちゃん'),
			_Utils_Tuple2('おばさん', '伯母さん'),
			_Utils_Tuple2('おはよう', 'おはよう'),
			_Utils_Tuple2('おはようございます', 'おはようございます'),
			_Utils_Tuple2('おぼえる', '覚える'),
			_Utils_Tuple2('おまたせしました', 'お待たせしました'),
			_Utils_Tuple2('おもい', '重い'),
			_Utils_Tuple2('おもう', '思う'),
			_Utils_Tuple2('おもしろい', '面白い'),
			_Utils_Tuple2('おや', '親'),
			_Utils_Tuple2('おやすみ', 'お休み'),
			_Utils_Tuple2('およぐ', '泳ぐ'),
			_Utils_Tuple2('おりる', '降りる'),
			_Utils_Tuple2('おりる', '下りる'),
			_Utils_Tuple2('おる', 'おる'),
			_Utils_Tuple2('おれんじ', 'オレンジ'),
			_Utils_Tuple2('おわり', '終わり'),
			_Utils_Tuple2('おわる', '終わる'),
			_Utils_Tuple2('おんがく', '音楽'),
			_Utils_Tuple2('おんせん', '温泉'),
			_Utils_Tuple2('おんな', '女'),
			_Utils_Tuple2('おんなのこ', '女の子'),
			_Utils_Tuple2('おんなのひと', '女の人'),
			_Utils_Tuple2('おんよみ', '音読み'),
			_Utils_Tuple2('か', '課'),
			_Utils_Tuple2('か', '日'),
			_Utils_Tuple2('か', '家'),
			_Utils_Tuple2('か', '火'),
			_Utils_Tuple2('か', '歌'),
			_Utils_Tuple2('が', 'が'),
			_Utils_Tuple2('かーど', 'カード'),
			_Utils_Tuple2('かい', '回'),
			_Utils_Tuple2('かい', '階'),
			_Utils_Tuple2('かい', '海'),
			_Utils_Tuple2('かい', '買い'),
			_Utils_Tuple2('かい', '回'),
			_Utils_Tuple2('がいこく', '外国'),
			_Utils_Tuple2('がいこくご', '外国語'),
			_Utils_Tuple2('がいこくじん', '外国人'),
			_Utils_Tuple2('かいしゃ', '会社'),
			_Utils_Tuple2('かいしゃいん', '会社員'),
			_Utils_Tuple2('かいだん', '階段'),
			_Utils_Tuple2('かいもの', '買い物'),
			_Utils_Tuple2('かいわ', '会話'),
			_Utils_Tuple2('かう', '買う'),
			_Utils_Tuple2('かう', '飼う'),
			_Utils_Tuple2('かえす', '返す'),
			_Utils_Tuple2('かえり', '帰り'),
			_Utils_Tuple2('かえる', '帰る'),
			_Utils_Tuple2('かえる', '変える'),
			_Utils_Tuple2('かお', '顔'),
			_Utils_Tuple2('かがみ', '鏡'),
			_Utils_Tuple2('かぎ', 'かぎ'),
			_Utils_Tuple2('かく', '書く'),
			_Utils_Tuple2('かぐ', '家具'),
			_Utils_Tuple2('がくせい', '学生'),
			_Utils_Tuple2('がくひ', '学費'),
			_Utils_Tuple2('かげつ', '箇月'),
			_Utils_Tuple2('かさ', '傘'),
			_Utils_Tuple2('かし', '菓子'),
			_Utils_Tuple2('かじ', '火事'),
			_Utils_Tuple2('かしゅ', '歌手'),
			_Utils_Tuple2('かす', '貸す'),
			_Utils_Tuple2('かず', '数'),
			_Utils_Tuple2('かぜ', '風邪'),
			_Utils_Tuple2('かぜ', '風'),
			_Utils_Tuple2('かぜぐすり', '風邪薬'),
			_Utils_Tuple2('かぞく', '家族'),
			_Utils_Tuple2('がそりんすたんど', 'ガソリンスタンド'),
			_Utils_Tuple2('かた', '肩'),
			_Utils_Tuple2('かたかな', '片仮名'),
			_Utils_Tuple2('がつ', '月'),
			_Utils_Tuple2('がっき', '学期'),
			_Utils_Tuple2('がっこう', '学校'),
			_Utils_Tuple2('かっぷ', 'カップ'),
			_Utils_Tuple2('かど', '角'),
			_Utils_Tuple2('かなしい', '悲しい'),
			_Utils_Tuple2('かなだ', 'カナダ'),
			_Utils_Tuple2('かね', '金'),
			_Utils_Tuple2('かねもち', '金持ち'),
			_Utils_Tuple2('かのじょ', '彼女'),
			_Utils_Tuple2('かばん', 'かばん'),
			_Utils_Tuple2('かふぇ', 'カフェ'),
			_Utils_Tuple2('かべ', '壁'),
			_Utils_Tuple2('かみ', '紙'),
			_Utils_Tuple2('かみ', '神'),
			_Utils_Tuple2('かみ', '髪'),
			_Utils_Tuple2('かみさま', '神様'),
			_Utils_Tuple2('かみのけ', '髪の毛'),
			_Utils_Tuple2('がむ', 'ガム'),
			_Utils_Tuple2('かめら', 'カメラ'),
			_Utils_Tuple2('かめらまん', 'カメラマン'),
			_Utils_Tuple2('かよう', '火曜'),
			_Utils_Tuple2('かようび', '火曜日'),
			_Utils_Tuple2('からい', '辛い'),
			_Utils_Tuple2('からおけ', 'カラオケ'),
			_Utils_Tuple2('がらす', 'ガラス'),
			_Utils_Tuple2('からだ', '体'),
			_Utils_Tuple2('かりる', '借りる'),
			_Utils_Tuple2('かるい', '軽い'),
			_Utils_Tuple2('かれ', '彼'),
			_Utils_Tuple2('かれー', 'カレー'),
			_Utils_Tuple2('かれーらいす', 'カレーライス'),
			_Utils_Tuple2('かれし', '彼氏'),
			_Utils_Tuple2('かれんだー', 'カレンダー'),
			_Utils_Tuple2('かわ', '川'),
			_Utils_Tuple2('かわいい', 'かわいい'),
			_Utils_Tuple2('かわいそう', 'かわいそう'),
			_Utils_Tuple2('かわく', '渇く'),
			_Utils_Tuple2('かわる', '変わる'),
			_Utils_Tuple2('かん', '館'),
			_Utils_Tuple2('かんがえる', '考える'),
			_Utils_Tuple2('かんこく', '韓国'),
			_Utils_Tuple2('かんじ', '漢字'),
			_Utils_Tuple2('かんたん', '簡単'),
			_Utils_Tuple2('がんばる', '頑張る'),
			_Utils_Tuple2('き', '木'),
			_Utils_Tuple2('き', '機'),
			_Utils_Tuple2('きいろ', '黄色'),
			_Utils_Tuple2('きいろい', '黄色い'),
			_Utils_Tuple2('きえる', '消える'),
			_Utils_Tuple2('きく', '聞く'),
			_Utils_Tuple2('きけん', '危険'),
			_Utils_Tuple2('きこえる', '聞こえる'),
			_Utils_Tuple2('きこく', '帰国'),
			_Utils_Tuple2('きす', 'キス'),
			_Utils_Tuple2('きた', '北'),
			_Utils_Tuple2('ぎたー', 'ギター'),
			_Utils_Tuple2('きたあめりか', '北アメリカ'),
			_Utils_Tuple2('きたない', '汚い'),
			_Utils_Tuple2('きっさてん', '喫茶店'),
			_Utils_Tuple2('きって', '切手'),
			_Utils_Tuple2('きっぷ', '切符'),
			_Utils_Tuple2('きのう', '昨日'),
			_Utils_Tuple2('きびしい', '厳しい'),
			_Utils_Tuple2('きぶん', '気分'),
			_Utils_Tuple2('きまる', '決まる'),
			_Utils_Tuple2('きむち', 'キムチ'),
			_Utils_Tuple2('きめる', '決める'),
			_Utils_Tuple2('きもち', '気持ち'),
			_Utils_Tuple2('きもちいい', '気持ち良い'),
			_Utils_Tuple2('きゃく', '客'),
			_Utils_Tuple2('きゃっしゅかーど', 'キャッシュカード'),
			_Utils_Tuple2('きゃべつ', 'キャベツ'),
			_Utils_Tuple2('きゅう', '九'),
			_Utils_Tuple2('きゅう', '九'),
			_Utils_Tuple2('きゅうじつ', '休日'),
			_Utils_Tuple2('きゅうしゅう', '九州'),
			_Utils_Tuple2('ぎゅうどん', '牛丼'),
			_Utils_Tuple2('ぎゅうにく', '牛肉'),
			_Utils_Tuple2('ぎゅうにゅう', '牛乳'),
			_Utils_Tuple2('きゅうり', 'きゅうり'),
			_Utils_Tuple2('きょう', '今日'),
			_Utils_Tuple2('きょうかい', '教会'),
			_Utils_Tuple2('きょうかしょ', '教科書'),
			_Utils_Tuple2('きょうし', '教師'),
			_Utils_Tuple2('きょうしつ', '教室'),
			_Utils_Tuple2('きょうだい', '兄弟'),
			_Utils_Tuple2('きょく', '局'),
			_Utils_Tuple2('きょねん', '去年'),
			_Utils_Tuple2('きらい', '嫌い'),
			_Utils_Tuple2('きる', '着る'),
			_Utils_Tuple2('きる', '切る'),
			_Utils_Tuple2('きれい', 'きれい'),
			_Utils_Tuple2('きろ', 'キロ'),
			_Utils_Tuple2('きろ', 'キロ'),
			_Utils_Tuple2('きろぐらむ', 'キログラム'),
			_Utils_Tuple2('きろぐらむ', 'キログラム'),
			_Utils_Tuple2('きろめーとる', 'キロメートル'),
			_Utils_Tuple2('きん', '金'),
			_Utils_Tuple2('ぎんこう', '銀行'),
			_Utils_Tuple2('ぎんざ', '銀座'),
			_Utils_Tuple2('きんよう', '金曜'),
			_Utils_Tuple2('きんようび', '金曜日'),
			_Utils_Tuple2('く', '区'),
			_Utils_Tuple2('く', '区'),
			_Utils_Tuple2('くうこう', '空港'),
			_Utils_Tuple2('くがつ', '九月'),
			_Utils_Tuple2('くすり', '薬'),
			_Utils_Tuple2('くすりゆび', '薬指'),
			_Utils_Tuple2('くだもの', '果物'),
			_Utils_Tuple2('くち', '口'),
			_Utils_Tuple2('くつ', '靴'),
			_Utils_Tuple2('くっきー', 'クッキー'),
			_Utils_Tuple2('くつした', '靴下'),
			_Utils_Tuple2('くに', '国'),
			_Utils_Tuple2('くび', '首'),
			_Utils_Tuple2('くみ', '組'),
			_Utils_Tuple2('くも', '雲'),
			_Utils_Tuple2('くもり', '曇り'),
			_Utils_Tuple2('くもる', '曇る'),
			_Utils_Tuple2('くやくしょ', '区役所'),
			_Utils_Tuple2('くらい', '暗い'),
			_Utils_Tuple2('くらす', 'クラス'),
			_Utils_Tuple2('くらすめーと', 'クラスメート'),
			_Utils_Tuple2('くらぶ', 'クラブ'),
			_Utils_Tuple2('ぐらむ', 'グラム'),
			_Utils_Tuple2('ぐらむ', 'グラム'),
			_Utils_Tuple2('くりすます', 'クリスマス'),
			_Utils_Tuple2('くりすますいぶ', 'クリスマスイブ'),
			_Utils_Tuple2('くりすますかーど', 'クリスマスカード'),
			_Utils_Tuple2('くりすますけーき', 'クリスマスケーキ'),
			_Utils_Tuple2('くりすますぷれぜんと', 'クリスマスプレゼント'),
			_Utils_Tuple2('くる', '来る'),
			_Utils_Tuple2('ぐるーぷ', 'グループ'),
			_Utils_Tuple2('くるま', '車'),
			_Utils_Tuple2('ぐれーぷふるーつ', 'グレープフルーツ'),
			_Utils_Tuple2('くれる', 'くれる'),
			_Utils_Tuple2('くろ', '黒'),
			_Utils_Tuple2('くろい', '黒い'),
			_Utils_Tuple2('くん', '君'),
			_Utils_Tuple2('け', '毛'),
			_Utils_Tuple2('けいさつ', '警察'),
			_Utils_Tuple2('けいたいでんわ', '携帯電話'),
			_Utils_Tuple2('けいようし', '形容詞'),
			_Utils_Tuple2('けーき', 'ケーキ'),
			_Utils_Tuple2('げーむ', 'ゲーム'),
			_Utils_Tuple2('けさ', '今朝'),
			_Utils_Tuple2('けす', '消す'),
			_Utils_Tuple2('けちゃっぷ', 'ケチャップ'),
			_Utils_Tuple2('げつ', '月'),
			_Utils_Tuple2('けっこん', '結婚'),
			_Utils_Tuple2('けっこんしき', '結婚式'),
			_Utils_Tuple2('けっせき', '欠席'),
			_Utils_Tuple2('げつよう', '月曜'),
			_Utils_Tuple2('げつようひ', '月曜日'),
			_Utils_Tuple2('げんき', '元気'),
			_Utils_Tuple2('こ', '個'),
			_Utils_Tuple2('こ', '子'),
			_Utils_Tuple2('ご', '五'),
			_Utils_Tuple2('ご', '五'),
			_Utils_Tuple2('ご', '後'),
			_Utils_Tuple2('こいぬ', '子犬'),
			_Utils_Tuple2('こいびと', '恋人'),
			_Utils_Tuple2('こう', 'こう'),
			_Utils_Tuple2('こう', '校'),
			_Utils_Tuple2('こうえん', '公園'),
			_Utils_Tuple2('こうこう', '高校'),
			_Utils_Tuple2('こうこうせい', '高校生'),
			_Utils_Tuple2('こうちゃ', '紅茶'),
			_Utils_Tuple2('こうちょう', '校長'),
			_Utils_Tuple2('こうつう', '交通'),
			_Utils_Tuple2('こうつうじこ', '交通事故'),
			_Utils_Tuple2('こうばん', '交番'),
			_Utils_Tuple2('こえ', '声'),
			_Utils_Tuple2('こーと', 'コート'),
			_Utils_Tuple2('こーひー', 'コーヒー'),
			_Utils_Tuple2('こーら', 'コーラ'),
			_Utils_Tuple2('こおり', '氷'),
			_Utils_Tuple2('ごがつ', '五月'),
			_Utils_Tuple2('こく', '国'),
			_Utils_Tuple2('ごくろうさま', '御苦労様'),
			_Utils_Tuple2('ここ', 'ここ'),
			_Utils_Tuple2('ごご', '午後'),
			_Utils_Tuple2('ここあ', 'ココア'),
			_Utils_Tuple2('ここのか', '九日'),
			_Utils_Tuple2('ここのつ', '九つ'),
			_Utils_Tuple2('こころ', '心'),
			_Utils_Tuple2('ごぜん', '午前'),
			_Utils_Tuple2('こたえ', '答え'),
			_Utils_Tuple2('こたえる', '答える'),
			_Utils_Tuple2('ごちそうさま', '御馳走様'),
			_Utils_Tuple2('こちら', 'こちら'),
			_Utils_Tuple2('こっぷ', 'コップ'),
			_Utils_Tuple2('こと', '事'),
			_Utils_Tuple2('こと', '事　'),
			_Utils_Tuple2('ことし', '今年'),
			_Utils_Tuple2('ことば', '言葉'),
			_Utils_Tuple2('こども', '子供'),
			_Utils_Tuple2('この', 'この'),
			_Utils_Tuple2('このごろ', 'このごろ'),
			_Utils_Tuple2('ごはん', '御飯'),
			_Utils_Tuple2('こぴー', 'コピー'),
			_Utils_Tuple2('こまる', '困る'),
			_Utils_Tuple2('ごみ', 'ごみ'),
			_Utils_Tuple2('こめ', '米'),
			_Utils_Tuple2('ごめん', 'ごめん'),
			_Utils_Tuple2('こゆび', '小指'),
			_Utils_Tuple2('ごるふ', 'ゴルフ'),
			_Utils_Tuple2('これ', 'これ'),
			_Utils_Tuple2('これから', 'これから'),
			_Utils_Tuple2('これら', 'これら'),
			_Utils_Tuple2('ころ', 'ころ'),
			_Utils_Tuple2('こわい', '怖い'),
			_Utils_Tuple2('こわす', '壊す'),
			_Utils_Tuple2('こんげつ', '今月'),
			_Utils_Tuple2('こんさーと', 'コンサート'),
			_Utils_Tuple2('こんしゅう', '今週'),
			_Utils_Tuple2('こんたくとれんず', 'コンタクトレンズ'),
			_Utils_Tuple2('こんど', '今度'),
			_Utils_Tuple2('こんな', 'こんな'),
			_Utils_Tuple2('こんな', 'こんな'),
			_Utils_Tuple2('こんにちは', 'こんにちは'),
			_Utils_Tuple2('こんばん', '今晩'),
			_Utils_Tuple2('こんばんは', 'こんばんは'),
			_Utils_Tuple2('こんびに', 'コンビニ'),
			_Utils_Tuple2('こんぴゅーたー', 'コンピューター'),
			_Utils_Tuple2('こんや', '今夜'),
			_Utils_Tuple2('さ', 'さ'),
			_Utils_Tuple2('さい', '歳'),
			_Utils_Tuple2('さいきん', '最近'),
			_Utils_Tuple2('さいご', '最後'),
			_Utils_Tuple2('さいふ', '財布'),
			_Utils_Tuple2('さかな', '魚'),
			_Utils_Tuple2('さくぶん', '作文'),
			_Utils_Tuple2('さくら', '桜'),
			_Utils_Tuple2('さけ', '酒'),
			_Utils_Tuple2('さけ', 'さけ'),
			_Utils_Tuple2('さしみ', '刺し身'),
			_Utils_Tuple2('さつ', '冊'),
			_Utils_Tuple2('さっかー', 'サッカー'),
			_Utils_Tuple2('ざっし', '雑誌'),
			_Utils_Tuple2('さとう', '砂糖'),
			_Utils_Tuple2('さま', '様'),
			_Utils_Tuple2('さむい', '寒い'),
			_Utils_Tuple2('さようなら', 'さようなら'),
			_Utils_Tuple2('さよなら', 'さよなら'),
			_Utils_Tuple2('さら', '皿'),
			_Utils_Tuple2('さらいねん', '再来年'),
			_Utils_Tuple2('さらだ', 'サラダ'),
			_Utils_Tuple2('さる', '猿'),
			_Utils_Tuple2('さん', '三'),
			_Utils_Tuple2('さん', 'さん'),
			_Utils_Tuple2('さん', '山'),
			_Utils_Tuple2('さんがつ', '三月'),
			_Utils_Tuple2('さんぐらす', 'サングラス'),
			_Utils_Tuple2('さんじ', '三時'),
			_Utils_Tuple2('さんど', '三度'),
			_Utils_Tuple2('さんどいっち', 'サンドイッチ'),
			_Utils_Tuple2('ざんねん', '残念'),
			_Utils_Tuple2('ざんねん', '残念'),
			_Utils_Tuple2('さんぽ', '散歩'),
			_Utils_Tuple2('し', '四'),
			_Utils_Tuple2('じ', '時'),
			_Utils_Tuple2('じ', '時'),
			_Utils_Tuple2('じ', '時'),
			_Utils_Tuple2('じいさん', 'じいさん'),
			_Utils_Tuple2('しーでぃー', 'ＣＤ'),
			_Utils_Tuple2('じーんず', 'ジーンズ'),
			_Utils_Tuple2('じぇーあーる', 'ＪＲ'),
			_Utils_Tuple2('しお', '塩'),
			_Utils_Tuple2('しかし', 'しかし'),
			_Utils_Tuple2('しがつ', '四月'),
			_Utils_Tuple2('じかん', '時間'),
			_Utils_Tuple2('じかん', '時間'),
			_Utils_Tuple2('しけん', '試験'),
			_Utils_Tuple2('じこ', '事故'),
			_Utils_Tuple2('じこしょうかい', '自己紹介'),
			_Utils_Tuple2('しごと', '仕事'),
			_Utils_Tuple2('じしょ', '辞書'),
			_Utils_Tuple2('しずか', '静か'),
			_Utils_Tuple2('した', '下'),
			_Utils_Tuple2('しち', '七'),
			_Utils_Tuple2('しちがつ', '七月'),
			_Utils_Tuple2('じつ', '日'),
			_Utils_Tuple2('しつもん', '質問'),
			_Utils_Tuple2('しつれい', '失礼'),
			_Utils_Tuple2('じてんしゃ', '自転車'),
			_Utils_Tuple2('じどうしゃ', '自動車'),
			_Utils_Tuple2('しぬ', '死ぬ'),
			_Utils_Tuple2('じぶん', '自分'),
			_Utils_Tuple2('しまる', '閉まる'),
			_Utils_Tuple2('しめる', '閉める'),
			_Utils_Tuple2('しゃ', '車'),
			_Utils_Tuple2('しゃ', '社'),
			_Utils_Tuple2('じゃ', 'じゃ'),
			_Utils_Tuple2('しゃーぺん', 'シャーペン'),
			_Utils_Tuple2('しゃいん', '社員'),
			_Utils_Tuple2('しゃかい', '社会'),
			_Utils_Tuple2('じゃがいも', 'ジャガ芋'),
			_Utils_Tuple2('しゃしん', '写真'),
			_Utils_Tuple2('じゃず', 'ジャズ'),
			_Utils_Tuple2('しゃちょう', '社長'),
			_Utils_Tuple2('しゃつ', 'シャツ'),
			_Utils_Tuple2('じゃむ', 'ジャム'),
			_Utils_Tuple2('しゃわー', 'シャワー'),
			_Utils_Tuple2('じゃんけん', 'じゃんけん'),
			_Utils_Tuple2('しゃんぷー', 'シャンプー'),
			_Utils_Tuple2('しゅ', '酒'),
			_Utils_Tuple2('しゅう', '週'),
			_Utils_Tuple2('しゅう', '週'),
			_Utils_Tuple2('じゅう', '十'),
			_Utils_Tuple2('じゅういちがつ', '十一月'),
			_Utils_Tuple2('じゅうがつ', '十月'),
			_Utils_Tuple2('しゅうかん', '週間'),
			_Utils_Tuple2('じゅうしょ', '住所'),
			_Utils_Tuple2('じゅーす', 'ジュース'),
			_Utils_Tuple2('じゅうどう', '柔道'),
			_Utils_Tuple2('じゅうにがつ', '十二月'),
			_Utils_Tuple2('しゅうまつ', '週末'),
			_Utils_Tuple2('じゅうろく', '十六'),
			_Utils_Tuple2('じゅぎょう', '授業'),
			_Utils_Tuple2('しゅくだい', '宿題'),
			_Utils_Tuple2('しゅじん', '主人'),
			_Utils_Tuple2('しゅみ', '趣味'),
			_Utils_Tuple2('じょ', '女'),
			_Utils_Tuple2('じょ', '女'),
			_Utils_Tuple2('しょうかい', '紹介'),
			_Utils_Tuple2('しょうがつ', '正月'),
			_Utils_Tuple2('じょうず', '上手'),
			_Utils_Tuple2('じょうず', '上手'),
			_Utils_Tuple2('しょうねん', '少年'),
			_Utils_Tuple2('しょうゆ', 'しょうゆ'),
			_Utils_Tuple2('しょきゅう', '初級'),
			_Utils_Tuple2('しょくじ', '食事'),
			_Utils_Tuple2('しょくどう', '食堂'),
			_Utils_Tuple2('じょし', '女子'),
			_Utils_Tuple2('じょせい', '女性'),
			_Utils_Tuple2('しょっぴんぐ', 'ショッピング'),
			_Utils_Tuple2('しらべる', '調べる'),
			_Utils_Tuple2('しる', '知る'),
			_Utils_Tuple2('しろ', '白'),
			_Utils_Tuple2('しろい', '白い'),
			_Utils_Tuple2('じん', '人'),
			_Utils_Tuple2('しんかんせん', '新幹線'),
			_Utils_Tuple2('しんごう', '信号'),
			_Utils_Tuple2('じんこう', '人口'),
			_Utils_Tuple2('じんじゃ', '神社'),
			_Utils_Tuple2('しんせき', '親戚'),
			_Utils_Tuple2('しんせつ', '親切'),
			_Utils_Tuple2('しんぱい', '心配'),
			_Utils_Tuple2('しんぶん', '新聞'),
			_Utils_Tuple2('しんぶんし', '新聞紙'),
			_Utils_Tuple2('すい', '水'),
			_Utils_Tuple2('すいえい', '水泳'),
			_Utils_Tuple2('すいか', 'すいか'),
			_Utils_Tuple2('すいよう', '水曜'),
			_Utils_Tuple2('すいようび', '水曜日'),
			_Utils_Tuple2('すう', '吸う'),
			_Utils_Tuple2('すーつ', 'スーツ'),
			_Utils_Tuple2('すーつけーす', 'スーツケース'),
			_Utils_Tuple2('すーぱーまーけっと', 'スーパーマーケット'),
			_Utils_Tuple2('すーぷ', 'スープ'),
			_Utils_Tuple2('すかーと', 'スカート'),
			_Utils_Tuple2('すき', '好き'),
			_Utils_Tuple2('すき', '好き'),
			_Utils_Tuple2('すきー', 'スキー'),
			_Utils_Tuple2('すきやき', 'すき焼き'),
			_Utils_Tuple2('すく', 'すく'),
			_Utils_Tuple2('すぐ', 'すぐ'),
			_Utils_Tuple2('すくない', '少ない'),
			_Utils_Tuple2('すぐに', 'すぐに'),
			_Utils_Tuple2('すけーと', 'スケート'),
			_Utils_Tuple2('すこし', '少し'),
			_Utils_Tuple2('すし', 'すし'),
			_Utils_Tuple2('すずしい', '涼しい'),
			_Utils_Tuple2('すすむ', '進む'),
			_Utils_Tuple2('ずっと', 'ずっと'),
			_Utils_Tuple2('すてーき', 'ステーキ'),
			_Utils_Tuple2('すてる', '捨てる'),
			_Utils_Tuple2('すとーぶ', 'ストーブ'),
			_Utils_Tuple2('すぱげってぃ', 'スパゲッティ'),
			_Utils_Tuple2('すばらしい', '素晴らしい'),
			_Utils_Tuple2('すぷーん', 'スプーン'),
			_Utils_Tuple2('すぽーつ', 'スポーツ'),
			_Utils_Tuple2('ずぼん', 'ズボン'),
			_Utils_Tuple2('すみません', '済みません'),
			_Utils_Tuple2('すむ', '住む'),
			_Utils_Tuple2('すりっぱ', 'スリッパ'),
			_Utils_Tuple2('する', 'する'),
			_Utils_Tuple2('すわる', '座る'),
			_Utils_Tuple2('せ', '背'),
			_Utils_Tuple2('せいかつ', '生活'),
			_Utils_Tuple2('せいと', '生徒'),
			_Utils_Tuple2('せいねんつきひ', '生年月日'),
			_Utils_Tuple2('せーたー', 'セーター'),
			_Utils_Tuple2('せかい', '世界'),
			_Utils_Tuple2('せき', '席'),
			_Utils_Tuple2('せき', 'せき'),
			_Utils_Tuple2('せっけん', 'せっけん'),
			_Utils_Tuple2('せなか', '背中'),
			_Utils_Tuple2('せぶん', 'セブン'),
			_Utils_Tuple2('せまい', '狭い'),
			_Utils_Tuple2('ぜろ', 'ゼロ'),
			_Utils_Tuple2('せろてーぷ', 'セロテープ'),
			_Utils_Tuple2('せん', '千'),
			_Utils_Tuple2('せん', '線'),
			_Utils_Tuple2('せんげつ', '先月'),
			_Utils_Tuple2('せんしゅう', '先週'),
			_Utils_Tuple2('せんせい', '先生'),
			_Utils_Tuple2('ぜんぜん', '全然'),
			_Utils_Tuple2('せんたく', '洗濯'),
			_Utils_Tuple2('せんたくき', '洗濯機'),
			_Utils_Tuple2('せんち', 'センチ'),
			_Utils_Tuple2('せんち', 'センチ'),
			_Utils_Tuple2('せんちめーとる', 'センチメートル'),
			_Utils_Tuple2('ぜんぶ', '全部'),
			_Utils_Tuple2('ぞう', '象'),
			_Utils_Tuple2('そうじ', '掃除'),
			_Utils_Tuple2('そうじき', '掃除機'),
			_Utils_Tuple2('そく', '足'),
			_Utils_Tuple2('そこ', 'そこ'),
			_Utils_Tuple2('そして', 'そして'),
			_Utils_Tuple2('そちら', 'そちら'),
			_Utils_Tuple2('そと', '外'),
			_Utils_Tuple2('その', 'その'),
			_Utils_Tuple2('その', 'その'),
			_Utils_Tuple2('そば', '側'),
			_Utils_Tuple2('そば', 'そば'),
			_Utils_Tuple2('そふ', '祖父'),
			_Utils_Tuple2('そふァー', 'ソファー'),
			_Utils_Tuple2('そぼ', '祖母'),
			_Utils_Tuple2('そら', '空'),
			_Utils_Tuple2('それ', 'それ'),
			_Utils_Tuple2('それから', 'それから'),
			_Utils_Tuple2('それは', 'それは'),
			_Utils_Tuple2('だい', '台'),
			_Utils_Tuple2('だいがく', '大学'),
			_Utils_Tuple2('だいがくいん', '大学院'),
			_Utils_Tuple2('だいがくせい', '大学生'),
			_Utils_Tuple2('たいしかん', '大使館'),
			_Utils_Tuple2('だいじょうぶ', '大丈夫'),
			_Utils_Tuple2('だいじょうぶ', '大丈夫'),
			_Utils_Tuple2('だいすき', '大好き'),
			_Utils_Tuple2('たいせつ', '大切'),
			_Utils_Tuple2('だいどころ', '台所'),
			_Utils_Tuple2('たいへん', '大変'),
			_Utils_Tuple2('たいへん', '大変'),
			_Utils_Tuple2('たかい', '高い'),
			_Utils_Tuple2('たくさん', '沢山'),
			_Utils_Tuple2('たくさん', '沢山'),
			_Utils_Tuple2('たくしー', 'タクシー'),
			_Utils_Tuple2('だす', '出す'),
			_Utils_Tuple2('ただいま', 'ただいま'),
			_Utils_Tuple2('たち', 'たち'),
			_Utils_Tuple2('たつ', '立つ'),
			_Utils_Tuple2('たっきゅう', '卓球'),
			_Utils_Tuple2('たてもの', '建物'),
			_Utils_Tuple2('たてる', '立てる'),
			_Utils_Tuple2('たのしい', '楽しい'),
			_Utils_Tuple2('たのしみ', '楽しみ'),
			_Utils_Tuple2('たのしむ', '楽しむ'),
			_Utils_Tuple2('たばこ', 'タバコ'),
			_Utils_Tuple2('たぶん', '多分'),
			_Utils_Tuple2('たべもの', '食べ物'),
			_Utils_Tuple2('たべる', '食べる'),
			_Utils_Tuple2('たまご', '卵'),
			_Utils_Tuple2('たまねぎ', 'たまねぎ'),
			_Utils_Tuple2('だめ', '駄目'),
			_Utils_Tuple2('だれ', '誰'),
			_Utils_Tuple2('だれか', 'だれか'),
			_Utils_Tuple2('たんじょうひ', '誕生日'),
			_Utils_Tuple2('だんす', 'ダンス'),
			_Utils_Tuple2('だんせい', '男性'),
			_Utils_Tuple2('ちいさい', '小さい'),
			_Utils_Tuple2('ちいさな', '小さな'),
			_Utils_Tuple2('ちーず', 'チーズ'),
			_Utils_Tuple2('ちかい', '近い'),
			_Utils_Tuple2('ちがう', '違う'),
			_Utils_Tuple2('ちかく', '近く'),
			_Utils_Tuple2('ちかてつ', '地下鉄'),
			_Utils_Tuple2('ちけっと', 'チケット'),
			_Utils_Tuple2('ちず', '地図'),
			_Utils_Tuple2('ちち', '父'),
			_Utils_Tuple2('ちちのひ', '父の日'),
			_Utils_Tuple2('ちゃ', '茶'),
			_Utils_Tuple2('ちゃいろ', '茶色'),
			_Utils_Tuple2('ちゃん', 'ちゃん'),
			_Utils_Tuple2('ちゅうい', '注意'),
			_Utils_Tuple2('ちゅうがく', '中学'),
			_Utils_Tuple2('ちゅうがくせい', '中学生'),
			_Utils_Tuple2('ちゅうがっこう', '中学校'),
			_Utils_Tuple2('ちゅうかりょうり', '中華料理'),
			_Utils_Tuple2('ちょうめ', '丁目'),
			_Utils_Tuple2('ちょこ', 'チョコ'),
			_Utils_Tuple2('ちょこれーと', 'チョコレート'),
			_Utils_Tuple2('ちょっと', 'ちょっと'),
			_Utils_Tuple2('ついたち', '一日'),
			_Utils_Tuple2('つかう', '使う'),
			_Utils_Tuple2('つかれる', '疲れる'),
			_Utils_Tuple2('つき', '月'),
			_Utils_Tuple2('つき', '月'),
			_Utils_Tuple2('つくえ', '机'),
			_Utils_Tuple2('つくる', '作る'),
			_Utils_Tuple2('つま', '妻'),
			_Utils_Tuple2('つめ', '爪'),
			_Utils_Tuple2('つめたい', '冷たい'),
			_Utils_Tuple2('つよい', '強い'),
			_Utils_Tuple2('て', '手'),
			_Utils_Tuple2('てぃーしゃつ', 'Ｔシャツ'),
			_Utils_Tuple2('てぃっしゅ', 'ティッシュ'),
			_Utils_Tuple2('てぃっしゅぺーぱー', 'ティッシュペーパー'),
			_Utils_Tuple2('てーぷ', 'テープ'),
			_Utils_Tuple2('てーぶる', 'テーブル'),
			_Utils_Tuple2('てーぷれこーだー', 'テープレコーダー'),
			_Utils_Tuple2('でかける', '出掛ける'),
			_Utils_Tuple2('てがみ', '手紙'),
			_Utils_Tuple2('てきすと', 'テキスト'),
			_Utils_Tuple2('できる', '出来る'),
			_Utils_Tuple2('でぐち', '出口'),
			_Utils_Tuple2('てすと', 'テスト'),
			_Utils_Tuple2('てにす', 'テニス'),
			_Utils_Tuple2('でぱーと', 'デパート'),
			_Utils_Tuple2('てら', '寺'),
			_Utils_Tuple2('でる', '出る'),
			_Utils_Tuple2('てれび', 'テレビ'),
			_Utils_Tuple2('てん', '店'),
			_Utils_Tuple2('てん', '点'),
			_Utils_Tuple2('てんいん', '店員'),
			_Utils_Tuple2('てんき', '天気'),
			_Utils_Tuple2('でんき', '電気'),
			_Utils_Tuple2('でんしゃ', '電車'),
			_Utils_Tuple2('でんわ', '電話'),
			_Utils_Tuple2('でんわばんごう', '電話番号'),
			_Utils_Tuple2('ど', '度'),
			_Utils_Tuple2('どあ', 'ドア'),
			_Utils_Tuple2('といれ', 'トイレ'),
			_Utils_Tuple2('といれっとぺーぱー', 'トイレットペーパー'),
			_Utils_Tuple2('どう', 'どう'),
			_Utils_Tuple2('とうさん', '父さん'),
			_Utils_Tuple2('どうし', '動詞'),
			_Utils_Tuple2('どうぞ', 'どうぞ'),
			_Utils_Tuple2('どうぶつ', '動物'),
			_Utils_Tuple2('どうぶつえん', '動物園'),
			_Utils_Tuple2('どうも', 'どうも'),
			_Utils_Tuple2('とうもろこし', 'とうもろこし'),
			_Utils_Tuple2('とお', '十'),
			_Utils_Tuple2('とおい', '遠い'),
			_Utils_Tuple2('とおか', '十日'),
			_Utils_Tuple2('とおく', '遠く'),
			_Utils_Tuple2('とーすと', 'トースト'),
			_Utils_Tuple2('どーなつ', 'ドーナツ'),
			_Utils_Tuple2('とき', '時'),
			_Utils_Tuple2('ときどき', '時々'),
			_Utils_Tuple2('とけい', '時計'),
			_Utils_Tuple2('どこ', 'どこ'),
			_Utils_Tuple2('どこか', 'どこか'),
			_Utils_Tuple2('ところ', '所'),
			_Utils_Tuple2('とし', '年'),
			_Utils_Tuple2('としょかん', '図書館'),
			_Utils_Tuple2('としょしつ', '図書室'),
			_Utils_Tuple2('どちら', 'どちら'),
			_Utils_Tuple2('とても', 'とても'),
			_Utils_Tuple2('とどく', '届く'),
			_Utils_Tuple2('どなた', 'どなた'),
			_Utils_Tuple2('となり', '隣'),
			_Utils_Tuple2('どの', 'どの'),
			_Utils_Tuple2('どのくらい', 'どのくらい'),
			_Utils_Tuple2('とぶ', '飛ぶ'),
			_Utils_Tuple2('とまと', 'トマト'),
			_Utils_Tuple2('とまる', '止まる'),
			_Utils_Tuple2('ともだち', '友達'),
			_Utils_Tuple2('どよう', '土曜'),
			_Utils_Tuple2('どようび', '土曜日'),
			_Utils_Tuple2('とり', '鳥'),
			_Utils_Tuple2('とる', '取る'),
			_Utils_Tuple2('とる', '撮る'),
			_Utils_Tuple2('どる', 'ドル'),
			_Utils_Tuple2('どれ', 'どれ'),
			_Utils_Tuple2('どんな', 'どんな'),
			_Utils_Tuple2('どんな', 'どんな'),
			_Utils_Tuple2('ない', 'ない'),
			_Utils_Tuple2('ないふ', 'ナイフ'),
			_Utils_Tuple2('なか', '中'),
			_Utils_Tuple2('ながい', '長い'),
			_Utils_Tuple2('なぜ', 'なぜ'),
			_Utils_Tuple2('なつ', '夏'),
			_Utils_Tuple2('なつやすみ', '夏休み'),
			_Utils_Tuple2('なな', '七'),
			_Utils_Tuple2('ななつ', '七つ'),
			_Utils_Tuple2('なに', '何'),
			_Utils_Tuple2('なにか', '何か'),
			_Utils_Tuple2('なにも', '何も'),
			_Utils_Tuple2('なのか', '七日'),
			_Utils_Tuple2('なまえ', '名前'),
			_Utils_Tuple2('なる', '成る'),
			_Utils_Tuple2('なん', '何'),
			_Utils_Tuple2('なんかい', '何回'),
			_Utils_Tuple2('なんこ', '何個'),
			_Utils_Tuple2('なんで', '何で'),
			_Utils_Tuple2('なんでも', '何でも'),
			_Utils_Tuple2('なんど', '何度'),
			_Utils_Tuple2('なんにち', '何日'),
			_Utils_Tuple2('なんにん', '何人'),
			_Utils_Tuple2('なんねん', '何年'),
			_Utils_Tuple2('なんの', '何の'),
			_Utils_Tuple2('なんふん', '何分'),
			_Utils_Tuple2('に', '二'),
			_Utils_Tuple2('にいさん', '兄さん'),
			_Utils_Tuple2('にいさん', '兄さん'),
			_Utils_Tuple2('にかい', '二階'),
			_Utils_Tuple2('にがい', '苦い'),
			_Utils_Tuple2('にがつ', '二月'),
			_Utils_Tuple2('にぎやか', 'にぎやか'),
			_Utils_Tuple2('にく', '肉'),
			_Utils_Tuple2('にし', '西'),
			_Utils_Tuple2('にじゅう', '二十'),
			_Utils_Tuple2('にち', '日'),
			_Utils_Tuple2('にち', '日'),
			_Utils_Tuple2('にちよう', '日曜'),
			_Utils_Tuple2('にちようび', '日曜日'),
			_Utils_Tuple2('にっぽん', '日本'),
			_Utils_Tuple2('にほんご', '日本語'),
			_Utils_Tuple2('にほんしゅ', '日本酒'),
			_Utils_Tuple2('にもつ', '荷物'),
			_Utils_Tuple2('にゅーす', 'ニュース'),
			_Utils_Tuple2('にわ', '庭'),
			_Utils_Tuple2('にん', '人'),
			_Utils_Tuple2('にん', '人'),
			_Utils_Tuple2('にんぎょう', '人形'),
			_Utils_Tuple2('にんじん', 'にんじん'),
			_Utils_Tuple2('ぬぐ', '脱ぐ'),
			_Utils_Tuple2('ねえさん', '姉さん'),
			_Utils_Tuple2('ねえさん', '姉さん'),
			_Utils_Tuple2('ねくたい', 'ネクタイ'),
			_Utils_Tuple2('ねこ', '猫'),
			_Utils_Tuple2('ねだん', '値段'),
			_Utils_Tuple2('ねつ', '熱'),
			_Utils_Tuple2('ねむい', '眠い'),
			_Utils_Tuple2('ねる', '寝る'),
			_Utils_Tuple2('ねん', '年'),
			_Utils_Tuple2('ねん', '年'),
			_Utils_Tuple2('のーと', 'ノート'),
			_Utils_Tuple2('のど', '喉'),
			_Utils_Tuple2('のぼる', '登る'),
			_Utils_Tuple2('のみもの', '飲み物'),
			_Utils_Tuple2('のむ', '飲む'),
			_Utils_Tuple2('のる', '乗る'),
			_Utils_Tuple2('は', '歯'),
			_Utils_Tuple2('ぱーてぃー', 'パーティー'),
			_Utils_Tuple2('はい', 'はい'),
			_Utils_Tuple2('はい', '杯'),
			_Utils_Tuple2('ばいく', 'バイク'),
			_Utils_Tuple2('はいしゃ', '歯医者'),
			_Utils_Tuple2('ばいと', 'バイト'),
			_Utils_Tuple2('ぱいなっぷる', 'パイナップル'),
			_Utils_Tuple2('はいひーる', 'ハイヒール'),
			_Utils_Tuple2('はいる', '入る'),
			_Utils_Tuple2('はがき', '葉書'),
			_Utils_Tuple2('はく', '履く'),
			_Utils_Tuple2('はし', '箸'),
			_Utils_Tuple2('はし', '橋'),
			_Utils_Tuple2('はじまる', '始まる'),
			_Utils_Tuple2('はじめて', '初めて'),
			_Utils_Tuple2('はじめまして', '初めまして'),
			_Utils_Tuple2('はじめる', '始める'),
			_Utils_Tuple2('ばしょ', '場所'),
			_Utils_Tuple2('はしる', '走る'),
			_Utils_Tuple2('ばすけっとぼーる', 'バスケットボール'),
			_Utils_Tuple2('ぱすぽーと', 'パスポート'),
			_Utils_Tuple2('ぱそこん', 'パソコン'),
			_Utils_Tuple2('ばたー', 'バター'),
			_Utils_Tuple2('はたち', '二十歳'),
			_Utils_Tuple2('はたらく', '働く'),
			_Utils_Tuple2('はち', '八'),
			_Utils_Tuple2('はちがつ', '八月'),
			_Utils_Tuple2('はちじゅう', '八十'),
			_Utils_Tuple2('ばつ', 'ばつ'),
			_Utils_Tuple2('はつか', '二十日'),
			_Utils_Tuple2('はつか', '二十日'),
			_Utils_Tuple2('ばっぐ', 'バッグ'),
			_Utils_Tuple2('はな', '花'),
			_Utils_Tuple2('はな', '鼻'),
			_Utils_Tuple2('はなし', '話'),
			_Utils_Tuple2('はなす', '話す'),
			_Utils_Tuple2('ばなな', 'バナナ'),
			_Utils_Tuple2('はは', '母'),
			_Utils_Tuple2('ぱぱ', 'パパ'),
			_Utils_Tuple2('はぶらし', '歯ブラシ'),
			_Utils_Tuple2('はやい', '早い'),
			_Utils_Tuple2('はやい', '速い'),
			_Utils_Tuple2('はやく', '早く'),
			_Utils_Tuple2('はる', '春'),
			_Utils_Tuple2('はるやすみ', '春休み'),
			_Utils_Tuple2('はれ', '晴れ'),
			_Utils_Tuple2('ばれーぼーる', 'バレーボール'),
			_Utils_Tuple2('はれる', '晴れる'),
			_Utils_Tuple2('はん', '半'),
			_Utils_Tuple2('ばん', '番'),
			_Utils_Tuple2('ばん', '晩'),
			_Utils_Tuple2('ぱん', 'パン'),
			_Utils_Tuple2('はんかち', 'ハンカチ'),
			_Utils_Tuple2('ばんごう', '番号'),
			_Utils_Tuple2('ばんごはん', '晩御飯'),
			_Utils_Tuple2('ぱんだ', 'パンダ'),
			_Utils_Tuple2('ぱんつ', 'パンツ'),
			_Utils_Tuple2('はんとし', '半年'),
			_Utils_Tuple2('はんばーがー', 'ハンバーガー'),
			_Utils_Tuple2('はんばーぐ', 'ハンバーグ'),
			_Utils_Tuple2('はんぶん', '半分'),
			_Utils_Tuple2('ひ', '日'),
			_Utils_Tuple2('ひ', '火'),
			_Utils_Tuple2('ぴあの', 'ピアノ'),
			_Utils_Tuple2('びーる', 'ビール'),
			_Utils_Tuple2('ひがし', '東'),
			_Utils_Tuple2('ひき', '匹'),
			_Utils_Tuple2('ひく', '引く'),
			_Utils_Tuple2('ひく', '弾く'),
			_Utils_Tuple2('ひくい', '低い'),
			_Utils_Tuple2('ひこうき', '飛行機'),
			_Utils_Tuple2('びざ', 'ビザ'),
			_Utils_Tuple2('ぴざ', 'ピザ'),
			_Utils_Tuple2('ひだり', '左'),
			_Utils_Tuple2('ひだりあし', '左足'),
			_Utils_Tuple2('ひだりて', '左手'),
			_Utils_Tuple2('びでお', 'ビデオ'),
			_Utils_Tuple2('ひと', '人'),
			_Utils_Tuple2('ひと', '一'),
			_Utils_Tuple2('ひとつ', '一つ'),
			_Utils_Tuple2('ひとり', '一人'),
			_Utils_Tuple2('ひま', '暇'),
			_Utils_Tuple2('ひゃく', '百'),
			_Utils_Tuple2('ひゃくねん', '百年'),
			_Utils_Tuple2('ひゃくまん', '百万'),
			_Utils_Tuple2('びょういん', '病院'),
			_Utils_Tuple2('びよういん', '美容院'),
			_Utils_Tuple2('びょうき', '病気'),
			_Utils_Tuple2('ひらがな', '平仮名'),
			_Utils_Tuple2('ひる', '昼'),
			_Utils_Tuple2('ひるごはん', '昼御飯'),
			_Utils_Tuple2('ひるやすみ', '昼休み'),
			_Utils_Tuple2('ひろい', '広い'),
			_Utils_Tuple2('ぴんく', 'ピンク'),
			_Utils_Tuple2('ふァっくす', 'ファックス'),
			_Utils_Tuple2('ぷーる', 'プール'),
			_Utils_Tuple2('ふぉーく', 'フォーク'),
			_Utils_Tuple2('ふく', '服'),
			_Utils_Tuple2('ふた', '二'),
			_Utils_Tuple2('ぶた', '豚'),
			_Utils_Tuple2('ふたつ', '二つ'),
			_Utils_Tuple2('ぶたにく', '豚肉'),
			_Utils_Tuple2('ふたり', '二人'),
			_Utils_Tuple2('ふつか', '二日'),
			_Utils_Tuple2('ふとい', '太い'),
			_Utils_Tuple2('ふね', '船'),
			_Utils_Tuple2('ふべん', '不便'),
			_Utils_Tuple2('ふゆ', '冬'),
			_Utils_Tuple2('ふゆやすみ', '冬休み'),
			_Utils_Tuple2('ふらんすご', 'フランス語'),
			_Utils_Tuple2('ふらんすりょうり', 'フランス料理'),
			_Utils_Tuple2('ふる', '降る'),
			_Utils_Tuple2('ふるい', '古い'),
			_Utils_Tuple2('ぷれぜんと', 'プレゼント'),
			_Utils_Tuple2('ふろ', '風呂'),
			_Utils_Tuple2('ふん', '分'),
			_Utils_Tuple2('ぶん', '文'),
			_Utils_Tuple2('ぺーじ', 'ページ'),
			_Utils_Tuple2('ぺーじ', 'ページ'),
			_Utils_Tuple2('へた', '下手'),
			_Utils_Tuple2('べっど', 'ベッド'),
			_Utils_Tuple2('ぺっと', 'ペット'),
			_Utils_Tuple2('へや', '部屋'),
			_Utils_Tuple2('べると', 'ベルト'),
			_Utils_Tuple2('ぺん', 'ペン'),
			_Utils_Tuple2('べんきょう', '勉強'),
			_Utils_Tuple2('べんとう', '弁当'),
			_Utils_Tuple2('べんり', '便利'),
			_Utils_Tuple2('ぼうし', '帽子'),
			_Utils_Tuple2('ぼーるぺん', 'ボールペン'),
			_Utils_Tuple2('ぼく', '僕'),
			_Utils_Tuple2('ぽけっと', 'ポケット'),
			_Utils_Tuple2('ほしい', '欲しい'),
			_Utils_Tuple2('ぽすと', 'ポスト'),
			_Utils_Tuple2('ほそい', '細い'),
			_Utils_Tuple2('ぼたん', 'ボタン'),
			_Utils_Tuple2('ほっかいどう', '北海道'),
			_Utils_Tuple2('ぽてと', 'ポテト'),
			_Utils_Tuple2('ほてる', 'ホテル'),
			_Utils_Tuple2('ほん', '本'),
			_Utils_Tuple2('ほん', '本'),
			_Utils_Tuple2('ほん', '本'),
			_Utils_Tuple2('ほんだな', '本棚'),
			_Utils_Tuple2('ほんとう', '本当'),
			_Utils_Tuple2('ほんや', '本屋'),
			_Utils_Tuple2('まい', '枚'),
			_Utils_Tuple2('まい', '毎'),
			_Utils_Tuple2('まいあさ', '毎朝'),
			_Utils_Tuple2('まいく', 'マイク'),
			_Utils_Tuple2('まいしゅう', '毎週'),
			_Utils_Tuple2('まいつき', '毎月'),
			_Utils_Tuple2('まいとし', '毎年'),
			_Utils_Tuple2('まいにち', '毎日'),
			_Utils_Tuple2('まいばん', '毎晩'),
			_Utils_Tuple2('まえ', '前'),
			_Utils_Tuple2('まご', '孫'),
			_Utils_Tuple2('まじめ', 'まじめ'),
			_Utils_Tuple2('まだ', 'まだ'),
			_Utils_Tuple2('まち', '町'),
			_Utils_Tuple2('まつ', '待つ'),
			_Utils_Tuple2('まど', '窓'),
			_Utils_Tuple2('まま', 'ママ'),
			_Utils_Tuple2('まん', '万'),
			_Utils_Tuple2('まんが', '漫画'),
			_Utils_Tuple2('みえる', '見える'),
			_Utils_Tuple2('みがく', '磨く'),
			_Utils_Tuple2('みかん', 'みかん'),
			_Utils_Tuple2('みぎ', '右'),
			_Utils_Tuple2('みぎあし', '右足'),
			_Utils_Tuple2('みぎて', '右手'),
			_Utils_Tuple2('みじかい', '短い'),
			_Utils_Tuple2('みず', '水'),
			_Utils_Tuple2('みせ', '店'),
			_Utils_Tuple2('みせる', '見せる'),
			_Utils_Tuple2('みそしる', 'みそ汁'),
			_Utils_Tuple2('みち', '道'),
			_Utils_Tuple2('みっか', '三日'),
			_Utils_Tuple2('みっつ', '三つ'),
			_Utils_Tuple2('みどり', '緑'),
			_Utils_Tuple2('みどりいろ', '緑色'),
			_Utils_Tuple2('みなさん', '皆さん'),
			_Utils_Tuple2('みなみ', '南'),
			_Utils_Tuple2('みみ', '耳'),
			_Utils_Tuple2('みる', '見る'),
			_Utils_Tuple2('みるく', 'ミルク'),
			_Utils_Tuple2('む', '六'),
			_Utils_Tuple2('むいか', '六日'),
			_Utils_Tuple2('むずかしい', '難しい'),
			_Utils_Tuple2('むすこ', '息子'),
			_Utils_Tuple2('むすめ', '娘'),
			_Utils_Tuple2('むっつ', '六つ'),
			_Utils_Tuple2('め', '目'),
			_Utils_Tuple2('めーとる', 'メートル'),
			_Utils_Tuple2('めがね', 'めがね'),
			_Utils_Tuple2('もう', 'もう'),
			_Utils_Tuple2('もく', '木'),
			_Utils_Tuple2('もくよう', '木曜'),
			_Utils_Tuple2('もくようび', '木曜日'),
			_Utils_Tuple2('もし', 'もし'),
			_Utils_Tuple2('もしもし', 'もしもし'),
			_Utils_Tuple2('もつ', '持つ'),
			_Utils_Tuple2('もっと', 'もっと'),
			_Utils_Tuple2('もらう', 'もらう'),
			_Utils_Tuple2('もんだい', '問題'),
			_Utils_Tuple2('や', '屋'),
			_Utils_Tuple2('や', '八'),
			_Utils_Tuple2('やきそば', '焼きそば'),
			_Utils_Tuple2('やきにく', '焼肉'),
			_Utils_Tuple2('やきゅう', '野球'),
			_Utils_Tuple2('やさい', '野菜'),
			_Utils_Tuple2('やさしい', '優しい'),
			_Utils_Tuple2('やさしい', '易しい'),
			_Utils_Tuple2('やすい', '安い'),
			_Utils_Tuple2('やすみ', '休み'),
			_Utils_Tuple2('やすむ', '休む'),
			_Utils_Tuple2('やっつ', '八つ'),
			_Utils_Tuple2('やま', '山'),
			_Utils_Tuple2('ゆうがた', '夕方'),
			_Utils_Tuple2('ゆうしょく', '夕食'),
			_Utils_Tuple2('ゆうはん', '夕飯'),
			_Utils_Tuple2('ゆうびんきょく', '郵便局'),
			_Utils_Tuple2('ゆうめい', '有名'),
			_Utils_Tuple2('ゆうめい', '有名'),
			_Utils_Tuple2('ゆき', '雪'),
			_Utils_Tuple2('ゆび', '指'),
			_Utils_Tuple2('よい', '良い'),
			_Utils_Tuple2('よい', '良い'),
			_Utils_Tuple2('ようか', '八日'),
			_Utils_Tuple2('ようび', '曜日'),
			_Utils_Tuple2('ようふく', '洋服'),
			_Utils_Tuple2('よーろっぱ', 'ヨーロッパ'),
			_Utils_Tuple2('よく', '良く'),
			_Utils_Tuple2('よこ', '横'),
			_Utils_Tuple2('よっか', '四日'),
			_Utils_Tuple2('よっつ', '四つ'),
			_Utils_Tuple2('よぶ', '呼ぶ'),
			_Utils_Tuple2('よむ', '読む'),
			_Utils_Tuple2('よる', '夜'),
			_Utils_Tuple2('よろしく', 'よろしく'),
			_Utils_Tuple2('よわい', '弱い'),
			_Utils_Tuple2('よん', '四'),
			_Utils_Tuple2('らーめん', 'ラーメン'),
			_Utils_Tuple2('らいおん', 'ライオン'),
			_Utils_Tuple2('らいげつ', '来月'),
			_Utils_Tuple2('らいしゅう', '来週'),
			_Utils_Tuple2('らいねん', '来年'),
			_Utils_Tuple2('らじお', 'ラジオ'),
			_Utils_Tuple2('りゅうがく', '留学'),
			_Utils_Tuple2('りゅうがくせい', '留学生'),
			_Utils_Tuple2('りょうしん', '両親'),
			_Utils_Tuple2('りょうり', '料理'),
			_Utils_Tuple2('りょこう', '旅行'),
			_Utils_Tuple2('りんご', 'りんご'),
			_Utils_Tuple2('れい', '例'),
			_Utils_Tuple2('れいぞうこ', '冷蔵庫'),
			_Utils_Tuple2('れすとらん', 'レストラン'),
			_Utils_Tuple2('れもん', 'レモン'),
			_Utils_Tuple2('れんしゅう', '練習'),
			_Utils_Tuple2('ろく', '六'),
			_Utils_Tuple2('ろくがつ', '六月'),
			_Utils_Tuple2('ろくじ', '六時'),
			_Utils_Tuple2('ろくじゅう', '六十'),
			_Utils_Tuple2('ろしあご', 'ロシア語'),
			_Utils_Tuple2('わいん', 'ワイン'),
			_Utils_Tuple2('わかい', '若い'),
			_Utils_Tuple2('わかる', '分かる'),
			_Utils_Tuple2('わすれる', '忘れる'),
			_Utils_Tuple2('わたし', '私'),
			_Utils_Tuple2('わるい', '悪い'),
			_Utils_Tuple2('わんぴーす', 'ワンピース'),
			_Utils_Tuple2('あ', 'あ'),
			_Utils_Tuple2('ああ', 'ああ'),
			_Utils_Tuple2('あい', '愛'),
			_Utils_Tuple2('あいさつ', '挨拶'),
			_Utils_Tuple2('あいす', 'アイス'),
			_Utils_Tuple2('あいすくりーむ', 'アイスクリーム'),
			_Utils_Tuple2('あいすこーひー', 'アイスコーヒー'),
			_Utils_Tuple2('あいすすけーと', 'アイススケート'),
			_Utils_Tuple2('あいだ', '間'),
			_Utils_Tuple2('あいてぃー', 'ＩＴ'),
			_Utils_Tuple2('あいろん', 'アイロン'),
			_Utils_Tuple2('あう', '会う'),
			_Utils_Tuple2('あう', '合う'),
			_Utils_Tuple2('あお', '青'),
			_Utils_Tuple2('あおい', '青い'),
			_Utils_Tuple2('あおいろ', '青色'),
			_Utils_Tuple2('あおしんごう', '青信号'),
			_Utils_Tuple2('あか', '赤'),
			_Utils_Tuple2('あかい', '赤い'),
			_Utils_Tuple2('あかいろ', '赤色'),
			_Utils_Tuple2('あかしんごう', '赤信号'),
			_Utils_Tuple2('あかちゃん', '赤ちゃん'),
			_Utils_Tuple2('あがる', '上がる'),
			_Utils_Tuple2('あかるい', '明るい'),
			_Utils_Tuple2('あき', '秋'),
			_Utils_Tuple2('あく', '開く'),
			_Utils_Tuple2('あける', '開ける'),
			_Utils_Tuple2('あげる', '上げる'),
			_Utils_Tuple2('あさ', '朝'),
			_Utils_Tuple2('あさごはん', '朝御飯'),
			_Utils_Tuple2('あさって', 'あさって'),
			_Utils_Tuple2('あし', '足'),
			_Utils_Tuple2('あじ', '味'),
			_Utils_Tuple2('あじあ', 'アジア'),
			_Utils_Tuple2('あした', '明日'),
			_Utils_Tuple2('あそこ', 'あそこ'),
			_Utils_Tuple2('あそび', '遊び'),
			_Utils_Tuple2('あそぶ', '遊ぶ'),
			_Utils_Tuple2('あたたかい', '暖かい'),
			_Utils_Tuple2('あたたかい', '温かい'),
			_Utils_Tuple2('あたま', '頭'),
			_Utils_Tuple2('あたらしい', '新しい'),
			_Utils_Tuple2('あたり', '辺り'),
			_Utils_Tuple2('あちら', 'あちら'),
			_Utils_Tuple2('あっ', 'あっ'),
			_Utils_Tuple2('あつい', '暑い'),
			_Utils_Tuple2('あつい', '熱い'),
			_Utils_Tuple2('あつい', '厚い'),
			_Utils_Tuple2('あつまる', '集まる'),
			_Utils_Tuple2('あと', '後'),
			_Utils_Tuple2('あなた', 'あなた'),
			_Utils_Tuple2('あに', '兄'),
			_Utils_Tuple2('あにめ', 'アニメ'),
			_Utils_Tuple2('あにめーしょん', 'アニメーション'),
			_Utils_Tuple2('あね', '姉'),
			_Utils_Tuple2('あの', 'あの'),
			_Utils_Tuple2('あぱーと', 'アパート'),
			_Utils_Tuple2('あびる', '浴びる'),
			_Utils_Tuple2('あぶない', '危ない'),
			_Utils_Tuple2('あふりか', 'アフリカ'),
			_Utils_Tuple2('あまい', '甘い'),
			_Utils_Tuple2('あまり', '余り'),
			_Utils_Tuple2('あめ', '雨'),
			_Utils_Tuple2('あめ', 'あめ'),
			_Utils_Tuple2('あめりか', 'アメリカ'),
			_Utils_Tuple2('あめりかがっしゅうこく', 'アメリカ合衆国'),
			_Utils_Tuple2('あらう', '洗う'),
			_Utils_Tuple2('ありがとう', 'ありがとう'),
			_Utils_Tuple2('ある', 'ある'),
			_Utils_Tuple2('あるく', '歩く'),
			_Utils_Tuple2('あるばいと', 'アルバイト'),
			_Utils_Tuple2('あれ', 'あれ'),
			_Utils_Tuple2('あれ', 'あれ'),
			_Utils_Tuple2('あんしん', '安心'),
			_Utils_Tuple2('あんな', 'あんな'),
			_Utils_Tuple2('あんない', '案内'),
			_Utils_Tuple2('いいえ', 'いいえ'),
			_Utils_Tuple2('いう', '言う'),
			_Utils_Tuple2('いえ', '家'),
			_Utils_Tuple2('いえ', 'いえ'),
			_Utils_Tuple2('いき', '行き'),
			_Utils_Tuple2('いぎりす', 'イギリス'),
			_Utils_Tuple2('いきる', '生きる'),
			_Utils_Tuple2('いく', '行く'),
			_Utils_Tuple2('いくつ', '幾つ'),
			_Utils_Tuple2('いくら', '幾ら'),
			_Utils_Tuple2('いくら', '幾ら'),
			_Utils_Tuple2('いくら', '幾ら'),
			_Utils_Tuple2('いけ', '池'),
			_Utils_Tuple2('いざかや', '居酒屋'),
			_Utils_Tuple2('いしゃ', '医者'),
			_Utils_Tuple2('いす', 'いす'),
			_Utils_Tuple2('いそがしい', '忙しい'),
			_Utils_Tuple2('いたい', '痛い'),
			_Utils_Tuple2('いたりあ', 'イタリア'),
			_Utils_Tuple2('いち', '一'),
			_Utils_Tuple2('いちがつ', '一月'),
			_Utils_Tuple2('いちご', 'いちご'),
			_Utils_Tuple2('いちつ', '一つ'),
			_Utils_Tuple2('いちど', '一度'),
			_Utils_Tuple2('いちにち', '一日'),
			_Utils_Tuple2('いちねん', '一年'),
			_Utils_Tuple2('いちはい', '一杯'),
			_Utils_Tuple2('いちばん', '一番'),
			_Utils_Tuple2('いちばん', '一番'),
			_Utils_Tuple2('いちまい', '一枚'),
			_Utils_Tuple2('いつ', 'いつ'),
			_Utils_Tuple2('いつか', '五日'),
			_Utils_Tuple2('いっしょ', '一緒'),
			_Utils_Tuple2('いっしょうけんめい', '一生懸命'),
			_Utils_Tuple2('いつつ', '五つ'),
			_Utils_Tuple2('いつでも', '何時でも'),
			_Utils_Tuple2('いっぱい', '一杯'),
			_Utils_Tuple2('いっぱい', '一杯'),
			_Utils_Tuple2('いつまで', 'いつまで'),
			_Utils_Tuple2('いつも', 'いつも'),
			_Utils_Tuple2('いぬ', '犬'),
			_Utils_Tuple2('いま', '今'),
			_Utils_Tuple2('いみ', '意味'),
			_Utils_Tuple2('いもうと', '妹'),
			_Utils_Tuple2('いや', '嫌'),
			_Utils_Tuple2('いやりんぐ', 'イヤリング'),
			_Utils_Tuple2('いらっしゃいませ', 'いらっしゃいませ'),
			_Utils_Tuple2('いらっしゃる', 'いらっしゃる'),
			_Utils_Tuple2('いりぐち', '入り口'),
			_Utils_Tuple2('いる', 'いる'),
			_Utils_Tuple2('いる', '要る'),
			_Utils_Tuple2('いれる', '入れる'),
			_Utils_Tuple2('いろ', '色'),
			_Utils_Tuple2('いろいろ', '色々'),
			_Utils_Tuple2('いろいろ', '色々'),
			_Utils_Tuple2('いんたーねっと', 'インターネット'),
			_Utils_Tuple2('いんど', 'インド'),
			_Utils_Tuple2('いんどねしあ', 'インドネシア'),
			_Utils_Tuple2('ういすきー', 'ウイスキー'),
			_Utils_Tuple2('うーろんちゃ', 'ウーロン茶'),
			_Utils_Tuple2('ううん', 'ううん'),
			_Utils_Tuple2('うえ', '上'),
			_Utils_Tuple2('うさぎ', 'うさぎ'),
			_Utils_Tuple2('うし', '牛'),
			_Utils_Tuple2('うしろ', '後ろ'),
			_Utils_Tuple2('うすい', '薄い'),
			_Utils_Tuple2('うた', '歌'),
			_Utils_Tuple2('うたう', '歌う'),
			_Utils_Tuple2('うで', '腕'),
			_Utils_Tuple2('うでどけい', '腕時計'),
			_Utils_Tuple2('うどん', 'うどん'),
			_Utils_Tuple2('うま', '馬'),
			_Utils_Tuple2('うまい', 'うまい'),
			_Utils_Tuple2('うまれる', '生まれる'),
			_Utils_Tuple2('うみ', '海'),
			_Utils_Tuple2('うりば', '売り場'),
			_Utils_Tuple2('うる', '売る'),
			_Utils_Tuple2('うるさい', 'うるさい'),
			_Utils_Tuple2('うれしい', 'うれしい'),
			_Utils_Tuple2('うん', 'うん'),
			_Utils_Tuple2('うんうん', 'うんうん'),
			_Utils_Tuple2('うんどう', '運動'),
			_Utils_Tuple2('うんどうじょう', '運動場'),
			_Utils_Tuple2('え', '絵'),
			_Utils_Tuple2('え', 'え'),
			_Utils_Tuple2('えあこん', 'エアコン'),
			_Utils_Tuple2('えいが', '映画'),
			_Utils_Tuple2('えいがかん', '映画館'),
			_Utils_Tuple2('えいご', '英語'),
			_Utils_Tuple2('ええ', 'ええ'),
			_Utils_Tuple2('えーえむ', 'ＡＭ'),
			_Utils_Tuple2('ええと', 'ええと'),
			_Utils_Tuple2('えき', '駅'),
			_Utils_Tuple2('えきいん', '駅員'),
			_Utils_Tuple2('えじぷと', 'エジプト'),
			_Utils_Tuple2('えすかれーたー', 'エスカレーター'),
			_Utils_Tuple2('えすさいず', 'Ｓサイズ'),
			_Utils_Tuple2('えっ', 'えっ'),
			_Utils_Tuple2('えらぶ', '選ぶ'),
			_Utils_Tuple2('えれべーた', 'エレベーター'),
			_Utils_Tuple2('えれべーたー', 'エレベーター'),
			_Utils_Tuple2('えん', '円'),
			_Utils_Tuple2('えんぴつ', '鉛筆'),
			_Utils_Tuple2('おい', 'おい'),
			_Utils_Tuple2('おいしい', 'おいしい'),
			_Utils_Tuple2('おおい', '多い'),
			_Utils_Tuple2('おおきい', '大きい'),
			_Utils_Tuple2('おおきな', '大きな'),
			_Utils_Tuple2('おおさか', '大阪'),
			_Utils_Tuple2('おーすとらりあ', 'オーストラリア'),
			_Utils_Tuple2('おーとばい', 'オートバイ'),
			_Utils_Tuple2('おかね', 'お金'),
			_Utils_Tuple2('おきゃくさん', 'お客さん'),
			_Utils_Tuple2('おきる', '起きる'),
			_Utils_Tuple2('おく', '置く'),
			_Utils_Tuple2('おく', '奥'),
			_Utils_Tuple2('おく', '億'),
			_Utils_Tuple2('おくさま', '奥様'),
			_Utils_Tuple2('おくさん', '奥さん'),
			_Utils_Tuple2('おくる', '送る'),
			_Utils_Tuple2('おくれる', '遅れる'),
			_Utils_Tuple2('おじ', '伯父'),
			_Utils_Tuple2('おじいさん', 'おじいさん'),
			_Utils_Tuple2('おじいちゃん', 'おじいちゃん'),
			_Utils_Tuple2('おしえる', '教える'),
			_Utils_Tuple2('おじさん', '伯父さん'),
			_Utils_Tuple2('おす', '押す'),
			_Utils_Tuple2('おそい', '遅い'),
			_Utils_Tuple2('おちる', '落ちる'),
			_Utils_Tuple2('おっと', '夫'),
			_Utils_Tuple2('おてあらい', 'お手洗い'),
			_Utils_Tuple2('おと', '音'),
			_Utils_Tuple2('おとうさん', 'お父さん'),
			_Utils_Tuple2('おとうと', '弟'),
			_Utils_Tuple2('おとこ', '男'),
			_Utils_Tuple2('おとこのこ', '男の子'),
			_Utils_Tuple2('おとこのひと', '男の人'),
			_Utils_Tuple2('おとす', '落とす'),
			_Utils_Tuple2('おととい', 'おととい'),
			_Utils_Tuple2('おととし', 'おととし'),
			_Utils_Tuple2('おとな', '大人'),
			_Utils_Tuple2('おなか', 'おなか'),
			_Utils_Tuple2('おなじ', '同じ'),
			_Utils_Tuple2('おなじ', '同じ'),
			_Utils_Tuple2('おにいさん', 'お兄さん'),
			_Utils_Tuple2('おにぎり', 'お握り'),
			_Utils_Tuple2('おねえさん', 'お姉さん'),
			_Utils_Tuple2('おねがい', 'おねがい'),
			_Utils_Tuple2('おねがいします', 'お願いします'),
			_Utils_Tuple2('おば', '伯母'),
			_Utils_Tuple2('おばあさん', 'おばあさん'),
			_Utils_Tuple2('おばあちゃん', 'おばあちゃん'),
			_Utils_Tuple2('おばさん', '伯母さん'),
			_Utils_Tuple2('おはよう', 'おはよう'),
			_Utils_Tuple2('おはようございます', 'おはようございます'),
			_Utils_Tuple2('おぼえる', '覚える'),
			_Utils_Tuple2('おまたせしました', 'お待たせしました'),
			_Utils_Tuple2('おもい', '重い'),
			_Utils_Tuple2('おもう', '思う'),
			_Utils_Tuple2('おもしろい', '面白い'),
			_Utils_Tuple2('おや', '親'),
			_Utils_Tuple2('おやすみ', 'お休み'),
			_Utils_Tuple2('およぐ', '泳ぐ'),
			_Utils_Tuple2('おりる', '降りる'),
			_Utils_Tuple2('おりる', '下りる'),
			_Utils_Tuple2('おる', 'おる'),
			_Utils_Tuple2('おれんじ', 'オレンジ'),
			_Utils_Tuple2('おわり', '終わり'),
			_Utils_Tuple2('おわる', '終わる'),
			_Utils_Tuple2('おんがく', '音楽'),
			_Utils_Tuple2('おんせん', '温泉'),
			_Utils_Tuple2('おんな', '女'),
			_Utils_Tuple2('おんなのこ', '女の子'),
			_Utils_Tuple2('おんなのひと', '女の人'),
			_Utils_Tuple2('おんよみ', '音読み'),
			_Utils_Tuple2('か', '課'),
			_Utils_Tuple2('か', '日'),
			_Utils_Tuple2('か', '家'),
			_Utils_Tuple2('か', '火'),
			_Utils_Tuple2('か', '歌'),
			_Utils_Tuple2('が', 'が'),
			_Utils_Tuple2('かーど', 'カード'),
			_Utils_Tuple2('かい', '回'),
			_Utils_Tuple2('かい', '階'),
			_Utils_Tuple2('かい', '海'),
			_Utils_Tuple2('かい', '買い'),
			_Utils_Tuple2('かい', '回'),
			_Utils_Tuple2('がいこく', '外国'),
			_Utils_Tuple2('がいこくご', '外国語'),
			_Utils_Tuple2('がいこくじん', '外国人'),
			_Utils_Tuple2('かいしゃ', '会社'),
			_Utils_Tuple2('かいしゃいん', '会社員'),
			_Utils_Tuple2('かいだん', '階段'),
			_Utils_Tuple2('かいもの', '買い物'),
			_Utils_Tuple2('かいわ', '会話'),
			_Utils_Tuple2('かう', '買う'),
			_Utils_Tuple2('かう', '飼う'),
			_Utils_Tuple2('かえす', '返す'),
			_Utils_Tuple2('かえり', '帰り'),
			_Utils_Tuple2('かえる', '帰る'),
			_Utils_Tuple2('かえる', '変える'),
			_Utils_Tuple2('かお', '顔'),
			_Utils_Tuple2('かがみ', '鏡'),
			_Utils_Tuple2('かぎ', 'かぎ'),
			_Utils_Tuple2('かく', '書く'),
			_Utils_Tuple2('かぐ', '家具'),
			_Utils_Tuple2('がくせい', '学生'),
			_Utils_Tuple2('がくひ', '学費'),
			_Utils_Tuple2('かげつ', '箇月'),
			_Utils_Tuple2('かさ', '傘'),
			_Utils_Tuple2('かし', '菓子'),
			_Utils_Tuple2('かじ', '火事'),
			_Utils_Tuple2('かしゅ', '歌手'),
			_Utils_Tuple2('かす', '貸す'),
			_Utils_Tuple2('かず', '数'),
			_Utils_Tuple2('かぜ', '風邪'),
			_Utils_Tuple2('かぜ', '風'),
			_Utils_Tuple2('かぜぐすり', '風邪薬'),
			_Utils_Tuple2('かぞく', '家族'),
			_Utils_Tuple2('がそりんすたんど', 'ガソリンスタンド'),
			_Utils_Tuple2('かた', '肩'),
			_Utils_Tuple2('かたかな', '片仮名'),
			_Utils_Tuple2('がつ', '月'),
			_Utils_Tuple2('がっき', '学期'),
			_Utils_Tuple2('がっこう', '学校'),
			_Utils_Tuple2('かっぷ', 'カップ'),
			_Utils_Tuple2('かど', '角'),
			_Utils_Tuple2('かなしい', '悲しい'),
			_Utils_Tuple2('かなだ', 'カナダ'),
			_Utils_Tuple2('かね', '金'),
			_Utils_Tuple2('かねもち', '金持ち'),
			_Utils_Tuple2('かのじょ', '彼女'),
			_Utils_Tuple2('かばん', 'かばん'),
			_Utils_Tuple2('かふぇ', 'カフェ'),
			_Utils_Tuple2('かべ', '壁'),
			_Utils_Tuple2('かみ', '紙'),
			_Utils_Tuple2('かみ', '神'),
			_Utils_Tuple2('かみ', '髪'),
			_Utils_Tuple2('かみさま', '神様'),
			_Utils_Tuple2('かみのけ', '髪の毛'),
			_Utils_Tuple2('がむ', 'ガム'),
			_Utils_Tuple2('かめら', 'カメラ'),
			_Utils_Tuple2('かめらまん', 'カメラマン'),
			_Utils_Tuple2('かよう', '火曜'),
			_Utils_Tuple2('かようび', '火曜日'),
			_Utils_Tuple2('からい', '辛い'),
			_Utils_Tuple2('からおけ', 'カラオケ'),
			_Utils_Tuple2('がらす', 'ガラス'),
			_Utils_Tuple2('からだ', '体'),
			_Utils_Tuple2('かりる', '借りる'),
			_Utils_Tuple2('かるい', '軽い'),
			_Utils_Tuple2('かれ', '彼'),
			_Utils_Tuple2('かれー', 'カレー'),
			_Utils_Tuple2('かれーらいす', 'カレーライス'),
			_Utils_Tuple2('かれし', '彼氏'),
			_Utils_Tuple2('かれんだー', 'カレンダー'),
			_Utils_Tuple2('かわ', '川'),
			_Utils_Tuple2('かわいい', 'かわいい'),
			_Utils_Tuple2('かわいそう', 'かわいそう'),
			_Utils_Tuple2('かわく', '渇く'),
			_Utils_Tuple2('かわる', '変わる'),
			_Utils_Tuple2('かん', '館'),
			_Utils_Tuple2('かんがえる', '考える'),
			_Utils_Tuple2('かんこく', '韓国'),
			_Utils_Tuple2('かんじ', '漢字'),
			_Utils_Tuple2('かんたん', '簡単'),
			_Utils_Tuple2('がんばる', '頑張る'),
			_Utils_Tuple2('き', '木'),
			_Utils_Tuple2('き', '機'),
			_Utils_Tuple2('きいろ', '黄色'),
			_Utils_Tuple2('きいろい', '黄色い'),
			_Utils_Tuple2('きえる', '消える'),
			_Utils_Tuple2('きく', '聞く'),
			_Utils_Tuple2('きけん', '危険'),
			_Utils_Tuple2('きこえる', '聞こえる'),
			_Utils_Tuple2('きこく', '帰国'),
			_Utils_Tuple2('きす', 'キス'),
			_Utils_Tuple2('きた', '北'),
			_Utils_Tuple2('ぎたー', 'ギター'),
			_Utils_Tuple2('きたあめりか', '北アメリカ'),
			_Utils_Tuple2('きたない', '汚い'),
			_Utils_Tuple2('きっさてん', '喫茶店'),
			_Utils_Tuple2('きって', '切手'),
			_Utils_Tuple2('きっぷ', '切符'),
			_Utils_Tuple2('きのう', '昨日'),
			_Utils_Tuple2('きびしい', '厳しい'),
			_Utils_Tuple2('きぶん', '気分'),
			_Utils_Tuple2('きまる', '決まる'),
			_Utils_Tuple2('きむち', 'キムチ'),
			_Utils_Tuple2('きめる', '決める'),
			_Utils_Tuple2('きもち', '気持ち'),
			_Utils_Tuple2('きもちいい', '気持ち良い'),
			_Utils_Tuple2('きゃく', '客'),
			_Utils_Tuple2('きゃっしゅかーど', 'キャッシュカード'),
			_Utils_Tuple2('きゃべつ', 'キャベツ'),
			_Utils_Tuple2('きゅう', '九'),
			_Utils_Tuple2('きゅう', '九'),
			_Utils_Tuple2('きゅうじつ', '休日'),
			_Utils_Tuple2('きゅうしゅう', '九州'),
			_Utils_Tuple2('ぎゅうどん', '牛丼'),
			_Utils_Tuple2('ぎゅうにく', '牛肉'),
			_Utils_Tuple2('ぎゅうにゅう', '牛乳'),
			_Utils_Tuple2('きゅうり', 'きゅうり'),
			_Utils_Tuple2('きょう', '今日'),
			_Utils_Tuple2('きょうかい', '教会'),
			_Utils_Tuple2('きょうかしょ', '教科書'),
			_Utils_Tuple2('きょうし', '教師'),
			_Utils_Tuple2('きょうしつ', '教室'),
			_Utils_Tuple2('きょうだい', '兄弟'),
			_Utils_Tuple2('きょく', '局'),
			_Utils_Tuple2('きょねん', '去年'),
			_Utils_Tuple2('きらい', '嫌い'),
			_Utils_Tuple2('きる', '着る'),
			_Utils_Tuple2('きる', '切る'),
			_Utils_Tuple2('きれい', 'きれい'),
			_Utils_Tuple2('きろ', 'キロ'),
			_Utils_Tuple2('きろ', 'キロ'),
			_Utils_Tuple2('きろぐらむ', 'キログラム'),
			_Utils_Tuple2('きろぐらむ', 'キログラム'),
			_Utils_Tuple2('きろめーとる', 'キロメートル'),
			_Utils_Tuple2('きん', '金'),
			_Utils_Tuple2('ぎんこう', '銀行'),
			_Utils_Tuple2('ぎんざ', '銀座'),
			_Utils_Tuple2('きんよう', '金曜'),
			_Utils_Tuple2('きんようび', '金曜日'),
			_Utils_Tuple2('く', '区'),
			_Utils_Tuple2('く', '区'),
			_Utils_Tuple2('くうこう', '空港'),
			_Utils_Tuple2('くがつ', '九月'),
			_Utils_Tuple2('くすり', '薬'),
			_Utils_Tuple2('くすりゆび', '薬指'),
			_Utils_Tuple2('くだもの', '果物'),
			_Utils_Tuple2('くち', '口'),
			_Utils_Tuple2('くつ', '靴'),
			_Utils_Tuple2('くっきー', 'クッキー'),
			_Utils_Tuple2('くつした', '靴下'),
			_Utils_Tuple2('くに', '国'),
			_Utils_Tuple2('くび', '首'),
			_Utils_Tuple2('くみ', '組'),
			_Utils_Tuple2('くも', '雲'),
			_Utils_Tuple2('くもり', '曇り'),
			_Utils_Tuple2('くもる', '曇る'),
			_Utils_Tuple2('くやくしょ', '区役所'),
			_Utils_Tuple2('くらい', '暗い'),
			_Utils_Tuple2('くらす', 'クラス'),
			_Utils_Tuple2('くらすめーと', 'クラスメート'),
			_Utils_Tuple2('くらぶ', 'クラブ'),
			_Utils_Tuple2('ぐらむ', 'グラム'),
			_Utils_Tuple2('ぐらむ', 'グラム'),
			_Utils_Tuple2('くりすます', 'クリスマス'),
			_Utils_Tuple2('くりすますいぶ', 'クリスマスイブ'),
			_Utils_Tuple2('くりすますかーど', 'クリスマスカード'),
			_Utils_Tuple2('くりすますけーき', 'クリスマスケーキ'),
			_Utils_Tuple2('くりすますぷれぜんと', 'クリスマスプレゼント'),
			_Utils_Tuple2('くる', '来る'),
			_Utils_Tuple2('ぐるーぷ', 'グループ'),
			_Utils_Tuple2('くるま', '車'),
			_Utils_Tuple2('ぐれーぷふるーつ', 'グレープフルーツ'),
			_Utils_Tuple2('くれる', 'くれる'),
			_Utils_Tuple2('くろ', '黒'),
			_Utils_Tuple2('くろい', '黒い'),
			_Utils_Tuple2('くん', '君'),
			_Utils_Tuple2('け', '毛'),
			_Utils_Tuple2('けいさつ', '警察'),
			_Utils_Tuple2('けいたいでんわ', '携帯電話'),
			_Utils_Tuple2('けいようし', '形容詞'),
			_Utils_Tuple2('けーき', 'ケーキ'),
			_Utils_Tuple2('げーむ', 'ゲーム'),
			_Utils_Tuple2('けさ', '今朝'),
			_Utils_Tuple2('けす', '消す'),
			_Utils_Tuple2('けちゃっぷ', 'ケチャップ'),
			_Utils_Tuple2('げつ', '月'),
			_Utils_Tuple2('けっこん', '結婚'),
			_Utils_Tuple2('けっこんしき', '結婚式'),
			_Utils_Tuple2('けっせき', '欠席'),
			_Utils_Tuple2('げつよう', '月曜'),
			_Utils_Tuple2('げつようひ', '月曜日'),
			_Utils_Tuple2('げんき', '元気'),
			_Utils_Tuple2('こ', '個'),
			_Utils_Tuple2('こ', '子'),
			_Utils_Tuple2('ご', '五'),
			_Utils_Tuple2('ご', '五'),
			_Utils_Tuple2('ご', '後'),
			_Utils_Tuple2('こいぬ', '子犬'),
			_Utils_Tuple2('こいびと', '恋人'),
			_Utils_Tuple2('こう', 'こう'),
			_Utils_Tuple2('こう', '校'),
			_Utils_Tuple2('こうえん', '公園'),
			_Utils_Tuple2('こうこう', '高校'),
			_Utils_Tuple2('こうこうせい', '高校生'),
			_Utils_Tuple2('こうちゃ', '紅茶'),
			_Utils_Tuple2('こうちょう', '校長'),
			_Utils_Tuple2('こうつう', '交通'),
			_Utils_Tuple2('こうつうじこ', '交通事故'),
			_Utils_Tuple2('こうばん', '交番'),
			_Utils_Tuple2('こえ', '声'),
			_Utils_Tuple2('こーと', 'コート'),
			_Utils_Tuple2('こーひー', 'コーヒー'),
			_Utils_Tuple2('こーら', 'コーラ'),
			_Utils_Tuple2('こおり', '氷'),
			_Utils_Tuple2('ごがつ', '五月'),
			_Utils_Tuple2('こく', '国'),
			_Utils_Tuple2('ごくろうさま', '御苦労様'),
			_Utils_Tuple2('ここ', 'ここ'),
			_Utils_Tuple2('ごご', '午後'),
			_Utils_Tuple2('ここあ', 'ココア'),
			_Utils_Tuple2('ここのか', '九日'),
			_Utils_Tuple2('ここのつ', '九つ'),
			_Utils_Tuple2('こころ', '心'),
			_Utils_Tuple2('ごぜん', '午前'),
			_Utils_Tuple2('こたえ', '答え'),
			_Utils_Tuple2('こたえる', '答える'),
			_Utils_Tuple2('ごちそうさま', '御馳走様'),
			_Utils_Tuple2('こちら', 'こちら'),
			_Utils_Tuple2('こっぷ', 'コップ'),
			_Utils_Tuple2('こと', '事'),
			_Utils_Tuple2('こと', '事　'),
			_Utils_Tuple2('ことし', '今年'),
			_Utils_Tuple2('ことば', '言葉'),
			_Utils_Tuple2('こども', '子供'),
			_Utils_Tuple2('この', 'この'),
			_Utils_Tuple2('このごろ', 'このごろ'),
			_Utils_Tuple2('ごはん', '御飯'),
			_Utils_Tuple2('こぴー', 'コピー'),
			_Utils_Tuple2('こまる', '困る'),
			_Utils_Tuple2('ごみ', 'ごみ'),
			_Utils_Tuple2('こめ', '米'),
			_Utils_Tuple2('ごめん', 'ごめん'),
			_Utils_Tuple2('こゆび', '小指'),
			_Utils_Tuple2('ごるふ', 'ゴルフ'),
			_Utils_Tuple2('これ', 'これ'),
			_Utils_Tuple2('これから', 'これから'),
			_Utils_Tuple2('これら', 'これら'),
			_Utils_Tuple2('ころ', 'ころ'),
			_Utils_Tuple2('こわい', '怖い'),
			_Utils_Tuple2('こわす', '壊す'),
			_Utils_Tuple2('こんげつ', '今月'),
			_Utils_Tuple2('こんさーと', 'コンサート'),
			_Utils_Tuple2('こんしゅう', '今週'),
			_Utils_Tuple2('こんたくとれんず', 'コンタクトレンズ'),
			_Utils_Tuple2('こんど', '今度'),
			_Utils_Tuple2('こんな', 'こんな'),
			_Utils_Tuple2('こんな', 'こんな'),
			_Utils_Tuple2('こんにちは', 'こんにちは'),
			_Utils_Tuple2('こんばん', '今晩'),
			_Utils_Tuple2('こんばんは', 'こんばんは'),
			_Utils_Tuple2('こんびに', 'コンビニ'),
			_Utils_Tuple2('こんぴゅーたー', 'コンピューター'),
			_Utils_Tuple2('こんや', '今夜'),
			_Utils_Tuple2('さ', 'さ'),
			_Utils_Tuple2('さい', '歳'),
			_Utils_Tuple2('さいきん', '最近'),
			_Utils_Tuple2('さいご', '最後'),
			_Utils_Tuple2('さいふ', '財布'),
			_Utils_Tuple2('さかな', '魚'),
			_Utils_Tuple2('さくぶん', '作文'),
			_Utils_Tuple2('さくら', '桜'),
			_Utils_Tuple2('さけ', '酒'),
			_Utils_Tuple2('さけ', 'さけ'),
			_Utils_Tuple2('さしみ', '刺し身'),
			_Utils_Tuple2('さつ', '冊'),
			_Utils_Tuple2('さっかー', 'サッカー'),
			_Utils_Tuple2('ざっし', '雑誌'),
			_Utils_Tuple2('さとう', '砂糖'),
			_Utils_Tuple2('さま', '様'),
			_Utils_Tuple2('さむい', '寒い'),
			_Utils_Tuple2('さようなら', 'さようなら'),
			_Utils_Tuple2('さよなら', 'さよなら'),
			_Utils_Tuple2('さら', '皿'),
			_Utils_Tuple2('さらいねん', '再来年'),
			_Utils_Tuple2('さらだ', 'サラダ'),
			_Utils_Tuple2('さる', '猿'),
			_Utils_Tuple2('さん', '三'),
			_Utils_Tuple2('さん', 'さん'),
			_Utils_Tuple2('さん', '山'),
			_Utils_Tuple2('さんがつ', '三月'),
			_Utils_Tuple2('さんぐらす', 'サングラス'),
			_Utils_Tuple2('さんじ', '三時'),
			_Utils_Tuple2('さんど', '三度'),
			_Utils_Tuple2('さんどいっち', 'サンドイッチ'),
			_Utils_Tuple2('ざんねん', '残念'),
			_Utils_Tuple2('ざんねん', '残念'),
			_Utils_Tuple2('さんぽ', '散歩'),
			_Utils_Tuple2('し', '四'),
			_Utils_Tuple2('じ', '時'),
			_Utils_Tuple2('じ', '時'),
			_Utils_Tuple2('じ', '時'),
			_Utils_Tuple2('じいさん', 'じいさん'),
			_Utils_Tuple2('しーでぃー', 'ＣＤ'),
			_Utils_Tuple2('じーんず', 'ジーンズ'),
			_Utils_Tuple2('じぇーあーる', 'ＪＲ'),
			_Utils_Tuple2('しお', '塩'),
			_Utils_Tuple2('しかし', 'しかし'),
			_Utils_Tuple2('しがつ', '四月'),
			_Utils_Tuple2('じかん', '時間'),
			_Utils_Tuple2('じかん', '時間'),
			_Utils_Tuple2('しけん', '試験'),
			_Utils_Tuple2('じこ', '事故'),
			_Utils_Tuple2('じこしょうかい', '自己紹介'),
			_Utils_Tuple2('しごと', '仕事'),
			_Utils_Tuple2('じしょ', '辞書'),
			_Utils_Tuple2('しずか', '静か'),
			_Utils_Tuple2('した', '下'),
			_Utils_Tuple2('しち', '七'),
			_Utils_Tuple2('しちがつ', '七月'),
			_Utils_Tuple2('じつ', '日'),
			_Utils_Tuple2('しつもん', '質問'),
			_Utils_Tuple2('しつれい', '失礼'),
			_Utils_Tuple2('じてんしゃ', '自転車'),
			_Utils_Tuple2('じどうしゃ', '自動車'),
			_Utils_Tuple2('しぬ', '死ぬ'),
			_Utils_Tuple2('じぶん', '自分'),
			_Utils_Tuple2('しまる', '閉まる'),
			_Utils_Tuple2('しめる', '閉める'),
			_Utils_Tuple2('しゃ', '車'),
			_Utils_Tuple2('しゃ', '社'),
			_Utils_Tuple2('じゃ', 'じゃ'),
			_Utils_Tuple2('しゃーぺん', 'シャーペン'),
			_Utils_Tuple2('しゃいん', '社員'),
			_Utils_Tuple2('しゃかい', '社会'),
			_Utils_Tuple2('じゃがいも', 'ジャガ芋'),
			_Utils_Tuple2('しゃしん', '写真'),
			_Utils_Tuple2('じゃず', 'ジャズ'),
			_Utils_Tuple2('しゃちょう', '社長'),
			_Utils_Tuple2('しゃつ', 'シャツ'),
			_Utils_Tuple2('じゃむ', 'ジャム'),
			_Utils_Tuple2('しゃわー', 'シャワー'),
			_Utils_Tuple2('じゃんけん', 'じゃんけん'),
			_Utils_Tuple2('しゃんぷー', 'シャンプー'),
			_Utils_Tuple2('しゅ', '酒'),
			_Utils_Tuple2('しゅう', '週'),
			_Utils_Tuple2('しゅう', '週'),
			_Utils_Tuple2('じゅう', '十'),
			_Utils_Tuple2('じゅういちがつ', '十一月'),
			_Utils_Tuple2('じゅうがつ', '十月'),
			_Utils_Tuple2('しゅうかん', '週間'),
			_Utils_Tuple2('じゅうしょ', '住所'),
			_Utils_Tuple2('じゅーす', 'ジュース'),
			_Utils_Tuple2('じゅうどう', '柔道'),
			_Utils_Tuple2('じゅうにがつ', '十二月'),
			_Utils_Tuple2('しゅうまつ', '週末'),
			_Utils_Tuple2('じゅうろく', '十六'),
			_Utils_Tuple2('じゅぎょう', '授業'),
			_Utils_Tuple2('しゅくだい', '宿題'),
			_Utils_Tuple2('しゅじん', '主人'),
			_Utils_Tuple2('しゅみ', '趣味'),
			_Utils_Tuple2('じょ', '女'),
			_Utils_Tuple2('じょ', '女'),
			_Utils_Tuple2('しょうかい', '紹介'),
			_Utils_Tuple2('しょうがつ', '正月'),
			_Utils_Tuple2('じょうず', '上手'),
			_Utils_Tuple2('じょうず', '上手'),
			_Utils_Tuple2('しょうねん', '少年'),
			_Utils_Tuple2('しょうゆ', 'しょうゆ'),
			_Utils_Tuple2('しょきゅう', '初級'),
			_Utils_Tuple2('しょくじ', '食事'),
			_Utils_Tuple2('しょくどう', '食堂'),
			_Utils_Tuple2('じょし', '女子'),
			_Utils_Tuple2('じょせい', '女性'),
			_Utils_Tuple2('しょっぴんぐ', 'ショッピング'),
			_Utils_Tuple2('しらべる', '調べる'),
			_Utils_Tuple2('しる', '知る'),
			_Utils_Tuple2('しろ', '白'),
			_Utils_Tuple2('しろい', '白い'),
			_Utils_Tuple2('じん', '人'),
			_Utils_Tuple2('しんかんせん', '新幹線'),
			_Utils_Tuple2('しんごう', '信号'),
			_Utils_Tuple2('じんこう', '人口'),
			_Utils_Tuple2('じんじゃ', '神社'),
			_Utils_Tuple2('しんせき', '親戚'),
			_Utils_Tuple2('しんせつ', '親切'),
			_Utils_Tuple2('しんぱい', '心配'),
			_Utils_Tuple2('しんぶん', '新聞'),
			_Utils_Tuple2('しんぶんし', '新聞紙'),
			_Utils_Tuple2('すい', '水'),
			_Utils_Tuple2('すいえい', '水泳'),
			_Utils_Tuple2('すいか', 'すいか'),
			_Utils_Tuple2('すいよう', '水曜'),
			_Utils_Tuple2('すいようび', '水曜日'),
			_Utils_Tuple2('すう', '吸う'),
			_Utils_Tuple2('すーつ', 'スーツ'),
			_Utils_Tuple2('すーつけーす', 'スーツケース'),
			_Utils_Tuple2('すーぱーまーけっと', 'スーパーマーケット'),
			_Utils_Tuple2('すーぷ', 'スープ'),
			_Utils_Tuple2('すかーと', 'スカート'),
			_Utils_Tuple2('すき', '好き'),
			_Utils_Tuple2('すき', '好き'),
			_Utils_Tuple2('すきー', 'スキー'),
			_Utils_Tuple2('すきやき', 'すき焼き'),
			_Utils_Tuple2('すく', 'すく'),
			_Utils_Tuple2('すぐ', 'すぐ'),
			_Utils_Tuple2('すくない', '少ない'),
			_Utils_Tuple2('すぐに', 'すぐに'),
			_Utils_Tuple2('すけーと', 'スケート'),
			_Utils_Tuple2('すこし', '少し'),
			_Utils_Tuple2('すし', 'すし'),
			_Utils_Tuple2('すずしい', '涼しい'),
			_Utils_Tuple2('すすむ', '進む'),
			_Utils_Tuple2('ずっと', 'ずっと'),
			_Utils_Tuple2('すてーき', 'ステーキ'),
			_Utils_Tuple2('すてる', '捨てる'),
			_Utils_Tuple2('すとーぶ', 'ストーブ'),
			_Utils_Tuple2('すぱげってぃ', 'スパゲッティ'),
			_Utils_Tuple2('すばらしい', '素晴らしい'),
			_Utils_Tuple2('すぷーん', 'スプーン'),
			_Utils_Tuple2('すぽーつ', 'スポーツ'),
			_Utils_Tuple2('ずぼん', 'ズボン'),
			_Utils_Tuple2('すみません', '済みません'),
			_Utils_Tuple2('すむ', '住む'),
			_Utils_Tuple2('すりっぱ', 'スリッパ'),
			_Utils_Tuple2('する', 'する'),
			_Utils_Tuple2('すわる', '座る'),
			_Utils_Tuple2('せ', '背'),
			_Utils_Tuple2('せいかつ', '生活'),
			_Utils_Tuple2('せいと', '生徒'),
			_Utils_Tuple2('せいねんつきひ', '生年月日'),
			_Utils_Tuple2('せーたー', 'セーター'),
			_Utils_Tuple2('せかい', '世界'),
			_Utils_Tuple2('せき', '席'),
			_Utils_Tuple2('せき', 'せき'),
			_Utils_Tuple2('せっけん', 'せっけん'),
			_Utils_Tuple2('せなか', '背中'),
			_Utils_Tuple2('せぶん', 'セブン'),
			_Utils_Tuple2('せまい', '狭い'),
			_Utils_Tuple2('ぜろ', 'ゼロ'),
			_Utils_Tuple2('せろてーぷ', 'セロテープ'),
			_Utils_Tuple2('せん', '千'),
			_Utils_Tuple2('せん', '線'),
			_Utils_Tuple2('せんげつ', '先月'),
			_Utils_Tuple2('せんしゅう', '先週'),
			_Utils_Tuple2('せんせい', '先生'),
			_Utils_Tuple2('ぜんぜん', '全然'),
			_Utils_Tuple2('せんたく', '洗濯'),
			_Utils_Tuple2('せんたくき', '洗濯機'),
			_Utils_Tuple2('せんち', 'センチ'),
			_Utils_Tuple2('せんち', 'センチ'),
			_Utils_Tuple2('せんちめーとる', 'センチメートル'),
			_Utils_Tuple2('ぜんぶ', '全部'),
			_Utils_Tuple2('ぞう', '象'),
			_Utils_Tuple2('そうじ', '掃除'),
			_Utils_Tuple2('そうじき', '掃除機'),
			_Utils_Tuple2('そく', '足'),
			_Utils_Tuple2('そこ', 'そこ'),
			_Utils_Tuple2('そして', 'そして'),
			_Utils_Tuple2('そちら', 'そちら'),
			_Utils_Tuple2('そと', '外'),
			_Utils_Tuple2('その', 'その'),
			_Utils_Tuple2('その', 'その'),
			_Utils_Tuple2('そば', '側'),
			_Utils_Tuple2('そば', 'そば'),
			_Utils_Tuple2('そふ', '祖父'),
			_Utils_Tuple2('そふァー', 'ソファー'),
			_Utils_Tuple2('そぼ', '祖母'),
			_Utils_Tuple2('そら', '空'),
			_Utils_Tuple2('それ', 'それ'),
			_Utils_Tuple2('それから', 'それから'),
			_Utils_Tuple2('それは', 'それは'),
			_Utils_Tuple2('だい', '台'),
			_Utils_Tuple2('だいがく', '大学'),
			_Utils_Tuple2('だいがくいん', '大学院'),
			_Utils_Tuple2('だいがくせい', '大学生'),
			_Utils_Tuple2('たいしかん', '大使館'),
			_Utils_Tuple2('だいじょうぶ', '大丈夫'),
			_Utils_Tuple2('だいじょうぶ', '大丈夫'),
			_Utils_Tuple2('だいすき', '大好き'),
			_Utils_Tuple2('たいせつ', '大切'),
			_Utils_Tuple2('だいどころ', '台所'),
			_Utils_Tuple2('たいへん', '大変'),
			_Utils_Tuple2('たいへん', '大変'),
			_Utils_Tuple2('たかい', '高い'),
			_Utils_Tuple2('たくさん', '沢山'),
			_Utils_Tuple2('たくさん', '沢山'),
			_Utils_Tuple2('たくしー', 'タクシー'),
			_Utils_Tuple2('だす', '出す'),
			_Utils_Tuple2('ただいま', 'ただいま'),
			_Utils_Tuple2('たち', 'たち'),
			_Utils_Tuple2('たつ', '立つ'),
			_Utils_Tuple2('たっきゅう', '卓球'),
			_Utils_Tuple2('たてもの', '建物'),
			_Utils_Tuple2('たてる', '立てる'),
			_Utils_Tuple2('たのしい', '楽しい'),
			_Utils_Tuple2('たのしみ', '楽しみ'),
			_Utils_Tuple2('たのしむ', '楽しむ'),
			_Utils_Tuple2('たばこ', 'タバコ'),
			_Utils_Tuple2('たぶん', '多分'),
			_Utils_Tuple2('たべもの', '食べ物'),
			_Utils_Tuple2('たべる', '食べる'),
			_Utils_Tuple2('たまご', '卵'),
			_Utils_Tuple2('たまねぎ', 'たまねぎ'),
			_Utils_Tuple2('だめ', '駄目'),
			_Utils_Tuple2('だれ', '誰'),
			_Utils_Tuple2('だれか', 'だれか'),
			_Utils_Tuple2('たんじょうひ', '誕生日'),
			_Utils_Tuple2('だんす', 'ダンス'),
			_Utils_Tuple2('だんせい', '男性'),
			_Utils_Tuple2('ちいさい', '小さい'),
			_Utils_Tuple2('ちいさな', '小さな'),
			_Utils_Tuple2('ちーず', 'チーズ'),
			_Utils_Tuple2('ちかい', '近い'),
			_Utils_Tuple2('ちがう', '違う'),
			_Utils_Tuple2('ちかく', '近く'),
			_Utils_Tuple2('ちかてつ', '地下鉄'),
			_Utils_Tuple2('ちけっと', 'チケット'),
			_Utils_Tuple2('ちず', '地図'),
			_Utils_Tuple2('ちち', '父'),
			_Utils_Tuple2('ちちのひ', '父の日'),
			_Utils_Tuple2('ちゃ', '茶'),
			_Utils_Tuple2('ちゃいろ', '茶色'),
			_Utils_Tuple2('ちゃん', 'ちゃん'),
			_Utils_Tuple2('ちゅうい', '注意'),
			_Utils_Tuple2('ちゅうがく', '中学'),
			_Utils_Tuple2('ちゅうがくせい', '中学生'),
			_Utils_Tuple2('ちゅうがっこう', '中学校'),
			_Utils_Tuple2('ちゅうかりょうり', '中華料理'),
			_Utils_Tuple2('ちょうめ', '丁目'),
			_Utils_Tuple2('ちょこ', 'チョコ'),
			_Utils_Tuple2('ちょこれーと', 'チョコレート'),
			_Utils_Tuple2('ちょっと', 'ちょっと'),
			_Utils_Tuple2('ついたち', '一日'),
			_Utils_Tuple2('つかう', '使う'),
			_Utils_Tuple2('つかれる', '疲れる'),
			_Utils_Tuple2('つき', '月'),
			_Utils_Tuple2('つき', '月'),
			_Utils_Tuple2('つくえ', '机'),
			_Utils_Tuple2('つくる', '作る'),
			_Utils_Tuple2('つま', '妻'),
			_Utils_Tuple2('つめ', '爪'),
			_Utils_Tuple2('つめたい', '冷たい'),
			_Utils_Tuple2('つよい', '強い'),
			_Utils_Tuple2('て', '手'),
			_Utils_Tuple2('てぃーしゃつ', 'Ｔシャツ'),
			_Utils_Tuple2('てぃっしゅ', 'ティッシュ'),
			_Utils_Tuple2('てぃっしゅぺーぱー', 'ティッシュペーパー'),
			_Utils_Tuple2('てーぷ', 'テープ'),
			_Utils_Tuple2('てーぶる', 'テーブル'),
			_Utils_Tuple2('てーぷれこーだー', 'テープレコーダー'),
			_Utils_Tuple2('でかける', '出掛ける'),
			_Utils_Tuple2('てがみ', '手紙'),
			_Utils_Tuple2('てきすと', 'テキスト'),
			_Utils_Tuple2('できる', '出来る'),
			_Utils_Tuple2('でぐち', '出口'),
			_Utils_Tuple2('てすと', 'テスト'),
			_Utils_Tuple2('てにす', 'テニス'),
			_Utils_Tuple2('でぱーと', 'デパート'),
			_Utils_Tuple2('てら', '寺'),
			_Utils_Tuple2('でる', '出る'),
			_Utils_Tuple2('てれび', 'テレビ'),
			_Utils_Tuple2('てん', '店'),
			_Utils_Tuple2('てん', '点'),
			_Utils_Tuple2('てんいん', '店員'),
			_Utils_Tuple2('てんき', '天気'),
			_Utils_Tuple2('でんき', '電気'),
			_Utils_Tuple2('でんしゃ', '電車'),
			_Utils_Tuple2('でんわ', '電話'),
			_Utils_Tuple2('でんわばんごう', '電話番号'),
			_Utils_Tuple2('ど', '度'),
			_Utils_Tuple2('どあ', 'ドア'),
			_Utils_Tuple2('といれ', 'トイレ'),
			_Utils_Tuple2('といれっとぺーぱー', 'トイレットペーパー'),
			_Utils_Tuple2('どう', 'どう'),
			_Utils_Tuple2('とうさん', '父さん'),
			_Utils_Tuple2('どうし', '動詞'),
			_Utils_Tuple2('どうぞ', 'どうぞ'),
			_Utils_Tuple2('どうぶつ', '動物'),
			_Utils_Tuple2('どうぶつえん', '動物園'),
			_Utils_Tuple2('どうも', 'どうも'),
			_Utils_Tuple2('とうもろこし', 'とうもろこし'),
			_Utils_Tuple2('とお', '十'),
			_Utils_Tuple2('とおい', '遠い'),
			_Utils_Tuple2('とおか', '十日'),
			_Utils_Tuple2('とおく', '遠く'),
			_Utils_Tuple2('とーすと', 'トースト'),
			_Utils_Tuple2('どーなつ', 'ドーナツ'),
			_Utils_Tuple2('とき', '時'),
			_Utils_Tuple2('ときどき', '時々'),
			_Utils_Tuple2('とけい', '時計'),
			_Utils_Tuple2('どこ', 'どこ'),
			_Utils_Tuple2('どこか', 'どこか'),
			_Utils_Tuple2('ところ', '所'),
			_Utils_Tuple2('とし', '年'),
			_Utils_Tuple2('としょかん', '図書館'),
			_Utils_Tuple2('としょしつ', '図書室'),
			_Utils_Tuple2('どちら', 'どちら'),
			_Utils_Tuple2('とても', 'とても'),
			_Utils_Tuple2('とどく', '届く'),
			_Utils_Tuple2('どなた', 'どなた'),
			_Utils_Tuple2('となり', '隣'),
			_Utils_Tuple2('どの', 'どの'),
			_Utils_Tuple2('どのくらい', 'どのくらい'),
			_Utils_Tuple2('とぶ', '飛ぶ'),
			_Utils_Tuple2('とまと', 'トマト'),
			_Utils_Tuple2('とまる', '止まる'),
			_Utils_Tuple2('ともだち', '友達'),
			_Utils_Tuple2('どよう', '土曜'),
			_Utils_Tuple2('どようび', '土曜日'),
			_Utils_Tuple2('とり', '鳥'),
			_Utils_Tuple2('とる', '取る'),
			_Utils_Tuple2('とる', '撮る'),
			_Utils_Tuple2('どる', 'ドル'),
			_Utils_Tuple2('どれ', 'どれ'),
			_Utils_Tuple2('どんな', 'どんな'),
			_Utils_Tuple2('どんな', 'どんな'),
			_Utils_Tuple2('ない', 'ない'),
			_Utils_Tuple2('ないふ', 'ナイフ'),
			_Utils_Tuple2('なか', '中'),
			_Utils_Tuple2('ながい', '長い'),
			_Utils_Tuple2('なぜ', 'なぜ'),
			_Utils_Tuple2('なつ', '夏'),
			_Utils_Tuple2('なつやすみ', '夏休み'),
			_Utils_Tuple2('なな', '七'),
			_Utils_Tuple2('ななつ', '七つ'),
			_Utils_Tuple2('なに', '何'),
			_Utils_Tuple2('なにか', '何か'),
			_Utils_Tuple2('なにも', '何も'),
			_Utils_Tuple2('なのか', '七日'),
			_Utils_Tuple2('なまえ', '名前'),
			_Utils_Tuple2('なる', '成る'),
			_Utils_Tuple2('なん', '何'),
			_Utils_Tuple2('なんかい', '何回'),
			_Utils_Tuple2('なんこ', '何個'),
			_Utils_Tuple2('なんで', '何で'),
			_Utils_Tuple2('なんでも', '何でも'),
			_Utils_Tuple2('なんど', '何度'),
			_Utils_Tuple2('なんにち', '何日'),
			_Utils_Tuple2('なんにん', '何人'),
			_Utils_Tuple2('なんねん', '何年'),
			_Utils_Tuple2('なんの', '何の'),
			_Utils_Tuple2('なんふん', '何分'),
			_Utils_Tuple2('に', '二'),
			_Utils_Tuple2('にいさん', '兄さん'),
			_Utils_Tuple2('にいさん', '兄さん'),
			_Utils_Tuple2('にかい', '二階'),
			_Utils_Tuple2('にがい', '苦い'),
			_Utils_Tuple2('にがつ', '二月'),
			_Utils_Tuple2('にぎやか', 'にぎやか'),
			_Utils_Tuple2('にく', '肉'),
			_Utils_Tuple2('にし', '西'),
			_Utils_Tuple2('にじゅう', '二十'),
			_Utils_Tuple2('にち', '日'),
			_Utils_Tuple2('にち', '日'),
			_Utils_Tuple2('にちよう', '日曜'),
			_Utils_Tuple2('にちようび', '日曜日'),
			_Utils_Tuple2('にっぽん', '日本'),
			_Utils_Tuple2('にほんご', '日本語'),
			_Utils_Tuple2('にほんしゅ', '日本酒'),
			_Utils_Tuple2('にもつ', '荷物'),
			_Utils_Tuple2('にゅーす', 'ニュース'),
			_Utils_Tuple2('にわ', '庭'),
			_Utils_Tuple2('にん', '人'),
			_Utils_Tuple2('にん', '人'),
			_Utils_Tuple2('にんぎょう', '人形'),
			_Utils_Tuple2('にんじん', 'にんじん'),
			_Utils_Tuple2('ぬぐ', '脱ぐ'),
			_Utils_Tuple2('ねえさん', '姉さん'),
			_Utils_Tuple2('ねえさん', '姉さん'),
			_Utils_Tuple2('ねくたい', 'ネクタイ'),
			_Utils_Tuple2('ねこ', '猫'),
			_Utils_Tuple2('ねだん', '値段'),
			_Utils_Tuple2('ねつ', '熱'),
			_Utils_Tuple2('ねむい', '眠い'),
			_Utils_Tuple2('ねる', '寝る'),
			_Utils_Tuple2('ねん', '年'),
			_Utils_Tuple2('ねん', '年'),
			_Utils_Tuple2('のーと', 'ノート'),
			_Utils_Tuple2('のど', '喉'),
			_Utils_Tuple2('のぼる', '登る'),
			_Utils_Tuple2('のみもの', '飲み物'),
			_Utils_Tuple2('のむ', '飲む'),
			_Utils_Tuple2('のる', '乗る'),
			_Utils_Tuple2('は', '歯'),
			_Utils_Tuple2('ぱーてぃー', 'パーティー'),
			_Utils_Tuple2('はい', 'はい'),
			_Utils_Tuple2('はい', '杯'),
			_Utils_Tuple2('ばいく', 'バイク'),
			_Utils_Tuple2('はいしゃ', '歯医者'),
			_Utils_Tuple2('ばいと', 'バイト'),
			_Utils_Tuple2('ぱいなっぷる', 'パイナップル'),
			_Utils_Tuple2('はいひーる', 'ハイヒール'),
			_Utils_Tuple2('はいる', '入る'),
			_Utils_Tuple2('はがき', '葉書'),
			_Utils_Tuple2('はく', '履く'),
			_Utils_Tuple2('はし', '箸'),
			_Utils_Tuple2('はし', '橋'),
			_Utils_Tuple2('はじまる', '始まる'),
			_Utils_Tuple2('はじめて', '初めて'),
			_Utils_Tuple2('はじめまして', '初めまして'),
			_Utils_Tuple2('はじめる', '始める'),
			_Utils_Tuple2('ばしょ', '場所'),
			_Utils_Tuple2('はしる', '走る'),
			_Utils_Tuple2('ばすけっとぼーる', 'バスケットボール'),
			_Utils_Tuple2('ぱすぽーと', 'パスポート'),
			_Utils_Tuple2('ぱそこん', 'パソコン'),
			_Utils_Tuple2('ばたー', 'バター'),
			_Utils_Tuple2('はたち', '二十歳'),
			_Utils_Tuple2('はたらく', '働く'),
			_Utils_Tuple2('はち', '八'),
			_Utils_Tuple2('はちがつ', '八月'),
			_Utils_Tuple2('はちじゅう', '八十'),
			_Utils_Tuple2('ばつ', 'ばつ'),
			_Utils_Tuple2('はつか', '二十日'),
			_Utils_Tuple2('はつか', '二十日'),
			_Utils_Tuple2('ばっぐ', 'バッグ'),
			_Utils_Tuple2('はな', '花'),
			_Utils_Tuple2('はな', '鼻'),
			_Utils_Tuple2('はなし', '話'),
			_Utils_Tuple2('はなす', '話す'),
			_Utils_Tuple2('ばなな', 'バナナ'),
			_Utils_Tuple2('はは', '母'),
			_Utils_Tuple2('ぱぱ', 'パパ'),
			_Utils_Tuple2('はぶらし', '歯ブラシ'),
			_Utils_Tuple2('はやい', '早い'),
			_Utils_Tuple2('はやい', '速い'),
			_Utils_Tuple2('はやく', '早く'),
			_Utils_Tuple2('はる', '春'),
			_Utils_Tuple2('はるやすみ', '春休み'),
			_Utils_Tuple2('はれ', '晴れ'),
			_Utils_Tuple2('ばれーぼーる', 'バレーボール'),
			_Utils_Tuple2('はれる', '晴れる'),
			_Utils_Tuple2('はん', '半'),
			_Utils_Tuple2('ばん', '番'),
			_Utils_Tuple2('ばん', '晩'),
			_Utils_Tuple2('ぱん', 'パン'),
			_Utils_Tuple2('はんかち', 'ハンカチ'),
			_Utils_Tuple2('ばんごう', '番号'),
			_Utils_Tuple2('ばんごはん', '晩御飯'),
			_Utils_Tuple2('ぱんだ', 'パンダ'),
			_Utils_Tuple2('ぱんつ', 'パンツ'),
			_Utils_Tuple2('はんとし', '半年'),
			_Utils_Tuple2('はんばーがー', 'ハンバーガー'),
			_Utils_Tuple2('はんばーぐ', 'ハンバーグ'),
			_Utils_Tuple2('はんぶん', '半分'),
			_Utils_Tuple2('ひ', '日'),
			_Utils_Tuple2('ひ', '火'),
			_Utils_Tuple2('ぴあの', 'ピアノ'),
			_Utils_Tuple2('びーる', 'ビール'),
			_Utils_Tuple2('ひがし', '東'),
			_Utils_Tuple2('ひき', '匹'),
			_Utils_Tuple2('ひく', '引く'),
			_Utils_Tuple2('ひく', '弾く'),
			_Utils_Tuple2('ひくい', '低い'),
			_Utils_Tuple2('ひこうき', '飛行機'),
			_Utils_Tuple2('びざ', 'ビザ'),
			_Utils_Tuple2('ぴざ', 'ピザ'),
			_Utils_Tuple2('ひだり', '左'),
			_Utils_Tuple2('ひだりあし', '左足'),
			_Utils_Tuple2('ひだりて', '左手'),
			_Utils_Tuple2('びでお', 'ビデオ'),
			_Utils_Tuple2('ひと', '人'),
			_Utils_Tuple2('ひと', '一'),
			_Utils_Tuple2('ひとつ', '一つ'),
			_Utils_Tuple2('ひとり', '一人'),
			_Utils_Tuple2('ひま', '暇'),
			_Utils_Tuple2('ひゃく', '百'),
			_Utils_Tuple2('ひゃくねん', '百年'),
			_Utils_Tuple2('ひゃくまん', '百万'),
			_Utils_Tuple2('びょういん', '病院'),
			_Utils_Tuple2('びよういん', '美容院'),
			_Utils_Tuple2('びょうき', '病気'),
			_Utils_Tuple2('ひらがな', '平仮名'),
			_Utils_Tuple2('ひる', '昼'),
			_Utils_Tuple2('ひるごはん', '昼御飯'),
			_Utils_Tuple2('ひるやすみ', '昼休み'),
			_Utils_Tuple2('ひろい', '広い'),
			_Utils_Tuple2('ぴんく', 'ピンク'),
			_Utils_Tuple2('ふァっくす', 'ファックス'),
			_Utils_Tuple2('ぷーる', 'プール'),
			_Utils_Tuple2('ふぉーく', 'フォーク'),
			_Utils_Tuple2('ふく', '服'),
			_Utils_Tuple2('ふた', '二'),
			_Utils_Tuple2('ぶた', '豚'),
			_Utils_Tuple2('ふたつ', '二つ'),
			_Utils_Tuple2('ぶたにく', '豚肉'),
			_Utils_Tuple2('ふたり', '二人'),
			_Utils_Tuple2('ふつか', '二日'),
			_Utils_Tuple2('ふとい', '太い'),
			_Utils_Tuple2('ふね', '船'),
			_Utils_Tuple2('ふべん', '不便'),
			_Utils_Tuple2('ふゆ', '冬'),
			_Utils_Tuple2('ふゆやすみ', '冬休み'),
			_Utils_Tuple2('ふらんすご', 'フランス語'),
			_Utils_Tuple2('ふらんすりょうり', 'フランス料理'),
			_Utils_Tuple2('ふる', '降る'),
			_Utils_Tuple2('ふるい', '古い'),
			_Utils_Tuple2('ぷれぜんと', 'プレゼント'),
			_Utils_Tuple2('ふろ', '風呂'),
			_Utils_Tuple2('ふん', '分'),
			_Utils_Tuple2('ぶん', '文'),
			_Utils_Tuple2('ぺーじ', 'ページ'),
			_Utils_Tuple2('ぺーじ', 'ページ'),
			_Utils_Tuple2('へた', '下手'),
			_Utils_Tuple2('べっど', 'ベッド'),
			_Utils_Tuple2('ぺっと', 'ペット'),
			_Utils_Tuple2('へや', '部屋'),
			_Utils_Tuple2('べると', 'ベルト'),
			_Utils_Tuple2('ぺん', 'ペン'),
			_Utils_Tuple2('べんきょう', '勉強'),
			_Utils_Tuple2('べんとう', '弁当'),
			_Utils_Tuple2('べんり', '便利'),
			_Utils_Tuple2('ぼうし', '帽子'),
			_Utils_Tuple2('ぼーるぺん', 'ボールペン'),
			_Utils_Tuple2('ぼく', '僕'),
			_Utils_Tuple2('ぽけっと', 'ポケット'),
			_Utils_Tuple2('ほしい', '欲しい'),
			_Utils_Tuple2('ぽすと', 'ポスト'),
			_Utils_Tuple2('ほそい', '細い'),
			_Utils_Tuple2('ぼたん', 'ボタン'),
			_Utils_Tuple2('ほっかいどう', '北海道'),
			_Utils_Tuple2('ぽてと', 'ポテト'),
			_Utils_Tuple2('ほてる', 'ホテル'),
			_Utils_Tuple2('ほん', '本'),
			_Utils_Tuple2('ほん', '本'),
			_Utils_Tuple2('ほん', '本'),
			_Utils_Tuple2('ほんだな', '本棚'),
			_Utils_Tuple2('ほんとう', '本当'),
			_Utils_Tuple2('ほんや', '本屋'),
			_Utils_Tuple2('まい', '枚'),
			_Utils_Tuple2('まい', '毎'),
			_Utils_Tuple2('まいあさ', '毎朝'),
			_Utils_Tuple2('まいく', 'マイク'),
			_Utils_Tuple2('まいしゅう', '毎週'),
			_Utils_Tuple2('まいつき', '毎月'),
			_Utils_Tuple2('まいとし', '毎年'),
			_Utils_Tuple2('まいにち', '毎日'),
			_Utils_Tuple2('まいばん', '毎晩'),
			_Utils_Tuple2('まえ', '前'),
			_Utils_Tuple2('まご', '孫'),
			_Utils_Tuple2('まじめ', 'まじめ'),
			_Utils_Tuple2('まだ', 'まだ'),
			_Utils_Tuple2('まち', '町'),
			_Utils_Tuple2('まつ', '待つ'),
			_Utils_Tuple2('まど', '窓'),
			_Utils_Tuple2('まま', 'ママ'),
			_Utils_Tuple2('まん', '万'),
			_Utils_Tuple2('まんが', '漫画'),
			_Utils_Tuple2('みえる', '見える'),
			_Utils_Tuple2('みがく', '磨く'),
			_Utils_Tuple2('みかん', 'みかん'),
			_Utils_Tuple2('みぎ', '右'),
			_Utils_Tuple2('みぎあし', '右足'),
			_Utils_Tuple2('みぎて', '右手'),
			_Utils_Tuple2('みじかい', '短い'),
			_Utils_Tuple2('みず', '水'),
			_Utils_Tuple2('みせ', '店'),
			_Utils_Tuple2('みせる', '見せる'),
			_Utils_Tuple2('みそしる', 'みそ汁'),
			_Utils_Tuple2('みち', '道'),
			_Utils_Tuple2('みっか', '三日'),
			_Utils_Tuple2('みっつ', '三つ'),
			_Utils_Tuple2('みどり', '緑'),
			_Utils_Tuple2('みどりいろ', '緑色'),
			_Utils_Tuple2('みなさん', '皆さん'),
			_Utils_Tuple2('みなみ', '南'),
			_Utils_Tuple2('みみ', '耳'),
			_Utils_Tuple2('みる', '見る'),
			_Utils_Tuple2('みるく', 'ミルク'),
			_Utils_Tuple2('む', '六'),
			_Utils_Tuple2('むいか', '六日'),
			_Utils_Tuple2('むずかしい', '難しい'),
			_Utils_Tuple2('むすこ', '息子'),
			_Utils_Tuple2('むすめ', '娘'),
			_Utils_Tuple2('むっつ', '六つ'),
			_Utils_Tuple2('め', '目'),
			_Utils_Tuple2('めーとる', 'メートル'),
			_Utils_Tuple2('めがね', 'めがね'),
			_Utils_Tuple2('もう', 'もう'),
			_Utils_Tuple2('もく', '木'),
			_Utils_Tuple2('もくよう', '木曜'),
			_Utils_Tuple2('もくようび', '木曜日'),
			_Utils_Tuple2('もし', 'もし'),
			_Utils_Tuple2('もしもし', 'もしもし'),
			_Utils_Tuple2('もつ', '持つ'),
			_Utils_Tuple2('もっと', 'もっと'),
			_Utils_Tuple2('もらう', 'もらう'),
			_Utils_Tuple2('もんだい', '問題'),
			_Utils_Tuple2('や', '屋'),
			_Utils_Tuple2('や', '八'),
			_Utils_Tuple2('やきそば', '焼きそば'),
			_Utils_Tuple2('やきにく', '焼肉'),
			_Utils_Tuple2('やきゅう', '野球'),
			_Utils_Tuple2('やさい', '野菜'),
			_Utils_Tuple2('やさしい', '優しい'),
			_Utils_Tuple2('やさしい', '易しい'),
			_Utils_Tuple2('やすい', '安い'),
			_Utils_Tuple2('やすみ', '休み'),
			_Utils_Tuple2('やすむ', '休む'),
			_Utils_Tuple2('やっつ', '八つ'),
			_Utils_Tuple2('やま', '山'),
			_Utils_Tuple2('ゆうがた', '夕方'),
			_Utils_Tuple2('ゆうしょく', '夕食'),
			_Utils_Tuple2('ゆうはん', '夕飯'),
			_Utils_Tuple2('ゆうびんきょく', '郵便局'),
			_Utils_Tuple2('ゆうめい', '有名'),
			_Utils_Tuple2('ゆうめい', '有名'),
			_Utils_Tuple2('ゆき', '雪'),
			_Utils_Tuple2('ゆび', '指'),
			_Utils_Tuple2('よい', '良い'),
			_Utils_Tuple2('よい', '良い'),
			_Utils_Tuple2('ようか', '八日'),
			_Utils_Tuple2('ようび', '曜日'),
			_Utils_Tuple2('ようふく', '洋服'),
			_Utils_Tuple2('よーろっぱ', 'ヨーロッパ'),
			_Utils_Tuple2('よく', '良く'),
			_Utils_Tuple2('よこ', '横'),
			_Utils_Tuple2('よっか', '四日'),
			_Utils_Tuple2('よっつ', '四つ'),
			_Utils_Tuple2('よぶ', '呼ぶ'),
			_Utils_Tuple2('よむ', '読む'),
			_Utils_Tuple2('よる', '夜'),
			_Utils_Tuple2('よろしく', 'よろしく'),
			_Utils_Tuple2('よわい', '弱い'),
			_Utils_Tuple2('よん', '四'),
			_Utils_Tuple2('らーめん', 'ラーメン'),
			_Utils_Tuple2('らいおん', 'ライオン'),
			_Utils_Tuple2('らいげつ', '来月'),
			_Utils_Tuple2('らいしゅう', '来週'),
			_Utils_Tuple2('らいねん', '来年'),
			_Utils_Tuple2('らじお', 'ラジオ'),
			_Utils_Tuple2('りゅうがく', '留学'),
			_Utils_Tuple2('りゅうがくせい', '留学生'),
			_Utils_Tuple2('りょうしん', '両親'),
			_Utils_Tuple2('りょうり', '料理'),
			_Utils_Tuple2('りょこう', '旅行'),
			_Utils_Tuple2('りんご', 'りんご'),
			_Utils_Tuple2('れい', '例'),
			_Utils_Tuple2('れいぞうこ', '冷蔵庫'),
			_Utils_Tuple2('れすとらん', 'レストラン'),
			_Utils_Tuple2('れもん', 'レモン'),
			_Utils_Tuple2('れんしゅう', '練習'),
			_Utils_Tuple2('ろく', '六'),
			_Utils_Tuple2('ろくがつ', '六月'),
			_Utils_Tuple2('ろくじ', '六時'),
			_Utils_Tuple2('ろくじゅう', '六十'),
			_Utils_Tuple2('ろしあご', 'ロシア語'),
			_Utils_Tuple2('わいん', 'ワイン'),
			_Utils_Tuple2('わかい', '若い'),
			_Utils_Tuple2('わかる', '分かる'),
			_Utils_Tuple2('わすれる', '忘れる'),
			_Utils_Tuple2('わたし', '私'),
			_Utils_Tuple2('わるい', '悪い'),
			_Utils_Tuple2('わんぴーす', 'ワンピース'),
			_Utils_Tuple2('あーけーど', 'アーケード'),
			_Utils_Tuple2('あーち', 'アーチ'),
			_Utils_Tuple2('あーちすと', 'アーチスト'),
			_Utils_Tuple2('あーと', 'アート'),
			_Utils_Tuple2('あーもんど', 'アーモンド'),
			_Utils_Tuple2('あいけん', '愛犬'),
			_Utils_Tuple2('あいこくしん', '愛国心'),
			_Utils_Tuple2('あいしゃ', '愛車'),
			_Utils_Tuple2('あいしょう', '相性'),
			_Utils_Tuple2('あいしょう', '愛称'),
			_Utils_Tuple2('あいじょう', '愛情'),
			_Utils_Tuple2('あいず', '合図'),
			_Utils_Tuple2('あいする', '愛する'),
			_Utils_Tuple2('あいちゃく', '愛着'),
			_Utils_Tuple2('あいつ', 'あいつ'),
			_Utils_Tuple2('あいづち', 'あいづち'),
			_Utils_Tuple2('あいて', '相手'),
			_Utils_Tuple2('あいでぃあ', 'アイディア'),
			_Utils_Tuple2('あいまい', '曖昧'),
			_Utils_Tuple2('あいよう', '愛用'),
			_Utils_Tuple2('あいるらんど', 'アイルランド'),
			_Utils_Tuple2('あう', '遭う'),
			_Utils_Tuple2('あう', '合う'),
			_Utils_Tuple2('あうと', 'アウト'),
			_Utils_Tuple2('あうとどあ', 'アウトドア'),
			_Utils_Tuple2('あおあお', '青々'),
			_Utils_Tuple2('あおぞら', '青空'),
			_Utils_Tuple2('あかあか', '赤々'),
			_Utils_Tuple2('あかじ', '赤字'),
			_Utils_Tuple2('あかり', '明かり'),
			_Utils_Tuple2('あがり', '上がり'),
			_Utils_Tuple2('あがり', '上がり'),
			_Utils_Tuple2('あかんぼう', '赤ん坊'),
			_Utils_Tuple2('あきかん', '空き缶'),
			_Utils_Tuple2('あきち', '空き地'),
			_Utils_Tuple2('あきびん', '空き瓶'),
			_Utils_Tuple2('あきまつり', '秋祭り'),
			_Utils_Tuple2('あきらか', '明らか'),
			_Utils_Tuple2('あきらめ', 'あきらめ'),
			_Utils_Tuple2('あきらめる', 'あきらめる'),
			_Utils_Tuple2('あきる', '飽きる'),
			_Utils_Tuple2('あきれる', 'あきれる'),
			_Utils_Tuple2('あく', '悪'),
			_Utils_Tuple2('あく', '空く'),
			_Utils_Tuple2('あく', '悪'),
			_Utils_Tuple2('あく', '明く'),
			_Utils_Tuple2('あくい', '悪意'),
			_Utils_Tuple2('あくえいきょう', '悪影響'),
			_Utils_Tuple2('あくじ', '悪事'),
			_Utils_Tuple2('あくしつ', '悪質'),
			_Utils_Tuple2('あくしでんと', 'アクシデント'),
			_Utils_Tuple2('あくしゅ', '握手'),
			_Utils_Tuple2('あくしゅう', '悪臭'),
			_Utils_Tuple2('あくしょん', 'アクション'),
			_Utils_Tuple2('あくせい', '悪性'),
			_Utils_Tuple2('あくせさりー', 'アクセサリー'),
			_Utils_Tuple2('あくせす', 'アクセス'),
			_Utils_Tuple2('あくせる', 'アクセル'),
			_Utils_Tuple2('あくせんと', 'アクセント'),
			_Utils_Tuple2('あくてぃぶ', 'アクティブ'),
			_Utils_Tuple2('あくにん', '悪人'),
			_Utils_Tuple2('あくま', '悪魔'),
			_Utils_Tuple2('あくむ', '悪夢'),
			_Utils_Tuple2('あくやく', '悪役'),
			_Utils_Tuple2('あくよう', '悪用'),
			_Utils_Tuple2('あげ', '揚げ'),
			_Utils_Tuple2('あげ', '揚げ'),
			_Utils_Tuple2('あげあぶら', '揚げ油'),
			_Utils_Tuple2('あげおろし', '上げ下ろし'),
			_Utils_Tuple2('あけがた', '明け方'),
			_Utils_Tuple2('あげさげ', '上げ下げ'),
			_Utils_Tuple2('あける', '明ける'),
			_Utils_Tuple2('あける', '空ける'),
			_Utils_Tuple2('あげる', '挙げる'),
			_Utils_Tuple2('あげる', '揚げる'),
			_Utils_Tuple2('あご', 'あご'),
			_Utils_Tuple2('あこがれ', '憧れ'),
			_Utils_Tuple2('あこがれる', '憧れる'),
			_Utils_Tuple2('あさい', '浅い'),
			_Utils_Tuple2('あさいち', '朝市'),
			_Utils_Tuple2('あさがた', '朝方'),
			_Utils_Tuple2('あさねぼう', '朝寝坊'),
			_Utils_Tuple2('あさばん', '朝晩'),
			_Utils_Tuple2('あさひ', '朝日'),
			_Utils_Tuple2('あざやか', '鮮やか'),
			_Utils_Tuple2('あさゆう', '朝夕'),
			_Utils_Tuple2('あし', '脚'),
			_Utils_Tuple2('あじ', 'あじ'),
			_Utils_Tuple2('あしあと', '足跡'),
			_Utils_Tuple2('あしおと', '足音'),
			_Utils_Tuple2('あしくび', '足首'),
			_Utils_Tuple2('あしこし', '足腰'),
			_Utils_Tuple2('あしすたんと', 'アシスタント'),
			_Utils_Tuple2('あしすと', 'アシスト'),
			_Utils_Tuple2('あじつけ', '味付け'),
			_Utils_Tuple2('あしどり', '足取り'),
			_Utils_Tuple2('あしぶみ', '足踏み'),
			_Utils_Tuple2('あじみ', '味見'),
			_Utils_Tuple2('あしもと', '足元'),
			_Utils_Tuple2('あじわう', '味わう'),
			_Utils_Tuple2('あす', '明日'),
			_Utils_Tuple2('あずかる', '預かる'),
			_Utils_Tuple2('あずける', '預ける'),
			_Utils_Tuple2('あすぱらがす', 'アスパラガス'),
			_Utils_Tuple2('あせ', '汗'),
			_Utils_Tuple2('あたい', '値'),
			_Utils_Tuple2('あたえる', '与える'),
			_Utils_Tuple2('あたたか', '温か'),
			_Utils_Tuple2('あたたかみ', '温かみ'),
			_Utils_Tuple2('あたたまる', '温まる'),
			_Utils_Tuple2('あたためる', '温める'),
			_Utils_Tuple2('あたっく', 'アタック'),
			_Utils_Tuple2('あだぷたー', 'アダプター'),
			_Utils_Tuple2('あたまきん', '頭金'),
			_Utils_Tuple2('あたり', '当たり'),
			_Utils_Tuple2('あたり', '当たり'),
			_Utils_Tuple2('あたりまえ', '当たり前'),
			_Utils_Tuple2('あたる', '当たる'),
			_Utils_Tuple2('あだると', 'アダルト'),
			_Utils_Tuple2('あちらこちら', 'あちこち'),
			_Utils_Tuple2('あつあげ', '厚揚げ'),
			_Utils_Tuple2('あつあつ', '熱々'),
			_Utils_Tuple2('あっか', '悪化'),
			_Utils_Tuple2('あつかう', '扱う'),
			_Utils_Tuple2('あつかましい', '厚かましい'),
			_Utils_Tuple2('あっさり', 'あっさり'),
			_Utils_Tuple2('あつで', '厚手'),
			_Utils_Tuple2('あっとうてき', '圧倒的'),
			_Utils_Tuple2('あっぷ', 'アップ'),
			_Utils_Tuple2('あっぷる', 'アップル'),
			_Utils_Tuple2('あっぷるぱい', 'アップルパイ'),
			_Utils_Tuple2('あつまり', '集まり'),
			_Utils_Tuple2('あつみ', '厚み'),
			_Utils_Tuple2('あつめる', '集める'),
			_Utils_Tuple2('あつりょく', '圧力'),
			_Utils_Tuple2('あて', '宛て'),
			_Utils_Tuple2('あてさき', '宛て先'),
			_Utils_Tuple2('あてる', '当てる'),
			_Utils_Tuple2('あと', '跡'),
			_Utils_Tuple2('あとあじ', '後味'),
			_Utils_Tuple2('あとかたづけ', '後片付け'),
			_Utils_Tuple2('あとしまつ', '後始末'),
			_Utils_Tuple2('あどばいざー', 'アドバイザー'),
			_Utils_Tuple2('あどばいす', 'アドバイス'),
			_Utils_Tuple2('あどべんちゃー', 'アドベンチャー'),
			_Utils_Tuple2('あとまわし', '後回し'),
			_Utils_Tuple2('あとらくしょん', 'アトラクション'),
			_Utils_Tuple2('あどれす', 'アドレス'),
			_Utils_Tuple2('あな', '穴'),
			_Utils_Tuple2('あなうんさ', 'アナウンサー'),
			_Utils_Tuple2('あなうんさー', 'アナウンサー'),
			_Utils_Tuple2('あなうんす', 'アナウンス'),
			_Utils_Tuple2('あなろぐ', 'アナログ'),
			_Utils_Tuple2('あにまる', 'アニマル'),
			_Utils_Tuple2('あばれる', '暴れる'),
			_Utils_Tuple2('あぴーる', 'アピール'),
			_Utils_Tuple2('あひる', 'あひる'),
			_Utils_Tuple2('あふがにすたん', 'アフガニスタン'),
			_Utils_Tuple2('あふたー', 'アフター'),
			_Utils_Tuple2('あふたーさーびす', 'アフターサービス'),
			_Utils_Tuple2('あふたぬーん', 'アフタヌーン'),
			_Utils_Tuple2('あぶら', '油'),
			_Utils_Tuple2('あぶら', '脂'),
			_Utils_Tuple2('あふれる', '溢れる'),
			_Utils_Tuple2('あまえる', '甘える'),
			_Utils_Tuple2('あまから', '甘辛'),
			_Utils_Tuple2('あまからい', '甘辛い'),
			_Utils_Tuple2('あまくち', '甘口'),
			_Utils_Tuple2('あまぐも', '雨雲'),
			_Utils_Tuple2('あまちゅあ', 'アマチュア'),
			_Utils_Tuple2('あまみ', '甘み'),
			_Utils_Tuple2('あまやかす', '甘やかす'),
			_Utils_Tuple2('あまり', '余り'),
			_Utils_Tuple2('あまる', '余る'),
			_Utils_Tuple2('あみだな', '網棚'),
			_Utils_Tuple2('あみど', '網戸'),
			_Utils_Tuple2('あみもの', '編み物'),
			_Utils_Tuple2('あむ', '編む'),
			_Utils_Tuple2('あめふり', '雨降り'),
			_Utils_Tuple2('あめりかん', 'アメリカン'),
			_Utils_Tuple2('あやうい', '危うい'),
			_Utils_Tuple2('あやしい', '怪しい'),
			_Utils_Tuple2('あやしむ', '怪しむ'),
			_Utils_Tuple2('あやまち', '過ち'),
			_Utils_Tuple2('あやまり', '誤り'),
			_Utils_Tuple2('あやまる', '謝る'),
			_Utils_Tuple2('あやまる', '誤る'),
			_Utils_Tuple2('あら', 'あら'),
			_Utils_Tuple2('あらーむ', 'アラーム'),
			_Utils_Tuple2('あらい', '洗い'),
			_Utils_Tuple2('あらい', '粗い'),
			_Utils_Tuple2('あらい', '荒い'),
			_Utils_Tuple2('あらいながす', '洗い流す'),
			_Utils_Tuple2('あらいば', '洗い場'),
			_Utils_Tuple2('あらいもの', '洗い物'),
			_Utils_Tuple2('あらかじめ', 'あらかじめ'),
			_Utils_Tuple2('あらし', '嵐'),
			_Utils_Tuple2('あらそい', '争い'),
			_Utils_Tuple2('あらそう', '争う'),
			_Utils_Tuple2('あらた', '新た'),
			_Utils_Tuple2('あらためて', '改めて'),
			_Utils_Tuple2('あらためる', '改める'),
			_Utils_Tuple2('あらぶ', 'アラブ'),
			_Utils_Tuple2('あらゆる', 'あらゆる'),
			_Utils_Tuple2('あり', 'あり'),
			_Utils_Tuple2('あり', '有り'),
			_Utils_Tuple2('ありがたい', '有り難い'),
			_Utils_Tuple2('ありく', '歩く'),
			_Utils_Tuple2('ある', '或る'),
			_Utils_Tuple2('あるいは', 'あるいは'),
			_Utils_Tuple2('あるきまわる', '歩き回る'),
			_Utils_Tuple2('あるこーる', 'アルコール'),
			_Utils_Tuple2('あるぜんちん', 'アルゼンチン'),
			_Utils_Tuple2('あるばむ', 'アルバム'),
			_Utils_Tuple2('あるふァべっと', 'アルファベット'),
			_Utils_Tuple2('あるぷす', 'アルプス'),
			_Utils_Tuple2('あるみ', 'アルミ'),
			_Utils_Tuple2('あるみほいる', 'アルミホイル'),
			_Utils_Tuple2('あれこれ', 'あれこれ'),
			_Utils_Tuple2('あれるぎー', 'アレルギー'),
			_Utils_Tuple2('あわ', '泡'),
			_Utils_Tuple2('あわせ', '合わせ'),
			_Utils_Tuple2('あわせる', '合わせる'),
			_Utils_Tuple2('あわただしい', '慌ただしい'),
			_Utils_Tuple2('あわてる', '慌てる'),
			_Utils_Tuple2('あん', '案'),
			_Utils_Tuple2('あんい', '安易'),
			_Utils_Tuple2('あんか', '安価'),
			_Utils_Tuple2('あんがい', '案外'),
			_Utils_Tuple2('あんき', '暗記'),
			_Utils_Tuple2('あんけーと', 'アンケート'),
			_Utils_Tuple2('あんこ', 'あんこ'),
			_Utils_Tuple2('あんこーる', 'アンコール'),
			_Utils_Tuple2('あんしょう', '暗証'),
			_Utils_Tuple2('あんしょうばんごう', '暗証番号'),
			_Utils_Tuple2('あんせい', '安静'),
			_Utils_Tuple2('あんぜん', '安全'),
			_Utils_Tuple2('あんぜんせい', '安全性'),
			_Utils_Tuple2('あんだーらいん', 'アンダーライン'),
			_Utils_Tuple2('あんてい', '安定'),
			_Utils_Tuple2('あんてな', 'アンテナ'),
			_Utils_Tuple2('あんな', 'あんな'),
			_Utils_Tuple2('あんないしょ', '案内書'),
			_Utils_Tuple2('あんないじょう', '案内状'),
			_Utils_Tuple2('あんばらんす', 'アンバランス'),
			_Utils_Tuple2('あんぴ', '安否'),
			_Utils_Tuple2('い', '胃'),
			_Utils_Tuple2('い', '位'),
			_Utils_Tuple2('い', '異'),
			_Utils_Tuple2('い', '意'),
			_Utils_Tuple2('い', '医'),
			_Utils_Tuple2('いいあい', '言い合い'),
			_Utils_Tuple2('いいあう', '言い合う'),
			_Utils_Tuple2('いいあらわす', '言い表わす'),
			_Utils_Tuple2('いいかえす', '言い返す'),
			_Utils_Tuple2('いいかえる', '言い替える'),
			_Utils_Tuple2('いいきかせる', '言い聞かせる'),
			_Utils_Tuple2('いいきる', '言い切る'),
			_Utils_Tuple2('いいわけ', '言い訳'),
			_Utils_Tuple2('いいん', '委員'),
			_Utils_Tuple2('いいん', '医院'),
			_Utils_Tuple2('いいんかい', '委員会'),
			_Utils_Tuple2('いえいえ', 'いえいえ'),
			_Utils_Tuple2('いえいえ', '家々'),
			_Utils_Tuple2('いえす', 'イエス'),
			_Utils_Tuple2('いえで', '家出'),
			_Utils_Tuple2('いえろー', 'イエロー'),
			_Utils_Tuple2('いか', 'いか'),
			_Utils_Tuple2('いか', '以下'),
			_Utils_Tuple2('いか', '医科'),
			_Utils_Tuple2('いがい', '以外'),
			_Utils_Tuple2('いかが', 'いかが'),
			_Utils_Tuple2('いがく', '医学'),
			_Utils_Tuple2('いがくしょ', '医学書'),
			_Utils_Tuple2('いかす', '生かす'),
			_Utils_Tuple2('いかだいがく', '医科大学'),
			_Utils_Tuple2('いかめら', '胃カメラ'),
			_Utils_Tuple2('いかり', '怒り'),
			_Utils_Tuple2('いき', '息'),
			_Utils_Tuple2('いき', '域'),
			_Utils_Tuple2('いき', '生き'),
			_Utils_Tuple2('いぎ', '意義'),
			_Utils_Tuple2('いぎ', '異議'),
			_Utils_Tuple2('いきいき', '生き生き'),
			_Utils_Tuple2('いきおい', '勢い'),
			_Utils_Tuple2('いきがい', '生きがい'),
			_Utils_Tuple2('いきかえる', '生き返る'),
			_Utils_Tuple2('いきき', '行き来'),
			_Utils_Tuple2('いきぐるしい', '息苦しい'),
			_Utils_Tuple2('いきさき', '行き先'),
			_Utils_Tuple2('いきつく', '行き着く'),
			_Utils_Tuple2('いきなり', 'いきなり'),
			_Utils_Tuple2('いきぬき', '息抜き'),
			_Utils_Tuple2('いきもの', '生き物'),
			_Utils_Tuple2('いくじ', '育児'),
			_Utils_Tuple2('いくつか', '幾つか'),
			_Utils_Tuple2('いくら', 'イクラ'),
			_Utils_Tuple2('いくらか', '幾らか'),
			_Utils_Tuple2('いけばな', '生け花'),
			_Utils_Tuple2('いける', '生ける'),
			_Utils_Tuple2('いけん', '意見'),
			_Utils_Tuple2('いご', '以後'),
			_Utils_Tuple2('いご', '囲碁'),
			_Utils_Tuple2('いこう', '以降'),
			_Utils_Tuple2('いこう', '移行'),
			_Utils_Tuple2('いこう', '意向'),
			_Utils_Tuple2('いこーる', 'イコール'),
			_Utils_Tuple2('いこく', '異国'),
			_Utils_Tuple2('いごこち', '居心地'),
			_Utils_Tuple2('いさん', '遺産'),
			_Utils_Tuple2('いし', '石'),
			_Utils_Tuple2('いし', '医師'),
			_Utils_Tuple2('いし', '意志'),
			_Utils_Tuple2('いし', '意思'),
			_Utils_Tuple2('いじ', '維持'),
			_Utils_Tuple2('いしき', '意識'),
			_Utils_Tuple2('いしきてき', '意識的'),
			_Utils_Tuple2('いしつ', '異質'),
			_Utils_Tuple2('いじめ', 'いじめ'),
			_Utils_Tuple2('いじめっこ', 'いじめっこ'),
			_Utils_Tuple2('いじめる', 'いじめる'),
			_Utils_Tuple2('いじゅう', '移住'),
			_Utils_Tuple2('いじょう', '以上'),
			_Utils_Tuple2('いじょう', '異常'),
			_Utils_Tuple2('いしょく', '移植'),
			_Utils_Tuple2('いしょくじゅう', '衣食住'),
			_Utils_Tuple2('いじわる', '意地悪'),
			_Utils_Tuple2('いじわる', '意地悪'),
			_Utils_Tuple2('いず', '伊豆'),
			_Utils_Tuple2('いずみ', '泉'),
			_Utils_Tuple2('いすらむ', 'イスラム'),
			_Utils_Tuple2('いすらむきょう', 'イスラム教'),
			_Utils_Tuple2('いせい', '異性'),
			_Utils_Tuple2('いせき', '遺跡'),
			_Utils_Tuple2('いぜん', '以前'),
			_Utils_Tuple2('いぜん', '依然'),
			_Utils_Tuple2('いそぎ', '急ぎ'),
			_Utils_Tuple2('いそぎあし', '急ぎ足'),
			_Utils_Tuple2('いそぐ', '急ぐ'),
			_Utils_Tuple2('いぞん', '依存'),
			_Utils_Tuple2('いた', '板'),
			_Utils_Tuple2('いたい', '遺体'),
			_Utils_Tuple2('いだい', '偉大'),
			_Utils_Tuple2('いたいたしい', '痛々しい'),
			_Utils_Tuple2('いだく', '抱く'),
			_Utils_Tuple2('いたす', '致す'),
			_Utils_Tuple2('いたずら', 'いたずら'),
			_Utils_Tuple2('いただく', '頂く'),
			_Utils_Tuple2('いたちょこ', '板チョコ'),
			_Utils_Tuple2('いたみ', '痛み'),
			_Utils_Tuple2('いたみどめ', '痛み止め'),
			_Utils_Tuple2('いたむ', '痛む'),
			_Utils_Tuple2('いためる', '炒める'),
			_Utils_Tuple2('いためる', '痛める'),
			_Utils_Tuple2('いたる', '至る'),
			_Utils_Tuple2('いたるところ', '至る所'),
			_Utils_Tuple2('いち', '位置'),
			_Utils_Tuple2('いちいん', '一員'),
			_Utils_Tuple2('いちいん', '一因'),
			_Utils_Tuple2('いちえん', '一円'),
			_Utils_Tuple2('いちおう', '一応'),
			_Utils_Tuple2('いちきのう', '一昨日'),
			_Utils_Tuple2('いちぎょう', '一行'),
			_Utils_Tuple2('いちこだて', '一戸建て'),
			_Utils_Tuple2('いちじ', '一時'),
			_Utils_Tuple2('いちじき', '一時期'),
			_Utils_Tuple2('いちじてき', '一時的'),
			_Utils_Tuple2('いちしゅ', '一種'),
			_Utils_Tuple2('いちじるしい', '著しい'),
			_Utils_Tuple2('いちせい', '一生'),
			_Utils_Tuple2('いちぞく', '一族'),
			_Utils_Tuple2('いちたい', '一体'),
			_Utils_Tuple2('いちだんと', '一段と'),
			_Utils_Tuple2('いちだんらく', '一段落'),
			_Utils_Tuple2('いちどう', '一同'),
			_Utils_Tuple2('いちどく', '一読'),
			_Utils_Tuple2('いちどに', '一度に'),
			_Utils_Tuple2('いちにんひとり', '一人一人'),
			_Utils_Tuple2('いちにんまえ', '一人前'),
			_Utils_Tuple2('いちにんまえ', '一人前'),
			_Utils_Tuple2('いちばい', '一倍'),
			_Utils_Tuple2('いちぶ', '一部'),
			_Utils_Tuple2('いちぶ', '一部'),
			_Utils_Tuple2('いちぶぶん', '一部分'),
			_Utils_Tuple2('いちぶん', '一文'),
			_Utils_Tuple2('いちまわり', '一回り'),
			_Utils_Tuple2('いちめい', '一名'),
			_Utils_Tuple2('いちめん', '一面'),
			_Utils_Tuple2('いちめん', '一面'),
			_Utils_Tuple2('いちょう', '胃腸'),
			_Utils_Tuple2('いちりつ', '一律'),
			_Utils_Tuple2('いちりゅう', '一流'),
			_Utils_Tuple2('いちれい', '一例'),
			_Utils_Tuple2('いちれつ', '一列'),
			_Utils_Tuple2('いつう', '胃痛'),
			_Utils_Tuple2('いっか', '一家'),
			_Utils_Tuple2('いつか', 'いつか'),
			_Utils_Tuple2('いっきに', '一気に'),
			_Utils_Tuple2('いっけん', '一見'),
			_Utils_Tuple2('いっけん', '一見'),
			_Utils_Tuple2('いっさい', '一切'),
			_Utils_Tuple2('いっさくじつ', '一昨日'),
			_Utils_Tuple2('いっさくねん', '一昨年'),
			_Utils_Tuple2('いっしき', '一式'),
			_Utils_Tuple2('いっしつ', '一室'),
			_Utils_Tuple2('いっしゅ', '一種'),
			_Utils_Tuple2('いっしゅん', '一瞬'),
			_Utils_Tuple2('いっしょう', '一生'),
			_Utils_Tuple2('いっしょく', '一色'),
			_Utils_Tuple2('いっせいに', '一斉に'),
			_Utils_Tuple2('いっそう', '一層'),
			_Utils_Tuple2('いったい', '一体'),
			_Utils_Tuple2('いったい', '一体'),
			_Utils_Tuple2('いったん', 'いったん'),
			_Utils_Tuple2('いっち', '一致'),
			_Utils_Tuple2('いってい', '一定'),
			_Utils_Tuple2('いっとう', '一等'),
			_Utils_Tuple2('いっぱいいっぱい', '一杯一杯'),
			_Utils_Tuple2('いっぱん', '一般'),
			_Utils_Tuple2('いっぱんてき', '一般的'),
			_Utils_Tuple2('いっぱんに', '一般に'),
			_Utils_Tuple2('いっぱんろん', '一般論'),
			_Utils_Tuple2('いっぽ', '一歩'),
			_Utils_Tuple2('いっぽう', '一方'),
			_Utils_Tuple2('いっぽう', '一方'),
			_Utils_Tuple2('いっぽうつうこう', '一方通行'),
			_Utils_Tuple2('いっぽうてき', '一方的'),
			_Utils_Tuple2('いっぽん', '一本'),
			_Utils_Tuple2('いつまでも', 'いつまでも'),
			_Utils_Tuple2('いてん', '移転'),
			_Utils_Tuple2('いでん', '遺伝'),
			_Utils_Tuple2('いと', '糸'),
			_Utils_Tuple2('いど', '井戸'),
			_Utils_Tuple2('いどう', '移動'),
			_Utils_Tuple2('いとこ', 'いとこ'),
			_Utils_Tuple2('いとま', 'いとま'),
			_Utils_Tuple2('いない', '以内'),
			_Utils_Tuple2('いなか', '田舎'),
			_Utils_Tuple2('いなかもの', '田舎者'),
			_Utils_Tuple2('いにしゃる', 'イニシャル'),
			_Utils_Tuple2('いね', '稲'),
			_Utils_Tuple2('いねむり', '居眠り'),
			_Utils_Tuple2('いのしし', '猪'),
			_Utils_Tuple2('いのち', '命'),
			_Utils_Tuple2('いのり', '祈り'),
			_Utils_Tuple2('いのる', '祈る'),
			_Utils_Tuple2('いはん', '違反'),
			_Utils_Tuple2('いふく', '衣服'),
			_Utils_Tuple2('いへん', '異変'),
			_Utils_Tuple2('いべんと', 'イベント'),
			_Utils_Tuple2('いほう', '違法'),
			_Utils_Tuple2('いま', '居間'),
			_Utils_Tuple2('いまいち', '今一'),
			_Utils_Tuple2('いまごろ', '今頃'),
			_Utils_Tuple2('いまじねーしょん', 'イマジネーション'),
			_Utils_Tuple2('いまだ', '未だ'),
			_Utils_Tuple2('いまどき', '今時'),
			_Utils_Tuple2('いまにも', '今にも'),
			_Utils_Tuple2('いまひとつ', '今一つ'),
			_Utils_Tuple2('いまふう', '今風'),
			_Utils_Tuple2('いままで', '今まで'),
			_Utils_Tuple2('いみん', '移民'),
			_Utils_Tuple2('いめーじ', 'イメージ'),
			_Utils_Tuple2('いも', '芋'),
			_Utils_Tuple2('いや', 'いや'),
			_Utils_Tuple2('いや', 'いや'),
			_Utils_Tuple2('いやいや', '嫌々'),
			_Utils_Tuple2('いやいや', 'いやいや'),
			_Utils_Tuple2('いやがる', '嫌がる'),
			_Utils_Tuple2('いやく', '医薬'),
			_Utils_Tuple2('いやくひん', '医薬品'),
			_Utils_Tuple2('いやほん', 'イヤホン'),
			_Utils_Tuple2('いよいよ', 'いよいよ'),
			_Utils_Tuple2('いよく', '意欲'),
			_Utils_Tuple2('いよくてき', '意欲的'),
			_Utils_Tuple2('いらい', '以来'),
			_Utils_Tuple2('いらい', '依頼'),
			_Utils_Tuple2('いらいら', 'いらいら'),
			_Utils_Tuple2('いらく', 'イラク'),
			_Utils_Tuple2('いらすと', 'イラスト'),
			_Utils_Tuple2('いらすとれーたー', 'イラストレーター'),
			_Utils_Tuple2('いらん', 'イラン'),
			_Utils_Tuple2('いり', '入り'),
			_Utils_Tuple2('いり', '入り'),
			_Utils_Tuple2('いりょう', '医療'),
			_Utils_Tuple2('いりょう', '衣料'),
			_Utils_Tuple2('いる', '入る'),
			_Utils_Tuple2('いるい', '衣類'),
			_Utils_Tuple2('いるみねーしょん', 'イルミネーション'),
			_Utils_Tuple2('いれかえ', '入れ替え'),
			_Utils_Tuple2('いれかえる', '入れ替える'),
			_Utils_Tuple2('いれもの', '入れ物'),
			_Utils_Tuple2('いろがみ', '色紙'),
			_Utils_Tuple2('いろんな', '色んな'),
			_Utils_Tuple2('いわ', '岩'),
			_Utils_Tuple2('いわい', '祝い'),
			_Utils_Tuple2('いわいごと', '祝い事'),
			_Utils_Tuple2('いわう', '祝う'),
			_Utils_Tuple2('いわかん', '違和感'),
			_Utils_Tuple2('いん', '員'),
			_Utils_Tuple2('いん', '院'),
			_Utils_Tuple2('いん', '印'),
			_Utils_Tuple2('いん', '院'),
			_Utils_Tuple2('いんかん', '印鑑'),
			_Utils_Tuple2('いんく', 'インク'),
			_Utils_Tuple2('いんぐりっしゅ', 'イングリッシュ'),
			_Utils_Tuple2('いんさいど', 'インサイド'),
			_Utils_Tuple2('いんさつ', '印刷'),
			_Utils_Tuple2('いんしゅ', '飲酒'),
			_Utils_Tuple2('いんしゅうんてん', '飲酒運転'),
			_Utils_Tuple2('いんしょう', '印象'),
			_Utils_Tuple2('いんしょうてき', '印象的'),
			_Utils_Tuple2('いんしょく', '飲食'),
			_Utils_Tuple2('いんしょくてん', '飲食店'),
			_Utils_Tuple2('いんすたんと', 'インスタント'),
			_Utils_Tuple2('いんすたんとしょくひん', 'インスタント食品'),
			_Utils_Tuple2('いんすとらくたー', 'インストラクター'),
			_Utils_Tuple2('いんたーなしょなる', 'インターナショナル'),
			_Utils_Tuple2('いんたーほん', 'インターホン'),
			_Utils_Tuple2('いんたい', '引退'),
			_Utils_Tuple2('いんたびゅー', 'インタビュー'),
			_Utils_Tuple2('いんち', 'インチ'),
			_Utils_Tuple2('いんちょう', '院長'),
			_Utils_Tuple2('いんてりあ', 'インテリア'),
			_Utils_Tuple2('いんぱくと', 'インパクト'),
			_Utils_Tuple2('いんふぉめーしょん', 'インフォメーション'),
			_Utils_Tuple2('いんふるえんざ', 'インフルエンザ'),
			_Utils_Tuple2('いんふれ', 'インフレ'),
			_Utils_Tuple2('いんりょう', '飲料'),
			_Utils_Tuple2('いんりょうすい', '飲料水'),
			_Utils_Tuple2('う', '雨'),
			_Utils_Tuple2('ういーくえんど', 'ウイークエンド'),
			_Utils_Tuple2('ういーくりー', 'ウイークリー'),
			_Utils_Tuple2('ういるす', 'ウイルス'),
			_Utils_Tuple2('ういんく', 'ウインク'),
			_Utils_Tuple2('ういんどー', 'ウインドー'),
			_Utils_Tuple2('ういんどーしょっぴんぐ', 'ウインドーショッピング'),
			_Utils_Tuple2('うーまん', 'ウーマン'),
			_Utils_Tuple2('うーる', 'ウール'),
			_Utils_Tuple2('うぇあ', 'ウェア'),
			_Utils_Tuple2('うえーたー', 'ウエーター'),
			_Utils_Tuple2('うえーとれす', 'ウエートレス'),
			_Utils_Tuple2('うえーぶ', 'ウエーブ'),
			_Utils_Tuple2('うえき', '植木'),
			_Utils_Tuple2('うえでぃんぐ', 'ウエディング'),
			_Utils_Tuple2('うえでぃんぐどれす', 'ウエディングドレス'),
			_Utils_Tuple2('うえの', '上野'),
			_Utils_Tuple2('うぇぶ', 'ウェブ'),
			_Utils_Tuple2('うえる', '植える'),
			_Utils_Tuple2('うぉーきんぐ', 'ウォーキング'),
			_Utils_Tuple2('うぉーたー', 'ウォーター'),
			_Utils_Tuple2('うぉっか', 'ウォッカ'),
			_Utils_Tuple2('うぉっち', 'ウォッチ'),
			_Utils_Tuple2('うぉっちんぐ', 'ウォッチング'),
			_Utils_Tuple2('うかがう', '伺う'),
			_Utils_Tuple2('うかぶ', '浮かぶ'),
			_Utils_Tuple2('うかべる', '浮かべる'),
			_Utils_Tuple2('うかる', '受かる'),
			_Utils_Tuple2('うき', '雨期'),
			_Utils_Tuple2('うきあがる', '浮き上がる'),
			_Utils_Tuple2('うく', '浮く'),
			_Utils_Tuple2('うけ', '受け'),
			_Utils_Tuple2('うけ', '受け'),
			_Utils_Tuple2('うけいれ', '受け入れ'),
			_Utils_Tuple2('うけいれる', '受け入れる'),
			_Utils_Tuple2('うけたまわる', '承る'),
			_Utils_Tuple2('うけつぐ', '受け継ぐ'),
			_Utils_Tuple2('うけつけ', '受け付け'),
			_Utils_Tuple2('うけつける', '受け付ける'),
			_Utils_Tuple2('うけとめる', '受け止める'),
			_Utils_Tuple2('うけとり', '受け取り'),
			_Utils_Tuple2('うけとる', '受け取る'),
			_Utils_Tuple2('うけみ', '受け身'),
			_Utils_Tuple2('うけもつ', '受け持つ'),
			_Utils_Tuple2('うける', '受ける'),
			_Utils_Tuple2('うけわたし', '受け渡し'),
			_Utils_Tuple2('うごかす', '動かす'),
			_Utils_Tuple2('うごき', '動き'),
			_Utils_Tuple2('うごきまわる', '動き回る'),
			_Utils_Tuple2('うごく', '動く'),
			_Utils_Tuple2('うしなう', '失う'),
			_Utils_Tuple2('うしろあし', '後ろ足'),
			_Utils_Tuple2('うしろすがた', '後ろ姿'),
			_Utils_Tuple2('うしろむき', '後ろ向き'),
			_Utils_Tuple2('うすあじ', '薄味'),
			_Utils_Tuple2('うすぎ', '薄着'),
			_Utils_Tuple2('うすぎり', '薄切り'),
			_Utils_Tuple2('うすぐらい', '薄暗い'),
			_Utils_Tuple2('うすめ', '薄め'),
			_Utils_Tuple2('うすめる', '薄める'),
			_Utils_Tuple2('うせつ', '右折'),
			_Utils_Tuple2('うそ', 'うそ'),
			_Utils_Tuple2('うそつき', 'うそつき'),
			_Utils_Tuple2('うたがい', '疑い'),
			_Utils_Tuple2('うたがう', '疑う'),
			_Utils_Tuple2('うたごえ', '歌声'),
			_Utils_Tuple2('うち', '内'),
			_Utils_Tuple2('うちあける', '打ち明ける'),
			_Utils_Tuple2('うちあげる', '打ち上げる'),
			_Utils_Tuple2('うちあわせ', '打ち合わせ'),
			_Utils_Tuple2('うちがわ', '内側'),
			_Utils_Tuple2('うちき', '内気'),
			_Utils_Tuple2('うちゅう', '宇宙'),
			_Utils_Tuple2('うちゅうくうかん', '宇宙空間'),
			_Utils_Tuple2('うちゅうじん', '宇宙人'),
			_Utils_Tuple2('うちゅうせん', '宇宙船'),
			_Utils_Tuple2('うちゅうひこうし', '宇宙飛行士'),
			_Utils_Tuple2('うちゅうふく', '宇宙服'),
			_Utils_Tuple2('うちわ', 'うちわ'),
			_Utils_Tuple2('うつ', '打つ'),
			_Utils_Tuple2('うっかり', 'うっかり'),
			_Utils_Tuple2('うつくしい', '美しい'),
			_Utils_Tuple2('うつし', '写し'),
			_Utils_Tuple2('うつしだす', '映し出す'),
			_Utils_Tuple2('うつす', '移す'),
			_Utils_Tuple2('うつす', '写す'),
			_Utils_Tuple2('うつす', '映す'),
			_Utils_Tuple2('うったえ', '訴え'),
			_Utils_Tuple2('うったえる', '訴える'),
			_Utils_Tuple2('うつりかわり', '移り変わり'),
			_Utils_Tuple2('うつりかわる', '移り変わる'),
			_Utils_Tuple2('うつる', '移る'),
			_Utils_Tuple2('うつる', '映る'),
			_Utils_Tuple2('うつる', '写る'),
			_Utils_Tuple2('うつわ', '器'),
			_Utils_Tuple2('うでまえ', '腕前'),
			_Utils_Tuple2('うてん', '雨天'),
			_Utils_Tuple2('うとうと', 'うとうと'),
			_Utils_Tuple2('うなぎ', 'うなぎ'),
			_Utils_Tuple2('うなずく', 'うなずく'),
			_Utils_Tuple2('うばいとる', '奪い取る'),
			_Utils_Tuple2('うばう', '奪う'),
			_Utils_Tuple2('うまみ', '甘み'),
			_Utils_Tuple2('うまる', '埋まる'),
			_Utils_Tuple2('うまれ', '生まれ'),
			_Utils_Tuple2('うまれかわる', '生まれ変わる'),
			_Utils_Tuple2('うみだす', '生み出す'),
			_Utils_Tuple2('うみべ', '海辺'),
			_Utils_Tuple2('うむ', '生む'),
			_Utils_Tuple2('うむ', '有無'),
			_Utils_Tuple2('うめ', '梅'),
			_Utils_Tuple2('うめしゅ', '梅酒'),
			_Utils_Tuple2('うめたて', '埋め立て'),
			_Utils_Tuple2('うめたてる', '埋め立てる'),
			_Utils_Tuple2('うめぼし', '梅干し'),
			_Utils_Tuple2('うめる', '埋める'),
			_Utils_Tuple2('うら', '裏'),
			_Utils_Tuple2('うらおもて', '裏表'),
			_Utils_Tuple2('うらがえす', '裏返す'),
			_Utils_Tuple2('うらがわ', '裏側'),
			_Utils_Tuple2('うらぎり', '裏切り'),
			_Utils_Tuple2('うらぎる', '裏切る'),
			_Utils_Tuple2('うらない', '占い'),
			_Utils_Tuple2('うらなう', '占う'),
			_Utils_Tuple2('うらみ', '恨み'),
			_Utils_Tuple2('うらみち', '裏道'),
			_Utils_Tuple2('うらむ', '恨む'),
			_Utils_Tuple2('うらやま', '裏山'),
			_Utils_Tuple2('うらやましい', 'うらやましい'),
			_Utils_Tuple2('うり', '売り'),
			_Utils_Tuple2('うり', '売り'),
			_Utils_Tuple2('うりあげ', '売り上げ'),
			_Utils_Tuple2('うりきれ', '売り切れ'),
			_Utils_Tuple2('うりきれる', '売り切れる'),
			_Utils_Tuple2('うりこみ', '売り込み'),
			_Utils_Tuple2('うりこむ', '売り込む'),
			_Utils_Tuple2('うりだす', '売り出す'),
			_Utils_Tuple2('うりもの', '売り物'),
			_Utils_Tuple2('うれっこ', '売れっ子'),
			_Utils_Tuple2('うれのこり', '売れ残り'),
			_Utils_Tuple2('うれのこる', '売れ残る'),
			_Utils_Tuple2('うれゆき', '売れ行き'),
			_Utils_Tuple2('うろうろ', 'うろうろ'),
			_Utils_Tuple2('うわき', '浮気'),
			_Utils_Tuple2('うわぎ', '上着'),
			_Utils_Tuple2('うわさ', 'うわさ'),
			_Utils_Tuple2('うわさばなし', 'うわさばなし'),
			_Utils_Tuple2('うわまわる', '上回る'),
			_Utils_Tuple2('うん', 'うん'),
			_Utils_Tuple2('うん', '運'),
			_Utils_Tuple2('うんえい', '運営'),
			_Utils_Tuple2('うんが', '運河'),
			_Utils_Tuple2('うんきゅう', '運休'),
			_Utils_Tuple2('うんこ', 'うんこ'),
			_Utils_Tuple2('うんこう', '運行'),
			_Utils_Tuple2('うんこう', '運航'),
			_Utils_Tuple2('うんせい', '運勢'),
			_Utils_Tuple2('うんそう', '運送'),
			_Utils_Tuple2('うんち', 'うんち'),
			_Utils_Tuple2('うんちん', '運賃'),
			_Utils_Tuple2('うんてん', '運転'),
			_Utils_Tuple2('うんてんし', '運転士'),
			_Utils_Tuple2('うんてんしゅ', '運転手'),
			_Utils_Tuple2('うんてんめんきょしょう', '運転免許証'),
			_Utils_Tuple2('うんと', 'うんと'),
			_Utils_Tuple2('うんどうかい', '運動会'),
			_Utils_Tuple2('うんどうしんけい', '運動神経'),
			_Utils_Tuple2('うんめい', '運命'),
			_Utils_Tuple2('うんめいてき', '運命的'),
			_Utils_Tuple2('うんよう', '運用'),
			_Utils_Tuple2('えあらいん', 'エアライン'),
			_Utils_Tuple2('えあろびくす', 'エアロビクス'),
			_Utils_Tuple2('えい', '英'),
			_Utils_Tuple2('えい', 'えい'),
			_Utils_Tuple2('えいえん', '永遠'),
			_Utils_Tuple2('えいがか', '映画化'),
			_Utils_Tuple2('えいがかんとく', '映画監督'),
			_Utils_Tuple2('えいきゅう', '永久'),
			_Utils_Tuple2('えいきょう', '影響'),
			_Utils_Tuple2('えいぎょう', '営業'),
			_Utils_Tuple2('えいきょうりょく', '影響力'),
			_Utils_Tuple2('えいこく', '英国'),
			_Utils_Tuple2('えいじ', '英字'),
			_Utils_Tuple2('えいじゅう', '永住'),
			_Utils_Tuple2('えいず', 'ＡＩＤＳ'),
			_Utils_Tuple2('えいせい', '衛星'),
			_Utils_Tuple2('えいせい', '衛生'),
			_Utils_Tuple2('えいぞう', '映像'),
			_Utils_Tuple2('えいぶん', '英文'),
			_Utils_Tuple2('えいぶんがく', '英文学'),
			_Utils_Tuple2('えいやく', '英訳'),
			_Utils_Tuple2('えいゆう', '英雄'),
			_Utils_Tuple2('えいよう', '栄養'),
			_Utils_Tuple2('えいわ', '英和'),
			_Utils_Tuple2('えーがた', 'Ａ型'),
			_Utils_Tuple2('えーす', 'エース'),
			_Utils_Tuple2('えがお', '笑顔'),
			_Utils_Tuple2('えがく', '描く'),
			_Utils_Tuple2('えき', '液'),
			_Utils_Tuple2('えきたい', '液体'),
			_Utils_Tuple2('えきでん', '駅伝'),
			_Utils_Tuple2('えきべん', '駅弁'),
			_Utils_Tuple2('えきまえ', '駅前'),
			_Utils_Tuple2('えこのみー', 'エコノミー'),
			_Utils_Tuple2('えこのみーくらす', 'エコノミークラス'),
			_Utils_Tuple2('えさ', 'えさ'),
			_Utils_Tuple2('えしゃく', '会釈'),
			_Utils_Tuple2('えすえふ', 'ＳＦ'),
			_Utils_Tuple2('えすおーえす', 'ＳＯＳ'),
			_Utils_Tuple2('えすかれーた', 'エスカレーター'),
			_Utils_Tuple2('えすて', 'エステ'),
			_Utils_Tuple2('えすにっく', 'エスニック'),
			_Utils_Tuple2('えだ', '枝'),
			_Utils_Tuple2('えちけっと', 'エチケット'),
			_Utils_Tuple2('えっぐ', 'エッグ'),
			_Utils_Tuple2('えっせー', 'エッセー'),
			_Utils_Tuple2('えっと', 'えっと'),
			_Utils_Tuple2('えど', '江戸'),
			_Utils_Tuple2('えぬじー', 'ＮＧ'),
			_Utils_Tuple2('えねるぎー', 'エネルギー'),
			_Utils_Tuple2('えのぐ', '絵の具'),
			_Utils_Tuple2('えはがき', '絵葉書'),
			_Utils_Tuple2('えび', 'えび'),
			_Utils_Tuple2('えぴそーど', 'エピソード'),
			_Utils_Tuple2('えふえむ', 'ＦＭ'),
			_Utils_Tuple2('えふえむほうそう', 'ＦＭ放送'),
			_Utils_Tuple2('えぷろん', 'エプロン'),
			_Utils_Tuple2('えほん', '絵本'),
			_Utils_Tuple2('えむさいず', 'Ｍサイズ'),
			_Utils_Tuple2('えむぶいぴー', 'ＭＶＰ'),
			_Utils_Tuple2('えもじ', '絵文字'),
			_Utils_Tuple2('えらー', 'エラー'),
			_Utils_Tuple2('えらい', '偉い'),
			_Utils_Tuple2('えり', '襟'),
			_Utils_Tuple2('えりあ', 'エリア'),
			_Utils_Tuple2('えりーと', 'エリート'),
			_Utils_Tuple2('える', '得る'),
			_Utils_Tuple2('えるえる', 'ＬＬ'),
			_Utils_Tuple2('えるさいず', 'Ｌサイズ'),
			_Utils_Tuple2('えるでぃーけー', 'ＬＤＫ'),
			_Utils_Tuple2('えれきぎたー', 'エレキギター'),
			_Utils_Tuple2('えん', '円'),
			_Utils_Tuple2('えんか', '演歌'),
			_Utils_Tuple2('えんかい', '宴会'),
			_Utils_Tuple2('えんがん', '沿岸'),
			_Utils_Tuple2('えんき', '延期'),
			_Utils_Tuple2('えんぎ', '演技'),
			_Utils_Tuple2('えんきょり', '遠距離'),
			_Utils_Tuple2('えんぐらふ', '円グラフ'),
			_Utils_Tuple2('えんけい', '円形'),
			_Utils_Tuple2('えんげい', '園芸'),
			_Utils_Tuple2('えんげき', '演劇'),
			_Utils_Tuple2('えんじぇる', 'エンジェル'),
			_Utils_Tuple2('えんじにあ', 'エンジニア'),
			_Utils_Tuple2('えんしゅつ', '演出'),
			_Utils_Tuple2('えんじょ', '援助'),
			_Utils_Tuple2('えんじょい', 'エンジョイ'),
			_Utils_Tuple2('えんじん', 'エンジン'),
			_Utils_Tuple2('えんぜつ', '演説'),
			_Utils_Tuple2('えんせん', '沿線'),
			_Utils_Tuple2('えんそう', '演奏'),
			_Utils_Tuple2('えんそく', '遠足'),
			_Utils_Tuple2('えんたーていんめんと', 'エンターテインメント'),
			_Utils_Tuple2('えんだか', '円高'),
			_Utils_Tuple2('えんちょう', '園長'),
			_Utils_Tuple2('えんちょう', '延長'),
			_Utils_Tuple2('えんでぃんぐ', 'エンディング'),
			_Utils_Tuple2('えんど', 'エンド'),
			_Utils_Tuple2('えんとつ', '煙突'),
			_Utils_Tuple2('えんどれす', 'エンドレス'),
			_Utils_Tuple2('えんぶん', '塩分'),
			_Utils_Tuple2('えんぽう', '遠方'),
			_Utils_Tuple2('えんまん', '円満'),
			_Utils_Tuple2('えんりょ', '遠慮'),
			_Utils_Tuple2('お', 'お'),
			_Utils_Tuple2('お', 'オ'),
			_Utils_Tuple2('お', '尾'),
			_Utils_Tuple2('おあしす', 'オアシス'),
			_Utils_Tuple2('おい', 'おい'),
			_Utils_Tuple2('おいあげる', '追い上げる'),
			_Utils_Tuple2('おいおい', 'おいおい'),
			_Utils_Tuple2('おいかける', '追い掛ける'),
			_Utils_Tuple2('おいこし', '追い越し'),
			_Utils_Tuple2('おいこす', '追い越す'),
			_Utils_Tuple2('おいだす', '追い出す'),
			_Utils_Tuple2('おいつく', '追い付く'),
			_Utils_Tuple2('おいっこ', 'おいっ子'),
			_Utils_Tuple2('おいぬく', '追い抜く'),
			_Utils_Tuple2('おいもとめる', '追い求める'),
			_Utils_Tuple2('おいる', 'オイル'),
			_Utils_Tuple2('おいる', '老いる'),
			_Utils_Tuple2('おう', '王'),
			_Utils_Tuple2('おう', '欧'),
			_Utils_Tuple2('おう', 'おう'),
			_Utils_Tuple2('おう', '追う'),
			_Utils_Tuple2('おう', '負う'),
			_Utils_Tuple2('おういん', '押印'),
			_Utils_Tuple2('おうえん', '応援'),
			_Utils_Tuple2('おうけ', '王家'),
			_Utils_Tuple2('おうこく', '王国'),
			_Utils_Tuple2('おうごん', '黄金'),
			_Utils_Tuple2('おうさま', '王様'),
			_Utils_Tuple2('おうじ', '王子'),
			_Utils_Tuple2('おうしつ', '王室'),
			_Utils_Tuple2('おうじょ', '王女'),
			_Utils_Tuple2('おうしょく', '黄色'),
			_Utils_Tuple2('おうずる', '応ずる'),
			_Utils_Tuple2('おうせつしつ', '応接室'),
			_Utils_Tuple2('おうぞく', '王族'),
			_Utils_Tuple2('おうたい', '応対'),
			_Utils_Tuple2('おうだん', '横断'),
			_Utils_Tuple2('おうだんほどう', '横断歩道'),
			_Utils_Tuple2('おうとう', '応答'),
			_Utils_Tuple2('おうふう', '欧風'),
			_Utils_Tuple2('おうふく', '往復'),
			_Utils_Tuple2('おうべい', '欧米'),
			_Utils_Tuple2('おうぼ', '応募'),
			_Utils_Tuple2('おうよう', '応用'),
			_Utils_Tuple2('おうらい', '往来'),
			_Utils_Tuple2('おえる', '終える'),
			_Utils_Tuple2('おお', '大'),
			_Utils_Tuple2('おお', 'おお'),
			_Utils_Tuple2('おおあたり', '大当たり'),
			_Utils_Tuple2('おおあめ', '大雨'),
			_Utils_Tuple2('おーい', 'おーい'),
			_Utils_Tuple2('おおいそぎ', '大急ぎ'),
			_Utils_Tuple2('おおいた', '大分'),
			_Utils_Tuple2('おおいに', '大いに'),
			_Utils_Tuple2('おおう', '覆う'),
			_Utils_Tuple2('おーえる', 'ＯＬ'),
			_Utils_Tuple2('おおがた', '大型'),
			_Utils_Tuple2('おーがた', 'Ｏ型'),
			_Utils_Tuple2('おおかみ', 'おおかみ'),
			_Utils_Tuple2('おおく', '多く'),
			_Utils_Tuple2('おおく', '多く'),
			_Utils_Tuple2('おおぐい', '大食い'),
			_Utils_Tuple2('おーくしょん', 'オークション'),
			_Utils_Tuple2('おおぐち', '大口'),
			_Utils_Tuple2('おーけー', 'オーケー'),
			_Utils_Tuple2('おおげさ', '大げさ'),
			_Utils_Tuple2('おーけすとら', 'オーケストラ'),
			_Utils_Tuple2('おおごえ', '大声'),
			_Utils_Tuple2('おおさわぎ', '大騒ぎ'),
			_Utils_Tuple2('おーすとりあ', 'オーストリア'),
			_Utils_Tuple2('おおぜい', '大勢'),
			_Utils_Tuple2('おーだー', 'オーダー'),
			_Utils_Tuple2('おーでぃお', 'オーディオ'),
			_Utils_Tuple2('おーでぃしょん', 'オーディション'),
			_Utils_Tuple2('おおどおり', '大通り'),
			_Utils_Tuple2('おーどぶる', 'オードブル'),
			_Utils_Tuple2('おーとめーしょん', 'オートメーション'),
			_Utils_Tuple2('おーなー', 'オーナー'),
			_Utils_Tuple2('おーばー', 'オーバー'),
			_Utils_Tuple2('おおはば', '大幅'),
			_Utils_Tuple2('おーびー', 'ＯＢ'),
			_Utils_Tuple2('おーぷにんぐ', 'オープニング'),
			_Utils_Tuple2('おーぶん', 'オーブン'),
			_Utils_Tuple2('おーぷん', 'オープン'),
			_Utils_Tuple2('おおみそか', '大みそか'),
			_Utils_Tuple2('おおめ', '多め'),
			_Utils_Tuple2('おおもの', '大物'),
			_Utils_Tuple2('おおや', '大家'),
			_Utils_Tuple2('おおゆき', '大雪'),
			_Utils_Tuple2('おおよそ', 'おおよそ'),
			_Utils_Tuple2('おーるど', 'オールド'),
			_Utils_Tuple2('おーろら', 'オーロラ'),
			_Utils_Tuple2('おおわらい', '大笑い'),
			_Utils_Tuple2('おか', '丘'),
			_Utils_Tuple2('おかげ', 'おかげ'),
			_Utils_Tuple2('おかげさまで', 'おかげさまで'),
			_Utils_Tuple2('おかしい', 'おかしい'),
			_Utils_Tuple2('おかしな', 'おかしな'),
			_Utils_Tuple2('おかす', '犯す'),
			_Utils_Tuple2('おかず', 'おかず'),
			_Utils_Tuple2('おかゆ', 'おかゆ'),
			_Utils_Tuple2('おがわ', '小川'),
			_Utils_Tuple2('おき', '置き'),
			_Utils_Tuple2('おき', '起き'),
			_Utils_Tuple2('おきあがる', '起き上がる'),
			_Utils_Tuple2('おきかえる', '置き換える'),
			_Utils_Tuple2('おぎなう', '補う'),
			_Utils_Tuple2('おきば', '置き場'),
			_Utils_Tuple2('おくがい', '屋外'),
			_Utils_Tuple2('おくじょう', '屋上'),
			_Utils_Tuple2('おくない', '屋内'),
			_Utils_Tuple2('おくば', '奥歯'),
			_Utils_Tuple2('おくびょう', '臆病'),
			_Utils_Tuple2('おくらせる', '遅らせる'),
			_Utils_Tuple2('おくり', '送り'),
			_Utils_Tuple2('おくりかえす', '送り返す'),
			_Utils_Tuple2('おくりこむ', '送り込む'),
			_Utils_Tuple2('おくりだす', '送り出す'),
			_Utils_Tuple2('おくりとどける', '送り届ける'),
			_Utils_Tuple2('おくりぬし', '送り主'),
			_Utils_Tuple2('おくりむかえ', '送り迎え'),
			_Utils_Tuple2('おくりもの', '贈り物'),
			_Utils_Tuple2('おくる', '贈る'),
			_Utils_Tuple2('おくれ', '遅れ'),
			_Utils_Tuple2('おこす', '起こす'),
			_Utils_Tuple2('おこない', '行い'),
			_Utils_Tuple2('おこなう', '行う'),
			_Utils_Tuple2('おこのみやき', 'お好み焼き'),
			_Utils_Tuple2('おこり', '起こり'),
			_Utils_Tuple2('おこる', '起こる'),
			_Utils_Tuple2('おこる', '怒る'),
			_Utils_Tuple2('おさえ', '押さえ'),
			_Utils_Tuple2('おさえる', '押さえる'),
			_Utils_Tuple2('おさえる', '抑える'),
			_Utils_Tuple2('おさき', 'お先'),
			_Utils_Tuple2('おさない', '幼い'),
			_Utils_Tuple2('おさめる', '収める'),
			_Utils_Tuple2('おし', '押し'),
			_Utils_Tuple2('おしあう', '押し合う'),
			_Utils_Tuple2('おしい', '惜しい'),
			_Utils_Tuple2('おしいれ', '押し入れ'),
			_Utils_Tuple2('おしうり', '押し売り'),
			_Utils_Tuple2('おしえ', '教え'),
			_Utils_Tuple2('おじぎ', 'おじぎ'),
			_Utils_Tuple2('おしこむ', '押し込む'),
			_Utils_Tuple2('おしだす', '押し出す'),
			_Utils_Tuple2('おしつける', '押し付ける'),
			_Utils_Tuple2('おしっこ', 'おしっこ'),
			_Utils_Tuple2('おしまい', 'おしまい'),
			_Utils_Tuple2('おしむ', '惜しむ'),
			_Utils_Tuple2('おしゃべり', 'おしゃべり'),
			_Utils_Tuple2('おしゃれ', 'おしゃれ'),
			_Utils_Tuple2('おしよせる', '押し寄せる'),
			_Utils_Tuple2('おす', '雄'),
			_Utils_Tuple2('おせあにあ', 'オセアニア'),
			_Utils_Tuple2('おせいぼ', 'お歳暮'),
			_Utils_Tuple2('おせじ', 'お世辞'),
			_Utils_Tuple2('おせち', 'お節'),
			_Utils_Tuple2('おせわ', 'お世話'),
			_Utils_Tuple2('おせん', '汚染'),
			_Utils_Tuple2('おそう', '襲う'),
			_Utils_Tuple2('おそらく', '恐らく'),
			_Utils_Tuple2('おそれ', '恐れ'),
			_Utils_Tuple2('おそれる', '恐れる'),
			_Utils_Tuple2('おそろしい', '恐ろしい'),
			_Utils_Tuple2('おそわる', '教わる'),
			_Utils_Tuple2('おだやか', '穏やか'),
			_Utils_Tuple2('おち', '落ち'),
			_Utils_Tuple2('おちこむ', '落ち込む'),
			_Utils_Tuple2('おちつき', '落ち着き'),
			_Utils_Tuple2('おちつく', '落ち着く'),
			_Utils_Tuple2('おちば', '落ち葉'),
			_Utils_Tuple2('おっしゃる', 'おっしゃる'),
			_Utils_Tuple2('おっと', 'おっと'),
			_Utils_Tuple2('おつまみ', 'おつまみ'),
			_Utils_Tuple2('おでこ', 'おでこ'),
			_Utils_Tuple2('おでん', 'おでん'),
			_Utils_Tuple2('おとこまえ', '男前'),
			_Utils_Tuple2('おとこらしい', '男らしい'),
			_Utils_Tuple2('おとし', '落とし'),
			_Utils_Tuple2('おとしもの', '落とし物'),
			_Utils_Tuple2('おとずれ', '訪れ'),
			_Utils_Tuple2('おとずれる', '訪れる'),
			_Utils_Tuple2('おとなしい', 'おとなしい'),
			_Utils_Tuple2('おどり', '踊り'),
			_Utils_Tuple2('おとる', '劣る'),
			_Utils_Tuple2('おどる', '踊る'),
			_Utils_Tuple2('おとろえる', '衰える'),
			_Utils_Tuple2('おどろかす', '驚かす'),
			_Utils_Tuple2('おどろき', '驚き'),
			_Utils_Tuple2('おどろく', '驚く'),
			_Utils_Tuple2('おないどし', '同い年'),
			_Utils_Tuple2('おなじく', '同じく'),
			_Utils_Tuple2('おなら', 'おなら'),
			_Utils_Tuple2('おに', '鬼'),
			_Utils_Tuple2('おにおん', 'オニオン'),
			_Utils_Tuple2('おにごっこ', '鬼ごっこ'),
			_Utils_Tuple2('おはなし', '御話'),
			_Utils_Tuple2('おび', '帯'),
			_Utils_Tuple2('おふ', 'オフ'),
			_Utils_Tuple2('おふぃす', 'オフィス'),
			_Utils_Tuple2('おぺら', 'オペラ'),
			_Utils_Tuple2('おぺらはうす', 'オペラハウス'),
			_Utils_Tuple2('おぼえ', '覚え'),
			_Utils_Tuple2('おぼれる', 'おぼれる'),
			_Utils_Tuple2('おまえ', 'おまえ'),
			_Utils_Tuple2('おみくじ', 'おみくじ'),
			_Utils_Tuple2('おみやげ', 'お土産'),
			_Utils_Tuple2('おむらいす', 'オムライス'),
			_Utils_Tuple2('おむれつ', 'オムレツ'),
			_Utils_Tuple2('おめでたい', 'おめでたい'),
			_Utils_Tuple2('おめでとう', 'おめでとう'),
			_Utils_Tuple2('おもい', '思い'),
			_Utils_Tuple2('おもいうかぶ', '思い浮かぶ'),
			_Utils_Tuple2('おもいうかべる', '思い浮かべる'),
			_Utils_Tuple2('おもいきり', '思い切り'),
			_Utils_Tuple2('おもいきり', '思い切り'),
			_Utils_Tuple2('おもいだす', '思い出す'),
			_Utils_Tuple2('おもいつき', '思い付き'),
			_Utils_Tuple2('おもいつく', '思い付く'),
			_Utils_Tuple2('おもいで', '思い出'),
			_Utils_Tuple2('おもいとどまる', '思いとどまる'),
			_Utils_Tuple2('おもいなおす', '思い直す'),
			_Utils_Tuple2('おもしろおかしい', '面白おかしい'),
			_Utils_Tuple2('おもしろみ', '面白み'),
			_Utils_Tuple2('おもたい', '重たい'),
			_Utils_Tuple2('おもちゃ', 'おもちゃ'),
			_Utils_Tuple2('おもて', '表'),
			_Utils_Tuple2('おもな', '主な'),
			_Utils_Tuple2('おもなが', '面長'),
			_Utils_Tuple2('おもに', '主に'),
			_Utils_Tuple2('おもみ', '重み'),
			_Utils_Tuple2('おやこ', '親子'),
			_Utils_Tuple2('おやこうこう', '親孝行'),
			_Utils_Tuple2('おやごころ', '親心'),
			_Utils_Tuple2('おやこどんぶり', '親子どんぶり'),
			_Utils_Tuple2('おやじ', 'おやじ'),
			_Utils_Tuple2('おやつ', 'おやつ'),
			_Utils_Tuple2('おやふこう', '親不孝'),
			_Utils_Tuple2('おやゆび', '親指'),
			_Utils_Tuple2('およぎ', '泳ぎ'),
			_Utils_Tuple2('およそ', 'およそ'),
			_Utils_Tuple2('および', '及び'),
			_Utils_Tuple2('およぶ', '及ぶ'),
			_Utils_Tuple2('およぼす', '及ぼす'),
			_Utils_Tuple2('おらんうーたん', 'オランウータン'),
			_Utils_Tuple2('おらんだ', 'オランダ'),
			_Utils_Tuple2('おらんだご', 'オランダ語'),
			_Utils_Tuple2('おりーぶ', 'オリーブ'),
			_Utils_Tuple2('おりがみ', '折り紙'),
			_Utils_Tuple2('おりじなる', 'オリジナル'),
			_Utils_Tuple2('おりづる', '折りづる'),
			_Utils_Tuple2('おりまげる', '折り曲げる'),
			_Utils_Tuple2('おりんぴっく', 'オリンピック'),
			_Utils_Tuple2('おる', '折る'),
			_Utils_Tuple2('おるがん', 'オルガン'),
			_Utils_Tuple2('おるごーる', 'オルゴール'),
			_Utils_Tuple2('おれ', 'おれ'),
			_Utils_Tuple2('おれせん', '折れ線'),
			_Utils_Tuple2('おれる', '折れる'),
			_Utils_Tuple2('おろす', '下ろす'),
			_Utils_Tuple2('おろす', '降ろす'),
			_Utils_Tuple2('おわび', 'おわび'),
			_Utils_Tuple2('おわん', 'おわん'),
			_Utils_Tuple2('おん', '音'),
			_Utils_Tuple2('おん', '恩'),
			_Utils_Tuple2('おん', '御'),
			_Utils_Tuple2('おん', 'オン'),
			_Utils_Tuple2('おんがくか', '音楽家'),
			_Utils_Tuple2('おんくん', '音訓'),
			_Utils_Tuple2('おんしつ', '温室'),
			_Utils_Tuple2('おんしつこうか', '温室効果'),
			_Utils_Tuple2('おんすい', '温水'),
			_Utils_Tuple2('おんたい', '温帯'),
			_Utils_Tuple2('おんだん', '温暖'),
			_Utils_Tuple2('おんち', '音痴'),
			_Utils_Tuple2('おんちゅう', '御中'),
			_Utils_Tuple2('おんど', '温度'),
			_Utils_Tuple2('おんどく', '音読'),
			_Utils_Tuple2('おんどけい', '温度計'),
			_Utils_Tuple2('おんならしい', '女らしい'),
			_Utils_Tuple2('おんぷう', '温風'),
			_Utils_Tuple2('おんらいん', 'オンライン'),
			_Utils_Tuple2('おんりー', 'オンリー'),
			_Utils_Tuple2('おんりょう', '音量'),
			_Utils_Tuple2('おんわ', '温和'),
			_Utils_Tuple2('か', '化'),
			_Utils_Tuple2('か', '蚊'),
			_Utils_Tuple2('か', '科'),
			_Utils_Tuple2('か', '彼'),
			_Utils_Tuple2('か', '課'),
			_Utils_Tuple2('か', '下'),
			_Utils_Tuple2('か', '過'),
			_Utils_Tuple2('か', '可'),
			_Utils_Tuple2('が', '画'),
			_Utils_Tuple2('かー', 'カー'),
			_Utils_Tuple2('かあさん', '母さん'),
			_Utils_Tuple2('がーぜ', 'ガーゼ'),
			_Utils_Tuple2('かーそる', 'カーソル'),
			_Utils_Tuple2('かあちゃん', '母ちゃん'),
			_Utils_Tuple2('かーでぃがん', 'カーディガン'),
			_Utils_Tuple2('がーでにんぐ', 'ガーデニング'),
			_Utils_Tuple2('かーてん', 'カーテン'),
			_Utils_Tuple2('がーでん', 'ガーデン'),
			_Utils_Tuple2('かーと', 'カート'),
			_Utils_Tuple2('がーど', 'ガード'),
			_Utils_Tuple2('がーどまん', 'ガードマン'),
			_Utils_Tuple2('がーどれーる', 'ガードレール'),
			_Utils_Tuple2('かーにばる', 'カーニバル'),
			_Utils_Tuple2('かーぺっと', 'カーペット'),
			_Utils_Tuple2('がーりっく', 'ガーリック'),
			_Utils_Tuple2('かーる', 'カール'),
			_Utils_Tuple2('がーる', 'ガール'),
			_Utils_Tuple2('かい', '会'),
			_Utils_Tuple2('かい', '貝'),
			_Utils_Tuple2('かい', '階'),
			_Utils_Tuple2('かい', '界'),
			_Utils_Tuple2('かい', '快'),
			_Utils_Tuple2('かい', '解'),
			_Utils_Tuple2('がい', '外'),
			_Utils_Tuple2('がい', '街'),
			_Utils_Tuple2('がい', '害'),
			_Utils_Tuple2('かいいん', '会員'),
			_Utils_Tuple2('かいえん', '開演'),
			_Utils_Tuple2('かいおき', '買い置き'),
			_Utils_Tuple2('かいか', '開花'),
			_Utils_Tuple2('かいが', '絵画'),
			_Utils_Tuple2('かいかい', '開会'),
			_Utils_Tuple2('かいがい', '海外'),
			_Utils_Tuple2('かいがいぼうえき', '海外貿易'),
			_Utils_Tuple2('かいかえる', '買い換える'),
			_Utils_Tuple2('かいかく', '改革'),
			_Utils_Tuple2('かいかた', '買い方'),
			_Utils_Tuple2('かいかん', '会館'),
			_Utils_Tuple2('かいかん', '快感'),
			_Utils_Tuple2('かいかん', '開館'),
			_Utils_Tuple2('かいがん', '海岸'),
			_Utils_Tuple2('がいかん', '外観'),
			_Utils_Tuple2('かいがんせん', '海岸線'),
			_Utils_Tuple2('かいぎ', '会議'),
			_Utils_Tuple2('がいき', '外気'),
			_Utils_Tuple2('かいぎしつ', '会議室'),
			_Utils_Tuple2('かいぎょう', '開業'),
			_Utils_Tuple2('かいぐん', '海軍'),
			_Utils_Tuple2('かいけい', '会計'),
			_Utils_Tuple2('かいけつ', '解決'),
			_Utils_Tuple2('かいけん', '会見'),
			_Utils_Tuple2('がいけん', '外見'),
			_Utils_Tuple2('かいこ', '解雇'),
			_Utils_Tuple2('かいご', '介護'),
			_Utils_Tuple2('かいこう', '開講'),
			_Utils_Tuple2('がいこう', '外交'),
			_Utils_Tuple2('がいこうかん', '外交官'),
			_Utils_Tuple2('かいこく', '開国'),
			_Utils_Tuple2('がいこくぶんがく', '外国文学'),
			_Utils_Tuple2('かいさつ', '改札'),
			_Utils_Tuple2('かいさつぐち', '改札口'),
			_Utils_Tuple2('かいさん', '解散'),
			_Utils_Tuple2('かいし', '開始'),
			_Utils_Tuple2('がいしゃ', '外車'),
			_Utils_Tuple2('かいしゅう', '回収'),
			_Utils_Tuple2('がいしゅつ', '外出'),
			_Utils_Tuple2('かいじょ', '解除'),
			_Utils_Tuple2('かいしょう', '解消'),
			_Utils_Tuple2('かいじょう', '会場'),
			_Utils_Tuple2('かいじょう', '海上'),
			_Utils_Tuple2('かいじょう', '開場'),
			_Utils_Tuple2('がいしょう', '外相'),
			_Utils_Tuple2('かいしょく', '会食'),
			_Utils_Tuple2('がいしょく', '外食'),
			_Utils_Tuple2('がいしょくさんぎょう', '外食産業'),
			_Utils_Tuple2('がいじん', '外人'),
			_Utils_Tuple2('かいすい', '海水'),
			_Utils_Tuple2('かいすいよく', '海水浴'),
			_Utils_Tuple2('かいすう', '回数'),
			_Utils_Tuple2('かいすうけん', '回数券'),
			_Utils_Tuple2('がいする', '害する'),
			_Utils_Tuple2('かいせい', '改正'),
			_Utils_Tuple2('かいせい', '快晴'),
			_Utils_Tuple2('かいせつ', '解説'),
			_Utils_Tuple2('かいせつ', '開設'),
			_Utils_Tuple2('かいせん', '回線'),
			_Utils_Tuple2('かいぜん', '改善'),
			_Utils_Tuple2('かいそう', '改装'),
			_Utils_Tuple2('かいそう', '海草'),
			_Utils_Tuple2('かいそう', '回送'),
			_Utils_Tuple2('かいそう', '回想'),
			_Utils_Tuple2('かいそう', '海藻'),
			_Utils_Tuple2('かいぞう', '改造'),
			_Utils_Tuple2('がいそう', '外装'),
			_Utils_Tuple2('かいそく', '快速'),
			_Utils_Tuple2('かいたい', '解体'),
			_Utils_Tuple2('かいだし', '買い出し'),
			_Utils_Tuple2('かいたす', '買い足す'),
			_Utils_Tuple2('かいだめ', '買いだめ'),
			_Utils_Tuple2('かいだん', '怪談'),
			_Utils_Tuple2('かいだん', '会談'),
			_Utils_Tuple2('かいちく', '改築'),
			_Utils_Tuple2('かいちゅう', '海中'),
			_Utils_Tuple2('がいちゅう', '害虫'),
			_Utils_Tuple2('かいちゅうでんとう', '懐中電灯'),
			_Utils_Tuple2('かいちょう', '会長'),
			_Utils_Tuple2('かいちょう', '快調'),
			_Utils_Tuple2('かいつう', '開通'),
			_Utils_Tuple2('かいて', '買い手'),
			_Utils_Tuple2('かいてい', '改定'),
			_Utils_Tuple2('かいてい', '海底'),
			_Utils_Tuple2('かいてき', '快適'),
			_Utils_Tuple2('がいてき', '外的'),
			_Utils_Tuple2('かいてん', '開店'),
			_Utils_Tuple2('かいてん', '回転'),
			_Utils_Tuple2('かいてん', '回転'),
			_Utils_Tuple2('がいど', 'ガイド'),
			_Utils_Tuple2('かいとう', '解答'),
			_Utils_Tuple2('かいとう', '解凍'),
			_Utils_Tuple2('かいとう', '回答'),
			_Utils_Tuple2('かいどう', '街道'),
			_Utils_Tuple2('かいどき', '買い時'),
			_Utils_Tuple2('がいどぶっく', 'ガイドブック'),
			_Utils_Tuple2('かいとり', '買い取り'),
			_Utils_Tuple2('かいとる', '買い取る'),
			_Utils_Tuple2('かいぬし', '飼い主'),
			_Utils_Tuple2('がいはく', '外泊'),
			_Utils_Tuple2('かいはつ', '開発'),
			_Utils_Tuple2('かいはつとじょうこく', '開発途上国'),
			_Utils_Tuple2('かいひ', '会費'),
			_Utils_Tuple2('がいぶ', '外部'),
			_Utils_Tuple2('かいふう', '開封'),
			_Utils_Tuple2('かいふく', '回復'),
			_Utils_Tuple2('かいぶつ', '怪物'),
			_Utils_Tuple2('かいへい', '開閉'),
			_Utils_Tuple2('がいへき', '外壁'),
			_Utils_Tuple2('かいほう', '解放'),
			_Utils_Tuple2('かいほう', '開放'),
			_Utils_Tuple2('かいほうてき', '開放的'),
			_Utils_Tuple2('かいまく', '開幕'),
			_Utils_Tuple2('がいむしょう', '外務省'),
			_Utils_Tuple2('がいむだいじん', '外務大臣'),
			_Utils_Tuple2('かいめい', '解明'),
			_Utils_Tuple2('かいめん', '海面'),
			_Utils_Tuple2('かいやく', '解約'),
			_Utils_Tuple2('がいらい', '外来'),
			_Utils_Tuple2('がいらいご', '外来語'),
			_Utils_Tuple2('かいらん', '回覧'),
			_Utils_Tuple2('かいりょう', '改良'),
			_Utils_Tuple2('かいわぶん', '会話文'),
			_Utils_Tuple2('かうんせらー', 'カウンセラー'),
			_Utils_Tuple2('かうんせりんぐ', 'カウンセリング'),
			_Utils_Tuple2('かうんたー', 'カウンター'),
			_Utils_Tuple2('かうんと', 'カウント'),
			_Utils_Tuple2('かえ', '替え'),
			_Utils_Tuple2('かえうた', '替え歌'),
			_Utils_Tuple2('かえし', '返し'),
			_Utils_Tuple2('かえす', '帰す'),
			_Utils_Tuple2('かえって', 'かえって'),
			_Utils_Tuple2('かえりみち', '帰り道'),
			_Utils_Tuple2('かえる', 'かえる'),
			_Utils_Tuple2('かえる', '返る'),
			_Utils_Tuple2('かえる', '換える'),
			_Utils_Tuple2('かえる', '替える'),
			_Utils_Tuple2('かおいろ', '顔色'),
			_Utils_Tuple2('かおじゃしん', '顔写真'),
			_Utils_Tuple2('かおり', '香り'),
			_Utils_Tuple2('かおる', '香る'),
			_Utils_Tuple2('がか', '画家'),
			_Utils_Tuple2('かがいしゃ', '加害者'),
			_Utils_Tuple2('かかえる', '抱える'),
			_Utils_Tuple2('かかお', 'カカオ'),
			_Utils_Tuple2('かかく', '価格'),
			_Utils_Tuple2('かがく', '科学'),
			_Utils_Tuple2('かがく', '化学'),
			_Utils_Tuple2('かがくしゃ', '科学者'),
			_Utils_Tuple2('かがくてき', '科学的'),
			_Utils_Tuple2('かがくてき', '化学的'),
			_Utils_Tuple2('かかげる', '掲げる'),
			_Utils_Tuple2('かかと', 'かかと'),
			_Utils_Tuple2('かがやき', '輝き'),
			_Utils_Tuple2('かがやく', '輝く'),
			_Utils_Tuple2('かかり', '係'),
			_Utils_Tuple2('かかりいん', '係員'),
			_Utils_Tuple2('かかりちょう', '係長'),
			_Utils_Tuple2('かかる', '掛かる'),
			_Utils_Tuple2('かかる', '係る'),
			_Utils_Tuple2('かかわる', 'かかわる'),
			_Utils_Tuple2('かき', '書き'),
			_Utils_Tuple2('かき', '柿'),
			_Utils_Tuple2('かき', '夏期'),
			_Utils_Tuple2('かき', '下記'),
			_Utils_Tuple2('かき', 'かき'),
			_Utils_Tuple2('かき', '火気'),
			_Utils_Tuple2('かき', '夏季'),
			_Utils_Tuple2('かきかえる', '書き替える'),
			_Utils_Tuple2('かきくわえる', '書き加える'),
			_Utils_Tuple2('かきこむ', '書き込む'),
			_Utils_Tuple2('かきとめ', '書留'),
			_Utils_Tuple2('かきとる', '書き取る'),
			_Utils_Tuple2('かきなおす', '書き直す'),
			_Utils_Tuple2('かきまわす', 'かき回す'),
			_Utils_Tuple2('かぎり', '限り'),
			_Utils_Tuple2('かぎりない', '限り無い'),
			_Utils_Tuple2('かぎる', '限る'),
			_Utils_Tuple2('かく', 'かく'),
			_Utils_Tuple2('かく', '各'),
			_Utils_Tuple2('かく', '核'),
			_Utils_Tuple2('かく', '角'),
			_Utils_Tuple2('かく', '欠く'),
			_Utils_Tuple2('かぐ', 'かぐ'),
			_Utils_Tuple2('がく', '学'),
			_Utils_Tuple2('がく', '楽'),
			_Utils_Tuple2('がく', '額'),
			_Utils_Tuple2('がく', '学'),
			_Utils_Tuple2('がくいん', '学院'),
			_Utils_Tuple2('かくう', '架空'),
			_Utils_Tuple2('かくえき', '各駅'),
			_Utils_Tuple2('かくえきていしゃ', '各駅停車'),
			_Utils_Tuple2('がくえん', '学園'),
			_Utils_Tuple2('かくかぞく', '核家族'),
			_Utils_Tuple2('がくぎょう', '学業'),
			_Utils_Tuple2('かくご', '覚悟'),
			_Utils_Tuple2('かくさ', '格差'),
			_Utils_Tuple2('かくじ', '各自'),
			_Utils_Tuple2('かくじつ', '確実'),
			_Utils_Tuple2('かくじっけん', '核実験'),
			_Utils_Tuple2('がくしゃ', '学者'),
			_Utils_Tuple2('がくしゃ', '学者'),
			_Utils_Tuple2('かくしゅ', '各種'),
			_Utils_Tuple2('がくしゅう', '学習'),
			_Utils_Tuple2('がくしゅうじゅく', '学習塾'),
			_Utils_Tuple2('かくしん', '確信'),
			_Utils_Tuple2('かくじん', '各人'),
			_Utils_Tuple2('かくす', '隠す'),
			_Utils_Tuple2('がくせいうんどう', '学生運動'),
			_Utils_Tuple2('かくだい', '拡大'),
			_Utils_Tuple2('かくち', '各地'),
			_Utils_Tuple2('がくちょう', '学長'),
			_Utils_Tuple2('かくてる', 'カクテル'),
			_Utils_Tuple2('かくど', '角度'),
			_Utils_Tuple2('がくどう', '学童'),
			_Utils_Tuple2('かくとく', '獲得'),
			_Utils_Tuple2('がくない', '学内'),
			_Utils_Tuple2('かくにん', '確認'),
			_Utils_Tuple2('がくねん', '学年'),
			_Utils_Tuple2('がくぶ', '学部'),
			_Utils_Tuple2('がくぶ', '学部'),
			_Utils_Tuple2('かくめい', '革命'),
			_Utils_Tuple2('がくもん', '学問'),
			_Utils_Tuple2('がくもんてき', '学問的'),
			_Utils_Tuple2('かくやす', '格安'),
			_Utils_Tuple2('かくりつ', '確率'),
			_Utils_Tuple2('がくりょく', '学力'),
			_Utils_Tuple2('かくれ', '隠れ'),
			_Utils_Tuple2('がくれき', '学歴'),
			_Utils_Tuple2('がくれきしゃかい', '学歴社会'),
			_Utils_Tuple2('かくれる', '隠れる'),
			_Utils_Tuple2('がくわり', '学割'),
			_Utils_Tuple2('かけ', '掛け'),
			_Utils_Tuple2('かげ', '影'),
			_Utils_Tuple2('かげ', '陰'),
			_Utils_Tuple2('かけい', '家計'),
			_Utils_Tuple2('かけこむ', '駆け込む'),
			_Utils_Tuple2('かけざん', '掛け算'),
			_Utils_Tuple2('かけつ', '可決'),
			_Utils_Tuple2('かける', '掛ける'),
			_Utils_Tuple2('かける', '欠ける'),
			_Utils_Tuple2('かける', '賭ける'),
			_Utils_Tuple2('かげん', '加減'),
			_Utils_Tuple2('かこ', '過去'),
			_Utils_Tuple2('かご', '籠'),
			_Utils_Tuple2('かこい', '囲い'),
			_Utils_Tuple2('かこう', '下降'),
			_Utils_Tuple2('かこう', '加工'),
			_Utils_Tuple2('かこう', '囲う'),
			_Utils_Tuple2('かごう', '化合'),
			_Utils_Tuple2('かこむ', '囲む'),
			_Utils_Tuple2('かさい', '火災'),
			_Utils_Tuple2('かさいほけん', '火災保険'),
			_Utils_Tuple2('かさかさ', 'かさかさ'),
			_Utils_Tuple2('かさなる', '重なる'),
			_Utils_Tuple2('かさね', '重ね'),
			_Utils_Tuple2('かさねぎ', '重ね着'),
			_Utils_Tuple2('かさねる', '重ねる'),
			_Utils_Tuple2('かざり', '飾り'),
			_Utils_Tuple2('かざる', '飾る'),
			_Utils_Tuple2('かさん', '加算'),
			_Utils_Tuple2('かざん', '火山'),
			_Utils_Tuple2('かざんたい', '火山帯'),
			_Utils_Tuple2('かし', '貸し'),
			_Utils_Tuple2('かし', '歌詞'),
			_Utils_Tuple2('かじ', '家事'),
			_Utils_Tuple2('かしかり', '貸し借り'),
			_Utils_Tuple2('かしきる', '貸し切る'),
			_Utils_Tuple2('かしこい', '賢い'),
			_Utils_Tuple2('かしだし', '貸し出し'),
			_Utils_Tuple2('かしだす', '貸し出す'),
			_Utils_Tuple2('かしつ', '過失'),
			_Utils_Tuple2('かじつ', '果実'),
			_Utils_Tuple2('がしつ', '画質'),
			_Utils_Tuple2('かじつしゅ', '果実酒'),
			_Utils_Tuple2('かじゅある', 'カジュアル'),
			_Utils_Tuple2('かじゅう', '果汁'),
			_Utils_Tuple2('かしょ', '箇所'),
			_Utils_Tuple2('かしょ', '箇所'),
			_Utils_Tuple2('かじょうかき', '箇条書き'),
			_Utils_Tuple2('かしら', '頭'),
			_Utils_Tuple2('がす', 'ガス'),
			_Utils_Tuple2('かずかず', '数々'),
			_Utils_Tuple2('かすてら', 'カステラ'),
			_Utils_Tuple2('かせい', '火星'),
			_Utils_Tuple2('かぜい', '課税'),
			_Utils_Tuple2('かせき', '化石'),
			_Utils_Tuple2('かせぐ', '稼ぐ'),
			_Utils_Tuple2('かせつ', '仮説'),
			_Utils_Tuple2('かせっと', 'カセット'),
			_Utils_Tuple2('かせっとてーぷ', 'カセットテープ'),
			_Utils_Tuple2('かぜとおし', '風通し'),
			_Utils_Tuple2('かせん', '下線'),
			_Utils_Tuple2('かせん', '河川'),
			_Utils_Tuple2('かそう', '仮想'),
			_Utils_Tuple2('かそう', '火葬'),
			_Utils_Tuple2('がぞう', '画像'),
			_Utils_Tuple2('かぞえる', '数える'),
			_Utils_Tuple2('かそく', '加速'),
			_Utils_Tuple2('かぞくせいど', '家族制度'),
			_Utils_Tuple2('がそりん', 'ガソリン'),
			_Utils_Tuple2('がそりんえんじん', 'ガソリンエンジン'),
			_Utils_Tuple2('かた', '方'),
			_Utils_Tuple2('かた', '方'),
			_Utils_Tuple2('かた', '型'),
			_Utils_Tuple2('がた', '型'),
			_Utils_Tuple2('がた', '形'),
			_Utils_Tuple2('かたあし', '片足'),
			_Utils_Tuple2('かたい', '硬い'),
			_Utils_Tuple2('かたい', '固い'),
			_Utils_Tuple2('かたい', '堅い'),
			_Utils_Tuple2('かだい', '課題'),
			_Utils_Tuple2('かだい', '過大'),
			_Utils_Tuple2('がたい', '難い'),
			_Utils_Tuple2('かたおもい', '片思い'),
			_Utils_Tuple2('かたがき', '肩書き'),
			_Utils_Tuple2('かたがた', '方々'),
			_Utils_Tuple2('がたがた', 'がたがた'),
			_Utils_Tuple2('がたがた', 'がたがた'),
			_Utils_Tuple2('かたがわ', '片側'),
			_Utils_Tuple2('かたこり', '肩凝り'),
			_Utils_Tuple2('かたち', '形'),
			_Utils_Tuple2('かたづく', '片付く'),
			_Utils_Tuple2('かたづけ', '片付け'),
			_Utils_Tuple2('かたづける', '片付ける'),
			_Utils_Tuple2('かたて', '片手'),
			_Utils_Tuple2('かたな', '刀'),
			_Utils_Tuple2('かたほう', '片方'),
			_Utils_Tuple2('かたまる', '固まる'),
			_Utils_Tuple2('かたみち', '片道'),
			_Utils_Tuple2('かたむく', '傾く'),
			_Utils_Tuple2('かたむける', '傾ける'),
			_Utils_Tuple2('かため', '片目'),
			_Utils_Tuple2('かためる', '固める'),
			_Utils_Tuple2('かためん', '片面'),
			_Utils_Tuple2('かたより', '偏り'),
			_Utils_Tuple2('かたよる', '偏る'),
			_Utils_Tuple2('かたる', '語る'),
			_Utils_Tuple2('かたろぐ', 'カタログ'),
			_Utils_Tuple2('かだん', '花壇'),
			_Utils_Tuple2('かち', '価値'),
			_Utils_Tuple2('かち', '勝ち'),
			_Utils_Tuple2('がち', '勝ち'),
			_Utils_Tuple2('かちかち', 'かちかち'),
			_Utils_Tuple2('かちかん', '価値観'),
			_Utils_Tuple2('かちとる', '勝ち取る'),
			_Utils_Tuple2('かちのこる', '勝ち残る'),
			_Utils_Tuple2('かちまけ', '勝ち負け'),
			_Utils_Tuple2('がちゃがちゃ', 'がちゃがちゃ'),
			_Utils_Tuple2('かちょう', '課長'),
			_Utils_Tuple2('かちょう', '課長'),
			_Utils_Tuple2('かつ', '勝つ'),
			_Utils_Tuple2('かつ', 'カツ'),
			_Utils_Tuple2('かつ', '且つ'),
			_Utils_Tuple2('がっか', '学科'),
			_Utils_Tuple2('がっか', '学科'),
			_Utils_Tuple2('がっかい', '学会'),
			_Utils_Tuple2('がっかり', 'がっかり'),
			_Utils_Tuple2('かっき', '活気'),
			_Utils_Tuple2('がっき', '楽器'),
			_Utils_Tuple2('がっきゅう', '学級'),
			_Utils_Tuple2('がっきゅういいん', '学級委員'),
			_Utils_Tuple2('かっこ', '括弧'),
			_Utils_Tuple2('かっこう', '格好'),
			_Utils_Tuple2('かっこく', '各国'),
			_Utils_Tuple2('がっしゅうこく', '合衆国'),
			_Utils_Tuple2('がっしゅく', '合宿'),
			_Utils_Tuple2('がっしょう', '合唱'),
			_Utils_Tuple2('かっせいか', '活性化'),
			_Utils_Tuple2('かったー', 'カッター'),
			_Utils_Tuple2('かって', '勝手'),
			_Utils_Tuple2('かつて', 'かつて'),
			_Utils_Tuple2('かっと', 'カット'),
			_Utils_Tuple2('かつどう', '活動'),
			_Utils_Tuple2('かっぱ', '合羽'),
			_Utils_Tuple2('かっぱつ', '活発'),
			_Utils_Tuple2('かっぷけーき', 'カップケーキ'),
			_Utils_Tuple2('かっぷる', 'カップル'),
			_Utils_Tuple2('かつやく', '活躍'),
			_Utils_Tuple2('かつよう', '活用'),
			_Utils_Tuple2('かつら', '鬘'),
			_Utils_Tuple2('かつりょく', '活力'),
			_Utils_Tuple2('かてい', '家庭'),
			_Utils_Tuple2('かてい', '過程'),
			_Utils_Tuple2('かてい', '課程'),
			_Utils_Tuple2('かてい', '仮定'),
			_Utils_Tuple2('かていか', '家庭科'),
			_Utils_Tuple2('かていきょうし', '家庭教師'),
			_Utils_Tuple2('かていてき', '家庭的'),
			_Utils_Tuple2('かでん', '家電'),
			_Utils_Tuple2('かど', '過度'),
			_Utils_Tuple2('かど', '過度'),
			_Utils_Tuple2('かどう', '華道'),
			_Utils_Tuple2('かとりっく', 'カトリック'),
			_Utils_Tuple2('かな', '仮名'),
			_Utils_Tuple2('かない', '家内'),
			_Utils_Tuple2('かなう', 'かなう'),
			_Utils_Tuple2('かなえる', 'かなえる'),
			_Utils_Tuple2('かなしみ', '悲しみ'),
			_Utils_Tuple2('かなしむ', '悲しむ'),
			_Utils_Tuple2('かなた', 'かなた'),
			_Utils_Tuple2('かならず', '必ず'),
			_Utils_Tuple2('かならずしも', '必ずしも'),
			_Utils_Tuple2('かなり', 'かなり'),
			_Utils_Tuple2('かに', 'かに'),
			_Utils_Tuple2('かにゅう', '加入'),
			_Utils_Tuple2('かね', '鐘'),
			_Utils_Tuple2('かねそなえる', '兼ね備える'),
			_Utils_Tuple2('かねつ', '加熱'),
			_Utils_Tuple2('かねもうけ', '金もうけ'),
			_Utils_Tuple2('かねる', '兼ねる'),
			_Utils_Tuple2('かねる', '兼ねる'),
			_Utils_Tuple2('かねん', '可燃'),
			_Utils_Tuple2('かねんせい', '可燃性'),
			_Utils_Tuple2('かのう', '可能'),
			_Utils_Tuple2('かのう', '可能'),
			_Utils_Tuple2('かのうせい', '可能性'),
			_Utils_Tuple2('かばー', 'カバー'),
			_Utils_Tuple2('かはんしん', '下半身'),
			_Utils_Tuple2('かはんすう', '過半数'),
			_Utils_Tuple2('かびん', '花瓶'),
			_Utils_Tuple2('かぶ', '下部'),
			_Utils_Tuple2('かふぇてりあ', 'カフェテリア'),
			_Utils_Tuple2('かぶき', '歌舞伎'),
			_Utils_Tuple2('かぶしき', '株式'),
			_Utils_Tuple2('かぷせる', 'カプセル'),
			_Utils_Tuple2('かぶる', 'かぶる'),
			_Utils_Tuple2('かふん', '花粉'),
			_Utils_Tuple2('かふんしょう', '花粉症'),
			_Utils_Tuple2('かぼちゃ', 'カボチャ'),
			_Utils_Tuple2('かま', 'かま'),
			_Utils_Tuple2('かまう', '構う'),
			_Utils_Tuple2('かまくら', '鎌倉'),
			_Utils_Tuple2('かまぼこ', '蒲鉾'),
			_Utils_Tuple2('がまん', '我慢'),
			_Utils_Tuple2('かみ', '上'),
			_Utils_Tuple2('かみがた', '髪型'),
			_Utils_Tuple2('がみがみ', 'がみがみ'),
			_Utils_Tuple2('かみそり', 'かみそり'),
			_Utils_Tuple2('かみなり', '雷'),
			_Utils_Tuple2('かみぶくろ', '紙袋'),
			_Utils_Tuple2('かみん', '仮眠'),
			_Utils_Tuple2('かむ', 'かむ'),
			_Utils_Tuple2('がむてーぷ', 'ガムテープ'),
			_Utils_Tuple2('かめ', '亀'),
			_Utils_Tuple2('かめい', '加盟'),
			_Utils_Tuple2('かめん', '仮面'),
			_Utils_Tuple2('がめん', '画面'),
			_Utils_Tuple2('かもく', '科目'),
			_Utils_Tuple2('かゆ', 'かゆ'),
			_Utils_Tuple2('かゆい', 'かゆい'),
			_Utils_Tuple2('かよい', '通い'),
			_Utils_Tuple2('かよう', '通う'),
			_Utils_Tuple2('から', '空'),
			_Utils_Tuple2('から', '殻'),
			_Utils_Tuple2('がら', '柄'),
			_Utils_Tuple2('からかう', 'からかう'),
			_Utils_Tuple2('からから', 'からから'),
			_Utils_Tuple2('からくち', '辛口'),
			_Utils_Tuple2('からし', 'からし'),
			_Utils_Tuple2('からす', 'からす'),
			_Utils_Tuple2('からっと', 'からっと'),
			_Utils_Tuple2('からっぽ', '空っぽ'),
			_Utils_Tuple2('からて', '空手'),
			_Utils_Tuple2('からふる', 'カラフル'),
			_Utils_Tuple2('かり', '仮'),
			_Utils_Tuple2('かり', '借り'),
			_Utils_Tuple2('かりきゅらむ', 'カリキュラム'),
			_Utils_Tuple2('かりて', '借り手'),
			_Utils_Tuple2('かりに', '仮に'),
			_Utils_Tuple2('かりもの', '借り物'),
			_Utils_Tuple2('かりゅう', '下流'),
			_Utils_Tuple2('かりょく', '火力'),
			_Utils_Tuple2('かるしうむ', 'カルシウム'),
			_Utils_Tuple2('かるちゃー', 'カルチャー'),
			_Utils_Tuple2('かるちゃーしょっく', 'カルチャーショック'),
			_Utils_Tuple2('かるちゃーせんたー', 'カルチャーセンター'),
			_Utils_Tuple2('かれーこ', 'カレー粉'),
			_Utils_Tuple2('がれーじ', 'ガレージ'),
			_Utils_Tuple2('かれは', '枯れ葉'),
			_Utils_Tuple2('かれる', '枯れる'),
			_Utils_Tuple2('かろう', '過労'),
			_Utils_Tuple2('かろりー', 'カロリー'),
			_Utils_Tuple2('かわ', '皮'),
			_Utils_Tuple2('かわ', '革'),
			_Utils_Tuple2('がわ', '側'),
			_Utils_Tuple2('かわいがる', 'かわいがる'),
			_Utils_Tuple2('かわいらしい', 'かわいらしい'),
			_Utils_Tuple2('かわかす', '乾かす'),
			_Utils_Tuple2('かわき', '乾き'),
			_Utils_Tuple2('かわぎし', '川岸'),
			_Utils_Tuple2('かわく', '乾く'),
			_Utils_Tuple2('かわぐつ', '革靴'),
			_Utils_Tuple2('かわり', '代わり'),
			_Utils_Tuple2('かわり', '変わり'),
			_Utils_Tuple2('かわる', '代わる'),
			_Utils_Tuple2('かわる', '替わる'),
			_Utils_Tuple2('かん', '間'),
			_Utils_Tuple2('かん', '間'),
			_Utils_Tuple2('かん', '缶'),
			_Utils_Tuple2('かん', '巻'),
			_Utils_Tuple2('かん', '感'),
			_Utils_Tuple2('かん', '観'),
			_Utils_Tuple2('かん', '官'),
			_Utils_Tuple2('かん', '缶'),
			_Utils_Tuple2('かん', '勘'),
			_Utils_Tuple2('かん', '完'),
			_Utils_Tuple2('かん', '寒'),
			_Utils_Tuple2('がん', 'がん'),
			_Utils_Tuple2('がん', '岸'),
			_Utils_Tuple2('がん', '岩'),
			_Utils_Tuple2('がんか', '眼科'),
			_Utils_Tuple2('かんがえ', '考え'),
			_Utils_Tuple2('かんがえごと', '考え事'),
			_Utils_Tuple2('かんがえこむ', '考え込む'),
			_Utils_Tuple2('かんがえなおす', '考え直す'),
			_Utils_Tuple2('かんかく', '感覚'),
			_Utils_Tuple2('かんかく', '間隔'),
			_Utils_Tuple2('かんき', '換気'),
			_Utils_Tuple2('かんきせん', '換気扇'),
			_Utils_Tuple2('かんきゃく', '観客'),
			_Utils_Tuple2('かんきょう', '環境'),
			_Utils_Tuple2('かんけい', '関係'),
			_Utils_Tuple2('かんげい', '歓迎'),
			_Utils_Tuple2('かんげき', '感激'),
			_Utils_Tuple2('かんご', '漢語'),
			_Utils_Tuple2('かんご', '看護'),
			_Utils_Tuple2('がんこ', '頑固'),
			_Utils_Tuple2('がんこ', '頑固'),
			_Utils_Tuple2('かんこう', '観光'),
			_Utils_Tuple2('かんごし', '看護師'),
			_Utils_Tuple2('かんごふ', '看護婦'),
			_Utils_Tuple2('かんさい', '関西'),
			_Utils_Tuple2('かんさつ', '観察'),
			_Utils_Tuple2('かんじ', '感じ'),
			_Utils_Tuple2('かんじ', '幹事'),
			_Utils_Tuple2('かんしゃ', '感謝'),
			_Utils_Tuple2('かんじゃ', '患者'),
			_Utils_Tuple2('かんしゅう', '慣習'),
			_Utils_Tuple2('がんしょ', '願書'),
			_Utils_Tuple2('かんしょう', '鑑賞'),
			_Utils_Tuple2('かんしょう', '観賞'),
			_Utils_Tuple2('かんじょう', '感情'),
			_Utils_Tuple2('かんじょう', '勘定'),
			_Utils_Tuple2('がんじょう', '頑丈'),
			_Utils_Tuple2('かんじょういにゅう', '感情移入'),
			_Utils_Tuple2('かんじょうてき', '感情的'),
			_Utils_Tuple2('かんしょく', '間食'),
			_Utils_Tuple2('かんしょく', '感触'),
			_Utils_Tuple2('かんしん', '関心'),
			_Utils_Tuple2('かんしん', '感心'),
			_Utils_Tuple2('かんしん', '感心'),
			_Utils_Tuple2('かんする', '関する'),
			_Utils_Tuple2('かんせい', '完成'),
			_Utils_Tuple2('かんせい', '歓声'),
			_Utils_Tuple2('かんぜい', '関税'),
			_Utils_Tuple2('かんせつ', '間接'),
			_Utils_Tuple2('かんぜん', '完全'),
			_Utils_Tuple2('かんぜん', '完全'),
			_Utils_Tuple2('かんそう', '感想'),
			_Utils_Tuple2('かんそう', '乾燥'),
			_Utils_Tuple2('かんそう', '完走'),
			_Utils_Tuple2('かんぞう', '肝臓'),
			_Utils_Tuple2('かんそく', '観測'),
			_Utils_Tuple2('かんだん', '寒暖'),
			_Utils_Tuple2('がんたん', '元旦'),
			_Utils_Tuple2('かんちがい', '勘違い'),
			_Utils_Tuple2('かんちょう', '館長'),
			_Utils_Tuple2('かんづめ', '缶詰め'),
			_Utils_Tuple2('かんでんち', '乾電池'),
			_Utils_Tuple2('かんど', '感度'),
			_Utils_Tuple2('かんとう', '関東'),
			_Utils_Tuple2('かんどう', '感動'),
			_Utils_Tuple2('かんとうちほう', '関東地方'),
			_Utils_Tuple2('かんどうてき', '感動的'),
			_Utils_Tuple2('かんとく', '監督'),
			_Utils_Tuple2('かんとりー', 'カントリー'),
			_Utils_Tuple2('かんない', '館内'),
			_Utils_Tuple2('かんにんぐ', 'カンニング'),
			_Utils_Tuple2('かんばい', '完売'),
			_Utils_Tuple2('かんぱい', '乾杯'),
			_Utils_Tuple2('かんぱにー', 'カンパニー'),
			_Utils_Tuple2('かんばん', '看板'),
			_Utils_Tuple2('かんび', '完備'),
			_Utils_Tuple2('かんびょう', '看病'),
			_Utils_Tuple2('かんぺき', '完璧'),
			_Utils_Tuple2('かんべん', '勘弁'),
			_Utils_Tuple2('かんぽう', '漢方'),
			_Utils_Tuple2('がんぼう', '願望'),
			_Utils_Tuple2('かんゆう', '勧誘'),
			_Utils_Tuple2('かんよ', '関与'),
			_Utils_Tuple2('かんようく', '慣用句'),
			_Utils_Tuple2('かんり', '管理'),
			_Utils_Tuple2('かんりじん', '管理人'),
			_Utils_Tuple2('かんりょう', '完了'),
			_Utils_Tuple2('かんりょう', '官僚'),
			_Utils_Tuple2('かんれん', '関連'),
			_Utils_Tuple2('かんわじてん', '漢和辞典'),
			_Utils_Tuple2('き', '期'),
			_Utils_Tuple2('き', '黄'),
			_Utils_Tuple2('き', '記'),
			_Utils_Tuple2('き', '期'),
			_Utils_Tuple2('き', '気'),
			_Utils_Tuple2('き', '器'),
			_Utils_Tuple2('き', '期'),
			_Utils_Tuple2('き', '機'),
			_Utils_Tuple2('き', '貴'),
			_Utils_Tuple2('き', '記'),
			_Utils_Tuple2('ぎ', '着'),
			_Utils_Tuple2('ぎ', '技'),
			_Utils_Tuple2('きあつ', '気圧'),
			_Utils_Tuple2('きー', 'キー'),
			_Utils_Tuple2('きーぱー', 'キーパー'),
			_Utils_Tuple2('きーぷ', 'キープ'),
			_Utils_Tuple2('きーぼーど', 'キーボード'),
			_Utils_Tuple2('きーほるだー', 'キーホルダー'),
			_Utils_Tuple2('きーわーど', 'キーワード'),
			_Utils_Tuple2('ぎいん', '議員'),
			_Utils_Tuple2('きうい', 'キウイ'),
			_Utils_Tuple2('きおく', '記憶'),
			_Utils_Tuple2('きおん', '気温'),
			_Utils_Tuple2('きか', '帰化'),
			_Utils_Tuple2('きかい', '機械'),
			_Utils_Tuple2('きかい', '機会'),
			_Utils_Tuple2('きがい', '危害'),
			_Utils_Tuple2('ぎかい', '議会'),
			_Utils_Tuple2('きかいか', '機械化'),
			_Utils_Tuple2('きかいこうぎょう', '機械工業'),
			_Utils_Tuple2('きかいてき', '機械的'),
			_Utils_Tuple2('きがえ', '着替え'),
			_Utils_Tuple2('きかえる', '着替える'),
			_Utils_Tuple2('きかく', '企画'),
			_Utils_Tuple2('きかす', '聞かす'),
			_Utils_Tuple2('きがる', '気軽'),
			_Utils_Tuple2('きかん', '期間'),
			_Utils_Tuple2('きかん', '機関'),
			_Utils_Tuple2('きき', '聞き'),
			_Utils_Tuple2('きき', '危機'),
			_Utils_Tuple2('きき', '聞き'),
			_Utils_Tuple2('きき', '機器'),
			_Utils_Tuple2('ききかえす', '聞き返す'),
			_Utils_Tuple2('ききかた', '聞き方'),
			_Utils_Tuple2('ききかん', '危機感'),
			_Utils_Tuple2('ききて', '聞き手'),
			_Utils_Tuple2('ききて', '利き手'),
			_Utils_Tuple2('ききとる', '聞き取る'),
			_Utils_Tuple2('ききめ', '効き目'),
			_Utils_Tuple2('きぎょう', '企業'),
			_Utils_Tuple2('きぎょう', '起業'),
			_Utils_Tuple2('ききわける', '聞き分ける'),
			_Utils_Tuple2('きく', '効く'),
			_Utils_Tuple2('きく', '菊'),
			_Utils_Tuple2('きく', '利く'),
			_Utils_Tuple2('きぐ', '器具'),
			_Utils_Tuple2('きげき', '喜劇'),
			_Utils_Tuple2('きげん', '期限'),
			_Utils_Tuple2('きげん', '機嫌'),
			_Utils_Tuple2('きけんせい', '危険性'),
			_Utils_Tuple2('きげんぜん', '紀元前'),
			_Utils_Tuple2('きこう', '気候'),
			_Utils_Tuple2('きごう', '記号'),
			_Utils_Tuple2('きこむ', '着込む'),
			_Utils_Tuple2('きこん', '既婚'),
			_Utils_Tuple2('きざむ', '刻む'),
			_Utils_Tuple2('きじ', '記事'),
			_Utils_Tuple2('ぎし', '技師'),
			_Utils_Tuple2('きじつ', '期日'),
			_Utils_Tuple2('きしゃ', '記者'),
			_Utils_Tuple2('きしゃ', '汽車'),
			_Utils_Tuple2('きしゅ', '機種'),
			_Utils_Tuple2('きじゅつ', '記述'),
			_Utils_Tuple2('ぎじゅつ', '技術'),
			_Utils_Tuple2('ぎじゅつしゃ', '技術者'),
			_Utils_Tuple2('ぎじゅつてき', '技術的'),
			_Utils_Tuple2('きじゅん', '基準'),
			_Utils_Tuple2('きしょう', '起床'),
			_Utils_Tuple2('きず', '傷'),
			_Utils_Tuple2('きすう', '奇数'),
			_Utils_Tuple2('きずく', '築く'),
			_Utils_Tuple2('きずぐち', '傷口'),
			_Utils_Tuple2('きせい', '帰省'),
			_Utils_Tuple2('きせい', '規制'),
			_Utils_Tuple2('ぎせい', '犠牲'),
			_Utils_Tuple2('ぎせいしゃ', '犠牲者'),
			_Utils_Tuple2('きせき', '奇跡'),
			_Utils_Tuple2('きせきてき', '奇跡的'),
			_Utils_Tuple2('きせつ', '季節'),
			_Utils_Tuple2('きぜつ', '気絶'),
			_Utils_Tuple2('きせる', '着せる'),
			_Utils_Tuple2('きそ', '基礎'),
			_Utils_Tuple2('きそう', '競う'),
			_Utils_Tuple2('きそく', '規則'),
			_Utils_Tuple2('きそくただしい', '規則正しい'),
			_Utils_Tuple2('きたい', '期待'),
			_Utils_Tuple2('きたい', '気体'),
			_Utils_Tuple2('きたえる', '鍛える'),
			_Utils_Tuple2('きたがわ', '北側'),
			_Utils_Tuple2('きたく', '帰宅'),
			_Utils_Tuple2('きたぐに', '北国'),
			_Utils_Tuple2('きたはんきゅう', '北半球'),
			_Utils_Tuple2('ぎたりすと', 'ギタリスト'),
			_Utils_Tuple2('きち', '基地'),
			_Utils_Tuple2('きちょう', '貴重'),
			_Utils_Tuple2('きちょう', '貴重'),
			_Utils_Tuple2('ぎちょう', '議長'),
			_Utils_Tuple2('きちょうひん', '貴重品'),
			_Utils_Tuple2('きちょうめん', 'きちょうめん'),
			_Utils_Tuple2('きちんと', 'きちんと'),
			_Utils_Tuple2('きつい', 'きつい'),
			_Utils_Tuple2('きつえん', '喫煙'),
			_Utils_Tuple2('きづかい', '気遣い'),
			_Utils_Tuple2('きづかう', '気遣う'),
			_Utils_Tuple2('きっかけ', '切っ掛け'),
			_Utils_Tuple2('きっく', 'キック'),
			_Utils_Tuple2('きづく', '気付く'),
			_Utils_Tuple2('きっくおふ', 'キックオフ'),
			_Utils_Tuple2('きっくぼくしんぐ', 'キックボクシング'),
			_Utils_Tuple2('きっさ', '喫茶'),
			_Utils_Tuple2('ぎっしり', 'ぎっしり'),
			_Utils_Tuple2('きっちり', 'きっちり'),
			_Utils_Tuple2('きっちん', 'キッチン'),
			_Utils_Tuple2('きっと', 'きっと'),
			_Utils_Tuple2('きつね', 'きつね'),
			_Utils_Tuple2('きっぱり', 'きっぱり'),
			_Utils_Tuple2('きてん', '起点'),
			_Utils_Tuple2('きどう', '起動'),
			_Utils_Tuple2('きない', '機内'),
			_Utils_Tuple2('きにいり', '気に入り'),
			_Utils_Tuple2('きにはいる', '気に入る'),
			_Utils_Tuple2('きにゅう', '記入'),
			_Utils_Tuple2('きぬ', '絹'),
			_Utils_Tuple2('きねん', '記念'),
			_Utils_Tuple2('きねんきって', '記念切手'),
			_Utils_Tuple2('きのう', '機能'),
			_Utils_Tuple2('ぎのう', '技能'),
			_Utils_Tuple2('きのうきょう', '昨日今日'),
			_Utils_Tuple2('きのこ', 'きのこ'),
			_Utils_Tuple2('きのどく', '気の毒'),
			_Utils_Tuple2('きのぼり', '木登り'),
			_Utils_Tuple2('きふ', '寄付'),
			_Utils_Tuple2('ぎふ', '義父'),
			_Utils_Tuple2('ぎぶあっぷ', 'ギブアップ'),
			_Utils_Tuple2('ぎふと', 'ギフト'),
			_Utils_Tuple2('きぼ', '規模'),
			_Utils_Tuple2('ぎぼ', '義母'),
			_Utils_Tuple2('きぼう', '希望'),
			_Utils_Tuple2('きほん', '基本'),
			_Utils_Tuple2('きほんきゅう', '基本給'),
			_Utils_Tuple2('きほんてき', '基本的'),
			_Utils_Tuple2('きまつ', '期末'),
			_Utils_Tuple2('きまり', '決まり'),
			_Utils_Tuple2('きみ', '君'),
			_Utils_Tuple2('きみ', '黄身'),
			_Utils_Tuple2('きみ', '気味'),
			_Utils_Tuple2('きみょう', '奇妙'),
			_Utils_Tuple2('ぎむ', '義務'),
			_Utils_Tuple2('ぎむきょういく', '義務教育'),
			_Utils_Tuple2('きめい', '記名'),
			_Utils_Tuple2('きもの', '着物'),
			_Utils_Tuple2('ぎもん', '疑問'),
			_Utils_Tuple2('ぎもんぶん', '疑問文'),
			_Utils_Tuple2('きゃく', '脚'),
			_Utils_Tuple2('きやく', '規約'),
			_Utils_Tuple2('ぎゃく', '逆'),
			_Utils_Tuple2('ぎゃぐ', 'ギャグ'),
			_Utils_Tuple2('ぎゃくこうか', '逆効果'),
			_Utils_Tuple2('ぎゃくさん', '逆算'),
			_Utils_Tuple2('きゃくしつ', '客室'),
			_Utils_Tuple2('きゃくせき', '客席'),
			_Utils_Tuple2('きゃくせん', '客船'),
			_Utils_Tuple2('ぎゃくたい', '虐待'),
			_Utils_Tuple2('ぎゃくてん', '逆転'),
			_Utils_Tuple2('きゃくま', '客間'),
			_Utils_Tuple2('ぎゃくゆにゅう', '逆輸入'),
			_Utils_Tuple2('きゃくよう', '客用'),
			_Utils_Tuple2('きゃすたー', 'キャスター'),
			_Utils_Tuple2('きゃっかん', '客観'),
			_Utils_Tuple2('きゃっかんてき', '客観的'),
			_Utils_Tuple2('きゃっしゅ', 'キャッシュ'),
			_Utils_Tuple2('きゃっち', 'キャッチ'),
			_Utils_Tuple2('きゃっちゃー', 'キャッチャー'),
			_Utils_Tuple2('ぎゃっぷ', 'ギャップ'),
			_Utils_Tuple2('きゃでぃー', 'キャディー'),
			_Utils_Tuple2('きゃびあ', 'キャビア'),
			_Utils_Tuple2('きゃぷてん', 'キャプテン'),
			_Utils_Tuple2('きゃみそーる', 'キャミソール'),
			_Utils_Tuple2('きゃらくたー', 'キャラクター'),
			_Utils_Tuple2('きゃらめる', 'キャラメル'),
			_Utils_Tuple2('ぎゃらりー', 'ギャラリー'),
			_Utils_Tuple2('ぎゃる', 'ギャル'),
			_Utils_Tuple2('ぎゃんぐ', 'ギャング'),
			_Utils_Tuple2('きゃんせる', 'キャンセル'),
			_Utils_Tuple2('きゃんでー', 'キャンデー'),
			_Utils_Tuple2('きゃんどる', 'キャンドル'),
			_Utils_Tuple2('きゃんばす', 'キャンバス'),
			_Utils_Tuple2('きゃんぱす', 'キャンパス'),
			_Utils_Tuple2('きゃんぴんぐかー', 'キャンピングカー'),
			_Utils_Tuple2('きゃんぷ', 'キャンプ'),
			_Utils_Tuple2('ぎゃんぶる', 'ギャンブル'),
			_Utils_Tuple2('きゃんぺーん', 'キャンペーン'),
			_Utils_Tuple2('きゅう', '急'),
			_Utils_Tuple2('きゅう', '級'),
			_Utils_Tuple2('きゅう', '級'),
			_Utils_Tuple2('きゅう', '旧'),
			_Utils_Tuple2('きゅう', '球'),
			_Utils_Tuple2('ぎゅう', '牛'),
			_Utils_Tuple2('きゅうか', '休暇'),
			_Utils_Tuple2('きゅうかん', '休館'),
			_Utils_Tuple2('きゅうかん', '休刊'),
			_Utils_Tuple2('きゅうきゅう', '救急'),
			_Utils_Tuple2('きゅうきゅうしゃ', '救急車'),
			_Utils_Tuple2('きゅうぎょう', '休業'),
			_Utils_Tuple2('きゅうけい', '休憩'),
			_Utils_Tuple2('きゅうげき', '急激'),
			_Utils_Tuple2('きゅうこう', '急行'),
			_Utils_Tuple2('きゅうこう', '休講'),
			_Utils_Tuple2('きゅうし', '休止'),
			_Utils_Tuple2('きゅうしゅう', '吸収'),
			_Utils_Tuple2('きゅうしゅつ', '救出'),
			_Utils_Tuple2('きゅうじょ', '救助'),
			_Utils_Tuple2('きゅうじょう', '球場'),
			_Utils_Tuple2('きゅうじょうしょう', '急上昇'),
			_Utils_Tuple2('きゅうしょく', '給食'),
			_Utils_Tuple2('きゅうしょく', '休職'),
			_Utils_Tuple2('きゅうしん', '急進'),
			_Utils_Tuple2('きゅうじん', '求人'),
			_Utils_Tuple2('きゅうぞう', '急増'),
			_Utils_Tuple2('きゅうそく', '急速'),
			_Utils_Tuple2('きゅうそく', '休息'),
			_Utils_Tuple2('きゅうそく', '急速'),
			_Utils_Tuple2('きゅーと', 'キュート'),
			_Utils_Tuple2('きゅうへん', '急変'),
			_Utils_Tuple2('きゅうゆ', '給油'),
			_Utils_Tuple2('きゅうよ', '給与'),
			_Utils_Tuple2('きゅうよう', '急用'),
			_Utils_Tuple2('きゅうよう', '休養'),
			_Utils_Tuple2('きゅうりゅう', '急流'),
			_Utils_Tuple2('きゅうりょう', '給料'),
			_Utils_Tuple2('きゅうれき', '旧暦'),
			_Utils_Tuple2('ぎゅっと', 'ぎゅっと'),
			_Utils_Tuple2('きょう', '強'),
			_Utils_Tuple2('きょう', '教'),
			_Utils_Tuple2('きょう', '京'),
			_Utils_Tuple2('きょう', '鏡'),
			_Utils_Tuple2('きょう', '橋'),
			_Utils_Tuple2('きよう', '起用'),
			_Utils_Tuple2('きよう', '器用'),
			_Utils_Tuple2('ぎょう', '行'),
			_Utils_Tuple2('ぎょう', '行'),
			_Utils_Tuple2('ぎょう', '業'),
			_Utils_Tuple2('きょういく', '教育'),
			_Utils_Tuple2('きょういくしゃ', '教育者'),
			_Utils_Tuple2('きょういくてき', '教育的'),
			_Utils_Tuple2('きょういん', '教員'),
			_Utils_Tuple2('きょうえん', '共演'),
			_Utils_Tuple2('きょうか', '教科'),
			_Utils_Tuple2('きょうか', '強化'),
			_Utils_Tuple2('きょうかい', '協会'),
			_Utils_Tuple2('きょうかい', '境界'),
			_Utils_Tuple2('ぎょうかい', '業界'),
			_Utils_Tuple2('ぎょうかい', '業界'),
			_Utils_Tuple2('きょうかん', '共感'),
			_Utils_Tuple2('きょうぎ', '競技'),
			_Utils_Tuple2('ぎょうぎ', '行儀'),
			_Utils_Tuple2('きょうぎじょう', '競技場'),
			_Utils_Tuple2('きょうきゅう', '供給'),
			_Utils_Tuple2('きょうこ', '強固'),
			_Utils_Tuple2('きょうざい', '教材'),
			_Utils_Tuple2('きょうさんしゅぎ', '共産主義'),
			_Utils_Tuple2('きょうさんとう', '共産党'),
			_Utils_Tuple2('ぎょうじ', '行事'),
			_Utils_Tuple2('きょうしゃ', '強者'),
			_Utils_Tuple2('ぎょうしゃ', '業者'),
			_Utils_Tuple2('ぎょうしゃ', '業者'),
			_Utils_Tuple2('きょうじゃく', '強弱'),
			_Utils_Tuple2('きょうじゅ', '教授'),
			_Utils_Tuple2('きょうしゅうしょ', '教習所'),
			_Utils_Tuple2('きょうせい', '強制'),
			_Utils_Tuple2('ぎょうせい', '行政'),
			_Utils_Tuple2('きょうせいてき', '強制的'),
			_Utils_Tuple2('ぎょうせき', '業績'),
			_Utils_Tuple2('きょうそう', '競争'),
			_Utils_Tuple2('きょうそう', '競走'),
			_Utils_Tuple2('きょうそうりょく', '競争力'),
			_Utils_Tuple2('きょうぞん', '共存'),
			_Utils_Tuple2('きょうだい', '強大'),
			_Utils_Tuple2('きょうちょう', '協調'),
			_Utils_Tuple2('きょうちょう', '強調'),
			_Utils_Tuple2('きょうつう', '共通'),
			_Utils_Tuple2('きょうつうご', '共通語'),
			_Utils_Tuple2('きょうど', '強度'),
			_Utils_Tuple2('きょうど', '郷土'),
			_Utils_Tuple2('きょうとう', '教頭'),
			_Utils_Tuple2('きょうどう', '共同'),
			_Utils_Tuple2('きょうどう', '協同'),
			_Utils_Tuple2('きょうどうたい', '共同体'),
			_Utils_Tuple2('きょうふ', '恐怖'),
			_Utils_Tuple2('きょうぶ', '胸部'),
			_Utils_Tuple2('きょうふう', '強風'),
			_Utils_Tuple2('きょうみ', '興味'),
			_Utils_Tuple2('きょうみぶかい', '興味深い'),
			_Utils_Tuple2('きょうみほんい', '興味本位'),
			_Utils_Tuple2('ぎょうむ', '業務'),
			_Utils_Tuple2('きょうゆう', '共有'),
			_Utils_Tuple2('きょうよう', '教養'),
			_Utils_Tuple2('きょうよう', '強要'),
			_Utils_Tuple2('きょうよう', '共用'),
			_Utils_Tuple2('きょうりょく', '協力'),
			_Utils_Tuple2('きょうりょく', '強力'),
			_Utils_Tuple2('ぎょうれつ', '行列'),
			_Utils_Tuple2('きょか', '許可'),
			_Utils_Tuple2('ぎょかいるい', '魚介類'),
			_Utils_Tuple2('きょがく', '巨額'),
			_Utils_Tuple2('ぎょぎょう', '漁業'),
			_Utils_Tuple2('きょく', '曲'),
			_Utils_Tuple2('きょく', '局'),
			_Utils_Tuple2('きょく', '極'),
			_Utils_Tuple2('ぎょく', '玉'),
			_Utils_Tuple2('きょくせん', '曲線'),
			_Utils_Tuple2('きょくたん', '極端'),
			_Utils_Tuple2('きょくちょう', '局長'),
			_Utils_Tuple2('きょくちょう', '局長'),
			_Utils_Tuple2('きょくど', '極度'),
			_Utils_Tuple2('きょくりょく', '極力'),
			_Utils_Tuple2('きょしき', '挙式'),
			_Utils_Tuple2('きょじゅう', '居住'),
			_Utils_Tuple2('きょじん', '巨人'),
			_Utils_Tuple2('きょぜつ', '拒絶'),
			_Utils_Tuple2('ぎょせん', '漁船'),
			_Utils_Tuple2('ぎょそん', '漁村'),
			_Utils_Tuple2('きょだい', '巨大'),
			_Utils_Tuple2('きょだい', '巨大'),
			_Utils_Tuple2('ぎょっと', 'ぎょっと'),
			_Utils_Tuple2('きょひ', '拒否'),
			_Utils_Tuple2('きょよう', '許容'),
			_Utils_Tuple2('きょり', '距離'),
			_Utils_Tuple2('ぎょるい', '魚類'),
			_Utils_Tuple2('きょろきょろ', 'きょろきょろ'),
			_Utils_Tuple2('きらう', '嫌う'),
			_Utils_Tuple2('きらきら', 'きらきら'),
			_Utils_Tuple2('ぎらぎら', 'ぎらぎら'),
			_Utils_Tuple2('きらく', '気楽'),
			_Utils_Tuple2('きらく', '気楽'),
			_Utils_Tuple2('きり', '切り'),
			_Utils_Tuple2('きり', '霧'),
			_Utils_Tuple2('ぎり', '義理'),
			_Utils_Tuple2('きりきざむ', '切り刻む'),
			_Utils_Tuple2('ぎりぎり', 'ぎりぎり'),
			_Utils_Tuple2('きりすと', 'キリスト'),
			_Utils_Tuple2('きりすときょう', 'キリスト教'),
			_Utils_Tuple2('きりすときょうと', 'キリスト教徒'),
			_Utils_Tuple2('きりつ', '起立'),
			_Utils_Tuple2('きりとる', '切り取る'),
			_Utils_Tuple2('きりはなす', '切り離す'),
			_Utils_Tuple2('きりみ', '切り身'),
			_Utils_Tuple2('きりょく', '気力'),
			_Utils_Tuple2('きりん', 'きりん'),
			_Utils_Tuple2('きれ', '切れ'),
			_Utils_Tuple2('きれ', '切れ'),
			_Utils_Tuple2('きれめ', '切れ目'),
			_Utils_Tuple2('きれる', '切れる'),
			_Utils_Tuple2('きろ', '帰路'),
			_Utils_Tuple2('きろかろりー', 'キロカロリー'),
			_Utils_Tuple2('きろく', '記録'),
			_Utils_Tuple2('きろりっとる', 'キロリットル'),
			_Utils_Tuple2('きろわっと', 'キロワット'),
			_Utils_Tuple2('ぎろん', '議論'),
			_Utils_Tuple2('ぎわく', '疑惑'),
			_Utils_Tuple2('きん', '金'),
			_Utils_Tuple2('きん', '禁'),
			_Utils_Tuple2('ぎん', '銀'),
			_Utils_Tuple2('きんいつ', '均一'),
			_Utils_Tuple2('きんいろ', '金色'),
			_Utils_Tuple2('ぎんいろ', '銀色'),
			_Utils_Tuple2('きんえん', '禁煙'),
			_Utils_Tuple2('ぎんが', '銀河'),
			_Utils_Tuple2('きんかく', '金閣'),
			_Utils_Tuple2('きんがく', '金額'),
			_Utils_Tuple2('ぎんがみ', '銀紙'),
			_Utils_Tuple2('きんがん', '近眼'),
			_Utils_Tuple2('きんきゅう', '緊急'),
			_Utils_Tuple2('きんきゅうじたい', '緊急事態'),
			_Utils_Tuple2('きんぎょ', '金魚'),
			_Utils_Tuple2('きんきょう', '近況'),
			_Utils_Tuple2('きんきょり', '近距離'),
			_Utils_Tuple2('きんぎん', '金銀'),
			_Utils_Tuple2('きんぐ', 'キング'),
			_Utils_Tuple2('きんけん', '金券'),
			_Utils_Tuple2('きんこ', '金庫'),
			_Utils_Tuple2('きんこう', '近郊'),
			_Utils_Tuple2('きんし', '禁止'),
			_Utils_Tuple2('きんし', '近視'),
			_Utils_Tuple2('きんじつ', '近日'),
			_Utils_Tuple2('きんしゅ', '禁酒'),
			_Utils_Tuple2('きんじょ', '近所'),
			_Utils_Tuple2('きんせん', '金銭'),
			_Utils_Tuple2('きんぞく', '金属'),
			_Utils_Tuple2('きんぞく', '勤続'),
			_Utils_Tuple2('きんだい', '近代'),
			_Utils_Tuple2('きんだいか', '近代化'),
			_Utils_Tuple2('きんだいてき', '近代的'),
			_Utils_Tuple2('きんちょう', '緊張'),
			_Utils_Tuple2('きんとう', '均等'),
			_Utils_Tuple2('きんにく', '筋肉'),
			_Utils_Tuple2('きんにくしつ', '筋肉質'),
			_Utils_Tuple2('きんねん', '近年'),
			_Utils_Tuple2('きんぱつ', '金髪'),
			_Utils_Tuple2('きんべん', '勤勉'),
			_Utils_Tuple2('きんぺん', '近辺'),
			_Utils_Tuple2('きんむ', '勤務'),
			_Utils_Tuple2('きんめだる', '金メダル'),
			_Utils_Tuple2('きんゆうきかん', '金融機関'),
			_Utils_Tuple2('く', '苦'),
			_Utils_Tuple2('ぐ', '具'),
			_Utils_Tuple2('ぐあい', '具合'),
			_Utils_Tuple2('くいーん', 'クイーン'),
			_Utils_Tuple2('くいき', '区域'),
			_Utils_Tuple2('ぐいぐい', 'ぐいぐい'),
			_Utils_Tuple2('くいず', 'クイズ'),
			_Utils_Tuple2('くいっく', 'クイック'),
			_Utils_Tuple2('くう', '食う'),
			_Utils_Tuple2('くうかん', '空間'),
			_Utils_Tuple2('くうき', '空気'),
			_Utils_Tuple2('ぐうぐう', 'ぐうぐう'),
			_Utils_Tuple2('ぐうすう', '偶数'),
			_Utils_Tuple2('くうせき', '空席'),
			_Utils_Tuple2('ぐうぜん', '偶然'),
			_Utils_Tuple2('ぐうぜん', '偶然'),
			_Utils_Tuple2('くうそう', '空想'),
			_Utils_Tuple2('くうちゅう', '空中'),
			_Utils_Tuple2('くうちょう', '空調'),
			_Utils_Tuple2('くうふく', '空腹'),
			_Utils_Tuple2('くーぽん', 'クーポン'),
			_Utils_Tuple2('くーらー', 'クーラー'),
			_Utils_Tuple2('くうらん', '空欄'),
			_Utils_Tuple2('くーりんぐおふ', 'クーリングオフ'),
			_Utils_Tuple2('くーる', 'クール'),
			_Utils_Tuple2('くーる', 'クール'),
			_Utils_Tuple2('くえすちょん', 'クエスチョン'),
			_Utils_Tuple2('くかん', '区間'),
			_Utils_Tuple2('くぎる', '区切る'),
			_Utils_Tuple2('くさ', '草'),
			_Utils_Tuple2('くさい', '臭い'),
			_Utils_Tuple2('くさい', '臭い'),
			_Utils_Tuple2('くさき', '草木'),
			_Utils_Tuple2('くさばな', '草花'),
			_Utils_Tuple2('くさる', '腐る'),
			_Utils_Tuple2('くし', 'くし'),
			_Utils_Tuple2('くし', 'くし'),
			_Utils_Tuple2('くじ', 'くじ'),
			_Utils_Tuple2('くしかつ', 'くしカツ'),
			_Utils_Tuple2('くしゃくしゃ', 'くしゃくしゃ'),
			_Utils_Tuple2('くしゃみ', 'くしゃみ'),
			_Utils_Tuple2('くじょう', '苦情'),
			_Utils_Tuple2('くじら', '鯨'),
			_Utils_Tuple2('くず', 'くず'),
			_Utils_Tuple2('くすくす', 'くすくす'),
			_Utils_Tuple2('くずす', '崩す'),
			_Utils_Tuple2('くずれ', '崩れ'),
			_Utils_Tuple2('くずれる', '崩れる'),
			_Utils_Tuple2('くせ', '癖'),
			_Utils_Tuple2('くせん', '苦戦'),
			_Utils_Tuple2('ぐたいてき', '具体的'),
			_Utils_Tuple2('くだく', '砕く'),
			_Utils_Tuple2('くたくた', 'くたくた'),
			_Utils_Tuple2('くださる', '下さる'),
			_Utils_Tuple2('くだり', '下り'),
			_Utils_Tuple2('くだりざか', '下り坂'),
			_Utils_Tuple2('くだる', '下る'),
			_Utils_Tuple2('ぐち', '愚痴'),
			_Utils_Tuple2('くちびる', '唇'),
			_Utils_Tuple2('くちぶえ', '口笛'),
			_Utils_Tuple2('くちべに', '口紅'),
			_Utils_Tuple2('くちもと', '口元'),
			_Utils_Tuple2('ぐちゃぐちゃ', 'ぐちゃぐちゃ'),
			_Utils_Tuple2('くつう', '苦痛'),
			_Utils_Tuple2('くっきり', 'くっきり'),
			_Utils_Tuple2('くっきんぐ', 'クッキング'),
			_Utils_Tuple2('ぐつぐつ', 'ぐつぐつ'),
			_Utils_Tuple2('くっしょん', 'クッション'),
			_Utils_Tuple2('ぐっず', 'グッズ'),
			_Utils_Tuple2('ぐっすり', 'ぐっすり'),
			_Utils_Tuple2('ぐったり', 'ぐったり'),
			_Utils_Tuple2('くっつく', 'くっ付く'),
			_Utils_Tuple2('くっつける', 'くっ付ける'),
			_Utils_Tuple2('ぐっと', 'ぐっと'),
			_Utils_Tuple2('ぐっど', 'グッド'),
			_Utils_Tuple2('ぐっどもーにんぐ', 'グッドモーニング'),
			_Utils_Tuple2('くどい', 'くどい'),
			_Utils_Tuple2('くのう', '苦悩'),
			_Utils_Tuple2('くばる', '配る'),
			_Utils_Tuple2('くびわ', '首輪'),
			_Utils_Tuple2('くふう', '工夫'),
			_Utils_Tuple2('くぶん', '区分'),
			_Utils_Tuple2('くべつ', '区別'),
			_Utils_Tuple2('くま', '熊'),
			_Utils_Tuple2('くみ', '組'),
			_Utils_Tuple2('くみあい', '組合'),
			_Utils_Tuple2('くみあわせ', '組み合わせ'),
			_Utils_Tuple2('くみあわせる', '組み合わせる'),
			_Utils_Tuple2('くむ', '組む'),
			_Utils_Tuple2('くも', 'くも'),
			_Utils_Tuple2('くやしい', '悔しい'),
			_Utils_Tuple2('くよくよ', 'くよくよ'),
			_Utils_Tuple2('くらいあんと', 'クライアント'),
			_Utils_Tuple2('くらいまっくす', 'クライマックス'),
			_Utils_Tuple2('ぐらうんど', 'グラウンド'),
			_Utils_Tuple2('くらがり', '暗がり'),
			_Utils_Tuple2('くらくしょん', 'クラクション'),
			_Utils_Tuple2('ぐらぐら', 'ぐらぐら'),
			_Utils_Tuple2('くらし', '暮らし'),
			_Utils_Tuple2('くらしっく', 'クラシック'),
			_Utils_Tuple2('くらしっくおんがく', 'クラシック音楽'),
			_Utils_Tuple2('くらす', '暮らす'),
			_Utils_Tuple2('ぐらたん', 'グラタン'),
			_Utils_Tuple2('くらっかー', 'クラッカー'),
			_Utils_Tuple2('ぐらふ', 'グラフ'),
			_Utils_Tuple2('くらぶかつどう', 'クラブ活動'),
			_Utils_Tuple2('くらべる', '比べる'),
			_Utils_Tuple2('ぐらんど', 'グランド'),
			_Utils_Tuple2('ぐらんぷり', 'グランプリ'),
			_Utils_Tuple2('くり', 'くり'),
			_Utils_Tuple2('くりあ', 'クリア'),
			_Utils_Tuple2('くりあ', 'クリア'),
			_Utils_Tuple2('くりーなー', 'クリーナー'),
			_Utils_Tuple2('くりーにんぐ', 'クリーニング'),
			_Utils_Tuple2('くりーむ', 'クリーム'),
			_Utils_Tuple2('くりーん', 'クリーン'),
			_Utils_Tuple2('ぐりーん', 'グリーン'),
			_Utils_Tuple2('ぐりーんしゃ', 'グリーン車'),
			_Utils_Tuple2('くりかえし', '繰り返し'),
			_Utils_Tuple2('くりかえす', '繰り返す'),
			_Utils_Tuple2('くりすちゃん', 'クリスチャン'),
			_Utils_Tuple2('くりっく', 'クリック'),
			_Utils_Tuple2('くりっぷ', 'クリップ'),
			_Utils_Tuple2('くりにっく', 'クリニック'),
			_Utils_Tuple2('くるくる', 'くるくる'),
			_Utils_Tuple2('ぐるぐる', 'ぐるぐる'),
			_Utils_Tuple2('くるしい', '苦しい'),
			_Utils_Tuple2('くるしみ', '苦しみ'),
			_Utils_Tuple2('くるしむ', '苦しむ'),
			_Utils_Tuple2('くるしめる', '苦しめる'),
			_Utils_Tuple2('ぐるめ', 'グルメ'),
			_Utils_Tuple2('くるり', 'くるり'),
			_Utils_Tuple2('くれ', '暮れ'),
			_Utils_Tuple2('ぐれー', 'グレー'),
			_Utils_Tuple2('くれーぷ', 'クレープ'),
			_Utils_Tuple2('くれーむ', 'クレーム'),
			_Utils_Tuple2('くれじっと', 'クレジット'),
			_Utils_Tuple2('くれじっとかーど', 'クレジットカード'),
			_Utils_Tuple2('くれよん', 'クレヨン'),
			_Utils_Tuple2('くれる', '暮れる'),
			_Utils_Tuple2('くろう', '苦労'),
			_Utils_Tuple2('くろーばー', 'クローバー'),
			_Utils_Tuple2('ぐろーばる', 'グローバル'),
			_Utils_Tuple2('くろかみ', '黒髪'),
			_Utils_Tuple2('くろぐろ', '黒々'),
			_Utils_Tuple2('くろじ', '黒字'),
			_Utils_Tuple2('くろすわーど', 'クロスワード'),
			_Utils_Tuple2('くろすわーどぱずる', 'クロスワードパズル'),
			_Utils_Tuple2('くろわっさん', 'クロワッサン'),
			_Utils_Tuple2('くわえる', '加える'),
			_Utils_Tuple2('くわしい', '詳しい'),
			_Utils_Tuple2('くわわる', '加わる'),
			_Utils_Tuple2('くん', '訓'),
			_Utils_Tuple2('ぐん', '郡'),
			_Utils_Tuple2('ぐん', '軍'),
			_Utils_Tuple2('ぐん', '群'),
			_Utils_Tuple2('ぐんぐん', 'ぐんぐん'),
			_Utils_Tuple2('ぐんじん', '軍人'),
			_Utils_Tuple2('ぐんたい', '軍隊'),
			_Utils_Tuple2('くんよみ', '訓読み'),
			_Utils_Tuple2('くんれん', '訓練'),
			_Utils_Tuple2('け', '気'),
			_Utils_Tuple2('げ', '気'),
			_Utils_Tuple2('けあ', 'ケア'),
			_Utils_Tuple2('けい', '形'),
			_Utils_Tuple2('けい', '計'),
			_Utils_Tuple2('けい', '系'),
			_Utils_Tuple2('けい', '系'),
			_Utils_Tuple2('けい', '計'),
			_Utils_Tuple2('けい', '刑'),
			_Utils_Tuple2('げい', '芸'),
			_Utils_Tuple2('けいい', '敬意'),
			_Utils_Tuple2('けいえい', '経営'),
			_Utils_Tuple2('けいえいがく', '経営学'),
			_Utils_Tuple2('けいえいしゃ', '経営者'),
			_Utils_Tuple2('けいか', '経過'),
			_Utils_Tuple2('けいかい', '軽快'),
			_Utils_Tuple2('けいかく', '計画'),
			_Utils_Tuple2('けいかくてき', '計画的'),
			_Utils_Tuple2('けいかん', '警官'),
			_Utils_Tuple2('けいき', '景気'),
			_Utils_Tuple2('けいぐ', '敬具'),
			_Utils_Tuple2('けいけん', '経験'),
			_Utils_Tuple2('けいげん', '軽減'),
			_Utils_Tuple2('けいこ', 'けいこ'),
			_Utils_Tuple2('けいご', '敬語'),
			_Utils_Tuple2('けいこう', '傾向'),
			_Utils_Tuple2('けいこく', '警告'),
			_Utils_Tuple2('けいざい', '経済'),
			_Utils_Tuple2('けいざいがく', '経済学'),
			_Utils_Tuple2('けいざいせいちょう', '経済成長'),
			_Utils_Tuple2('けいざいてき', '経済的'),
			_Utils_Tuple2('けいさつかん', '警察官'),
			_Utils_Tuple2('けいさつしょ', '警察署'),
			_Utils_Tuple2('けいさつちょう', '警察庁'),
			_Utils_Tuple2('けいさん', '計算'),
			_Utils_Tuple2('けいさんしき', '計算式'),
			_Utils_Tuple2('けいし', '軽視'),
			_Utils_Tuple2('けいじ', '刑事'),
			_Utils_Tuple2('けいじ', '掲示'),
			_Utils_Tuple2('けいしき', '形式'),
			_Utils_Tuple2('けいしきてき', '形式的'),
			_Utils_Tuple2('けいじばん', '掲示板'),
			_Utils_Tuple2('げいしゃ', '芸者'),
			_Utils_Tuple2('げいじゅつ', '芸術'),
			_Utils_Tuple2('げいじゅつか', '芸術家'),
			_Utils_Tuple2('げいじゅつてき', '芸術的'),
			_Utils_Tuple2('げいじゅつひん', '芸術品'),
			_Utils_Tuple2('けいせい', '形成'),
			_Utils_Tuple2('けいぞく', '継続'),
			_Utils_Tuple2('けいたい', '携帯'),
			_Utils_Tuple2('けいとう', '系統'),
			_Utils_Tuple2('けいにく', '鶏肉'),
			_Utils_Tuple2('げいにん', '芸人'),
			_Utils_Tuple2('げいのう', '芸能'),
			_Utils_Tuple2('げいのうかい', '芸能界'),
			_Utils_Tuple2('げいのうじん', '芸能人'),
			_Utils_Tuple2('けいば', '競馬'),
			_Utils_Tuple2('けいひ', '経費'),
			_Utils_Tuple2('けいび', '警備'),
			_Utils_Tuple2('けいひん', '景品'),
			_Utils_Tuple2('けいむしょ', '刑務所'),
			_Utils_Tuple2('けいやく', '契約'),
			_Utils_Tuple2('けいやくしょ', '契約書'),
			_Utils_Tuple2('けいゆ', '経由'),
			_Utils_Tuple2('けいよう', '形容'),
			_Utils_Tuple2('けいり', '経理'),
			_Utils_Tuple2('けいりょう', '軽量'),
			_Utils_Tuple2('けいれき', '経歴'),
			_Utils_Tuple2('けーおー', 'ＫＯ'),
			_Utils_Tuple2('けーす', 'ケース'),
			_Utils_Tuple2('げーと', 'ゲート'),
			_Utils_Tuple2('げーむせんたー', 'ゲームセンター'),
			_Utils_Tuple2('けが', 'けが'),
			_Utils_Tuple2('げか', '外科'),
			_Utils_Tuple2('けがわ', '毛皮'),
			_Utils_Tuple2('げき', '劇'),
			_Utils_Tuple2('げきげん', '激減'),
			_Utils_Tuple2('げきじょう', '劇場'),
			_Utils_Tuple2('げきど', '激怒'),
			_Utils_Tuple2('げきへん', '激変'),
			_Utils_Tuple2('げこう', '下校'),
			_Utils_Tuple2('げざん', '下山'),
			_Utils_Tuple2('けしいん', '消印'),
			_Utils_Tuple2('けしき', '景色'),
			_Utils_Tuple2('けしごむ', '消しゴム'),
			_Utils_Tuple2('げしゃ', '下車'),
			_Utils_Tuple2('げしゅく', '下宿'),
			_Utils_Tuple2('げじゅん', '下旬'),
			_Utils_Tuple2('けしょう', '化粧'),
			_Utils_Tuple2('けしょうすい', '化粧水'),
			_Utils_Tuple2('けしょうひん', '化粧品'),
			_Utils_Tuple2('げすい', '下水'),
			_Utils_Tuple2('げすいどう', '下水道'),
			_Utils_Tuple2('げすと', 'ゲスト'),
			_Utils_Tuple2('けずる', '削る'),
			_Utils_Tuple2('けた', 'けた'),
			_Utils_Tuple2('けた', 'けた'),
			_Utils_Tuple2('げた', 'げた'),
			_Utils_Tuple2('けち', 'けち'),
			_Utils_Tuple2('けつ', '血'),
			_Utils_Tuple2('けつあつ', '血圧'),
			_Utils_Tuple2('けつい', '決意'),
			_Utils_Tuple2('けつえき', '血液'),
			_Utils_Tuple2('けつえきがた', '血液型'),
			_Utils_Tuple2('けっか', '結果'),
			_Utils_Tuple2('げつがく', '月額'),
			_Utils_Tuple2('けっかてき', '結果的'),
			_Utils_Tuple2('げっかん', '月間'),
			_Utils_Tuple2('げっかん', '月刊'),
			_Utils_Tuple2('けつぎ', '決議'),
			_Utils_Tuple2('げっきゅう', '月給'),
			_Utils_Tuple2('けっきょく', '結局'),
			_Utils_Tuple2('けっきん', '欠勤'),
			_Utils_Tuple2('けっこう', '結構'),
			_Utils_Tuple2('けっこう', '結構'),
			_Utils_Tuple2('けっこう', '欠航'),
			_Utils_Tuple2('けっして', '決して'),
			_Utils_Tuple2('げっしゃ', '月謝'),
			_Utils_Tuple2('げっしゅう', '月収'),
			_Utils_Tuple2('けっしょう', '決勝'),
			_Utils_Tuple2('けつじょう', '欠場'),
			_Utils_Tuple2('けっしょうせん', '決勝戦'),
			_Utils_Tuple2('けっしん', '決心'),
			_Utils_Tuple2('けっせい', '結成'),
			_Utils_Tuple2('けっそく', '結束'),
			_Utils_Tuple2('けつぞく', '血族'),
			_Utils_Tuple2('けつだん', '決断'),
			_Utils_Tuple2('けっちゃく', '決着'),
			_Utils_Tuple2('けってい', '決定'),
			_Utils_Tuple2('けっていてき', '決定的'),
			_Utils_Tuple2('けってん', '欠点'),
			_Utils_Tuple2('けつまつ', '結末'),
			_Utils_Tuple2('げつまつ', '月末'),
			_Utils_Tuple2('けつろん', '結論'),
			_Utils_Tuple2('げねつ', '解熱'),
			_Utils_Tuple2('けびょう', '仮病'),
			_Utils_Tuple2('げひん', '下品'),
			_Utils_Tuple2('けぶかい', '毛深い'),
			_Utils_Tuple2('けむり', '煙'),
			_Utils_Tuple2('げらく', '下落'),
			_Utils_Tuple2('けり', 'けり'),
			_Utils_Tuple2('げり', '下痢'),
			_Utils_Tuple2('ける', 'ける'),
			_Utils_Tuple2('けれど', 'けれど'),
			_Utils_Tuple2('けれども', 'けれども'),
			_Utils_Tuple2('げれんで', 'ゲレンデ'),
			_Utils_Tuple2('けん', '県'),
			_Utils_Tuple2('けん', '犬'),
			_Utils_Tuple2('けん', '券'),
			_Utils_Tuple2('けん', '権'),
			_Utils_Tuple2('けん', '軒'),
			_Utils_Tuple2('けん', '件'),
			_Utils_Tuple2('けん', '件'),
			_Utils_Tuple2('げん', '現'),
			_Utils_Tuple2('げん', '減'),
			_Utils_Tuple2('げんいん', '原因'),
			_Utils_Tuple2('げんえき', '現役'),
			_Utils_Tuple2('けんか', 'けんか'),
			_Utils_Tuple2('けんがい', '圏外'),
			_Utils_Tuple2('けんがい', '県外'),
			_Utils_Tuple2('げんかい', '限界'),
			_Utils_Tuple2('けんがく', '見学'),
			_Utils_Tuple2('げんがく', '減額'),
			_Utils_Tuple2('げんかん', '玄関'),
			_Utils_Tuple2('けんきゅう', '研究'),
			_Utils_Tuple2('けんきゅうしつ', '研究室'),
			_Utils_Tuple2('げんきん', '現金'),
			_Utils_Tuple2('げんきん', '現金'),
			_Utils_Tuple2('げんきん', '厳禁'),
			_Utils_Tuple2('げんきんかきとめ', '現金書留'),
			_Utils_Tuple2('げんご', '言語'),
			_Utils_Tuple2('けんこう', '健康'),
			_Utils_Tuple2('げんこう', '現行'),
			_Utils_Tuple2('げんこう', '原稿'),
			_Utils_Tuple2('けんこうしょくひん', '健康食品'),
			_Utils_Tuple2('けんこうしんだん', '健康診断'),
			_Utils_Tuple2('けんこうほけん', '健康保険'),
			_Utils_Tuple2('げんこうようし', '原稿用紙'),
			_Utils_Tuple2('げんごがく', '言語学'),
			_Utils_Tuple2('けんさ', '検査'),
			_Utils_Tuple2('げんざい', '現在'),
			_Utils_Tuple2('げんざいりょう', '原材料'),
			_Utils_Tuple2('けんさく', '検索'),
			_Utils_Tuple2('げんさく', '原作'),
			_Utils_Tuple2('げんさん', '原産'),
			_Utils_Tuple2('げんじつ', '現実'),
			_Utils_Tuple2('げんじつせい', '現実性'),
			_Utils_Tuple2('げんじつてき', '現実的'),
			_Utils_Tuple2('げんじてん', '現時点'),
			_Utils_Tuple2('けんしゅう', '研修'),
			_Utils_Tuple2('けんじゅう', 'けん銃'),
			_Utils_Tuple2('げんじゅう', '厳重'),
			_Utils_Tuple2('けんじょう', '謙譲'),
			_Utils_Tuple2('げんしょう', '現象'),
			_Utils_Tuple2('げんしょう', '減少'),
			_Utils_Tuple2('げんじょう', '現状'),
			_Utils_Tuple2('けんじょうご', '謙譲語'),
			_Utils_Tuple2('げんしょく', '現職'),
			_Utils_Tuple2('げんしょく', '原色'),
			_Utils_Tuple2('げんしりょく', '原子力'),
			_Utils_Tuple2('げんしりょくはつでん', '原子力発電'),
			_Utils_Tuple2('けんしん', '検診'),
			_Utils_Tuple2('けんすう', '件数'),
			_Utils_Tuple2('げんぜい', '減税'),
			_Utils_Tuple2('けんせつ', '建設'),
			_Utils_Tuple2('けんぜん', '健全'),
			_Utils_Tuple2('けんぞうぶつ', '建造物'),
			_Utils_Tuple2('げんそく', '減速'),
			_Utils_Tuple2('げんそく', '原則'),
			_Utils_Tuple2('げんそく', '原則'),
			_Utils_Tuple2('けんそん', '謙そん'),
			_Utils_Tuple2('げんだい', '現代'),
			_Utils_Tuple2('げんだいご', '現代語'),
			_Utils_Tuple2('げんだいぶん', '現代文'),
			_Utils_Tuple2('げんだんかい', '現段階'),
			_Utils_Tuple2('げんち', '現地'),
			_Utils_Tuple2('けんちく', '建築'),
			_Utils_Tuple2('けんちくし', '建築士'),
			_Utils_Tuple2('けんちくぶつ', '建築物'),
			_Utils_Tuple2('けんちょう', '県庁'),
			_Utils_Tuple2('けんてい', '検定'),
			_Utils_Tuple2('げんてい', '限定'),
			_Utils_Tuple2('げんてん', '原点'),
			_Utils_Tuple2('げんてん', '減点'),
			_Utils_Tuple2('げんど', '限度'),
			_Utils_Tuple2('けんとう', '検討'),
			_Utils_Tuple2('けんとう', '見当'),
			_Utils_Tuple2('けんどう', '剣道'),
			_Utils_Tuple2('けんどう', '県道'),
			_Utils_Tuple2('げんどう', '言動'),
			_Utils_Tuple2('げんどうりょく', '原動力'),
			_Utils_Tuple2('けんない', '圏内'),
			_Utils_Tuple2('けんない', '県内'),
			_Utils_Tuple2('げんば', '現場'),
			_Utils_Tuple2('けんばいき', '券売機'),
			_Utils_Tuple2('げんぴん', '現品'),
			_Utils_Tuple2('けんぶつ', '見物'),
			_Utils_Tuple2('げんぶつ', '現物'),
			_Utils_Tuple2('けんぽう', '憲法'),
			_Utils_Tuple2('げんまい', '玄米'),
			_Utils_Tuple2('けんみん', '県民'),
			_Utils_Tuple2('けんめい', '懸命'),
			_Utils_Tuple2('けんよう', '兼用'),
			_Utils_Tuple2('けんり', '権利'),
			_Utils_Tuple2('げんり', '原理'),
			_Utils_Tuple2('けんりつ', '県立'),
			_Utils_Tuple2('げんりょう', '原料'),
			_Utils_Tuple2('げんりょう', '減量'),
			_Utils_Tuple2('けんりょく', '権力'),
			_Utils_Tuple2('こ', '小'),
			_Utils_Tuple2('こ', '湖'),
			_Utils_Tuple2('こ', '庫'),
			_Utils_Tuple2('こ', '粉'),
			_Utils_Tuple2('こ', '戸'),
			_Utils_Tuple2('こ', '古'),
			_Utils_Tuple2('こ', '個'),
			_Utils_Tuple2('こ', '戸'),
			_Utils_Tuple2('こ', '故'),
			_Utils_Tuple2('ご', '語'),
			_Utils_Tuple2('ご', '御'),
			_Utils_Tuple2('ご', '御'),
			_Utils_Tuple2('ご', '誤'),
			_Utils_Tuple2('ごあいさつ', 'ご挨拶'),
			_Utils_Tuple2('こあら', 'コアラ'),
			_Utils_Tuple2('こい', '濃い'),
			_Utils_Tuple2('こい', '恋'),
			_Utils_Tuple2('こい', 'こい'),
			_Utils_Tuple2('こい', '故意'),
			_Utils_Tuple2('ごい', '語彙'),
			_Utils_Tuple2('こいごころ', '恋心'),
			_Utils_Tuple2('こいしい', '恋しい'),
			_Utils_Tuple2('こいつ', 'こいつ'),
			_Utils_Tuple2('こいのぼり', 'こいのぼり'),
			_Utils_Tuple2('こいん', 'コイン'),
			_Utils_Tuple2('こいんらんどりー', 'コインランドリー'),
			_Utils_Tuple2('こう', '高'),
			_Utils_Tuple2('こう', '好'),
			_Utils_Tuple2('こう', '高'),
			_Utils_Tuple2('こう', '港'),
			_Utils_Tuple2('こう', '広'),
			_Utils_Tuple2('こう', '幸'),
			_Utils_Tuple2('こう', '工'),
			_Utils_Tuple2('こう', '紅'),
			_Utils_Tuple2('こう', '光'),
			_Utils_Tuple2('ごう', '号'),
			_Utils_Tuple2('こうい', '行為'),
			_Utils_Tuple2('こうい', '好意'),
			_Utils_Tuple2('ごうい', '合意'),
			_Utils_Tuple2('こういう', 'こういう'),
			_Utils_Tuple2('こういてき', '好意的'),
			_Utils_Tuple2('ごういん', '強引'),
			_Utils_Tuple2('ごうう', '豪雨'),
			_Utils_Tuple2('こううん', '幸運'),
			_Utils_Tuple2('こうえい', '公営'),
			_Utils_Tuple2('こうえい', '光栄'),
			_Utils_Tuple2('こうえいじゅうたく', '公営住宅'),
			_Utils_Tuple2('こうえん', '講演'),
			_Utils_Tuple2('こうえん', '公演'),
			_Utils_Tuple2('こうおん', '高音'),
			_Utils_Tuple2('こうおん', '高温'),
			_Utils_Tuple2('こうか', '効果'),
			_Utils_Tuple2('こうか', '硬貨'),
			_Utils_Tuple2('こうか', '高価'),
			_Utils_Tuple2('ごうか', '豪華'),
			_Utils_Tuple2('こうかい', '後悔'),
			_Utils_Tuple2('こうかい', '公開'),
			_Utils_Tuple2('こうがい', '公害'),
			_Utils_Tuple2('こうがい', '郊外'),
			_Utils_Tuple2('こうかいこうざ', '公開講座'),
			_Utils_Tuple2('こうがく', '工学'),
			_Utils_Tuple2('こうがく', '高額'),
			_Utils_Tuple2('ごうかく', '合格'),
			_Utils_Tuple2('こうがくねん', '高学年'),
			_Utils_Tuple2('こうかてき', '効果的'),
			_Utils_Tuple2('こうかん', '交換'),
			_Utils_Tuple2('こうかん', '好感'),
			_Utils_Tuple2('こうき', '後期'),
			_Utils_Tuple2('こうぎ', '講義'),
			_Utils_Tuple2('こうぎ', '抗議'),
			_Utils_Tuple2('こうきあつ', '高気圧'),
			_Utils_Tuple2('こうきしん', '好奇心'),
			_Utils_Tuple2('こうきゅう', '高級'),
			_Utils_Tuple2('こうきゅう', '高給'),
			_Utils_Tuple2('こうきょ', '皇居'),
			_Utils_Tuple2('こうきょう', '公共'),
			_Utils_Tuple2('こうぎょう', '工業'),
			_Utils_Tuple2('こうきょうじぎょう', '公共事業'),
			_Utils_Tuple2('こうきょうりょうきん', '公共料金'),
			_Utils_Tuple2('こうくう', '航空'),
			_Utils_Tuple2('こうくうき', '航空機'),
			_Utils_Tuple2('こうくうびん', '航空便'),
			_Utils_Tuple2('こうけい', '光景'),
			_Utils_Tuple2('こうげい', '工芸'),
			_Utils_Tuple2('ごうけい', '合計'),
			_Utils_Tuple2('こうけいき', '好景気'),
			_Utils_Tuple2('こうけいしゃ', '後継者'),
			_Utils_Tuple2('こうげいひん', '工芸品'),
			_Utils_Tuple2('こうげき', '攻撃'),
			_Utils_Tuple2('こうけつあつ', '高血圧'),
			_Utils_Tuple2('こうげん', '公言'),
			_Utils_Tuple2('こうげん', '高原'),
			_Utils_Tuple2('こうご', '交互'),
			_Utils_Tuple2('こうこう', '孝行'),
			_Utils_Tuple2('こうこく', '広告'),
			_Utils_Tuple2('こうこくぶん', '広告文'),
			_Utils_Tuple2('こうさ', '交差'),
			_Utils_Tuple2('こうざ', '口座'),
			_Utils_Tuple2('こうざ', '講座'),
			_Utils_Tuple2('こうさい', '交際'),
			_Utils_Tuple2('こうさてん', '交差点'),
			_Utils_Tuple2('こうし', '講師'),
			_Utils_Tuple2('こうじ', '工事'),
			_Utils_Tuple2('こうしき', '公式'),
			_Utils_Tuple2('こうしきせん', '公式戦'),
			_Utils_Tuple2('こうしつ', '皇室'),
			_Utils_Tuple2('こうじつ', '口実'),
			_Utils_Tuple2('こうしゃ', '後者'),
			_Utils_Tuple2('こうしゃ', '校舎'),
			_Utils_Tuple2('こうしゅう', '講習'),
			_Utils_Tuple2('こうしゅう', '公衆'),
			_Utils_Tuple2('こうしゅうでんわ', '公衆電話'),
			_Utils_Tuple2('こうしゅうべんじょ', '公衆便所'),
			_Utils_Tuple2('こうしょ', '高所'),
			_Utils_Tuple2('こうしょう', '交渉'),
			_Utils_Tuple2('こうじょう', '工場'),
			_Utils_Tuple2('こうじょう', '向上'),
			_Utils_Tuple2('こうじょうけん', '好条件'),
			_Utils_Tuple2('こうしん', '行進'),
			_Utils_Tuple2('こうしん', '更新'),
			_Utils_Tuple2('こうしんりょう', '香辛料'),
			_Utils_Tuple2('こうすい', '香水'),
			_Utils_Tuple2('こうずい', '洪水'),
			_Utils_Tuple2('こうすいりょう', '降水量'),
			_Utils_Tuple2('こうせい', '構成'),
			_Utils_Tuple2('こうせい', '公正'),
			_Utils_Tuple2('こうせい', '後世'),
			_Utils_Tuple2('ごうせい', '合成'),
			_Utils_Tuple2('こうせき', '功績'),
			_Utils_Tuple2('こうぜん', '公然'),
			_Utils_Tuple2('こうそう', '高層'),
			_Utils_Tuple2('こうそう', '構想'),
			_Utils_Tuple2('こうぞう', '構造'),
			_Utils_Tuple2('こうそく', '高速'),
			_Utils_Tuple2('こうそく', '拘束'),
			_Utils_Tuple2('こうそく', '校則'),
			_Utils_Tuple2('こうぞく', '皇族'),
			_Utils_Tuple2('こうぞく', '後続'),
			_Utils_Tuple2('こうそくどうろ', '高速道路'),
			_Utils_Tuple2('こうそつ', '高卒'),
			_Utils_Tuple2('こうたい', '交代'),
			_Utils_Tuple2('こうたい', '後退'),
			_Utils_Tuple2('こうだい', '広大'),
			_Utils_Tuple2('こうたいし', '皇太子'),
			_Utils_Tuple2('こうち', '高地'),
			_Utils_Tuple2('こうちょう', '好調'),
			_Utils_Tuple2('こうつうきかん', '交通機関'),
			_Utils_Tuple2('こうつうじゅうたい', '交通渋滞'),
			_Utils_Tuple2('こうつうもう', '交通網'),
			_Utils_Tuple2('こうてい', '肯定'),
			_Utils_Tuple2('こうてい', '高低'),
			_Utils_Tuple2('こうてい', '校庭'),
			_Utils_Tuple2('こうていてき', '肯定的'),
			_Utils_Tuple2('こうてき', '公的'),
			_Utils_Tuple2('こうど', '高度'),
			_Utils_Tuple2('こうとう', '高等'),
			_Utils_Tuple2('こうとう', '口頭'),
			_Utils_Tuple2('こうどう', '行動'),
			_Utils_Tuple2('こうどう', '公道'),
			_Utils_Tuple2('ごうとう', '強盗'),
			_Utils_Tuple2('ごうどう', '合同'),
			_Utils_Tuple2('こうとうがっこう', '高等学校'),
			_Utils_Tuple2('こうとうせんもんがっこう', '高等専門学校'),
			_Utils_Tuple2('こうどせいちょう', '高度成長'),
			_Utils_Tuple2('こうない', '校内'),
			_Utils_Tuple2('こうない', '構内'),
			_Utils_Tuple2('こうにゅう', '購入'),
			_Utils_Tuple2('こうにん', '公認'),
			_Utils_Tuple2('こうねつ', '高熱'),
			_Utils_Tuple2('こうねつひ', '光熱費'),
			_Utils_Tuple2('こうのう', '効能'),
			_Utils_Tuple2('こうはい', '後輩'),
			_Utils_Tuple2('こうはく', '紅白'),
			_Utils_Tuple2('こうはん', '後半'),
			_Utils_Tuple2('こうはんい', '広範囲'),
			_Utils_Tuple2('ごうひ', '合否'),
			_Utils_Tuple2('こうひょう', '好評'),
			_Utils_Tuple2('こうひょう', '公表'),
			_Utils_Tuple2('こうひょう', '好評'),
			_Utils_Tuple2('こうぶ', '後部'),
			_Utils_Tuple2('こうふう', '校風'),
			_Utils_Tuple2('こうふく', '幸福'),
			_Utils_Tuple2('こうぶつ', '好物'),
			_Utils_Tuple2('こうふん', '興奮'),
			_Utils_Tuple2('こうへい', '公平'),
			_Utils_Tuple2('こうほ', '候補'),
			_Utils_Tuple2('こうほう', '後方'),
			_Utils_Tuple2('こうみんかん', '公民館'),
			_Utils_Tuple2('こうむ', '公務'),
			_Utils_Tuple2('こうむいん', '公務員'),
			_Utils_Tuple2('こうもく', '項目'),
			_Utils_Tuple2('こうよう', '紅葉'),
			_Utils_Tuple2('こうよう', '公用'),
			_Utils_Tuple2('こうよう', '効用'),
			_Utils_Tuple2('こうようご', '公用語'),
			_Utils_Tuple2('ごうりか', '合理化'),
			_Utils_Tuple2('こうりつ', '公立'),
			_Utils_Tuple2('こうりつ', '効率'),
			_Utils_Tuple2('こうりつてき', '効率的'),
			_Utils_Tuple2('こうりゅう', '交流'),
			_Utils_Tuple2('ごうりゅう', '合流'),
			_Utils_Tuple2('こうれい', '高齢'),
			_Utils_Tuple2('こえる', '超える'),
			_Utils_Tuple2('こえる', '越える'),
			_Utils_Tuple2('ごー', 'ゴー'),
			_Utils_Tuple2('こーす', 'コース'),
			_Utils_Tuple2('こーすたー', 'コースター'),
			_Utils_Tuple2('ごーすと', 'ゴースト'),
			_Utils_Tuple2('こーち', 'コーチ'),
			_Utils_Tuple2('こーでぃねーと', 'コーディネート'),
			_Utils_Tuple2('こーなー', 'コーナー'),
			_Utils_Tuple2('こーひーまめ', 'コーヒー豆'),
			_Utils_Tuple2('こーぽれーしょん', 'コーポレーション'),
			_Utils_Tuple2('こーらす', 'コーラス'),
			_Utils_Tuple2('こおりみず', '氷水'),
			_Utils_Tuple2('こおる', '凍る'),
			_Utils_Tuple2('ごーる', 'ゴール'),
			_Utils_Tuple2('ごーるきっく', 'ゴールキック'),
			_Utils_Tuple2('ごーるでん', 'ゴールデン'),
			_Utils_Tuple2('ごーるど', 'ゴールド'),
			_Utils_Tuple2('ごかい', '誤解'),
			_Utils_Tuple2('ごがく', '語学'),
			_Utils_Tuple2('こがす', '焦がす'),
			_Utils_Tuple2('こがた', '小型'),
			_Utils_Tuple2('こがら', '小柄'),
			_Utils_Tuple2('こぎって', '小切手'),
			_Utils_Tuple2('ごきぶり', 'ごきぶり'),
			_Utils_Tuple2('こきゅう', '呼吸'),
			_Utils_Tuple2('こきょう', '故郷'),
			_Utils_Tuple2('こぐ', 'こぐ'),
			_Utils_Tuple2('ごく', '語句'),
			_Utils_Tuple2('こくえい', '国営'),
			_Utils_Tuple2('こくおう', '国王'),
			_Utils_Tuple2('こくがい', '国外'),
			_Utils_Tuple2('こくぎ', '国技'),
			_Utils_Tuple2('こくご', '国語'),
			_Utils_Tuple2('ごくごく', 'ごくごく'),
			_Utils_Tuple2('こくごじてん', '国語辞典'),
			_Utils_Tuple2('こくさい', '国際'),
			_Utils_Tuple2('こくさいか', '国際化'),
			_Utils_Tuple2('こくさいかいぎ', '国際会議'),
			_Utils_Tuple2('こくさいくうこう', '国際空港'),
			_Utils_Tuple2('こくさいけっこん', '国際結婚'),
			_Utils_Tuple2('こくさいてき', '国際的'),
			_Utils_Tuple2('こくさいでんわ', '国際電話'),
			_Utils_Tuple2('こくさいとし', '国際都市'),
			_Utils_Tuple2('こくさいれんごう', '国際連合'),
			_Utils_Tuple2('こくさん', '国産'),
			_Utils_Tuple2('こくじん', '黒人'),
			_Utils_Tuple2('こくせき', '国籍'),
			_Utils_Tuple2('こくてつ', '国鉄'),
			_Utils_Tuple2('こくど', '国土'),
			_Utils_Tuple2('こくない', '国内'),
			_Utils_Tuple2('こくはく', '告白'),
			_Utils_Tuple2('こくばん', '黒板'),
			_Utils_Tuple2('こくふく', '克服'),
			_Utils_Tuple2('こくほう', '国宝'),
			_Utils_Tuple2('こくみん', '国民'),
			_Utils_Tuple2('こくみんけんこうほけん', '国民健康保険'),
			_Utils_Tuple2('こくみんせい', '国民性'),
			_Utils_Tuple2('こくみんのしゅくじつ', '国民の祝日'),
			_Utils_Tuple2('こくめい', '国名'),
			_Utils_Tuple2('こくもつ', '穀物'),
			_Utils_Tuple2('こくりつ', '国立'),
			_Utils_Tuple2('こくりつこうえん', '国立公園'),
			_Utils_Tuple2('こくれん', '国連'),
			_Utils_Tuple2('こげる', '焦げる'),
			_Utils_Tuple2('ここ', '個々'),
			_Utils_Tuple2('こごえ', '小声'),
			_Utils_Tuple2('ここなっつ', 'ココナッツ'),
			_Utils_Tuple2('ここら', 'ここら'),
			_Utils_Tuple2('こころから', '心から'),
			_Utils_Tuple2('こころぼそい', '心細い'),
			_Utils_Tuple2('こころみる', '試みる'),
			_Utils_Tuple2('こざら', '小皿'),
			_Utils_Tuple2('こし', '腰'),
			_Utils_Tuple2('こしつ', '個室'),
			_Utils_Tuple2('ごじつ', '後日'),
			_Utils_Tuple2('ごしっく', 'ゴシック'),
			_Utils_Tuple2('ごじゅうおん', '五十音'),
			_Utils_Tuple2('ごじゅうおんじゅん', '五十音順'),
			_Utils_Tuple2('こしょう', '胡椒'),
			_Utils_Tuple2('こしょう', '故障'),
			_Utils_Tuple2('こじん', '個人'),
			_Utils_Tuple2('こじんさ', '個人差'),
			_Utils_Tuple2('こじんしゅぎ', '個人主義'),
			_Utils_Tuple2('こじんてき', '個人的'),
			_Utils_Tuple2('こじんめい', '個人名'),
			_Utils_Tuple2('こす', '越す'),
			_Utils_Tuple2('こす', '超す'),
			_Utils_Tuple2('こすう', '個数'),
			_Utils_Tuple2('こすと', 'コスト'),
			_Utils_Tuple2('こすとだうん', 'コストダウン'),
			_Utils_Tuple2('こすもす', 'コスモス'),
			_Utils_Tuple2('こする', 'こする'),
			_Utils_Tuple2('こせい', '個性'),
			_Utils_Tuple2('こせいてき', '個性的'),
			_Utils_Tuple2('こぜに', '小銭'),
			_Utils_Tuple2('こそこそ', 'こそこそ'),
			_Utils_Tuple2('こそだて', '子育て'),
			_Utils_Tuple2('こたい', '固体'),
			_Utils_Tuple2('こたい', '個体'),
			_Utils_Tuple2('こだい', '古代'),
			_Utils_Tuple2('こたえる', '応える'),
			_Utils_Tuple2('こたつ', 'こたつ'),
			_Utils_Tuple2('ごちそう', 'ごちそう'),
			_Utils_Tuple2('ごちゃごちゃ', 'ごちゃごちゃ'),
			_Utils_Tuple2('ごちゃごちゃ', 'ごちゃごちゃ'),
			_Utils_Tuple2('こちらこそ', 'こちらこそ'),
			_Utils_Tuple2('こつ', 'こつ'),
			_Utils_Tuple2('こっか', '国家'),
			_Utils_Tuple2('こっか', '国歌'),
			_Utils_Tuple2('こっかい', '国会'),
			_Utils_Tuple2('こづかい', '小遣い'),
			_Utils_Tuple2('こっかいぎいん', '国会議員'),
			_Utils_Tuple2('こっかいぎじどう', '国会議事堂'),
			_Utils_Tuple2('こっかしけん', '国家試験'),
			_Utils_Tuple2('こっき', '国旗'),
			_Utils_Tuple2('こっきょう', '国境'),
			_Utils_Tuple2('こつこつ', 'こつこつ'),
			_Utils_Tuple2('こっせつ', '骨折'),
			_Utils_Tuple2('こっそり', 'こっそり'),
			_Utils_Tuple2('ごっど', 'ゴッド'),
			_Utils_Tuple2('こっとん', 'コットン'),
			_Utils_Tuple2('こてい', '固定'),
			_Utils_Tuple2('こと', '古都'),
			_Utils_Tuple2('ごと', '毎'),
			_Utils_Tuple2('ことがら', '事柄'),
			_Utils_Tuple2('こどく', '孤独'),
			_Utils_Tuple2('ことなる', '異なる'),
			_Utils_Tuple2('ことに', '殊に'),
			_Utils_Tuple2('ことばづかい', '言葉遣い'),
			_Utils_Tuple2('こどもらしい', '子供らしい'),
			_Utils_Tuple2('ことり', '小鳥'),
			_Utils_Tuple2('ことわざ', 'ことわざ'),
			_Utils_Tuple2('ことわる', '断る'),
			_Utils_Tuple2('こな', '粉'),
			_Utils_Tuple2('こなごな', '粉々'),
			_Utils_Tuple2('こね', 'コネ'),
			_Utils_Tuple2('こねこ', '子猫'),
			_Utils_Tuple2('このたび', 'このたび'),
			_Utils_Tuple2('このは', '木の葉'),
			_Utils_Tuple2('このまま', '此の侭'),
			_Utils_Tuple2('このみ', '好み'),
			_Utils_Tuple2('このむ', '好む'),
			_Utils_Tuple2('このよ', 'この世'),
			_Utils_Tuple2('こぴーらいたー', 'コピーライター'),
			_Utils_Tuple2('こふう', '古風'),
			_Utils_Tuple2('ごぶさた', 'ごぶさた'),
			_Utils_Tuple2('こべつ', '個別'),
			_Utils_Tuple2('ごぼう', 'ごぼう'),
			_Utils_Tuple2('こぼす', 'こぼす'),
			_Utils_Tuple2('こぼれる', 'こぼれる'),
			_Utils_Tuple2('こま', 'こま'),
			_Utils_Tuple2('ごま', 'ごま'),
			_Utils_Tuple2('こまーしゃる', 'コマーシャル'),
			_Utils_Tuple2('こまかい', '細かい'),
			_Utils_Tuple2('ごまかす', 'ごまかす'),
			_Utils_Tuple2('こみ', '込み'),
			_Utils_Tuple2('こみあう', '込み合う'),
			_Utils_Tuple2('こみっく', 'コミック'),
			_Utils_Tuple2('こみゅにけーしょん', 'コミュニケーション'),
			_Utils_Tuple2('こみゅにけーと', 'コミュニケート'),
			_Utils_Tuple2('こみゅにてぃー', 'コミュニティー'),
			_Utils_Tuple2('こむ', '込む'),
			_Utils_Tuple2('ごむ', 'ゴム'),
			_Utils_Tuple2('こむぎ', '小麦'),
			_Utils_Tuple2('こむぎこ', '小麦粉'),
			_Utils_Tuple2('こめでぃあん', 'コメディアン'),
			_Utils_Tuple2('こめでぃー', 'コメディー'),
			_Utils_Tuple2('こめる', '込める'),
			_Utils_Tuple2('ごめんください', 'ごめんください'),
			_Utils_Tuple2('こめんと', 'コメント'),
			_Utils_Tuple2('こもの', '小物'),
			_Utils_Tuple2('こもり', '子守り'),
			_Utils_Tuple2('こよう', '雇用'),
			_Utils_Tuple2('こら', 'こら'),
			_Utils_Tuple2('こらーげん', 'コラーゲン'),
			_Utils_Tuple2('ごらく', '娯楽'),
			_Utils_Tuple2('こらむ', 'コラム'),
			_Utils_Tuple2('ごらん', '御覧'),
			_Utils_Tuple2('ごらんになる', '御覧になる'),
			_Utils_Tuple2('こりつ', '孤立'),
			_Utils_Tuple2('ごりら', 'ゴリラ'),
			_Utils_Tuple2('こるく', 'コルク'),
			_Utils_Tuple2('ごるふァー', 'ゴルファー'),
			_Utils_Tuple2('これくしょん', 'コレクション'),
			_Utils_Tuple2('これくたー', 'コレクター'),
			_Utils_Tuple2('これすてろーる', 'コレステロール'),
			_Utils_Tuple2('これまで', 'これまで'),
			_Utils_Tuple2('ころがす', '転がす'),
			_Utils_Tuple2('ころがる', '転がる'),
			_Utils_Tuple2('ころころ', 'ころころ'),
			_Utils_Tuple2('ごろごろ', 'ごろごろ'),
			_Utils_Tuple2('ころし', '殺し'),
			_Utils_Tuple2('ころしあむ', 'コロシアム'),
			_Utils_Tuple2('ころす', '殺す'),
			_Utils_Tuple2('ころっけ', 'コロッケ'),
			_Utils_Tuple2('ころぶ', '転ぶ'),
			_Utils_Tuple2('ころもがえ', '衣替え'),
			_Utils_Tuple2('こわれもの', '壊れ物'),
			_Utils_Tuple2('こわれる', '壊れる'),
			_Utils_Tuple2('こん', '今'),
			_Utils_Tuple2('こん', '紺'),
			_Utils_Tuple2('こんいろ', '紺色'),
			_Utils_Tuple2('こんかい', '今回'),
			_Utils_Tuple2('こんき', '今季'),
			_Utils_Tuple2('こんき', '今期'),
			_Utils_Tuple2('こんくーる', 'コンクール'),
			_Utils_Tuple2('こんくりーと', 'コンクリート'),
			_Utils_Tuple2('こんけつ', '混血'),
			_Utils_Tuple2('こんご', '今後'),
			_Utils_Tuple2('こんごう', '混合'),
			_Utils_Tuple2('こんざつ', '混雑'),
			_Utils_Tuple2('こんしゅん', '今春'),
			_Utils_Tuple2('こんそめ', 'コンソメ'),
			_Utils_Tuple2('こんたくと', 'コンタクト'),
			_Utils_Tuple2('こんだて', '献立'),
			_Utils_Tuple2('こんでぃしょん', 'コンディション'),
			_Utils_Tuple2('こんてすと', 'コンテスト'),
			_Utils_Tuple2('こんと', 'コント'),
			_Utils_Tuple2('こんどう', '混同'),
			_Utils_Tuple2('こんとろーる', 'コントロール'),
			_Utils_Tuple2('こんなん', '困難'),
			_Utils_Tuple2('こんにち', '今日'),
			_Utils_Tuple2('こんにゃく', 'こんにゃく'),
			_Utils_Tuple2('こんねんど', '今年度'),
			_Utils_Tuple2('こんねんど', '今年度'),
			_Utils_Tuple2('こんぱ', 'コンパ'),
			_Utils_Tuple2('こんぱくと', 'コンパクト'),
			_Utils_Tuple2('こんぱくとでぃすく', 'コンパクトディスク'),
			_Utils_Tuple2('こんび', 'コンビ'),
			_Utils_Tuple2('こんびにえんす', 'コンビニエンス'),
			_Utils_Tuple2('こんびねーしょん', 'コンビネーション'),
			_Utils_Tuple2('こんぴゅーた', 'コンピュータ'),
			_Utils_Tuple2('こんぽん', '根本'),
			_Utils_Tuple2('こんぽんてき', '根本的'),
			_Utils_Tuple2('こんやく', '婚約'),
			_Utils_Tuple2('こんらん', '混乱'),
			_Utils_Tuple2('こんろ', 'こんろ'),
			_Utils_Tuple2('さ', '差'),
			_Utils_Tuple2('さあ', 'さあ'),
			_Utils_Tuple2('さーかす', 'サーカス'),
			_Utils_Tuple2('さーくる', 'サークル'),
			_Utils_Tuple2('ざーさい', 'ザーサイ'),
			_Utils_Tuple2('さーびす', 'サービス'),
			_Utils_Tuple2('さーびすえりあ', 'サービスエリア'),
			_Utils_Tuple2('さーびすぎょう', 'サービス業'),
			_Utils_Tuple2('さーふァー', 'サーファー'),
			_Utils_Tuple2('さーふぃん', 'サーフィン'),
			_Utils_Tuple2('さーもん', 'サーモン'),
			_Utils_Tuple2('さい', '最'),
			_Utils_Tuple2('さい', '際'),
			_Utils_Tuple2('さい', '再'),
			_Utils_Tuple2('さい', '祭'),
			_Utils_Tuple2('さい', '才'),
			_Utils_Tuple2('ざい', '材'),
			_Utils_Tuple2('ざい', '剤'),
			_Utils_Tuple2('さいあく', '最悪'),
			_Utils_Tuple2('さいかい', '再会'),
			_Utils_Tuple2('さいかい', '再開'),
			_Utils_Tuple2('さいがい', '災害'),
			_Utils_Tuple2('ざいがく', '在学'),
			_Utils_Tuple2('さいきょう', '最強'),
			_Utils_Tuple2('さいくりんぐ', 'サイクリング'),
			_Utils_Tuple2('さいくる', 'サイクル'),
			_Utils_Tuple2('さいけん', '再建'),
			_Utils_Tuple2('さいげん', '再現'),
			_Utils_Tuple2('さいこ', '最古'),
			_Utils_Tuple2('さいご', '最期'),
			_Utils_Tuple2('さいこう', '最高'),
			_Utils_Tuple2('さいこうきゅう', '最高級'),
			_Utils_Tuple2('さいころ', 'さいころ'),
			_Utils_Tuple2('さいこん', '再婚'),
			_Utils_Tuple2('ざいさん', '財産'),
			_Utils_Tuple2('さいし', '妻子'),
			_Utils_Tuple2('さいじつ', '祭日'),
			_Utils_Tuple2('ざいしつ', '材質'),
			_Utils_Tuple2('さいしゅう', '最終'),
			_Utils_Tuple2('さいしゅう', '採集'),
			_Utils_Tuple2('ざいじゅう', '在住'),
			_Utils_Tuple2('さいしゅうてき', '最終的'),
			_Utils_Tuple2('さいしょ', '最初'),
			_Utils_Tuple2('さいしょう', '最少'),
			_Utils_Tuple2('さいしょう', '最小'),
			_Utils_Tuple2('さいじょう', '最上'),
			_Utils_Tuple2('さいしょうげん', '最小限'),
			_Utils_Tuple2('ざいしょく', '在職'),
			_Utils_Tuple2('さいしん', '最新'),
			_Utils_Tuple2('さいしんしき', '最新式'),
			_Utils_Tuple2('さいず', 'サイズ'),
			_Utils_Tuple2('さいせい', '再生'),
			_Utils_Tuple2('ざいせき', '在籍'),
			_Utils_Tuple2('さいぜんれつ', '最前列'),
			_Utils_Tuple2('さいそく', '催促'),
			_Utils_Tuple2('さいた', '最多'),
			_Utils_Tuple2('さいだー', 'サイダー'),
			_Utils_Tuple2('さいだい', '最大'),
			_Utils_Tuple2('さいだいきゅう', '最大級'),
			_Utils_Tuple2('さいだいげん', '最大限'),
			_Utils_Tuple2('さいたん', '最短'),
			_Utils_Tuple2('さいちゅう', '最中'),
			_Utils_Tuple2('さいちょう', '最長'),
			_Utils_Tuple2('さいてい', '最低'),
			_Utils_Tuple2('さいていげん', '最低限'),
			_Utils_Tuple2('さいてき', '最適'),
			_Utils_Tuple2('さいてん', '採点'),
			_Utils_Tuple2('さいと', 'サイト'),
			_Utils_Tuple2('さいど', 'サイド'),
			_Utils_Tuple2('さいど', '再度'),
			_Utils_Tuple2('ざいにち', '在日'),
			_Utils_Tuple2('さいのう', '才能'),
			_Utils_Tuple2('さいはつ', '再発'),
			_Utils_Tuple2('さいはっけん', '再発見'),
			_Utils_Tuple2('ざいもく', '材木'),
			_Utils_Tuple2('さいゆうせん', '最優先'),
			_Utils_Tuple2('さいよう', '採用'),
			_Utils_Tuple2('ざいりゅう', '在留'),
			_Utils_Tuple2('さいりょう', '最良'),
			_Utils_Tuple2('ざいりょう', '材料'),
			_Utils_Tuple2('さいれん', 'サイレン'),
			_Utils_Tuple2('さうな', 'サウナ'),
			_Utils_Tuple2('さうんど', 'サウンド'),
			_Utils_Tuple2('さか', '坂'),
			_Utils_Tuple2('さかい', '境'),
			_Utils_Tuple2('さかいめ', '境目'),
			_Utils_Tuple2('さかさ', '逆さ'),
			_Utils_Tuple2('さがしだす', '捜し出す'),
			_Utils_Tuple2('さがす', '探す'),
			_Utils_Tuple2('さかみち', '坂道'),
			_Utils_Tuple2('さかや', '酒屋'),
			_Utils_Tuple2('さからう', '逆らう'),
			_Utils_Tuple2('さがり', '下がり'),
			_Utils_Tuple2('さがる', '下がる'),
			_Utils_Tuple2('さかん', '盛ん'),
			_Utils_Tuple2('さき', '先'),
			_Utils_Tuple2('さき', '咲き'),
			_Utils_Tuple2('さきに', '先に'),
			_Utils_Tuple2('さきほど', '先程'),
			_Utils_Tuple2('さきほど', '先ほど'),
			_Utils_Tuple2('さぎょう', '作業'),
			_Utils_Tuple2('さく', '咲く'),
			_Utils_Tuple2('さく', '昨'),
			_Utils_Tuple2('さく', '作'),
			_Utils_Tuple2('さく', '裂く'),
			_Utils_Tuple2('さく', '作'),
			_Utils_Tuple2('さく', '割く'),
			_Utils_Tuple2('さくし', '作詞'),
			_Utils_Tuple2('さくしゃ', '作者'),
			_Utils_Tuple2('さくじょ', '削除'),
			_Utils_Tuple2('さくず', '作図'),
			_Utils_Tuple2('さくせい', '作成'),
			_Utils_Tuple2('さくせい', '作製'),
			_Utils_Tuple2('さくせん', '作戦'),
			_Utils_Tuple2('さくねん', '昨年'),
			_Utils_Tuple2('さくばん', '昨晩'),
			_Utils_Tuple2('さくひん', '作品'),
			_Utils_Tuple2('さくもつ', '作物'),
			_Utils_Tuple2('さくや', '昨夜'),
			_Utils_Tuple2('さくらもち', '桜もち'),
			_Utils_Tuple2('さぐる', '探る'),
			_Utils_Tuple2('さけくさい', '酒臭い'),
			_Utils_Tuple2('さけずき', '酒好き'),
			_Utils_Tuple2('さけび', '叫び'),
			_Utils_Tuple2('さけぶ', '叫ぶ'),
			_Utils_Tuple2('さける', '避ける'),
			_Utils_Tuple2('さげる', '下げる'),
			_Utils_Tuple2('ささ', 'ささ'),
			_Utils_Tuple2('ささえ', '支え'),
			_Utils_Tuple2('ささえる', '支える'),
			_Utils_Tuple2('ささる', '刺さる'),
			_Utils_Tuple2('さじ', 'さじ'),
			_Utils_Tuple2('さしあげる', '差し上げる'),
			_Utils_Tuple2('さしころす', '刺し殺す'),
			_Utils_Tuple2('さす', '指す'),
			_Utils_Tuple2('さす', '刺す'),
			_Utils_Tuple2('さす', '差す'),
			_Utils_Tuple2('さすが', 'さすが'),
			_Utils_Tuple2('さすが', '流石'),
			_Utils_Tuple2('さすぺんす', 'サスペンス'),
			_Utils_Tuple2('ざせき', '座席'),
			_Utils_Tuple2('させつ', '左折'),
			_Utils_Tuple2('ざせつ', '挫折'),
			_Utils_Tuple2('さそい', '誘い'),
			_Utils_Tuple2('さそう', '誘う'),
			_Utils_Tuple2('さち', '幸'),
			_Utils_Tuple2('さつ', '札'),
			_Utils_Tuple2('ざつ', '雑'),
			_Utils_Tuple2('さつい', '殺意'),
			_Utils_Tuple2('さつえい', '撮影'),
			_Utils_Tuple2('ざつおん', '雑音'),
			_Utils_Tuple2('さっか', '作家'),
			_Utils_Tuple2('ざっか', '雑貨'),
			_Utils_Tuple2('さつがい', '殺害'),
			_Utils_Tuple2('さっきょく', '作曲'),
			_Utils_Tuple2('さっさと', 'さっさと'),
			_Utils_Tuple2('さつじん', '殺人'),
			_Utils_Tuple2('さっそく', '早速'),
			_Utils_Tuple2('ざつだん', '雑談'),
			_Utils_Tuple2('さっと', 'さっと'),
			_Utils_Tuple2('ざっと', 'ざっと'),
			_Utils_Tuple2('さっぱり', 'さっぱり'),
			_Utils_Tuple2('さて', 'さて'),
			_Utils_Tuple2('さどう', '茶道'),
			_Utils_Tuple2('さどう', '作動'),
			_Utils_Tuple2('さばく', '砂漠'),
			_Utils_Tuple2('さびしい', '寂しい'),
			_Utils_Tuple2('ざふとん', '座布団'),
			_Utils_Tuple2('さぷりめんと', 'サプリメント'),
			_Utils_Tuple2('さべつ', '差別'),
			_Utils_Tuple2('さぽーたー', 'サポーター'),
			_Utils_Tuple2('さぽーと', 'サポート'),
			_Utils_Tuple2('さぼる', 'サボる'),
			_Utils_Tuple2('さまー', 'サマー'),
			_Utils_Tuple2('さまざま', '様々'),
			_Utils_Tuple2('さまざま', '様々'),
			_Utils_Tuple2('さます', '覚ます'),
			_Utils_Tuple2('さます', '冷ます'),
			_Utils_Tuple2('さまたげ', '妨げ'),
			_Utils_Tuple2('さまたげる', '妨げる'),
			_Utils_Tuple2('さみっと', 'サミット'),
			_Utils_Tuple2('さむけ', '寒気'),
			_Utils_Tuple2('さむらい', '侍'),
			_Utils_Tuple2('さめる', '冷める'),
			_Utils_Tuple2('さめる', '覚める'),
			_Utils_Tuple2('さゆう', '左右'),
			_Utils_Tuple2('さら', '皿'),
			_Utils_Tuple2('さらに', '更に'),
			_Utils_Tuple2('さらみ', 'サラミ'),
			_Utils_Tuple2('さらりー', 'サラリー'),
			_Utils_Tuple2('さらりーまん', 'サラリーマン'),
			_Utils_Tuple2('さる', 'さる'),
			_Utils_Tuple2('さる', '去る'),
			_Utils_Tuple2('ざる', 'ざる'),
			_Utils_Tuple2('ざるそば', 'ざるそば'),
			_Utils_Tuple2('さわがしい', '騒がしい'),
			_Utils_Tuple2('さわぎ', '騒ぎ'),
			_Utils_Tuple2('さわぐ', '騒ぐ'),
			_Utils_Tuple2('ざわざわ', 'ざわざわ'),
			_Utils_Tuple2('さわやか', 'さわやか'),
			_Utils_Tuple2('さわる', '触る'),
			_Utils_Tuple2('さん', '産'),
			_Utils_Tuple2('さん', '産'),
			_Utils_Tuple2('さんか', '参加'),
			_Utils_Tuple2('さんかく', '三角'),
			_Utils_Tuple2('さんかくけい', '三角形'),
			_Utils_Tuple2('さんかん', '参観'),
			_Utils_Tuple2('さんぎょう', '産業'),
			_Utils_Tuple2('ざんぎょう', '残業'),
			_Utils_Tuple2('さんぎょうかくめい', '産業革命'),
			_Utils_Tuple2('ざんきん', '残金'),
			_Utils_Tuple2('さんこう', '参考'),
			_Utils_Tuple2('さんこうしょ', '参考書'),
			_Utils_Tuple2('さんしゅつ', '算出'),
			_Utils_Tuple2('ざんしょ', '残暑'),
			_Utils_Tuple2('さんしょう', '参照'),
			_Utils_Tuple2('さんしょく', '三色'),
			_Utils_Tuple2('さんすう', '算数'),
			_Utils_Tuple2('さんせい', '賛成'),
			_Utils_Tuple2('さんそ', '酸素'),
			_Utils_Tuple2('さんだい', '三代'),
			_Utils_Tuple2('ざんだか', '残高'),
			_Utils_Tuple2('さんだる', 'サンダル'),
			_Utils_Tuple2('さんち', '山地'),
			_Utils_Tuple2('さんち', '産地'),
			_Utils_Tuple2('さんちゅう', '山中'),
			_Utils_Tuple2('さんちょう', '山頂'),
			_Utils_Tuple2('さんどう', '賛同'),
			_Utils_Tuple2('さんなん', '三男'),
			_Utils_Tuple2('さんば', 'サンバ'),
			_Utils_Tuple2('さんふじんか', '産婦人科'),
			_Utils_Tuple2('さんぶつ', '産物'),
			_Utils_Tuple2('さんぷる', 'サンプル'),
			_Utils_Tuple2('さんま', 'さんま'),
			_Utils_Tuple2('さんりんしゃ', '三輪車'),
			_Utils_Tuple2('し', '市'),
			_Utils_Tuple2('し', '氏'),
			_Utils_Tuple2('し', '子'),
			_Utils_Tuple2('し', '死'),
			_Utils_Tuple2('し', '誌'),
			_Utils_Tuple2('し', '詞'),
			_Utils_Tuple2('し', '士'),
			_Utils_Tuple2('し', '師'),
			_Utils_Tuple2('し', '紙'),
			_Utils_Tuple2('し', '詩'),
			_Utils_Tuple2('し', '氏'),
			_Utils_Tuple2('し', '私'),
			_Utils_Tuple2('じ', '寺'),
			_Utils_Tuple2('じ', '字'),
			_Utils_Tuple2('じ', '事'),
			_Utils_Tuple2('じ', '次'),
			_Utils_Tuple2('じ', '次'),
			_Utils_Tuple2('しあい', '試合'),
			_Utils_Tuple2('しあがり', '仕上がり'),
			_Utils_Tuple2('しあがる', '仕上がる'),
			_Utils_Tuple2('しあたー', 'シアター'),
			_Utils_Tuple2('しあわせ', '幸せ'),
			_Utils_Tuple2('じい', 'じい'),
			_Utils_Tuple2('しーえむ', 'ＣＭ'),
			_Utils_Tuple2('しーえむそんぐ', 'ＣＭソング'),
			_Utils_Tuple2('しーしー', 'ＣＣ'),
			_Utils_Tuple2('しーじー', 'ＣＧ'),
			_Utils_Tuple2('しーずん', 'シーズン'),
			_Utils_Tuple2('しいたけ', '椎茸'),
			_Utils_Tuple2('じいちゃん', 'じいちゃん'),
			_Utils_Tuple2('しーつ', 'シーツ'),
			_Utils_Tuple2('しーでぃーぷれーやー', 'ＣＤプレーヤー'),
			_Utils_Tuple2('じーぱん', 'ジーパン'),
			_Utils_Tuple2('しーふーど', 'シーフード'),
			_Utils_Tuple2('しーる', 'シール'),
			_Utils_Tuple2('しいれ', '仕入れ'),
			_Utils_Tuple2('しいれる', '仕入れる'),
			_Utils_Tuple2('しいん', '子音'),
			_Utils_Tuple2('しーん', 'シーン'),
			_Utils_Tuple2('じいん', '寺院'),
			_Utils_Tuple2('しぇあ', 'シェア'),
			_Utils_Tuple2('しえい', '市営'),
			_Utils_Tuple2('じえい', '自営'),
			_Utils_Tuple2('じえいたい', '自衛隊'),
			_Utils_Tuple2('しぇーばー', 'シェーバー'),
			_Utils_Tuple2('しぇーぷあっぷ', 'シェープアップ'),
			_Utils_Tuple2('じぇすちゃー', 'ジェスチャー'),
			_Utils_Tuple2('じぇっとこーすたー', 'ジェットコースター'),
			_Utils_Tuple2('じぇねれーしょん', 'ジェネレーション'),
			_Utils_Tuple2('しぇふ', 'シェフ'),
			_Utils_Tuple2('しえん', '支援'),
			_Utils_Tuple2('しおあじ', '塩味'),
			_Utils_Tuple2('しおかげん', '塩加減'),
			_Utils_Tuple2('しおからい', '塩辛い'),
			_Utils_Tuple2('しか', 'しか'),
			_Utils_Tuple2('しか', '歯科'),
			_Utils_Tuple2('しかい', '司会'),
			_Utils_Tuple2('しがい', '市外'),
			_Utils_Tuple2('じかい', '次回'),
			_Utils_Tuple2('しがいせん', '紫外線'),
			_Utils_Tuple2('しかく', '四角'),
			_Utils_Tuple2('しかく', '資格'),
			_Utils_Tuple2('しかく', '視覚'),
			_Utils_Tuple2('じかく', '自覚'),
			_Utils_Tuple2('しかくい', '四角い'),
			_Utils_Tuple2('しかくしけん', '資格試験'),
			_Utils_Tuple2('じかせい', '自家製'),
			_Utils_Tuple2('しかた', '仕方'),
			_Utils_Tuple2('しかたない', '仕方無い'),
			_Utils_Tuple2('しかも', 'しかも'),
			_Utils_Tuple2('じかよう', '自家用'),
			_Utils_Tuple2('じかようしゃ', '自家用車'),
			_Utils_Tuple2('しかる', 'しかる'),
			_Utils_Tuple2('じかんがい', '時間外'),
			_Utils_Tuple2('じかんめ', '時間目'),
			_Utils_Tuple2('じかんわり', '時間割'),
			_Utils_Tuple2('しき', '四季'),
			_Utils_Tuple2('しき', '式'),
			_Utils_Tuple2('しき', '式'),
			_Utils_Tuple2('じき', '時期'),
			_Utils_Tuple2('じき', '次期'),
			_Utils_Tuple2('しききん', '敷金'),
			_Utils_Tuple2('しきしゃ', '指揮者'),
			_Utils_Tuple2('しきじょう', '式場'),
			_Utils_Tuple2('しきゅう', '至急'),
			_Utils_Tuple2('しきゅう', '支給'),
			_Utils_Tuple2('じきゅう', '時給'),
			_Utils_Tuple2('しきょ', '死去'),
			_Utils_Tuple2('じぎょう', '事業'),
			_Utils_Tuple2('しきん', '資金'),
			_Utils_Tuple2('しく', '敷く'),
			_Utils_Tuple2('しくみ', '仕組み'),
			_Utils_Tuple2('しけい', '死刑'),
			_Utils_Tuple2('しげき', '刺激'),
			_Utils_Tuple2('しげん', '資源'),
			_Utils_Tuple2('じけん', '事件'),
			_Utils_Tuple2('しけんてき', '試験的'),
			_Utils_Tuple2('しご', '私語'),
			_Utils_Tuple2('しご', '死後'),
			_Utils_Tuple2('じこ', '自己'),
			_Utils_Tuple2('しこう', '思考'),
			_Utils_Tuple2('しこく', '四国'),
			_Utils_Tuple2('じこく', '自国'),
			_Utils_Tuple2('じこく', '時刻'),
			_Utils_Tuple2('じごく', '地獄'),
			_Utils_Tuple2('じこくひょう', '時刻表'),
			_Utils_Tuple2('じこまんぞく', '自己満足'),
			_Utils_Tuple2('じさ', '時差'),
			_Utils_Tuple2('しさく', '試作'),
			_Utils_Tuple2('じさく', '自作'),
			_Utils_Tuple2('じさつ', '自殺'),
			_Utils_Tuple2('しさん', '資産'),
			_Utils_Tuple2('じさん', '持参'),
			_Utils_Tuple2('しじ', '支持'),
			_Utils_Tuple2('しじ', '指示'),
			_Utils_Tuple2('じじ', '時事'),
			_Utils_Tuple2('じしつ', '自室'),
			_Utils_Tuple2('じじつ', '事実'),
			_Utils_Tuple2('ししゃ', '支社'),
			_Utils_Tuple2('ししゃ', '死者'),
			_Utils_Tuple2('ししゃ', '使者'),
			_Utils_Tuple2('じしゃ', '寺社'),
			_Utils_Tuple2('じしゃく', '磁石'),
			_Utils_Tuple2('じしゅう', '自習'),
			_Utils_Tuple2('ししゅつ', '支出'),
			_Utils_Tuple2('じしゅてき', '自主的'),
			_Utils_Tuple2('ししゅんき', '思春期'),
			_Utils_Tuple2('じじょ', '次女'),
			_Utils_Tuple2('ししょう', '支障'),
			_Utils_Tuple2('ししょう', '死傷'),
			_Utils_Tuple2('しじょう', '市場'),
			_Utils_Tuple2('しじょう', '史上'),
			_Utils_Tuple2('しじょう', '紙上'),
			_Utils_Tuple2('じしょう', '自称'),
			_Utils_Tuple2('じじょう', '事情'),
			_Utils_Tuple2('しじょうかかく', '市場価格'),
			_Utils_Tuple2('しじょうちょうさ', '市場調査'),
			_Utils_Tuple2('ししょく', '試食'),
			_Utils_Tuple2('じしょく', '辞職'),
			_Utils_Tuple2('しじん', '詩人'),
			_Utils_Tuple2('じしん', '地震'),
			_Utils_Tuple2('じしん', '自身'),
			_Utils_Tuple2('じしん', '自信'),
			_Utils_Tuple2('じすい', '自炊'),
			_Utils_Tuple2('しすてむ', 'システム'),
			_Utils_Tuple2('しすてむえんじにあ', 'システムエンジニア'),
			_Utils_Tuple2('しすてむきっちん', 'システムキッチン'),
			_Utils_Tuple2('しずむ', '沈む'),
			_Utils_Tuple2('しずめる', '沈める'),
			_Utils_Tuple2('しずめる', '静める'),
			_Utils_Tuple2('しせい', '姿勢'),
			_Utils_Tuple2('じせだい', '次世代'),
			_Utils_Tuple2('しせつ', '施設'),
			_Utils_Tuple2('しせん', '視線'),
			_Utils_Tuple2('しぜん', '自然'),
			_Utils_Tuple2('しぜん', '自然'),
			_Utils_Tuple2('じぜん', '事前'),
			_Utils_Tuple2('しぜんかい', '自然界'),
			_Utils_Tuple2('しぜんかがく', '自然科学'),
			_Utils_Tuple2('しぜんげんしょう', '自然現象'),
			_Utils_Tuple2('しぜんさいがい', '自然災害'),
			_Utils_Tuple2('しそう', '思想'),
			_Utils_Tuple2('じそく', '時速'),
			_Utils_Tuple2('じぞく', '持続'),
			_Utils_Tuple2('しそん', '子孫'),
			_Utils_Tuple2('した', '舌'),
			_Utils_Tuple2('したい', '死体'),
			_Utils_Tuple2('しだい', '次第'),
			_Utils_Tuple2('しだい', '私大'),
			_Utils_Tuple2('じたい', '自体'),
			_Utils_Tuple2('じたい', '事態'),
			_Utils_Tuple2('じだい', '時代'),
			_Utils_Tuple2('じだいおくれ', '時代遅れ'),
			_Utils_Tuple2('じだいげき', '時代劇'),
			_Utils_Tuple2('しだいに', '次第に'),
			_Utils_Tuple2('したがう', '従う'),
			_Utils_Tuple2('したがき', '下書き'),
			_Utils_Tuple2('したぎ', '下着'),
			_Utils_Tuple2('したく', '支度'),
			_Utils_Tuple2('じたく', '自宅'),
			_Utils_Tuple2('したしい', '親しい'),
			_Utils_Tuple2('したじき', '下敷き'),
			_Utils_Tuple2('したしみ', '親しみ'),
			_Utils_Tuple2('したしむ', '親しむ'),
			_Utils_Tuple2('したまち', '下町'),
			_Utils_Tuple2('したまわる', '下回る'),
			_Utils_Tuple2('したむき', '下向き'),
			_Utils_Tuple2('じち', '自治'),
			_Utils_Tuple2('しちごさん', '七五三'),
			_Utils_Tuple2('じちたい', '自治体'),
			_Utils_Tuple2('しちゃく', '試着'),
			_Utils_Tuple2('しちゅー', 'シチュー'),
			_Utils_Tuple2('しちゅえーしょん', 'シチュエーション'),
			_Utils_Tuple2('しちょう', '市長'),
			_Utils_Tuple2('しちょう', '試聴'),
			_Utils_Tuple2('しちょうしゃ', '視聴者'),
			_Utils_Tuple2('しちょうそん', '市町村'),
			_Utils_Tuple2('しちょうりつ', '視聴率'),
			_Utils_Tuple2('しつ', '室'),
			_Utils_Tuple2('しつ', '質'),
			_Utils_Tuple2('しつ', '質'),
			_Utils_Tuple2('じつ', '実'),
			_Utils_Tuple2('しつおん', '室温'),
			_Utils_Tuple2('じっか', '実家'),
			_Utils_Tuple2('しつがい', '室外'),
			_Utils_Tuple2('しっかく', '失格'),
			_Utils_Tuple2('しっかり', 'しっかり'),
			_Utils_Tuple2('じっかん', '実感'),
			_Utils_Tuple2('じつぎ', '実技'),
			_Utils_Tuple2('しつぎょう', '失業'),
			_Utils_Tuple2('じつぎょうか', '実業家'),
			_Utils_Tuple2('じっくり', 'じっくり'),
			_Utils_Tuple2('しっけ', '湿気'),
			_Utils_Tuple2('じっけん', '実験'),
			_Utils_Tuple2('じつげん', '実現'),
			_Utils_Tuple2('じっけんしつ', '実験室'),
			_Utils_Tuple2('しつこい', 'しつこい'),
			_Utils_Tuple2('じっこう', '実行'),
			_Utils_Tuple2('じっさい', '実際'),
			_Utils_Tuple2('じつざい', '実在'),
			_Utils_Tuple2('じっし', '実施'),
			_Utils_Tuple2('じっしゅう', '実習'),
			_Utils_Tuple2('じっせき', '実績'),
			_Utils_Tuple2('しってん', '失点'),
			_Utils_Tuple2('しつど', '湿度'),
			_Utils_Tuple2('じっと', 'じっと'),
			_Utils_Tuple2('しつない', '室内'),
			_Utils_Tuple2('じつに', '実に'),
			_Utils_Tuple2('じつは', '実は'),
			_Utils_Tuple2('じっぱー', 'ジッパー'),
			_Utils_Tuple2('しっぱい', '失敗'),
			_Utils_Tuple2('じつぶつ', '実物'),
			_Utils_Tuple2('しっぽ', 'しっぽ'),
			_Utils_Tuple2('しつぼう', '失望'),
			_Utils_Tuple2('じつめい', '実名'),
			_Utils_Tuple2('じつよう', '実用'),
			_Utils_Tuple2('じつようか', '実用化'),
			_Utils_Tuple2('じつようてき', '実用的'),
			_Utils_Tuple2('しつりょう', '質量'),
			_Utils_Tuple2('じつりょく', '実力'),
			_Utils_Tuple2('しつれん', '失恋'),
			_Utils_Tuple2('じつわ', '実話'),
			_Utils_Tuple2('してい', '指定'),
			_Utils_Tuple2('してぃー', 'シティー'),
			_Utils_Tuple2('していせき', '指定席'),
			_Utils_Tuple2('してき', '指摘'),
			_Utils_Tuple2('してき', '私的'),
			_Utils_Tuple2('してつ', '私鉄'),
			_Utils_Tuple2('してん', '支店'),
			_Utils_Tuple2('してん', '視点'),
			_Utils_Tuple2('じてん', '自転'),
			_Utils_Tuple2('じてん', '辞典'),
			_Utils_Tuple2('じてん', '事典'),
			_Utils_Tuple2('じてん', '時点'),
			_Utils_Tuple2('じでん', '自伝'),
			_Utils_Tuple2('しどう', '指導'),
			_Utils_Tuple2('しどう', '始動'),
			_Utils_Tuple2('しどう', '私道'),
			_Utils_Tuple2('じどう', '自動'),
			_Utils_Tuple2('じどう', '児童'),
			_Utils_Tuple2('じどうし', '自動詞'),
			_Utils_Tuple2('じどうてき', '自動的'),
			_Utils_Tuple2('じどうはんばいき', '自動販売機'),
			_Utils_Tuple2('しな', '品'),
			_Utils_Tuple2('しな', '品'),
			_Utils_Tuple2('しない', '市内'),
			_Utils_Tuple2('しなかず', '品数'),
			_Utils_Tuple2('しなもの', '品物'),
			_Utils_Tuple2('しなもん', 'シナモン'),
			_Utils_Tuple2('しなりお', 'シナリオ'),
			_Utils_Tuple2('じなん', '次男'),
			_Utils_Tuple2('しにがみ', '死神'),
			_Utils_Tuple2('じにん', '辞任'),
			_Utils_Tuple2('しねま', 'シネマ'),
			_Utils_Tuple2('しば', '芝'),
			_Utils_Tuple2('しはい', '支配'),
			_Utils_Tuple2('しばい', '芝居'),
			_Utils_Tuple2('しはつ', '始発'),
			_Utils_Tuple2('じはつてき', '自発的'),
			_Utils_Tuple2('しばふ', '芝生'),
			_Utils_Tuple2('しはらい', '支払い'),
			_Utils_Tuple2('しはらう', '支払う'),
			_Utils_Tuple2('しばらく', 'しばらく'),
			_Utils_Tuple2('しばる', '縛る'),
			_Utils_Tuple2('しはん', '市販'),
			_Utils_Tuple2('じはんき', '自販機'),
			_Utils_Tuple2('じひ', '自費'),
			_Utils_Tuple2('しびあ', 'シビア'),
			_Utils_Tuple2('じびか', '耳鼻科'),
			_Utils_Tuple2('じひょう', '辞表'),
			_Utils_Tuple2('じびょう', '持病'),
			_Utils_Tuple2('しびれる', 'しびれる'),
			_Utils_Tuple2('しぶ', '支部'),
			_Utils_Tuple2('しふく', '私服'),
			_Utils_Tuple2('しぶつ', '私物'),
			_Utils_Tuple2('しふと', 'シフト'),
			_Utils_Tuple2('じぶんかって', '自分勝手'),
			_Utils_Tuple2('じぶんじしん', '自分自身'),
			_Utils_Tuple2('しべつ', '死別'),
			_Utils_Tuple2('しほう', '四方'),
			_Utils_Tuple2('しぼう', '死亡'),
			_Utils_Tuple2('しぼう', '志望'),
			_Utils_Tuple2('しぼう', '脂肪'),
			_Utils_Tuple2('しぼうりつ', '死亡率'),
			_Utils_Tuple2('しぼる', '絞る'),
			_Utils_Tuple2('しほん', '資本'),
			_Utils_Tuple2('しほんか', '資本家'),
			_Utils_Tuple2('しほんしゅぎ', '資本主義'),
			_Utils_Tuple2('しま', '島'),
			_Utils_Tuple2('しま', 'しま'),
			_Utils_Tuple2('しまい', '姉妹'),
			_Utils_Tuple2('しまう', '仕舞う'),
			_Utils_Tuple2('しまうま', 'シマウマ'),
			_Utils_Tuple2('じまく', '字幕'),
			_Utils_Tuple2('しまぐに', '島国'),
			_Utils_Tuple2('しまじま', '島々'),
			_Utils_Tuple2('しまつ', '始末'),
			_Utils_Tuple2('しまる', '締まる'),
			_Utils_Tuple2('じまん', '自慢'),
			_Utils_Tuple2('しみ', '染み'),
			_Utils_Tuple2('じみ', '地味'),
			_Utils_Tuple2('しみる', '染みる'),
			_Utils_Tuple2('しみん', '市民'),
			_Utils_Tuple2('しみんけん', '市民権'),
			_Utils_Tuple2('じむ', '事務'),
			_Utils_Tuple2('じむ', 'ジム'),
			_Utils_Tuple2('じむいん', '事務員'),
			_Utils_Tuple2('じむきょく', '事務局'),
			_Utils_Tuple2('じむしょ', '事務所'),
			_Utils_Tuple2('じむてき', '事務的'),
			_Utils_Tuple2('しめい', '氏名'),
			_Utils_Tuple2('しめい', '使命'),
			_Utils_Tuple2('しめい', '指名'),
			_Utils_Tuple2('しめきり', '締め切り'),
			_Utils_Tuple2('しめきる', '締め切る'),
			_Utils_Tuple2('じめじめ', 'じめじめ'),
			_Utils_Tuple2('しめす', '示す'),
			_Utils_Tuple2('しめる', '締める'),
			_Utils_Tuple2('しめる', '占める'),
			_Utils_Tuple2('しめる', '絞める'),
			_Utils_Tuple2('しめる', '湿る'),
			_Utils_Tuple2('しめん', '紙面'),
			_Utils_Tuple2('じめん', '地面'),
			_Utils_Tuple2('しもん', '指紋'),
			_Utils_Tuple2('しゃ', '者'),
			_Utils_Tuple2('しゃ', '社'),
			_Utils_Tuple2('しや', '視野'),
			_Utils_Tuple2('じゃーなりすと', 'ジャーナリスト'),
			_Utils_Tuple2('じゃーなりずむ', 'ジャーナリズム'),
			_Utils_Tuple2('しゃーぷ', 'シャープ'),
			_Utils_Tuple2('しゃーべっと', 'シャーベット'),
			_Utils_Tuple2('しゃい', 'シャイ'),
			_Utils_Tuple2('じゃいあんと', 'ジャイアント'),
			_Utils_Tuple2('しゃがい', '社外'),
			_Utils_Tuple2('しゃかいがく', '社会学'),
			_Utils_Tuple2('しゃかいきょういく', '社会教育'),
			_Utils_Tuple2('しゃかいげんしょう', '社会現象'),
			_Utils_Tuple2('しゃかいしゅぎ', '社会主義'),
			_Utils_Tuple2('しゃかいじん', '社会人'),
			_Utils_Tuple2('しゃかいせい', '社会性'),
			_Utils_Tuple2('しゃかいせいかつ', '社会生活'),
			_Utils_Tuple2('しゃかいてき', '社会的'),
			_Utils_Tuple2('しゃかいふくし', '社会福祉'),
			_Utils_Tuple2('しゃかいめん', '社会面'),
			_Utils_Tuple2('しゃかいもんだい', '社会問題'),
			_Utils_Tuple2('しゃがむ', 'しゃがむ'),
			_Utils_Tuple2('じゃく', '弱'),
			_Utils_Tuple2('じゃく', '弱'),
			_Utils_Tuple2('じゃくしゃ', '弱者'),
			_Utils_Tuple2('しやくしょ', '市役所'),
			_Utils_Tuple2('じゃくしょう', '弱小'),
			_Utils_Tuple2('じゃくてん', '弱点'),
			_Utils_Tuple2('じゃけっと', 'ジャケット'),
			_Utils_Tuple2('しゃこ', '車庫'),
			_Utils_Tuple2('しゃこうてき', '社交的'),
			_Utils_Tuple2('しゃざい', '謝罪'),
			_Utils_Tuple2('しゃしょう', '車掌'),
			_Utils_Tuple2('しゃしんか', '写真家'),
			_Utils_Tuple2('しゃしんき', '写真機'),
			_Utils_Tuple2('しゃしんや', '写真屋'),
			_Utils_Tuple2('じゃすと', 'ジャスト'),
			_Utils_Tuple2('じゃすみん', 'ジャスミン'),
			_Utils_Tuple2('しゃせつ', '社説'),
			_Utils_Tuple2('しゃせん', '斜線'),
			_Utils_Tuple2('しゃせん', '車線'),
			_Utils_Tuple2('しゃたく', '社宅'),
			_Utils_Tuple2('しゃちゅう', '車中'),
			_Utils_Tuple2('しゃっきん', '借金'),
			_Utils_Tuple2('しゃっくり', 'しゃっくり'),
			_Utils_Tuple2('じゃっじ', 'ジャッジ'),
			_Utils_Tuple2('しゃったー', 'シャッター'),
			_Utils_Tuple2('しゃどう', '車道'),
			_Utils_Tuple2('しゃない', '車内'),
			_Utils_Tuple2('しゃない', '社内'),
			_Utils_Tuple2('しゃないほう', '社内報'),
			_Utils_Tuple2('じゃぱにーず', 'ジャパニーズ'),
			_Utils_Tuple2('じゃぱん', 'ジャパン'),
			_Utils_Tuple2('しゃふう', '社風'),
			_Utils_Tuple2('しゃぶしゃぶ', 'しゃぶしゃぶ'),
			_Utils_Tuple2('しゃべる', 'しゃべる'),
			_Utils_Tuple2('しゃべる', 'シャベル'),
			_Utils_Tuple2('じゃま', '邪魔'),
			_Utils_Tuple2('しゃめい', '社名'),
			_Utils_Tuple2('しゃめん', '斜面'),
			_Utils_Tuple2('しゃりょう', '車両'),
			_Utils_Tuple2('しゃりん', '車輪'),
			_Utils_Tuple2('しゃれ', 'しゃれ'),
			_Utils_Tuple2('じゃんぐる', 'ジャングル'),
			_Utils_Tuple2('じゃんぱー', 'ジャンパー'),
			_Utils_Tuple2('しゃんぱん', 'シャンパン'),
			_Utils_Tuple2('じゃんぷ', 'ジャンプ'),
			_Utils_Tuple2('じゃんぼ', 'ジャンボ'),
			_Utils_Tuple2('しゅ', '手'),
			_Utils_Tuple2('しゅ', '種'),
			_Utils_Tuple2('しゅ', '種'),
			_Utils_Tuple2('しゅい', '首位'),
			_Utils_Tuple2('しゅう', '周'),
			_Utils_Tuple2('しゅう', '州'),
			_Utils_Tuple2('しゅう', '集'),
			_Utils_Tuple2('しゆう', '私有'),
			_Utils_Tuple2('じゅう', '中'),
			_Utils_Tuple2('じゅう', '銃'),
			_Utils_Tuple2('じゅう', '住'),
			_Utils_Tuple2('じゆう', '自由'),
			_Utils_Tuple2('しゅうい', '周囲'),
			_Utils_Tuple2('しゅうえき', '収益'),
			_Utils_Tuple2('じゆうか', '自由化'),
			_Utils_Tuple2('しゅうかい', '集会'),
			_Utils_Tuple2('しゅうかく', '収穫'),
			_Utils_Tuple2('しゅうがく', '就学'),
			_Utils_Tuple2('しゅうがくりつ', '就学率'),
			_Utils_Tuple2('しゅうがくりょこう', '修学旅行'),
			_Utils_Tuple2('しゅうかん', '習慣'),
			_Utils_Tuple2('しゅうかん', '週刊'),
			_Utils_Tuple2('しゅうかんし', '週刊誌'),
			_Utils_Tuple2('しゅうき', '周期'),
			_Utils_Tuple2('しゅうきゅう', '週休'),
			_Utils_Tuple2('じゅうきょ', '住居'),
			_Utils_Tuple2('しゅうきょう', '宗教'),
			_Utils_Tuple2('しゅうぎょう', '就業'),
			_Utils_Tuple2('しゅうぎょう', '終業'),
			_Utils_Tuple2('じゆうぎょう', '自由業'),
			_Utils_Tuple2('じゅうぎょういん', '従業員'),
			_Utils_Tuple2('しゅうぎょうしき', '終業式'),
			_Utils_Tuple2('じゆうきょうそう', '自由競争'),
			_Utils_Tuple2('しゅうきん', '集金'),
			_Utils_Tuple2('しゅーくりーむ', 'シュークリーム'),
			_Utils_Tuple2('しゅうけい', '集計'),
			_Utils_Tuple2('しゅうごう', '集合'),
			_Utils_Tuple2('しゅうごうじゅうたく', '集合住宅'),
			_Utils_Tuple2('じゆうこうどう', '自由行動'),
			_Utils_Tuple2('じゅーさー', 'ジューサー'),
			_Utils_Tuple2('しゅうし', '修士'),
			_Utils_Tuple2('しゅうし', '収支'),
			_Utils_Tuple2('しゅうし', '終始'),
			_Utils_Tuple2('しゅうし', '終始'),
			_Utils_Tuple2('しゅうじ', '習字'),
			_Utils_Tuple2('じゅうし', '重視'),
			_Utils_Tuple2('じゅうじ', '十字'),
			_Utils_Tuple2('じゅうじか', '十字架'),
			_Utils_Tuple2('しゅうしかてい', '修士課程'),
			_Utils_Tuple2('じゆうじざい', '自由自在'),
			_Utils_Tuple2('しゅうじつ', '終日'),
			_Utils_Tuple2('じゅうじつ', '充実'),
			_Utils_Tuple2('しゅうしゅう', '収集'),
			_Utils_Tuple2('じゆうしゅぎ', '自由主義'),
			_Utils_Tuple2('じゅうしょう', '重症'),
			_Utils_Tuple2('じゅうしょう', '重傷'),
			_Utils_Tuple2('しゅうしょく', '就職'),
			_Utils_Tuple2('しゅーず', 'シューズ'),
			_Utils_Tuple2('しゅうせい', '修正'),
			_Utils_Tuple2('しゅうせき', '集積'),
			_Utils_Tuple2('しゅうせん', '終戦'),
			_Utils_Tuple2('じゅうたい', '渋滞'),
			_Utils_Tuple2('じゅうだい', '十代'),
			_Utils_Tuple2('じゅうだい', '重大'),
			_Utils_Tuple2('じゅうだい', '重大'),
			_Utils_Tuple2('じゅうたく', '住宅'),
			_Utils_Tuple2('しゅうだん', '集団'),
			_Utils_Tuple2('じゅうたん', 'じゅうたん'),
			_Utils_Tuple2('じゅうだん', '縦断'),
			_Utils_Tuple2('しゅうちゅう', '集中'),
			_Utils_Tuple2('しゅうちゅうごうう', '集中豪雨'),
			_Utils_Tuple2('しゅうてん', '終点'),
			_Utils_Tuple2('しゅうでん', '終電'),
			_Utils_Tuple2('じゅうてん', '重点'),
			_Utils_Tuple2('じゅうでん', '充電'),
			_Utils_Tuple2('じゅうてんてき', '重点的'),
			_Utils_Tuple2('しゅうとく', '習得'),
			_Utils_Tuple2('しゅうとく', '修得'),
			_Utils_Tuple2('じゅうなん', '柔軟'),
			_Utils_Tuple2('しゅうにゅう', '収入'),
			_Utils_Tuple2('しゅうにん', '就任'),
			_Utils_Tuple2('じゅうにん', '住人'),
			_Utils_Tuple2('しゅうのう', '収納'),
			_Utils_Tuple2('しゅうふく', '修復'),
			_Utils_Tuple2('じゅうふん', '十分'),
			_Utils_Tuple2('じゅうぶん', '十分'),
			_Utils_Tuple2('じゅうぶん', '十分'),
			_Utils_Tuple2('しゅうへん', '周辺'),
			_Utils_Tuple2('じゅうみん', '住民'),
			_Utils_Tuple2('じゅうみんひょう', '住民票'),
			_Utils_Tuple2('じゅうやく', '重役'),
			_Utils_Tuple2('じゅうよう', '重要'),
			_Utils_Tuple2('じゅうよう', '重要'),
			_Utils_Tuple2('じゅうようし', '重要視'),
			_Utils_Tuple2('じゅうようせい', '重要性'),
			_Utils_Tuple2('しゅうり', '修理'),
			_Utils_Tuple2('しゅうりょう', '修了'),
			_Utils_Tuple2('しゅうりょう', '終了'),
			_Utils_Tuple2('じゅうりょう', '重量'),
			_Utils_Tuple2('じゅうりょく', '重力'),
			_Utils_Tuple2('しゅうろう', '就労'),
			_Utils_Tuple2('じゅうろうどう', '重労働'),
			_Utils_Tuple2('しゅうろく', '収録'),
			_Utils_Tuple2('じゅえりー', 'ジュエリー'),
			_Utils_Tuple2('しゅえん', '主演'),
			_Utils_Tuple2('しゅがー', 'シュガー'),
			_Utils_Tuple2('しゅかん', '主観'),
			_Utils_Tuple2('しゅかんてき', '主観的'),
			_Utils_Tuple2('しゅぎ', '主義'),
			_Utils_Tuple2('じゅきゅう', '受給'),
			_Utils_Tuple2('じゅぎょうりょう', '授業料'),
			_Utils_Tuple2('しゅく', '祝'),
			_Utils_Tuple2('じゅく', '塾'),
			_Utils_Tuple2('じゅくご', '熟語'),
			_Utils_Tuple2('しゅくさいじつ', '祝祭日'),
			_Utils_Tuple2('しゅくじつ', '祝日'),
			_Utils_Tuple2('しゅくしゃ', '宿舎'),
			_Utils_Tuple2('しゅくしょう', '縮小'),
			_Utils_Tuple2('しゅくはく', '宿泊'),
			_Utils_Tuple2('しゅくふく', '祝福'),
			_Utils_Tuple2('しゅげい', '手芸'),
			_Utils_Tuple2('じゅけん', '受験'),
			_Utils_Tuple2('じゅけんせい', '受験生'),
			_Utils_Tuple2('しゅご', '主語'),
			_Utils_Tuple2('じゅこう', '受講'),
			_Utils_Tuple2('しゅさい', '主催'),
			_Utils_Tuple2('しゅざい', '取材'),
			_Utils_Tuple2('しゅじゅつ', '手術'),
			_Utils_Tuple2('しゅしょう', '首相'),
			_Utils_Tuple2('じゅしょう', '受賞'),
			_Utils_Tuple2('しゅしょく', '主食'),
			_Utils_Tuple2('じゅしん', '受診'),
			_Utils_Tuple2('じゅしん', '受信'),
			_Utils_Tuple2('しゅじんこう', '主人公'),
			_Utils_Tuple2('しゅぜい', '酒税'),
			_Utils_Tuple2('しゅだい', '主題'),
			_Utils_Tuple2('しゅだん', '手段'),
			_Utils_Tuple2('じゅちゅう', '受注'),
			_Utils_Tuple2('しゅちょう', '主張'),
			_Utils_Tuple2('しゅつえん', '出演'),
			_Utils_Tuple2('しゅっか', '出火'),
			_Utils_Tuple2('しゅつがん', '出願'),
			_Utils_Tuple2('しゅっきん', '出勤'),
			_Utils_Tuple2('しゅっけつ', '出血'),
			_Utils_Tuple2('しゅっけつ', '出欠'),
			_Utils_Tuple2('しゅつげん', '出現'),
			_Utils_Tuple2('じゅつご', '述語'),
			_Utils_Tuple2('しゅっこく', '出国'),
			_Utils_Tuple2('しゅっさん', '出産'),
			_Utils_Tuple2('しゅっしゃ', '出社'),
			_Utils_Tuple2('しゅつじょう', '出場'),
			_Utils_Tuple2('しゅっしん', '出身'),
			_Utils_Tuple2('しゅっせ', '出世'),
			_Utils_Tuple2('しゅっせき', '出席'),
			_Utils_Tuple2('しゅつだい', '出題'),
			_Utils_Tuple2('しゅっちょう', '出張'),
			_Utils_Tuple2('しゅつにゅうこく', '出入国'),
			_Utils_Tuple2('しゅっぱつ', '出発'),
			_Utils_Tuple2('しゅっぱつてん', '出発点'),
			_Utils_Tuple2('しゅっぱん', '出版'),
			_Utils_Tuple2('しゅつりょく', '出力'),
			_Utils_Tuple2('しゅと', '首都'),
			_Utils_Tuple2('しゅどう', '手動'),
			_Utils_Tuple2('しゅとく', '取得'),
			_Utils_Tuple2('しゅとけん', '首都圏'),
			_Utils_Tuple2('じゅにあ', 'ジュニア'),
			_Utils_Tuple2('しゅにん', '主任'),
			_Utils_Tuple2('しゅのう', '首脳'),
			_Utils_Tuple2('しゅのうかいだん', '首脳会談'),
			_Utils_Tuple2('しゅふ', '主婦'),
			_Utils_Tuple2('じゅみょう', '寿命'),
			_Utils_Tuple2('しゅもく', '種目'),
			_Utils_Tuple2('しゅやく', '主役'),
			_Utils_Tuple2('しゅよう', '主要'),
			_Utils_Tuple2('しゅよう', '主要'),
			_Utils_Tuple2('じゅよう', '需要'),
			_Utils_Tuple2('しゅりゅう', '主流'),
			_Utils_Tuple2('しゅりょく', '主力'),
			_Utils_Tuple2('しゅるい', '種類'),
			_Utils_Tuple2('しゅるい', '酒類'),
			_Utils_Tuple2('しゅれっだー', 'シュレッダー'),
			_Utils_Tuple2('しゅわ', '手話'),
			_Utils_Tuple2('じゅわき', '受話器'),
			_Utils_Tuple2('じゅん', '順'),
			_Utils_Tuple2('じゅんい', '順位'),
			_Utils_Tuple2('しゅんかん', '瞬間'),
			_Utils_Tuple2('じゅんかん', '循環'),
			_Utils_Tuple2('じゅんきゅう', '準急'),
			_Utils_Tuple2('じゅんけっしょう', '準決勝'),
			_Utils_Tuple2('じゅんじゅんに', '順々に'),
			_Utils_Tuple2('じゅんじょ', '順序'),
			_Utils_Tuple2('じゅんすい', '純粋'),
			_Utils_Tuple2('じゅんちょう', '順調'),
			_Utils_Tuple2('じゅんちょう', '順調'),
			_Utils_Tuple2('じゅんに', '順に'),
			_Utils_Tuple2('じゅんばん', '順番'),
			_Utils_Tuple2('じゅんび', '準備'),
			_Utils_Tuple2('しょ', '所'),
			_Utils_Tuple2('しょ', '書'),
			_Utils_Tuple2('しょ', '署'),
			_Utils_Tuple2('じょい', '女医'),
			_Utils_Tuple2('しょう', '小'),
			_Utils_Tuple2('しょう', '少'),
			_Utils_Tuple2('しょう', '小'),
			_Utils_Tuple2('しょう', '小'),
			_Utils_Tuple2('しょう', '省'),
			_Utils_Tuple2('しょう', '賞'),
			_Utils_Tuple2('しょう', '証'),
			_Utils_Tuple2('しょう', '症'),
			_Utils_Tuple2('しょう', '章'),
			_Utils_Tuple2('しょう', '傷'),
			_Utils_Tuple2('しよう', '使用'),
			_Utils_Tuple2('しよう', '私用'),
			_Utils_Tuple2('しよう', '試用'),
			_Utils_Tuple2('じょう', '場'),
			_Utils_Tuple2('じょう', '上'),
			_Utils_Tuple2('じょう', '上'),
			_Utils_Tuple2('じょう', '畳'),
			_Utils_Tuple2('じょう', '城'),
			_Utils_Tuple2('じょう', '状'),
			_Utils_Tuple2('じょう', '情'),
			_Utils_Tuple2('じょうい', '上位'),
			_Utils_Tuple2('じょういん', '乗員'),
			_Utils_Tuple2('じょうえい', '上映'),
			_Utils_Tuple2('しょうえね', '省エネ'),
			_Utils_Tuple2('しょうえねるぎー', '省エネルギー'),
			_Utils_Tuple2('じょうえん', '上演'),
			_Utils_Tuple2('しょうおん', '消音'),
			_Utils_Tuple2('じょうおん', '常温'),
			_Utils_Tuple2('しょうか', '消化'),
			_Utils_Tuple2('しょうか', '消火'),
			_Utils_Tuple2('しょうが', 'しょうが'),
			_Utils_Tuple2('じょうか', '上下'),
			_Utils_Tuple2('しょうかい', '商会'),
			_Utils_Tuple2('しょうがい', '障害'),
			_Utils_Tuple2('しょうがい', '傷害'),
			_Utils_Tuple2('しょうがいしゃ', '障害者'),
			_Utils_Tuple2('しょうかき', '消火器'),
			_Utils_Tuple2('しょうがく', '少額'),
			_Utils_Tuple2('しょうがくきん', '奨学金'),
			_Utils_Tuple2('しょうがくせい', '小学生'),
			_Utils_Tuple2('しょうがっこう', '小学校'),
			_Utils_Tuple2('しようがない', 'しようがない'),
			_Utils_Tuple2('しょうかふりょう', '消化不良'),
			_Utils_Tuple2('しょうぎ', '将棋'),
			_Utils_Tuple2('じょうき', '蒸気'),
			_Utils_Tuple2('じょうき', '上記'),
			_Utils_Tuple2('じょうぎ', '定規'),
			_Utils_Tuple2('じょうきゃく', '乗客'),
			_Utils_Tuple2('じょうきゅう', '上級'),
			_Utils_Tuple2('しょうきょ', '消去'),
			_Utils_Tuple2('しょうぎょう', '商業'),
			_Utils_Tuple2('じょうきょう', '状況'),
			_Utils_Tuple2('じょうきょう', '上京'),
			_Utils_Tuple2('しょうきょく', '消極'),
			_Utils_Tuple2('しょうきょくてき', '消極的'),
			_Utils_Tuple2('しょうきん', '賞金'),
			_Utils_Tuple2('じょうきん', '常勤'),
			_Utils_Tuple2('じょうくう', '上空'),
			_Utils_Tuple2('じょうげ', '上下'),
			_Utils_Tuple2('じょうけい', '情景'),
			_Utils_Tuple2('しょうげん', '証言'),
			_Utils_Tuple2('じょうけん', '条件'),
			_Utils_Tuple2('じょうげん', '上限'),
			_Utils_Tuple2('しょうこ', '証拠'),
			_Utils_Tuple2('しょうご', '正午'),
			_Utils_Tuple2('しょうさい', '詳細'),
			_Utils_Tuple2('しょうじ', '障子'),
			_Utils_Tuple2('じょうし', '上司'),
			_Utils_Tuple2('じょうじ', '常時'),
			_Utils_Tuple2('しょうしか', '少子化'),
			_Utils_Tuple2('しょうじき', '正直'),
			_Utils_Tuple2('しょうじき', '正直'),
			_Utils_Tuple2('じょうしき', '常識'),
			_Utils_Tuple2('じょうしきてき', '常識的'),
			_Utils_Tuple2('しょうしつ', '消失'),
			_Utils_Tuple2('じょうしつ', '上質'),
			_Utils_Tuple2('しょうしゃ', '商社'),
			_Utils_Tuple2('しょうしゃ', '勝者'),
			_Utils_Tuple2('しようしゃ', '使用者'),
			_Utils_Tuple2('じょうしゃ', '乗車'),
			_Utils_Tuple2('じょうしゃけん', '乗車券'),
			_Utils_Tuple2('しょうしゅう', '消臭'),
			_Utils_Tuple2('じょうじゅん', '上旬'),
			_Utils_Tuple2('しょうしょ', '証書'),
			_Utils_Tuple2('しょうじょ', '少女'),
			_Utils_Tuple2('しょうしょう', '少々'),
			_Utils_Tuple2('しょうじょう', '賞状'),
			_Utils_Tuple2('しょうじょう', '症状'),
			_Utils_Tuple2('じょうしょう', '上昇'),
			_Utils_Tuple2('しょうすう', '少数'),
			_Utils_Tuple2('しょうすう', '小数'),
			_Utils_Tuple2('しょうすうてん', '小数点'),
			_Utils_Tuple2('しょうすうみんぞく', '少数民族'),
			_Utils_Tuple2('しょうせつ', '小説'),
			_Utils_Tuple2('しょうせつか', '小説家'),
			_Utils_Tuple2('じょうせん', '乗船'),
			_Utils_Tuple2('しょうたい', '招待'),
			_Utils_Tuple2('しょうたい', '正体'),
			_Utils_Tuple2('じょうたい', '状態'),
			_Utils_Tuple2('しょうたいじょう', '招待状'),
			_Utils_Tuple2('じょうたつ', '上達'),
			_Utils_Tuple2('しょうだん', '商談'),
			_Utils_Tuple2('じょうだん', '冗談'),
			_Utils_Tuple2('じょうだん', '上段'),
			_Utils_Tuple2('しょうち', '承知'),
			_Utils_Tuple2('しょうちゅう', '焼ちゅう'),
			_Utils_Tuple2('しょうちょう', '象徴'),
			_Utils_Tuple2('しょうてん', '商店'),
			_Utils_Tuple2('しょうてん', '焦点'),
			_Utils_Tuple2('しょうてんがい', '商店街'),
			_Utils_Tuple2('じょうとう', '上等'),
			_Utils_Tuple2('しょうどく', '消毒'),
			_Utils_Tuple2('しょうとつ', '衝突'),
			_Utils_Tuple2('しょうに', '小児'),
			_Utils_Tuple2('しょうにか', '小児科'),
			_Utils_Tuple2('しょうにん', '商人'),
			_Utils_Tuple2('しょうにん', '承認'),
			_Utils_Tuple2('しょうにん', '証人'),
			_Utils_Tuple2('しょうにんずう', '少人数'),
			_Utils_Tuple2('じょうねつ', '情熱'),
			_Utils_Tuple2('じょうば', '乗馬'),
			_Utils_Tuple2('しょうはい', '勝敗'),
			_Utils_Tuple2('しょうばい', '商売'),
			_Utils_Tuple2('じょうはんしん', '上半身'),
			_Utils_Tuple2('しょうひ', '消費'),
			_Utils_Tuple2('しょうひきげん', '消費期限'),
			_Utils_Tuple2('しょうひしゃ', '消費者'),
			_Utils_Tuple2('しょうひぜい', '消費税'),
			_Utils_Tuple2('しょうひん', '商品'),
			_Utils_Tuple2('しょうひん', '賞品'),
			_Utils_Tuple2('じょうひん', '上品'),
			_Utils_Tuple2('しょうひんけん', '商品券'),
			_Utils_Tuple2('しょうぶ', '勝負'),
			_Utils_Tuple2('じょうぶ', '丈夫'),
			_Utils_Tuple2('じょうぶ', '上部'),
			_Utils_Tuple2('しょうべん', '小便'),
			_Utils_Tuple2('しょうぼう', '消防'),
			_Utils_Tuple2('じょうほう', '情報'),
			_Utils_Tuple2('じょうほうかしゃかい', '情報化社会'),
			_Utils_Tuple2('じょうほうけんさく', '情報検索'),
			_Utils_Tuple2('しょうぼうし', '消防士'),
			_Utils_Tuple2('じょうほうしょり', '情報処理'),
			_Utils_Tuple2('しょうぼうだん', '消防団'),
			_Utils_Tuple2('しょうみきげん', '賞味期限'),
			_Utils_Tuple2('しょうめい', '証明'),
			_Utils_Tuple2('しょうめい', '照明'),
			_Utils_Tuple2('しょうめいしょ', '証明書'),
			_Utils_Tuple2('しょうめん', '正面'),
			_Utils_Tuple2('じょうよう', '乗用'),
			_Utils_Tuple2('じょうようしゃ', '乗用車'),
			_Utils_Tuple2('しょうらい', '将来'),
			_Utils_Tuple2('しょうらいせい', '将来性'),
			_Utils_Tuple2('しょうり', '勝利'),
			_Utils_Tuple2('しょうりゃく', '省略'),
			_Utils_Tuple2('じょうりゅう', '上流'),
			_Utils_Tuple2('しょうりょう', '少量'),
			_Utils_Tuple2('しようりょう', '使用料'),
			_Utils_Tuple2('しようりょう', '使用量'),
			_Utils_Tuple2('しょうろんぶん', '小論文'),
			_Utils_Tuple2('しょうわ', '昭和'),
			_Utils_Tuple2('しょー', 'ショー'),
			_Utils_Tuple2('じょおう', '女王'),
			_Utils_Tuple2('じょーく', 'ジョーク'),
			_Utils_Tuple2('しょーけーす', 'ショーケース'),
			_Utils_Tuple2('しょーつ', 'ショーツ'),
			_Utils_Tuple2('しょーと', 'ショート'),
			_Utils_Tuple2('しょーとかっと', 'ショートカット'),
			_Utils_Tuple2('しょーる', 'ショール'),
			_Utils_Tuple2('しょーるーむ', 'ショールーム'),
			_Utils_Tuple2('しょか', '初夏'),
			_Utils_Tuple2('しょかい', '初回'),
			_Utils_Tuple2('じょがい', '除外'),
			_Utils_Tuple2('しょき', '初期'),
			_Utils_Tuple2('じょきょうじゅ', '助教授'),
			_Utils_Tuple2('じょぎんぐ', 'ジョギング'),
			_Utils_Tuple2('しょく', '食'),
			_Utils_Tuple2('しょく', '色'),
			_Utils_Tuple2('しょく', '色'),
			_Utils_Tuple2('しょく', '食'),
			_Utils_Tuple2('しょく', '職'),
			_Utils_Tuple2('しょくいん', '職員'),
			_Utils_Tuple2('しょくえん', '食塩'),
			_Utils_Tuple2('しょくぎょう', '職業'),
			_Utils_Tuple2('しょくご', '食後'),
			_Utils_Tuple2('しょくざい', '食材'),
			_Utils_Tuple2('しょくしゅ', '職種'),
			_Utils_Tuple2('しょくせいかつ', '食生活'),
			_Utils_Tuple2('しょくぜん', '食前'),
			_Utils_Tuple2('しょくたく', '食卓'),
			_Utils_Tuple2('しょくちゅうどく', '食中毒'),
			_Utils_Tuple2('しょくどう', '食道'),
			_Utils_Tuple2('しょくにん', '職人'),
			_Utils_Tuple2('しょくば', '職場'),
			_Utils_Tuple2('しょくひ', '食費'),
			_Utils_Tuple2('しょくひん', '食品'),
			_Utils_Tuple2('しょくぶつ', '植物'),
			_Utils_Tuple2('しょくぶつ', '食物'),
			_Utils_Tuple2('しょくぶつえん', '植物園'),
			_Utils_Tuple2('しょくぶつせい', '植物性'),
			_Utils_Tuple2('しょくもつ', '食物'),
			_Utils_Tuple2('しょくよう', '食用'),
			_Utils_Tuple2('しょくよく', '食欲'),
			_Utils_Tuple2('しょくりょう', '食糧'),
			_Utils_Tuple2('しょくりょう', '食料'),
			_Utils_Tuple2('しょくりょうひん', '食料品'),
			_Utils_Tuple2('しょくれき', '職歴'),
			_Utils_Tuple2('しょこら', 'ショコラ'),
			_Utils_Tuple2('しょじ', '所持'),
			_Utils_Tuple2('じょし', '助詞'),
			_Utils_Tuple2('じょじ', '女児'),
			_Utils_Tuple2('じょしがくせい', '女子学生'),
			_Utils_Tuple2('じょしだいがく', '女子大学'),
			_Utils_Tuple2('じょしゅ', '助手'),
			_Utils_Tuple2('じょしゅせき', '助手席'),
			_Utils_Tuple2('しょじゅん', '初旬'),
			_Utils_Tuple2('じょじょに', '徐々に'),
			_Utils_Tuple2('しょしん', '初診'),
			_Utils_Tuple2('しょしん', '初心'),
			_Utils_Tuple2('しょしんしゃ', '初心者'),
			_Utils_Tuple2('じょせいてき', '女性的'),
			_Utils_Tuple2('しょせん', '初戦'),
			_Utils_Tuple2('しょぞく', '所属'),
			_Utils_Tuple2('しょたい', '書体'),
			_Utils_Tuple2('しょだい', '初代'),
			_Utils_Tuple2('しょたいめん', '初対面'),
			_Utils_Tuple2('しょちゅう', '暑中'),
			_Utils_Tuple2('しょちゅうみまい', '暑中見舞い'),
			_Utils_Tuple2('しょちょう', '所長'),
			_Utils_Tuple2('しょっき', '食器'),
			_Utils_Tuple2('じょっき', 'ジョッキ'),
			_Utils_Tuple2('しょっきんぐ', 'ショッキング'),
			_Utils_Tuple2('しょっく', 'ショック'),
			_Utils_Tuple2('しょっけん', '食券'),
			_Utils_Tuple2('しょっちゅう', 'しょっちゅう'),
			_Utils_Tuple2('しょっぱい', 'しょっぱい'),
			_Utils_Tuple2('しょっぴんぐせんたー', 'ショッピングセンター'),
			_Utils_Tuple2('しょっぷ', 'ショップ'),
			_Utils_Tuple2('しょてい', '所定'),
			_Utils_Tuple2('しょてん', '書店'),
			_Utils_Tuple2('しょどう', '書道'),
			_Utils_Tuple2('じょどうし', '助動詞'),
			_Utils_Tuple2('しょにち', '初日'),
			_Utils_Tuple2('しょほ', '初歩'),
			_Utils_Tuple2('しょみん', '庶民'),
			_Utils_Tuple2('しょめい', '署名'),
			_Utils_Tuple2('しょめい', '書名'),
			_Utils_Tuple2('しょめん', '書面'),
			_Utils_Tuple2('しょもつ', '書物'),
			_Utils_Tuple2('じょや', '除夜'),
			_Utils_Tuple2('じょやのかね', '除夜の鐘'),
			_Utils_Tuple2('しょゆう', '所有'),
			_Utils_Tuple2('じょゆう', '女優'),
			_Utils_Tuple2('しょゆうけん', '所有権'),
			_Utils_Tuple2('しょよう', '所要'),
			_Utils_Tuple2('しょり', '処理'),
			_Utils_Tuple2('しょるい', '書類'),
			_Utils_Tuple2('しょるだー', 'ショルダー'),
			_Utils_Tuple2('しょろう', '初老'),
			_Utils_Tuple2('しらす', '知らす'),
			_Utils_Tuple2('しらせ', '知らせ'),
			_Utils_Tuple2('しらべ', '調べ'),
			_Utils_Tuple2('しり', '尻'),
			_Utils_Tuple2('しりあい', '知り合い'),
			_Utils_Tuple2('しりあう', '知り合う'),
			_Utils_Tuple2('しりあす', 'シリアス'),
			_Utils_Tuple2('しりーず', 'シリーズ'),
			_Utils_Tuple2('じりき', '自力'),
			_Utils_Tuple2('しりつ', '私立'),
			_Utils_Tuple2('しりつ', '市立'),
			_Utils_Tuple2('じりつ', '自立'),
			_Utils_Tuple2('じりつ', '自律'),
			_Utils_Tuple2('しりとり', 'しりとり'),
			_Utils_Tuple2('しりょう', '資料'),
			_Utils_Tuple2('しりょう', '飼料'),
			_Utils_Tuple2('しりょく', '視力'),
			_Utils_Tuple2('しる', '汁'),
			_Utils_Tuple2('しるえっと', 'シルエット'),
			_Utils_Tuple2('しるく', 'シルク'),
			_Utils_Tuple2('しるし', '印'),
			_Utils_Tuple2('しるす', '記す'),
			_Utils_Tuple2('しるばー', 'シルバー'),
			_Utils_Tuple2('しれい', '指令'),
			_Utils_Tuple2('しれる', '知れる'),
			_Utils_Tuple2('しろ', '城'),
			_Utils_Tuple2('しろいめ', '白い目'),
			_Utils_Tuple2('しろうと', '素人'),
			_Utils_Tuple2('しろくろ', '白黒'),
			_Utils_Tuple2('しろじ', '白地'),
			_Utils_Tuple2('じろじろ', 'じろじろ'),
			_Utils_Tuple2('しろっぷ', 'シロップ'),
			_Utils_Tuple2('しろばい', '白バイ'),
			_Utils_Tuple2('しろみ', '白身'),
			_Utils_Tuple2('しわ', 'しわ'),
			_Utils_Tuple2('しわす', '師走'),
			_Utils_Tuple2('しん', '新'),
			_Utils_Tuple2('しん', '心'),
			_Utils_Tuple2('しん', '心'),
			_Utils_Tuple2('しん', '真'),
			_Utils_Tuple2('しん', '芯'),
			_Utils_Tuple2('しん', '神'),
			_Utils_Tuple2('しんあい', '親愛'),
			_Utils_Tuple2('しんいり', '新入り'),
			_Utils_Tuple2('じんいん', '人員'),
			_Utils_Tuple2('しんか', '進化'),
			_Utils_Tuple2('しんがー', 'シンガー'),
			_Utils_Tuple2('しんがく', '進学'),
			_Utils_Tuple2('じんかく', '人格'),
			_Utils_Tuple2('しんがた', '新型'),
			_Utils_Tuple2('しんかん', '新館'),
			_Utils_Tuple2('しんき', '新規'),
			_Utils_Tuple2('しんき', '新規'),
			_Utils_Tuple2('しんきゅう', '新旧'),
			_Utils_Tuple2('しんきゅう', '進級'),
			_Utils_Tuple2('しんきょ', '新居'),
			_Utils_Tuple2('しんきょく', '新曲'),
			_Utils_Tuple2('しんきんかん', '親近感'),
			_Utils_Tuple2('しんぐ', '寝具'),
			_Utils_Tuple2('しんけい', '神経'),
			_Utils_Tuple2('しんけいしつ', '神経質'),
			_Utils_Tuple2('しんけん', '真剣'),
			_Utils_Tuple2('しんげん', '震源'),
			_Utils_Tuple2('じんけん', '人権'),
			_Utils_Tuple2('しんげんち', '震源地'),
			_Utils_Tuple2('じんけんひ', '人件費'),
			_Utils_Tuple2('しんこう', '進行'),
			_Utils_Tuple2('しんこう', '親交'),
			_Utils_Tuple2('じんこう', '人工'),
			_Utils_Tuple2('じんこうえいせい', '人工衛星'),
			_Utils_Tuple2('しんこうけい', '進行形'),
			_Utils_Tuple2('じんこうこきゅう', '人工呼吸'),
			_Utils_Tuple2('じんこうてき', '人工的'),
			_Utils_Tuple2('じんこうみつど', '人口密度'),
			_Utils_Tuple2('しんこきゅう', '深呼吸'),
			_Utils_Tuple2('しんこく', '深刻'),
			_Utils_Tuple2('しんこく', '申告'),
			_Utils_Tuple2('しんこん', '新婚'),
			_Utils_Tuple2('しんさ', '審査'),
			_Utils_Tuple2('しんさい', '震災'),
			_Utils_Tuple2('じんざい', '人材'),
			_Utils_Tuple2('しんさく', '新作'),
			_Utils_Tuple2('しんさつ', '診察'),
			_Utils_Tuple2('しんし', '紳士'),
			_Utils_Tuple2('じんじ', '人事'),
			_Utils_Tuple2('しんしつ', '寝室'),
			_Utils_Tuple2('しんじつ', '真実'),
			_Utils_Tuple2('しんしゃ', '新車'),
			_Utils_Tuple2('しんじゃ', '信者'),
			_Utils_Tuple2('しんしゅ', '新種'),
			_Utils_Tuple2('しんじゅ', '真珠'),
			_Utils_Tuple2('じんしゅ', '人種'),
			_Utils_Tuple2('じんしゅさべつ', '人種差別'),
			_Utils_Tuple2('しんしゅつ', '進出'),
			_Utils_Tuple2('しんしょ', '新書'),
			_Utils_Tuple2('しんしょく', '新色'),
			_Utils_Tuple2('しんしん', '心身'),
			_Utils_Tuple2('しんじん', '新人'),
			_Utils_Tuple2('じんしん', '人身'),
			_Utils_Tuple2('じんしんじこ', '人身事故'),
			_Utils_Tuple2('しんずる', '信ずる'),
			_Utils_Tuple2('しんせい', '申請'),
			_Utils_Tuple2('じんせい', '人生'),
			_Utils_Tuple2('じんせいかん', '人生観'),
			_Utils_Tuple2('しんせいじ', '新生児'),
			_Utils_Tuple2('しんせつ', '新設'),
			_Utils_Tuple2('しんせん', '新鮮'),
			_Utils_Tuple2('しんせん', '新鮮'),
			_Utils_Tuple2('しんぞう', '心臓'),
			_Utils_Tuple2('じんぞう', '腎臓'),
			_Utils_Tuple2('しんぞく', '親族'),
			_Utils_Tuple2('しんそつ', '新卒'),
			_Utils_Tuple2('しんたい', '身体'),
			_Utils_Tuple2('しんだい', '寝台'),
			_Utils_Tuple2('じんたい', '人体'),
			_Utils_Tuple2('しんたいけんさ', '身体検査'),
			_Utils_Tuple2('しんたいしょうがいしゃ', '身体障害者'),
			_Utils_Tuple2('しんだん', '診断'),
			_Utils_Tuple2('しんだんしょ', '診断書'),
			_Utils_Tuple2('しんちく', '新築'),
			_Utils_Tuple2('しんちょう', '身長'),
			_Utils_Tuple2('しんちょう', '慎重'),
			_Utils_Tuple2('しんど', '震度'),
			_Utils_Tuple2('しんど', '進度'),
			_Utils_Tuple2('しんにゅう', '新入'),
			_Utils_Tuple2('しんにゅう', '進入'),
			_Utils_Tuple2('しんにゅうせい', '新入生'),
			_Utils_Tuple2('しんにん', '新任'),
			_Utils_Tuple2('しんねん', '新年'),
			_Utils_Tuple2('しんねん', '信念'),
			_Utils_Tuple2('しんぱん', '審判'),
			_Utils_Tuple2('しんぴん', '新品'),
			_Utils_Tuple2('しんぷ', '新婦'),
			_Utils_Tuple2('しんぷ', '神父'),
			_Utils_Tuple2('じんぶつ', '人物'),
			_Utils_Tuple2('しんぷる', 'シンプル'),
			_Utils_Tuple2('しんぶんきしゃ', '新聞記者'),
			_Utils_Tuple2('しんぽ', '進歩'),
			_Utils_Tuple2('しんぽじうむ', 'シンポジウム'),
			_Utils_Tuple2('しんぼる', 'シンボル'),
			_Utils_Tuple2('しんみつ', '親密'),
			_Utils_Tuple2('じんみん', '人民'),
			_Utils_Tuple2('じんめい', '人命'),
			_Utils_Tuple2('じんめい', '人名'),
			_Utils_Tuple2('しんや', '深夜'),
			_Utils_Tuple2('しんゆう', '親友'),
			_Utils_Tuple2('しんよう', '信用'),
			_Utils_Tuple2('しんらい', '信頼'),
			_Utils_Tuple2('しんり', '心理'),
			_Utils_Tuple2('しんりがく', '心理学'),
			_Utils_Tuple2('じんりき', '人力'),
			_Utils_Tuple2('じんりきしゃ', '人力車'),
			_Utils_Tuple2('しんりてき', '心理的'),
			_Utils_Tuple2('しんるい', '親類'),
			_Utils_Tuple2('じんるい', '人類'),
			_Utils_Tuple2('じんるいがく', '人類学'),
			_Utils_Tuple2('しんろ', '進路'),
			_Utils_Tuple2('しんろう', '新郎'),
			_Utils_Tuple2('しんろしどう', '進路指導'),
			_Utils_Tuple2('しんわ', '神話'),
			_Utils_Tuple2('す', '酢'),
			_Utils_Tuple2('ず', '図'),
			_Utils_Tuple2('すい', '水'),
			_Utils_Tuple2('すいおん', '水温'),
			_Utils_Tuple2('すいがい', '水害'),
			_Utils_Tuple2('すいこむ', '吸い込む'),
			_Utils_Tuple2('すいじ', '炊事'),
			_Utils_Tuple2('すいしつ', '水質'),
			_Utils_Tuple2('すいじゅん', '水準'),
			_Utils_Tuple2('すいじょう', '水上'),
			_Utils_Tuple2('すいじょうき', '水蒸気'),
			_Utils_Tuple2('すいせい', '水星'),
			_Utils_Tuple2('すいせい', '水性'),
			_Utils_Tuple2('すいせん', '推薦'),
			_Utils_Tuple2('すいせん', '水洗'),
			_Utils_Tuple2('すいぞくかん', '水族館'),
			_Utils_Tuple2('すいちゅう', '水中'),
			_Utils_Tuple2('すいちょく', '垂直'),
			_Utils_Tuple2('すいっち', 'スイッチ'),
			_Utils_Tuple2('すいてい', '推定'),
			_Utils_Tuple2('すいとう', '水筒'),
			_Utils_Tuple2('すいどう', '水道'),
			_Utils_Tuple2('すいはんき', '炊飯器'),
			_Utils_Tuple2('すいぶん', '水分'),
			_Utils_Tuple2('ずいぶん', '随分'),
			_Utils_Tuple2('すいへい', '水平'),
			_Utils_Tuple2('すいみん', '睡眠'),
			_Utils_Tuple2('すいめん', '水面'),
			_Utils_Tuple2('すいり', '推理'),
			_Utils_Tuple2('すいりしょうせつ', '推理小説'),
			_Utils_Tuple2('すいりゅう', '水流'),
			_Utils_Tuple2('すいりょう', '水量'),
			_Utils_Tuple2('すいりょく', '水力'),
			_Utils_Tuple2('すいりょくはつでん', '水力発電'),
			_Utils_Tuple2('すいろ', '水路'),
			_Utils_Tuple2('すう', '数'),
			_Utils_Tuple2('すう', '数'),
			_Utils_Tuple2('すうがく', '数学'),
			_Utils_Tuple2('すうこ', '数個'),
			_Utils_Tuple2('すうじ', '数字'),
			_Utils_Tuple2('すうしき', '数式'),
			_Utils_Tuple2('すうじつ', '数日'),
			_Utils_Tuple2('すうち', '数値'),
			_Utils_Tuple2('すうにん', '数人'),
			_Utils_Tuple2('すーぱー', 'スーパー'),
			_Utils_Tuple2('すーぱーまん', 'スーパーマン'),
			_Utils_Tuple2('ずーむ', 'ズーム'),
			_Utils_Tuple2('すうりょう', '数量'),
			_Utils_Tuple2('すえ', '末'),
			_Utils_Tuple2('すえっこ', '末っ子'),
			_Utils_Tuple2('すかーふ', 'スカーフ'),
			_Utils_Tuple2('すかい', 'スカイ'),
			_Utils_Tuple2('すかいだいびんぐ', 'スカイダイビング'),
			_Utils_Tuple2('すがた', '姿'),
			_Utils_Tuple2('すかっと', 'すかっと'),
			_Utils_Tuple2('ずかん', '図鑑'),
			_Utils_Tuple2('すき', 'すき'),
			_Utils_Tuple2('すぎ', '過ぎ'),
			_Utils_Tuple2('すぎ', '杉'),
			_Utils_Tuple2('すきーやー', 'スキーヤー'),
			_Utils_Tuple2('すききらい', '好き嫌い'),
			_Utils_Tuple2('すぎさる', '過ぎ去る'),
			_Utils_Tuple2('ずきずき', 'ずきずき'),
			_Utils_Tuple2('すきっぷ', 'スキップ'),
			_Utils_Tuple2('すきとおる', '透き通る'),
			_Utils_Tuple2('すきゃんだる', 'スキャンダル'),
			_Utils_Tuple2('すぎる', '過ぎる'),
			_Utils_Tuple2('すきん', 'スキン'),
			_Utils_Tuple2('すく', '好く'),
			_Utils_Tuple2('すくい', '救い'),
			_Utils_Tuple2('すくう', '救う'),
			_Utils_Tuple2('すくーたー', 'スクーター'),
			_Utils_Tuple2('すくーぷ', 'スクープ'),
			_Utils_Tuple2('すくーる', 'スクール'),
			_Utils_Tuple2('すくなめ', '少なめ'),
			_Utils_Tuple2('すくらっぷぶっく', 'スクラップブック'),
			_Utils_Tuple2('すくりーん', 'スクリーン'),
			_Utils_Tuple2('すぐれる', '優れる'),
			_Utils_Tuple2('ずけい', '図形'),
			_Utils_Tuple2('すけっち', 'スケッチ'),
			_Utils_Tuple2('すけっちぶっく', 'スケッチブック'),
			_Utils_Tuple2('すこあ', 'スコア'),
			_Utils_Tuple2('すごい', 'すごい'),
			_Utils_Tuple2('すこしも', '少しも'),
			_Utils_Tuple2('すごす', '過ごす'),
			_Utils_Tuple2('ずしき', '図式'),
			_Utils_Tuple2('ずじょう', '頭上'),
			_Utils_Tuple2('すず', '鈴'),
			_Utils_Tuple2('すずむ', '涼む'),
			_Utils_Tuple2('すすめ', '勧め'),
			_Utils_Tuple2('すずめ', 'すずめ'),
			_Utils_Tuple2('すすめる', '進める'),
			_Utils_Tuple2('すすめる', '勧める'),
			_Utils_Tuple2('すたー', 'スター'),
			_Utils_Tuple2('すたーと', 'スタート'),
			_Utils_Tuple2('すたーとらいん', 'スタートライン'),
			_Utils_Tuple2('すたいりすと', 'スタイリスト'),
			_Utils_Tuple2('すたいる', 'スタイル'),
			_Utils_Tuple2('すたじお', 'スタジオ'),
			_Utils_Tuple2('すたみな', 'スタミナ'),
			_Utils_Tuple2('すたんど', 'スタンド'),
			_Utils_Tuple2('すたんぷ', 'スタンプ'),
			_Utils_Tuple2('すちーむ', 'スチーム'),
			_Utils_Tuple2('すちーむあいろん', 'スチームアイロン'),
			_Utils_Tuple2('すちゅわーです', 'スチュワーデス'),
			_Utils_Tuple2('ずつう', '頭痛'),
			_Utils_Tuple2('すっかり', 'すっかり'),
			_Utils_Tuple2('すっきり', 'すっきり'),
			_Utils_Tuple2('すっぱい', '酸っぱい'),
			_Utils_Tuple2('すてぃっく', 'スティック'),
			_Utils_Tuple2('すてーじ', 'ステージ'),
			_Utils_Tuple2('すてーしょん', 'ステーション'),
			_Utils_Tuple2('すてき', '素敵'),
			_Utils_Tuple2('すてっかー', 'ステッカー'),
			_Utils_Tuple2('すてっき', 'ステッキ'),
			_Utils_Tuple2('すでに', '既に'),
			_Utils_Tuple2('すてれお', 'ステレオ'),
			_Utils_Tuple2('すてれおたいぷ', 'ステレオタイプ'),
			_Utils_Tuple2('すとーりー', 'ストーリー'),
			_Utils_Tuple2('すとっきんぐ', 'ストッキング'),
			_Utils_Tuple2('すとっぱー', 'ストッパー'),
			_Utils_Tuple2('すとっぷ', 'ストップ'),
			_Utils_Tuple2('すとらいぷ', 'ストライプ'),
			_Utils_Tuple2('すとらっぷ', 'ストラップ'),
			_Utils_Tuple2('すとりーと', 'ストリート'),
			_Utils_Tuple2('すとれーと', 'ストレート'),
			_Utils_Tuple2('すとれす', 'ストレス'),
			_Utils_Tuple2('すとれっち', 'ストレッチ'),
			_Utils_Tuple2('すとろー', 'ストロー'),
			_Utils_Tuple2('すとろべりー', 'ストロベリー'),
			_Utils_Tuple2('すな', '砂'),
			_Utils_Tuple2('すなお', '素直'),
			_Utils_Tuple2('すなば', '砂場'),
			_Utils_Tuple2('すなはま', '砂浜'),
			_Utils_Tuple2('すなわち', 'すなわち'),
			_Utils_Tuple2('すにーかー', 'スニーカー'),
			_Utils_Tuple2('すのー', 'スノー'),
			_Utils_Tuple2('すのーぼーど', 'スノーボード'),
			_Utils_Tuple2('すぱい', 'スパイ'),
			_Utils_Tuple2('すぱいす', 'スパイス'),
			_Utils_Tuple2('すぴーかー', 'スピーカー'),
			_Utils_Tuple2('すぴーち', 'スピーチ'),
			_Utils_Tuple2('すぴーでぃー', 'スピーディー'),
			_Utils_Tuple2('すぴーど', 'スピード'),
			_Utils_Tuple2('すぴーどあっぷ', 'スピードアップ'),
			_Utils_Tuple2('ずひょう', '図表'),
			_Utils_Tuple2('すぶた', '酢豚'),
			_Utils_Tuple2('すぷりんぐ', 'スプリング'),
			_Utils_Tuple2('すぷれー', 'スプレー'),
			_Utils_Tuple2('すぺーす', 'スペース'),
			_Utils_Tuple2('すぺーすしゃとる', 'スペースシャトル'),
			_Utils_Tuple2('すぺしゃりすと', 'スペシャリスト'),
			_Utils_Tuple2('すぺしゃる', 'スペシャル'),
			_Utils_Tuple2('すべて', '全て'),
			_Utils_Tuple2('すべり', '滑り'),
			_Utils_Tuple2('すべる', '滑る'),
			_Utils_Tuple2('すぺる', 'スペル'),
			_Utils_Tuple2('すぽーつかー', 'スポーツカー'),
			_Utils_Tuple2('すぽーつまん', 'スポーツマン'),
			_Utils_Tuple2('すぽーてぃー', 'スポーティー'),
			_Utils_Tuple2('すぽっと', 'スポット'),
			_Utils_Tuple2('すぽんさー', 'スポンサー'),
			_Utils_Tuple2('すぽんじ', 'スポンジ'),
			_Utils_Tuple2('すぽんじけーき', 'スポンジケーキ'),
			_Utils_Tuple2('すまーと', 'スマート'),
			_Utils_Tuple2('すまい', '住まい'),
			_Utils_Tuple2('すまいる', 'スマイル'),
			_Utils_Tuple2('すます', '済ます'),
			_Utils_Tuple2('すみ', '隅'),
			_Utils_Tuple2('ずみ', '済み'),
			_Utils_Tuple2('すむ', '済む'),
			_Utils_Tuple2('すむーず', 'スムーズ'),
			_Utils_Tuple2('すもう', 'すもう'),
			_Utils_Tuple2('すもうとり', 'すもうとり'),
			_Utils_Tuple2('すもーく', 'スモーク'),
			_Utils_Tuple2('すらいす', 'スライス'),
			_Utils_Tuple2('すらっくす', 'スラックス'),
			_Utils_Tuple2('ずらっと', 'ずらっと'),
			_Utils_Tuple2('すり', 'すり'),
			_Utils_Tuple2('すりー', 'スリー'),
			_Utils_Tuple2('すりっぷ', 'スリップ'),
			_Utils_Tuple2('すりむ', 'スリム'),
			_Utils_Tuple2('すりる', 'スリル'),
			_Utils_Tuple2('ずるい', 'ずるい'),
			_Utils_Tuple2('すると', 'すると'),
			_Utils_Tuple2('するどい', '鋭い'),
			_Utils_Tuple2('ずるやすみ', 'ずる休み'),
			_Utils_Tuple2('すれちがう', '擦れ違う'),
			_Utils_Tuple2('ずれる', 'ずれる'),
			_Utils_Tuple2('ずれる', 'ずれる'),
			_Utils_Tuple2('すろー', 'スロー'),
			_Utils_Tuple2('すろー', 'スロー'),
			_Utils_Tuple2('すわり', '座り'),
			_Utils_Tuple2('すわりこむ', '座り込む'),
			_Utils_Tuple2('すわん', 'スワン'),
			_Utils_Tuple2('せい', 'せい'),
			_Utils_Tuple2('せい', '製'),
			_Utils_Tuple2('せい', '性'),
			_Utils_Tuple2('せい', '性'),
			_Utils_Tuple2('せい', '生'),
			_Utils_Tuple2('せい', '姓'),
			_Utils_Tuple2('せい', '世'),
			_Utils_Tuple2('せい', '星'),
			_Utils_Tuple2('せい', '正'),
			_Utils_Tuple2('せい', '静'),
			_Utils_Tuple2('せい', '声'),
			_Utils_Tuple2('せい', '正'),
			_Utils_Tuple2('ぜい', '税'),
			_Utils_Tuple2('せいいっぱい', '精一杯'),
			_Utils_Tuple2('せいか', '成果'),
			_Utils_Tuple2('せいかい', '正解'),
			_Utils_Tuple2('せいかく', '性格'),
			_Utils_Tuple2('せいかく', '正確'),
			_Utils_Tuple2('ぜいがく', '税額'),
			_Utils_Tuple2('せいかつすいじゅん', '生活水準'),
			_Utils_Tuple2('せいかつひ', '生活費'),
			_Utils_Tuple2('ぜいかん', '税関'),
			_Utils_Tuple2('せいき', '世紀'),
			_Utils_Tuple2('せいきゅう', '請求'),
			_Utils_Tuple2('せいきゅうしょ', '請求書'),
			_Utils_Tuple2('ぜいきん', '税金'),
			_Utils_Tuple2('せいけい', '整形'),
			_Utils_Tuple2('せいけいげか', '整形外科'),
			_Utils_Tuple2('せいけん', '政権'),
			_Utils_Tuple2('せいげん', '制限'),
			_Utils_Tuple2('せいご', '生後'),
			_Utils_Tuple2('せいこう', '成功'),
			_Utils_Tuple2('ぜいこみ', '税込み'),
			_Utils_Tuple2('せいざ', '正座'),
			_Utils_Tuple2('せいざ', '星座'),
			_Utils_Tuple2('せいさく', '政策'),
			_Utils_Tuple2('せいさく', '製作'),
			_Utils_Tuple2('せいさく', '制作'),
			_Utils_Tuple2('せいさん', '生産'),
			_Utils_Tuple2('せいさん', '清算'),
			_Utils_Tuple2('せいさん', '精算'),
			_Utils_Tuple2('せいさんてき', '生産的'),
			_Utils_Tuple2('せいし', '静止'),
			_Utils_Tuple2('せいじ', '政治'),
			_Utils_Tuple2('せいじか', '政治家'),
			_Utils_Tuple2('せいじがく', '政治学'),
			_Utils_Tuple2('せいじかつどう', '政治活動'),
			_Utils_Tuple2('せいしき', '正式'),
			_Utils_Tuple2('せいしつ', '性質'),
			_Utils_Tuple2('せいじてき', '政治的'),
			_Utils_Tuple2('せいしゅん', '青春'),
			_Utils_Tuple2('せいしょ', '聖書'),
			_Utils_Tuple2('せいじょう', '正常'),
			_Utils_Tuple2('せいしょうねん', '青少年'),
			_Utils_Tuple2('せいしん', '精神'),
			_Utils_Tuple2('せいじん', '成人'),
			_Utils_Tuple2('せいじんしき', '成人式'),
			_Utils_Tuple2('せいしんてき', '精神的'),
			_Utils_Tuple2('せいじんのひ', '成人の日'),
			_Utils_Tuple2('せいしんりょく', '精神力'),
			_Utils_Tuple2('せいせき', '成績'),
			_Utils_Tuple2('せいせきひょう', '成績表'),
			_Utils_Tuple2('せいせん', '生鮮'),
			_Utils_Tuple2('せいぜん', '生前'),
			_Utils_Tuple2('せいせんしょくひん', '生鮮食品'),
			_Utils_Tuple2('せいそう', '清掃'),
			_Utils_Tuple2('せいそう', '正装'),
			_Utils_Tuple2('せいぞう', '製造'),
			_Utils_Tuple2('せいそうしゃ', '清掃車'),
			_Utils_Tuple2('せいぞん', '生存'),
			_Utils_Tuple2('ぜいたく', 'ぜいたく'),
			_Utils_Tuple2('ぜいたく', '贅沢'),
			_Utils_Tuple2('せいちょう', '成長'),
			_Utils_Tuple2('せいちょう', '生長'),
			_Utils_Tuple2('せいてん', '晴天'),
			_Utils_Tuple2('せいでんき', '静電気'),
			_Utils_Tuple2('せいど', '制度'),
			_Utils_Tuple2('せいとかい', '生徒会'),
			_Utils_Tuple2('せいとん', '整とん'),
			_Utils_Tuple2('せいねん', '青年'),
			_Utils_Tuple2('せいねん', '成年'),
			_Utils_Tuple2('せいのう', '性能'),
			_Utils_Tuple2('せいはんたい', '正反対'),
			_Utils_Tuple2('せいび', '整備'),
			_Utils_Tuple2('せいひん', '製品'),
			_Utils_Tuple2('せいふ', '政府'),
			_Utils_Tuple2('せいぶ', '西部'),
			_Utils_Tuple2('せいふく', '制服'),
			_Utils_Tuple2('せいふく', '征服'),
			_Utils_Tuple2('せいぶつ', '生物'),
			_Utils_Tuple2('せいぶつがく', '生物学'),
			_Utils_Tuple2('せいぶん', '成分'),
			_Utils_Tuple2('せいべつ', '性別'),
			_Utils_Tuple2('せいぼ', '歳暮'),
			_Utils_Tuple2('せいほうけい', '正方形'),
			_Utils_Tuple2('せいめい', '生命'),
			_Utils_Tuple2('せいめい', '姓名'),
			_Utils_Tuple2('せいめいほけん', '生命保険'),
			_Utils_Tuple2('せいめいりょく', '生命力'),
			_Utils_Tuple2('せいもん', '正門'),
			_Utils_Tuple2('せいやく', '製薬'),
			_Utils_Tuple2('せいゆう', '声優'),
			_Utils_Tuple2('せいよう', '西洋'),
			_Utils_Tuple2('せいよう', '静養'),
			_Utils_Tuple2('せいようふう', '西洋風'),
			_Utils_Tuple2('せいようりょうり', '西洋料理'),
			_Utils_Tuple2('せいり', '整理'),
			_Utils_Tuple2('せいり', '生理'),
			_Utils_Tuple2('ぜいりし', '税理士'),
			_Utils_Tuple2('せいりつ', '成立'),
			_Utils_Tuple2('ぜいりつ', '税率'),
			_Utils_Tuple2('せいりょく', '勢力'),
			_Utils_Tuple2('せいれき', '西暦'),
			_Utils_Tuple2('せいれつ', '整列'),
			_Utils_Tuple2('せーふ', 'セーフ'),
			_Utils_Tuple2('せーぶ', 'セーブ'),
			_Utils_Tuple2('せーふてぃー', 'セーフティー'),
			_Utils_Tuple2('せーらー', 'セーラー'),
			_Utils_Tuple2('せーらーふく', 'セーラー服'),
			_Utils_Tuple2('せーるす', 'セールス'),
			_Utils_Tuple2('せーるすまん', 'セールスマン'),
			_Utils_Tuple2('せおう', '背負う'),
			_Utils_Tuple2('せかいきろく', '世界記録'),
			_Utils_Tuple2('せかいたいせん', '世界大戦'),
			_Utils_Tuple2('せかいてき', '世界的'),
			_Utils_Tuple2('せかんど', 'セカンド'),
			_Utils_Tuple2('せき', '赤'),
			_Utils_Tuple2('せき', '石'),
			_Utils_Tuple2('せき', '籍'),
			_Utils_Tuple2('せきせつ', '積雪'),
			_Utils_Tuple2('せきたん', '石炭'),
			_Utils_Tuple2('せきどう', '赤道'),
			_Utils_Tuple2('せきにん', '責任'),
			_Utils_Tuple2('せきにんかん', '責任感'),
			_Utils_Tuple2('せきにんしゃ', '責任者'),
			_Utils_Tuple2('せきにんのうりょく', '責任能力'),
			_Utils_Tuple2('せきはん', '赤飯'),
			_Utils_Tuple2('せきゆ', '石油'),
			_Utils_Tuple2('せきゆすとーぶ', '石油ストーブ'),
			_Utils_Tuple2('せきゅりてぃー', 'セキュリティー'),
			_Utils_Tuple2('せくしー', 'セクシー'),
			_Utils_Tuple2('せくしゃる', 'セクシャル'),
			_Utils_Tuple2('せくはら', 'セクハラ'),
			_Utils_Tuple2('せだい', '世代'),
			_Utils_Tuple2('せつ', '説'),
			_Utils_Tuple2('せっかく', 'せっかく'),
			_Utils_Tuple2('せっきゃく', '接客'),
			_Utils_Tuple2('せっきょう', '説教'),
			_Utils_Tuple2('せっきょく', '積極'),
			_Utils_Tuple2('せっきょくてき', '積極的'),
			_Utils_Tuple2('せっきん', '接近'),
			_Utils_Tuple2('せっけい', '設計'),
			_Utils_Tuple2('ぜっけん', 'ゼッケン'),
			_Utils_Tuple2('ぜっこう', '絶好'),
			_Utils_Tuple2('せっする', '接する'),
			_Utils_Tuple2('せっせと', 'せっせと'),
			_Utils_Tuple2('せっせん', '接戦'),
			_Utils_Tuple2('せつぞく', '接続'),
			_Utils_Tuple2('せつぞくし', '接続詞'),
			_Utils_Tuple2('せったい', '接待'),
			_Utils_Tuple2('ぜったい', '絶対'),
			_Utils_Tuple2('ぜったいてき', '絶対的'),
			_Utils_Tuple2('せっち', '設置'),
			_Utils_Tuple2('せっちゃく', '接着'),
			_Utils_Tuple2('せってい', '設定'),
			_Utils_Tuple2('せってぃんぐ', 'セッティング'),
			_Utils_Tuple2('せってん', '接点'),
			_Utils_Tuple2('せつでん', '節電'),
			_Utils_Tuple2('せっと', 'セット'),
			_Utils_Tuple2('せっとく', '説得'),
			_Utils_Tuple2('せっとくりょく', '説得力'),
			_Utils_Tuple2('せつび', '設備'),
			_Utils_Tuple2('せつぶん', '節分'),
			_Utils_Tuple2('ぜつぼう', '絶望'),
			_Utils_Tuple2('ぜつぼうてき', '絶望的'),
			_Utils_Tuple2('せつめい', '説明'),
			_Utils_Tuple2('せつめいぶん', '説明文'),
			_Utils_Tuple2('せつやく', '節約'),
			_Utils_Tuple2('せつりつ', '設立'),
			_Utils_Tuple2('せのび', '背伸び'),
			_Utils_Tuple2('せばんごう', '背番号'),
			_Utils_Tuple2('ぜひ', '是非'),
			_Utils_Tuple2('ぜひ', '是非'),
			_Utils_Tuple2('ぜひとも', '是非とも'),
			_Utils_Tuple2('せびろ', '背広'),
			_Utils_Tuple2('せぼね', '背骨'),
			_Utils_Tuple2('せまる', '迫る'),
			_Utils_Tuple2('せみ', 'せみ'),
			_Utils_Tuple2('ぜみ', 'ゼミ'),
			_Utils_Tuple2('せみなー', 'セミナー'),
			_Utils_Tuple2('せめて', 'せめて'),
			_Utils_Tuple2('せめる', '攻める'),
			_Utils_Tuple2('せめる', '責める'),
			_Utils_Tuple2('せめんと', 'セメント'),
			_Utils_Tuple2('ぜらちん', 'ゼラチン'),
			_Utils_Tuple2('ぜりー', 'ゼリー'),
			_Utils_Tuple2('せりふ', 'せりふ'),
			_Utils_Tuple2('せるふ', 'セルフ'),
			_Utils_Tuple2('せれくと', 'セレクト'),
			_Utils_Tuple2('せろり', 'セロリ'),
			_Utils_Tuple2('せろん', '世論'),
			_Utils_Tuple2('せろんちょうさ', '世論調査'),
			_Utils_Tuple2('せわ', '世話'),
			_Utils_Tuple2('せわやく', '世話役'),
			_Utils_Tuple2('せん', '線'),
			_Utils_Tuple2('せん', '船'),
			_Utils_Tuple2('せん', '泉'),
			_Utils_Tuple2('せん', '栓'),
			_Utils_Tuple2('せん', '選'),
			_Utils_Tuple2('ぜん', '前'),
			_Utils_Tuple2('ぜん', '全'),
			_Utils_Tuple2('ぜん', '前'),
			_Utils_Tuple2('ぜん', '善'),
			_Utils_Tuple2('ぜんあく', '善悪'),
			_Utils_Tuple2('ぜんい', '善意'),
			_Utils_Tuple2('ぜんいき', '全域'),
			_Utils_Tuple2('せんいん', '船員'),
			_Utils_Tuple2('ぜんいん', '全員'),
			_Utils_Tuple2('ぜんかい', '前回'),
			_Utils_Tuple2('ぜんかい', '全開'),
			_Utils_Tuple2('ぜんかい', '全壊'),
			_Utils_Tuple2('ぜんがく', '全額'),
			_Utils_Tuple2('せんがん', '洗顔'),
			_Utils_Tuple2('ぜんかん', '全巻'),
			_Utils_Tuple2('ぜんかん', '全館'),
			_Utils_Tuple2('ぜんき', '前期'),
			_Utils_Tuple2('せんきょ', '選挙'),
			_Utils_Tuple2('せんぎょう', '専業'),
			_Utils_Tuple2('せんきょく', '選挙区'),
			_Utils_Tuple2('せんきょく', '選曲'),
			_Utils_Tuple2('ぜんきょく', '全曲'),
			_Utils_Tuple2('せんきょけん', '選挙権'),
			_Utils_Tuple2('せんぎり', '千切り'),
			_Utils_Tuple2('せんけつ', '先決'),
			_Utils_Tuple2('せんげん', '宣言'),
			_Utils_Tuple2('せんご', '戦後'),
			_Utils_Tuple2('ぜんご', '前後'),
			_Utils_Tuple2('せんこう', '専攻'),
			_Utils_Tuple2('せんこう', '線香'),
			_Utils_Tuple2('せんこう', '先行'),
			_Utils_Tuple2('せんこう', '選考'),
			_Utils_Tuple2('せんこうはなび', '線香花火'),
			_Utils_Tuple2('ぜんこく', '全国'),
			_Utils_Tuple2('ぜんこくく', '全国区'),
			_Utils_Tuple2('せんごくじだい', '戦国時代'),
			_Utils_Tuple2('せんさー', 'センサー'),
			_Utils_Tuple2('せんざい', '洗剤'),
			_Utils_Tuple2('ぜんさく', '前作'),
			_Utils_Tuple2('せんし', '戦死'),
			_Utils_Tuple2('せんじつ', '先日'),
			_Utils_Tuple2('ぜんじつ', '前日'),
			_Utils_Tuple2('せんしゃ', '洗車'),
			_Utils_Tuple2('せんしゃ', '戦車'),
			_Utils_Tuple2('ぜんしゃ', '前者'),
			_Utils_Tuple2('せんしゅ', '選手'),
			_Utils_Tuple2('ぜんしゅう', '全集'),
			_Utils_Tuple2('せんじょう', '戦場'),
			_Utils_Tuple2('せんじょう', '線上'),
			_Utils_Tuple2('ぜんしょう', '全焼'),
			_Utils_Tuple2('せんしん', '先進'),
			_Utils_Tuple2('ぜんしん', '前進'),
			_Utils_Tuple2('ぜんしん', '全身'),
			_Utils_Tuple2('せんしんこく', '先進国'),
			_Utils_Tuple2('せんす', 'センス'),
			_Utils_Tuple2('せんす', '扇子'),
			_Utils_Tuple2('せんぜん', '戦前'),
			_Utils_Tuple2('ぜんせん', '全線'),
			_Utils_Tuple2('ぜんせん', '前線'),
			_Utils_Tuple2('せんぞ', '先祖'),
			_Utils_Tuple2('せんそう', '戦争'),
			_Utils_Tuple2('せんぞく', '専属'),
			_Utils_Tuple2('せんたー', 'センター'),
			_Utils_Tuple2('せんたーらいん', 'センターライン'),
			_Utils_Tuple2('ぜんたい', '全体'),
			_Utils_Tuple2('せんたく', '選択'),
			_Utils_Tuple2('せんたくし', '選択肢'),
			_Utils_Tuple2('せんたくもの', '洗濯物'),
			_Utils_Tuple2('せんたん', '先端'),
			_Utils_Tuple2('せんちゃく', '先着'),
			_Utils_Tuple2('せんちょう', '船長'),
			_Utils_Tuple2('ぜんちょう', '全長'),
			_Utils_Tuple2('せんてい', '選定'),
			_Utils_Tuple2('せんでん', '宣伝'),
			_Utils_Tuple2('ぜんど', '全土'),
			_Utils_Tuple2('せんとう', '銭湯'),
			_Utils_Tuple2('せんとう', '先頭'),
			_Utils_Tuple2('せんにん', '専任'),
			_Utils_Tuple2('せんねん', '専念'),
			_Utils_Tuple2('ぜんねん', '前年'),
			_Utils_Tuple2('せんぱい', '先輩'),
			_Utils_Tuple2('せんぱつ', '先発'),
			_Utils_Tuple2('せんぱつ', '洗髪'),
			_Utils_Tuple2('ぜんはん', '前半'),
			_Utils_Tuple2('ぜんぱん', '全般'),
			_Utils_Tuple2('ぜんぱんてき', '全般的'),
			_Utils_Tuple2('せんびん', '船便'),
			_Utils_Tuple2('せんぷうき', '扇風機'),
			_Utils_Tuple2('ぜんぶん', '全文'),
			_Utils_Tuple2('ぜんぶん', '前文'),
			_Utils_Tuple2('せんべい', 'せんべい'),
			_Utils_Tuple2('せんべつ', '選別'),
			_Utils_Tuple2('せんぽう', '先方'),
			_Utils_Tuple2('ぜんぽう', '前方'),
			_Utils_Tuple2('せんめい', '鮮明'),
			_Utils_Tuple2('せんめん', '洗面'),
			_Utils_Tuple2('ぜんめん', '前面'),
			_Utils_Tuple2('ぜんめん', '全面'),
			_Utils_Tuple2('せんめんき', '洗面器'),
			_Utils_Tuple2('せんめんじょ', '洗面所'),
			_Utils_Tuple2('ぜんめんてき', '全面的'),
			_Utils_Tuple2('せんもん', '専門'),
			_Utils_Tuple2('せんもんか', '専門家'),
			_Utils_Tuple2('せんもんがっこう', '専門学校'),
			_Utils_Tuple2('せんもんてん', '専門店'),
			_Utils_Tuple2('ぜんや', '前夜'),
			_Utils_Tuple2('せんやく', '先約'),
			_Utils_Tuple2('せんよう', '専用'),
			_Utils_Tuple2('ぜんりゃく', '前略'),
			_Utils_Tuple2('ぜんりょく', '全力'),
			_Utils_Tuple2('ぜんれい', '前例'),
			_Utils_Tuple2('ぜんれつ', '前列'),
			_Utils_Tuple2('せんろ', '線路'),
			_Utils_Tuple2('ぞい', '沿い'),
			_Utils_Tuple2('そいつ', 'そいつ'),
			_Utils_Tuple2('そう', 'そう'),
			_Utils_Tuple2('そう', '層'),
			_Utils_Tuple2('そう', '僧'),
			_Utils_Tuple2('そう', '沿う'),
			_Utils_Tuple2('そう', '総'),
			_Utils_Tuple2('そう', '走'),
			_Utils_Tuple2('そう', '双'),
			_Utils_Tuple2('そう', '草'),
			_Utils_Tuple2('ぞう', '像'),
			_Utils_Tuple2('ぞう', '増'),
			_Utils_Tuple2('そうい', '相違'),
			_Utils_Tuple2('そうい', '総意'),
			_Utils_Tuple2('そういう', 'そういう'),
			_Utils_Tuple2('ぞういん', '増員'),
			_Utils_Tuple2('そうおん', '騒音'),
			_Utils_Tuple2('ぞうか', '増加'),
			_Utils_Tuple2('そうかい', '総会'),
			_Utils_Tuple2('そうがく', '総額'),
			_Utils_Tuple2('そうがんきょう', '双眼鏡'),
			_Utils_Tuple2('そうき', '早期'),
			_Utils_Tuple2('そうぎ', '葬儀'),
			_Utils_Tuple2('ぞうき', '臓器'),
			_Utils_Tuple2('そうぎょう', '創業'),
			_Utils_Tuple2('ぞうきょう', '増強'),
			_Utils_Tuple2('そうきん', '送金'),
			_Utils_Tuple2('ぞうきん', 'ぞうきん'),
			_Utils_Tuple2('そうげん', '草原'),
			_Utils_Tuple2('ぞうげん', '増減'),
			_Utils_Tuple2('そうこ', '倉庫'),
			_Utils_Tuple2('そうご', '相互'),
			_Utils_Tuple2('そうこう', '走行'),
			_Utils_Tuple2('そうごう', '総合'),
			_Utils_Tuple2('そうさ', '操作'),
			_Utils_Tuple2('そうさ', '捜査'),
			_Utils_Tuple2('そうさく', '創作'),
			_Utils_Tuple2('ぞうさん', '増産'),
			_Utils_Tuple2('そうしき', '葬式'),
			_Utils_Tuple2('そうじき', '掃除機'),
			_Utils_Tuple2('そうしゃ', '走者'),
			_Utils_Tuple2('そうしん', '送信'),
			_Utils_Tuple2('そうすう', '総数'),
			_Utils_Tuple2('ぞうぜい', '増税'),
			_Utils_Tuple2('そうせつ', '創設'),
			_Utils_Tuple2('ぞうせつ', '増設'),
			_Utils_Tuple2('そうせんきょ', '総選挙'),
			_Utils_Tuple2('そうそう', 'そうそう'),
			_Utils_Tuple2('そうそう', '早々'),
			_Utils_Tuple2('そうぞう', '想像'),
			_Utils_Tuple2('そうぞう', '創造'),
			_Utils_Tuple2('そうぞうりょく', '想像力'),
			_Utils_Tuple2('そうぞく', '相続'),
			_Utils_Tuple2('そうたい', '早退'),
			_Utils_Tuple2('ぞうだい', '増大'),
			_Utils_Tuple2('そうだん', '相談'),
			_Utils_Tuple2('そうち', '装置'),
			_Utils_Tuple2('そうちょう', '早朝'),
			_Utils_Tuple2('そうとう', '相当'),
			_Utils_Tuple2('そうとう', '相当'),
			_Utils_Tuple2('そうとう', '相当'),
			_Utils_Tuple2('そうどう', '騒動'),
			_Utils_Tuple2('ぞうに', '雑煮'),
			_Utils_Tuple2('そうにゅう', '挿入'),
			_Utils_Tuple2('そうふ', '送付'),
			_Utils_Tuple2('そうふう', '送風'),
			_Utils_Tuple2('そうべつ', '送別'),
			_Utils_Tuple2('そうめん', 'そうめん'),
			_Utils_Tuple2('そうり', '総理'),
			_Utils_Tuple2('そうりだいじん', '総理大臣'),
			_Utils_Tuple2('そうりつ', '創立'),
			_Utils_Tuple2('そうりょう', '送料'),
			_Utils_Tuple2('そうりょう', '総量'),
			_Utils_Tuple2('ぞうりょう', '増量'),
			_Utils_Tuple2('そうる', 'ソウル'),
			_Utils_Tuple2('そーせーじ', 'ソーセージ'),
			_Utils_Tuple2('そーだ', 'ソーダ'),
			_Utils_Tuple2('そーらー', 'ソーラー'),
			_Utils_Tuple2('ぞーん', 'ゾーン'),
			_Utils_Tuple2('そく', '即'),
			_Utils_Tuple2('ぞく', '続'),
			_Utils_Tuple2('そくし', '即死'),
			_Utils_Tuple2('そくじ', '即時'),
			_Utils_Tuple2('ぞくしゅつ', '続出'),
			_Utils_Tuple2('ぞくする', '属する'),
			_Utils_Tuple2('ぞくぞく', '続々'),
			_Utils_Tuple2('そくたつ', '速達'),
			_Utils_Tuple2('そくてい', '測定'),
			_Utils_Tuple2('そくど', '速度'),
			_Utils_Tuple2('そくとう', '即答'),
			_Utils_Tuple2('そくばい', '即売'),
			_Utils_Tuple2('ぞくへん', '続編'),
			_Utils_Tuple2('そくほう', '速報'),
			_Utils_Tuple2('そくめん', '側面'),
			_Utils_Tuple2('そくりょう', '測量'),
			_Utils_Tuple2('そこ', '底'),
			_Utils_Tuple2('そこく', '祖国'),
			_Utils_Tuple2('そこそこ', 'そこそこ'),
			_Utils_Tuple2('そこで', 'そこで'),
			_Utils_Tuple2('そざい', '素材'),
			_Utils_Tuple2('そしき', '組織'),
			_Utils_Tuple2('そしつ', '素質'),
			_Utils_Tuple2('そせん', '祖先'),
			_Utils_Tuple2('そだち', '育ち'),
			_Utils_Tuple2('そだつ', '育つ'),
			_Utils_Tuple2('そだてる', '育てる'),
			_Utils_Tuple2('そっきゅう', '速球'),
			_Utils_Tuple2('そつぎょう', '卒業'),
			_Utils_Tuple2('そつぎょうろんぶん', '卒業論文'),
			_Utils_Tuple2('そっくす', 'ソックス'),
			_Utils_Tuple2('そっくり', 'そっくり'),
			_Utils_Tuple2('そっくり', 'そっくり'),
			_Utils_Tuple2('ぞっこう', '続行'),
			_Utils_Tuple2('そっちょく', '率直'),
			_Utils_Tuple2('そっと', 'そっと'),
			_Utils_Tuple2('ぞっと', 'ぞっと'),
			_Utils_Tuple2('そつろん', '卒論'),
			_Utils_Tuple2('そとがわ', '外側'),
			_Utils_Tuple2('そとまわり', '外回り'),
			_Utils_Tuple2('そのうえ', 'そのうえ'),
			_Utils_Tuple2('そのうち', 'そのうち'),
			_Utils_Tuple2('そのかわり', 'そのかわり'),
			_Utils_Tuple2('そのころ', 'そのころ'),
			_Utils_Tuple2('そのほか', 'そのほか'),
			_Utils_Tuple2('そのまま', 'そのまま'),
			_Utils_Tuple2('そのまま', 'そのまま'),
			_Utils_Tuple2('そふと', 'ソフト'),
			_Utils_Tuple2('そふとうぇあ', 'ソフトウェア'),
			_Utils_Tuple2('そふとぼーる', 'ソフトボール'),
			_Utils_Tuple2('そふぼ', '祖父母'),
			_Utils_Tuple2('そぷらの', 'ソプラノ'),
			_Utils_Tuple2('そめる', '染める'),
			_Utils_Tuple2('そらまめ', '空豆'),
			_Utils_Tuple2('そる', 'そる'),
			_Utils_Tuple2('それぞれ', 'それぞれ'),
			_Utils_Tuple2('それだけ', 'それだけ'),
			_Utils_Tuple2('それで', 'それで'),
			_Utils_Tuple2('それでは', 'それでは'),
			_Utils_Tuple2('それでも', 'それでも'),
			_Utils_Tuple2('それと', 'それと'),
			_Utils_Tuple2('それとも', 'それとも'),
			_Utils_Tuple2('それなり', 'それなり'),
			_Utils_Tuple2('それに', 'それに'),
			_Utils_Tuple2('それほど', 'それほど'),
			_Utils_Tuple2('それん', 'ソ連'),
			_Utils_Tuple2('そろう', 'そろう'),
			_Utils_Tuple2('そろえる', 'そろえる'),
			_Utils_Tuple2('そろそろ', 'そろそろ'),
			_Utils_Tuple2('そん', '村'),
			_Utils_Tuple2('そん', '損'),
			_Utils_Tuple2('そんがい', '損害'),
			_Utils_Tuple2('そんぐ', 'ソング'),
			_Utils_Tuple2('そんけい', '尊敬'),
			_Utils_Tuple2('そんけいご', '尊敬語'),
			_Utils_Tuple2('そんざい', '存在'),
			_Utils_Tuple2('そんざいかん', '存在感'),
			_Utils_Tuple2('そんしつ', '損失'),
			_Utils_Tuple2('そんぞく', '存続'),
			_Utils_Tuple2('そんちょう', '尊重'),
			_Utils_Tuple2('そんちょう', '村長'),
			_Utils_Tuple2('そんとく', '損得'),
			_Utils_Tuple2('そんな', 'そんな'),
			_Utils_Tuple2('そんな', 'そんな'),
			_Utils_Tuple2('た', '他'),
			_Utils_Tuple2('た', '田'),
			_Utils_Tuple2('た', '多'),
			_Utils_Tuple2('た', '多'),
			_Utils_Tuple2('だーく', 'ダーク'),
			_Utils_Tuple2('たーげっと', 'ターゲット'),
			_Utils_Tuple2('だーす', 'ダース'),
			_Utils_Tuple2('たーみなる', 'ターミナル'),
			_Utils_Tuple2('たい', '体'),
			_Utils_Tuple2('たい', '対'),
			_Utils_Tuple2('たい', '体'),
			_Utils_Tuple2('たい', 'タイ'),
			_Utils_Tuple2('たい', '隊'),
			_Utils_Tuple2('たい', '隊'),
			_Utils_Tuple2('たい', 'タイ'),
			_Utils_Tuple2('だい', '第'),
			_Utils_Tuple2('だい', '大'),
			_Utils_Tuple2('だい', '代'),
			_Utils_Tuple2('だい', '台'),
			_Utils_Tuple2('だい', '大'),
			_Utils_Tuple2('だい', '題'),
			_Utils_Tuple2('だい', '代'),
			_Utils_Tuple2('だい', '台'),
			_Utils_Tuple2('だい', '題'),
			_Utils_Tuple2('だい', '代'),
			_Utils_Tuple2('たいいく', '体育'),
			_Utils_Tuple2('たいいくかん', '体育館'),
			_Utils_Tuple2('たいいくのひ', '体育の日'),
			_Utils_Tuple2('だいいち', '第一'),
			_Utils_Tuple2('だいいち', '第一'),
			_Utils_Tuple2('だいいちいんしょう', '第一印象'),
			_Utils_Tuple2('たいいん', '退院'),
			_Utils_Tuple2('たいいん', '隊員'),
			_Utils_Tuple2('たいいん', '隊員'),
			_Utils_Tuple2('だいえっと', 'ダイエット'),
			_Utils_Tuple2('たいおう', '対応'),
			_Utils_Tuple2('だいおう', '大王'),
			_Utils_Tuple2('たいおん', '体温'),
			_Utils_Tuple2('たいおんけい', '体温計'),
			_Utils_Tuple2('たいがー', 'タイガー'),
			_Utils_Tuple2('たいかい', '大会'),
			_Utils_Tuple2('たいかい', '退会'),
			_Utils_Tuple2('たいがい', '体外'),
			_Utils_Tuple2('たいかく', '体格'),
			_Utils_Tuple2('たいがく', '退学'),
			_Utils_Tuple2('たいかくせん', '対角線'),
			_Utils_Tuple2('だいがくりょう', '大学寮'),
			_Utils_Tuple2('だいかぞく', '大家族'),
			_Utils_Tuple2('たいかん', '体感'),
			_Utils_Tuple2('たいがん', '対岸'),
			_Utils_Tuple2('たいかんおんど', '体感温度'),
			_Utils_Tuple2('たいき', '大気'),
			_Utils_Tuple2('たいきおせん', '大気汚染'),
			_Utils_Tuple2('だいきぎょう', '大企業'),
			_Utils_Tuple2('だいきぼ', '大規模'),
			_Utils_Tuple2('だいきらい', '大嫌い'),
			_Utils_Tuple2('たいきん', '大金'),
			_Utils_Tuple2('だいきん', '代金'),
			_Utils_Tuple2('だいく', '大工'),
			_Utils_Tuple2('たいぐう', '待遇'),
			_Utils_Tuple2('たいくつ', '退屈'),
			_Utils_Tuple2('たいぐん', '大群'),
			_Utils_Tuple2('たいけい', '体型'),
			_Utils_Tuple2('たいけい', '体系'),
			_Utils_Tuple2('たいけい', '体形'),
			_Utils_Tuple2('たいけいてき', '体系的'),
			_Utils_Tuple2('たいけつ', '対決'),
			_Utils_Tuple2('たいけん', '体験'),
			_Utils_Tuple2('たいこ', '太鼓'),
			_Utils_Tuple2('だいこう', '代行'),
			_Utils_Tuple2('だいこん', '大根'),
			_Utils_Tuple2('たいさ', '大差'),
			_Utils_Tuple2('たいざい', '滞在'),
			_Utils_Tuple2('だいざい', '題材'),
			_Utils_Tuple2('たいさく', '対策'),
			_Utils_Tuple2('たいさく', '大作'),
			_Utils_Tuple2('だいさん', '第三'),
			_Utils_Tuple2('だいさんしゃ', '第三者'),
			_Utils_Tuple2('たいし', '大使'),
			_Utils_Tuple2('だいじ', '大事'),
			_Utils_Tuple2('だいじ', '大事'),
			_Utils_Tuple2('だいしぜん', '大自然'),
			_Utils_Tuple2('たいした', '大した'),
			_Utils_Tuple2('たいしつ', '退室'),
			_Utils_Tuple2('たいしつ', '体質'),
			_Utils_Tuple2('たいして', '大して'),
			_Utils_Tuple2('たいしゃ', '退社'),
			_Utils_Tuple2('たいじゅう', '体重'),
			_Utils_Tuple2('たいしゅつ', '退出'),
			_Utils_Tuple2('たいしょう', '対象'),
			_Utils_Tuple2('たいしょう', '大正'),
			_Utils_Tuple2('たいしょう', '対照'),
			_Utils_Tuple2('たいしょう', '対称'),
			_Utils_Tuple2('たいじょう', '退場'),
			_Utils_Tuple2('だいしょう', '大小'),
			_Utils_Tuple2('たいしょうてき', '対照的'),
			_Utils_Tuple2('たいしょく', '退職'),
			_Utils_Tuple2('たいしょくきん', '退職金'),
			_Utils_Tuple2('たいじん', '対人'),
			_Utils_Tuple2('だいじん', '大臣'),
			_Utils_Tuple2('だいしんさい', '大震災'),
			_Utils_Tuple2('だいず', '大豆'),
			_Utils_Tuple2('たいする', '対する'),
			_Utils_Tuple2('たいせい', '体制'),
			_Utils_Tuple2('たいせい', '大勢'),
			_Utils_Tuple2('たいせき', '体積'),
			_Utils_Tuple2('たいせん', '大戦'),
			_Utils_Tuple2('たいせん', '対戦'),
			_Utils_Tuple2('たいそう', '体操'),
			_Utils_Tuple2('だいそつ', '大卒'),
			_Utils_Tuple2('だいたい', '大体'),
			_Utils_Tuple2('だいだい', '代々'),
			_Utils_Tuple2('だいだいてき', '大々的'),
			_Utils_Tuple2('だいたすう', '大多数'),
			_Utils_Tuple2('たいだん', '退団'),
			_Utils_Tuple2('たいだん', '対談'),
			_Utils_Tuple2('だいたん', '大胆'),
			_Utils_Tuple2('だいち', '大地'),
			_Utils_Tuple2('たいちょう', '体調'),
			_Utils_Tuple2('たいちょう', '体長'),
			_Utils_Tuple2('たいちょう', '体長'),
			_Utils_Tuple2('たいつ', 'タイツ'),
			_Utils_Tuple2('たいてい', '大抵'),
			_Utils_Tuple2('たいてい', '大抵'),
			_Utils_Tuple2('たいど', '態度'),
			_Utils_Tuple2('たいとう', '対等'),
			_Utils_Tuple2('だいとうりょう', '大統領'),
			_Utils_Tuple2('だいとかい', '大都会'),
			_Utils_Tuple2('だいとし', '大都市'),
			_Utils_Tuple2('たいとる', 'タイトル'),
			_Utils_Tuple2('たいない', '体内'),
			_Utils_Tuple2('だいにんぐ', 'ダイニング'),
			_Utils_Tuple2('だいにんぐきっちん', 'ダイニングキッチン'),
			_Utils_Tuple2('だいばー', 'ダイバー'),
			_Utils_Tuple2('たいはい', '大敗'),
			_Utils_Tuple2('たいばつ', '体罰'),
			_Utils_Tuple2('たいはん', '大半'),
			_Utils_Tuple2('たいひ', '対比'),
			_Utils_Tuple2('だいひょう', '代表'),
			_Utils_Tuple2('だいひょうさく', '代表作'),
			_Utils_Tuple2('だいひょうてき', '代表的'),
			_Utils_Tuple2('だいびんぐ', 'ダイビング'),
			_Utils_Tuple2('だいぶ', '大分'),
			_Utils_Tuple2('たいふう', '台風'),
			_Utils_Tuple2('だいぶつ', '大仏'),
			_Utils_Tuple2('だいぶぶん', '大部分'),
			_Utils_Tuple2('たいへいよう', '太平洋'),
			_Utils_Tuple2('たいほ', '逮捕'),
			_Utils_Tuple2('たいまー', 'タイマー'),
			_Utils_Tuple2('たいみんぐ', 'タイミング'),
			_Utils_Tuple2('たいむりー', 'タイムリー'),
			_Utils_Tuple2('だいめい', '題名'),
			_Utils_Tuple2('だいめいし', '代名詞'),
			_Utils_Tuple2('たいめん', '対面'),
			_Utils_Tuple2('たいや', 'タイヤ'),
			_Utils_Tuple2('だいやく', '代役'),
			_Utils_Tuple2('だいやもんど', 'ダイヤモンド'),
			_Utils_Tuple2('だいやる', 'ダイヤル'),
			_Utils_Tuple2('たいよう', '太陽'),
			_Utils_Tuple2('だいよう', '代用'),
			_Utils_Tuple2('たいら', '平ら'),
			_Utils_Tuple2('だいり', '代理'),
			_Utils_Tuple2('たいりく', '大陸'),
			_Utils_Tuple2('たいりつ', '対立'),
			_Utils_Tuple2('だいりてん', '代理店'),
			_Utils_Tuple2('たいりょう', '大量'),
			_Utils_Tuple2('たいりょうせいさん', '大量生産'),
			_Utils_Tuple2('たいりょく', '体力'),
			_Utils_Tuple2('たいわ', '対話'),
			_Utils_Tuple2('たうん', 'タウン'),
			_Utils_Tuple2('だうん', 'ダウン'),
			_Utils_Tuple2('だうんろーど', 'ダウンロード'),
			_Utils_Tuple2('たおす', '倒す'),
			_Utils_Tuple2('たおる', 'タオル'),
			_Utils_Tuple2('たおれる', '倒れる'),
			_Utils_Tuple2('だか', '高'),
			_Utils_Tuple2('だが', 'だが'),
			_Utils_Tuple2('たがい', '互い'),
			_Utils_Tuple2('たがいに', '互いに'),
			_Utils_Tuple2('たがく', '多額'),
			_Utils_Tuple2('たかめ', '高め'),
			_Utils_Tuple2('たから', '宝'),
			_Utils_Tuple2('だから', 'だから'),
			_Utils_Tuple2('たからもの', '宝物'),
			_Utils_Tuple2('たき', '滝'),
			_Utils_Tuple2('だきあう', '抱き合う'),
			_Utils_Tuple2('だきあげる', '抱き上げる'),
			_Utils_Tuple2('だきかかえる', '抱き抱える'),
			_Utils_Tuple2('だきしめる', '抱き締める'),
			_Utils_Tuple2('たく', '宅'),
			_Utils_Tuple2('たく', '炊く'),
			_Utils_Tuple2('だく', '抱く'),
			_Utils_Tuple2('たくあん', 'たくあん'),
			_Utils_Tuple2('たくはい', '宅配'),
			_Utils_Tuple2('たくはいびん', '宅配便'),
			_Utils_Tuple2('たけ', '竹'),
			_Utils_Tuple2('たけのこ', '竹の子'),
			_Utils_Tuple2('たこ', 'たこ'),
			_Utils_Tuple2('たこ', 'たこ'),
			_Utils_Tuple2('たこあげ', 'たこ揚げ'),
			_Utils_Tuple2('たこく', '他国'),
			_Utils_Tuple2('たこくせき', '多国籍'),
			_Utils_Tuple2('だしあう', '出し合う'),
			_Utils_Tuple2('だしいれ', '出し入れ'),
			_Utils_Tuple2('たしか', '確か'),
			_Utils_Tuple2('たしか', '確か'),
			_Utils_Tuple2('たしかめる', '確かめる'),
			_Utils_Tuple2('たしざん', '足し算'),
			_Utils_Tuple2('たしゃ', '他者'),
			_Utils_Tuple2('たしゃ', '他社'),
			_Utils_Tuple2('たしゅ', '多種'),
			_Utils_Tuple2('たしょう', '多少'),
			_Utils_Tuple2('たす', '足す'),
			_Utils_Tuple2('たすう', '多数'),
			_Utils_Tuple2('たすうけつ', '多数決'),
			_Utils_Tuple2('たすかる', '助かる'),
			_Utils_Tuple2('たすけ', '助け'),
			_Utils_Tuple2('たすけあう', '助け合う'),
			_Utils_Tuple2('たすける', '助ける'),
			_Utils_Tuple2('たずねる', '尋ねる'),
			_Utils_Tuple2('たずねる', '訪ねる'),
			_Utils_Tuple2('たた', '多々'),
			_Utils_Tuple2('ただ', 'ただ'),
			_Utils_Tuple2('ただ', '只'),
			_Utils_Tuple2('ただ', 'ただ'),
			_Utils_Tuple2('ただい', '多大'),
			_Utils_Tuple2('たたかい', '戦い'),
			_Utils_Tuple2('たたかう', '戦う'),
			_Utils_Tuple2('たたく', 'たたく'),
			_Utils_Tuple2('ただし', '但し'),
			_Utils_Tuple2('ただしい', '正しい'),
			_Utils_Tuple2('ただちに', '直ちに'),
			_Utils_Tuple2('たたみ', '畳'),
			_Utils_Tuple2('たたむ', '畳む'),
			_Utils_Tuple2('たちあがる', '立ち上がる'),
			_Utils_Tuple2('たちいり', '立ち入り'),
			_Utils_Tuple2('たちいりきんし', '立ち入り禁止'),
			_Utils_Tuple2('たちいる', '立ち入る'),
			_Utils_Tuple2('たちぐい', '立ち食い'),
			_Utils_Tuple2('たちさる', '立ち去る'),
			_Utils_Tuple2('たちどまる', '立ち止まる'),
			_Utils_Tuple2('たちなおる', '立ち直る'),
			_Utils_Tuple2('たちば', '立場'),
			_Utils_Tuple2('たちばなし', '立ち話'),
			_Utils_Tuple2('たちみ', '立ち見'),
			_Utils_Tuple2('たちよみ', '立ち読み'),
			_Utils_Tuple2('たつ', '経つ'),
			_Utils_Tuple2('たつ', '建つ'),
			_Utils_Tuple2('たっする', '達する'),
			_Utils_Tuple2('たっせい', '達成'),
			_Utils_Tuple2('たっち', 'タッチ'),
			_Utils_Tuple2('だって', 'だって'),
			_Utils_Tuple2('たっぷり', 'たっぷり'),
			_Utils_Tuple2('たて', '縦'),
			_Utils_Tuple2('たて', '立て'),
			_Utils_Tuple2('たて', '立て'),
			_Utils_Tuple2('だて', '建て'),
			_Utils_Tuple2('たてかえる', '建て替える'),
			_Utils_Tuple2('たてがき', '縦書き'),
			_Utils_Tuple2('たてじま', '縦じま'),
			_Utils_Tuple2('たてまえ', '建前'),
			_Utils_Tuple2('たどうし', '他動詞'),
			_Utils_Tuple2('たとえ', '例え'),
			_Utils_Tuple2('たとえば', '例えば'),
			_Utils_Tuple2('たとえる', '例える'),
			_Utils_Tuple2('たな', '棚'),
			_Utils_Tuple2('たなばた', '七夕'),
			_Utils_Tuple2('たなばたまつり', '七夕祭り'),
			_Utils_Tuple2('たに', '谷'),
			_Utils_Tuple2('たにま', '谷間'),
			_Utils_Tuple2('たにん', '他人'),
			_Utils_Tuple2('たにんずう', '多人数'),
			_Utils_Tuple2('たぬき', 'たぬき'),
			_Utils_Tuple2('たね', '種'),
			_Utils_Tuple2('たのみ', '頼み'),
			_Utils_Tuple2('たのむ', '頼む'),
			_Utils_Tuple2('たのもしい', '頼もしい'),
			_Utils_Tuple2('たば', '束'),
			_Utils_Tuple2('たはた', '田畑'),
			_Utils_Tuple2('たはつ', '多発'),
			_Utils_Tuple2('たび', '旅'),
			_Utils_Tuple2('たび', '度'),
			_Utils_Tuple2('たびさき', '旅先'),
			_Utils_Tuple2('たびだち', '旅立ち'),
			_Utils_Tuple2('たびだつ', '旅立つ'),
			_Utils_Tuple2('たびたび', '度々'),
			_Utils_Tuple2('たびびと', '旅人'),
			_Utils_Tuple2('たびやかた', '旅館'),
			_Utils_Tuple2('だびんぐ', 'ダビング'),
			_Utils_Tuple2('たふ', 'タフ'),
			_Utils_Tuple2('たぶー', 'タブー'),
			_Utils_Tuple2('だぶる', 'ダブル'),
			_Utils_Tuple2('たべものや', '食べ物屋'),
			_Utils_Tuple2('たほう', '他方'),
			_Utils_Tuple2('たぼう', '多忙'),
			_Utils_Tuple2('たま', '玉'),
			_Utils_Tuple2('たま', '玉'),
			_Utils_Tuple2('だます', 'だます'),
			_Utils_Tuple2('たまたま', 'たまたま'),
			_Utils_Tuple2('たまに', 'たまに'),
			_Utils_Tuple2('たまる', 'たまる'),
			_Utils_Tuple2('たまる', '貯まる'),
			_Utils_Tuple2('だまる', '黙る'),
			_Utils_Tuple2('ため', '為'),
			_Utils_Tuple2('ためいき', 'ため息'),
			_Utils_Tuple2('だめーじ', 'ダメージ'),
			_Utils_Tuple2('ためし', '試し'),
			_Utils_Tuple2('ためす', '試す'),
			_Utils_Tuple2('ためる', '貯める'),
			_Utils_Tuple2('ためる', 'ためる'),
			_Utils_Tuple2('たもつ', '保つ'),
			_Utils_Tuple2('たよう', '多用'),
			_Utils_Tuple2('たより', '頼り'),
			_Utils_Tuple2('たより', '便り'),
			_Utils_Tuple2('たよる', '頼る'),
			_Utils_Tuple2('たらこ', 'たらこ'),
			_Utils_Tuple2('たりょう', '多量'),
			_Utils_Tuple2('たりる', '足りる'),
			_Utils_Tuple2('たる', '足る'),
			_Utils_Tuple2('だるい', 'だるい'),
			_Utils_Tuple2('たると', 'タルト'),
			_Utils_Tuple2('だれだれ', '誰々'),
			_Utils_Tuple2('たれんと', 'タレント'),
			_Utils_Tuple2('たわー', 'タワー'),
			_Utils_Tuple2('たん', '短'),
			_Utils_Tuple2('たん', '単'),
			_Utils_Tuple2('だん', '段'),
			_Utils_Tuple2('だん', '団'),
			_Utils_Tuple2('だん', '段'),
			_Utils_Tuple2('だん', '団'),
			_Utils_Tuple2('たんい', '単位'),
			_Utils_Tuple2('だんかい', '段階'),
			_Utils_Tuple2('たんき', '短期'),
			_Utils_Tuple2('たんきかん', '短期間'),
			_Utils_Tuple2('たんきだいがく', '短期大学'),
			_Utils_Tuple2('たんきょり', '短距離'),
			_Utils_Tuple2('たんく', 'タンク'),
			_Utils_Tuple2('たんくとっぷ', 'タンクトップ'),
			_Utils_Tuple2('たんけん', '探検'),
			_Utils_Tuple2('たんご', '単語'),
			_Utils_Tuple2('たんご', 'タンゴ'),
			_Utils_Tuple2('だんご', '団子'),
			_Utils_Tuple2('だんさー', 'ダンサー'),
			_Utils_Tuple2('たんざく', '短冊'),
			_Utils_Tuple2('たんさん', '炭酸'),
			_Utils_Tuple2('だんし', '男子'),
			_Utils_Tuple2('たんじかん', '短時間'),
			_Utils_Tuple2('たんじゅん', '単純'),
			_Utils_Tuple2('たんじゅん', '単純'),
			_Utils_Tuple2('たんじゅんか', '単純化'),
			_Utils_Tuple2('たんしょ', '短所'),
			_Utils_Tuple2('だんじょ', '男女'),
			_Utils_Tuple2('たんじょう', '誕生'),
			_Utils_Tuple2('たんす', 'たんす'),
			_Utils_Tuple2('だんすぱーてぃー', 'ダンスパーティー'),
			_Utils_Tuple2('だんせいてき', '男性的'),
			_Utils_Tuple2('たんだい', '短大'),
			_Utils_Tuple2('だんたい', '団体'),
			_Utils_Tuple2('だんだん', '段々'),
			_Utils_Tuple2('だんだん', '段々'),
			_Utils_Tuple2('だんち', '団地'),
			_Utils_Tuple2('だんちょう', '団長'),
			_Utils_Tuple2('たんてい', '探偵'),
			_Utils_Tuple2('だんてい', '断定'),
			_Utils_Tuple2('たんとう', '担当'),
			_Utils_Tuple2('たんどく', '単独'),
			_Utils_Tuple2('だんな', 'だんな'),
			_Utils_Tuple2('たんなる', '単なる'),
			_Utils_Tuple2('たんに', '単に'),
			_Utils_Tuple2('たんにん', '担任'),
			_Utils_Tuple2('たんぱん', '短パン'),
			_Utils_Tuple2('だんぼう', '暖房'),
			_Utils_Tuple2('たんめい', '短命'),
			_Utils_Tuple2('だんゆう', '男優'),
			_Utils_Tuple2('だんらく', '段落'),
			_Utils_Tuple2('ち', '血'),
			_Utils_Tuple2('ち', '地'),
			_Utils_Tuple2('ち', '知'),
			_Utils_Tuple2('ち', '池'),
			_Utils_Tuple2('ちあん', '治安'),
			_Utils_Tuple2('ちい', '地位'),
			_Utils_Tuple2('ちいき', '地域'),
			_Utils_Tuple2('ちいきしゃかい', '地域社会'),
			_Utils_Tuple2('ちーぷ', 'チープ'),
			_Utils_Tuple2('ちーむ', 'チーム'),
			_Utils_Tuple2('ちーむめーと', 'チームメート'),
			_Utils_Tuple2('ちーむわーく', 'チームワーク'),
			_Utils_Tuple2('ちえ', '知恵'),
			_Utils_Tuple2('ちぇあ', 'チェア'),
			_Utils_Tuple2('ちぇーん', 'チェーン'),
			_Utils_Tuple2('ちぇす', 'チェス'),
			_Utils_Tuple2('ちぇっく', 'チェック'),
			_Utils_Tuple2('ちぇっくあうと', 'チェックアウト'),
			_Utils_Tuple2('ちぇっくぽいんと', 'チェックポイント'),
			_Utils_Tuple2('ちぇっくりすと', 'チェックリスト'),
			_Utils_Tuple2('ちぇりー', 'チェリー'),
			_Utils_Tuple2('ちえん', '遅延'),
			_Utils_Tuple2('ちぇんじ', 'チェンジ'),
			_Utils_Tuple2('ちか', '地下'),
			_Utils_Tuple2('ちがい', '違い'),
			_Utils_Tuple2('ちかがい', '地下街'),
			_Utils_Tuple2('ちかごろ', '近ごろ'),
			_Utils_Tuple2('ちかすい', '地下水'),
			_Utils_Tuple2('ちかぢか', '近々'),
			_Utils_Tuple2('ちかどう', '地下道'),
			_Utils_Tuple2('ちかみち', '近道'),
			_Utils_Tuple2('ちかよる', '近寄る'),
			_Utils_Tuple2('ちから', '力'),
			_Utils_Tuple2('ちからづよい', '力強い'),
			_Utils_Tuple2('ちからもち', '力持ち'),
			_Utils_Tuple2('ちかん', '痴漢'),
			_Utils_Tuple2('ちきゅう', '地球'),
			_Utils_Tuple2('ちきゅうぎ', '地球儀'),
			_Utils_Tuple2('ちきん', 'チキン'),
			_Utils_Tuple2('ちきんらいす', 'チキンライス'),
			_Utils_Tuple2('ちく', '地区'),
			_Utils_Tuple2('ちけい', '地形'),
			_Utils_Tuple2('ちこく', '遅刻'),
			_Utils_Tuple2('ちじ', '知事'),
			_Utils_Tuple2('ちしき', '知識'),
			_Utils_Tuple2('ちじょう', '地上'),
			_Utils_Tuple2('ちじん', '知人'),
			_Utils_Tuple2('ちずちょう', '地図帳'),
			_Utils_Tuple2('ちせい', '知性'),
			_Utils_Tuple2('ちちおや', '父親'),
			_Utils_Tuple2('ちちかた', '父方'),
			_Utils_Tuple2('ちちはは', '父母'),
			_Utils_Tuple2('ちちゅう', '地中'),
			_Utils_Tuple2('ちっとも', 'ちっとも'),
			_Utils_Tuple2('ちてき', '知的'),
			_Utils_Tuple2('ちてん', '地点'),
			_Utils_Tuple2('ちなみに', 'ちなみに'),
			_Utils_Tuple2('ちへいせん', '地平線'),
			_Utils_Tuple2('ちほう', '地方'),
			_Utils_Tuple2('ちほうこうむいん', '地方公務員'),
			_Utils_Tuple2('ちめい', '地名'),
			_Utils_Tuple2('ちゃーじ', 'チャージ'),
			_Utils_Tuple2('ちゃーしゅー', 'チャーシュー'),
			_Utils_Tuple2('ちゃーはん', 'チャーハン'),
			_Utils_Tuple2('ちゃーみんぐ', 'チャーミング'),
			_Utils_Tuple2('ちゃいむ', 'チャイム'),
			_Utils_Tuple2('ちゃいろい', '茶色い'),
			_Utils_Tuple2('ちゃく', '着'),
			_Utils_Tuple2('ちゃく', '着'),
			_Utils_Tuple2('ちゃくせき', '着席'),
			_Utils_Tuple2('ちゃくばらい', '着払い'),
			_Utils_Tuple2('ちゃくりく', '着陸'),
			_Utils_Tuple2('ちゃっく', 'チャック'),
			_Utils_Tuple2('ちゃづけ', '茶漬け'),
			_Utils_Tuple2('ちゃぺる', 'チャペル'),
			_Utils_Tuple2('ちゃれんじ', 'チャレンジ'),
			_Utils_Tuple2('ちゃれんじゃー', 'チャレンジャー'),
			_Utils_Tuple2('ちゃわん', '茶碗'),
			_Utils_Tuple2('ちゃんす', 'チャンス'),
			_Utils_Tuple2('ちゃんと', 'ちゃんと'),
			_Utils_Tuple2('ちゃんねる', 'チャンネル'),
			_Utils_Tuple2('ちゃんぴおん', 'チャンピオン'),
			_Utils_Tuple2('ちゅう', '中'),
			_Utils_Tuple2('ちゅう', '中'),
			_Utils_Tuple2('ちゅう', '中'),
			_Utils_Tuple2('ちゅう', '注'),
			_Utils_Tuple2('ちゅういぶかい', '注意深い'),
			_Utils_Tuple2('ちゅうおう', '中央'),
			_Utils_Tuple2('ちゅうか', '中華'),
			_Utils_Tuple2('ちゅうから', '中辛'),
			_Utils_Tuple2('ちゅうかん', '中間'),
			_Utils_Tuple2('ちゅうき', '中期'),
			_Utils_Tuple2('ちゅうきゅう', '中級'),
			_Utils_Tuple2('ちゅうけい', '中継'),
			_Utils_Tuple2('ちゅうこ', '中古'),
			_Utils_Tuple2('ちゅうこうねん', '中高年'),
			_Utils_Tuple2('ちゅうこく', '忠告'),
			_Utils_Tuple2('ちゅうごくちほう', '中国地方'),
			_Utils_Tuple2('ちゅうこしゃ', '中古車'),
			_Utils_Tuple2('ちゅうこひん', '中古品'),
			_Utils_Tuple2('ちゅうし', '中止'),
			_Utils_Tuple2('ちゅうしゃ', '駐車'),
			_Utils_Tuple2('ちゅうしゃ', '注射'),
			_Utils_Tuple2('ちゅうしゃじょう', '駐車場'),
			_Utils_Tuple2('ちゅうじゅん', '中旬'),
			_Utils_Tuple2('ちゅうしょう', '抽象'),
			_Utils_Tuple2('ちゅうしょうきぎょう', '中小企業'),
			_Utils_Tuple2('ちゅうしょく', '昼食'),
			_Utils_Tuple2('ちゅうしん', '中心'),
			_Utils_Tuple2('ちゅうせん', '抽選'),
			_Utils_Tuple2('ちゅうたい', '中退'),
			_Utils_Tuple2('ちゅうと', '中途'),
			_Utils_Tuple2('ちゅうとう', '中東'),
			_Utils_Tuple2('ちゅうねん', '中年'),
			_Utils_Tuple2('ちゅうび', '中火'),
			_Utils_Tuple2('ちゅーぶ', 'チューブ'),
			_Utils_Tuple2('ちゅうぶ', '中部'),
			_Utils_Tuple2('ちゅうもく', '注目'),
			_Utils_Tuple2('ちゅうもん', '注文'),
			_Utils_Tuple2('ちゅうや', '昼夜'),
			_Utils_Tuple2('ちゅうりつ', '中立'),
			_Utils_Tuple2('ちゅうりゅう', '中流'),
			_Utils_Tuple2('ちゅうりん', '駐輪'),
			_Utils_Tuple2('ちょう', '朝'),
			_Utils_Tuple2('ちょう', '長'),
			_Utils_Tuple2('ちょう', '町'),
			_Utils_Tuple2('ちょう', '帳'),
			_Utils_Tuple2('ちょう', '庁'),
			_Utils_Tuple2('ちょう', '超'),
			_Utils_Tuple2('ちょう', '調'),
			_Utils_Tuple2('ちょう', '丁'),
			_Utils_Tuple2('ちょう', 'ちょう'),
			_Utils_Tuple2('ちょう', '超'),
			_Utils_Tuple2('ちょう', '兆'),
			_Utils_Tuple2('ちょう', '長'),
			_Utils_Tuple2('ちょうか', '超過'),
			_Utils_Tuple2('ちょうかん', '朝刊'),
			_Utils_Tuple2('ちょうかん', '長官'),
			_Utils_Tuple2('ちょうき', '長期'),
			_Utils_Tuple2('ちょうきょり', '長距離'),
			_Utils_Tuple2('ちょうこう', '聴講'),
			_Utils_Tuple2('ちょうこうそう', '超高層'),
			_Utils_Tuple2('ちょうこく', '彫刻'),
			_Utils_Tuple2('ちょうさ', '調査'),
			_Utils_Tuple2('ちょうし', '調子'),
			_Utils_Tuple2('ちょうじかん', '長時間'),
			_Utils_Tuple2('ちょうじゅ', '長寿'),
			_Utils_Tuple2('ちょうしょ', '長所'),
			_Utils_Tuple2('ちょうじょ', '長女'),
			_Utils_Tuple2('ちょうじょう', '頂上'),
			_Utils_Tuple2('ちょうしょく', '朝食'),
			_Utils_Tuple2('ちょうせい', '調整'),
			_Utils_Tuple2('ちょうせつ', '調節'),
			_Utils_Tuple2('ちょうせん', '挑戦'),
			_Utils_Tuple2('ちょうだい', 'ちょうだい'),
			_Utils_Tuple2('ちょうてん', '頂点'),
			_Utils_Tuple2('ちょうど', '丁度'),
			_Utils_Tuple2('ちょうない', '町内'),
			_Utils_Tuple2('ちょうないかい', '町内会'),
			_Utils_Tuple2('ちょうなん', '長男'),
			_Utils_Tuple2('ちょうはつ', '長髪'),
			_Utils_Tuple2('ちょうぶん', '長文'),
			_Utils_Tuple2('ちょうほうけい', '長方形'),
			_Utils_Tuple2('ちょうみ', '調味'),
			_Utils_Tuple2('ちょうみりょう', '調味料'),
			_Utils_Tuple2('ちょうみん', '町民'),
			_Utils_Tuple2('ちょうめ', '丁目'),
			_Utils_Tuple2('ちょうめい', '町名'),
			_Utils_Tuple2('ちょうり', '調理'),
			_Utils_Tuple2('ちょうりし', '調理師'),
			_Utils_Tuple2('ちょき', 'ちょき'),
			_Utils_Tuple2('ちょきん', '貯金'),
			_Utils_Tuple2('ちょくご', '直後'),
			_Utils_Tuple2('ちょくしん', '直進'),
			_Utils_Tuple2('ちょくせつ', '直接'),
			_Utils_Tuple2('ちょくせつ', '直接'),
			_Utils_Tuple2('ちょくせつてき', '直接的'),
			_Utils_Tuple2('ちょくせん', '直線'),
			_Utils_Tuple2('ちょくぜん', '直前'),
			_Utils_Tuple2('ちょくつう', '直通'),
			_Utils_Tuple2('ちょくめん', '直面'),
			_Utils_Tuple2('ちょくりつ', '直立'),
			_Utils_Tuple2('ちょしゃ', '著者'),
			_Utils_Tuple2('ちょしょ', '著書'),
			_Utils_Tuple2('ちょっかく', '直角'),
			_Utils_Tuple2('ちょっこう', '直行'),
			_Utils_Tuple2('ちょっぴり', 'ちょっぴり'),
			_Utils_Tuple2('ちらかす', '散らかす'),
			_Utils_Tuple2('ちらかる', '散らかる'),
			_Utils_Tuple2('ちらっと', 'ちらっと'),
			_Utils_Tuple2('ちらり', 'ちらり'),
			_Utils_Tuple2('ちらりと', 'ちらりと'),
			_Utils_Tuple2('ちり', '地理'),
			_Utils_Tuple2('ちりょう', '治療'),
			_Utils_Tuple2('ちる', '散る'),
			_Utils_Tuple2('ちん', '賃'),
			_Utils_Tuple2('ちんたい', '賃貸'),
			_Utils_Tuple2('ちんぱんじー', 'チンパンジー'),
			_Utils_Tuple2('つあー', 'ツアー'),
			_Utils_Tuple2('つい', 'つい'),
			_Utils_Tuple2('ついか', '追加'),
			_Utils_Tuple2('ついで', '次いで'),
			_Utils_Tuple2('ついに', 'ついに'),
			_Utils_Tuple2('ついん', 'ツイン'),
			_Utils_Tuple2('つう', '通'),
			_Utils_Tuple2('つう', '痛'),
			_Utils_Tuple2('つういん', '通院'),
			_Utils_Tuple2('つうか', '通過'),
			_Utils_Tuple2('つうか', '通貨'),
			_Utils_Tuple2('つうがく', '通学'),
			_Utils_Tuple2('つうきん', '通勤'),
			_Utils_Tuple2('つうこう', '通行'),
			_Utils_Tuple2('つうこうどめ', '通行止め'),
			_Utils_Tuple2('つうじょう', '通常'),
			_Utils_Tuple2('つうしん', '通信'),
			_Utils_Tuple2('つうしんはんばい', '通信販売'),
			_Utils_Tuple2('つうち', '通知'),
			_Utils_Tuple2('つうちょう', '通帳'),
			_Utils_Tuple2('つうはん', '通販'),
			_Utils_Tuple2('つうほう', '通報'),
			_Utils_Tuple2('つうやく', '通訳'),
			_Utils_Tuple2('つうよう', '通用'),
			_Utils_Tuple2('つーりすと', 'ツーリスト'),
			_Utils_Tuple2('つうろ', '通路'),
			_Utils_Tuple2('つうわ', '通話'),
			_Utils_Tuple2('つかい', '使い'),
			_Utils_Tuple2('つかい', '使い'),
			_Utils_Tuple2('つかいすて', '使い捨て'),
			_Utils_Tuple2('つかいみち', '使い道'),
			_Utils_Tuple2('つかいわけ', '使い分け'),
			_Utils_Tuple2('つかいわける', '使い分ける'),
			_Utils_Tuple2('つかまえる', 'つかまえる'),
			_Utils_Tuple2('つかまえる', '捕まえる'),
			_Utils_Tuple2('つかまる', 'つかまる'),
			_Utils_Tuple2('つかまる', '捕まる'),
			_Utils_Tuple2('つかむ', 'つかむ'),
			_Utils_Tuple2('つかれ', '疲れ'),
			_Utils_Tuple2('つき', '付き'),
			_Utils_Tuple2('つき', '付き'),
			_Utils_Tuple2('つぎ', '次'),
			_Utils_Tuple2('つきあい', '付き合い'),
			_Utils_Tuple2('つきあう', '付き合う'),
			_Utils_Tuple2('つきあたり', '突き当たり'),
			_Utils_Tuple2('つきあたる', '突き当たる'),
			_Utils_Tuple2('つきおとす', '突き落とす'),
			_Utils_Tuple2('つきささる', '突き刺さる'),
			_Utils_Tuple2('つきさす', '突き刺す'),
			_Utils_Tuple2('つきづき', '月々'),
			_Utils_Tuple2('つぎつぎ', '次々'),
			_Utils_Tuple2('つきとばす', '突き飛ばす'),
			_Utils_Tuple2('つぎに', '次に'),
			_Utils_Tuple2('つきひ', '月日'),
			_Utils_Tuple2('つきみ', '月見'),
			_Utils_Tuple2('つく', '付く'),
			_Utils_Tuple2('つく', '着く'),
			_Utils_Tuple2('つく', '点く'),
			_Utils_Tuple2('つく', '就く'),
			_Utils_Tuple2('つく', '突く'),
			_Utils_Tuple2('つぐ', '次ぐ'),
			_Utils_Tuple2('づくり', '作り'),
			_Utils_Tuple2('つくりあげる', '作り上げる'),
			_Utils_Tuple2('つくりだす', '作り出す'),
			_Utils_Tuple2('つくりなおす', '作り直す'),
			_Utils_Tuple2('つくりばなし', '作り話'),
			_Utils_Tuple2('つくる', '造る'),
			_Utils_Tuple2('つけ', '付け'),
			_Utils_Tuple2('つけ', '付け'),
			_Utils_Tuple2('つけかえる', '付け替える'),
			_Utils_Tuple2('つけくわえる', '付け加える'),
			_Utils_Tuple2('つけもの', '漬け物'),
			_Utils_Tuple2('つける', '付ける'),
			_Utils_Tuple2('つける', '点ける'),
			_Utils_Tuple2('つける', '着ける'),
			_Utils_Tuple2('つける', '漬ける'),
			_Utils_Tuple2('っこ', 'っ子'),
			_Utils_Tuple2('つごう', '都合'),
			_Utils_Tuple2('つたえ', '伝え'),
			_Utils_Tuple2('つたえる', '伝える'),
			_Utils_Tuple2('つたわる', '伝わる'),
			_Utils_Tuple2('つち', '土'),
			_Utils_Tuple2('つづき', '続き'),
			_Utils_Tuple2('つづく', '続く'),
			_Utils_Tuple2('つづける', '続ける'),
			_Utils_Tuple2('つつみ', '包み'),
			_Utils_Tuple2('つつむ', '包む'),
			_Utils_Tuple2('つとめ', '勤め'),
			_Utils_Tuple2('つとめさき', '勤め先'),
			_Utils_Tuple2('つとめる', '勤める'),
			_Utils_Tuple2('つとめる', '務める'),
			_Utils_Tuple2('つとめる', '努める'),
			_Utils_Tuple2('つな', 'ツナ'),
			_Utils_Tuple2('つながり', 'つながり'),
			_Utils_Tuple2('つながる', 'つながる'),
			_Utils_Tuple2('つなぐ', 'つなぐ'),
			_Utils_Tuple2('つなげる', 'つなげる'),
			_Utils_Tuple2('つなひき', '綱引き'),
			_Utils_Tuple2('つなみ', '津波'),
			_Utils_Tuple2('つねに', '常に'),
			_Utils_Tuple2('つぶれる', '潰れる'),
			_Utils_Tuple2('つぼみ', 'つぼみ'),
			_Utils_Tuple2('つまずく', 'つまづく'),
			_Utils_Tuple2('つまみ', 'つまみ'),
			_Utils_Tuple2('つまり', 'つまり'),
			_Utils_Tuple2('つまる', '詰まる'),
			_Utils_Tuple2('つみ', '罪'),
			_Utils_Tuple2('つみあげる', '積み上げる'),
			_Utils_Tuple2('つみかさなる', '積み重なる'),
			_Utils_Tuple2('つみかさねる', '積み重ねる'),
			_Utils_Tuple2('つみこむ', '積み込む'),
			_Utils_Tuple2('つみに', '積み荷'),
			_Utils_Tuple2('つむ', '積む'),
			_Utils_Tuple2('つめあわせ', '詰め合わせ'),
			_Utils_Tuple2('つめかえる', '詰め替える'),
			_Utils_Tuple2('つめきり', '爪切り'),
			_Utils_Tuple2('つめこむ', '詰め込む'),
			_Utils_Tuple2('つめる', '詰める'),
			_Utils_Tuple2('つもり', '積り'),
			_Utils_Tuple2('つもる', '積もる'),
			_Utils_Tuple2('つゆ', '梅雨'),
			_Utils_Tuple2('つゆ', 'つゆ'),
			_Utils_Tuple2('つゆいり', '梅雨入り'),
			_Utils_Tuple2('つよき', '強気'),
			_Utils_Tuple2('つよび', '強火'),
			_Utils_Tuple2('つよまる', '強まる'),
			_Utils_Tuple2('つよめる', '強める'),
			_Utils_Tuple2('つらい', 'つらい'),
			_Utils_Tuple2('づらい', 'づらい'),
			_Utils_Tuple2('つり', '釣り'),
			_Utils_Tuple2('つるつる', 'つるつる'),
			_Utils_Tuple2('つれ', '連れ'),
			_Utils_Tuple2('づれ', '連れ'),
			_Utils_Tuple2('つれだす', '連れ出す'),
			_Utils_Tuple2('つれもどす', '連れ戻す'),
			_Utils_Tuple2('つれる', '連れる'),
			_Utils_Tuple2('で', 'で'),
			_Utils_Tuple2('で', '出'),
			_Utils_Tuple2('で', '出'),
			_Utils_Tuple2('であい', '出会い'),
			_Utils_Tuple2('であう', '出会う'),
			_Utils_Tuple2('てあし', '手足'),
			_Utils_Tuple2('てあらい', '手洗い'),
			_Utils_Tuple2('てい', '低'),
			_Utils_Tuple2('ていあん', '提案'),
			_Utils_Tuple2('でぃーえむ', 'ＤＭ'),
			_Utils_Tuple2('でぃーじぇー', 'ＤＪ'),
			_Utils_Tuple2('てぃーぶい', 'ＴＶ'),
			_Utils_Tuple2('ていいん', '定員'),
			_Utils_Tuple2('ていえん', '庭園'),
			_Utils_Tuple2('ていおん', '低温'),
			_Utils_Tuple2('ていおん', '低音'),
			_Utils_Tuple2('ていか', '低下'),
			_Utils_Tuple2('ていか', '定価'),
			_Utils_Tuple2('ていがく', '定額'),
			_Utils_Tuple2('ていがくねん', '低学年'),
			_Utils_Tuple2('ていき', '定期'),
			_Utils_Tuple2('ていきあつ', '低気圧'),
			_Utils_Tuple2('ていきけん', '定期券'),
			_Utils_Tuple2('ていきてき', '定期的'),
			_Utils_Tuple2('ていきゅう', '定休'),
			_Utils_Tuple2('ていきょう', '提供'),
			_Utils_Tuple2('ていけい', '定形'),
			_Utils_Tuple2('ていこう', '抵抗'),
			_Utils_Tuple2('ていし', '停止'),
			_Utils_Tuple2('ていじ', '提示'),
			_Utils_Tuple2('ていじ', '定時'),
			_Utils_Tuple2('ていしゃ', '停車'),
			_Utils_Tuple2('ていしゅ', '亭主'),
			_Utils_Tuple2('ていじゅう', '定住'),
			_Utils_Tuple2('ていしゅつ', '提出'),
			_Utils_Tuple2('ていしょく', '定食'),
			_Utils_Tuple2('ていしょく', '定職'),
			_Utils_Tuple2('でぃすかうんと', 'ディスカウント'),
			_Utils_Tuple2('でぃすかっしょん', 'ディスカッション'),
			_Utils_Tuple2('でぃすく', 'ディスク'),
			_Utils_Tuple2('ていそく', '低速'),
			_Utils_Tuple2('ていち', '低地'),
			_Utils_Tuple2('ていでん', '停電'),
			_Utils_Tuple2('ていど', '程度'),
			_Utils_Tuple2('でぃなー', 'ディナー'),
			_Utils_Tuple2('ていねい', '丁寧'),
			_Utils_Tuple2('ていねい', '丁寧'),
			_Utils_Tuple2('ていねいご', '丁寧語'),
			_Utils_Tuple2('ていねん', '定年'),
			_Utils_Tuple2('でいり', '出入り'),
			_Utils_Tuple2('でいりくち', '出入り口'),
			_Utils_Tuple2('ていりゅうしょ', '停留所'),
			_Utils_Tuple2('ていれ', '手入れ'),
			_Utils_Tuple2('でぃれくたー', 'ディレクター'),
			_Utils_Tuple2('でー', 'デー'),
			_Utils_Tuple2('てーくあうと', 'テークアウト'),
			_Utils_Tuple2('でーた', 'データ'),
			_Utils_Tuple2('でーと', 'デート'),
			_Utils_Tuple2('てーぶるくろす', 'テーブルクロス'),
			_Utils_Tuple2('てーま', 'テーマ'),
			_Utils_Tuple2('てーまそんぐ', 'テーマソング'),
			_Utils_Tuple2('でかい', 'でかい'),
			_Utils_Tuple2('てかがみ', '手鏡'),
			_Utils_Tuple2('てがき', '手書き'),
			_Utils_Tuple2('てがる', '手軽'),
			_Utils_Tuple2('てき', '的'),
			_Utils_Tuple2('てき', '敵'),
			_Utils_Tuple2('でき', '出来'),
			_Utils_Tuple2('できあがり', '出来上がり'),
			_Utils_Tuple2('できあがる', '出来上がる'),
			_Utils_Tuple2('てきおう', '適応'),
			_Utils_Tuple2('てきかく', '的確'),
			_Utils_Tuple2('できごと', '出来事'),
			_Utils_Tuple2('てきする', '適する'),
			_Utils_Tuple2('てきせつ', '適切'),
			_Utils_Tuple2('てきせつ', '適切'),
			_Utils_Tuple2('てきたい', '敵対'),
			_Utils_Tuple2('てきど', '適度'),
			_Utils_Tuple2('てきとう', '適当'),
			_Utils_Tuple2('てきにん', '適任'),
			_Utils_Tuple2('てきぱき', 'てきぱき'),
			_Utils_Tuple2('てきよう', '適用'),
			_Utils_Tuple2('てきりょう', '適量'),
			_Utils_Tuple2('できるだけ', '出来るだけ'),
			_Utils_Tuple2('てきれい', '適齢'),
			_Utils_Tuple2('てくにかる', 'テクニカル'),
			_Utils_Tuple2('てくにっく', 'テクニック'),
			_Utils_Tuple2('てくのろじー', 'テクノロジー'),
			_Utils_Tuple2('てくび', '手首'),
			_Utils_Tuple2('でざーと', 'デザート'),
			_Utils_Tuple2('でざいなー', 'デザイナー'),
			_Utils_Tuple2('でざいん', 'デザイン'),
			_Utils_Tuple2('でじかめ', 'デジカメ'),
			_Utils_Tuple2('でじたる', 'デジタル'),
			_Utils_Tuple2('でじたるか', 'デジタル化'),
			_Utils_Tuple2('でじたるかめら', 'デジタルカメラ'),
			_Utils_Tuple2('てじな', '手品'),
			_Utils_Tuple2('てすうりょう', '手数料'),
			_Utils_Tuple2('ですく', 'デスク'),
			_Utils_Tuple2('てちょう', '手帳'),
			_Utils_Tuple2('てつ', '鉄'),
			_Utils_Tuple2('てつ', '鉄'),
			_Utils_Tuple2('てつがく', '哲学'),
			_Utils_Tuple2('てっきん', '鉄筋'),
			_Utils_Tuple2('てづくり', '手作り'),
			_Utils_Tuple2('てつだい', '手伝い'),
			_Utils_Tuple2('てつだう', '手伝う'),
			_Utils_Tuple2('てつづき', '手続き'),
			_Utils_Tuple2('てつどう', '鉄道'),
			_Utils_Tuple2('てっぱん', '鉄板'),
			_Utils_Tuple2('てつや', '徹夜'),
			_Utils_Tuple2('でにむ', 'デニム'),
			_Utils_Tuple2('てにもつ', '手荷物'),
			_Utils_Tuple2('てぬき', '手抜き'),
			_Utils_Tuple2('では', 'では'),
			_Utils_Tuple2('てはい', '手配'),
			_Utils_Tuple2('てばなす', '手放す'),
			_Utils_Tuple2('でばん', '出番'),
			_Utils_Tuple2('でびゅー', 'デビュー'),
			_Utils_Tuple2('でぶ', 'でぶ'),
			_Utils_Tuple2('てぶくろ', '手袋'),
			_Utils_Tuple2('てぶら', '手ぶら'),
			_Utils_Tuple2('てぶり', '手振り'),
			_Utils_Tuple2('てま', '手間'),
			_Utils_Tuple2('てまえ', '手前'),
			_Utils_Tuple2('でまえ', '出前'),
			_Utils_Tuple2('でまわる', '出回る'),
			_Utils_Tuple2('てみやげ', '手土産'),
			_Utils_Tuple2('でむかえ', '出迎え'),
			_Utils_Tuple2('でむかえる', '出迎える'),
			_Utils_Tuple2('でめりっと', 'デメリット'),
			_Utils_Tuple2('でも', 'デモ'),
			_Utils_Tuple2('てもと', '手元'),
			_Utils_Tuple2('てらす', '照らす'),
			_Utils_Tuple2('でらっくす', 'デラックス'),
			_Utils_Tuple2('てりょうり', '手料理'),
			_Utils_Tuple2('てる', '照る'),
			_Utils_Tuple2('てれびでんわ', 'テレビ電話'),
			_Utils_Tuple2('てれほん', 'テレホン'),
			_Utils_Tuple2('てれほんかーど', 'テレホンカード'),
			_Utils_Tuple2('てれる', '照れる'),
			_Utils_Tuple2('てろ', 'テロ'),
			_Utils_Tuple2('てろりすと', 'テロリスト'),
			_Utils_Tuple2('てわたし', '手渡し'),
			_Utils_Tuple2('てわたす', '手渡す'),
			_Utils_Tuple2('てん', '点'),
			_Utils_Tuple2('てん', '店'),
			_Utils_Tuple2('てん', '天'),
			_Utils_Tuple2('てん', '展'),
			_Utils_Tuple2('でんあつ', '電圧'),
			_Utils_Tuple2('てんか', '点火'),
			_Utils_Tuple2('でんか', '電化'),
			_Utils_Tuple2('てんかい', '展開'),
			_Utils_Tuple2('でんき', '電器'),
			_Utils_Tuple2('でんき', '電機'),
			_Utils_Tuple2('でんききぐ', '電気器具'),
			_Utils_Tuple2('てんきず', '天気図'),
			_Utils_Tuple2('でんきすたんど', '電気スタンド'),
			_Utils_Tuple2('でんきせんたくき', '電気洗濯機'),
			_Utils_Tuple2('でんきゅう', '電球'),
			_Utils_Tuple2('てんきょ', '転居'),
			_Utils_Tuple2('てんきよほう', '天気予報'),
			_Utils_Tuple2('てんきん', '転勤'),
			_Utils_Tuple2('てんけい', '典型'),
			_Utils_Tuple2('てんけいてき', '典型的'),
			_Utils_Tuple2('てんけん', '点検'),
			_Utils_Tuple2('でんげん', '電源'),
			_Utils_Tuple2('てんこう', '転校'),
			_Utils_Tuple2('てんこう', '天候'),
			_Utils_Tuple2('てんごく', '天国'),
			_Utils_Tuple2('でんごん', '伝言'),
			_Utils_Tuple2('てんさい', '天才'),
			_Utils_Tuple2('てんし', '天使'),
			_Utils_Tuple2('てんじ', '展示'),
			_Utils_Tuple2('でんし', '電子'),
			_Utils_Tuple2('てんしゅ', '店主'),
			_Utils_Tuple2('てんじょう', '天井'),
			_Utils_Tuple2('てんしょく', '転職'),
			_Utils_Tuple2('でんしれんじ', '電子レンジ'),
			_Utils_Tuple2('てんすう', '点数'),
			_Utils_Tuple2('でんせつ', '伝説'),
			_Utils_Tuple2('でんせん', '電線'),
			_Utils_Tuple2('でんせん', '伝染'),
			_Utils_Tuple2('でんせんびょう', '伝染病'),
			_Utils_Tuple2('てんそう', '転送'),
			_Utils_Tuple2('でんたく', '電卓'),
			_Utils_Tuple2('でんたつ', '伝達'),
			_Utils_Tuple2('でんち', '電池'),
			_Utils_Tuple2('でんちゅう', '電柱'),
			_Utils_Tuple2('てんてん', '点々'),
			_Utils_Tuple2('てんと', 'テント'),
			_Utils_Tuple2('てんとう', '店頭'),
			_Utils_Tuple2('てんとう', '転倒'),
			_Utils_Tuple2('でんとう', '伝統'),
			_Utils_Tuple2('でんとう', '電灯'),
			_Utils_Tuple2('でんどう', '電動'),
			_Utils_Tuple2('てんどん', '天丼'),
			_Utils_Tuple2('てんない', '店内'),
			_Utils_Tuple2('てんにゅう', '転入'),
			_Utils_Tuple2('てんねん', '天然'),
			_Utils_Tuple2('てんねんがす', '天然ガス'),
			_Utils_Tuple2('てんのう', '天皇'),
			_Utils_Tuple2('でんぱ', '電波'),
			_Utils_Tuple2('でんぴょう', '伝票'),
			_Utils_Tuple2('てんぷら', '天ぷら'),
			_Utils_Tuple2('てんぽ', 'テンポ'),
			_Utils_Tuple2('でんぽう', '電報'),
			_Utils_Tuple2('てんらく', '転落'),
			_Utils_Tuple2('てんらんかい', '展覧会'),
			_Utils_Tuple2('でんりゅう', '電流'),
			_Utils_Tuple2('でんりょく', '電力'),
			_Utils_Tuple2('でんわき', '電話機'),
			_Utils_Tuple2('でんわきょく', '電話局'),
			_Utils_Tuple2('と', '都'),
			_Utils_Tuple2('と', '戸'),
			_Utils_Tuple2('と', 'と'),
			_Utils_Tuple2('ど', '度'),
			_Utils_Tuple2('ど', 'ド'),
			_Utils_Tuple2('ど', '度'),
			_Utils_Tuple2('とい', '問い'),
			_Utils_Tuple2('といあわせ', '問い合わせ'),
			_Utils_Tuple2('といあわせる', '問い合わせる'),
			_Utils_Tuple2('といれっと', 'トイレット'),
			_Utils_Tuple2('とう', '島'),
			_Utils_Tuple2('とう', '頭'),
			_Utils_Tuple2('とう', '等'),
			_Utils_Tuple2('とう', '問う'),
			_Utils_Tuple2('とう', '当'),
			_Utils_Tuple2('とう', '等'),
			_Utils_Tuple2('とう', '島'),
			_Utils_Tuple2('どう', '堂'),
			_Utils_Tuple2('どう', '同'),
			_Utils_Tuple2('どう', '動'),
			_Utils_Tuple2('どう', '銅'),
			_Utils_Tuple2('どう', '堂'),
			_Utils_Tuple2('どう', '胴'),
			_Utils_Tuple2('どう', '動'),
			_Utils_Tuple2('とうあん', '答案'),
			_Utils_Tuple2('どうい', '同意'),
			_Utils_Tuple2('どういう', 'どういう'),
			_Utils_Tuple2('とういつ', '統一'),
			_Utils_Tuple2('どういつ', '同一'),
			_Utils_Tuple2('どういつ', '同一'),
			_Utils_Tuple2('どうおん', '同音'),
			_Utils_Tuple2('どうか', 'どうか'),
			_Utils_Tuple2('どうがく', '同額'),
			_Utils_Tuple2('とうがらし', '唐辛子'),
			_Utils_Tuple2('どうかん', '同感'),
			_Utils_Tuple2('とうき', '冬季'),
			_Utils_Tuple2('どうき', '同期'),
			_Utils_Tuple2('どうき', '動機'),
			_Utils_Tuple2('どうき', '同期'),
			_Utils_Tuple2('どうきゅう', '同級'),
			_Utils_Tuple2('どうきゅうせい', '同級生'),
			_Utils_Tuple2('どうきょ', '同居'),
			_Utils_Tuple2('どうぎょう', '同業'),
			_Utils_Tuple2('どうぐ', '道具'),
			_Utils_Tuple2('とうけい', '統計'),
			_Utils_Tuple2('とうこう', '登校'),
			_Utils_Tuple2('どうこう', '同行'),
			_Utils_Tuple2('どうさ', '動作'),
			_Utils_Tuple2('とうざい', '東西'),
			_Utils_Tuple2('とうさん', '倒産'),
			_Utils_Tuple2('とうじ', '当時'),
			_Utils_Tuple2('どうし', '同士'),
			_Utils_Tuple2('どうし', '同士'),
			_Utils_Tuple2('どうじ', '同時'),
			_Utils_Tuple2('どうじだい', '同時代'),
			_Utils_Tuple2('とうじつ', '当日'),
			_Utils_Tuple2('どうしつ', '同質'),
			_Utils_Tuple2('どうじつ', '同日'),
			_Utils_Tuple2('どうじに', '同時に'),
			_Utils_Tuple2('とうしゃ', '当社'),
			_Utils_Tuple2('どうしゅ', '同種'),
			_Utils_Tuple2('とうしょ', '当初'),
			_Utils_Tuple2('とうじょう', '搭乗'),
			_Utils_Tuple2('とうじょう', '登場'),
			_Utils_Tuple2('どうじょう', '同乗'),
			_Utils_Tuple2('どうじょう', '同情'),
			_Utils_Tuple2('どうじょう', '同上'),
			_Utils_Tuple2('どうしょくぶつ', '動植物'),
			_Utils_Tuple2('どうせ', 'どうせ'),
			_Utils_Tuple2('どうせい', '同性'),
			_Utils_Tuple2('どうせい', '同姓'),
			_Utils_Tuple2('とうせん', '当選'),
			_Utils_Tuple2('とうぜん', '当然'),
			_Utils_Tuple2('とうぜん', '当然'),
			_Utils_Tuple2('どうぜん', '同然'),
			_Utils_Tuple2('とうそう', '逃走'),
			_Utils_Tuple2('どうそうかい', '同窓会'),
			_Utils_Tuple2('どうぞよろしく', 'どうぞよろしく'),
			_Utils_Tuple2('とうだい', '灯台'),
			_Utils_Tuple2('とうちゃく', '到着'),
			_Utils_Tuple2('どうでも', 'どうでも'),
			_Utils_Tuple2('とうてん', '当店'),
			_Utils_Tuple2('どうてん', '同点'),
			_Utils_Tuple2('とうとう', '到頭'),
			_Utils_Tuple2('どうとう', '同等'),
			_Utils_Tuple2('どうどう', '堂々'),
			_Utils_Tuple2('どうとく', '道徳'),
			_Utils_Tuple2('どうとくてき', '道徳的'),
			_Utils_Tuple2('とうなん', '東南'),
			_Utils_Tuple2('とうなん', '盗難'),
			_Utils_Tuple2('とうなんあじあ', '東南アジア'),
			_Utils_Tuple2('どうにか', 'どうにか'),
			_Utils_Tuple2('とうにゅう', '豆乳'),
			_Utils_Tuple2('どうにゅう', '導入'),
			_Utils_Tuple2('どうねん', '同年'),
			_Utils_Tuple2('とうばん', '当番'),
			_Utils_Tuple2('とうひょう', '投票'),
			_Utils_Tuple2('とうふ', '豆腐'),
			_Utils_Tuple2('とうぶ', '東部'),
			_Utils_Tuple2('とうぶ', '頭部'),
			_Utils_Tuple2('どうふう', '同封'),
			_Utils_Tuple2('どうぶつせい', '動物性'),
			_Utils_Tuple2('とうぶん', '等分'),
			_Utils_Tuple2('とうぶん', '当分'),
			_Utils_Tuple2('とうほう', '東方'),
			_Utils_Tuple2('とうほく', '東北'),
			_Utils_Tuple2('とうほく', '東北'),
			_Utils_Tuple2('とうほくちほう', '東北地方'),
			_Utils_Tuple2('とうめい', '透明'),
			_Utils_Tuple2('とうめい', '透明'),
			_Utils_Tuple2('どうめい', '同名'),
			_Utils_Tuple2('どうめだる', '銅メダル'),
			_Utils_Tuple2('とうよう', '東洋'),
			_Utils_Tuple2('どうよう', '同様'),
			_Utils_Tuple2('どうりょう', '同僚'),
			_Utils_Tuple2('どうりょう', '同量'),
			_Utils_Tuple2('どうるい', '同類'),
			_Utils_Tuple2('どうろ', '道路'),
			_Utils_Tuple2('とうろく', '登録'),
			_Utils_Tuple2('とうろん', '討論'),
			_Utils_Tuple2('どうわ', '童話'),
			_Utils_Tuple2('とえい', '都営'),
			_Utils_Tuple2('とおす', '通す'),
			_Utils_Tuple2('とーすたー', 'トースター'),
			_Utils_Tuple2('とーたる', 'トータル'),
			_Utils_Tuple2('とーなめんと', 'トーナメント'),
			_Utils_Tuple2('とおまわり', '遠回り'),
			_Utils_Tuple2('どーむ', 'ドーム'),
			_Utils_Tuple2('とおり', '通り'),
			_Utils_Tuple2('とおり', '通り'),
			_Utils_Tuple2('とおりかかる', '通り掛かる'),
			_Utils_Tuple2('とおりすぎる', '通り過ぎる'),
			_Utils_Tuple2('とおりぬけ', '通り抜け'),
			_Utils_Tuple2('とおりぬける', '通り抜ける'),
			_Utils_Tuple2('とおる', '通る'),
			_Utils_Tuple2('とかい', '都会'),
			_Utils_Tuple2('とかす', '溶かす'),
			_Utils_Tuple2('どきどき', 'どきどき'),
			_Utils_Tuple2('ときに', '時に'),
			_Utils_Tuple2('ときには', '時には'),
			_Utils_Tuple2('どきゅめんたりー', 'ドキュメンタリー'),
			_Utils_Tuple2('とく', '溶く'),
			_Utils_Tuple2('とく', '解く'),
			_Utils_Tuple2('とく', '得'),
			_Utils_Tuple2('どく', '独'),
			_Utils_Tuple2('どく', 'どく'),
			_Utils_Tuple2('どく', '毒'),
			_Utils_Tuple2('とくい', '得意'),
			_Utils_Tuple2('どくがく', '独学'),
			_Utils_Tuple2('とくぎ', '特技'),
			_Utils_Tuple2('とくさん', '特産'),
			_Utils_Tuple2('どくしゃ', '読者'),
			_Utils_Tuple2('とくしゅう', '特集'),
			_Utils_Tuple2('どくしょ', '読書'),
			_Utils_Tuple2('とくしょく', '特色'),
			_Utils_Tuple2('どくしん', '独身'),
			_Utils_Tuple2('とくする', '得する'),
			_Utils_Tuple2('とくせい', '特性'),
			_Utils_Tuple2('とくせい', '特製'),
			_Utils_Tuple2('どくたー', 'ドクター'),
			_Utils_Tuple2('とくだい', '特大'),
			_Utils_Tuple2('とくちょう', '特徴'),
			_Utils_Tuple2('とくちょう', '特長'),
			_Utils_Tuple2('とくてい', '特定'),
			_Utils_Tuple2('とくてん', '得点'),
			_Utils_Tuple2('どくとく', '独特'),
			_Utils_Tuple2('とくに', '特に'),
			_Utils_Tuple2('とくばい', '特売'),
			_Utils_Tuple2('とくべつ', '特別'),
			_Utils_Tuple2('どくりつ', '独立'),
			_Utils_Tuple2('とけいだい', '時計台'),
			_Utils_Tuple2('とける', '溶ける'),
			_Utils_Tuple2('とける', '解ける'),
			_Utils_Tuple2('とこや', '床屋'),
			_Utils_Tuple2('ところが', 'ところが'),
			_Utils_Tuple2('ところで', 'ところで'),
			_Utils_Tuple2('ところどころ', '所々'),
			_Utils_Tuple2('とざん', '登山'),
			_Utils_Tuple2('とし', '都市'),
			_Utils_Tuple2('としうえ', '年上'),
			_Utils_Tuple2('としこし', '年越し'),
			_Utils_Tuple2('とじこめる', '閉じ込める'),
			_Utils_Tuple2('としした', '年下'),
			_Utils_Tuple2('としつき', '年月'),
			_Utils_Tuple2('としとる', '年取る'),
			_Utils_Tuple2('としのくれ', '年の暮れ'),
			_Utils_Tuple2('とじまり', '戸締まり'),
			_Utils_Tuple2('としょ', '図書'),
			_Utils_Tuple2('としより', '年寄り'),
			_Utils_Tuple2('とじる', '閉じる'),
			_Utils_Tuple2('としん', '都心'),
			_Utils_Tuple2('どそく', '土足'),
			_Utils_Tuple2('とたん', '途端'),
			_Utils_Tuple2('とち', '土地'),
			_Utils_Tuple2('とちじ', '都知事'),
			_Utils_Tuple2('とちゅう', '途中'),
			_Utils_Tuple2('とちょう', '都庁'),
			_Utils_Tuple2('どっかい', '読解'),
			_Utils_Tuple2('とっきゅう', '特急'),
			_Utils_Tuple2('とっくん', '特訓'),
			_Utils_Tuple2('とつぜん', '突然'),
			_Utils_Tuple2('とって', '取っ手'),
			_Utils_Tuple2('どっと', 'ドット'),
			_Utils_Tuple2('どっと', 'どっと'),
			_Utils_Tuple2('とっぷ', 'トップ'),
			_Utils_Tuple2('とどけ', '届け'),
			_Utils_Tuple2('とどけで', '届け出'),
			_Utils_Tuple2('とどけでる', '届け出る'),
			_Utils_Tuple2('とどける', '届ける'),
			_Utils_Tuple2('ととのう', '整う'),
			_Utils_Tuple2('ととのえる', '整える'),
			_Utils_Tuple2('とない', '都内'),
			_Utils_Tuple2('どなる', '怒鳴る'),
			_Utils_Tuple2('とにかく', 'とにかく'),
			_Utils_Tuple2('どの', '殿'),
			_Utils_Tuple2('とばす', '飛ばす'),
			_Utils_Tuple2('とびあがる', '飛び上がる'),
			_Utils_Tuple2('とびおきる', '飛び起きる'),
			_Utils_Tuple2('とびおりる', '飛び降りる'),
			_Utils_Tuple2('とびこみ', '飛び込み'),
			_Utils_Tuple2('とびこむ', '飛び込む'),
			_Utils_Tuple2('とびだし', '飛び出し'),
			_Utils_Tuple2('とびだす', '飛び出す'),
			_Utils_Tuple2('とぴっく', 'トピック'),
			_Utils_Tuple2('とびでる', '飛び出る'),
			_Utils_Tuple2('とびのる', '飛び乗る'),
			_Utils_Tuple2('とびまわる', '飛び回る'),
			_Utils_Tuple2('とびら', '扉'),
			_Utils_Tuple2('とぶ', '跳ぶ'),
			_Utils_Tuple2('とべい', '渡米'),
			_Utils_Tuple2('とほ', '徒歩'),
			_Utils_Tuple2('とまり', '泊まり'),
			_Utils_Tuple2('とまり', '止まり'),
			_Utils_Tuple2('とまる', '泊まる'),
			_Utils_Tuple2('とまる', '留まる'),
			_Utils_Tuple2('とめ', '止め'),
			_Utils_Tuple2('とめる', '止める'),
			_Utils_Tuple2('とめる', '泊める'),
			_Utils_Tuple2('とめる', '留める'),
			_Utils_Tuple2('とも', '友'),
			_Utils_Tuple2('とも', '共'),
			_Utils_Tuple2('とも', '共'),
			_Utils_Tuple2('ども', '共'),
			_Utils_Tuple2('ともかせぎ', '共稼ぎ'),
			_Utils_Tuple2('ともなう', '伴う'),
			_Utils_Tuple2('ともに', '共に'),
			_Utils_Tuple2('ともばたらき', '共働き'),
			_Utils_Tuple2('とら', '虎'),
			_Utils_Tuple2('とら', 'とら'),
			_Utils_Tuple2('とらい', 'トライ'),
			_Utils_Tuple2('どらい', 'ドライ'),
			_Utils_Tuple2('どらいあいす', 'ドライアイス'),
			_Utils_Tuple2('どらいばー', 'ドライバー'),
			_Utils_Tuple2('どらいぶ', 'ドライブ'),
			_Utils_Tuple2('どらいやー', 'ドライヤー'),
			_Utils_Tuple2('どらごん', 'ドラゴン'),
			_Utils_Tuple2('とらぶる', 'トラブル'),
			_Utils_Tuple2('とらべらー', 'トラベラー'),
			_Utils_Tuple2('どらま', 'ドラマ'),
			_Utils_Tuple2('どらまちっく', 'ドラマチック'),
			_Utils_Tuple2('どらむ', 'ドラム'),
			_Utils_Tuple2('とらんく', 'トランク'),
			_Utils_Tuple2('とらんくす', 'トランクス'),
			_Utils_Tuple2('とらんぺっと', 'トランペット'),
			_Utils_Tuple2('とり', '取り'),
			_Utils_Tuple2('どり', '取り'),
			_Utils_Tuple2('どりあ', 'ドリア'),
			_Utils_Tuple2('とりあい', '取り合い'),
			_Utils_Tuple2('とりあう', '取り合う'),
			_Utils_Tuple2('とりあえず', 'とりあえず'),
			_Utils_Tuple2('とりあげる', '取り上げる'),
			_Utils_Tuple2('とりい', '鳥居'),
			_Utils_Tuple2('とりーとめんと', 'トリートメント'),
			_Utils_Tuple2('どりーむ', 'ドリーム'),
			_Utils_Tuple2('とりいれる', '取り入れる'),
			_Utils_Tuple2('とりかえす', '取り返す'),
			_Utils_Tuple2('とりかえる', '取り替える'),
			_Utils_Tuple2('とりかこむ', '取り囲む'),
			_Utils_Tuple2('とりくむ', '取り組む'),
			_Utils_Tuple2('とりけし', '取り消し'),
			_Utils_Tuple2('とりけす', '取り消す'),
			_Utils_Tuple2('とりこむ', '取り込む'),
			_Utils_Tuple2('とりざら', '取り皿'),
			_Utils_Tuple2('とりだす', '取り出す'),
			_Utils_Tuple2('とりつ', '都立'),
			_Utils_Tuple2('とりっく', 'トリック'),
			_Utils_Tuple2('とりはずし', '取り外し'),
			_Utils_Tuple2('とりひき', '取り引き'),
			_Utils_Tuple2('とりぷる', 'トリプル'),
			_Utils_Tuple2('とりもどす', '取り戻す'),
			_Utils_Tuple2('どりょく', '努力'),
			_Utils_Tuple2('どりんく', 'ドリンク'),
			_Utils_Tuple2('とる', '捕る'),
			_Utils_Tuple2('とる', '採る'),
			_Utils_Tuple2('とれー', 'トレー'),
			_Utils_Tuple2('とれーなー', 'トレーナー'),
			_Utils_Tuple2('とれーにんぐ', 'トレーニング'),
			_Utils_Tuple2('どれす', 'ドレス'),
			_Utils_Tuple2('どれすあっぷ', 'ドレスアップ'),
			_Utils_Tuple2('どれっしんぐ', 'ドレッシング'),
			_Utils_Tuple2('とれる', '取れる'),
			_Utils_Tuple2('どろ', '泥'),
			_Utils_Tuple2('どろだらけ', '泥だらけ'),
			_Utils_Tuple2('どろっぷ', 'ドロップ'),
			_Utils_Tuple2('どろぼう', '泥棒'),
			_Utils_Tuple2('とん', 'トン'),
			_Utils_Tuple2('とん', '豚'),
			_Utils_Tuple2('どん', '丼'),
			_Utils_Tuple2('とんかつ', '豚カツ'),
			_Utils_Tuple2('とんじる', '豚汁'),
			_Utils_Tuple2('とんとん', 'とんとん'),
			_Utils_Tuple2('どんどん', 'どんどん'),
			_Utils_Tuple2('とんねる', 'トンネル'),
			_Utils_Tuple2('どんぶり', '丼'),
			_Utils_Tuple2('どんぶりもの', '丼物'),
			_Utils_Tuple2('とんぼ', 'とんぼ'),
			_Utils_Tuple2('な', '名'),
			_Utils_Tuple2('な', '菜'),
			_Utils_Tuple2('なあ', 'なあ'),
			_Utils_Tuple2('ない', '内'),
			_Utils_Tuple2('ないか', '内科'),
			_Utils_Tuple2('ないがい', '内外'),
			_Utils_Tuple2('ないしょ', '内緒'),
			_Utils_Tuple2('ないす', 'ナイス'),
			_Utils_Tuple2('ないせん', '内線'),
			_Utils_Tuple2('ないせん', '内戦'),
			_Utils_Tuple2('ないぶ', '内部'),
			_Utils_Tuple2('ないめん', '内面'),
			_Utils_Tuple2('ないよう', '内容'),
			_Utils_Tuple2('ないろん', 'ナイロン'),
			_Utils_Tuple2('なお', 'なお'),
			_Utils_Tuple2('なお', 'なお'),
			_Utils_Tuple2('なおさら', 'なおさら'),
			_Utils_Tuple2('なおし', '直し'),
			_Utils_Tuple2('なおす', '直す'),
			_Utils_Tuple2('なおす', '治す'),
			_Utils_Tuple2('なおる', '治る'),
			_Utils_Tuple2('なおる', '直る'),
			_Utils_Tuple2('なか', '仲'),
			_Utils_Tuple2('ながあめ', '長雨'),
			_Utils_Tuple2('ながいき', '長生き'),
			_Utils_Tuple2('ながぐつ', '長靴'),
			_Utils_Tuple2('なかごろ', '中ごろ'),
			_Utils_Tuple2('ながし', '流し'),
			_Utils_Tuple2('ながしだい', '流し台'),
			_Utils_Tuple2('なかす', '泣かす'),
			_Utils_Tuple2('ながす', '流す'),
			_Utils_Tuple2('ながそで', '長袖'),
			_Utils_Tuple2('ながたび', '長旅'),
			_Utils_Tuple2('なかでも', '中でも'),
			_Utils_Tuple2('なかでも', '中でも'),
			_Utils_Tuple2('なかなおり', '仲直り'),
			_Utils_Tuple2('なかなか', '中々'),
			_Utils_Tuple2('なかには', '中には'),
			_Utils_Tuple2('なかにわ', '中庭'),
			_Utils_Tuple2('ながねん', '長年'),
			_Utils_Tuple2('なかば', '半ば'),
			_Utils_Tuple2('ながびく', '長引く'),
			_Utils_Tuple2('なかほど', '中程'),
			_Utils_Tuple2('なかま', '仲間'),
			_Utils_Tuple2('なかまいり', '仲間入り'),
			_Utils_Tuple2('なかまはずれ', '仲間外れ'),
			_Utils_Tuple2('なかみ', '中身'),
			_Utils_Tuple2('ながめ', '眺め'),
			_Utils_Tuple2('ながめ', '長め'),
			_Utils_Tuple2('ながめる', '眺める'),
			_Utils_Tuple2('なかやすみ', '中休み'),
			_Utils_Tuple2('なかゆび', '中指'),
			_Utils_Tuple2('なかよし', '仲良し'),
			_Utils_Tuple2('ながれ', '流れ'),
			_Utils_Tuple2('ながれこむ', '流れ込む'),
			_Utils_Tuple2('ながれる', '流れる'),
			_Utils_Tuple2('なき', '泣き'),
			_Utils_Tuple2('なきごえ', '鳴き声'),
			_Utils_Tuple2('なきごえ', '泣き声'),
			_Utils_Tuple2('なきさけぶ', '泣き叫ぶ'),
			_Utils_Tuple2('なく', '泣く'),
			_Utils_Tuple2('なく', '鳴く'),
			_Utils_Tuple2('なぐさめる', '慰める'),
			_Utils_Tuple2('なくす', '無くす'),
			_Utils_Tuple2('なくす', '亡くす'),
			_Utils_Tuple2('なくなる', '無くなる'),
			_Utils_Tuple2('なくなる', '亡くなる'),
			_Utils_Tuple2('なぐる', '殴る'),
			_Utils_Tuple2('なげ', '投げ'),
			_Utils_Tuple2('なげいれる', '投げ入れる'),
			_Utils_Tuple2('なげこむ', '投げ込む'),
			_Utils_Tuple2('なげすてる', '投げ捨てる'),
			_Utils_Tuple2('なげとばす', '投げ飛ばす'),
			_Utils_Tuple2('なげる', '投げる'),
			_Utils_Tuple2('なさる', 'なさる'),
			_Utils_Tuple2('なし', '無し'),
			_Utils_Tuple2('なし', '梨'),
			_Utils_Tuple2('なしょなる', 'ナショナル'),
			_Utils_Tuple2('なす', 'なす'),
			_Utils_Tuple2('なす', '成す'),
			_Utils_Tuple2('なぞ', '謎'),
			_Utils_Tuple2('なぞとき', '謎解き'),
			_Utils_Tuple2('なちゅらる', 'ナチュラル'),
			_Utils_Tuple2('なつかしい', '懐かしい'),
			_Utils_Tuple2('なつかしむ', '懐かしむ'),
			_Utils_Tuple2('なづける', '名付ける'),
			_Utils_Tuple2('なつじかん', '夏時間'),
			_Utils_Tuple2('なっとう', '納豆'),
			_Utils_Tuple2('なっとく', '納得'),
			_Utils_Tuple2('なつば', '夏場'),
			_Utils_Tuple2('なつまつり', '夏祭り'),
			_Utils_Tuple2('なつやせ', '夏痩せ'),
			_Utils_Tuple2('なでる', 'なでる'),
			_Utils_Tuple2('ななめ', '斜め'),
			_Utils_Tuple2('なになに', '何々'),
			_Utils_Tuple2('なにもの', '何者'),
			_Utils_Tuple2('なにより', '何より'),
			_Utils_Tuple2('なのはな', '菜の花'),
			_Utils_Tuple2('なぷきん', 'ナプキン'),
			_Utils_Tuple2('なふだ', '名札'),
			_Utils_Tuple2('なべ', '鍋'),
			_Utils_Tuple2('なべもの', '鍋物'),
			_Utils_Tuple2('なべりょうり', '鍋料理'),
			_Utils_Tuple2('なま', '生'),
			_Utils_Tuple2('なま', '生'),
			_Utils_Tuple2('なまいき', '生意気'),
			_Utils_Tuple2('なまくりーむ', '生クリーム'),
			_Utils_Tuple2('なまける', '怠ける'),
			_Utils_Tuple2('なまたまご', '生卵'),
			_Utils_Tuple2('なまちゅうけい', '生中継'),
			_Utils_Tuple2('なまびーる', '生ビール'),
			_Utils_Tuple2('なまほうそう', '生放送'),
			_Utils_Tuple2('なまみず', '生水'),
			_Utils_Tuple2('なまもの', '生物'),
			_Utils_Tuple2('なみ', '波'),
			_Utils_Tuple2('なみ', '並み'),
			_Utils_Tuple2('なみ', '並み'),
			_Utils_Tuple2('なみき', '並木'),
			_Utils_Tuple2('なみだ', '涙'),
			_Utils_Tuple2('なやみ', '悩み'),
			_Utils_Tuple2('なやむ', '悩む'),
			_Utils_Tuple2('ならい', '習い'),
			_Utils_Tuple2('ならいごと', '習い事'),
			_Utils_Tuple2('ならう', '習う'),
			_Utils_Tuple2('ならす', '鳴らす'),
			_Utils_Tuple2('ならび', '並び'),
			_Utils_Tuple2('ならぶ', '並ぶ'),
			_Utils_Tuple2('ならべる', '並べる'),
			_Utils_Tuple2('ならわす', '習わす'),
			_Utils_Tuple2('なる', '鳴る'),
			_Utils_Tuple2('なるべく', 'なるべく'),
			_Utils_Tuple2('なるほど', '成る程'),
			_Utils_Tuple2('なれ', '慣れ'),
			_Utils_Tuple2('なれーしょん', 'ナレーション'),
			_Utils_Tuple2('なれる', '慣れる'),
			_Utils_Tuple2('なんきょく', '南極'),
			_Utils_Tuple2('なんごく', '南国'),
			_Utils_Tuple2('なんせい', '南西'),
			_Utils_Tuple2('なんだ', '何だ'),
			_Utils_Tuple2('なんだい', '難題'),
			_Utils_Tuple2('なんだか', '何だか'),
			_Utils_Tuple2('なんと', '何と'),
			_Utils_Tuple2('なんとう', '南東'),
			_Utils_Tuple2('なんとか', '何とか'),
			_Utils_Tuple2('なんとも', '何とも'),
			_Utils_Tuple2('なんばー', 'ナンバー'),
			_Utils_Tuple2('なんばーわん', 'ナンバーワン'),
			_Utils_Tuple2('なんぶ', '南部'),
			_Utils_Tuple2('なんぽう', '南方'),
			_Utils_Tuple2('なんぼく', '南北'),
			_Utils_Tuple2('なんみん', '難民'),
			_Utils_Tuple2('に', '荷'),
			_Utils_Tuple2('にあい', '似合い'),
			_Utils_Tuple2('にあう', '似合う'),
			_Utils_Tuple2('にーず', 'ニーズ'),
			_Utils_Tuple2('にえる', '煮える'),
			_Utils_Tuple2('におい', '匂い'),
			_Utils_Tuple2('におう', '匂う'),
			_Utils_Tuple2('にがおえ', '似顔絵'),
			_Utils_Tuple2('にがす', '逃がす'),
			_Utils_Tuple2('にがて', '苦手'),
			_Utils_Tuple2('にがみ', '苦味'),
			_Utils_Tuple2('にぎりしめる', '握り締める'),
			_Utils_Tuple2('にぎる', '握る'),
			_Utils_Tuple2('にぎわう', 'にぎわう'),
			_Utils_Tuple2('にくい', 'にくい'),
			_Utils_Tuple2('にくい', '憎い'),
			_Utils_Tuple2('にくがん', '肉眼'),
			_Utils_Tuple2('にくしょく', '肉食'),
			_Utils_Tuple2('にくしん', '肉親'),
			_Utils_Tuple2('にくたい', '肉体'),
			_Utils_Tuple2('にくたいてき', '肉体的'),
			_Utils_Tuple2('にくまん', '肉まん'),
			_Utils_Tuple2('にくむ', '憎む'),
			_Utils_Tuple2('にくるい', '肉類'),
			_Utils_Tuple2('にげ', '逃げ'),
			_Utils_Tuple2('にげまわる', '逃げ回る'),
			_Utils_Tuple2('にげる', '逃げる'),
			_Utils_Tuple2('にこにこ', 'にこにこ'),
			_Utils_Tuple2('にこやか', 'にこやか'),
			_Utils_Tuple2('にこり', 'にこり'),
			_Utils_Tuple2('にさんかたんそ', '二酸化炭素'),
			_Utils_Tuple2('にじ', '虹'),
			_Utils_Tuple2('にじ', '二次'),
			_Utils_Tuple2('にしがわ', '西側'),
			_Utils_Tuple2('にじゅう', '二重'),
			_Utils_Tuple2('にせ', '偽'),
			_Utils_Tuple2('にせさつ', '偽札'),
			_Utils_Tuple2('にせもの', '偽物'),
			_Utils_Tuple2('にだん', '二段'),
			_Utils_Tuple2('にだんめ', '二段目'),
			_Utils_Tuple2('にちあたり', '日当たり'),
			_Utils_Tuple2('にちじ', '日時'),
			_Utils_Tuple2('にちじょう', '日常'),
			_Utils_Tuple2('にちじょうせい', '日常性'),
			_Utils_Tuple2('にちべい', '日米'),
			_Utils_Tuple2('にちぼつ', '日没'),
			_Utils_Tuple2('にちよう', '日用'),
			_Utils_Tuple2('にちようひん', '日用品'),
			_Utils_Tuple2('にっか', '日課'),
			_Utils_Tuple2('にっかん', '日刊'),
			_Utils_Tuple2('にっき', '日記'),
			_Utils_Tuple2('にっきゅう', '日給'),
			_Utils_Tuple2('にっくねーむ', 'ニックネーム'),
			_Utils_Tuple2('にっけい', '日系'),
			_Utils_Tuple2('にっこう', '日光'),
			_Utils_Tuple2('にっすう', '日数'),
			_Utils_Tuple2('にっちゅう', '日中'),
			_Utils_Tuple2('にってい', '日程'),
			_Utils_Tuple2('にっと', 'ニット'),
			_Utils_Tuple2('にどと', '二度と'),
			_Utils_Tuple2('にぶ', '二部'),
			_Utils_Tuple2('にぶい', '鈍い'),
			_Utils_Tuple2('にぶん', '二分'),
			_Utils_Tuple2('にまいめ', '二枚目'),
			_Utils_Tuple2('にもの', '煮物'),
			_Utils_Tuple2('にゅー', 'ニュー'),
			_Utils_Tuple2('にゅう', '乳'),
			_Utils_Tuple2('にゅう', '乳'),
			_Utils_Tuple2('にゅういん', '入院'),
			_Utils_Tuple2('にゅうえん', '入園'),
			_Utils_Tuple2('にゅうかい', '入会'),
			_Utils_Tuple2('にゅうがく', '入学'),
			_Utils_Tuple2('にゅうがくきん', '入学金'),
			_Utils_Tuple2('にゅうがくしき', '入学式'),
			_Utils_Tuple2('にゅうがくしけん', '入学試験'),
			_Utils_Tuple2('にゅうかん', '入館'),
			_Utils_Tuple2('にゅうきょ', '入居'),
			_Utils_Tuple2('にゅうきん', '入金'),
			_Utils_Tuple2('にゅうこく', '入国'),
			_Utils_Tuple2('にゅうこくかんり', '入国管理'),
			_Utils_Tuple2('にゅうし', '入試'),
			_Utils_Tuple2('にゅうしつ', '入室'),
			_Utils_Tuple2('にゅうしゃ', '入社'),
			_Utils_Tuple2('にゅうじょう', '入場'),
			_Utils_Tuple2('にゅーすきゃすたー', 'ニュースキャスター'),
			_Utils_Tuple2('にゅうせいひん', '乳製品'),
			_Utils_Tuple2('にゅうぶ', '入部'),
			_Utils_Tuple2('にゅうよく', '入浴'),
			_Utils_Tuple2('にゅうりょう', '入寮'),
			_Utils_Tuple2('にゅうりょく', '入力'),
			_Utils_Tuple2('にょうぼう', '女房'),
			_Utils_Tuple2('にらむ', 'にらむ'),
			_Utils_Tuple2('にる', '似る'),
			_Utils_Tuple2('にる', '煮る'),
			_Utils_Tuple2('にわとり', '鶏'),
			_Utils_Tuple2('にんき', '人気'),
			_Utils_Tuple2('にんげん', '人間'),
			_Utils_Tuple2('にんげんかんけい', '人間関係'),
			_Utils_Tuple2('にんげんせい', '人間性'),
			_Utils_Tuple2('にんげんてき', '人間的'),
			_Utils_Tuple2('にんじゃ', '忍者'),
			_Utils_Tuple2('にんずう', '人数'),
			_Utils_Tuple2('にんにく', 'にんにく'),
			_Utils_Tuple2('にんぷ', '妊婦'),
			_Utils_Tuple2('ぬいぐるみ', 'ぬいぐるみ'),
			_Utils_Tuple2('ぬう', '縫う'),
			_Utils_Tuple2('ぬーどる', 'ヌードル'),
			_Utils_Tuple2('ぬき', '抜き'),
			_Utils_Tuple2('ぬく', '抜く'),
			_Utils_Tuple2('ぬくもり', '温もり'),
			_Utils_Tuple2('ぬける', '抜ける'),
			_Utils_Tuple2('ぬすむ', '盗む'),
			_Utils_Tuple2('ぬの', '布'),
			_Utils_Tuple2('ぬりぐすり', '塗り薬'),
			_Utils_Tuple2('ぬる', '塗る'),
			_Utils_Tuple2('ぬれる', 'ぬれる'),
			_Utils_Tuple2('ね', '根'),
			_Utils_Tuple2('ね', '寝'),
			_Utils_Tuple2('ねあがり', '値上がり'),
			_Utils_Tuple2('ねあげ', '値上げ'),
			_Utils_Tuple2('ねえ', 'ねえ'),
			_Utils_Tuple2('ねーみんぐ', 'ネーミング'),
			_Utils_Tuple2('ねーむ', 'ネーム'),
			_Utils_Tuple2('ねおき', '寝起き'),
			_Utils_Tuple2('ねがい', '願い'),
			_Utils_Tuple2('ねがいごと', '願い事'),
			_Utils_Tuple2('ねがう', '願う'),
			_Utils_Tuple2('ねがお', '寝顔'),
			_Utils_Tuple2('ねかす', '寝かす'),
			_Utils_Tuple2('ねかせる', '寝かせる'),
			_Utils_Tuple2('ねぎ', 'ねぎ'),
			_Utils_Tuple2('ねぎる', '値切る'),
			_Utils_Tuple2('ねさがり', '値下がり'),
			_Utils_Tuple2('ねさげ', '値下げ'),
			_Utils_Tuple2('ねずみ', 'ねずみ'),
			_Utils_Tuple2('ねつい', '熱意'),
			_Utils_Tuple2('ねっき', '熱気'),
			_Utils_Tuple2('ねっくれす', 'ネックレス'),
			_Utils_Tuple2('ねっしん', '熱心'),
			_Utils_Tuple2('ねっしん', '熱心'),
			_Utils_Tuple2('ねっちゅう', '熱中'),
			_Utils_Tuple2('ねっと', 'ネット'),
			_Utils_Tuple2('ねっとわーく', 'ネットワーク'),
			_Utils_Tuple2('ねっぷう', '熱風'),
			_Utils_Tuple2('ねばねば', 'ねばねば'),
			_Utils_Tuple2('ねびき', '値引き'),
			_Utils_Tuple2('ねふだ', '値札'),
			_Utils_Tuple2('ねぼう', '寝坊'),
			_Utils_Tuple2('ねむけ', '眠気'),
			_Utils_Tuple2('ねむり', '眠り'),
			_Utils_Tuple2('ねむりこむ', '眠り込む'),
			_Utils_Tuple2('ねむる', '眠る'),
			_Utils_Tuple2('ねもと', '根元'),
			_Utils_Tuple2('ねらう', '狙う'),
			_Utils_Tuple2('ねんがく', '年額'),
			_Utils_Tuple2('ねんがじょう', '年賀状'),
			_Utils_Tuple2('ねんがっぴ', '年月日'),
			_Utils_Tuple2('ねんかん', '年間'),
			_Utils_Tuple2('ねんかん', '年間'),
			_Utils_Tuple2('ねんきゅう', '年休'),
			_Utils_Tuple2('ねんきん', '年金'),
			_Utils_Tuple2('ねんげつ', '年月'),
			_Utils_Tuple2('ねんし', '年始'),
			_Utils_Tuple2('ねんしゅう', '年収'),
			_Utils_Tuple2('ねんじゅう', '年中'),
			_Utils_Tuple2('ねんじゅう', '年中'),
			_Utils_Tuple2('ねんじゅうむきゅう', '年中無休'),
			_Utils_Tuple2('ねんしょう', '年少'),
			_Utils_Tuple2('ねんすう', '年数'),
			_Utils_Tuple2('ねんせい', '年生'),
			_Utils_Tuple2('ねんだい', '年代'),
			_Utils_Tuple2('ねんだい', '年代'),
			_Utils_Tuple2('ねんど', '年度'),
			_Utils_Tuple2('ねんど', '年度'),
			_Utils_Tuple2('ねんない', '年内'),
			_Utils_Tuple2('ねんねん', '年々'),
			_Utils_Tuple2('ねんぱい', '年配'),
			_Utils_Tuple2('ねんぴ', '燃費'),
			_Utils_Tuple2('ねんぴょう', '年表'),
			_Utils_Tuple2('ねんまつ', '年末'),
			_Utils_Tuple2('ねんりょう', '燃料'),
			_Utils_Tuple2('ねんれい', '年齢'),
			_Utils_Tuple2('の', '野'),
			_Utils_Tuple2('のう', '脳'),
			_Utils_Tuple2('のう', '能'),
			_Utils_Tuple2('のう', '農'),
			_Utils_Tuple2('のう', '農'),
			_Utils_Tuple2('のうえん', '農園'),
			_Utils_Tuple2('のうか', '農家'),
			_Utils_Tuple2('のうぎょう', '農業'),
			_Utils_Tuple2('のうさぎょう', '農作業'),
			_Utils_Tuple2('のうさくぶつ', '農作物'),
			_Utils_Tuple2('のうさくもつ', '農作物'),
			_Utils_Tuple2('のうじょう', '農場'),
			_Utils_Tuple2('のうそん', '農村'),
			_Utils_Tuple2('のうはう', 'ノウハウ'),
			_Utils_Tuple2('のうみん', '農民'),
			_Utils_Tuple2('のうりつ', '能率'),
			_Utils_Tuple2('のうりょく', '能力'),
			_Utils_Tuple2('のー', 'ノー'),
			_Utils_Tuple2('のーすりーぶ', 'ノースリーブ'),
			_Utils_Tuple2('のーべるしょう', 'ノーベル賞'),
			_Utils_Tuple2('のーまる', 'ノーマル'),
			_Utils_Tuple2('のこす', '残す'),
			_Utils_Tuple2('のこり', '残り'),
			_Utils_Tuple2('のこりもの', '残り物'),
			_Utils_Tuple2('のこる', '残る'),
			_Utils_Tuple2('のせる', '乗せる'),
			_Utils_Tuple2('のせる', '載せる'),
			_Utils_Tuple2('のぞく', 'のぞく'),
			_Utils_Tuple2('のぞく', '除く'),
			_Utils_Tuple2('のぞみ', '望み'),
			_Utils_Tuple2('のぞむ', '望む'),
			_Utils_Tuple2('のち', '後'),
			_Utils_Tuple2('のちのち', '後々'),
			_Utils_Tuple2('のっく', 'ノック'),
			_Utils_Tuple2('のばす', '伸ばす'),
			_Utils_Tuple2('のばす', '延ばす'),
			_Utils_Tuple2('のび', '伸び'),
			_Utils_Tuple2('のびのび', '伸び伸び'),
			_Utils_Tuple2('のびる', '伸びる'),
			_Utils_Tuple2('のびる', '延びる'),
			_Utils_Tuple2('のべる', '述べる'),
			_Utils_Tuple2('のぼり', '上り'),
			_Utils_Tuple2('のぼり', '登り'),
			_Utils_Tuple2('のぼりおり', '上り下り'),
			_Utils_Tuple2('のぼりざか', '上り坂'),
			_Utils_Tuple2('のぼる', '上る'),
			_Utils_Tuple2('のぼる', '昇る'),
			_Utils_Tuple2('のます', '飲ます'),
			_Utils_Tuple2('のみくい', '飲み食い'),
			_Utils_Tuple2('のみや', '飲み屋'),
			_Utils_Tuple2('のやま', '野山'),
			_Utils_Tuple2('のり', 'のり'),
			_Utils_Tuple2('のり', 'のり'),
			_Utils_Tuple2('のり', '乗り'),
			_Utils_Tuple2('のり', '乗り'),
			_Utils_Tuple2('のりおくれる', '乗り遅れる'),
			_Utils_Tuple2('のりおり', '乗り降り'),
			_Utils_Tuple2('のりかえ', '乗り換え'),
			_Utils_Tuple2('のりかえる', '乗り換える'),
			_Utils_Tuple2('のりこす', '乗り越す'),
			_Utils_Tuple2('のりば', '乗り場'),
			_Utils_Tuple2('のりまき', 'のり巻き'),
			_Utils_Tuple2('のりもの', '乗り物'),
			_Utils_Tuple2('のる', '載る'),
			_Utils_Tuple2('のるま', 'ノルマ'),
			_Utils_Tuple2('のんすとっぷ', 'ノンストップ'),
			_Utils_Tuple2('のんびり', 'のんびり'),
			_Utils_Tuple2('は', '葉'),
			_Utils_Tuple2('ば', '場'),
			_Utils_Tuple2('ば', '馬'),
			_Utils_Tuple2('はあ', 'はあ'),
			_Utils_Tuple2('ばー', 'バー'),
			_Utils_Tuple2('ばあい', '場合'),
			_Utils_Tuple2('ぱーきんぐ', 'パーキング'),
			_Utils_Tuple2('ぱーく', 'パーク'),
			_Utils_Tuple2('ぱーく', 'パーク'),
			_Utils_Tuple2('ばーげん', 'バーゲン'),
			_Utils_Tuple2('ばあさん', 'ばあさん'),
			_Utils_Tuple2('ばーすでー', 'バースデー'),
			_Utils_Tuple2('ぱーせんと', 'パーセント'),
			_Utils_Tuple2('ぱーせんと', 'パーセント'),
			_Utils_Tuple2('ぱーそなるこんぴゅーたー', 'パーソナルコンピューター'),
			_Utils_Tuple2('ばあちゃん', 'ばあちゃん'),
			_Utils_Tuple2('はーと', 'ハート'),
			_Utils_Tuple2('はーど', 'ハード'),
			_Utils_Tuple2('ばーど', 'バード'),
			_Utils_Tuple2('ぱーと', 'パート'),
			_Utils_Tuple2('はーどうぇあ', 'ハードウェア'),
			_Utils_Tuple2('はーどでぃすく', 'ハードディスク'),
			_Utils_Tuple2('ぱーとなー', 'パートナー'),
			_Utils_Tuple2('はーどる', 'ハードル'),
			_Utils_Tuple2('はーふ', 'ハーフ'),
			_Utils_Tuple2('はーぶ', 'ハーブ'),
			_Utils_Tuple2('ぱーふぇくと', 'パーフェクト'),
			_Utils_Tuple2('はーふさいず', 'ハーフサイズ'),
			_Utils_Tuple2('ぱーぷる', 'パープル'),
			_Utils_Tuple2('ばーべきゅー', 'バーベキュー'),
			_Utils_Tuple2('ぱーま', 'パーマ'),
			_Utils_Tuple2('ぱーる', 'パール'),
			_Utils_Tuple2('ばい', '倍'),
			_Utils_Tuple2('ばい', '倍'),
			_Utils_Tuple2('ぱい', 'パイ'),
			_Utils_Tuple2('はいいろ', '灰色'),
			_Utils_Tuple2('はいうえー', 'ハイウエー'),
			_Utils_Tuple2('ばいおりん', 'バイオリン'),
			_Utils_Tuple2('はいきんぐ', 'ハイキング'),
			_Utils_Tuple2('ばいきんぐ', 'バイキング'),
			_Utils_Tuple2('はいく', '俳句'),
			_Utils_Tuple2('はいけん', '拝見'),
			_Utils_Tuple2('はいざら', '灰皿'),
			_Utils_Tuple2('はいそう', '配送'),
			_Utils_Tuple2('はいたつ', '配達'),
			_Utils_Tuple2('はいてく', 'ハイテク'),
			_Utils_Tuple2('ばいてん', '売店'),
			_Utils_Tuple2('はいはい', 'はいはい'),
			_Utils_Tuple2('ばいばい', '売買'),
			_Utils_Tuple2('はいふ', '配布'),
			_Utils_Tuple2('はいゆう', '俳優'),
			_Utils_Tuple2('ばいりんがる', 'バイリンガル'),
			_Utils_Tuple2('ぱいろっと', 'パイロット'),
			_Utils_Tuple2('ぱいん', 'パイン'),
			_Utils_Tuple2('はうす', 'ハウス'),
			_Utils_Tuple2('はえ', 'はえ'),
			_Utils_Tuple2('はえる', '生える'),
			_Utils_Tuple2('はか', '墓'),
			_Utils_Tuple2('ばか', '馬鹿'),
			_Utils_Tuple2('はかせ', '博士'),
			_Utils_Tuple2('はかる', '測る'),
			_Utils_Tuple2('ばかんす', 'バカンス'),
			_Utils_Tuple2('はきけ', '吐き気'),
			_Utils_Tuple2('はきもの', '履物'),
			_Utils_Tuple2('はく', '泊'),
			_Utils_Tuple2('はく', '白'),
			_Utils_Tuple2('はく', '吐く'),
			_Utils_Tuple2('はくさい', '白菜'),
			_Utils_Tuple2('はくし', '白紙'),
			_Utils_Tuple2('はくしゅ', '拍手'),
			_Utils_Tuple2('はくじん', '白人'),
			_Utils_Tuple2('はくせん', '白線'),
			_Utils_Tuple2('ばくだん', '爆弾'),
			_Utils_Tuple2('はくちょう', '白鳥'),
			_Utils_Tuple2('はくはつ', '白髪'),
			_Utils_Tuple2('ばくはつ', '爆発'),
			_Utils_Tuple2('ばくはつてき', '爆発的'),
			_Utils_Tuple2('はくぶつかん', '博物館'),
			_Utils_Tuple2('はげしい', '激しい'),
			_Utils_Tuple2('ばけつ', 'バケツ'),
			_Utils_Tuple2('はげまし', '励まし'),
			_Utils_Tuple2('はげます', '励ます'),
			_Utils_Tuple2('はこ', '箱'),
			_Utils_Tuple2('はこぶ', '運ぶ'),
			_Utils_Tuple2('ばざー', 'バザー'),
			_Utils_Tuple2('はさまる', '挟まる'),
			_Utils_Tuple2('はさみ', 'はさみ'),
			_Utils_Tuple2('はさむ', '挟む'),
			_Utils_Tuple2('はさん', '破産'),
			_Utils_Tuple2('はし', '端'),
			_Utils_Tuple2('はじ', '恥'),
			_Utils_Tuple2('はしおき', '箸置き'),
			_Utils_Tuple2('はしご', 'はしご'),
			_Utils_Tuple2('はじまり', '始まり'),
			_Utils_Tuple2('はじめ', '始め'),
			_Utils_Tuple2('はじめ', '初め'),
			_Utils_Tuple2('ばしゃ', '馬車'),
			_Utils_Tuple2('ぱじゃま', 'パジャマ'),
			_Utils_Tuple2('はしら', '柱'),
			_Utils_Tuple2('はしり', '走り'),
			_Utils_Tuple2('はしりまわる', '走り回る'),
			_Utils_Tuple2('はず', 'はず'),
			_Utils_Tuple2('はずかしい', '恥ずかしい'),
			_Utils_Tuple2('はずす', '外す'),
			_Utils_Tuple2('ぱすた', 'パスタ'),
			_Utils_Tuple2('ばすと', 'バスト'),
			_Utils_Tuple2('ぱずる', 'パズル'),
			_Utils_Tuple2('ばするーむ', 'バスルーム'),
			_Utils_Tuple2('はずれ', '外れ'),
			_Utils_Tuple2('はずれ', '外れ'),
			_Utils_Tuple2('はずれる', '外れる'),
			_Utils_Tuple2('ぱすわーど', 'パスワード'),
			_Utils_Tuple2('はた', '旗'),
			_Utils_Tuple2('はだ', '肌'),
			_Utils_Tuple2('ぱたーん', 'パターン'),
			_Utils_Tuple2('はだいろ', '肌色'),
			_Utils_Tuple2('はだか', '裸'),
			_Utils_Tuple2('はだぎ', '肌着'),
			_Utils_Tuple2('はたけ', '畑'),
			_Utils_Tuple2('はだし', 'はだし'),
			_Utils_Tuple2('はたして', '果たして'),
			_Utils_Tuple2('はたらかす', '働かす'),
			_Utils_Tuple2('はたらき', '働き'),
			_Utils_Tuple2('はたらきもの', '働き者'),
			_Utils_Tuple2('はち', '蜂'),
			_Utils_Tuple2('ぱちぱち', 'ぱちぱち'),
			_Utils_Tuple2('はちみつ', '蜂蜜'),
			_Utils_Tuple2('ぱちんこ', 'パチンコ'),
			_Utils_Tuple2('はつ', '発'),
			_Utils_Tuple2('はつ', '発'),
			_Utils_Tuple2('はつ', '初'),
			_Utils_Tuple2('はつおん', '発音'),
			_Utils_Tuple2('はっきり', 'はっきり'),
			_Utils_Tuple2('ばっきん', '罰金'),
			_Utils_Tuple2('ばっく', 'バック'),
			_Utils_Tuple2('ばっくあっぷ', 'バックアップ'),
			_Utils_Tuple2('ぱっけーじ', 'パッケージ'),
			_Utils_Tuple2('はっけん', '発見'),
			_Utils_Tuple2('はつげん', '発言'),
			_Utils_Tuple2('はつこい', '初恋'),
			_Utils_Tuple2('はっこう', '発行'),
			_Utils_Tuple2('はっさん', '発散'),
			_Utils_Tuple2('はっしゃ', '発車'),
			_Utils_Tuple2('はっしん', '発信'),
			_Utils_Tuple2('はっせい', '発生'),
			_Utils_Tuple2('はっそう', '発想'),
			_Utils_Tuple2('はっそう', '発送'),
			_Utils_Tuple2('はつたいけん', '初体験'),
			_Utils_Tuple2('はったつ', '発達'),
			_Utils_Tuple2('はっちゃく', '発着'),
			_Utils_Tuple2('ばっちり', 'ばっちり'),
			_Utils_Tuple2('はってん', '発展'),
			_Utils_Tuple2('はつでんしょ', '発電所'),
			_Utils_Tuple2('はってんとじょうこく', '発展途上国'),
			_Utils_Tuple2('はっと', 'はっと'),
			_Utils_Tuple2('ぱっと', 'ぱっと'),
			_Utils_Tuple2('はっぱ', '葉っぱ'),
			_Utils_Tuple2('はつばい', '発売'),
			_Utils_Tuple2('はっぴー', 'ハッピー'),
			_Utils_Tuple2('はっぴーえんど', 'ハッピーエンド'),
			_Utils_Tuple2('はっぴょう', '発表'),
			_Utils_Tuple2('はつみみ', '初耳'),
			_Utils_Tuple2('はつめい', '発明'),
			_Utils_Tuple2('はつもうで', '初詣で'),
			_Utils_Tuple2('はつゆき', '初雪'),
			_Utils_Tuple2('はで', '派手'),
			_Utils_Tuple2('はと', 'はと'),
			_Utils_Tuple2('ぱとかー', 'パトカー'),
			_Utils_Tuple2('ばどみんとん', 'バドミントン'),
			_Utils_Tuple2('ぱとろーる', 'パトロール'),
			_Utils_Tuple2('ばとん', 'バトン'),
			_Utils_Tuple2('はながら', '花柄'),
			_Utils_Tuple2('はなしあい', '話し合い'),
			_Utils_Tuple2('はなしあう', '話し合う'),
			_Utils_Tuple2('はなしかける', '話し掛ける'),
			_Utils_Tuple2('はなしごえ', '話し声'),
			_Utils_Tuple2('はなしことば', '話し言葉'),
			_Utils_Tuple2('はなして', '話し手'),
			_Utils_Tuple2('はなす', '離す'),
			_Utils_Tuple2('はなす', '放す'),
			_Utils_Tuple2('はなたば', '花束'),
			_Utils_Tuple2('はなぢ', '鼻血'),
			_Utils_Tuple2('はなばたけ', '花畑'),
			_Utils_Tuple2('はなび', '花火'),
			_Utils_Tuple2('はなみ', '花見'),
			_Utils_Tuple2('はなみず', '鼻水'),
			_Utils_Tuple2('はなむこ', '花婿'),
			_Utils_Tuple2('はなやか', '華やか'),
			_Utils_Tuple2('はなよめ', '花嫁'),
			_Utils_Tuple2('はならび', '歯並び'),
			_Utils_Tuple2('はなれる', '離れる'),
			_Utils_Tuple2('ぱにっく', 'パニック'),
			_Utils_Tuple2('ばにら', 'バニラ'),
			_Utils_Tuple2('はね', '羽根'),
			_Utils_Tuple2('はねむーん', 'ハネムーン'),
			_Utils_Tuple2('はば', '幅'),
			_Utils_Tuple2('ははおや', '母親'),
			_Utils_Tuple2('ははのひ', '母の日'),
			_Utils_Tuple2('はばひろい', '幅広い'),
			_Utils_Tuple2('ぱふぇ', 'パフェ'),
			_Utils_Tuple2('はぶく', '省く'),
			_Utils_Tuple2('はぷにんぐ', 'ハプニング'),
			_Utils_Tuple2('はま', '浜'),
			_Utils_Tuple2('はまべ', '浜辺'),
			_Utils_Tuple2('はみがき', '歯磨き'),
			_Utils_Tuple2('はむすたー', 'ハムスター'),
			_Utils_Tuple2('ばめん', '場面'),
			_Utils_Tuple2('はもの', '刃物'),
			_Utils_Tuple2('はやうまれ', '早生まれ'),
			_Utils_Tuple2('はやおき', '早起き'),
			_Utils_Tuple2('はやし', '林'),
			_Utils_Tuple2('はやしらいす', 'ハヤシライス'),
			_Utils_Tuple2('はやす', '生やす'),
			_Utils_Tuple2('はやね', '早寝'),
			_Utils_Tuple2('はやめ', '早め'),
			_Utils_Tuple2('はやり', 'はやり'),
			_Utils_Tuple2('はやる', 'はやる'),
			_Utils_Tuple2('はら', '腹'),
			_Utils_Tuple2('ばら', 'ばら'),
			_Utils_Tuple2('はらい', '払い'),
			_Utils_Tuple2('はらいもどし', '払い戻し'),
			_Utils_Tuple2('はらいもどす', '払い戻す'),
			_Utils_Tuple2('はらう', '払う'),
			_Utils_Tuple2('ばらえてぃー', 'バラエティー'),
			_Utils_Tuple2('ぱらだいす', 'パラダイス'),
			_Utils_Tuple2('はらだつ', '腹立つ'),
			_Utils_Tuple2('はらはら', 'はらはら'),
			_Utils_Tuple2('ばらばら', 'ばらばら'),
			_Utils_Tuple2('はらまき', '腹巻き'),
			_Utils_Tuple2('ばらんす', 'バランス'),
			_Utils_Tuple2('はり', '針'),
			_Utils_Tuple2('はりがみ', '張り紙'),
			_Utils_Tuple2('はりきる', '張り切る'),
			_Utils_Tuple2('はりけーん', 'ハリケーン'),
			_Utils_Tuple2('はる', '張る'),
			_Utils_Tuple2('ばるこにー', 'バルコニー'),
			_Utils_Tuple2('はるさき', '春先'),
			_Utils_Tuple2('はるまき', '春巻き'),
			_Utils_Tuple2('ばれえ', 'バレエ'),
			_Utils_Tuple2('ぱれーど', 'パレード'),
			_Utils_Tuple2('はれる', '腫れる'),
			_Utils_Tuple2('ばれる', 'ばれる'),
			_Utils_Tuple2('ばれんたいん', 'バレンタイン'),
			_Utils_Tuple2('ばれんたいんでー', 'バレンタインデー'),
			_Utils_Tuple2('ぱわー', 'パワー'),
			_Utils_Tuple2('ぱわーあっぷ', 'パワーアップ'),
			_Utils_Tuple2('はわいあん', 'ハワイアン'),
			_Utils_Tuple2('ぱわふる', 'パワフル'),
			_Utils_Tuple2('はん', '飯'),
			_Utils_Tuple2('はん', '班'),
			_Utils_Tuple2('はん', '判'),
			_Utils_Tuple2('はん', '反'),
			_Utils_Tuple2('ばん', '番'),
			_Utils_Tuple2('ばん', '板'),
			_Utils_Tuple2('はんい', '範囲'),
			_Utils_Tuple2('はんえい', '反映'),
			_Utils_Tuple2('はんえん', '半円'),
			_Utils_Tuple2('はんがく', '半額'),
			_Utils_Tuple2('はんかん', '反感'),
			_Utils_Tuple2('はんきゅう', '半球'),
			_Utils_Tuple2('ばんく', 'バンク'),
			_Utils_Tuple2('ばんぐみ', '番組'),
			_Utils_Tuple2('はんぐりー', 'ハングリー'),
			_Utils_Tuple2('ぱんけーき', 'パンケーキ'),
			_Utils_Tuple2('ばんけん', '番犬'),
			_Utils_Tuple2('はんこ', '判子'),
			_Utils_Tuple2('はんこう', '反抗'),
			_Utils_Tuple2('はんこうき', '反抗期'),
			_Utils_Tuple2('はんこうてき', '反抗的'),
			_Utils_Tuple2('はんざい', '犯罪'),
			_Utils_Tuple2('ばんざい', '万歳'),
			_Utils_Tuple2('はんさむ', 'ハンサム'),
			_Utils_Tuple2('はんしん', '半身'),
			_Utils_Tuple2('はんすう', '半数'),
			_Utils_Tuple2('はんずぼん', '半ズボン'),
			_Utils_Tuple2('はんする', '反する'),
			_Utils_Tuple2('はんせい', '反省'),
			_Utils_Tuple2('はんそく', '反則'),
			_Utils_Tuple2('はんそで', '半袖'),
			_Utils_Tuple2('はんたい', '反対'),
			_Utils_Tuple2('ばんだな', 'バンダナ'),
			_Utils_Tuple2('はんだん', '判断'),
			_Utils_Tuple2('ばんち', '番地'),
			_Utils_Tuple2('ぱんち', 'パンチ'),
			_Utils_Tuple2('はんつき', '半月'),
			_Utils_Tuple2('ぱんてぃー', 'パンティー'),
			_Utils_Tuple2('はんど', 'ハンド'),
			_Utils_Tuple2('はんとう', '半島'),
			_Utils_Tuple2('はんどくりーむ', 'ハンドクリーム'),
			_Utils_Tuple2('はんどばっぐ', 'ハンドバッグ'),
			_Utils_Tuple2('はんどる', 'ハンドル'),
			_Utils_Tuple2('はんにち', '半日'),
			_Utils_Tuple2('はんにん', '犯人'),
			_Utils_Tuple2('はんのう', '反応'),
			_Utils_Tuple2('はんばい', '販売'),
			_Utils_Tuple2('ばんぱく', '万博'),
			_Utils_Tuple2('はんぱつ', '反発'),
			_Utils_Tuple2('はんはん', '半々'),
			_Utils_Tuple2('ぱんふ', 'パンフ'),
			_Utils_Tuple2('ぱんぷす', 'パンプス'),
			_Utils_Tuple2('ぱんふれっと', 'パンフレット'),
			_Utils_Tuple2('はんべつ', '判別'),
			_Utils_Tuple2('はんまー', 'ハンマー'),
			_Utils_Tuple2('はんめい', '判明'),
			_Utils_Tuple2('ばんめし', '晩飯'),
			_Utils_Tuple2('はんめん', '反面'),
			_Utils_Tuple2('はんろん', '反論'),
			_Utils_Tuple2('ひ', '非'),
			_Utils_Tuple2('ひ', '費'),
			_Utils_Tuple2('ひ', '非'),
			_Utils_Tuple2('び', '美'),
			_Utils_Tuple2('ぴあす', 'ピアス'),
			_Utils_Tuple2('ひあたり', '日当たり'),
			_Utils_Tuple2('ぴあにすと', 'ピアニスト'),
			_Utils_Tuple2('ぴーあーる', 'ＰＲ'),
			_Utils_Tuple2('びーがた', 'Ｂ型'),
			_Utils_Tuple2('ぴーく', 'ピーク'),
			_Utils_Tuple2('びーじーえむ', 'ＢＧＭ'),
			_Utils_Tuple2('ひーたー', 'ヒーター'),
			_Utils_Tuple2('びーち', 'ビーチ'),
			_Utils_Tuple2('ぴーち', 'ピーチ'),
			_Utils_Tuple2('ぴーてぃーえー', 'ＰＴＡ'),
			_Utils_Tuple2('ぴーなっつ', 'ピーナッツ'),
			_Utils_Tuple2('びーふ', 'ビーフ'),
			_Utils_Tuple2('ぴーまん', 'ピーマン'),
			_Utils_Tuple2('ひーる', 'ヒール'),
			_Utils_Tuple2('ひーろー', 'ヒーロー'),
			_Utils_Tuple2('ひえる', '冷える'),
			_Utils_Tuple2('ぴえろ', 'ピエロ'),
			_Utils_Tuple2('ひがい', '被害'),
			_Utils_Tuple2('ひがいしゃ', '被害者'),
			_Utils_Tuple2('ひがえり', '日帰り'),
			_Utils_Tuple2('ひかく', '比較'),
			_Utils_Tuple2('ひかくてき', '比較的'),
			_Utils_Tuple2('ひかげ', '日陰'),
			_Utils_Tuple2('ひかげん', '火加減'),
			_Utils_Tuple2('ひがさ', '日傘'),
			_Utils_Tuple2('ひがしあじあ', '東アジア'),
			_Utils_Tuple2('ひがしがわ', '東側'),
			_Utils_Tuple2('ひがしむき', '東向き'),
			_Utils_Tuple2('ぴかぴか', 'ぴかぴか'),
			_Utils_Tuple2('ひかり', '光'),
			_Utils_Tuple2('ひかる', '光る'),
			_Utils_Tuple2('ひがわり', '日替わり'),
			_Utils_Tuple2('ひき', '引き'),
			_Utils_Tuple2('ひきあげる', '引き上げる'),
			_Utils_Tuple2('ひきうける', '引き受ける'),
			_Utils_Tuple2('ひきかえす', '引き返す'),
			_Utils_Tuple2('ひきざん', '引き算'),
			_Utils_Tuple2('ひきだし', '引き出し'),
			_Utils_Tuple2('ひきだす', '引き出す'),
			_Utils_Tuple2('びぎなー', 'ビギナー'),
			_Utils_Tuple2('びきに', 'ビキニ'),
			_Utils_Tuple2('ひきわけ', '引き分け'),
			_Utils_Tuple2('ひきわける', '引き分ける'),
			_Utils_Tuple2('ひく', 'ひく'),
			_Utils_Tuple2('ぴくちゃー', 'ピクチャー'),
			_Utils_Tuple2('ぴくにっく', 'ピクニック'),
			_Utils_Tuple2('びくびく', 'びくびく'),
			_Utils_Tuple2('ひくめ', '低め'),
			_Utils_Tuple2('ぴくるす', 'ピクルス'),
			_Utils_Tuple2('ひぐれ', '日暮れ'),
			_Utils_Tuple2('ひげ', 'ひげ'),
			_Utils_Tuple2('ひこう', '飛行'),
			_Utils_Tuple2('ひこうかい', '非公開'),
			_Utils_Tuple2('ひこうし', '飛行士'),
			_Utils_Tuple2('ひこうじょう', '飛行場'),
			_Utils_Tuple2('ひこうせん', '飛行船'),
			_Utils_Tuple2('ひざ', '膝'),
			_Utils_Tuple2('ひざし', '日差し'),
			_Utils_Tuple2('ひじ', '肘'),
			_Utils_Tuple2('びじたー', 'ビジター'),
			_Utils_Tuple2('びじねす', 'ビジネス'),
			_Utils_Tuple2('びじねすほてる', 'ビジネスホテル'),
			_Utils_Tuple2('びじねすまん', 'ビジネスマン'),
			_Utils_Tuple2('びじゅある', 'ビジュアル'),
			_Utils_Tuple2('びじゅつ', '美術'),
			_Utils_Tuple2('びじゅつかん', '美術館'),
			_Utils_Tuple2('ひしょ', '秘書'),
			_Utils_Tuple2('びじょ', '美女'),
			_Utils_Tuple2('ひじょう', '非常'),
			_Utils_Tuple2('ひじょう', '非常'),
			_Utils_Tuple2('びしょうねん', '美少年'),
			_Utils_Tuple2('びしょびしょ', 'びしょびしょ'),
			_Utils_Tuple2('びじん', '美人'),
			_Utils_Tuple2('びすけっと', 'ビスケット'),
			_Utils_Tuple2('ぴすとる', 'ピストル'),
			_Utils_Tuple2('ひたい', '額'),
			_Utils_Tuple2('びたみん', 'ビタミン'),
			_Utils_Tuple2('ひだりがわ', '左側'),
			_Utils_Tuple2('ひだりきき', '左利き'),
			_Utils_Tuple2('ひだりはし', '左端'),
			_Utils_Tuple2('ひだりまわり', '左回り'),
			_Utils_Tuple2('びだんし', '美男子'),
			_Utils_Tuple2('ひっかかる', '引っ掛かる'),
			_Utils_Tuple2('ひっき', '筆記'),
			_Utils_Tuple2('ひっきしけん', '筆記試験'),
			_Utils_Tuple2('びっぐ', 'ビッグ'),
			_Utils_Tuple2('ぴっくあっぷ', 'ピックアップ'),
			_Utils_Tuple2('びっくり', 'びっくり'),
			_Utils_Tuple2('ひっくりかえす', '引っ繰り返す'),
			_Utils_Tuple2('ひっこし', '引っ越し'),
			_Utils_Tuple2('ひっこす', '引っ越す'),
			_Utils_Tuple2('ひっし', '必死'),
			_Utils_Tuple2('ひっし', '必死'),
			_Utils_Tuple2('ひつじ', '羊'),
			_Utils_Tuple2('ひつじ', 'ひつじ'),
			_Utils_Tuple2('ひっしゃ', '筆者'),
			_Utils_Tuple2('ひつじゅひん', '必需品'),
			_Utils_Tuple2('びっしょり', 'びっしょり'),
			_Utils_Tuple2('ひっそり', 'ひっそり'),
			_Utils_Tuple2('ぴったり', 'ぴったり'),
			_Utils_Tuple2('ぴったり', 'ぴったり'),
			_Utils_Tuple2('ひっぱる', '引っ張る'),
			_Utils_Tuple2('ひっぷ', 'ヒップ'),
			_Utils_Tuple2('ひつよう', '必要'),
			_Utils_Tuple2('ひつようせい', '必要性'),
			_Utils_Tuple2('ひてい', '否定'),
			_Utils_Tuple2('ひていてき', '否定的'),
			_Utils_Tuple2('びでおてーぷ', 'ビデオテープ'),
			_Utils_Tuple2('びでおでっき', 'ビデオデッキ'),
			_Utils_Tuple2('ひとあし', '一足'),
			_Utils_Tuple2('ひどい', 'ひどい'),
			_Utils_Tuple2('ひとくち', '一口'),
			_Utils_Tuple2('ひとこと', '一言'),
			_Utils_Tuple2('ひとごみ', '人込み'),
			_Utils_Tuple2('ひとさしゆび', '人差し指'),
			_Utils_Tuple2('ひとしい', '等しい'),
			_Utils_Tuple2('ひとつひとつ', '一つ一つ'),
			_Utils_Tuple2('ひとで', '人手'),
			_Utils_Tuple2('ひととおり', '一通り'),
			_Utils_Tuple2('ひととき', '一時'),
			_Utils_Tuple2('ひとなみ', '人並み'),
			_Utils_Tuple2('ひとびと', '人々'),
			_Utils_Tuple2('ひとまえ', '人前'),
			_Utils_Tuple2('ひとみ', '瞳'),
			_Utils_Tuple2('ひとみしり', '人見知り'),
			_Utils_Tuple2('ひとやすみ', '一休み'),
			_Utils_Tuple2('ひとり', '独り'),
			_Utils_Tuple2('ひとりぐらし', '独り暮らし'),
			_Utils_Tuple2('ひとりごと', '独り言'),
			_Utils_Tuple2('ひとりっこ', '一人っ子'),
			_Utils_Tuple2('ひとりひとり', '一人一人'),
			_Utils_Tuple2('ひなにんぎょう', 'ひな人形'),
			_Utils_Tuple2('ひなまつり', 'ひな祭り'),
			_Utils_Tuple2('ひなん', '避難'),
			_Utils_Tuple2('びにーる', 'ビニール'),
			_Utils_Tuple2('びにーるはうす', 'ビニールハウス'),
			_Utils_Tuple2('ひにく', '皮肉'),
			_Utils_Tuple2('ひにちじょう', '非日常'),
			_Utils_Tuple2('ひので', '日の出'),
			_Utils_Tuple2('ひのまる', '日の丸'),
			_Utils_Tuple2('ひび', '日々'),
			_Utils_Tuple2('ひびく', '響く'),
			_Utils_Tuple2('ひふ', '皮膚'),
			_Utils_Tuple2('ひまわり', 'ひまわり'),
			_Utils_Tuple2('ひまん', '肥満'),
			_Utils_Tuple2('びみ', '美味'),
			_Utils_Tuple2('ひみつ', '秘密'),
			_Utils_Tuple2('びみょう', '微妙'),
			_Utils_Tuple2('ひめい', '悲鳴'),
			_Utils_Tuple2('ひも', 'ひも'),
			_Utils_Tuple2('ひゃくはちじゅうど', '百八十度'),
			_Utils_Tuple2('ひやけ', '日焼け'),
			_Utils_Tuple2('ひやす', '冷やす'),
			_Utils_Tuple2('ひゃっかじてん', '百科事典'),
			_Utils_Tuple2('ひゃっかてん', '百貨店'),
			_Utils_Tuple2('ぴゅあ', 'ピュア'),
			_Utils_Tuple2('びゅーてぃー', 'ビューティー'),
			_Utils_Tuple2('ひょう', '表'),
			_Utils_Tuple2('ひよう', '費用'),
			_Utils_Tuple2('びょう', '秒'),
			_Utils_Tuple2('びょう', '病'),
			_Utils_Tuple2('びよう', '美容'),
			_Utils_Tuple2('ひょうか', '評価'),
			_Utils_Tuple2('ひょうき', '表記'),
			_Utils_Tuple2('ひょうげん', '表現'),
			_Utils_Tuple2('ひょうし', '表紙'),
			_Utils_Tuple2('ひょうじ', '表示'),
			_Utils_Tuple2('びょうし', '病死'),
			_Utils_Tuple2('びようし', '美容師'),
			_Utils_Tuple2('ひょうしき', '標識'),
			_Utils_Tuple2('びょうしつ', '病室'),
			_Utils_Tuple2('びょうしゃ', '描写'),
			_Utils_Tuple2('ひょうじゅん', '標準'),
			_Utils_Tuple2('ひょうじゅんご', '標準語'),
			_Utils_Tuple2('ひょうじょう', '表情'),
			_Utils_Tuple2('びょうじょう', '病状'),
			_Utils_Tuple2('びょうどう', '平等'),
			_Utils_Tuple2('びょうにん', '病人'),
			_Utils_Tuple2('ひょうばん', '評判'),
			_Utils_Tuple2('びょうめい', '病名'),
			_Utils_Tuple2('ひょうめん', '表面'),
			_Utils_Tuple2('ひょうめんか', '表面化'),
			_Utils_Tuple2('ひらき', '開き'),
			_Utils_Tuple2('ひらく', '開く'),
			_Utils_Tuple2('ひらける', '開ける'),
			_Utils_Tuple2('ひらたい', '平たい'),
			_Utils_Tuple2('ひらひら', 'ひらひら'),
			_Utils_Tuple2('ぴらふ', 'ピラフ'),
			_Utils_Tuple2('ぴらみっど', 'ピラミッド'),
			_Utils_Tuple2('びり', 'びり'),
			_Utils_Tuple2('びりびり', 'びりびり'),
			_Utils_Tuple2('びりやーど', 'ビリヤード'),
			_Utils_Tuple2('ひるすぎ', '昼過ぎ'),
			_Utils_Tuple2('ひるね', '昼寝'),
			_Utils_Tuple2('ひるま', '昼間'),
			_Utils_Tuple2('ひるまえ', '昼前'),
			_Utils_Tuple2('ひるめし', '昼飯'),
			_Utils_Tuple2('ひれい', '比例'),
			_Utils_Tuple2('ひろい', '拾い'),
			_Utils_Tuple2('ひろいん', 'ヒロイン'),
			_Utils_Tuple2('ひろう', '拾う'),
			_Utils_Tuple2('ひろう', '疲労'),
			_Utils_Tuple2('ひろがり', '広がり'),
			_Utils_Tuple2('ひろがる', '広がる'),
			_Utils_Tuple2('ひろげる', '広げる'),
			_Utils_Tuple2('ひろば', '広場'),
			_Utils_Tuple2('ひろびろ', '広々'),
			_Utils_Tuple2('ひろまる', '広まる'),
			_Utils_Tuple2('ひろめる', '広める'),
			_Utils_Tuple2('ひん', '品'),
			_Utils_Tuple2('ひん', '品'),
			_Utils_Tuple2('びん', '瓶'),
			_Utils_Tuple2('びん', '便'),
			_Utils_Tuple2('ひんけつ', '貧血'),
			_Utils_Tuple2('ひんしつ', '品質'),
			_Utils_Tuple2('ひんしつかんり', '品質管理'),
			_Utils_Tuple2('ぴんち', 'ピンチ'),
			_Utils_Tuple2('ひんと', 'ヒント'),
			_Utils_Tuple2('ひんぷ', '貧富'),
			_Utils_Tuple2('びんぼう', '貧乏'),
			_Utils_Tuple2('ぴんぽん', 'ピンポン'),
			_Utils_Tuple2('ひんめい', '品名'),
			_Utils_Tuple2('ふ', '不'),
			_Utils_Tuple2('ふ', '府'),
			_Utils_Tuple2('ぶ', '部'),
			_Utils_Tuple2('ぶ', '部'),
			_Utils_Tuple2('ぶ', '無'),
			_Utils_Tuple2('ぶ', '部'),
			_Utils_Tuple2('ふァいと', 'ファイト'),
			_Utils_Tuple2('ふァいなる', 'ファイナル'),
			_Utils_Tuple2('ふァいぶ', 'ファイブ'),
			_Utils_Tuple2('ふァいる', 'ファイル'),
			_Utils_Tuple2('ふァくしみり', 'ファクシミリ'),
			_Utils_Tuple2('ふァすなー', 'ファスナー'),
			_Utils_Tuple2('ぶあつい', '分厚い'),
			_Utils_Tuple2('ふァっしょん', 'ファッション'),
			_Utils_Tuple2('ふァっしょんしょー', 'ファッションショー'),
			_Utils_Tuple2('ふァっしょんもでる', 'ファッションモデル'),
			_Utils_Tuple2('ふァみりー', 'ファミリー'),
			_Utils_Tuple2('ふァみりーれすとらん', 'ファミリーレストラン'),
			_Utils_Tuple2('ふあん', '不安'),
			_Utils_Tuple2('ふあんかん', '不安感'),
			_Utils_Tuple2('ふあんてい', '不安定'),
			_Utils_Tuple2('ふぃっしゅ', 'フィッシュ'),
			_Utils_Tuple2('ふぃっと', 'フィット'),
			_Utils_Tuple2('ふぃっとねす', 'フィットネス'),
			_Utils_Tuple2('ぶいてぃーあーる', 'ＶＴＲ'),
			_Utils_Tuple2('ふぃにっしゅ', 'フィニッシュ'),
			_Utils_Tuple2('ふぃるたー', 'フィルター'),
			_Utils_Tuple2('ふぃるむ', 'フィルム'),
			_Utils_Tuple2('ぶいん', '部員'),
			_Utils_Tuple2('ぶいん', '部員'),
			_Utils_Tuple2('ふう', '風'),
			_Utils_Tuple2('ふうう', '風雨'),
			_Utils_Tuple2('ふうけい', '風景'),
			_Utils_Tuple2('ふうせん', '風船'),
			_Utils_Tuple2('ふうそく', '風速'),
			_Utils_Tuple2('ぶーつ', 'ブーツ'),
			_Utils_Tuple2('ふうとう', '封筒'),
			_Utils_Tuple2('ふうふ', '夫婦'),
			_Utils_Tuple2('ふうふう', 'ふうふう'),
			_Utils_Tuple2('ぶーむ', 'ブーム'),
			_Utils_Tuple2('ふうりょく', '風力'),
			_Utils_Tuple2('ふうりん', '風鈴'),
			_Utils_Tuple2('ぷーるさいど', 'プールサイド'),
			_Utils_Tuple2('ふぇあ', 'フェア'),
			_Utils_Tuple2('ふぇすてぃばる', 'フェスティバル'),
			_Utils_Tuple2('ふぇりー', 'フェリー'),
			_Utils_Tuple2('ふえる', '増える'),
			_Utils_Tuple2('ふぇんす', 'フェンス'),
			_Utils_Tuple2('ふぉーかす', 'フォーカス'),
			_Utils_Tuple2('ふぉーまる', 'フォーマル'),
			_Utils_Tuple2('ふぉと', 'フォト'),
			_Utils_Tuple2('ふぉろー', 'フォロー'),
			_Utils_Tuple2('ふか', '不可'),
			_Utils_Tuple2('ぶか', '部下'),
			_Utils_Tuple2('ふかい', '深い'),
			_Utils_Tuple2('ふかい', '不快'),
			_Utils_Tuple2('ふかけつ', '不可欠'),
			_Utils_Tuple2('ふかけつ', '不可欠'),
			_Utils_Tuple2('ふかのう', '不可能'),
			_Utils_Tuple2('ふかふか', 'ふかふか'),
			_Utils_Tuple2('ふかまる', '深まる'),
			_Utils_Tuple2('ふかめる', '深める'),
			_Utils_Tuple2('ふかんぜん', '不完全'),
			_Utils_Tuple2('ふきそく', '不規則'),
			_Utils_Tuple2('ふきゅう', '普及'),
			_Utils_Tuple2('ふきょう', '不況'),
			_Utils_Tuple2('ふきん', '布巾'),
			_Utils_Tuple2('ふきん', '付近'),
			_Utils_Tuple2('ふく', '拭く'),
			_Utils_Tuple2('ふく', '吹く'),
			_Utils_Tuple2('ふく', '副'),
			_Utils_Tuple2('ふく', '福'),
			_Utils_Tuple2('ふくぎょう', '副業'),
			_Utils_Tuple2('ふくざつ', '複雑'),
			_Utils_Tuple2('ふくさよう', '副作用'),
			_Utils_Tuple2('ふくし', '副詞'),
			_Utils_Tuple2('ふくしゅう', '復習'),
			_Utils_Tuple2('ふくすう', '複数'),
			_Utils_Tuple2('ふくそう', '服装'),
			_Utils_Tuple2('ふくつう', '腹痛'),
			_Utils_Tuple2('ふくむ', '含む'),
			_Utils_Tuple2('ふくめる', '含める'),
			_Utils_Tuple2('ふくよう', '服用'),
			_Utils_Tuple2('ふくろ', '袋'),
			_Utils_Tuple2('ぶくろ', '袋'),
			_Utils_Tuple2('ふけいき', '不景気'),
			_Utils_Tuple2('ふけん', '府県'),
			_Utils_Tuple2('ふこう', '不幸'),
			_Utils_Tuple2('ふごう', '符号'),
			_Utils_Tuple2('ふごうかく', '不合格'),
			_Utils_Tuple2('ふこうへい', '不公平'),
			_Utils_Tuple2('ぶざー', 'ブザー'),
			_Utils_Tuple2('ふさい', '夫妻'),
			_Utils_Tuple2('ふざい', '不在'),
			_Utils_Tuple2('ふざける', 'ふざける'),
			_Utils_Tuple2('ふさふさ', 'ふさふさ'),
			_Utils_Tuple2('ふさわしい', 'ふさわしい'),
			_Utils_Tuple2('ふじ', '富士'),
			_Utils_Tuple2('ぶじ', '無事'),
			_Utils_Tuple2('ぶじ', '無事'),
			_Utils_Tuple2('ふしあわせ', '不幸せ'),
			_Utils_Tuple2('ふしぎ', '不思議'),
			_Utils_Tuple2('ふしぜん', '不自然'),
			_Utils_Tuple2('ぶしゅ', '部首'),
			_Utils_Tuple2('ふじゆう', '不自由'),
			_Utils_Tuple2('ふじゅうぶん', '不十分'),
			_Utils_Tuple2('ふじん', '婦人'),
			_Utils_Tuple2('ふじん', '夫人'),
			_Utils_Tuple2('ふじんか', '婦人科'),
			_Utils_Tuple2('ふしんかん', '不信感'),
			_Utils_Tuple2('ふしんせつ', '不親切'),
			_Utils_Tuple2('ふせい', '不正'),
			_Utils_Tuple2('ふせぐ', '防ぐ'),
			_Utils_Tuple2('ふそく', '不足'),
			_Utils_Tuple2('ふぞく', '付属'),
			_Utils_Tuple2('ふぞくひん', '付属品'),
			_Utils_Tuple2('ふた', 'ふた'),
			_Utils_Tuple2('ふだ', '札'),
			_Utils_Tuple2('ぶたい', '舞台'),
			_Utils_Tuple2('ふたご', '双子'),
			_Utils_Tuple2('ふたしか', '不確か'),
			_Utils_Tuple2('ふたたび', '再び'),
			_Utils_Tuple2('ふたつめ', '二つ目'),
			_Utils_Tuple2('ふたん', '負担'),
			_Utils_Tuple2('ふだん', '普段'),
			_Utils_Tuple2('ふだんぎ', '普段着'),
			_Utils_Tuple2('ふちゅうい', '不注意'),
			_Utils_Tuple2('ふちょう', '不調'),
			_Utils_Tuple2('ぶちょう', '部長'),
			_Utils_Tuple2('ふつ', '仏'),
			_Utils_Tuple2('ぶつ', '物'),
			_Utils_Tuple2('ぶつ', 'ぶつ'),
			_Utils_Tuple2('ふつう', '普通'),
			_Utils_Tuple2('ふつう', '不通'),
			_Utils_Tuple2('ぶっか', '物価'),
			_Utils_Tuple2('ふっかつ', '復活'),
			_Utils_Tuple2('ふつかよい', '二日酔い'),
			_Utils_Tuple2('ぶつかる', 'ぶつかる'),
			_Utils_Tuple2('ぶっきょう', '仏教'),
			_Utils_Tuple2('ぶっく', 'ブック'),
			_Utils_Tuple2('ふっくら', 'ふっくら'),
			_Utils_Tuple2('ぶつける', 'ぶつける'),
			_Utils_Tuple2('ぷっしゅ', 'プッシュ'),
			_Utils_Tuple2('ぶつぞう', '仏像'),
			_Utils_Tuple2('ふっと', 'フット'),
			_Utils_Tuple2('ふっと', 'ふっと'),
			_Utils_Tuple2('ふっとぼーる', 'フットボール'),
			_Utils_Tuple2('ぶつぶつ', 'ぶつぶつ'),
			_Utils_Tuple2('ぶつぶつ', 'ぶつぶつ'),
			_Utils_Tuple2('ぶつり', '物理'),
			_Utils_Tuple2('ぶつりがく', '物理学'),
			_Utils_Tuple2('ぶつりてき', '物理的'),
			_Utils_Tuple2('ふで', '筆'),
			_Utils_Tuple2('ふていき', '不定期'),
			_Utils_Tuple2('ぶてぃっく', 'ブティック'),
			_Utils_Tuple2('ふでばこ', '筆箱'),
			_Utils_Tuple2('ふと', 'ふと'),
			_Utils_Tuple2('ぶどう', 'ぶどう'),
			_Utils_Tuple2('ぶどう', '武道'),
			_Utils_Tuple2('ふどうさん', '不動産'),
			_Utils_Tuple2('ふとじ', '太字'),
			_Utils_Tuple2('ふとめ', '太め'),
			_Utils_Tuple2('ふとる', '太る'),
			_Utils_Tuple2('ふとん', '布団'),
			_Utils_Tuple2('ふなたび', '船旅'),
			_Utils_Tuple2('ふなよい', '船酔い'),
			_Utils_Tuple2('ふなれ', '不慣れ'),
			_Utils_Tuple2('ふねん', '不燃'),
			_Utils_Tuple2('ふひつよう', '不必要'),
			_Utils_Tuple2('ふびょうどう', '不平等'),
			_Utils_Tuple2('ぶひん', '部品'),
			_Utils_Tuple2('ふぶき', '吹雪'),
			_Utils_Tuple2('ぶぶん', '部分'),
			_Utils_Tuple2('ぶぶんてき', '部分的'),
			_Utils_Tuple2('ふへい', '不平'),
			_Utils_Tuple2('ふぼ', '父母'),
			_Utils_Tuple2('ふほう', '不法'),
			_Utils_Tuple2('ふほう', '不法'),
			_Utils_Tuple2('ふまじめ', '不まじめ'),
			_Utils_Tuple2('ふまん', '不満'),
			_Utils_Tuple2('ふみきり', '踏切'),
			_Utils_Tuple2('ふみん', '不眠'),
			_Utils_Tuple2('ふむ', '踏む'),
			_Utils_Tuple2('ふむき', '不向き'),
			_Utils_Tuple2('ふめい', '不明'),
			_Utils_Tuple2('ぶもん', '部門'),
			_Utils_Tuple2('ふやす', '増やす'),
			_Utils_Tuple2('ふゆかい', '不愉快'),
			_Utils_Tuple2('ふゆば', '冬場'),
			_Utils_Tuple2('ふゆふく', '冬服'),
			_Utils_Tuple2('ふゆもの', '冬物'),
			_Utils_Tuple2('ふゆやま', '冬山'),
			_Utils_Tuple2('ふよう', '不要'),
			_Utils_Tuple2('ふよう', '不要'),
			_Utils_Tuple2('ぷらいす', 'プライス'),
			_Utils_Tuple2('ぶらいだる', 'ブライダル'),
			_Utils_Tuple2('ふらいと', 'フライト'),
			_Utils_Tuple2('ぷらいど', 'プライド'),
			_Utils_Tuple2('ぷらいばしー', 'プライバシー'),
			_Utils_Tuple2('ふらいぱん', 'フライパン'),
			_Utils_Tuple2('ぷらいべーと', 'プライベート'),
			_Utils_Tuple2('ぶらいんど', 'ブラインド'),
			_Utils_Tuple2('ぶらうす', 'ブラウス'),
			_Utils_Tuple2('ぶらうん', 'ブラウン'),
			_Utils_Tuple2('ぶらざー', 'ブラザー'),
			_Utils_Tuple2('ぶらし', 'ブラシ'),
			_Utils_Tuple2('ぶらじゃー', 'ブラジャー'),
			_Utils_Tuple2('ぷらす', 'プラス'),
			_Utils_Tuple2('ぷらすちっく', 'プラスチック'),
			_Utils_Tuple2('ぶらっく', 'ブラック'),
			_Utils_Tuple2('ぷらっとほーむ', 'プラットホーム'),
			_Utils_Tuple2('ふらふら', 'ふらふら'),
			_Utils_Tuple2('ふらふら', 'ふらふら'),
			_Utils_Tuple2('ぶらぶら', 'ぶらぶら'),
			_Utils_Tuple2('ふらわー', 'フラワー'),
			_Utils_Tuple2('ぷらん', 'プラン'),
			_Utils_Tuple2('ぶらんど', 'ブランド'),
			_Utils_Tuple2('ふり', '不利'),
			_Utils_Tuple2('ふり', '降り'),
			_Utils_Tuple2('ふりーたー', 'フリーター'),
			_Utils_Tuple2('ぶりーふ', 'ブリーフ'),
			_Utils_Tuple2('ふりえき', '不利益'),
			_Utils_Tuple2('ふりかえ', '振り替え'),
			_Utils_Tuple2('ふりかえる', '振り返る'),
			_Utils_Tuple2('ふりがな', '振り仮名'),
			_Utils_Tuple2('ふりこみ', '振り込み'),
			_Utils_Tuple2('ふりこむ', '振り込む'),
			_Utils_Tuple2('ぶりっじ', 'ブリッジ'),
			_Utils_Tuple2('ぷりぺいどかーど', 'プリペイドカード'),
			_Utils_Tuple2('ふりむく', '振り向く'),
			_Utils_Tuple2('ふりょう', '不良'),
			_Utils_Tuple2('ぷりんす', 'プリンス'),
			_Utils_Tuple2('ぷりんせす', 'プリンセス'),
			_Utils_Tuple2('ぷりんたー', 'プリンター'),
			_Utils_Tuple2('ぷりんと', 'プリント'),
			_Utils_Tuple2('ふる', '振る'),
			_Utils_Tuple2('ふる', 'フル'),
			_Utils_Tuple2('ふる', 'フル'),
			_Utils_Tuple2('ぶるー', 'ブルー'),
			_Utils_Tuple2('ふるーつ', 'フルーツ'),
			_Utils_Tuple2('ふるえる', '震える'),
			_Utils_Tuple2('ふるぎ', '古着'),
			_Utils_Tuple2('ふるく', '古く'),
			_Utils_Tuple2('ふるさと', '古里'),
			_Utils_Tuple2('ふるねーむ', 'フルネーム'),
			_Utils_Tuple2('ぶるぶる', 'ぶるぶる'),
			_Utils_Tuple2('ふるほん', '古本'),
			_Utils_Tuple2('ふれあう', '触れ合う'),
			_Utils_Tuple2('ぷれー', 'プレー'),
			_Utils_Tuple2('ぶれーかー', 'ブレーカー'),
			_Utils_Tuple2('ぶれーき', 'ブレーキ'),
			_Utils_Tuple2('ふれーく', 'フレーク'),
			_Utils_Tuple2('ぷれーと', 'プレート'),
			_Utils_Tuple2('ぷれーやー', 'プレーヤー'),
			_Utils_Tuple2('ぷれじでんと', 'プレジデント'),
			_Utils_Tuple2('ぶれすれっと', 'ブレスレット'),
			_Utils_Tuple2('ぷれぜん', 'プレゼン'),
			_Utils_Tuple2('ぷれぜんてーしょん', 'プレゼンテーション'),
			_Utils_Tuple2('ぷれっしゃー', 'プレッシャー'),
			_Utils_Tuple2('ふれっしゅ', 'フレッシュ'),
			_Utils_Tuple2('ふれる', '触れる'),
			_Utils_Tuple2('ふれんち', 'フレンチ'),
			_Utils_Tuple2('ふれんちどれっしんぐ', 'フレンチドレッシング'),
			_Utils_Tuple2('ふれんど', 'フレンド'),
			_Utils_Tuple2('ふろあ', 'フロア'),
			_Utils_Tuple2('ふろーりんぐ', 'フローリング'),
			_Utils_Tuple2('ぷろぐらまー', 'プログラマー'),
			_Utils_Tuple2('ぷろぐらむ', 'プログラム'),
			_Utils_Tuple2('ぷろじぇくと', 'プロジェクト'),
			_Utils_Tuple2('ぷろせす', 'プロセス'),
			_Utils_Tuple2('ぶろっこりー', 'ブロッコリー'),
			_Utils_Tuple2('ふろっぴー', 'フロッピー'),
			_Utils_Tuple2('ふろっぴーでぃすく', 'フロッピーディスク'),
			_Utils_Tuple2('ふろば', '風呂場'),
			_Utils_Tuple2('ぷろふぇっしょなる', 'プロフェッショナル'),
			_Utils_Tuple2('ぷろぽーず', 'プロポーズ'),
			_Utils_Tuple2('ふろや', '風呂屋'),
			_Utils_Tuple2('ぷろれす', 'プロレス'),
			_Utils_Tuple2('ふろんと', 'フロント'),
			_Utils_Tuple2('ふわふわ', 'ふわふわ'),
			_Utils_Tuple2('ぶん', '分'),
			_Utils_Tuple2('ぶん', '分'),
			_Utils_Tuple2('ふんいき', '雰囲気'),
			_Utils_Tuple2('ぶんか', '文化'),
			_Utils_Tuple2('ぶんかい', '分解'),
			_Utils_Tuple2('ぶんがく', '文学'),
			_Utils_Tuple2('ぶんがくさくひん', '文学作品'),
			_Utils_Tuple2('ぶんがくてき', '文学的'),
			_Utils_Tuple2('ぶんかてき', '文化的'),
			_Utils_Tuple2('ぶんぐ', '文具'),
			_Utils_Tuple2('ぶんしゅう', '文集'),
			_Utils_Tuple2('ぶんしょう', '文章'),
			_Utils_Tuple2('ふんすい', '噴水'),
			_Utils_Tuple2('ぶんたい', '文体'),
			_Utils_Tuple2('ぶんたん', '分担'),
			_Utils_Tuple2('ぶんちゅう', '文中'),
			_Utils_Tuple2('ぶんべつ', '分別'),
			_Utils_Tuple2('ぶんぽう', '文法'),
			_Utils_Tuple2('ぶんぼうぐ', '文房具'),
			_Utils_Tuple2('ぶんまつ', '文末'),
			_Utils_Tuple2('ぶんめい', '文明'),
			_Utils_Tuple2('ぶんめん', '文面'),
			_Utils_Tuple2('ぶんや', '分野'),
			_Utils_Tuple2('ぶんりょう', '分量'),
			_Utils_Tuple2('ぶんるい', '分類'),
			_Utils_Tuple2('ふんわり', 'ふんわり'),
			_Utils_Tuple2('へあ', 'ヘア'),
			_Utils_Tuple2('ぺあ', 'ペア'),
			_Utils_Tuple2('へあすたいる', 'ヘアスタイル'),
			_Utils_Tuple2('へあすぷれー', 'ヘアスプレー'),
			_Utils_Tuple2('べい', '米'),
			_Utils_Tuple2('へいかい', '閉会'),
			_Utils_Tuple2('へいき', '平気'),
			_Utils_Tuple2('へいきん', '平均'),
			_Utils_Tuple2('へいきんじゅみょう', '平均寿命'),
			_Utils_Tuple2('へいきんてん', '平均点'),
			_Utils_Tuple2('へいこう', '平行'),
			_Utils_Tuple2('べいこく', '米国'),
			_Utils_Tuple2('へいじつ', '平日'),
			_Utils_Tuple2('へいじょう', '平常'),
			_Utils_Tuple2('へいせい', '平成'),
			_Utils_Tuple2('へいぜん', '平然'),
			_Utils_Tuple2('へいてん', '閉店'),
			_Utils_Tuple2('へいねん', '平年'),
			_Utils_Tuple2('へいほう', '平方'),
			_Utils_Tuple2('へいぼん', '平凡'),
			_Utils_Tuple2('へいめん', '平面'),
			_Utils_Tuple2('へいや', '平野'),
			_Utils_Tuple2('へいわ', '平和'),
			_Utils_Tuple2('べーかりー', 'ベーカリー'),
			_Utils_Tuple2('べーこん', 'ベーコン'),
			_Utils_Tuple2('べーしっく', 'ベーシック'),
			_Utils_Tuple2('べーじゅ', 'ベージュ'),
			_Utils_Tuple2('ぺーす', 'ペース'),
			_Utils_Tuple2('べーすぼーる', 'ベースボール'),
			_Utils_Tuple2('ぺーぱー', 'ペーパー'),
			_Utils_Tuple2('ぺーぱーてすと', 'ペーパーテスト'),
			_Utils_Tuple2('ぺこぺこ', 'ぺこぺこ'),
			_Utils_Tuple2('べじたりあん', 'ベジタリアン'),
			_Utils_Tuple2('べすと', 'ベスト'),
			_Utils_Tuple2('べすと', 'ベスト'),
			_Utils_Tuple2('へそ', 'へそ'),
			_Utils_Tuple2('べたー', 'ベター'),
			_Utils_Tuple2('べたべた', 'べたべた'),
			_Utils_Tuple2('ぺだる', 'ペダル'),
			_Utils_Tuple2('べつ', '別'),
			_Utils_Tuple2('べっかん', '別館'),
			_Utils_Tuple2('べっきょ', '別居'),
			_Utils_Tuple2('べっしつ', '別室'),
			_Utils_Tuple2('べつじん', '別人'),
			_Utils_Tuple2('べっそう', '別荘'),
			_Utils_Tuple2('へっど', 'ヘッド'),
			_Utils_Tuple2('へっどほん', 'ヘッドホン'),
			_Utils_Tuple2('べっどるーむ', 'ベッドルーム'),
			_Utils_Tuple2('べつに', '別に'),
			_Utils_Tuple2('ぺっぱー', 'ペッパー'),
			_Utils_Tuple2('べつべつ', '別々'),
			_Utils_Tuple2('べつめい', '別名'),
			_Utils_Tuple2('べつもの', '別物'),
			_Utils_Tuple2('べつもんだい', '別問題'),
			_Utils_Tuple2('べてらん', 'ベテラン'),
			_Utils_Tuple2('へとへと', 'へとへと'),
			_Utils_Tuple2('べとべと', 'べとべと'),
			_Utils_Tuple2('へび', '蛇'),
			_Utils_Tuple2('べびー', 'ベビー'),
			_Utils_Tuple2('べびーしったー', 'ベビーシッター'),
			_Utils_Tuple2('へらす', '減らす'),
			_Utils_Tuple2('べらべら', 'べらべら'),
			_Utils_Tuple2('ぺらぺら', 'ぺらぺら'),
			_Utils_Tuple2('べらんだ', 'ベランダ'),
			_Utils_Tuple2('へり', 'ヘリ'),
			_Utils_Tuple2('ぺりかん', 'ペリカン'),
			_Utils_Tuple2('へりこぷたー', 'ヘリコプター'),
			_Utils_Tuple2('へる', '減る'),
			_Utils_Tuple2('べる', 'ベル'),
			_Utils_Tuple2('へるしー', 'ヘルシー'),
			_Utils_Tuple2('へるぱー', 'ヘルパー'),
			_Utils_Tuple2('へるめっと', 'ヘルメット'),
			_Utils_Tuple2('へん', '変'),
			_Utils_Tuple2('へん', '辺'),
			_Utils_Tuple2('へんか', '変化'),
			_Utils_Tuple2('へんかん', '変換'),
			_Utils_Tuple2('ぺんき', 'ペンキ'),
			_Utils_Tuple2('へんきゃく', '返却'),
			_Utils_Tuple2('へんきん', '返金'),
			_Utils_Tuple2('ぺんぎん', 'ペンギン'),
			_Utils_Tuple2('へんけい', '変形'),
			_Utils_Tuple2('へんこう', '変更'),
			_Utils_Tuple2('べんごし', '弁護士'),
			_Utils_Tuple2('へんじ', '返事'),
			_Utils_Tuple2('べんじょ', '便所'),
			_Utils_Tuple2('へんしょく', '変色'),
			_Utils_Tuple2('ぺんしょん', 'ペンション'),
			_Utils_Tuple2('ぺんしる', 'ペンシル'),
			_Utils_Tuple2('へんしん', '変身'),
			_Utils_Tuple2('へんしん', '返信'),
			_Utils_Tuple2('へんじん', '変人'),
			_Utils_Tuple2('へんそう', '返送'),
			_Utils_Tuple2('ぺんだんと', 'ペンダント'),
			_Utils_Tuple2('べんち', 'ベンチ'),
			_Utils_Tuple2('べんつ', 'ベンツ'),
			_Utils_Tuple2('へんとう', '返答'),
			_Utils_Tuple2('べんぴ', '便秘'),
			_Utils_Tuple2('へんぴん', '返品'),
			_Utils_Tuple2('ほ', '歩'),
			_Utils_Tuple2('ほいくえん', '保育園'),
			_Utils_Tuple2('ほいくしょ', '保育所'),
			_Utils_Tuple2('ぼいす', 'ボイス'),
			_Utils_Tuple2('ほいっする', 'ホイッスル'),
			_Utils_Tuple2('ほいる', 'ホイル'),
			_Utils_Tuple2('ぼいん', '母音'),
			_Utils_Tuple2('ぽいんと', 'ポイント'),
			_Utils_Tuple2('ぽいんと', 'ポイント'),
			_Utils_Tuple2('ほう', '方'),
			_Utils_Tuple2('ほう', '方'),
			_Utils_Tuple2('ほう', '法'),
			_Utils_Tuple2('ほう', '法'),
			_Utils_Tuple2('ぼう', '棒'),
			_Utils_Tuple2('ぼうえき', '貿易'),
			_Utils_Tuple2('ほうがく', '方角'),
			_Utils_Tuple2('ほうがく', '法学'),
			_Utils_Tuple2('ほうかご', '放課後'),
			_Utils_Tuple2('ぼうかん', '防寒'),
			_Utils_Tuple2('ほうき', 'ほうき'),
			_Utils_Tuple2('ほうげん', '方言'),
			_Utils_Tuple2('ぼうけん', '冒険'),
			_Utils_Tuple2('ほうこう', '方向'),
			_Utils_Tuple2('ぼうこう', '暴行'),
			_Utils_Tuple2('ほうこく', '報告'),
			_Utils_Tuple2('ぼうさい', '防災'),
			_Utils_Tuple2('ぼうさん', '坊さん'),
			_Utils_Tuple2('ぼうし', '防止'),
			_Utils_Tuple2('ほうしん', '方針'),
			_Utils_Tuple2('ほうすい', '放水'),
			_Utils_Tuple2('ぼうすい', '防水'),
			_Utils_Tuple2('ほうせき', '宝石'),
			_Utils_Tuple2('ほうそう', '放送'),
			_Utils_Tuple2('ほうそう', '包装'),
			_Utils_Tuple2('ぼうそう', '暴走'),
			_Utils_Tuple2('ほうそうきょく', '放送局'),
			_Utils_Tuple2('ほうたい', '包帯'),
			_Utils_Tuple2('ほうだい', '放題'),
			_Utils_Tuple2('ぼうちゅう', '防虫'),
			_Utils_Tuple2('ほうちょう', '包丁'),
			_Utils_Tuple2('ぼうっと', 'ぼうっと'),
			_Utils_Tuple2('ぽうっと', 'ぽうっと'),
			_Utils_Tuple2('ほうてき', '法的'),
			_Utils_Tuple2('ほうどう', '報道'),
			_Utils_Tuple2('ほうにち', '訪日'),
			_Utils_Tuple2('ぼうねんかい', '忘年会'),
			_Utils_Tuple2('ぼうはん', '防犯'),
			_Utils_Tuple2('ほうふ', '豊富'),
			_Utils_Tuple2('ぼうふう', '暴風'),
			_Utils_Tuple2('ほうほう', '方法'),
			_Utils_Tuple2('ほうぼう', '方々'),
			_Utils_Tuple2('ほうめん', '方面'),
			_Utils_Tuple2('ほうもん', '訪問'),
			_Utils_Tuple2('ほうもんはんばい', '訪問販売'),
			_Utils_Tuple2('ほうりつ', '法律'),
			_Utils_Tuple2('ほうりつがく', '法律学'),
			_Utils_Tuple2('ほうりつじむしょ', '法律事務所'),
			_Utils_Tuple2('ぼうりょく', '暴力'),
			_Utils_Tuple2('ぼうりんぐ', 'ボウリング'),
			_Utils_Tuple2('ほうる', '放る'),
			_Utils_Tuple2('ぼうる', 'ボウル'),
			_Utils_Tuple2('ほうれんそう', 'ほうれん草'),
			_Utils_Tuple2('ほえる', 'ほえる'),
			_Utils_Tuple2('ほお', '頬'),
			_Utils_Tuple2('ぼーい', 'ボーイ'),
			_Utils_Tuple2('ぼーいふれんど', 'ボーイフレンド'),
			_Utils_Tuple2('ぽーく', 'ポーク'),
			_Utils_Tuple2('ぼーだー', 'ボーダー'),
			_Utils_Tuple2('ぼーだーらいん', 'ボーダーライン'),
			_Utils_Tuple2('ぼーど', 'ボード'),
			_Utils_Tuple2('ぼーなす', 'ボーナス'),
			_Utils_Tuple2('ほーむしっく', 'ホームシック'),
			_Utils_Tuple2('ほーむすてい', 'ホームステイ'),
			_Utils_Tuple2('ほーむれす', 'ホームレス'),
			_Utils_Tuple2('ほーる', 'ホール'),
			_Utils_Tuple2('ぽーる', 'ポール'),
			_Utils_Tuple2('ほか', '他'),
			_Utils_Tuple2('ぽかぽか', 'ぽかぽか'),
			_Utils_Tuple2('ぼくし', '牧師'),
			_Utils_Tuple2('ほくじょう', '北上'),
			_Utils_Tuple2('ぼくじょう', '牧場'),
			_Utils_Tuple2('ぼくしんぐ', 'ボクシング'),
			_Utils_Tuple2('ほくせい', '北西'),
			_Utils_Tuple2('ほくとう', '北東'),
			_Utils_Tuple2('ほくぶ', '北部'),
			_Utils_Tuple2('ほくろ', 'ほくろ'),
			_Utils_Tuple2('ほけん', '保険'),
			_Utils_Tuple2('ほけん', '保健'),
			_Utils_Tuple2('ほけんしょう', '保険証'),
			_Utils_Tuple2('ほけんりょう', '保険料'),
			_Utils_Tuple2('ぼご', '母語'),
			_Utils_Tuple2('ほこう', '歩行'),
			_Utils_Tuple2('ぼこう', '母校'),
			_Utils_Tuple2('ほこうしゃ', '歩行者'),
			_Utils_Tuple2('ぼこく', '母国'),
			_Utils_Tuple2('ぼこくご', '母国語'),
			_Utils_Tuple2('ほこり', 'ほこり'),
			_Utils_Tuple2('ほし', '星'),
			_Utils_Tuple2('ぼし', '母子'),
			_Utils_Tuple2('ほしうらない', '星占い'),
			_Utils_Tuple2('ほしぞら', '星空'),
			_Utils_Tuple2('ほしゅう', '補習'),
			_Utils_Tuple2('ぼしゅう', '募集'),
			_Utils_Tuple2('ほじょ', '補助'),
			_Utils_Tuple2('ほしょう', '保証'),
			_Utils_Tuple2('ほしょうしょ', '保証書'),
			_Utils_Tuple2('ほしょうにん', '保証人'),
			_Utils_Tuple2('ほす', '干す'),
			_Utils_Tuple2('ぼす', 'ボス'),
			_Utils_Tuple2('ぽすたー', 'ポスター'),
			_Utils_Tuple2('ほすとふァみりー', 'ホストファミリー'),
			_Utils_Tuple2('ほそながい', '細長い'),
			_Utils_Tuple2('ほぞん', '保存'),
			_Utils_Tuple2('ほちきす', 'ホチキス'),
			_Utils_Tuple2('ぼっくす', 'ボックス'),
			_Utils_Tuple2('ほっけー', 'ホッケー'),
			_Utils_Tuple2('ぽっちゃり', 'ぽっちゃり'),
			_Utils_Tuple2('ほっと', 'ホット'),
			_Utils_Tuple2('ほっと', 'ほっと'),
			_Utils_Tuple2('ぽっと', 'ポット'),
			_Utils_Tuple2('ほっとけーき', 'ホットケーキ'),
			_Utils_Tuple2('ほっとこーひー', 'ホットコーヒー'),
			_Utils_Tuple2('ほっとぷれーと', 'ホットプレート'),
			_Utils_Tuple2('ぽっぷこーん', 'ポップコーン'),
			_Utils_Tuple2('ぽっぷす', 'ポップス'),
			_Utils_Tuple2('ほっぺた', 'ほっぺた'),
			_Utils_Tuple2('ほっぽう', '北方'),
			_Utils_Tuple2('ぼでぃー', 'ボディー'),
			_Utils_Tuple2('ぼでぃーがーど', 'ボディーガード'),
			_Utils_Tuple2('ほど', '程'),
			_Utils_Tuple2('ほどう', '歩道'),
			_Utils_Tuple2('ほどうきょう', '歩道橋'),
			_Utils_Tuple2('ぼとる', 'ボトル'),
			_Utils_Tuple2('ほとんど', 'ほとんど'),
			_Utils_Tuple2('ほね', '骨'),
			_Utils_Tuple2('ぽぴゅらー', 'ポピュラー'),
			_Utils_Tuple2('ほぼ', 'ほぼ'),
			_Utils_Tuple2('ほぼ', '保母'),
			_Utils_Tuple2('ほほえむ', 'ほほえむ'),
			_Utils_Tuple2('ほめる', '褒める'),
			_Utils_Tuple2('ほゆう', '保有'),
			_Utils_Tuple2('ほら', 'ほら'),
			_Utils_Tuple2('ぼらんてぃあ', 'ボランティア'),
			_Utils_Tuple2('ほりでー', 'ホリデー'),
			_Utils_Tuple2('ぽりぶくろ', 'ポリ袋'),
			_Utils_Tuple2('ほりゅう', '保留'),
			_Utils_Tuple2('ぼりゅーむ', 'ボリューム'),
			_Utils_Tuple2('ほる', '掘る'),
			_Utils_Tuple2('ほるもん', 'ホルモン'),
			_Utils_Tuple2('ぽろしゃつ', 'ポロシャツ'),
			_Utils_Tuple2('ぼろぼろ', 'ぼろぼろ'),
			_Utils_Tuple2('ほわいと', 'ホワイト'),
			_Utils_Tuple2('ぼん', '盆'),
			_Utils_Tuple2('ぼんおどり', '盆踊り'),
			_Utils_Tuple2('ほんかん', '本館'),
			_Utils_Tuple2('ほんき', '本気'),
			_Utils_Tuple2('ほんこう', '本校'),
			_Utils_Tuple2('ほんごく', '本国'),
			_Utils_Tuple2('ほんじつ', '本日'),
			_Utils_Tuple2('ほんしゃ', '本社'),
			_Utils_Tuple2('ほんしゅう', '本州'),
			_Utils_Tuple2('ほんしん', '本心'),
			_Utils_Tuple2('ぼんじん', '凡人'),
			_Utils_Tuple2('ほんすう', '本数'),
			_Utils_Tuple2('ほんたて', '本立て'),
			_Utils_Tuple2('ほんてん', '本店'),
			_Utils_Tuple2('ぽんど', 'ポンド'),
			_Utils_Tuple2('ぽんど', 'ポンド'),
			_Utils_Tuple2('ほんとうに', '本当に'),
			_Utils_Tuple2('ほんにん', '本人'),
			_Utils_Tuple2('ほんね', '本音'),
			_Utils_Tuple2('ほんばこ', '本箱'),
			_Utils_Tuple2('ほんばん', '本番'),
			_Utils_Tuple2('ぽんぷ', 'ポンプ'),
			_Utils_Tuple2('ほんぶん', '本文'),
			_Utils_Tuple2('ぼんべ', 'ボンベ'),
			_Utils_Tuple2('ほんみょう', '本名'),
			_Utils_Tuple2('ほんもの', '本物'),
			_Utils_Tuple2('ほんやく', '翻訳'),
			_Utils_Tuple2('ぼんやり', 'ぼんやり'),
			_Utils_Tuple2('ほんらい', '本来'),
			_Utils_Tuple2('ま', '間'),
			_Utils_Tuple2('ま', '間'),
			_Utils_Tuple2('ま', '真'),
			_Utils_Tuple2('まあ', 'まあ'),
			_Utils_Tuple2('まあ', 'まあ'),
			_Utils_Tuple2('まーがりん', 'マーガリン'),
			_Utils_Tuple2('まーく', 'マーク'),
			_Utils_Tuple2('まーけっと', 'マーケット'),
			_Utils_Tuple2('まあたらしい', '真新しい'),
			_Utils_Tuple2('まあまあ', 'まあまあ'),
			_Utils_Tuple2('まい', 'マイ'),
			_Utils_Tuple2('まいかー', 'マイカー'),
			_Utils_Tuple2('まいかい', '毎回'),
			_Utils_Tuple2('まいく', 'マイク'),
			_Utils_Tuple2('まいご', '迷子'),
			_Utils_Tuple2('まいしょく', '毎食'),
			_Utils_Tuple2('まいすう', '枚数'),
			_Utils_Tuple2('まいど', '毎度'),
			_Utils_Tuple2('まいなす', 'マイナス'),
			_Utils_Tuple2('まいびょう', '毎秒'),
			_Utils_Tuple2('まいぺーす', 'マイペース'),
			_Utils_Tuple2('まいる', '参る'),
			_Utils_Tuple2('まいる', 'マイル'),
			_Utils_Tuple2('まいる', 'マイル'),
			_Utils_Tuple2('まいるど', 'マイルド'),
			_Utils_Tuple2('まうえ', '真上'),
			_Utils_Tuple2('まうす', 'マウス'),
			_Utils_Tuple2('まえあし', '前足'),
			_Utils_Tuple2('まえうり', '前売り'),
			_Utils_Tuple2('まえがみ', '前髪'),
			_Utils_Tuple2('まえば', '前歯'),
			_Utils_Tuple2('まえばらい', '前払い'),
			_Utils_Tuple2('まがじん', 'マガジン'),
			_Utils_Tuple2('まかせる', '任せる'),
			_Utils_Tuple2('まがり', '曲がり'),
			_Utils_Tuple2('まがりかど', '曲がり角'),
			_Utils_Tuple2('まがる', '曲がる'),
			_Utils_Tuple2('まかろに', 'マカロニ'),
			_Utils_Tuple2('まく', '巻く'),
			_Utils_Tuple2('まぐにちゅーど', 'マグニチュード'),
			_Utils_Tuple2('まくら', '枕'),
			_Utils_Tuple2('まぐろ', 'まぐろ'),
			_Utils_Tuple2('まけ', '負け'),
			_Utils_Tuple2('まける', '負ける'),
			_Utils_Tuple2('まげる', '曲げる'),
			_Utils_Tuple2('まさか', 'まさか'),
			_Utils_Tuple2('まさに', '正に'),
			_Utils_Tuple2('まざる', '混ざる'),
			_Utils_Tuple2('まし', '増し'),
			_Utils_Tuple2('まし', '増し'),
			_Utils_Tuple2('まじしゃん', 'マジシャン'),
			_Utils_Tuple2('まじっく', 'マジック'),
			_Utils_Tuple2('ましゅまろ', 'マシュマロ'),
			_Utils_Tuple2('ましょうめん', '真正面'),
			_Utils_Tuple2('まじる', '混じる'),
			_Utils_Tuple2('まじる', '交じる'),
			_Utils_Tuple2('ましん', 'マシン'),
			_Utils_Tuple2('ます', '増す'),
			_Utils_Tuple2('まず', 'まず'),
			_Utils_Tuple2('まずい', 'まずい'),
			_Utils_Tuple2('ますかっと', 'マスカット'),
			_Utils_Tuple2('ますく', 'マスク'),
			_Utils_Tuple2('ますこみ', 'マスコミ'),
			_Utils_Tuple2('まずしい', '貧しい'),
			_Utils_Tuple2('ますたー', 'マスター'),
			_Utils_Tuple2('ますたーど', 'マスタード'),
			_Utils_Tuple2('ますます', 'ますます'),
			_Utils_Tuple2('ますめでぃあ', 'マスメディア'),
			_Utils_Tuple2('まぜる', '交ぜる'),
			_Utils_Tuple2('まぜる', '混ぜる'),
			_Utils_Tuple2('また', '又'),
			_Utils_Tuple2('また', '又'),
			_Utils_Tuple2('または', '又は'),
			_Utils_Tuple2('またまた', '又々'),
			_Utils_Tuple2('まだまだ', 'まだまだ'),
			_Utils_Tuple2('まち', '街'),
			_Utils_Tuple2('まち', '待ち'),
			_Utils_Tuple2('まちあわせ', '待ち合わせ'),
			_Utils_Tuple2('まちあわせる', '待ち合わせる'),
			_Utils_Tuple2('まぢか', '間近'),
			_Utils_Tuple2('まちがい', '間違い'),
			_Utils_Tuple2('まちがう', '間違う'),
			_Utils_Tuple2('まちがえる', '間違える'),
			_Utils_Tuple2('まちなか', '町中'),
			_Utils_Tuple2('まつ', '末'),
			_Utils_Tuple2('まつ', '松'),
			_Utils_Tuple2('まっか', '真っ赤'),
			_Utils_Tuple2('まっくら', '真っ暗'),
			_Utils_Tuple2('まっくら', '真っ暗'),
			_Utils_Tuple2('まっくろ', '真っ黒'),
			_Utils_Tuple2('まっくろい', '真っ黒い'),
			_Utils_Tuple2('まっさーじ', 'マッサージ'),
			_Utils_Tuple2('まっさお', '真っ青'),
			_Utils_Tuple2('まっさき', '真っ先'),
			_Utils_Tuple2('まつじつ', '末日'),
			_Utils_Tuple2('まっしゅるーむ', 'マッシュルーム'),
			_Utils_Tuple2('まっしろ', '真っ白'),
			_Utils_Tuple2('まっしろ', '真っ白'),
			_Utils_Tuple2('まっしろい', '真っ白い'),
			_Utils_Tuple2('まっすぐ', 'まっすぐ'),
			_Utils_Tuple2('まっすぐ', 'まっすぐ'),
			_Utils_Tuple2('まったく', '全く'),
			_Utils_Tuple2('まっちゃ', '抹茶'),
			_Utils_Tuple2('まっと', 'マット'),
			_Utils_Tuple2('まつり', '祭り'),
			_Utils_Tuple2('まどがらす', '窓ガラス'),
			_Utils_Tuple2('まどぎわ', '窓際'),
			_Utils_Tuple2('まどぐち', '窓口'),
			_Utils_Tuple2('まとまる', 'まとまる'),
			_Utils_Tuple2('まとめ', 'まとめ'),
			_Utils_Tuple2('まとめる', 'まとめる'),
			_Utils_Tuple2('まどり', '間取り'),
			_Utils_Tuple2('まなー', 'マナー'),
			_Utils_Tuple2('まないた', 'まないた'),
			_Utils_Tuple2('まなつ', '真夏'),
			_Utils_Tuple2('まなび', '学び'),
			_Utils_Tuple2('まなぶ', '学ぶ'),
			_Utils_Tuple2('まにあ', 'マニア'),
			_Utils_Tuple2('まにあう', '間に合う'),
			_Utils_Tuple2('まにあっく', 'マニアック'),
			_Utils_Tuple2('まにきゅあ', 'マニキュア'),
			_Utils_Tuple2('まにゅある', 'マニュアル'),
			_Utils_Tuple2('まね', 'まね'),
			_Utils_Tuple2('まねー', 'マネー'),
			_Utils_Tuple2('まねーじゃー', 'マネージャー'),
			_Utils_Tuple2('まねきん', 'マネキン'),
			_Utils_Tuple2('まねく', '招く'),
			_Utils_Tuple2('まねる', 'まねる'),
			_Utils_Tuple2('まぶしい', 'まぶしい'),
			_Utils_Tuple2('まふゆ', '真冬'),
			_Utils_Tuple2('まふらー', 'マフラー'),
			_Utils_Tuple2('まめ', '豆'),
			_Utils_Tuple2('まもり', '守り'),
			_Utils_Tuple2('まもる', '守る'),
			_Utils_Tuple2('まやく', '麻薬'),
			_Utils_Tuple2('まゆ', 'まゆ'),
			_Utils_Tuple2('まゆげ', 'まゆ毛'),
			_Utils_Tuple2('まよい', '迷い'),
			_Utils_Tuple2('まよう', '迷う'),
			_Utils_Tuple2('まよなか', '真夜中'),
			_Utils_Tuple2('まよねーず', 'マヨネーズ'),
			_Utils_Tuple2('まらそん', 'マラソン'),
			_Utils_Tuple2('まる', '丸'),
			_Utils_Tuple2('まるい', '丸い'),
			_Utils_Tuple2('まるで', '丸で'),
			_Utils_Tuple2('まれ', 'まれ'),
			_Utils_Tuple2('まろん', 'マロン'),
			_Utils_Tuple2('まわす', '回す'),
			_Utils_Tuple2('まわり', '周り'),
			_Utils_Tuple2('まわり', '回り'),
			_Utils_Tuple2('まわり', '回り'),
			_Utils_Tuple2('まわり', '周り'),
			_Utils_Tuple2('まわり', '回り'),
			_Utils_Tuple2('まわりみち', '回り道'),
			_Utils_Tuple2('まわる', '回る'),
			_Utils_Tuple2('まん', 'マン'),
			_Utils_Tuple2('まん', '満'),
			_Utils_Tuple2('まんいち', '万一'),
			_Utils_Tuple2('まんいち', '万一'),
			_Utils_Tuple2('まんいん', '満員'),
			_Utils_Tuple2('まんかい', '満開'),
			_Utils_Tuple2('まんがいち', '万が一'),
			_Utils_Tuple2('まんげつ', '満月'),
			_Utils_Tuple2('まんごー', 'マンゴー'),
			_Utils_Tuple2('まんじゅう', 'まんじゅう'),
			_Utils_Tuple2('まんしょん', 'マンション'),
			_Utils_Tuple2('まんせき', '満席'),
			_Utils_Tuple2('まんぞく', '満足'),
			_Utils_Tuple2('まんぞくかん', '満足感'),
			_Utils_Tuple2('まんてん', '満点'),
			_Utils_Tuple2('まんなか', '真ん中'),
			_Utils_Tuple2('まんねんひつ', '万年筆'),
			_Utils_Tuple2('まんぱい', '満杯'),
			_Utils_Tuple2('まんびき', '万引き'),
			_Utils_Tuple2('まんぷく', '満腹'),
			_Utils_Tuple2('まんまる', '真ん丸'),
			_Utils_Tuple2('み', '身'),
			_Utils_Tuple2('み', '味'),
			_Utils_Tuple2('み', '未'),
			_Utils_Tuple2('みあい', '見合い'),
			_Utils_Tuple2('みあう', '見合う'),
			_Utils_Tuple2('みあげる', '見上げる'),
			_Utils_Tuple2('みーてぃんぐ', 'ミーティング'),
			_Utils_Tuple2('みうしなう', '見失う'),
			_Utils_Tuple2('みうち', '身内'),
			_Utils_Tuple2('みおくり', '見送り'),
			_Utils_Tuple2('みおくる', '見送る'),
			_Utils_Tuple2('みおろす', '見下ろす'),
			_Utils_Tuple2('みがき', '磨き'),
			_Utils_Tuple2('みかく', '味覚'),
			_Utils_Tuple2('みかけ', '見掛け'),
			_Utils_Tuple2('みかた', '味方'),
			_Utils_Tuple2('みかんせい', '未完成'),
			_Utils_Tuple2('みぎうで', '右腕'),
			_Utils_Tuple2('みぎがわ', '右側'),
			_Utils_Tuple2('みきき', '見聞き'),
			_Utils_Tuple2('みぎきき', '右利き'),
			_Utils_Tuple2('みきさー', 'ミキサー'),
			_Utils_Tuple2('みぎはし', '右端'),
			_Utils_Tuple2('みぎひだり', '右左'),
			_Utils_Tuple2('みぎまわり', '右回り'),
			_Utils_Tuple2('みくらべる', '見比べる'),
			_Utils_Tuple2('みけいけん', '未経験'),
			_Utils_Tuple2('みごと', '見事'),
			_Utils_Tuple2('みじかめ', '短め'),
			_Utils_Tuple2('みじゅく', '未熟'),
			_Utils_Tuple2('みずあそび', '水遊び'),
			_Utils_Tuple2('みずあらい', '水洗い'),
			_Utils_Tuple2('みずいろ', '水色'),
			_Utils_Tuple2('みずうみ', '湖'),
			_Utils_Tuple2('みずから', '自ら'),
			_Utils_Tuple2('みずぎ', '水着'),
			_Utils_Tuple2('みずたま', '水玉'),
			_Utils_Tuple2('みすてりー', 'ミステリー'),
			_Utils_Tuple2('みせいねん', '未成年'),
			_Utils_Tuple2('みせいねんしゃ', '未成年者'),
			_Utils_Tuple2('みせさき', '店先'),
			_Utils_Tuple2('みせす', 'ミセス'),
			_Utils_Tuple2('みそ', 'みそ'),
			_Utils_Tuple2('みだし', '見出し'),
			_Utils_Tuple2('みぢか', '身近'),
			_Utils_Tuple2('みちじゅん', '道順'),
			_Utils_Tuple2('みつかる', '見付かる'),
			_Utils_Tuple2('みっくす', 'ミックス'),
			_Utils_Tuple2('みつける', '見付ける'),
			_Utils_Tuple2('みっどないと', 'ミッドナイト'),
			_Utils_Tuple2('みっともない', 'みっともない'),
			_Utils_Tuple2('みつめる', '見詰める'),
			_Utils_Tuple2('みてい', '未定'),
			_Utils_Tuple2('みどころ', '見所'),
			_Utils_Tuple2('みとめる', '認める'),
			_Utils_Tuple2('みどりのまどぐち', '緑の窓口'),
			_Utils_Tuple2('みどる', 'ミドル'),
			_Utils_Tuple2('みな', '皆'),
			_Utils_Tuple2('みなおし', '見直し'),
			_Utils_Tuple2('みなおす', '見直す'),
			_Utils_Tuple2('みなさま', '皆様'),
			_Utils_Tuple2('みなと', '港'),
			_Utils_Tuple2('みなみがわ', '南側'),
			_Utils_Tuple2('みなみはんきゅう', '南半球'),
			_Utils_Tuple2('みなみむき', '南向き'),
			_Utils_Tuple2('みに', 'ミニ'),
			_Utils_Tuple2('みにくい', '醜い'),
			_Utils_Tuple2('みにすかーと', 'ミニスカート'),
			_Utils_Tuple2('みにちゅあ', 'ミニチュア'),
			_Utils_Tuple2('みぶん', '身分'),
			_Utils_Tuple2('みぶんしょうめいしょ', '身分証明書'),
			_Utils_Tuple2('みほん', '見本'),
			_Utils_Tuple2('みまい', '見舞い'),
			_Utils_Tuple2('みまもる', '見守る'),
			_Utils_Tuple2('みまわす', '見回す'),
			_Utils_Tuple2('みまわり', '見回り'),
			_Utils_Tuple2('みまん', '未満'),
			_Utils_Tuple2('みやげ', '土産'),
			_Utils_Tuple2('みやげもの', '土産物'),
			_Utils_Tuple2('みゅーじあむ', 'ミュージアム'),
			_Utils_Tuple2('みゅーじかる', 'ミュージカル'),
			_Utils_Tuple2('みゅーじっく', 'ミュージック'),
			_Utils_Tuple2('みょうごにち', '明後日'),
			_Utils_Tuple2('みょうじ', '名字'),
			_Utils_Tuple2('みょうちょう', '明朝'),
			_Utils_Tuple2('みょうにち', '明日'),
			_Utils_Tuple2('みょうばん', '明晩'),
			_Utils_Tuple2('みらー', 'ミラー'),
			_Utils_Tuple2('みらい', '未来'),
			_Utils_Tuple2('みらくる', 'ミラクル'),
			_Utils_Tuple2('みり', 'ミリ'),
			_Utils_Tuple2('みり', 'ミリ'),
			_Utils_Tuple2('みりぐらむ', 'ミリグラム'),
			_Utils_Tuple2('みりめーとる', 'ミリメートル'),
			_Utils_Tuple2('みりょく', '魅力'),
			_Utils_Tuple2('みりりっとる', 'ミリリットル'),
			_Utils_Tuple2('みわけ', '見分け'),
			_Utils_Tuple2('みわたす', '見渡す'),
			_Utils_Tuple2('みんしゅく', '民宿'),
			_Utils_Tuple2('みんぞく', '民族'),
			_Utils_Tuple2('みんと', 'ミント'),
			_Utils_Tuple2('む', '無'),
			_Utils_Tuple2('む', '無'),
			_Utils_Tuple2('むいみ', '無意味'),
			_Utils_Tuple2('むーど', 'ムード'),
			_Utils_Tuple2('むーびー', 'ムービー'),
			_Utils_Tuple2('むかい', '向かい'),
			_Utils_Tuple2('むがい', '無害'),
			_Utils_Tuple2('むかいあう', '向かい合う'),
			_Utils_Tuple2('むかいがわ', '向かい側'),
			_Utils_Tuple2('むかう', '向かう'),
			_Utils_Tuple2('むかえる', '迎える'),
			_Utils_Tuple2('むかし', '昔'),
			_Utils_Tuple2('むかしばなし', '昔話'),
			_Utils_Tuple2('むかしむかし', '昔々'),
			_Utils_Tuple2('むかつく', 'むかつく'),
			_Utils_Tuple2('むかむか', 'むかむか'),
			_Utils_Tuple2('むかんけい', '無関係'),
			_Utils_Tuple2('むかんしん', '無関心'),
			_Utils_Tuple2('むき', '向き'),
			_Utils_Tuple2('むき', '向き'),
			_Utils_Tuple2('むぎ', '麦'),
			_Utils_Tuple2('むぎちゃ', '麦茶'),
			_Utils_Tuple2('むきゅう', '無休'),
			_Utils_Tuple2('むく', 'むく'),
			_Utils_Tuple2('むく', '向く'),
			_Utils_Tuple2('むくち', '無口'),
			_Utils_Tuple2('むけ', '向け'),
			_Utils_Tuple2('むけいかく', '無計画'),
			_Utils_Tuple2('むける', '向ける'),
			_Utils_Tuple2('むげん', '無限'),
			_Utils_Tuple2('むこう', '向こう'),
			_Utils_Tuple2('むこうがわ', '向こう側'),
			_Utils_Tuple2('むごん', '無言'),
			_Utils_Tuple2('むし', '虫'),
			_Utils_Tuple2('むし', '無視'),
			_Utils_Tuple2('むしあつい', '蒸し暑い'),
			_Utils_Tuple2('むしば', '虫歯'),
			_Utils_Tuple2('むじょうけん', '無条件'),
			_Utils_Tuple2('むしょく', '無職'),
			_Utils_Tuple2('むしょく', '無色'),
			_Utils_Tuple2('むしろ', 'むしろ'),
			_Utils_Tuple2('むじん', '無人'),
			_Utils_Tuple2('むす', '蒸す'),
			_Utils_Tuple2('むすう', '無数'),
			_Utils_Tuple2('むすぶ', '結ぶ'),
			_Utils_Tuple2('むせいげん', '無制限'),
			_Utils_Tuple2('むせきにん', '無責任'),
			_Utils_Tuple2('むだ', '無駄'),
			_Utils_Tuple2('むだづかい', '無駄遣い'),
			_Utils_Tuple2('むだん', '無断'),
			_Utils_Tuple2('むちゅう', '夢中'),
			_Utils_Tuple2('むちゅう', '夢中'),
			_Utils_Tuple2('むね', '胸'),
			_Utils_Tuple2('むひょうじょう', '無表情'),
			_Utils_Tuple2('むめい', '無名'),
			_Utils_Tuple2('むら', '村'),
			_Utils_Tuple2('むらさき', '紫'),
			_Utils_Tuple2('むらさきいろ', '紫色'),
			_Utils_Tuple2('むらびと', '村人'),
			_Utils_Tuple2('むり', '無理'),
			_Utils_Tuple2('むりやり', '無理矢理'),
			_Utils_Tuple2('むりょう', '無料'),
			_Utils_Tuple2('むりょく', '無力'),
			_Utils_Tuple2('め', '目'),
			_Utils_Tuple2('め', '芽'),
			_Utils_Tuple2('めあて', '目当て'),
			_Utils_Tuple2('めい', '名'),
			_Utils_Tuple2('めい', 'めい'),
			_Utils_Tuple2('めい', '名'),
			_Utils_Tuple2('めい', '名'),
			_Utils_Tuple2('めいかく', '明確'),
			_Utils_Tuple2('めいきょく', '名曲'),
			_Utils_Tuple2('めいさく', '名作'),
			_Utils_Tuple2('めいさん', '名産'),
			_Utils_Tuple2('めいし', '名詞'),
			_Utils_Tuple2('めいし', '名刺'),
			_Utils_Tuple2('めいじ', '明治'),
			_Utils_Tuple2('めいじじだい', '明治時代'),
			_Utils_Tuple2('めいしょ', '名所'),
			_Utils_Tuple2('めいしん', '迷信'),
			_Utils_Tuple2('めいじん', '名人'),
			_Utils_Tuple2('めいぶつ', '名物'),
			_Utils_Tuple2('めいぼ', '名簿'),
			_Utils_Tuple2('めいれい', '命令'),
			_Utils_Tuple2('めいれいけい', '命令形'),
			_Utils_Tuple2('めいれいぶん', '命令文'),
			_Utils_Tuple2('めいわく', '迷惑'),
			_Utils_Tuple2('めうえ', '目上'),
			_Utils_Tuple2('めーかー', 'メーカー'),
			_Utils_Tuple2('めーたー', 'メーター'),
			_Utils_Tuple2('めーぷるしろっぷ', 'メープルシロップ'),
			_Utils_Tuple2('めぐすり', '目薬'),
			_Utils_Tuple2('めぐまれる', '恵まれる'),
			_Utils_Tuple2('めざす', '目指す'),
			_Utils_Tuple2('めざまし', '目覚まし'),
			_Utils_Tuple2('めざましどけい', '目覚まし時計'),
			_Utils_Tuple2('めざめる', '目覚める'),
			_Utils_Tuple2('めし', '飯'),
			_Utils_Tuple2('めしあがる', '召し上がる'),
			_Utils_Tuple2('めした', '目下'),
			_Utils_Tuple2('めじるし', '目印'),
			_Utils_Tuple2('めす', '雌'),
			_Utils_Tuple2('めずらしい', '珍しい'),
			_Utils_Tuple2('めせん', '目線'),
			_Utils_Tuple2('めだつ', '目立つ'),
			_Utils_Tuple2('めだま', '目玉'),
			_Utils_Tuple2('めだりすと', 'メダリスト'),
			_Utils_Tuple2('めだる', 'メダル'),
			_Utils_Tuple2('めちゃくちゃ', '目茶苦茶'),
			_Utils_Tuple2('めちゃくちゃ', '目茶苦茶'),
			_Utils_Tuple2('めちゃめちゃ', '目茶目茶'),
			_Utils_Tuple2('めっせーじ', 'メッセージ'),
			_Utils_Tuple2('めった', '滅多'),
			_Utils_Tuple2('めでぃあ', 'メディア'),
			_Utils_Tuple2('めでたい', 'めでたい'),
			_Utils_Tuple2('めとろ', 'メトロ'),
			_Utils_Tuple2('めにゅー', 'メニュー'),
			_Utils_Tuple2('めのまえ', '目の前'),
			_Utils_Tuple2('めも', 'メモ'),
			_Utils_Tuple2('めもりー', 'メモリー'),
			_Utils_Tuple2('めやす', '目安'),
			_Utils_Tuple2('めろん', 'メロン'),
			_Utils_Tuple2('めん', '面'),
			_Utils_Tuple2('めん', '麺'),
			_Utils_Tuple2('めん', '綿'),
			_Utils_Tuple2('めんきょ', '免許'),
			_Utils_Tuple2('めんきょしょう', '免許証'),
			_Utils_Tuple2('めんぜい', '免税'),
			_Utils_Tuple2('めんせき', '面積'),
			_Utils_Tuple2('めんせつ', '面接'),
			_Utils_Tuple2('めんだん', '面談'),
			_Utils_Tuple2('めんちかつ', 'メンチカツ'),
			_Utils_Tuple2('めんどう', '面倒'),
			_Utils_Tuple2('めんどうくさい', '面倒臭い'),
			_Utils_Tuple2('めんばー', 'メンバー'),
			_Utils_Tuple2('めんるい', '麺類'),
			_Utils_Tuple2('もう', '毛'),
			_Utils_Tuple2('もうかる', 'もうかる'),
			_Utils_Tuple2('もうけ', 'もうけ'),
			_Utils_Tuple2('もうける', 'もうける'),
			_Utils_Tuple2('もうしあげる', '申し上げる'),
			_Utils_Tuple2('もうしこみ', '申し込み'),
			_Utils_Tuple2('もうしこむ', '申し込む'),
			_Utils_Tuple2('もうしわけ', '申し訳'),
			_Utils_Tuple2('もうしわけない', '申し訳無い'),
			_Utils_Tuple2('もうす', '申す'),
			_Utils_Tuple2('もうふ', '毛布'),
			_Utils_Tuple2('もえあがる', '燃え上がる'),
			_Utils_Tuple2('もえひろがる', '燃え広がる'),
			_Utils_Tuple2('もえる', '燃える'),
			_Utils_Tuple2('もーたー', 'モーター'),
			_Utils_Tuple2('もーにんぐ', 'モーニング'),
			_Utils_Tuple2('もくざい', '木材'),
			_Utils_Tuple2('もくじ', '目次'),
			_Utils_Tuple2('もくせい', '木製'),
			_Utils_Tuple2('もくぜん', '目前'),
			_Utils_Tuple2('もくぞう', '木造'),
			_Utils_Tuple2('もくてき', '目的'),
			_Utils_Tuple2('もくひょう', '目標'),
			_Utils_Tuple2('もくもく', 'もくもく'),
			_Utils_Tuple2('もし', '模試'),
			_Utils_Tuple2('もじ', '文字'),
			_Utils_Tuple2('もしも', 'もしも'),
			_Utils_Tuple2('もたす', '持たす'),
			_Utils_Tuple2('もだん', 'モダン'),
			_Utils_Tuple2('もち', 'もち'),
			_Utils_Tuple2('もち', '持ち'),
			_Utils_Tuple2('もちあがる', '持ち上がる'),
			_Utils_Tuple2('もちあげる', '持ち上げる'),
			_Utils_Tuple2('もちいる', '用いる'),
			_Utils_Tuple2('もちかえり', '持ち帰り'),
			_Utils_Tuple2('もちかえる', '持ち帰る'),
			_Utils_Tuple2('もちこむ', '持ち込む'),
			_Utils_Tuple2('もちだし', '持ち出し'),
			_Utils_Tuple2('もちだす', '持ち出す'),
			_Utils_Tuple2('もちぬし', '持ち主'),
			_Utils_Tuple2('もちはこび', '持ち運び'),
			_Utils_Tuple2('もちはこぶ', '持ち運ぶ'),
			_Utils_Tuple2('もちもの', '持ち物'),
			_Utils_Tuple2('もちろん', 'もちろん'),
			_Utils_Tuple2('もつ', 'もつ'),
			_Utils_Tuple2('もったいない', 'もったいない'),
			_Utils_Tuple2('もっとも', '最も'),
			_Utils_Tuple2('もてる', 'もてる'),
			_Utils_Tuple2('もでる', 'モデル'),
			_Utils_Tuple2('もと', '元'),
			_Utils_Tuple2('もと', '下'),
			_Utils_Tuple2('もと', '基'),
			_Utils_Tuple2('もと', 'もと'),
			_Utils_Tuple2('もどす', '戻す'),
			_Utils_Tuple2('もとづく', '基づく'),
			_Utils_Tuple2('もとめる', '求める'),
			_Utils_Tuple2('もともと', '元々'),
			_Utils_Tuple2('もともと', '元々'),
			_Utils_Tuple2('もどる', '戻る'),
			_Utils_Tuple2('もの', '物'),
			_Utils_Tuple2('もの', '物'),
			_Utils_Tuple2('もの', '者'),
			_Utils_Tuple2('もの', '物'),
			_Utils_Tuple2('ものおき', '物置'),
			_Utils_Tuple2('ものおと', '物音'),
			_Utils_Tuple2('ものがたり', '物語'),
			_Utils_Tuple2('ものごと', '物事'),
			_Utils_Tuple2('ものさし', '物差し'),
			_Utils_Tuple2('ものしり', '物知り'),
			_Utils_Tuple2('ものすごい', 'ものすごい'),
			_Utils_Tuple2('ものほし', '物干し'),
			_Utils_Tuple2('ものれーる', 'モノレール'),
			_Utils_Tuple2('もみじ', '紅葉'),
			_Utils_Tuple2('もむ', 'もむ'),
			_Utils_Tuple2('もめん', '木綿'),
			_Utils_Tuple2('もも', '桃'),
			_Utils_Tuple2('もやし', 'もやし'),
			_Utils_Tuple2('もやす', '燃やす'),
			_Utils_Tuple2('もよう', '模様'),
			_Utils_Tuple2('もより', '最寄り'),
			_Utils_Tuple2('もらる', 'モラル'),
			_Utils_Tuple2('もり', '森'),
			_Utils_Tuple2('もり', '盛り'),
			_Utils_Tuple2('もりあがる', '盛り上がる'),
			_Utils_Tuple2('もりあげる', '盛り上げる'),
			_Utils_Tuple2('もる', '盛る'),
			_Utils_Tuple2('もん', '門'),
			_Utils_Tuple2('もんきー', 'モンキー'),
			_Utils_Tuple2('もんく', '文句'),
			_Utils_Tuple2('もんげん', '門限'),
			_Utils_Tuple2('や', '夜'),
			_Utils_Tuple2('や', '野'),
			_Utils_Tuple2('や', 'や'),
			_Utils_Tuple2('やあ', 'やあ'),
			_Utils_Tuple2('やおや', '八百屋'),
			_Utils_Tuple2('やがい', '野外'),
			_Utils_Tuple2('やがて', 'やがて'),
			_Utils_Tuple2('やかましい', 'やかましい'),
			_Utils_Tuple2('やかん', 'やかん'),
			_Utils_Tuple2('やかん', '夜間'),
			_Utils_Tuple2('やき', '焼き'),
			_Utils_Tuple2('やぎ', 'ヤギ'),
			_Utils_Tuple2('やきあげる', '焼き上げる'),
			_Utils_Tuple2('やきとり', '焼き鳥'),
			_Utils_Tuple2('やきもち', '焼きもち'),
			_Utils_Tuple2('やく', '約'),
			_Utils_Tuple2('やく', '焼く'),
			_Utils_Tuple2('やく', '訳'),
			_Utils_Tuple2('やく', '役'),
			_Utils_Tuple2('やく', '約'),
			_Utils_Tuple2('やく', '薬'),
			_Utils_Tuple2('やくいん', '役員'),
			_Utils_Tuple2('やくがく', '薬学'),
			_Utils_Tuple2('やくざ', 'やくざ'),
			_Utils_Tuple2('やくしゃ', '役者'),
			_Utils_Tuple2('やくしょ', '役所'),
			_Utils_Tuple2('やくす', '訳す'),
			_Utils_Tuple2('やくそう', '薬草'),
			_Utils_Tuple2('やくそく', '約束'),
			_Utils_Tuple2('やくだつ', '役立つ'),
			_Utils_Tuple2('やくだてる', '役立てる'),
			_Utils_Tuple2('やくひん', '薬品'),
			_Utils_Tuple2('やくめ', '役目'),
			_Utils_Tuple2('やくめい', '役名'),
			_Utils_Tuple2('やくわり', '役割'),
			_Utils_Tuple2('やけ', '焼け'),
			_Utils_Tuple2('やけい', '夜景'),
			_Utils_Tuple2('やけど', 'やけど'),
			_Utils_Tuple2('やける', '焼ける'),
			_Utils_Tuple2('やこう', '夜行'),
			_Utils_Tuple2('やこうれっしゃ', '夜行列車'),
			_Utils_Tuple2('やしょく', '夜食'),
			_Utils_Tuple2('やじるし', '矢印'),
			_Utils_Tuple2('やすうり', '安売り'),
			_Utils_Tuple2('やすっぽい', '安っぽい'),
			_Utils_Tuple2('やすみやすみ', '休み休み'),
			_Utils_Tuple2('やすめ', '安め'),
			_Utils_Tuple2('やすめる', '休める'),
			_Utils_Tuple2('やすもの', '安物'),
			_Utils_Tuple2('やせる', 'やせる'),
			_Utils_Tuple2('やたい', '屋台'),
			_Utils_Tuple2('やちん', '家賃'),
			_Utils_Tuple2('やつ', 'やつ'),
			_Utils_Tuple2('やっきょく', '薬局'),
			_Utils_Tuple2('やっと', 'やっと'),
			_Utils_Tuple2('やど', '宿'),
			_Utils_Tuple2('やとう', '雇う'),
			_Utils_Tuple2('やぬし', '家主'),
			_Utils_Tuple2('やね', '屋根'),
			_Utils_Tuple2('やねうら', '屋根裏'),
			_Utils_Tuple2('やばい', 'やばい'),
			_Utils_Tuple2('やはり', 'やはり'),
			_Utils_Tuple2('やぶく', '破く'),
			_Utils_Tuple2('やぶる', '破る'),
			_Utils_Tuple2('やぶれる', '敗れる'),
			_Utils_Tuple2('やぶれる', '破れる'),
			_Utils_Tuple2('やまい', '病'),
			_Utils_Tuple2('やまおく', '山奥'),
			_Utils_Tuple2('やまごや', '山小屋'),
			_Utils_Tuple2('やまのぼり', '山登り'),
			_Utils_Tuple2('やまもり', '山盛り'),
			_Utils_Tuple2('やまやま', '山々'),
			_Utils_Tuple2('やむ', 'やむ'),
			_Utils_Tuple2('やめ', 'やめ'),
			_Utils_Tuple2('やめる', 'やめる'),
			_Utils_Tuple2('やめる', '辞める'),
			_Utils_Tuple2('やや', 'やや'),
			_Utils_Tuple2('やる', 'やる'),
			_Utils_Tuple2('やわらか', '柔らか'),
			_Utils_Tuple2('やわらかい', '軟らかい'),
			_Utils_Tuple2('やわらかい', '柔らかい'),
			_Utils_Tuple2('やんぐ', 'ヤング'),
			_Utils_Tuple2('ゆ', '湯'),
			_Utils_Tuple2('ゆ', '油'),
			_Utils_Tuple2('ゆいいつ', '唯一'),
			_Utils_Tuple2('ゆう', '夕'),
			_Utils_Tuple2('ゆう', '有'),
			_Utils_Tuple2('ゆうえい', '遊泳'),
			_Utils_Tuple2('ゆうえんち', '遊園地'),
			_Utils_Tuple2('ゆうがい', '有害'),
			_Utils_Tuple2('ゆうがい', '有害'),
			_Utils_Tuple2('ゆうかん', '夕刊'),
			_Utils_Tuple2('ゆうき', '勇気'),
			_Utils_Tuple2('ゆうぐれ', '夕暮れ'),
			_Utils_Tuple2('ゆうげん', '有限'),
			_Utils_Tuple2('ゆうこう', '有効'),
			_Utils_Tuple2('ゆうこう', '友好'),
			_Utils_Tuple2('ゆーざー', 'ユーザー'),
			_Utils_Tuple2('ゆうしゅう', '優秀'),
			_Utils_Tuple2('ゆうしゅう', '優秀'),
			_Utils_Tuple2('ゆうしょう', '優勝'),
			_Utils_Tuple2('ゆうじょう', '友情'),
			_Utils_Tuple2('ゆうじん', '友人'),
			_Utils_Tuple2('ゆうせん', '優先'),
			_Utils_Tuple2('ゆうせんてき', '優先的'),
			_Utils_Tuple2('ゆうそう', '郵送'),
			_Utils_Tuple2('ゆうだち', '夕立'),
			_Utils_Tuple2('ゆうのう', '有能'),
			_Utils_Tuple2('ゆうひ', '夕日'),
			_Utils_Tuple2('ゆうびん', '郵便'),
			_Utils_Tuple2('ゆうびんうけ', '郵便受け'),
			_Utils_Tuple2('ゆうびんちょきん', '郵便貯金'),
			_Utils_Tuple2('ゆうびんばんごう', '郵便番号'),
			_Utils_Tuple2('ゆうびんぶつ', '郵便物'),
			_Utils_Tuple2('ゆうべ', 'ゆうべ'),
			_Utils_Tuple2('ゆうべ', '夕べ'),
			_Utils_Tuple2('ゆーもあ', 'ユーモア'),
			_Utils_Tuple2('ゆうやけ', '夕焼け'),
			_Utils_Tuple2('ゆうり', '有利'),
			_Utils_Tuple2('ゆうりょう', '優良'),
			_Utils_Tuple2('ゆうりょう', '有料'),
			_Utils_Tuple2('ゆうりょうどうろ', '有料道路'),
			_Utils_Tuple2('ゆうりょく', '有力'),
			_Utils_Tuple2('ゆうれい', '幽霊'),
			_Utils_Tuple2('ゆか', '床'),
			_Utils_Tuple2('ゆかい', '愉快'),
			_Utils_Tuple2('ゆかた', '浴衣'),
			_Utils_Tuple2('ゆきおろし', '雪下ろし'),
			_Utils_Tuple2('ゆきぐに', '雪国'),
			_Utils_Tuple2('ゆきすぎる', '行き過ぎる'),
			_Utils_Tuple2('ゆきば', '行き場'),
			_Utils_Tuple2('ゆきまつり', '雪祭り'),
			_Utils_Tuple2('ゆくえ', '行方'),
			_Utils_Tuple2('ゆくえふめい', '行方不明'),
			_Utils_Tuple2('ゆげ', '湯気'),
			_Utils_Tuple2('ゆけつ', '輸血'),
			_Utils_Tuple2('ゆしゅつ', '輸出'),
			_Utils_Tuple2('ゆしゅつにゅう', '輸出入'),
			_Utils_Tuple2('ゆずる', '譲る'),
			_Utils_Tuple2('ゆせい', '油性'),
			_Utils_Tuple2('ゆそう', '輸送'),
			_Utils_Tuple2('ゆたか', '豊か'),
			_Utils_Tuple2('ゆだん', '油断'),
			_Utils_Tuple2('ゆっくり', 'ゆっくり'),
			_Utils_Tuple2('ゆったり', 'ゆったり'),
			_Utils_Tuple2('ゆでる', 'ゆでる'),
			_Utils_Tuple2('ゆにーく', 'ユニーク'),
			_Utils_Tuple2('ゆにほーむ', 'ユニホーム'),
			_Utils_Tuple2('ゆにゅう', '輸入'),
			_Utils_Tuple2('ゆのみ', '湯飲み'),
			_Utils_Tuple2('ゆびさき', '指先'),
			_Utils_Tuple2('ゆびさす', '指差す'),
			_Utils_Tuple2('ゆびわ', '指輪'),
			_Utils_Tuple2('ゆめ', '夢'),
			_Utils_Tuple2('ゆめうらない', '夢占い'),
			_Utils_Tuple2('ゆめみる', '夢見る'),
			_Utils_Tuple2('ゆるい', '緩い'),
			_Utils_Tuple2('ゆるす', '許す'),
			_Utils_Tuple2('ゆれ', '揺れ'),
			_Utils_Tuple2('ゆれる', '揺れる'),
			_Utils_Tuple2('よあけ', '夜明け'),
			_Utils_Tuple2('よあそび', '夜遊び'),
			_Utils_Tuple2('よい', '酔い'),
			_Utils_Tuple2('よいしょ', 'よいしょ'),
			_Utils_Tuple2('よう', '様'),
			_Utils_Tuple2('よう', '用'),
			_Utils_Tuple2('よう', '酔う'),
			_Utils_Tuple2('よう', '用'),
			_Utils_Tuple2('よう', '要'),
			_Utils_Tuple2('よう', '洋'),
			_Utils_Tuple2('よう', '様'),
			_Utils_Tuple2('よう', '要'),
			_Utils_Tuple2('ようい', '用意'),
			_Utils_Tuple2('ようい', '容易'),
			_Utils_Tuple2('ようが', '洋画'),
			_Utils_Tuple2('ようがく', '洋楽'),
			_Utils_Tuple2('ようき', '容器'),
			_Utils_Tuple2('ようき', '陽気'),
			_Utils_Tuple2('ようきゅう', '要求'),
			_Utils_Tuple2('ようぐ', '用具'),
			_Utils_Tuple2('ようけん', '用件'),
			_Utils_Tuple2('ようご', '用語'),
			_Utils_Tuple2('ようし', '用紙'),
			_Utils_Tuple2('ようし', '用紙'),
			_Utils_Tuple2('ようし', '容姿'),
			_Utils_Tuple2('ようじ', '用事'),
			_Utils_Tuple2('ようじ', '幼児'),
			_Utils_Tuple2('ようしき', '洋式'),
			_Utils_Tuple2('ようしつ', '洋室'),
			_Utils_Tuple2('ようしょ', '洋書'),
			_Utils_Tuple2('ようしょく', '洋食'),
			_Utils_Tuple2('ようじん', '用心'),
			_Utils_Tuple2('ようす', '様子'),
			_Utils_Tuple2('ようち', '幼稚'),
			_Utils_Tuple2('ようちえん', '幼稚園'),
			_Utils_Tuple2('ようちゅうい', '要注意'),
			_Utils_Tuple2('ようつう', '腰痛'),
			_Utils_Tuple2('ようてん', '要点'),
			_Utils_Tuple2('ようひん', '用品'),
			_Utils_Tuple2('ようひん', '用品'),
			_Utils_Tuple2('ようふう', '洋風'),
			_Utils_Tuple2('ようほう', '用法'),
			_Utils_Tuple2('ようやく', 'ようやく'),
			_Utils_Tuple2('ようやく', '要約'),
			_Utils_Tuple2('ようりょう', '容量'),
			_Utils_Tuple2('よーぐると', 'ヨーグルト'),
			_Utils_Tuple2('よかぜ', '夜風'),
			_Utils_Tuple2('よかん', '予感'),
			_Utils_Tuple2('よき', '予期'),
			_Utils_Tuple2('よきん', '預金'),
			_Utils_Tuple2('よく', '翌'),
			_Utils_Tuple2('よく', '欲'),
			_Utils_Tuple2('よくあさ', '翌朝'),
			_Utils_Tuple2('よくげつ', '翌月'),
			_Utils_Tuple2('よくしつ', '浴室'),
			_Utils_Tuple2('よくじつ', '翌日'),
			_Utils_Tuple2('よくしゅう', '翌週'),
			_Utils_Tuple2('よくちょう', '翌朝'),
			_Utils_Tuple2('よくねん', '翌年'),
			_Utils_Tuple2('よくばり', '欲張り'),
			_Utils_Tuple2('よくばり', '欲張り'),
			_Utils_Tuple2('よくばる', '欲張る'),
			_Utils_Tuple2('よけい', '余計'),
			_Utils_Tuple2('よけい', '余計'),
			_Utils_Tuple2('よける', 'よける'),
			_Utils_Tuple2('よこがお', '横顔'),
			_Utils_Tuple2('よこがき', '横書き'),
			_Utils_Tuple2('よこぎる', '横切る'),
			_Utils_Tuple2('よこく', '予告'),
			_Utils_Tuple2('よごす', '汚す'),
			_Utils_Tuple2('よこたわる', '横たわる'),
			_Utils_Tuple2('よこなが', '横長'),
			_Utils_Tuple2('よこむき', '横向き'),
			_Utils_Tuple2('よごれ', '汚れ'),
			_Utils_Tuple2('よごれる', '汚れる'),
			_Utils_Tuple2('よさん', '予算'),
			_Utils_Tuple2('よし', '良し'),
			_Utils_Tuple2('よし', 'よし'),
			_Utils_Tuple2('よしゅう', '予習'),
			_Utils_Tuple2('よす', 'よす'),
			_Utils_Tuple2('よせる', '寄せる'),
			_Utils_Tuple2('よせん', '予選'),
			_Utils_Tuple2('よそう', '予想'),
			_Utils_Tuple2('よそく', '予測'),
			_Utils_Tuple2('よぞら', '夜空'),
			_Utils_Tuple2('よつかど', '四つ角'),
			_Utils_Tuple2('よっきゅう', '欲求'),
			_Utils_Tuple2('よっと', 'ヨット'),
			_Utils_Tuple2('よっぱらい', '酔っ払い'),
			_Utils_Tuple2('よっぱらう', '酔っ払う'),
			_Utils_Tuple2('よてい', '予定'),
			_Utils_Tuple2('よどおし', '夜通し'),
			_Utils_Tuple2('よなか', '夜中'),
			_Utils_Tuple2('よのなか', '世の中'),
			_Utils_Tuple2('よび', '予備'),
			_Utils_Tuple2('よびかける', '呼び掛ける'),
			_Utils_Tuple2('よびこう', '予備校'),
			_Utils_Tuple2('よびすて', '呼び捨て'),
			_Utils_Tuple2('よびだし', '呼び出し'),
			_Utils_Tuple2('よびだす', '呼び出す'),
			_Utils_Tuple2('よびとめる', '呼び止める'),
			_Utils_Tuple2('よびな', '呼び名'),
			_Utils_Tuple2('よふかし', '夜更かし'),
			_Utils_Tuple2('よぶん', '余分'),
			_Utils_Tuple2('よほう', '予報'),
			_Utils_Tuple2('よぼう', '予防'),
			_Utils_Tuple2('よみ', '読み'),
			_Utils_Tuple2('よみかき', '読み書き'),
			_Utils_Tuple2('よみせ', '夜店'),
			_Utils_Tuple2('よみち', '夜道'),
			_Utils_Tuple2('よみて', '読み手'),
			_Utils_Tuple2('よみとる', '読み取る'),
			_Utils_Tuple2('よみもの', '読み物'),
			_Utils_Tuple2('よめ', '嫁'),
			_Utils_Tuple2('よやく', '予約'),
			_Utils_Tuple2('よゆう', '余裕'),
			_Utils_Tuple2('より', 'より'),
			_Utils_Tuple2('より', '寄り'),
			_Utils_Tuple2('よる', '寄る'),
			_Utils_Tuple2('よる', 'よる'),
			_Utils_Tuple2('よる', '因る'),
			_Utils_Tuple2('よろこび', '喜び'),
			_Utils_Tuple2('よろこぶ', '喜ぶ'),
			_Utils_Tuple2('よろしい', 'よろしい'),
			_Utils_Tuple2('よわき', '弱気'),
			_Utils_Tuple2('よわび', '弱火'),
			_Utils_Tuple2('よわまる', '弱まる'),
			_Utils_Tuple2('よわめる', '弱める'),
			_Utils_Tuple2('ら', 'ら'),
			_Utils_Tuple2('らい', '来'),
			_Utils_Tuple2('らい', '来'),
			_Utils_Tuple2('らいきゃく', '来客'),
			_Utils_Tuple2('らいしゅん', '来春'),
			_Utils_Tuple2('らいす', 'ライス'),
			_Utils_Tuple2('らいだー', 'ライダー'),
			_Utils_Tuple2('らいてん', '来店'),
			_Utils_Tuple2('らいと', 'ライト'),
			_Utils_Tuple2('らいにち', '来日'),
			_Utils_Tuple2('らいばる', 'ライバル'),
			_Utils_Tuple2('らいふ', 'ライフ'),
			_Utils_Tuple2('らいぶ', 'ライブ'),
			_Utils_Tuple2('らいふすたいる', 'ライフスタイル'),
			_Utils_Tuple2('らいぶらりー', 'ライブラリー'),
			_Utils_Tuple2('らいん', 'ライン'),
			_Utils_Tuple2('らく', '楽'),
			_Utils_Tuple2('らぐびー', 'ラグビー'),
			_Utils_Tuple2('らけっと', 'ラケット'),
			_Utils_Tuple2('らじおたいそう', 'ラジオ体操'),
			_Utils_Tuple2('らじかせ', 'ラジカセ'),
			_Utils_Tuple2('らすと', 'ラスト'),
			_Utils_Tuple2('らっきー', 'ラッキー'),
			_Utils_Tuple2('らっしゅ', 'ラッシュ'),
			_Utils_Tuple2('らべる', 'ラベル'),
			_Utils_Tuple2('らべんだー', 'ラベンダー'),
			_Utils_Tuple2('らん', '欄'),
			_Utils_Tuple2('らん', '卵'),
			_Utils_Tuple2('らんきんぐ', 'ランキング'),
			_Utils_Tuple2('らんく', 'ランク'),
			_Utils_Tuple2('らんどせる', 'ランドセル'),
			_Utils_Tuple2('らんどりー', 'ランドリー'),
			_Utils_Tuple2('らんなー', 'ランナー'),
			_Utils_Tuple2('らんにんぐ', 'ランニング'),
			_Utils_Tuple2('らんぼう', '乱暴'),
			_Utils_Tuple2('りーだーしっぷ', 'リーダーシップ'),
			_Utils_Tuple2('りえき', '利益'),
			_Utils_Tuple2('りか', '理科'),
			_Utils_Tuple2('りかい', '理解'),
			_Utils_Tuple2('りがい', '利害'),
			_Utils_Tuple2('りく', '陸'),
			_Utils_Tuple2('りくえすと', 'リクエスト'),
			_Utils_Tuple2('りこう', '利口'),
			_Utils_Tuple2('りこん', '離婚'),
			_Utils_Tuple2('りさーち', 'リサーチ'),
			_Utils_Tuple2('りさいくる', 'リサイクル'),
			_Utils_Tuple2('りし', '利子'),
			_Utils_Tuple2('りすく', 'リスク'),
			_Utils_Tuple2('りすとら', 'リストラ'),
			_Utils_Tuple2('りずむ', 'リズム'),
			_Utils_Tuple2('りそう', '理想'),
			_Utils_Tuple2('りそうてき', '理想的'),
			_Utils_Tuple2('りぞーと', 'リゾート'),
			_Utils_Tuple2('りそく', '利息'),
			_Utils_Tuple2('りつ', '率'),
			_Utils_Tuple2('りっち', 'リッチ'),
			_Utils_Tuple2('りっとる', 'リットル'),
			_Utils_Tuple2('りっとる', 'リットル'),
			_Utils_Tuple2('りっぱ', '立派'),
			_Utils_Tuple2('りてん', '利点'),
			_Utils_Tuple2('りぴーと', 'リピート'),
			_Utils_Tuple2('りびんぐ', 'リビング'),
			_Utils_Tuple2('りびんぐるーむ', 'リビングルーム'),
			_Utils_Tuple2('りふと', 'リフト'),
			_Utils_Tuple2('りふれっしゅ', 'リフレッシュ'),
			_Utils_Tuple2('りぼん', 'リボン'),
			_Utils_Tuple2('りもこん', 'リモコン'),
			_Utils_Tuple2('りゃくご', '略語'),
			_Utils_Tuple2('りゅう', '流'),
			_Utils_Tuple2('りゆう', '理由'),
			_Utils_Tuple2('りゅうこう', '流行'),
			_Utils_Tuple2('りゅうこうご', '流行語'),
			_Utils_Tuple2('りゅうつう', '流通'),
			_Utils_Tuple2('りゅっく', 'リュック'),
			_Utils_Tuple2('りゅっくさっく', 'リュックサック'),
			_Utils_Tuple2('りょう', '寮'),
			_Utils_Tuple2('りょう', '料'),
			_Utils_Tuple2('りょう', '量'),
			_Utils_Tuple2('りょう', '良'),
			_Utils_Tuple2('りょう', '漁'),
			_Utils_Tuple2('りょう', '両'),
			_Utils_Tuple2('りよう', '利用'),
			_Utils_Tuple2('りょうあし', '両足'),
			_Utils_Tuple2('りょううで', '両腕'),
			_Utils_Tuple2('りょうかい', '了解'),
			_Utils_Tuple2('りょうがえ', '両替'),
			_Utils_Tuple2('りょうがわ', '両側'),
			_Utils_Tuple2('りょうきん', '料金'),
			_Utils_Tuple2('りょうこう', '良好'),
			_Utils_Tuple2('りょうさん', '量産'),
			_Utils_Tuple2('りょうし', '漁師'),
			_Utils_Tuple2('りょうしゃ', '両者'),
			_Utils_Tuple2('りょうしゅうしょ', '領収書'),
			_Utils_Tuple2('りょうしん', '良心'),
			_Utils_Tuple2('りょうしんてき', '良心的'),
			_Utils_Tuple2('りょうて', '両手'),
			_Utils_Tuple2('りょうどなり', '両隣り'),
			_Utils_Tuple2('りょうひん', '良品'),
			_Utils_Tuple2('りょうほう', '両方'),
			_Utils_Tuple2('りょうめ', '両目'),
			_Utils_Tuple2('りょうめん', '両面'),
			_Utils_Tuple2('りょうりつ', '両立'),
			_Utils_Tuple2('りょうりや', '料理屋'),
			_Utils_Tuple2('りょく', '力'),
			_Utils_Tuple2('りょくちゃ', '緑茶'),
			_Utils_Tuple2('りょこうしゃ', '旅行者'),
			_Utils_Tuple2('りょっか', '緑化'),
			_Utils_Tuple2('りょひ', '旅費'),
			_Utils_Tuple2('りらっくす', 'リラックス'),
			_Utils_Tuple2('りりく', '離陸'),
			_Utils_Tuple2('りれー', 'リレー'),
			_Utils_Tuple2('りれきしょ', '履歴書'),
			_Utils_Tuple2('りん', '林'),
			_Utils_Tuple2('りん', '輪'),
			_Utils_Tuple2('りんぎょう', '林業'),
			_Utils_Tuple2('りんごく', '隣国'),
			_Utils_Tuple2('りんじ', '臨時'),
			_Utils_Tuple2('りんじん', '隣人'),
			_Utils_Tuple2('りんす', 'リンス'),
			_Utils_Tuple2('るい', '類'),
			_Utils_Tuple2('るい', '類'),
			_Utils_Tuple2('るー', 'ルー'),
			_Utils_Tuple2('るーむ', 'ルーム'),
			_Utils_Tuple2('るーむめーと', 'ルームメート'),
			_Utils_Tuple2('るーる', 'ルール'),
			_Utils_Tuple2('るす', '留守'),
			_Utils_Tuple2('るすばん', '留守番'),
			_Utils_Tuple2('るすばんでんわ', '留守番電話'),
			_Utils_Tuple2('るっくす', 'ルックス'),
			_Utils_Tuple2('れい', '零'),
			_Utils_Tuple2('れい', '礼'),
			_Utils_Tuple2('れいがい', '例外'),
			_Utils_Tuple2('れいぎ', '礼儀'),
			_Utils_Tuple2('れいぎただしい', '礼儀正しい'),
			_Utils_Tuple2('れいきん', '礼金'),
			_Utils_Tuple2('れいすい', '冷水'),
			_Utils_Tuple2('れいせい', '冷静'),
			_Utils_Tuple2('れいだんぼう', '冷暖房'),
			_Utils_Tuple2('れいてん', '零点'),
			_Utils_Tuple2('れいとう', '冷凍'),
			_Utils_Tuple2('れいとうしょくひん', '冷凍食品'),
			_Utils_Tuple2('れいねん', '例年'),
			_Utils_Tuple2('れいの', '例の'),
			_Utils_Tuple2('れいふう', '冷風'),
			_Utils_Tuple2('れいぶん', '例文'),
			_Utils_Tuple2('れいぼう', '冷房'),
			_Utils_Tuple2('れーる', 'レール'),
			_Utils_Tuple2('れきし', '歴史'),
			_Utils_Tuple2('れきしてき', '歴史的'),
			_Utils_Tuple2('れこーど', 'レコード'),
			_Utils_Tuple2('れじ', 'レジ'),
			_Utils_Tuple2('れしーと', 'レシート'),
			_Utils_Tuple2('れしぴ', 'レシピ'),
			_Utils_Tuple2('れじゃー', 'レジャー'),
			_Utils_Tuple2('れたー', 'レター'),
			_Utils_Tuple2('れたす', 'レタス'),
			_Utils_Tuple2('れつ', '列'),
			_Utils_Tuple2('れっしゃ', '列車'),
			_Utils_Tuple2('れっすん', 'レッスン'),
			_Utils_Tuple2('れっど', 'レッド'),
			_Utils_Tuple2('れっとう', '列島'),
			_Utils_Tuple2('れとると', 'レトルト'),
			_Utils_Tuple2('れふと', 'レフト'),
			_Utils_Tuple2('れべる', 'レベル'),
			_Utils_Tuple2('れべるあっぷ', 'レベルアップ'),
			_Utils_Tuple2('れぽーたー', 'レポーター'),
			_Utils_Tuple2('れぽーと', 'レポート'),
			_Utils_Tuple2('れんあい', '恋愛'),
			_Utils_Tuple2('れんあいけっこん', '恋愛結婚'),
			_Utils_Tuple2('れんきゅう', '連休'),
			_Utils_Tuple2('れんじ', 'レンジ'),
			_Utils_Tuple2('れんじつ', '連日'),
			_Utils_Tuple2('れんしょう', '連勝'),
			_Utils_Tuple2('れんず', 'レンズ'),
			_Utils_Tuple2('れんぞく', '連続'),
			_Utils_Tuple2('れんたかー', 'レンタカー'),
			_Utils_Tuple2('れんたる', 'レンタル'),
			_Utils_Tuple2('れんとげん', 'レントゲン'),
			_Utils_Tuple2('れんとげんしゃしん', 'レントゲン写真'),
			_Utils_Tuple2('れんらく', '連絡'),
			_Utils_Tuple2('ろう', '労'),
			_Utils_Tuple2('ろうか', '廊下'),
			_Utils_Tuple2('ろうか', '老化'),
			_Utils_Tuple2('ろうご', '老後'),
			_Utils_Tuple2('ろうじん', '老人'),
			_Utils_Tuple2('ろうそく', 'ろうそく'),
			_Utils_Tuple2('ろうどう', '労働'),
			_Utils_Tuple2('ろうどうしゃ', '労働者'),
			_Utils_Tuple2('ろうどうりょく', '労働力'),
			_Utils_Tuple2('ろーす', 'ロース'),
			_Utils_Tuple2('ろーず', 'ローズ'),
			_Utils_Tuple2('ろーぷ', 'ロープ'),
			_Utils_Tuple2('ろーまじ', 'ローマ字'),
			_Utils_Tuple2('ろくおん', '録音'),
			_Utils_Tuple2('ろくが', '録画'),
			_Utils_Tuple2('ろけっと', 'ロケット'),
			_Utils_Tuple2('ろじょう', '路上'),
			_Utils_Tuple2('ろせん', '路線'),
			_Utils_Tuple2('ろびー', 'ロビー'),
			_Utils_Tuple2('ろぼっと', 'ロボット'),
			_Utils_Tuple2('ろんぐ', 'ロング'),
			_Utils_Tuple2('ろんぶん', '論文'),
			_Utils_Tuple2('ろんり', '論理'),
			_Utils_Tuple2('ろんりてき', '論理的'),
			_Utils_Tuple2('わ', '話'),
			_Utils_Tuple2('わ', '和'),
			_Utils_Tuple2('わ', '羽'),
			_Utils_Tuple2('わ', '輪'),
			_Utils_Tuple2('わーく', 'ワーク'),
			_Utils_Tuple2('わーど', 'ワード'),
			_Utils_Tuple2('わいしゃつ', 'ワイシャツ'),
			_Utils_Tuple2('わいど', 'ワイド'),
			_Utils_Tuple2('わいんぐらす', 'ワイングラス'),
			_Utils_Tuple2('わえい', '和英'),
			_Utils_Tuple2('わが', '我が'),
			_Utils_Tuple2('わがくに', '我が国'),
			_Utils_Tuple2('わかす', '沸かす'),
			_Utils_Tuple2('わかて', '若手'),
			_Utils_Tuple2('わかば', '若葉'),
			_Utils_Tuple2('わがまま', 'わがまま'),
			_Utils_Tuple2('わかめ', 'わかめ'),
			_Utils_Tuple2('わかもの', '若者'),
			_Utils_Tuple2('わがや', '我が家'),
			_Utils_Tuple2('わかれ', '別れ'),
			_Utils_Tuple2('わかればなし', '別れ話'),
			_Utils_Tuple2('わかれる', '別れる'),
			_Utils_Tuple2('わかれる', '分かれる'),
			_Utils_Tuple2('わかわかしい', '若々しい'),
			_Utils_Tuple2('わぎゅう', '和牛'),
			_Utils_Tuple2('わく', '沸く'),
			_Utils_Tuple2('わく', '湧く'),
			_Utils_Tuple2('わくわく', 'わくわく'),
			_Utils_Tuple2('わけ', '訳'),
			_Utils_Tuple2('わけ', '分け'),
			_Utils_Tuple2('わけ', '分け'),
			_Utils_Tuple2('わける', '分ける'),
			_Utils_Tuple2('わごむ', '輪ゴム'),
			_Utils_Tuple2('わざと', 'わざと'),
			_Utils_Tuple2('わさび', 'わさび'),
			_Utils_Tuple2('わしき', '和式'),
			_Utils_Tuple2('わしつ', '和室'),
			_Utils_Tuple2('わしょく', '和食'),
			_Utils_Tuple2('わずか', 'わずか'),
			_Utils_Tuple2('わずか', 'わずか'),
			_Utils_Tuple2('わすれもの', '忘れ物'),
			_Utils_Tuple2('わせい', '和製'),
			_Utils_Tuple2('わた', '綿'),
			_Utils_Tuple2('わだい', '話題'),
			_Utils_Tuple2('わたす', '渡す'),
			_Utils_Tuple2('わたる', '渡る'),
			_Utils_Tuple2('わっと', 'ワット'),
			_Utils_Tuple2('わっと', 'ワット'),
			_Utils_Tuple2('わっふる', 'ワッフル'),
			_Utils_Tuple2('わに', 'ワニ'),
			_Utils_Tuple2('わふう', '和風'),
			_Utils_Tuple2('わふく', '和服'),
			_Utils_Tuple2('わらい', '笑い'),
			_Utils_Tuple2('わらいばなし', '笑い話'),
			_Utils_Tuple2('わらう', '笑う'),
			_Utils_Tuple2('わり', '割'),
			_Utils_Tuple2('わりあい', '割合'),
			_Utils_Tuple2('わりあい', '割合'),
			_Utils_Tuple2('わりかん', '割り勘'),
			_Utils_Tuple2('わりざん', '割り算'),
			_Utils_Tuple2('わりと', '割と'),
			_Utils_Tuple2('わりに', '割に'),
			_Utils_Tuple2('わりばし', '割りばし'),
			_Utils_Tuple2('わりびく', '割り引く'),
			_Utils_Tuple2('わる', '割る'),
			_Utils_Tuple2('わる', '悪'),
			_Utils_Tuple2('わるくち', '悪口'),
			_Utils_Tuple2('わるもの', '悪者'),
			_Utils_Tuple2('われもの', '割れ物'),
			_Utils_Tuple2('われる', '割れる'),
			_Utils_Tuple2('われわれ', '我々'),
			_Utils_Tuple2('わん', 'ワン'),
			_Utils_Tuple2('わん', '湾'),
			_Utils_Tuple2('わん', 'わん'),
			_Utils_Tuple2('わんたっち', 'ワンタッチ'),
			_Utils_Tuple2('わんだふる', 'ワンダフル'),
			_Utils_Tuple2('わんわん', 'わんわん'),
			_Utils_Tuple2('あ', '亜'),
			_Utils_Tuple2('あーむ', 'アーム'),
			_Utils_Tuple2('あーる', 'アール'),
			_Utils_Tuple2('あいあん', 'アイアン'),
			_Utils_Tuple2('あいえんか', '愛煙家'),
			_Utils_Tuple2('あいかた', '相方'),
			_Utils_Tuple2('あいかわらず', '相変わらず'),
			_Utils_Tuple2('あいがん', '哀願'),
			_Utils_Tuple2('あいきゅー', 'ＩＱ'),
			_Utils_Tuple2('あいきょう', '愛敬'),
			_Utils_Tuple2('あいご', '愛護'),
			_Utils_Tuple2('あいこう', '愛好'),
			_Utils_Tuple2('あいこく', '愛国'),
			_Utils_Tuple2('あいことば', '合い言葉'),
			_Utils_Tuple2('あいしゅう', '哀愁'),
			_Utils_Tuple2('あいじん', '愛人'),
			_Utils_Tuple2('あいそ', '愛想'),
			_Utils_Tuple2('あいだがら', '間柄'),
			_Utils_Tuple2('あいつぐ', '相次ぐ'),
			_Utils_Tuple2('あいてかた', '相手方'),
			_Utils_Tuple2('あいてやく', '相手役'),
			_Utils_Tuple2('あいでんてぃてぃー', 'アイデンティティー'),
			_Utils_Tuple2('あいどく', '愛読'),
			_Utils_Tuple2('あいにく', 'あいにく'),
			_Utils_Tuple2('あいぬ', 'アイヌ'),
			_Utils_Tuple2('あいのり', '相乗り'),
			_Utils_Tuple2('あいぼう', '相棒'),
			_Utils_Tuple2('あいま', '合間'),
			_Utils_Tuple2('あえぐ', '喘ぐ'),
			_Utils_Tuple2('あえて', '敢えて'),
			_Utils_Tuple2('あえもの', '和え物'),
			_Utils_Tuple2('あえる', '和える'),
			_Utils_Tuple2('あえん', '亜鉛'),
			_Utils_Tuple2('あおぐ', '仰ぐ'),
			_Utils_Tuple2('あおば', '青葉'),
			_Utils_Tuple2('あおむし', '青虫'),
			_Utils_Tuple2('あおる', 'あおる'),
			_Utils_Tuple2('あか', 'あか'),
			_Utils_Tuple2('あがく', 'あがく'),
			_Utils_Tuple2('あかけっきゅう', '赤血球'),
			_Utils_Tuple2('あかし', 'あかし'),
			_Utils_Tuple2('あかじゅうじ', '赤十字'),
			_Utils_Tuple2('あかす', '明かす'),
			_Utils_Tuple2('あかつき', '暁'),
			_Utils_Tuple2('あかみ', '赤身'),
			_Utils_Tuple2('あかみ', '赤み'),
			_Utils_Tuple2('あからさま', 'あからさま'),
			_Utils_Tuple2('あがりこむ', '上がり込む'),
			_Utils_Tuple2('あがる', '挙がる'),
			_Utils_Tuple2('あかるみ', '明るみ'),
			_Utils_Tuple2('あきす', '空き巣'),
			_Utils_Tuple2('あきばれ', '秋晴れ'),
			_Utils_Tuple2('あく', 'あく'),
			_Utils_Tuple2('あくじゅんかん', '悪循環'),
			_Utils_Tuple2('あくせく', 'あくせく'),
			_Utils_Tuple2('あくたい', '悪態'),
			_Utils_Tuple2('あくとう', '悪党'),
			_Utils_Tuple2('あくとく', '悪徳'),
			_Utils_Tuple2('あくび', 'あくび'),
			_Utils_Tuple2('あくまで', '飽くまで'),
			_Utils_Tuple2('あくめい', '悪名'),
			_Utils_Tuple2('あくめいたかい', '悪名高い'),
			_Utils_Tuple2('あくゆう', '悪友'),
			_Utils_Tuple2('あくりょく', '握力'),
			_Utils_Tuple2('あくりる', 'アクリル'),
			_Utils_Tuple2('あけ', '明け'),
			_Utils_Tuple2('あげ', '上げ'),
			_Utils_Tuple2('あげあし', '揚げ足'),
			_Utils_Tuple2('あげく', '挙げ句'),
			_Utils_Tuple2('あけくれる', '明け暮れる'),
			_Utils_Tuple2('あけぼの', '曙'),
			_Utils_Tuple2('あこーでぃおん', 'アコーディオン'),
			_Utils_Tuple2('あさ', '麻'),
			_Utils_Tuple2('あざ', 'あざ'),
			_Utils_Tuple2('あさがお', '朝顔'),
			_Utils_Tuple2('あさづけ', '浅漬け'),
			_Utils_Tuple2('あさはか', '浅はか'),
			_Utils_Tuple2('あざむく', '欺く'),
			_Utils_Tuple2('あさり', 'あさり'),
			_Utils_Tuple2('あさり', '歯振'),
			_Utils_Tuple2('あさる', 'あさる'),
			_Utils_Tuple2('あしがかり', '足掛かり'),
			_Utils_Tuple2('あじきない', '味気無い'),
			_Utils_Tuple2('あしば', '足場'),
			_Utils_Tuple2('あしらう', 'あしらう'),
			_Utils_Tuple2('あじわい', '味わい'),
			_Utils_Tuple2('あずき', '小豆'),
			_Utils_Tuple2('あずさ', 'あずさ'),
			_Utils_Tuple2('あせり', '焦り'),
			_Utils_Tuple2('あせる', '焦る'),
			_Utils_Tuple2('あせる', 'あせる'),
			_Utils_Tuple2('あぜん', 'あぜん'),
			_Utils_Tuple2('あだ', 'あだ'),
			_Utils_Tuple2('あたいする', '値する'),
			_Utils_Tuple2('あたかも', 'あたかも'),
			_Utils_Tuple2('あたふた', 'あたふた'),
			_Utils_Tuple2('あたまから', '頭から'),
			_Utils_Tuple2('あたりさわり', '当たり障り'),
			_Utils_Tuple2('あつ', '圧'),
			_Utils_Tuple2('あつかい', '扱い'),
			_Utils_Tuple2('あつがみ', '厚紙'),
			_Utils_Tuple2('あつくるしい', '熱苦しい'),
			_Utils_Tuple2('あっけない', 'あっけない'),
			_Utils_Tuple2('あっしゅく', '圧縮'),
			_Utils_Tuple2('あっしょう', '圧勝'),
			_Utils_Tuple2('あっせん', 'あっせん'),
			_Utils_Tuple2('あっとう', '圧倒'),
			_Utils_Tuple2('あっぱく', '圧迫'),
			_Utils_Tuple2('あっぷりけ', 'アップリケ'),
			_Utils_Tuple2('あつりょくなべ', '圧力鍋'),
			_Utils_Tuple2('あて', '当て'),
			_Utils_Tuple2('あてじ', '当て字'),
			_Utils_Tuple2('あてな', 'あて名'),
			_Utils_Tuple2('あてにげ', '当て逃げ'),
			_Utils_Tuple2('あてはまる', '当てはまる'),
			_Utils_Tuple2('あとあと', '後々'),
			_Utils_Tuple2('あとさき', '後先'),
			_Utils_Tuple2('あとつぎ', '跡継ぎ'),
			_Utils_Tuple2('あととり', '跡取り'),
			_Utils_Tuple2('あなうめ', '穴埋め'),
			_Utils_Tuple2('あながち', 'あながち'),
			_Utils_Tuple2('あなば', '穴場'),
			_Utils_Tuple2('あにき', '兄貴'),
			_Utils_Tuple2('あにでし', '兄弟子'),
			_Utils_Tuple2('あねったい', '亜熱帯'),
			_Utils_Tuple2('あばく', '暴く'),
			_Utils_Tuple2('あぱれる', 'アパレル'),
			_Utils_Tuple2('あびせる', '浴びせる'),
			_Utils_Tuple2('あぶらあげ', '油揚げ'),
			_Utils_Tuple2('あぶらえ', '油絵'),
			_Utils_Tuple2('あぷりけーしょん', 'アプリケーション'),
			_Utils_Tuple2('あぶる', '炙る'),
			_Utils_Tuple2('あぷろーち', 'アプローチ'),
			_Utils_Tuple2('あべっく', 'アベック'),
			_Utils_Tuple2('あへん', '阿片'),
			_Utils_Tuple2('あほう', '阿呆'),
			_Utils_Tuple2('あぼかど', 'アボカド'),
			_Utils_Tuple2('あま', '尼'),
			_Utils_Tuple2('あまざけ', '甘酒'),
			_Utils_Tuple2('あまじお', '甘塩'),
			_Utils_Tuple2('あまず', '甘酢'),
			_Utils_Tuple2('あまど', '雨戸'),
			_Utils_Tuple2('あまみず', '雨水'),
			_Utils_Tuple2('あまもり', '雨漏り'),
			_Utils_Tuple2('あまりもの', '余り物'),
			_Utils_Tuple2('あまんずる', '甘んずる'),
			_Utils_Tuple2('あみ', '網'),
			_Utils_Tuple2('あみだす', '編み出す'),
			_Utils_Tuple2('あみのさん', 'アミノ酸'),
			_Utils_Tuple2('あみのめ', '網の目'),
			_Utils_Tuple2('あやす', 'あやす'),
			_Utils_Tuple2('あやつる', '操る'),
			_Utils_Tuple2('あやぶむ', '危ぶむ'),
			_Utils_Tuple2('あやふや', 'あやふや'),
			_Utils_Tuple2('あゆ', 'あゆ'),
			_Utils_Tuple2('あゆ', '阿諛'),
			_Utils_Tuple2('あゆみ', '歩み'),
			_Utils_Tuple2('あゆみよる', '歩み寄る'),
			_Utils_Tuple2('あゆむ', '歩む'),
			_Utils_Tuple2('あら', 'あら'),
			_Utils_Tuple2('あらあらしい', '荒々しい'),
			_Utils_Tuple2('あらす', '荒らす'),
			_Utils_Tuple2('あらすじ', '粗筋'),
			_Utils_Tuple2('あらたまる', '改まる'),
			_Utils_Tuple2('あらため', '改め'),
			_Utils_Tuple2('あらっぽい', '荒っぽい'),
			_Utils_Tuple2('あらなみ', '荒波'),
			_Utils_Tuple2('あらびき', '粗びき'),
			_Utils_Tuple2('あらわ', '露わ'),
			_Utils_Tuple2('あらわす', '表す'),
			_Utils_Tuple2('あらわれる', '表れる'),
			_Utils_Tuple2('ありきたり', '在り来り'),
			_Utils_Tuple2('ありさま', '有様'),
			_Utils_Tuple2('ありばい', 'アリバイ'),
			_Utils_Tuple2('ありふれる', '有り触れる'),
			_Utils_Tuple2('あるかり', 'アルカリ'),
			_Utils_Tuple2('あるかりせい', 'アルカリ性'),
			_Utils_Tuple2('あるみにうむ', 'アルミニウム'),
			_Utils_Tuple2('あるみはく', 'アルミ箔'),
			_Utils_Tuple2('あれくるう', '荒れ狂う'),
			_Utils_Tuple2('あれち', '荒れ地'),
			_Utils_Tuple2('あれる', '荒れる'),
			_Utils_Tuple2('あれんじ', 'アレンジ'),
			_Utils_Tuple2('あろえ', 'アロエ'),
			_Utils_Tuple2('あろは', 'アロハ'),
			_Utils_Tuple2('あわい', '淡い'),
			_Utils_Tuple2('あわさる', '合わさる'),
			_Utils_Tuple2('あわす', '合わす'),
			_Utils_Tuple2('あわせ', '合わせ'),
			_Utils_Tuple2('あわだつ', '泡立つ'),
			_Utils_Tuple2('あわだてる', '泡立てる'),
			_Utils_Tuple2('あわてふためく', '慌てふためく'),
			_Utils_Tuple2('あわれ', '哀れ'),
			_Utils_Tuple2('あわれみ', '哀れみ'),
			_Utils_Tuple2('あわれむ', '哀れむ'),
			_Utils_Tuple2('あんかー', 'アンカー'),
			_Utils_Tuple2('あんかけ', 'あんかけ'),
			_Utils_Tuple2('あんぎゃ', '行脚'),
			_Utils_Tuple2('あんぐる', 'アングル'),
			_Utils_Tuple2('あんごう', '暗号'),
			_Utils_Tuple2('あんこく', '暗黒'),
			_Utils_Tuple2('あんさつ', '暗殺'),
			_Utils_Tuple2('あんざん', '安産'),
			_Utils_Tuple2('あんさんぶる', 'アンサンブル'),
			_Utils_Tuple2('あんじ', '暗示'),
			_Utils_Tuple2('あんじる', '案じる'),
			_Utils_Tuple2('あんずる', '案ずる'),
			_Utils_Tuple2('あんぜんほしょう', '安全保障'),
			_Utils_Tuple2('あんぜんほしょうじょうやく', '安全保障条約'),
			_Utils_Tuple2('あんだ', '安打'),
			_Utils_Tuple2('あんだー', 'アンダー'),
			_Utils_Tuple2('あんたい', '安泰'),
			_Utils_Tuple2('あんち', 'アンチ'),
			_Utils_Tuple2('あんち', '安置'),
			_Utils_Tuple2('あんてぃーく', 'アンティーク'),
			_Utils_Tuple2('あんのじょう', '案の定'),
			_Utils_Tuple2('あんぷ', 'アンプ'),
			_Utils_Tuple2('あんぺあ', 'アンペア'),
			_Utils_Tuple2('あんみん', '安眠'),
			_Utils_Tuple2('あんもく', '暗黙'),
			_Utils_Tuple2('あんもにあ', 'アンモニア'),
			_Utils_Tuple2('あんらく', '安楽'),
			_Utils_Tuple2('いあつ', '威圧'),
			_Utils_Tuple2('いあわせる', '居合わせる'),
			_Utils_Tuple2('いあん', '慰安'),
			_Utils_Tuple2('いいあてる', '言い当てる'),
			_Utils_Tuple2('いーじー', 'イージー'),
			_Utils_Tuple2('いーすたー', 'イースター'),
			_Utils_Tuple2('いいだす', '言い出す'),
			_Utils_Tuple2('いいつける', '言い付ける'),
			_Utils_Tuple2('いいつたえ', '言い伝え'),
			_Utils_Tuple2('いいつたえる', '言い伝える'),
			_Utils_Tuple2('いいはる', '言い張る'),
			_Utils_Tuple2('いいぶん', '言い分'),
			_Utils_Tuple2('いいまわし', '言い回し'),
			_Utils_Tuple2('いいわたす', '言い渡す'),
			_Utils_Tuple2('いえがら', '家柄'),
			_Utils_Tuple2('いえじ', '家路'),
			_Utils_Tuple2('いえもと', '家元'),
			_Utils_Tuple2('いおう', '硫黄'),
			_Utils_Tuple2('いかがわしい', 'いかがわしい'),
			_Utils_Tuple2('いかく', '威嚇'),
			_Utils_Tuple2('いかなる', 'いかなる'),
			_Utils_Tuple2('いかに', 'いかに'),
			_Utils_Tuple2('いかにも', 'いかにも'),
			_Utils_Tuple2('いかり', 'いかり'),
			_Utils_Tuple2('いかれる', 'いかれる'),
			_Utils_Tuple2('いかん', 'いかん'),
			_Utils_Tuple2('いかん', '遺憾'),
			_Utils_Tuple2('いき', '域'),
			_Utils_Tuple2('いき', '意気'),
			_Utils_Tuple2('いき', '遺棄'),
			_Utils_Tuple2('いき', '粋'),
			_Utils_Tuple2('いきあたり', '行き当たり'),
			_Utils_Tuple2('いきうめ', '生き埋め'),
			_Utils_Tuple2('いきこみ', '意気込み'),
			_Utils_Tuple2('いきさつ', '経緯'),
			_Utils_Tuple2('いきづまり', '行き詰まり'),
			_Utils_Tuple2('いきづまる', '行き詰まる'),
			_Utils_Tuple2('いきどおり', '憤り'),
			_Utils_Tuple2('いきとどく', '行き届く'),
			_Utils_Tuple2('いきのこり', '生き残り'),
			_Utils_Tuple2('いきのこる', '生き残る'),
			_Utils_Tuple2('いきのびる', '生き延びる'),
			_Utils_Tuple2('いく', '幾'),
			_Utils_Tuple2('いくさ', '戦'),
			_Utils_Tuple2('いくせい', '育成'),
			_Utils_Tuple2('いくた', '幾多'),
			_Utils_Tuple2('いくど', '幾度'),
			_Utils_Tuple2('いくぶん', '幾分'),
			_Utils_Tuple2('いこい', '憩い'),
			_Utils_Tuple2('いこつ', '遺骨'),
			_Utils_Tuple2('いざ', 'いざ'),
			_Utils_Tuple2('いさぎよい', '潔い'),
			_Utils_Tuple2('いさく', '遺作'),
			_Utils_Tuple2('いざこざ', 'いざこざ'),
			_Utils_Tuple2('いささか', 'いささか'),
			_Utils_Tuple2('いさましい', '勇ましい'),
			_Utils_Tuple2('いさんそうぞく', '遺産相続'),
			_Utils_Tuple2('いじ', '意地'),
			_Utils_Tuple2('いしがき', '石垣'),
			_Utils_Tuple2('いしころ', '石ころ'),
			_Utils_Tuple2('いしだん', '石段'),
			_Utils_Tuple2('いしひょうじ', '意思表示'),
			_Utils_Tuple2('いしゃりょう', '慰謝料'),
			_Utils_Tuple2('いしゅう', '異臭'),
			_Utils_Tuple2('いしゅく', '萎縮'),
			_Utils_Tuple2('いしょ', '遺書'),
			_Utils_Tuple2('いしょう', '衣装'),
			_Utils_Tuple2('いしょく', '異色'),
			_Utils_Tuple2('いしょく', '衣食'),
			_Utils_Tuple2('いじらしい', 'いじらしい'),
			_Utils_Tuple2('いじる', '弄る'),
			_Utils_Tuple2('いじわるい', '意地悪い'),
			_Utils_Tuple2('いじん', '異人'),
			_Utils_Tuple2('いじん', '偉人'),
			_Utils_Tuple2('いずこ', 'いずこ'),
			_Utils_Tuple2('いずれ', 'いずれ'),
			_Utils_Tuple2('いずれ', 'いずれ'),
			_Utils_Tuple2('いずれ', 'どれ'),
			_Utils_Tuple2('いずれも', 'いずれも'),
			_Utils_Tuple2('いすわる', '居座る'),
			_Utils_Tuple2('いせき', '移籍'),
			_Utils_Tuple2('いそ', 'いそ'),
			_Utils_Tuple2('いそいそ', 'いそいそ'),
			_Utils_Tuple2('いぞく', '遺族'),
			_Utils_Tuple2('いたく', '委託'),
			_Utils_Tuple2('いただき', '頂'),
			_Utils_Tuple2('いただきもの', 'いただき物'),
			_Utils_Tuple2('いたって', '至って'),
			_Utils_Tuple2('いたで', '痛手'),
			_Utils_Tuple2('いたのま', '板の間'),
			_Utils_Tuple2('いたまえ', '板前'),
			_Utils_Tuple2('いたましい', '痛ましい'),
			_Utils_Tuple2('いたみ', '傷み'),
			_Utils_Tuple2('いたむ', '傷む'),
			_Utils_Tuple2('いたり', '至り'),
			_Utils_Tuple2('いちいち', '一々'),
			_Utils_Tuple2('いちがいに', '一概に'),
			_Utils_Tuple2('いちがんれふ', '一眼レフ'),
			_Utils_Tuple2('いちぎょう', '一行'),
			_Utils_Tuple2('いちぐん', '一軍/１軍'),
			_Utils_Tuple2('いちげき', '一撃'),
			_Utils_Tuple2('いちげん', '一見'),
			_Utils_Tuple2('いちごいちえ', '一期一会'),
			_Utils_Tuple2('いちこじん', '一個人'),
			_Utils_Tuple2('いちじに', '一時に'),
			_Utils_Tuple2('いちじに', '一時に'),
			_Utils_Tuple2('いちず', 'いちず'),
			_Utils_Tuple2('いちだい', '一大'),
			_Utils_Tuple2('いちつける', '位置づける'),
			_Utils_Tuple2('いちばんのり', '一番乗り'),
			_Utils_Tuple2('いちぼう', '一望'),
			_Utils_Tuple2('いちみ', '一味'),
			_Utils_Tuple2('いちむかし', '一昔'),
			_Utils_Tuple2('いちめい', '一命'),
			_Utils_Tuple2('いちもく', '一目'),
			_Utils_Tuple2('いちもくさん', '一目散'),
			_Utils_Tuple2('いちもくりょうぜん', '一目瞭然'),
			_Utils_Tuple2('いちや', '一夜'),
			_Utils_Tuple2('いちゃもん', 'いちゃもん'),
			_Utils_Tuple2('いちょう', 'いちょう'),
			_Utils_Tuple2('いちょう', '医長'),
			_Utils_Tuple2('いちよう', '一様'),
			_Utils_Tuple2('いちらん', '一覧'),
			_Utils_Tuple2('いちるい', '一塁'),
			_Utils_Tuple2('いちるい', '一塁'),
			_Utils_Tuple2('いちれい', '一礼'),
			_Utils_Tuple2('いちれん', '一連'),
			_Utils_Tuple2('いちれん', '一連'),
			_Utils_Tuple2('いっかく', '一角'),
			_Utils_Tuple2('いっかくせんきん', '一獲千金'),
			_Utils_Tuple2('いっかつ', '一括'),
			_Utils_Tuple2('いっかん', '一貫'),
			_Utils_Tuple2('いっかん', '一環'),
			_Utils_Tuple2('いっき', '一気'),
			_Utils_Tuple2('いっこう', '一行'),
			_Utils_Tuple2('いっこう', '一向'),
			_Utils_Tuple2('いっこうに', '一向に'),
			_Utils_Tuple2('いっこく', '一刻'),
			_Utils_Tuple2('いっしん', '一心'),
			_Utils_Tuple2('いっすい', '一睡'),
			_Utils_Tuple2('いっする', '逸する'),
			_Utils_Tuple2('いっすん', '一寸'),
			_Utils_Tuple2('いっせい', '一斉'),
			_Utils_Tuple2('いっせきにちょう', '一石二鳥'),
			_Utils_Tuple2('いっせつ', '一説'),
			_Utils_Tuple2('いっせん', '一線'),
			_Utils_Tuple2('いっそ', 'いっそ'),
			_Utils_Tuple2('いったい', '一帯'),
			_Utils_Tuple2('いったん', '一端'),
			_Utils_Tuple2('いっちょう', '一丁'),
			_Utils_Tuple2('いって', '一手'),
			_Utils_Tuple2('いっとき', '一時'),
			_Utils_Tuple2('いっぷたさい', '一夫多妻'),
			_Utils_Tuple2('いっぺん', '一変'),
			_Utils_Tuple2('いっぺん', '一遍'),
			_Utils_Tuple2('いっぺんに', '一遍に'),
			_Utils_Tuple2('いっぽう', '一報'),
			_Utils_Tuple2('いつわる', '偽る'),
			_Utils_Tuple2('いでおろぎー', 'イデオロギー'),
			_Utils_Tuple2('いでんし', '遺伝子'),
			_Utils_Tuple2('いと', '意図'),
			_Utils_Tuple2('いど', '緯度'),
			_Utils_Tuple2('いとう', 'いとう'),
			_Utils_Tuple2('いどう', '異動'),
			_Utils_Tuple2('いとぐち', '糸口'),
			_Utils_Tuple2('いとしい', 'いとしい'),
			_Utils_Tuple2('いとなみ', '営み'),
			_Utils_Tuple2('いとなむ', '営む'),
			_Utils_Tuple2('いどむ', '挑む'),
			_Utils_Tuple2('いとも', 'いとも'),
			_Utils_Tuple2('いなさく', '稲作'),
			_Utils_Tuple2('いなずま', '稲妻'),
			_Utils_Tuple2('いなり', 'いなり'),
			_Utils_Tuple2('いにしえ', 'いにしえ'),
			_Utils_Tuple2('いにん', '委任'),
			_Utils_Tuple2('いにんじょう', '委任状'),
			_Utils_Tuple2('いのちがけ', '命懸け'),
			_Utils_Tuple2('いばる', '威張る'),
			_Utils_Tuple2('いびき', 'いびき'),
			_Utils_Tuple2('いびつ', 'いびつ'),
			_Utils_Tuple2('いびる', 'いびる'),
			_Utils_Tuple2('いぶ', 'イブ'),
			_Utils_Tuple2('いぶくろ', '胃袋'),
			_Utils_Tuple2('いぶつ', '異物'),
			_Utils_Tuple2('いぼ', 'いぼ'),
			_Utils_Tuple2('いぼ', '異母'),
			_Utils_Tuple2('いほうじん', '異邦人'),
			_Utils_Tuple2('いまさら', '今更'),
			_Utils_Tuple2('いましめる', '戒める'),
			_Utils_Tuple2('いまや', '今や'),
			_Utils_Tuple2('いみあい', '意味合い'),
			_Utils_Tuple2('いやがらせ', '嫌がらせ'),
			_Utils_Tuple2('いやけ', '嫌気'),
			_Utils_Tuple2('いやし', '癒し'),
			_Utils_Tuple2('いやす', '癒す'),
			_Utils_Tuple2('いやらしい', '嫌らしい'),
			_Utils_Tuple2('いよう', '異様'),
			_Utils_Tuple2('いらだつ', 'いらだつ'),
			_Utils_Tuple2('いりまじる', '入り交じる'),
			_Utils_Tuple2('いりょく', '威力'),
			_Utils_Tuple2('いる', '射る'),
			_Utils_Tuple2('いる', '炒る'),
			_Utils_Tuple2('いるか', 'いるか'),
			_Utils_Tuple2('いれい', '異例'),
			_Utils_Tuple2('いれかわり', '入れ替わり'),
			_Utils_Tuple2('いれかわる', '入れ代わる'),
			_Utils_Tuple2('いれこむ', '入れ込む'),
			_Utils_Tuple2('いれちがう', '入れ違う'),
			_Utils_Tuple2('いれば', '入れ歯'),
			_Utils_Tuple2('いろあい', '色合い'),
			_Utils_Tuple2('いろおち', '色落ち'),
			_Utils_Tuple2('いろがら', '色柄'),
			_Utils_Tuple2('いろけ', '色気'),
			_Utils_Tuple2('いろこい', '色濃い'),
			_Utils_Tuple2('いろどり', '彩り'),
			_Utils_Tuple2('いろどる', '彩る'),
			_Utils_Tuple2('いろなおし', '色直し'),
			_Utils_Tuple2('いろは', 'いろは'),
			_Utils_Tuple2('いろめがね', '色眼鏡'),
			_Utils_Tuple2('いろもの', '色物'),
			_Utils_Tuple2('いろり', 'いろり'),
			_Utils_Tuple2('いろわけ', '色分け'),
			_Utils_Tuple2('いろん', '異論'),
			_Utils_Tuple2('いわく', 'いわく'),
			_Utils_Tuple2('いわし', 'いわし'),
			_Utils_Tuple2('いわば', '言わば'),
			_Utils_Tuple2('いわはだ', '岩肌'),
			_Utils_Tuple2('いわゆる', 'いわゆる'),
			_Utils_Tuple2('いわれ', 'いわれ'),
			_Utils_Tuple2('いんが', '因果'),
			_Utils_Tuple2('いんがかんけい', '因果関係'),
			_Utils_Tuple2('いんかんしょうめい', '印鑑証明'),
			_Utils_Tuple2('いんき', '陰気'),
			_Utils_Tuple2('いんし', '印紙'),
			_Utils_Tuple2('いんじ', '印字'),
			_Utils_Tuple2('いんぜい', '印税'),
			_Utils_Tuple2('いんそつ', '引率'),
			_Utils_Tuple2('いんちき', 'いんちき'),
			_Utils_Tuple2('いんでぃあん', 'インディアン'),
			_Utils_Tuple2('いんでっくす', 'インデックス'),
			_Utils_Tuple2('いんてり', 'インテリ'),
			_Utils_Tuple2('いんどよう', 'インド洋'),
			_Utils_Tuple2('いんとろ', 'イントロ'),
			_Utils_Tuple2('いんなー', 'インナー'),
			_Utils_Tuple2('いんねん', '因縁'),
			_Utils_Tuple2('いんぷっと', 'インプット'),
			_Utils_Tuple2('いんぼう', '陰謀'),
			_Utils_Tuple2('いんもう', '陰毛'),
			_Utils_Tuple2('いんよう', '引用'),
			_Utils_Tuple2('いんりょく', '引力'),
			_Utils_Tuple2('う', 'ウ'),
			_Utils_Tuple2('ういういしい', '初々しい'),
			_Utils_Tuple2('ういにんぐ', 'ウイニング'),
			_Utils_Tuple2('ういろう', 'ういろう'),
			_Utils_Tuple2('ういんかー', 'ウインカー'),
			_Utils_Tuple2('ういんぐ', 'ウイング'),
			_Utils_Tuple2('ういんど', 'ウインド'),
			_Utils_Tuple2('うえーと', 'ウエート'),
			_Utils_Tuple2('うえすたん', 'ウエスタン'),
			_Utils_Tuple2('うえつける', '植え付ける'),
			_Utils_Tuple2('うえっとすーつ', 'ウエットスーツ'),
			_Utils_Tuple2('うえる', '飢える'),
			_Utils_Tuple2('うおうさおう', '右往左往'),
			_Utils_Tuple2('うおつり', '魚釣り'),
			_Utils_Tuple2('うかい', 'う回'),
			_Utils_Tuple2('うかがい', '伺い'),
			_Utils_Tuple2('うかがう', 'うかがう'),
			_Utils_Tuple2('うかびあがる', '浮かび上がる'),
			_Utils_Tuple2('うき', '浮き'),
			_Utils_Tuple2('うきうき', '浮き浮き'),
			_Utils_Tuple2('うきぼり', '浮き彫り'),
			_Utils_Tuple2('うけおう', '請け負う'),
			_Utils_Tuple2('うけぐち', '受け口'),
			_Utils_Tuple2('うけざら', '受け皿'),
			_Utils_Tuple2('うさんくさい', 'うさんくさい'),
			_Utils_Tuple2('うじゃうじゃ', 'うじゃうじゃ'),
			_Utils_Tuple2('うしろだて', '後ろ盾'),
			_Utils_Tuple2('うしろめたい', '後ろめたい'),
			_Utils_Tuple2('うず', '渦'),
			_Utils_Tuple2('うすうす', 'うすうす'),
			_Utils_Tuple2('うすかわ', '薄皮'),
			_Utils_Tuple2('うすくち', '薄口'),
			_Utils_Tuple2('うすっぺら', '薄っぺら'),
			_Utils_Tuple2('うすで', '薄手'),
			_Utils_Tuple2('うずまき', '渦巻き'),
			_Utils_Tuple2('うずめる', 'うずめる'),
			_Utils_Tuple2('うずもれる', 'うずもれる'),
			_Utils_Tuple2('うすやき', '薄焼き'),
			_Utils_Tuple2('うずら', 'うずら'),
			_Utils_Tuple2('うすらぐ', '薄らぐ'),
			_Utils_Tuple2('うすれる', '薄れる'),
			_Utils_Tuple2('うだうだ', 'うだうだ'),
			_Utils_Tuple2('うたがわしい', '疑わしい'),
			_Utils_Tuple2('うたひめ', '歌姫'),
			_Utils_Tuple2('うち', '打ち'),
			_Utils_Tuple2('うちかつ', '打ち勝つ'),
			_Utils_Tuple2('うちきり', '打ち切り'),
			_Utils_Tuple2('うちきる', '打ち切る'),
			_Utils_Tuple2('うちけし', '打ち消し'),
			_Utils_Tuple2('うちけす', '打ち消す'),
			_Utils_Tuple2('うちこむ', '打ち込む'),
			_Utils_Tuple2('うちだす', '打ち出す'),
			_Utils_Tuple2('うちたてる', '打ち立てる'),
			_Utils_Tuple2('うちとける', '打ち解ける'),
			_Utils_Tuple2('うちまた', '内股'),
			_Utils_Tuple2('うちやぶる', '打ち破る'),
			_Utils_Tuple2('うちょうてん', '有頂天'),
			_Utils_Tuple2('うちわ', '内輪'),
			_Utils_Tuple2('うちわけ', '内訳'),
			_Utils_Tuple2('うっすら', 'うっすら'),
			_Utils_Tuple2('うっとうしい', 'うっとうしい'),
			_Utils_Tuple2('うっとり', 'うっとり'),
			_Utils_Tuple2('うっぷん', 'うっぷん'),
			_Utils_Tuple2('うつらうつら', 'うつらうつら'),
			_Utils_Tuple2('うでたてふせ', '腕立て伏せ'),
			_Utils_Tuple2('うとい', '疎い'),
			_Utils_Tuple2('うながす', '促す'),
			_Utils_Tuple2('うば', '乳母'),
			_Utils_Tuple2('うぶ', '初'),
			_Utils_Tuple2('うぶごえ', '産声'),
			_Utils_Tuple2('うむ', 'うむ'),
			_Utils_Tuple2('うめこむ', '埋め込む'),
			_Utils_Tuple2('うもう', '羽毛'),
			_Utils_Tuple2('うもれる', '埋もれる'),
			_Utils_Tuple2('うやまう', '敬う'),
			_Utils_Tuple2('うよく', '右翼'),
			_Utils_Tuple2('うよくだんたい', '右翼団体'),
			_Utils_Tuple2('うらかた', '裏方'),
			_Utils_Tuple2('うらぐち', '裏口'),
			_Utils_Tuple2('うらじ', '裏地'),
			_Utils_Tuple2('うらづける', '裏付ける'),
			_Utils_Tuple2('うらどおり', '裏通り'),
			_Utils_Tuple2('うらばなし', '裏話'),
			_Utils_Tuple2('うり', '瓜'),
			_Utils_Tuple2('うりさばく', '売りさばく'),
			_Utils_Tuple2('うりだし', '売り出し'),
			_Utils_Tuple2('うりて', '売り手'),
			_Utils_Tuple2('うりぬし', '売り主'),
			_Utils_Tuple2('うりはらう', '売り払う'),
			_Utils_Tuple2('うるおい', '潤い'),
			_Utils_Tuple2('うるおう', '潤う'),
			_Utils_Tuple2('うるわしい', '麗しい'),
			_Utils_Tuple2('うれい', '憂い'),
			_Utils_Tuple2('うれる', '熟れる'),
			_Utils_Tuple2('うわくちびる', '上唇'),
			_Utils_Tuple2('うわのせ', '上乗せ'),
			_Utils_Tuple2('うわのそら', 'うわのそら'),
			_Utils_Tuple2('うわばき', '上履き'),
			_Utils_Tuple2('うわむく', '上向く'),
			_Utils_Tuple2('うわやく', '上役'),
			_Utils_Tuple2('うんざり', 'うんざり'),
			_Utils_Tuple2('うんぬん', 'うんぬん'),
			_Utils_Tuple2('うんぱん', '運搬'),
			_Utils_Tuple2('うんゆ', '運輸'),
			_Utils_Tuple2('うんゆしょう', '運輸省'),
			_Utils_Tuple2('えい', 'えい'),
			_Utils_Tuple2('えいこう', '栄光'),
			_Utils_Tuple2('えいぞく', '永続'),
			_Utils_Tuple2('えいようか', '栄養価'),
			_Utils_Tuple2('えいようそ', '栄養素'),
			_Utils_Tuple2('えいようぶん', '栄養分'),
			_Utils_Tuple2('えいり', '営利'),
			_Utils_Tuple2('えーでぃー', 'ＡＤ'),
			_Utils_Tuple2('えがら', '絵柄'),
			_Utils_Tuple2('えき', '易'),
			_Utils_Tuple2('えきさいと', 'エキサイト'),
			_Utils_Tuple2('えきしゃ', '駅舎'),
			_Utils_Tuple2('えきしょう', '液晶'),
			_Utils_Tuple2('えきじょう', '液状'),
			_Utils_Tuple2('えきす', 'エキス'),
			_Utils_Tuple2('えきすとら', 'エキストラ'),
			_Utils_Tuple2('えきすぱーと', 'エキスパート'),
			_Utils_Tuple2('えぐい', 'えぐい'),
			_Utils_Tuple2('えくぼ', 'えくぼ'),
			_Utils_Tuple2('えぐる', 'えぐる'),
			_Utils_Tuple2('えげつない', 'えげつない'),
			_Utils_Tuple2('えご', 'エゴ'),
			_Utils_Tuple2('えこー', 'エコー'),
			_Utils_Tuple2('えすえる', 'ＳＬ'),
			_Utils_Tuple2('えすかれーと', 'エスカレート'),
			_Utils_Tuple2('えすけーぷ', 'エスケープ'),
			_Utils_Tuple2('えすぴー', 'ＳＰ'),
			_Utils_Tuple2('えだげ', '枝毛'),
			_Utils_Tuple2('えだわかれ', '枝分かれ'),
			_Utils_Tuple2('えっくすせん', 'Ｘ線'),
			_Utils_Tuple2('えづけ', 'え付け'),
			_Utils_Tuple2('えっじ', 'エッジ'),
			_Utils_Tuple2('えっせいすと', 'エッセイスト'),
			_Utils_Tuple2('えっせんす', 'エッセンス'),
			_Utils_Tuple2('えつらん', '閲覧'),
			_Utils_Tuple2('えと', '干支'),
			_Utils_Tuple2('えとせとら', 'エトセトラ'),
			_Utils_Tuple2('えどっこ', '江戸っ子'),
			_Utils_Tuple2('えなめる', 'エナメル'),
			_Utils_Tuple2('えま', '絵馬'),
			_Utils_Tuple2('えまき', '絵巻'),
			_Utils_Tuple2('えみ', '笑み'),
			_Utils_Tuple2('えめらるど', 'エメラルド'),
			_Utils_Tuple2('えもの', '獲物'),
			_Utils_Tuple2('えら', 'えら'),
			_Utils_Tuple2('えりあし', '襟足'),
			_Utils_Tuple2('えりもと', '襟元'),
			_Utils_Tuple2('えるぴー', 'ＬＰ'),
			_Utils_Tuple2('えれき', 'エレキ'),
			_Utils_Tuple2('えれくとーん', 'エレクトーン'),
			_Utils_Tuple2('えれくとろにくす', 'エレクトロニクス'),
			_Utils_Tuple2('えろ', 'エロ'),
			_Utils_Tuple2('えろちっく', 'エロチック'),
			_Utils_Tuple2('えん', '宴'),
			_Utils_Tuple2('えん', '縁'),
			_Utils_Tuple2('えん', '煙'),
			_Utils_Tuple2('えん', '炎'),
			_Utils_Tuple2('えんえい', '遠泳'),
			_Utils_Tuple2('えんえん', '延々'),
			_Utils_Tuple2('えんかく', '遠隔'),
			_Utils_Tuple2('えんかくそうさ', '遠隔操作'),
			_Utils_Tuple2('えんかつ', '円滑'),
			_Utils_Tuple2('えんがわ', '縁側'),
			_Utils_Tuple2('えんぎ', '縁起'),
			_Utils_Tuple2('えんげい', '演芸'),
			_Utils_Tuple2('えんげーじ', 'エンゲージ'),
			_Utils_Tuple2('えんこ', '縁故'),
			_Utils_Tuple2('えんさん', '塩酸'),
			_Utils_Tuple2('えんざん', '演算'),
			_Utils_Tuple2('えんじ', '園児'),
			_Utils_Tuple2('えんしゅう', '演習'),
			_Utils_Tuple2('えんしょう', '炎症'),
			_Utils_Tuple2('えんすい', '塩水'),
			_Utils_Tuple2('えんすと', 'エンスト'),
			_Utils_Tuple2('えんずる', '演ずる'),
			_Utils_Tuple2('えんせい', '遠征'),
			_Utils_Tuple2('えんそ', '塩素'),
			_Utils_Tuple2('えんたい', '延滞'),
			_Utils_Tuple2('えんてん', '炎天'),
			_Utils_Tuple2('えんとりー', 'エントリー'),
			_Utils_Tuple2('えんにち', '縁日'),
			_Utils_Tuple2('えんばん', '円盤'),
			_Utils_Tuple2('えんめい', '延命'),
			_Utils_Tuple2('おい', '老い'),
			_Utils_Tuple2('おいおい', 'おいおい'),
			_Utils_Tuple2('おいかえす', '追い返す'),
			_Utils_Tuple2('おいこむ', '追い込む'),
			_Utils_Tuple2('おいしげる', '生い茂る'),
			_Utils_Tuple2('おいたち', '生い立ち'),
			_Utils_Tuple2('おいたてる', '追い立てる'),
			_Utils_Tuple2('おいつめる', '追い詰める'),
			_Utils_Tuple2('おいてけぼり', '置いてけぼり'),
			_Utils_Tuple2('おいはらう', '追い払う'),
			_Utils_Tuple2('おいまわす', '追い回す'),
			_Utils_Tuple2('おいら', 'おいら'),
			_Utils_Tuple2('おうきゅう', '応急'),
			_Utils_Tuple2('おうごんじだい', '黄金時代'),
			_Utils_Tuple2('おうざ', '王座'),
			_Utils_Tuple2('おうじゃ', '王者'),
			_Utils_Tuple2('おうしゅう', '押収'),
			_Utils_Tuple2('おうじる', '応じる'),
			_Utils_Tuple2('おうせい', '王政'),
			_Utils_Tuple2('おうせい', 'おうせい'),
			_Utils_Tuple2('おうせつ', '応接'),
			_Utils_Tuple2('おうちゃく', '横着'),
			_Utils_Tuple2('おうちょう', '王朝'),
			_Utils_Tuple2('おうてん', '横転'),
			_Utils_Tuple2('おうどう', '王道'),
			_Utils_Tuple2('おうとつ', '凹凸'),
			_Utils_Tuple2('おうねん', '往年'),
			_Utils_Tuple2('おうへい', '横柄'),
			_Utils_Tuple2('おうりょう', '横領'),
			_Utils_Tuple2('おうろ', '往路'),
			_Utils_Tuple2('おおおく', '大奥'),
			_Utils_Tuple2('おおがかり', '大掛かり'),
			_Utils_Tuple2('おおかた', '大方'),
			_Utils_Tuple2('おおかた', '大方'),
			_Utils_Tuple2('おおかみしょうねん', 'おおかみ少年'),
			_Utils_Tuple2('おおきに', '大きに'),
			_Utils_Tuple2('おおくらしょう', '大蔵省'),
			_Utils_Tuple2('おおざっぱ', '大雑把'),
			_Utils_Tuple2('おーしゃん', 'オーシャン'),
			_Utils_Tuple2('おおぜき', '大関'),
			_Utils_Tuple2('おーそどっくす', 'オーソドックス'),
			_Utils_Tuple2('おおだい', '大台'),
			_Utils_Tuple2('おおつぶ', '大粒'),
			_Utils_Tuple2('おおて', '大手'),
			_Utils_Tuple2('おーばーおーる', 'オーバーオール'),
			_Utils_Tuple2('おーばーこーと', 'オーバーコート'),
			_Utils_Tuple2('おーばーひーと', 'オーバーヒート'),
			_Utils_Tuple2('おーばーらん', 'オーバーラン'),
			_Utils_Tuple2('おーばーわーく', 'オーバーワーク'),
			_Utils_Tuple2('おおばん', '大判'),
			_Utils_Tuple2('おーぷんかー', 'オープンカー'),
			_Utils_Tuple2('おーぷんせん', 'オープン戦'),
			_Utils_Tuple2('おおまか', '大まか'),
			_Utils_Tuple2('おーむ', 'オーム'),
			_Utils_Tuple2('おおむかし', '大昔'),
			_Utils_Tuple2('おおむね', 'おおむね'),
			_Utils_Tuple2('おおめ', '大目'),
			_Utils_Tuple2('おおもうけ', '大もうけ'),
			_Utils_Tuple2('おおやけ', '公'),
			_Utils_Tuple2('おーら', 'オーラ'),
			_Utils_Tuple2('おおらか', '大らか'),
			_Utils_Tuple2('おーるまいてぃー', 'オールマイティー'),
			_Utils_Tuple2('おかか', 'おかか'),
			_Utils_Tuple2('おかす', '冒す'),
			_Utils_Tuple2('おかす', '侵す'),
			_Utils_Tuple2('おかみ', 'おかみ'),
			_Utils_Tuple2('おかみさん', 'おかみさん'),
			_Utils_Tuple2('おがむ', '拝む'),
			_Utils_Tuple2('おから', 'おから'),
			_Utils_Tuple2('おき', '沖'),
			_Utils_Tuple2('おきざり', '置き去り'),
			_Utils_Tuple2('おきて', 'おきて'),
			_Utils_Tuple2('おくそく', '憶測'),
			_Utils_Tuple2('おくたーぶ', 'オクターブ'),
			_Utils_Tuple2('おくち', '奥地'),
			_Utils_Tuple2('おくゆき', '奥行き'),
			_Utils_Tuple2('おくら', 'オクラ'),
			_Utils_Tuple2('おけ', 'おけ'),
			_Utils_Tuple2('おこがましい', 'おこがましいい'),
			_Utils_Tuple2('おこし', '起こし'),
			_Utils_Tuple2('おこたる', '怠る'),
			_Utils_Tuple2('おごる', 'おごる'),
			_Utils_Tuple2('おさけ', 'お酒'),
			_Utils_Tuple2('おさまる', '納まる'),
			_Utils_Tuple2('おさまる', '収まる'),
			_Utils_Tuple2('おさまる', '治まる'),
			_Utils_Tuple2('おさめ', '納め'),
			_Utils_Tuple2('おさめる', '納める'),
			_Utils_Tuple2('おさめる', '治める'),
			_Utils_Tuple2('おしえご', '教え子'),
			_Utils_Tuple2('おしえこむ', '教え込む'),
			_Utils_Tuple2('おしかける', '押し掛ける'),
			_Utils_Tuple2('おしきる', '押し切る'),
			_Utils_Tuple2('おしすすめる', '推し進める'),
			_Utils_Tuple2('おしのける', '押しのける'),
			_Utils_Tuple2('おしはかる', '推し量る'),
			_Utils_Tuple2('おじや', 'おじや'),
			_Utils_Tuple2('おじゃまします', 'お邪魔します'),
			_Utils_Tuple2('おしょく', '汚職'),
			_Utils_Tuple2('おす', '推す'),
			_Utils_Tuple2('おずおず', 'おずおず'),
			_Utils_Tuple2('おずおずと', 'おずおずと'),
			_Utils_Tuple2('おせっかい', 'お節介'),
			_Utils_Tuple2('おそなえもの', 'お供え物'),
			_Utils_Tuple2('おそるおそる', '恐る恐る'),
			_Utils_Tuple2('おそれいる', '恐れ入る'),
			_Utils_Tuple2('おぞん', 'オゾン'),
			_Utils_Tuple2('おだてる', 'おだてる'),
			_Utils_Tuple2('おちいる', '陥る'),
			_Utils_Tuple2('おちど', '落ち度'),
			_Utils_Tuple2('おちめ', '落ち目'),
			_Utils_Tuple2('おっくう', 'おっくう'),
			_Utils_Tuple2('おっとり', 'おっとり'),
			_Utils_Tuple2('おっぱい', 'おっぱい'),
			_Utils_Tuple2('おてあげ', 'お手上げ'),
			_Utils_Tuple2('おてまえ', 'お手前'),
			_Utils_Tuple2('おてん', '汚点'),
			_Utils_Tuple2('おどかす', '脅かす'),
			_Utils_Tuple2('おとこなき', '男泣き'),
			_Utils_Tuple2('おとこやく', '男役'),
			_Utils_Tuple2('おどし', '脅し'),
			_Utils_Tuple2('おとしいれる', '陥れる'),
			_Utils_Tuple2('おどす', '脅す'),
			_Utils_Tuple2('おとめ', '乙女'),
			_Utils_Tuple2('おどらす', '躍らす'),
			_Utils_Tuple2('おとり', 'おとり'),
			_Utils_Tuple2('おどりこ', '踊り子'),
			_Utils_Tuple2('おとろえ', '衰え'),
			_Utils_Tuple2('おなご', 'おなご'),
			_Utils_Tuple2('おなじみ', 'おなじみ'),
			_Utils_Tuple2('おねしょ', 'おねしょ'),
			_Utils_Tuple2('おの', '斧'),
			_Utils_Tuple2('おのおの', '各々'),
			_Utils_Tuple2('おのずと', '自ずと'),
			_Utils_Tuple2('おのれ', '己'),
			_Utils_Tuple2('おびえる', 'おびえる'),
			_Utils_Tuple2('おびやかす', '脅かす'),
			_Utils_Tuple2('おびる', '帯びる'),
			_Utils_Tuple2('おふァー', 'オファー'),
			_Utils_Tuple2('おふぃしゃる', 'オフィシャル'),
			_Utils_Tuple2('おふくろ', 'おふくろ'),
			_Utils_Tuple2('おふさいど', 'オフサイド'),
			_Utils_Tuple2('おぷしょん', 'オプション'),
			_Utils_Tuple2('おぶつ', '汚物'),
			_Utils_Tuple2('おべっか', 'おべっか'),
			_Utils_Tuple2('おぺれーしょん', 'オペレーション'),
			_Utils_Tuple2('おぺれーたー', 'オペレーター'),
			_Utils_Tuple2('おぼえこむ', '覚え込む'),
			_Utils_Tuple2('おまいり', 'お参り'),
			_Utils_Tuple2('おまけ', 'おまけ'),
			_Utils_Tuple2('おまる', 'おまる'),
			_Utils_Tuple2('おみ', '御御'),
			_Utils_Tuple2('おむつ', 'おむつ'),
			_Utils_Tuple2('おむにばす', 'オムニバス'),
			_Utils_Tuple2('おもいあたる', '思い当たる'),
			_Utils_Tuple2('おもいいれ', '思い入れ'),
			_Utils_Tuple2('おもいえがく', '思い描く'),
			_Utils_Tuple2('おもいおこす', '思い起こす'),
			_Utils_Tuple2('おもいおもい', '思い思い'),
			_Utils_Tuple2('おもいきる', '思い切る'),
			_Utils_Tuple2('おもいこみ', '思い込み'),
			_Utils_Tuple2('おもいこむ', '思い込む'),
			_Utils_Tuple2('おもいしる', '思い知る'),
			_Utils_Tuple2('おもいたつ', '思い立つ'),
			_Utils_Tuple2('おもいちがい', '思い違い'),
			_Utils_Tuple2('おもいつめる', '思い詰める'),
			_Utils_Tuple2('おもいなやむ', '思い悩む'),
			_Utils_Tuple2('おもいめぐらす', '思い巡らす'),
			_Utils_Tuple2('おもいやる', '思いやる'),
			_Utils_Tuple2('おもうぞんぶん', '思う存分'),
			_Utils_Tuple2('おもうに', '思うに'),
			_Utils_Tuple2('おもおもしい', '重々しい'),
			_Utils_Tuple2('おもかげ', '面影'),
			_Utils_Tuple2('おもし', '重し'),
			_Utils_Tuple2('おもてむき', '表向き'),
			_Utils_Tuple2('おもむき', '趣'),
			_Utils_Tuple2('おもむく', '赴く'),
			_Utils_Tuple2('おもや', '母屋'),
			_Utils_Tuple2('おもわく', '思惑'),
			_Utils_Tuple2('おもんずる', '重んずる'),
			_Utils_Tuple2('おやかた', '親方'),
			_Utils_Tuple2('おり', '折'),
			_Utils_Tuple2('おり', 'おり'),
			_Utils_Tuple2('おりあい', '折り合い'),
			_Utils_Tuple2('おりえんたる', 'オリエンタル'),
			_Utils_Tuple2('おりおり', '折々'),
			_Utils_Tuple2('おりかえし', '折り返し'),
			_Utils_Tuple2('おりかえす', '折り返す'),
			_Utils_Tuple2('おりから', '折から'),
			_Utils_Tuple2('おりこみ', '折り込み'),
			_Utils_Tuple2('おりじなりてぃー', 'オリジナリティー'),
			_Utils_Tuple2('おりしも', '折しも'),
			_Utils_Tuple2('おりたつ', '降り立つ'),
			_Utils_Tuple2('おりめ', '折り目'),
			_Utils_Tuple2('おりもの', '織物'),
			_Utils_Tuple2('おりんぴっくきょうぎ', 'オリンピック競技'),
			_Utils_Tuple2('おる', '織る'),
			_Utils_Tuple2('おれい', 'お礼'),
			_Utils_Tuple2('おろか', '愚か'),
			_Utils_Tuple2('おろかもの', '愚か者'),
			_Utils_Tuple2('おろし', '卸'),
			_Utils_Tuple2('おろしうり', '卸売り'),
			_Utils_Tuple2('おろす', '卸す'),
			_Utils_Tuple2('おろそか', 'おろそか'),
			_Utils_Tuple2('おんいき', '音域'),
			_Utils_Tuple2('おんかい', '音階'),
			_Utils_Tuple2('おんがえし', '恩返し'),
			_Utils_Tuple2('おんかん', '音感'),
			_Utils_Tuple2('おんきょう', '音響'),
			_Utils_Tuple2('おんけい', '恩恵'),
			_Utils_Tuple2('おんこう', '温厚'),
			_Utils_Tuple2('おんし', '恩師'),
			_Utils_Tuple2('おんしつ', '音質'),
			_Utils_Tuple2('おんしん', '音信'),
			_Utils_Tuple2('おんせい', '音声'),
			_Utils_Tuple2('おんせつ', '音節'),
			_Utils_Tuple2('おんぞん', '温存'),
			_Utils_Tuple2('おんたいていきあつ', '温帯低気圧'),
			_Utils_Tuple2('おんてい', '音程'),
			_Utils_Tuple2('おんど', '音頭'),
			_Utils_Tuple2('おんなあそび', '女遊び'),
			_Utils_Tuple2('おんぱ', '音波'),
			_Utils_Tuple2('おんぱれーど', 'オンパレード'),
			_Utils_Tuple2('おんぶ', '負んぶ'),
			_Utils_Tuple2('おんぷ', '音符'),
			_Utils_Tuple2('か', '価'),
			_Utils_Tuple2('か', '架'),
			_Utils_Tuple2('が', 'が'),
			_Utils_Tuple2('が', '我'),
			_Utils_Tuple2('かーとりっじ', 'カートリッジ'),
			_Utils_Tuple2('がーどる', 'ガードル'),
			_Utils_Tuple2('かーりんぐ', 'カーリング'),
			_Utils_Tuple2('かい', 'かい'),
			_Utils_Tuple2('かい', '怪'),
			_Utils_Tuple2('かい', '下位'),
			_Utils_Tuple2('かい', '怪'),
			_Utils_Tuple2('がい', 'がい'),
			_Utils_Tuple2('かいあく', '改悪'),
			_Utils_Tuple2('がいあつ', '外圧'),
			_Utils_Tuple2('かいいき', '海域'),
			_Utils_Tuple2('かいか', '開化'),
			_Utils_Tuple2('がいかい', '外界'),
			_Utils_Tuple2('かいかぶる', '買いかぶる'),
			_Utils_Tuple2('かいがら', '貝殻'),
			_Utils_Tuple2('かいき', '怪奇'),
			_Utils_Tuple2('かいきゅう', '階級'),
			_Utils_Tuple2('かいきょう', '海峡'),
			_Utils_Tuple2('かいぎょう', '改行'),
			_Utils_Tuple2('かいきん', '解禁'),
			_Utils_Tuple2('かいけいし', '会計士'),
			_Utils_Tuple2('かいこう', '開口'),
			_Utils_Tuple2('かいこう', '開港'),
			_Utils_Tuple2('かいごう', '会合'),
			_Utils_Tuple2('かいこむ', '買い込む'),
			_Utils_Tuple2('かいさい', '開催'),
			_Utils_Tuple2('がいさん', '概算'),
			_Utils_Tuple2('がいし', '外資'),
			_Utils_Tuple2('かいしめる', '買い占める'),
			_Utils_Tuple2('かいしゃく', '解釈'),
			_Utils_Tuple2('かいしゅう', '改修'),
			_Utils_Tuple2('かいじゅう', '怪獣'),
			_Utils_Tuple2('がいしゅう', '外周'),
			_Utils_Tuple2('かいじょ', '介助'),
			_Utils_Tuple2('かいじん', '怪人'),
			_Utils_Tuple2('かいする', '介する'),
			_Utils_Tuple2('かいする', '会する'),
			_Utils_Tuple2('かいせき', '解析'),
			_Utils_Tuple2('かいせき', '懐石'),
			_Utils_Tuple2('がいせん', 'がいせん'),
			_Utils_Tuple2('がいせんもん', 'がいせんもん'),
			_Utils_Tuple2('かいそう', '階層'),
			_Utils_Tuple2('かいぞく', '海賊'),
			_Utils_Tuple2('かいたく', '開拓'),
			_Utils_Tuple2('かいつけ', '買い付け'),
			_Utils_Tuple2('がいてき', '外敵'),
			_Utils_Tuple2('かいてしじょう', '買い手市場'),
			_Utils_Tuple2('がいとう', '該当'),
			_Utils_Tuple2('がいとう', '街頭'),
			_Utils_Tuple2('かいどく', '解読'),
			_Utils_Tuple2('かいどく', '買い得'),
			_Utils_Tuple2('かいにゅう', '介入'),
			_Utils_Tuple2('かいにん', '解任'),
			_Utils_Tuple2('かいぬし', '買い主'),
			_Utils_Tuple2('がいねん', '概念'),
			_Utils_Tuple2('かいばしら', '貝柱'),
			_Utils_Tuple2('かいひ', '回避'),
			_Utils_Tuple2('かいひょう', '開票'),
			_Utils_Tuple2('かいほう', '会報'),
			_Utils_Tuple2('かいぼう', '解剖'),
			_Utils_Tuple2('かいむ', '皆無'),
			_Utils_Tuple2('がいむ', '外務'),
			_Utils_Tuple2('かいめい', '改名'),
			_Utils_Tuple2('かいめつ', '壊滅'),
			_Utils_Tuple2('かいもとめる', '買い求める'),
			_Utils_Tuple2('がいや', '外野'),
			_Utils_Tuple2('がいやしゅ', '外野手'),
			_Utils_Tuple2('がいやせき', '外野席'),
			_Utils_Tuple2('かいよう', '海洋'),
			_Utils_Tuple2('がいよう', '概要'),
			_Utils_Tuple2('かいりき', '怪力'),
			_Utils_Tuple2('かいりつ', '戒律'),
			_Utils_Tuple2('がいりゃく', '概略'),
			_Utils_Tuple2('かいろ', '海路'),
			_Utils_Tuple2('かいろ', '回路'),
			_Utils_Tuple2('がいろ', '街路'),
			_Utils_Tuple2('がいろじゅ', '街路樹'),
			_Utils_Tuple2('がいろん', '概論'),
			_Utils_Tuple2('かいわい', 'かいわい'),
			_Utils_Tuple2('かえりみる', '顧みる'),
			_Utils_Tuple2('かえる', 'かえる'),
			_Utils_Tuple2('かおあわせ', '顔合わせ'),
			_Utils_Tuple2('かおく', '家屋'),
			_Utils_Tuple2('かおだし', '顔出し'),
			_Utils_Tuple2('かおつき', '顔付き'),
			_Utils_Tuple2('かおぶれ', '顔触れ'),
			_Utils_Tuple2('かおまけ', '顔負け'),
			_Utils_Tuple2('かおみしり', '顔見知り'),
			_Utils_Tuple2('かかえこむ', '抱え込む'),
			_Utils_Tuple2('ががく', '雅楽'),
			_Utils_Tuple2('かがくそうさ', '科学捜査'),
			_Utils_Tuple2('かがくはんのう', '化学反応'),
			_Utils_Tuple2('かがくひりょう', '化学肥料'),
			_Utils_Tuple2('かがくぶっしつ', '化学物質'),
			_Utils_Tuple2('かがくへいき', '化学兵器'),
			_Utils_Tuple2('かがくへんか', '化学変化'),
			_Utils_Tuple2('かかし', 'かかし'),
			_Utils_Tuple2('かかす', '欠かす'),
			_Utils_Tuple2('かがむ', 'かがむ'),
			_Utils_Tuple2('かがやかしい', '輝かしい'),
			_Utils_Tuple2('かかる', '架かる'),
			_Utils_Tuple2('かかる', '斯かる'),
			_Utils_Tuple2('かき', '花器'),
			_Utils_Tuple2('がき', '餓鬼'),
			_Utils_Tuple2('かぎあな', '鍵穴'),
			_Utils_Tuple2('かきあらためる', '書き改める'),
			_Utils_Tuple2('かきいれる', '書き入れる'),
			_Utils_Tuple2('かきこみ', '書き込み'),
			_Utils_Tuple2('かきだす', '書き出す'),
			_Utils_Tuple2('かぎっこ', 'かぎっ子'),
			_Utils_Tuple2('かきとめる', '書き留める'),
			_Utils_Tuple2('かきね', '垣根'),
			_Utils_Tuple2('かきもの', '書き物'),
			_Utils_Tuple2('かきゅう', '下級'),
			_Utils_Tuple2('かぎょう', '家業'),
			_Utils_Tuple2('かきわける', '書き分ける'),
			_Utils_Tuple2('かく', '閣'),
			_Utils_Tuple2('かく', '格'),
			_Utils_Tuple2('かく', '画'),
			_Utils_Tuple2('かく', 'かく'),
			_Utils_Tuple2('かくあげ', '格上げ'),
			_Utils_Tuple2('がくい', '学位'),
			_Utils_Tuple2('かくいつか', '画一化'),
			_Utils_Tuple2('かくいつてき', '画一的'),
			_Utils_Tuple2('がくげい', '学芸'),
			_Utils_Tuple2('かくげつ', '隔月'),
			_Utils_Tuple2('かくげん', '格言'),
			_Utils_Tuple2('かくさげ', '格下げ'),
			_Utils_Tuple2('かくさん', '拡散'),
			_Utils_Tuple2('がくし', '学資'),
			_Utils_Tuple2('がくしき', '学識'),
			_Utils_Tuple2('かくしつ', '確執'),
			_Utils_Tuple2('かくしつ', '角質'),
			_Utils_Tuple2('かくじゅう', '拡充'),
			_Utils_Tuple2('がくじゅつ', '学術'),
			_Utils_Tuple2('かくしん', '革新'),
			_Utils_Tuple2('かくすう', '画数'),
			_Utils_Tuple2('かくする', '画する'),
			_Utils_Tuple2('かくせい', '覚せい'),
			_Utils_Tuple2('かくせいざい', '覚せい剤'),
			_Utils_Tuple2('がくせつ', '学説'),
			_Utils_Tuple2('かくだん', '格段'),
			_Utils_Tuple2('がくだん', '楽団'),
			_Utils_Tuple2('かくちょう', '拡張'),
			_Utils_Tuple2('かくてい', '確定'),
			_Utils_Tuple2('かくていしんこく', '確定申告'),
			_Utils_Tuple2('かくとう', '格闘'),
			_Utils_Tuple2('かくに', '角煮'),
			_Utils_Tuple2('がくふ', '楽譜'),
			_Utils_Tuple2('かくぶんれつ', '核分裂'),
			_Utils_Tuple2('かくへいき', '核兵器'),
			_Utils_Tuple2('かくほ', '確保'),
			_Utils_Tuple2('がくぼう', '学帽'),
			_Utils_Tuple2('がくめい', '学名'),
			_Utils_Tuple2('がくめん', '額面'),
			_Utils_Tuple2('がくや', '楽屋'),
			_Utils_Tuple2('かくゆうごう', '核融合'),
			_Utils_Tuple2('かくり', '隔離'),
			_Utils_Tuple2('かくりつ', '確立'),
			_Utils_Tuple2('かくりょう', '閣僚'),
			_Utils_Tuple2('かけ', '掛け'),
			_Utils_Tuple2('かけ', 'かけ'),
			_Utils_Tuple2('かけ', '欠け'),
			_Utils_Tuple2('がけ', 'がけ'),
			_Utils_Tuple2('かけあう', '掛け合う'),
			_Utils_Tuple2('かけあがる', '駆け上がる'),
			_Utils_Tuple2('かけい', '家系'),
			_Utils_Tuple2('かけいぼ', '家計簿'),
			_Utils_Tuple2('かげえ', '影絵'),
			_Utils_Tuple2('かけがえ', '掛け替え'),
			_Utils_Tuple2('かげき', '歌劇'),
			_Utils_Tuple2('かげき', '過激'),
			_Utils_Tuple2('かけきん', '掛け金'),
			_Utils_Tuple2('かけごえ', '掛け声'),
			_Utils_Tuple2('かけじく', '掛け軸'),
			_Utils_Tuple2('かけつける', '駆け付ける'),
			_Utils_Tuple2('かけぬける', '駆け抜ける'),
			_Utils_Tuple2('かけひき', '駆け引き'),
			_Utils_Tuple2('かげぼし', '陰干し'),
			_Utils_Tuple2('かけまわる', '駆け回る'),
			_Utils_Tuple2('かけめぐる', '駆け巡る'),
			_Utils_Tuple2('かけもち', '掛け持ち'),
			_Utils_Tuple2('かけら', 'かけら'),
			_Utils_Tuple2('かげり', '陰り'),
			_Utils_Tuple2('かご', '加護'),
			_Utils_Tuple2('かごうぶつ', '化合物'),
			_Utils_Tuple2('かこく', '過酷'),
			_Utils_Tuple2('かごん', '過言'),
			_Utils_Tuple2('かさい', '家裁'),
			_Utils_Tuple2('かざい', '家財'),
			_Utils_Tuple2('がさがさ', 'がさがさ'),
			_Utils_Tuple2('かざす', 'かざす'),
			_Utils_Tuple2('がさつ', 'がさつ'),
			_Utils_Tuple2('かさなりあう', '重なり合う'),
			_Utils_Tuple2('かさばる', 'かさばる'),
			_Utils_Tuple2('かさむ', 'かさむ'),
			_Utils_Tuple2('かじ', 'かじ'),
			_Utils_Tuple2('がし', '餓死'),
			_Utils_Tuple2('かじかむ', 'かじかむ'),
			_Utils_Tuple2('かしこまる', 'かしこまる'),
			_Utils_Tuple2('かじの', 'カジノ'),
			_Utils_Tuple2('かしみあ', 'カシミア'),
			_Utils_Tuple2('かしや', '貸家'),
			_Utils_Tuple2('かじゅ', '果樹'),
			_Utils_Tuple2('かしゅう', '歌集'),
			_Utils_Tuple2('がしゅう', '画集'),
			_Utils_Tuple2('かじゅえん', '果樹園'),
			_Utils_Tuple2('かしょう', '歌唱'),
			_Utils_Tuple2('かしょう', '過小'),
			_Utils_Tuple2('かじょう', '過剰'),
			_Utils_Tuple2('かす', 'かす'),
			_Utils_Tuple2('かすか', 'かすか'),
			_Utils_Tuple2('かすたーど', 'カスタード'),
			_Utils_Tuple2('がすとう', 'ガス灯'),
			_Utils_Tuple2('かずのこ', '数の子'),
			_Utils_Tuple2('がすぼんべ', 'ガスボンベ'),
			_Utils_Tuple2('かすみ', 'かすみ'),
			_Utils_Tuple2('かする', '課する'),
			_Utils_Tuple2('かする', '化する'),
			_Utils_Tuple2('かする', '科する'),
			_Utils_Tuple2('かすれる', '掠れる'),
			_Utils_Tuple2('かせぎ', '稼ぎ'),
			_Utils_Tuple2('かせん', '化繊'),
			_Utils_Tuple2('かせんしき', '河川敷'),
			_Utils_Tuple2('かそ', '過疎'),
			_Utils_Tuple2('かぞえ', '数え'),
			_Utils_Tuple2('かぞえどし', '数え年'),
			_Utils_Tuple2('かそくど', '加速度'),
			_Utils_Tuple2('かた', '過多'),
			_Utils_Tuple2('かたかた', 'かたかた'),
			_Utils_Tuple2('かたがみ', '型紙'),
			_Utils_Tuple2('かたくりこ', 'かたくり粉'),
			_Utils_Tuple2('かたくるしい', '堅苦しい'),
			_Utils_Tuple2('かたこと', '片言'),
			_Utils_Tuple2('かたしき', '型式'),
			_Utils_Tuple2('かたすみ', '片隅'),
			_Utils_Tuple2('かたちづくる', '形作る'),
			_Utils_Tuple2('かたてま', '片手間'),
			_Utils_Tuple2('かたはし', '片端'),
			_Utils_Tuple2('かたはば', '肩幅'),
			_Utils_Tuple2('かたまり', '固まり'),
			_Utils_Tuple2('かたみ', '肩身'),
			_Utils_Tuple2('かたみ', '形見'),
			_Utils_Tuple2('かため', '固め'),
			_Utils_Tuple2('かたりあう', '語り合う'),
			_Utils_Tuple2('かたりつぐ', '語り継ぐ'),
			_Utils_Tuple2('かたりつたえる', '語り伝える'),
			_Utils_Tuple2('かたわら', '傍ら'),
			_Utils_Tuple2('かたん', '加担'),
			_Utils_Tuple2('かちく', '家畜'),
			_Utils_Tuple2('かちこす', '勝ち越す'),
			_Utils_Tuple2('かちゅう', '渦中'),
			_Utils_Tuple2('かつ', '喝'),
			_Utils_Tuple2('かつお', 'かつお'),
			_Utils_Tuple2('かっか', '閣下'),
			_Utils_Tuple2('がっかい', '学界'),
			_Utils_Tuple2('がつがつ', 'がつがつ'),
			_Utils_Tuple2('がっきょく', '楽曲'),
			_Utils_Tuple2('かつぐ', '担ぐ'),
			_Utils_Tuple2('がっく', '学区'),
			_Utils_Tuple2('がっくり', 'がっくり'),
			_Utils_Tuple2('かつじ', '活字'),
			_Utils_Tuple2('がっしょう', '合掌'),
			_Utils_Tuple2('かっせん', '合戦'),
			_Utils_Tuple2('かっそう', '滑走'),
			_Utils_Tuple2('かっそうろ', '滑走路'),
			_Utils_Tuple2('がったい', '合体'),
			_Utils_Tuple2('がっち', '合致'),
			_Utils_Tuple2('がっちり', 'がっちり'),
			_Utils_Tuple2('がっつ', 'ガッツ'),
			_Utils_Tuple2('かってぃんぐ', 'カッティング'),
			_Utils_Tuple2('がってん', '合点'),
			_Utils_Tuple2('かっと', 'かっと'),
			_Utils_Tuple2('かっとう', '葛藤'),
			_Utils_Tuple2('かっぱ', 'かっぱ'),
			_Utils_Tuple2('がっぺい', '合併'),
			_Utils_Tuple2('かつれつ', 'カツレツ'),
			_Utils_Tuple2('かつろ', '活路'),
			_Utils_Tuple2('かていきょういく', '家庭教育'),
			_Utils_Tuple2('かていさいばんしょ', '家庭裁判所'),
			_Utils_Tuple2('かてごりー', 'カテゴリー'),
			_Utils_Tuple2('かどう', '稼働'),
			_Utils_Tuple2('かとき', '過渡期'),
			_Utils_Tuple2('かどまつ', '門松'),
			_Utils_Tuple2('かとりせんこう', '蚊取り線香'),
			_Utils_Tuple2('かなあみ', '金網'),
			_Utils_Tuple2('かなう', 'かなう'),
			_Utils_Tuple2('かなぐ', '金具'),
			_Utils_Tuple2('かなもの', '金物'),
			_Utils_Tuple2('かならずや', '必ずや'),
			_Utils_Tuple2('かなりあ', 'カナリア'),
			_Utils_Tuple2('かぬー', 'カヌー'),
			_Utils_Tuple2('かねあい', '兼ね合い'),
			_Utils_Tuple2('かねて', '予て'),
			_Utils_Tuple2('かの', 'かの'),
			_Utils_Tuple2('かのう', '化のう'),
			_Utils_Tuple2('かばう', 'かばう'),
			_Utils_Tuple2('かび', 'かび'),
			_Utils_Tuple2('かびん', '過敏'),
			_Utils_Tuple2('かぶ', '株'),
			_Utils_Tuple2('かぶ', '株'),
			_Utils_Tuple2('かぶ', 'かぶ'),
			_Utils_Tuple2('かふう', '家風'),
			_Utils_Tuple2('かぶか', '株価'),
			_Utils_Tuple2('かふくぶ', '下腹部'),
			_Utils_Tuple2('かぶせる', 'かぶせる'),
			_Utils_Tuple2('かふそく', '過不足'),
			_Utils_Tuple2('かぶと', 'かぶと'),
			_Utils_Tuple2('かぶぬし', '株主'),
			_Utils_Tuple2('かぶぬしそうかい', '株主総会'),
			_Utils_Tuple2('かぶれる', '気触れる'),
			_Utils_Tuple2('かへい', '貨幣'),
			_Utils_Tuple2('かべがみ', '壁紙'),
			_Utils_Tuple2('かほご', '過保護'),
			_Utils_Tuple2('かぼそい', 'か細い'),
			_Utils_Tuple2('かまいたち', 'かまいたち'),
			_Utils_Tuple2('かまえ', '構え'),
			_Utils_Tuple2('かまえる', '構える'),
			_Utils_Tuple2('かまくら', 'かまくら'),
			_Utils_Tuple2('かます', 'かます'),
			_Utils_Tuple2('かみ', '加味'),
			_Utils_Tuple2('かみあう', 'かみ合う'),
			_Utils_Tuple2('かみかぜ', '神風'),
			_Utils_Tuple2('かみきれ', '紙切れ'),
			_Utils_Tuple2('かみころす', 'かみ殺す'),
			_Utils_Tuple2('かみざ', '上座'),
			_Utils_Tuple2('かみだな', '神棚'),
			_Utils_Tuple2('かみだのみ', '神頼み'),
			_Utils_Tuple2('かみつ', '過密'),
			_Utils_Tuple2('かみて', '上手'),
			_Utils_Tuple2('かむばっく', 'カムバック'),
			_Utils_Tuple2('かむふらーじゅ', 'カムフラージュ'),
			_Utils_Tuple2('かも', 'かも'),
			_Utils_Tuple2('かもしだす', '醸し出す'),
			_Utils_Tuple2('かもつ', '貨物'),
			_Utils_Tuple2('かもつれっしゃ', '貨物列車'),
			_Utils_Tuple2('かもん', '家紋'),
			_Utils_Tuple2('かや', '蚊帳'),
			_Utils_Tuple2('かやく', '火薬'),
			_Utils_Tuple2('かよう', '歌謡'),
			_Utils_Tuple2('かようきょく', '歌謡曲'),
			_Utils_Tuple2('がようし', '画用紙'),
			_Utils_Tuple2('かよわい', 'か弱い'),
			_Utils_Tuple2('がら', 'がら'),
			_Utils_Tuple2('からー', 'カラー'),
			_Utils_Tuple2('からから', 'からから'),
			_Utils_Tuple2('からくり', 'からくり'),
			_Utils_Tuple2('からだつき', '体付き'),
			_Utils_Tuple2('からっと', 'カラット'),
			_Utils_Tuple2('からぶり', '空振り'),
			_Utils_Tuple2('からまる', '絡まる'),
			_Utils_Tuple2('からまわり', '空回り'),
			_Utils_Tuple2('からみ', '絡み'),
			_Utils_Tuple2('がらみ', '絡み'),
			_Utils_Tuple2('からみあう', '絡み合う'),
			_Utils_Tuple2('からみつく', '絡み付く'),
			_Utils_Tuple2('からむ', '絡む'),
			_Utils_Tuple2('からめる', '絡める'),
			_Utils_Tuple2('がらりと', 'がらりと'),
			_Utils_Tuple2('かり', '狩り'),
			_Utils_Tuple2('がり', '刈り'),
			_Utils_Tuple2('かりうむ', 'カリウム'),
			_Utils_Tuple2('かりかり', 'かりかり'),
			_Utils_Tuple2('がりがり', 'がりがり'),
			_Utils_Tuple2('かりしょぶん', '仮処分'),
			_Utils_Tuple2('かりたてる', '駆り立てる'),
			_Utils_Tuple2('かりめん', '仮免'),
			_Utils_Tuple2('かりんとう', 'かりんとう'),
			_Utils_Tuple2('かる', '刈る'),
			_Utils_Tuple2('かる', '狩る'),
			_Utils_Tuple2('かるがるしい', '軽々しい'),
			_Utils_Tuple2('かるき', 'カルキ'),
			_Utils_Tuple2('かるた', 'カルタ'),
			_Utils_Tuple2('かるて', 'カルテ'),
			_Utils_Tuple2('かれい', '加齢'),
			_Utils_Tuple2('かれい', '華麗'),
			_Utils_Tuple2('かれこれ', 'かれこれ'),
			_Utils_Tuple2('かろうじて', '辛うじて'),
			_Utils_Tuple2('かろてん', 'カロテン'),
			_Utils_Tuple2('かわきり', '皮切り'),
			_Utils_Tuple2('かわす', '交わす'),
			_Utils_Tuple2('かわす', '躱す'),
			_Utils_Tuple2('かわず', 'かわず'),
			_Utils_Tuple2('かわせ', '為替'),
			_Utils_Tuple2('かわら', '瓦'),
			_Utils_Tuple2('かわりだね', '変わり種'),
			_Utils_Tuple2('かわりばえ', '代わり映え'),
			_Utils_Tuple2('かわりもの', '変わり者'),
			_Utils_Tuple2('かん', '管'),
			_Utils_Tuple2('かん', '肝'),
			_Utils_Tuple2('かん', '冠'),
			_Utils_Tuple2('がん', '眼'),
			_Utils_Tuple2('がん', '願'),
			_Utils_Tuple2('かんい', '簡易'),
			_Utils_Tuple2('かんいさいばんしょ', '簡易裁判所'),
			_Utils_Tuple2('がんか', '眼下'),
			_Utils_Tuple2('かんがい', '感慨'),
			_Utils_Tuple2('かんがえだす', '考え出す'),
			_Utils_Tuple2('かんがえつく', '考え付く'),
			_Utils_Tuple2('かんがえぶかい', '考え深い'),
			_Utils_Tuple2('かんかくき', '感覚器'),
			_Utils_Tuple2('かんかくてき', '感覚的'),
			_Utils_Tuple2('かんかつ', '管轄'),
			_Utils_Tuple2('かんがみる', 'かんがみる'),
			_Utils_Tuple2('かんかん', 'かんかん'),
			_Utils_Tuple2('がんがん', 'がんがん'),
			_Utils_Tuple2('かんき', '乾期'),
			_Utils_Tuple2('がんきゅう', '眼球'),
			_Utils_Tuple2('かんきょうちょう', '環境庁'),
			_Utils_Tuple2('かんきん', '換金'),
			_Utils_Tuple2('かんきん', '監禁'),
			_Utils_Tuple2('がんぐ', '玩具'),
			_Utils_Tuple2('かんげき', '観劇'),
			_Utils_Tuple2('かんけつ', '完結'),
			_Utils_Tuple2('かんけつ', '簡潔'),
			_Utils_Tuple2('かんげん', '還元'),
			_Utils_Tuple2('かんこう', '刊行'),
			_Utils_Tuple2('かんこう', '慣行'),
			_Utils_Tuple2('かんこく', '勧告'),
			_Utils_Tuple2('かんごく', '監獄'),
			_Utils_Tuple2('かんこんそうさい', '冠婚葬祭'),
			_Utils_Tuple2('かんさ', '監査'),
			_Utils_Tuple2('かんさん', '換算'),
			_Utils_Tuple2('かんし', '監視'),
			_Utils_Tuple2('かんじとる', '感じ取る'),
			_Utils_Tuple2('かんじゅせい', '感受性'),
			_Utils_Tuple2('かんしょう', '干渉'),
			_Utils_Tuple2('かんじょう', '環状'),
			_Utils_Tuple2('かんじょうせん', '環状線'),
			_Utils_Tuple2('かんじる', '感じる'),
			_Utils_Tuple2('かんじん', '肝心'),
			_Utils_Tuple2('かんすう', '関数'),
			_Utils_Tuple2('かんずる', '感ずる'),
			_Utils_Tuple2('かんせい', '閑静'),
			_Utils_Tuple2('かんせい', '感性'),
			_Utils_Tuple2('がんせき', '岩石'),
			_Utils_Tuple2('かんせつ', '関節'),
			_Utils_Tuple2('かんせつぜい', '間接税'),
			_Utils_Tuple2('かんせつてき', '間接的'),
			_Utils_Tuple2('かんせん', '感染'),
			_Utils_Tuple2('かんせん', '観戦'),
			_Utils_Tuple2('かんせん', '幹線'),
			_Utils_Tuple2('かんぜんしつぎょうしゃ', '完全失業者'),
			_Utils_Tuple2('かんぜんねんしょう', '完全燃焼'),
			_Utils_Tuple2('かんそ', '簡素'),
			_Utils_Tuple2('かんそ', '簡素'),
			_Utils_Tuple2('がんそ', '元祖'),
			_Utils_Tuple2('かんたい', '艦隊'),
			_Utils_Tuple2('かんだい', '寛大'),
			_Utils_Tuple2('かんち', '感知'),
			_Utils_Tuple2('かんち', '完治'),
			_Utils_Tuple2('かんちゅう', '寒中'),
			_Utils_Tuple2('かんちょう', '官庁'),
			_Utils_Tuple2('かんちょう', '干潮'),
			_Utils_Tuple2('かんつう', '貫通'),
			_Utils_Tuple2('かんてい', '鑑定'),
			_Utils_Tuple2('かんてつ', '貫徹'),
			_Utils_Tuple2('かんてん', '観点'),
			_Utils_Tuple2('かんてん', '寒天'),
			_Utils_Tuple2('かんでん', '感電'),
			_Utils_Tuple2('かんとう', '完投'),
			_Utils_Tuple2('かんぬし', '神主'),
			_Utils_Tuple2('かんねん', '観念'),
			_Utils_Tuple2('がんねん', '元年'),
			_Utils_Tuple2('かんのう', '官能'),
			_Utils_Tuple2('かんぱ', 'カンパ'),
			_Utils_Tuple2('かんぱ', '寒波'),
			_Utils_Tuple2('かんぱん', '甲板'),
			_Utils_Tuple2('かんぶ', '幹部'),
			_Utils_Tuple2('かんふー', 'カンフー'),
			_Utils_Tuple2('かんぷう', '完封'),
			_Utils_Tuple2('かんぶん', '漢文'),
			_Utils_Tuple2('かんべつ', '鑑別'),
			_Utils_Tuple2('かんぼうちょうかん', '官房長官'),
			_Utils_Tuple2('かんぼつ', '陥没'),
			_Utils_Tuple2('かんまつ', '巻末'),
			_Utils_Tuple2('かんみ', '甘味'),
			_Utils_Tuple2('かんみん', '官民'),
			_Utils_Tuple2('かんむり', '冠'),
			_Utils_Tuple2('かんめい', '感銘'),
			_Utils_Tuple2('かんよう', '慣用'),
			_Utils_Tuple2('かんよう', '寛容'),
			_Utils_Tuple2('かんようしょくぶつ', '観葉植物'),
			_Utils_Tuple2('がんらい', '元来'),
			_Utils_Tuple2('かんらん', '観覧'),
			_Utils_Tuple2('かんりしょく', '管理職'),
			_Utils_Tuple2('かんりゃく', '簡略'),
			_Utils_Tuple2('かんれい', '寒冷'),
			_Utils_Tuple2('かんれい', '慣例'),
			_Utils_Tuple2('かんれき', '還暦'),
			_Utils_Tuple2('かんわ', '緩和'),
			_Utils_Tuple2('かんわ', '漢和'),
			_Utils_Tuple2('き', '旗'),
			_Utils_Tuple2('き', '鬼'),
			_Utils_Tuple2('ぎ', '義'),
			_Utils_Tuple2('きあい', '気合い'),
			_Utils_Tuple2('きい', '奇異'),
			_Utils_Tuple2('きいん', '起因'),
			_Utils_Tuple2('きえさる', '消え去る'),
			_Utils_Tuple2('きが', '飢餓'),
			_Utils_Tuple2('きかい', '奇怪'),
			_Utils_Tuple2('きかがく', '幾何学'),
			_Utils_Tuple2('きがかり', '気掛かり'),
			_Utils_Tuple2('きかく', '規格'),
			_Utils_Tuple2('きかざる', '着飾る'),
			_Utils_Tuple2('きがね', '気兼ね'),
			_Utils_Tuple2('きかん', '気管'),
			_Utils_Tuple2('きかん', '器官'),
			_Utils_Tuple2('きかん', '帰還'),
			_Utils_Tuple2('きがん', '祈願'),
			_Utils_Tuple2('きかんし', '気管支'),
			_Utils_Tuple2('きかんしゃ', '機関車'),
			_Utils_Tuple2('きき', '効き'),
			_Utils_Tuple2('ききいる', '聞き入る'),
			_Utils_Tuple2('ききいれる', '聞き入れる'),
			_Utils_Tuple2('ききおぼえ', '聞き覚え'),
			_Utils_Tuple2('ききぐるしい', '聞き苦しい'),
			_Utils_Tuple2('ききこむ', '聞き込む'),
			_Utils_Tuple2('ききだす', '聞き出す'),
			_Utils_Tuple2('ききつける', '聞き付ける'),
			_Utils_Tuple2('ききながす', '聞き流す'),
			_Utils_Tuple2('ききのがす', '聞き逃す'),
			_Utils_Tuple2('ききょう', '帰郷'),
			_Utils_Tuple2('ききょう', '帰京'),
			_Utils_Tuple2('ききん', 'ききん'),
			_Utils_Tuple2('ききん', '基金'),
			_Utils_Tuple2('ききんぞく', '貴金属'),
			_Utils_Tuple2('きぐ', '危惧'),
			_Utils_Tuple2('ぎくしゃく', 'ぎくしゃく'),
			_Utils_Tuple2('きくばり', '気配り'),
			_Utils_Tuple2('きけい', '奇形'),
			_Utils_Tuple2('ぎけい', '義兄'),
			_Utils_Tuple2('きけつ', '帰結'),
			_Utils_Tuple2('ぎけつ', '議決'),
			_Utils_Tuple2('きけん', '棄権'),
			_Utils_Tuple2('きげん', '紀元'),
			_Utils_Tuple2('きげん', '起源'),
			_Utils_Tuple2('きご', '季語'),
			_Utils_Tuple2('きこう', '機構'),
			_Utils_Tuple2('きこう', '紀行'),
			_Utils_Tuple2('ぎこう', '技巧'),
			_Utils_Tuple2('きこうし', '貴公子'),
			_Utils_Tuple2('ぎこうてき', '技巧的'),
			_Utils_Tuple2('きさい', '記載'),
			_Utils_Tuple2('きざい', '機材'),
			_Utils_Tuple2('きざい', '器材'),
			_Utils_Tuple2('ぎざぎざ', 'ぎざぎざ'),
			_Utils_Tuple2('きさく', '気さく'),
			_Utils_Tuple2('きざし', '兆し'),
			_Utils_Tuple2('きざみ', '刻み'),
			_Utils_Tuple2('きざみこむ', '刻み込む'),
			_Utils_Tuple2('きし', '騎士'),
			_Utils_Tuple2('きじ', '生地'),
			_Utils_Tuple2('ぎじ', '疑似'),
			_Utils_Tuple2('ぎじ', '議事'),
			_Utils_Tuple2('ぎしき', '儀式'),
			_Utils_Tuple2('きしつ', '気質'),
			_Utils_Tuple2('ぎじどう', '議事堂'),
			_Utils_Tuple2('きしべ', '岸辺'),
			_Utils_Tuple2('きしゅ', '騎手'),
			_Utils_Tuple2('ぎじゅつかくしん', '技術革新'),
			_Utils_Tuple2('ぎじゅつや', '技術屋'),
			_Utils_Tuple2('きじゅん', '規準'),
			_Utils_Tuple2('きしょう', '気象'),
			_Utils_Tuple2('きしょう', '希少'),
			_Utils_Tuple2('きじょう', '机上'),
			_Utils_Tuple2('きじょう', '気丈'),
			_Utils_Tuple2('きしょうえいせい', '気象衛星'),
			_Utils_Tuple2('きしょうかち', '希少価値'),
			_Utils_Tuple2('きしょうだい', '気象台'),
			_Utils_Tuple2('きしょうちょう', '気象庁'),
			_Utils_Tuple2('ぎすぎす', 'ぎすぎす'),
			_Utils_Tuple2('きずな', 'きずな'),
			_Utils_Tuple2('きする', '期する'),
			_Utils_Tuple2('きせい', '既製'),
			_Utils_Tuple2('きせい', '既成'),
			_Utils_Tuple2('きせい', '奇声'),
			_Utils_Tuple2('きせい', '寄生'),
			_Utils_Tuple2('きせいちゅう', '寄生虫'),
			_Utils_Tuple2('きせき', '軌跡'),
			_Utils_Tuple2('ぎせき', '議席'),
			_Utils_Tuple2('きせつふう', '季節風'),
			_Utils_Tuple2('きせん', '汽船'),
			_Utils_Tuple2('きぜん', 'きぜん'),
			_Utils_Tuple2('ぎぜん', '偽善'),
			_Utils_Tuple2('ぎぜんしゃ', '偽善者'),
			_Utils_Tuple2('きそ', '起訴'),
			_Utils_Tuple2('ぎぞう', '偽造'),
			_Utils_Tuple2('きぞく', '貴族'),
			_Utils_Tuple2('きぞく', '帰属'),
			_Utils_Tuple2('ぎそく', '義足'),
			_Utils_Tuple2('きそたいおん', '基礎体温'),
			_Utils_Tuple2('きそん', '既存'),
			_Utils_Tuple2('きたい', '機体'),
			_Utils_Tuple2('きたす', '来す'),
			_Utils_Tuple2('きたる', '来たる'),
			_Utils_Tuple2('きち', '吉'),
			_Utils_Tuple2('きちがい', '気違い'),
			_Utils_Tuple2('きちょう', '記帳'),
			_Utils_Tuple2('きちょう', '基調'),
			_Utils_Tuple2('きっと', 'キット'),
			_Utils_Tuple2('きっぽう', '吉報'),
			_Utils_Tuple2('きてい', '規定'),
			_Utils_Tuple2('きてれつ', 'きてれつ'),
			_Utils_Tuple2('きてん', '機転'),
			_Utils_Tuple2('きどう', '軌道'),
			_Utils_Tuple2('きどうたい', '機動隊'),
			_Utils_Tuple2('きどり', '気取り'),
			_Utils_Tuple2('きどる', '気取る'),
			_Utils_Tuple2('きなが', '気長'),
			_Utils_Tuple2('きねんひ', '記念碑'),
			_Utils_Tuple2('きば', '牙'),
			_Utils_Tuple2('きはく', '希薄'),
			_Utils_Tuple2('きばつ', '奇抜'),
			_Utils_Tuple2('きばむ', '黄ばむ'),
			_Utils_Tuple2('きはん', '規範'),
			_Utils_Tuple2('きばん', '基盤'),
			_Utils_Tuple2('きびきび', 'きびきび'),
			_Utils_Tuple2('きひん', '気品'),
			_Utils_Tuple2('きほう', '気泡'),
			_Utils_Tuple2('ぎほう', '技法'),
			_Utils_Tuple2('きぼり', '木彫り'),
			_Utils_Tuple2('きほんてきじんけん', '基本的人権'),
			_Utils_Tuple2('きまずい', '気まずい'),
			_Utils_Tuple2('きまりもんく', '決まり文句'),
			_Utils_Tuple2('きみつ', '機密'),
			_Utils_Tuple2('きむずかしい', '気難しい'),
			_Utils_Tuple2('きめつける', '決め付ける'),
			_Utils_Tuple2('きめて', '決め手'),
			_Utils_Tuple2('きも', '肝'),
			_Utils_Tuple2('きもん', '鬼門'),
			_Utils_Tuple2('ぎや', 'ギア'),
			_Utils_Tuple2('ぎゃくさつ', '虐殺'),
			_Utils_Tuple2('ぎゃくしゅう', '逆襲'),
			_Utils_Tuple2('ぎゃくじょう', '逆上'),
			_Utils_Tuple2('ぎゃくせつ', '逆説'),
			_Utils_Tuple2('ぎゃくたんち', '逆探知'),
			_Utils_Tuple2('きゃくひき', '客引き'),
			_Utils_Tuple2('きゃくほん', '脚本'),
			_Utils_Tuple2('きゃしゃ', 'きゃしゃ'),
			_Utils_Tuple2('きやすい', '気安い'),
			_Utils_Tuple2('きゃすてぃんぐ', 'キャスティング'),
			_Utils_Tuple2('きゃすと', 'キャスト'),
			_Utils_Tuple2('きゃっか', '却下'),
			_Utils_Tuple2('きゃっちふれーず', 'キャッチフレーズ'),
			_Utils_Tuple2('きゃびん', 'キャビン'),
			_Utils_Tuple2('ぎゃら', 'ギャラ'),
			_Utils_Tuple2('きゃんぴんぐ', 'キャンピング'),
			_Utils_Tuple2('きゅういん', '吸引'),
			_Utils_Tuple2('きゅうえん', '救援'),
			_Utils_Tuple2('きゅうかい', '球界'),
			_Utils_Tuple2('きゅうかく', '嗅覚'),
			_Utils_Tuple2('きゅうきょ', '急遽'),
			_Utils_Tuple2('きゅうきょく', '究極'),
			_Utils_Tuple2('きゅうくつ', '窮屈'),
			_Utils_Tuple2('きゅうけい', '求刑'),
			_Utils_Tuple2('きゅうけつ', '吸血'),
			_Utils_Tuple2('きゅうけつき', '吸血鬼'),
			_Utils_Tuple2('きゅうさい', '救済'),
			_Utils_Tuple2('きゅうじ', '球児'),
			_Utils_Tuple2('きゅうしき', '旧式'),
			_Utils_Tuple2('きゅうしょく', '求職'),
			_Utils_Tuple2('きゅうしん', '休診'),
			_Utils_Tuple2('きゅうすい', '吸水'),
			_Utils_Tuple2('きゅうすい', '給水'),
			_Utils_Tuple2('きゅうせい', '旧姓'),
			_Utils_Tuple2('きゅうそく', '球速'),
			_Utils_Tuple2('きゅうだん', '球団'),
			_Utils_Tuple2('きゅうてい', '宮廷'),
			_Utils_Tuple2('きゅうでん', '宮殿'),
			_Utils_Tuple2('きゅうとう', '給湯'),
			_Utils_Tuple2('きゅうにゅう', '吸入'),
			_Utils_Tuple2('きゅうふ', '給付'),
			_Utils_Tuple2('きゅうぼう', '窮乏'),
			_Utils_Tuple2('きゅうむ', '急務'),
			_Utils_Tuple2('きゅうめい', '救命'),
			_Utils_Tuple2('きよ', '寄与'),
			_Utils_Tuple2('きよい', '清い'),
			_Utils_Tuple2('きょう', '凶'),
			_Utils_Tuple2('きょうあく', '凶悪'),
			_Utils_Tuple2('きょうい', '脅威'),
			_Utils_Tuple2('きょういくいいんかい', '教育委員会'),
			_Utils_Tuple2('きょういくかてい', '教育課程'),
			_Utils_Tuple2('きょういくじっしゅう', '教育実習'),
			_Utils_Tuple2('きょうかつ', '恐喝'),
			_Utils_Tuple2('きょうかん', '教官'),
			_Utils_Tuple2('きょうき', '凶器'),
			_Utils_Tuple2('きょうぎ', '協議'),
			_Utils_Tuple2('きょうぎかい', '協議会'),
			_Utils_Tuple2('きょうぐう', '境遇'),
			_Utils_Tuple2('きょうくん', '教訓'),
			_Utils_Tuple2('きょうけん', '強肩'),
			_Utils_Tuple2('きょうげん', '狂言'),
			_Utils_Tuple2('ぎょうこ', '凝固'),
			_Utils_Tuple2('きょうこう', '強行'),
			_Utils_Tuple2('きょうこう', '強硬'),
			_Utils_Tuple2('きょうさく', '凶作'),
			_Utils_Tuple2('きょうさん', '協賛'),
			_Utils_Tuple2('きょうじゅ', '享受'),
			_Utils_Tuple2('ぎょうしゅ', '業種'),
			_Utils_Tuple2('きょうしゅう', '教習'),
			_Utils_Tuple2('きょうしゅう', '郷愁'),
			_Utils_Tuple2('きょうしゅく', '恐縮'),
			_Utils_Tuple2('ぎょうしゅく', '凝縮'),
			_Utils_Tuple2('きょうじゅつ', '供述'),
			_Utils_Tuple2('きょうしょくいん', '教職員'),
			_Utils_Tuple2('きょうしん', '強震'),
			_Utils_Tuple2('きょうずる', '興ずる'),
			_Utils_Tuple2('きょうせい', '矯正'),
			_Utils_Tuple2('きょうせい', '共生'),
			_Utils_Tuple2('ぎょうせいかいかく', '行政改革'),
			_Utils_Tuple2('きょうせいしっこう', '強制執行'),
			_Utils_Tuple2('ぎょうせいしょぶん', '行政処分'),
			_Utils_Tuple2('きょうせいそうかん', '強制送還'),
			_Utils_Tuple2('きょうそ', '教祖'),
			_Utils_Tuple2('きょうだ', '強打'),
			_Utils_Tuple2('きょうだん', '教壇'),
			_Utils_Tuple2('きょうち', '境地'),
			_Utils_Tuple2('きょうつうこう', '共通項'),
			_Utils_Tuple2('きょうてい', '協定'),
			_Utils_Tuple2('ぎょうてん', '仰天'),
			_Utils_Tuple2('きょうと', '教徒'),
			_Utils_Tuple2('きょうどうくみあい', '協同組合'),
			_Utils_Tuple2('きょうばい', '競売'),
			_Utils_Tuple2('きょうはく', '脅迫'),
			_Utils_Tuple2('きょうはくざい', '脅迫罪'),
			_Utils_Tuple2('きょうふしょう', '恐怖症'),
			_Utils_Tuple2('きょうほん', '教本'),
			_Utils_Tuple2('きょうめい', '共鳴'),
			_Utils_Tuple2('きょうゆ', '教諭'),
			_Utils_Tuple2('きょうり', '郷里'),
			_Utils_Tuple2('きょうりゅう', '恐竜'),
			_Utils_Tuple2('きょうれつ', '強烈'),
			_Utils_Tuple2('きょくげん', '極限'),
			_Utils_Tuple2('きょくしょう', '極小'),
			_Utils_Tuple2('きょくとう', '極東'),
			_Utils_Tuple2('きょくばん', '局番'),
			_Utils_Tuple2('きょくめん', '局面'),
			_Utils_Tuple2('きょくもく', '曲目'),
			_Utils_Tuple2('ぎょこう', '漁港'),
			_Utils_Tuple2('きょてん', '拠点'),
			_Utils_Tuple2('きょひけん', '拒否権'),
			_Utils_Tuple2('きよめる', '清める'),
			_Utils_Tuple2('きよらか', '清らか'),
			_Utils_Tuple2('きらす', '切らす'),
			_Utils_Tuple2('きらめく', 'きらめく'),
			_Utils_Tuple2('きり', 'きり'),
			_Utils_Tuple2('きり', '切り'),
			_Utils_Tuple2('きり', '桐'),
			_Utils_Tuple2('きりあげる', '切り上げる'),
			_Utils_Tuple2('きりおとす', '切り落とす'),
			_Utils_Tuple2('きりかえ', '切り替え'),
			_Utils_Tuple2('きりかえし', '切り返し'),
			_Utils_Tuple2('きりかえる', '切り替える'),
			_Utils_Tuple2('きりかわる', '切り替わる'),
			_Utils_Tuple2('きりくち', '切り口'),
			_Utils_Tuple2('きりしたん', 'キリシタン'),
			_Utils_Tuple2('きりすて', '切り捨て'),
			_Utils_Tuple2('きりすてる', '切り捨てる'),
			_Utils_Tuple2('きりだす', '切り出す'),
			_Utils_Tuple2('きりつ', '規律'),
			_Utils_Tuple2('きりつめる', '切り詰める'),
			_Utils_Tuple2('きりとり', '切り取り'),
			_Utils_Tuple2('きりぬき', '切り抜き'),
			_Utils_Tuple2('きりぬく', '切り抜く'),
			_Utils_Tuple2('きりぬける', '切り抜ける'),
			_Utils_Tuple2('きりひらく', '切り開く'),
			_Utils_Tuple2('きりふき', '霧吹き'),
			_Utils_Tuple2('きりゅう', '気流'),
			_Utils_Tuple2('ぎりょう', '技量'),
			_Utils_Tuple2('きるてぃんぐ', 'キルティング'),
			_Utils_Tuple2('きれあじ', '切れ味'),
			_Utils_Tuple2('ぎれい', '儀礼'),
			_Utils_Tuple2('きれつ', '亀裂'),
			_Utils_Tuple2('きわだつ', '際立つ'),
			_Utils_Tuple2('きわどい', '際どい'),
			_Utils_Tuple2('きわまる', '極まる'),
			_Utils_Tuple2('きわみ', '極み'),
			_Utils_Tuple2('きわめ', '極め'),
			_Utils_Tuple2('きわめて', '極めて'),
			_Utils_Tuple2('きわめる', '極める'),
			_Utils_Tuple2('きん', '菌'),
			_Utils_Tuple2('きんか', '金貨'),
			_Utils_Tuple2('ぎんか', '銀貨'),
			_Utils_Tuple2('きんかい', '金塊'),
			_Utils_Tuple2('きんかい', '近海'),
			_Utils_Tuple2('ぎんかく', '銀閣'),
			_Utils_Tuple2('ぎんがけい', '銀河系'),
			_Utils_Tuple2('きんきん', '近々'),
			_Utils_Tuple2('きんこう', '均衡'),
			_Utils_Tuple2('きんじる', '禁じる'),
			_Utils_Tuple2('きんしん', '謹慎'),
			_Utils_Tuple2('きんずる', '禁ずる'),
			_Utils_Tuple2('きんせい', '金星'),
			_Utils_Tuple2('きんせい', '近世'),
			_Utils_Tuple2('ぎんせかい', '銀世界'),
			_Utils_Tuple2('きんせつ', '近接'),
			_Utils_Tuple2('きんだいぶんがく', '近代文学'),
			_Utils_Tuple2('きんちゃく', 'きんちゃく'),
			_Utils_Tuple2('ぎんなん', 'ぎんなん'),
			_Utils_Tuple2('きんぱく', '緊迫'),
			_Utils_Tuple2('きんぱく', '金ぱく'),
			_Utils_Tuple2('きんぴか', '金ぴか'),
			_Utils_Tuple2('きんぴら', 'きんぴら'),
			_Utils_Tuple2('きんもつ', '禁物'),
			_Utils_Tuple2('きんゆう', '金融'),
			_Utils_Tuple2('きんり', '金利'),
			_Utils_Tuple2('きんりょく', '筋力'),
			_Utils_Tuple2('きんりん', '近隣'),
			_Utils_Tuple2('きんろう', '勤労'),
			_Utils_Tuple2('きんろうしゃ', '勤労者'),
			_Utils_Tuple2('く', '句'),
			_Utils_Tuple2('ぐ', '具'),
			_Utils_Tuple2('くい', '悔い'),
			_Utils_Tuple2('くい', '食い'),
			_Utils_Tuple2('くい', 'くい'),
			_Utils_Tuple2('くいこむ', '食い込む'),
			_Utils_Tuple2('くいしんぼう', '食いしん坊'),
			_Utils_Tuple2('くいちがい', '食い違い'),
			_Utils_Tuple2('くいちがう', '食い違う'),
			_Utils_Tuple2('くいつぶす', '食いつぶす'),
			_Utils_Tuple2('くいとめる', '食い止める'),
			_Utils_Tuple2('くいる', '悔いる'),
			_Utils_Tuple2('くう', '空'),
			_Utils_Tuple2('くうきょ', '空虚'),
			_Utils_Tuple2('くうぐん', '空軍'),
			_Utils_Tuple2('くうぜん', '空前'),
			_Utils_Tuple2('ぐうたら', 'ぐうたら'),
			_Utils_Tuple2('くーでたー', 'クーデター'),
			_Utils_Tuple2('くうてん', '空転'),
			_Utils_Tuple2('くうどう', '空洞'),
			_Utils_Tuple2('くうぼ', '空母'),
			_Utils_Tuple2('くかく', '区画'),
			_Utils_Tuple2('くき', '茎'),
			_Utils_Tuple2('くぎ', 'くぎ'),
			_Utils_Tuple2('くぎり', '区切り'),
			_Utils_Tuple2('くくる', 'くくる'),
			_Utils_Tuple2('くぐる', 'くぐる'),
			_Utils_Tuple2('くさみ', '臭み'),
			_Utils_Tuple2('くさやきゅう', '草野球'),
			_Utils_Tuple2('くし', '駆使'),
			_Utils_Tuple2('ぐしゃぐしゃ', 'ぐしゃぐしゃ'),
			_Utils_Tuple2('くじょ', '駆除'),
			_Utils_Tuple2('くしょう', '苦笑'),
			_Utils_Tuple2('くしん', '苦心'),
			_Utils_Tuple2('くすぐる', 'くすぐる'),
			_Utils_Tuple2('ぐずつく', 'ぐずつく'),
			_Utils_Tuple2('くすむ', 'くすむ'),
			_Utils_Tuple2('ぐずる', '愚図る'),
			_Utils_Tuple2('くせもの', 'くせ者'),
			_Utils_Tuple2('くそ', 'くそ'),
			_Utils_Tuple2('くそ', 'くそ'),
			_Utils_Tuple2('くだ', '管'),
			_Utils_Tuple2('ぐたい', '具体'),
			_Utils_Tuple2('ぐたいさく', '具体策'),
			_Utils_Tuple2('くだける', '砕ける'),
			_Utils_Tuple2('くだす', '下す'),
			_Utils_Tuple2('くちあたり', '口当たり'),
			_Utils_Tuple2('くちかず', '口数'),
			_Utils_Tuple2('くちぐせ', '口癖'),
			_Utils_Tuple2('くちぐち', '口々'),
			_Utils_Tuple2('くちぐちに', '口々に'),
			_Utils_Tuple2('くちこみ', '口コミ'),
			_Utils_Tuple2('くちずさむ', '口ずさむ'),
			_Utils_Tuple2('くちだし', '口出し'),
			_Utils_Tuple2('くちづたえ', '口伝え'),
			_Utils_Tuple2('くちどめ', '口止め'),
			_Utils_Tuple2('くちぶり', '口振り'),
			_Utils_Tuple2('くちやくそく', '口約束'),
			_Utils_Tuple2('くちょう', '口調'),
			_Utils_Tuple2('くつがえす', '覆す'),
			_Utils_Tuple2('くつじょく', '屈辱'),
			_Utils_Tuple2('くっする', '屈する'),
			_Utils_Tuple2('くっせつ', '屈折'),
			_Utils_Tuple2('くっぷく', '屈伏'),
			_Utils_Tuple2('くつろぐ', 'くつろぐ'),
			_Utils_Tuple2('くどく', '口説く'),
			_Utils_Tuple2('くにく', '苦肉'),
			_Utils_Tuple2('くまで', '熊手'),
			_Utils_Tuple2('くみあう', '組み合う'),
			_Utils_Tuple2('くみいれる', '組み入れる'),
			_Utils_Tuple2('くみきょく', '組曲'),
			_Utils_Tuple2('くみこむ', '組み込む'),
			_Utils_Tuple2('くみたて', '組み立て'),
			_Utils_Tuple2('くみたてる', '組み立てる'),
			_Utils_Tuple2('くみとる', 'くみ取る'),
			_Utils_Tuple2('くむ', 'くむ'),
			_Utils_Tuple2('くめん', '工面'),
			_Utils_Tuple2('くやみ', '悔やみ'),
			_Utils_Tuple2('くやむ', '悔やむ'),
			_Utils_Tuple2('くよう', '供養'),
			_Utils_Tuple2('くら', '蔵'),
			_Utils_Tuple2('くらう', '食らう'),
			_Utils_Tuple2('くらげ', 'くらげ'),
			_Utils_Tuple2('くらしむき', '暮らし向き'),
			_Utils_Tuple2('ぐらでーしょん', 'グラデーション'),
			_Utils_Tuple2('ぐらにゅーとう', 'グラニュー糖'),
			_Utils_Tuple2('ぐらびあ', 'グラビア'),
			_Utils_Tuple2('ぐらふぃっく', 'グラフィック'),
			_Utils_Tuple2('くらべもの', '比べ物'),
			_Utils_Tuple2('くらます', 'くらます'),
			_Utils_Tuple2('くらやみ', '暗やみ'),
			_Utils_Tuple2('くらりねっと', 'クラリネット'),
			_Utils_Tuple2('くりあげる', '繰り上げる'),
			_Utils_Tuple2('くりくり', 'くりくり'),
			_Utils_Tuple2('くりけっと', 'クリケット'),
			_Utils_Tuple2('くりすたる', 'クリスタル'),
			_Utils_Tuple2('くりだす', '繰り出す'),
			_Utils_Tuple2('ぐりっぷ', 'グリップ'),
			_Utils_Tuple2('くりひろげる', '繰り広げる'),
			_Utils_Tuple2('ぐる', 'ぐる'),
			_Utils_Tuple2('くるい', '狂い'),
			_Utils_Tuple2('くるう', '狂う'),
			_Utils_Tuple2('くるー', 'クルー'),
			_Utils_Tuple2('くるぶし', 'くるぶし'),
			_Utils_Tuple2('ぐるみ', 'ぐるみ'),
			_Utils_Tuple2('くるむ', 'くるむ'),
			_Utils_Tuple2('くれーじー', 'クレージー'),
			_Utils_Tuple2('くれーたー', 'クレーター'),
			_Utils_Tuple2('ぐれーど', 'グレード'),
			_Utils_Tuple2('くれーん', 'クレーン'),
			_Utils_Tuple2('くれぐれ', 'くれぐれ'),
			_Utils_Tuple2('くれない', '紅'),
			_Utils_Tuple2('くれんざー', 'クレンザー'),
			_Utils_Tuple2('くろうと', '玄人'),
			_Utils_Tuple2('くろおび', '黒帯'),
			_Utils_Tuple2('くろす', 'クロス'),
			_Utils_Tuple2('くろずむ', '黒ずむ'),
			_Utils_Tuple2('ぐろてすく', 'グロテスク'),
			_Utils_Tuple2('くろふね', '黒船'),
			_Utils_Tuple2('くろまめ', '黒豆'),
			_Utils_Tuple2('くわえる', '銜える'),
			_Utils_Tuple2('くわがた', '鍬形'),
			_Utils_Tuple2('くわけ', '区分け'),
			_Utils_Tuple2('くわだて', '企て'),
			_Utils_Tuple2('ぐんかん', '軍艦'),
			_Utils_Tuple2('ぐんこく', '軍国'),
			_Utils_Tuple2('ぐんこくしゅぎ', '軍国主義'),
			_Utils_Tuple2('ぐんじ', '軍事'),
			_Utils_Tuple2('ぐんしゅう', '群集'),
			_Utils_Tuple2('くんしょう', '勲章'),
			_Utils_Tuple2('ぐんぜい', '軍勢'),
			_Utils_Tuple2('ぐんと', 'ぐんと'),
			_Utils_Tuple2('ぐんばい', '軍配'),
			_Utils_Tuple2('ぐんび', '軍備'),
			_Utils_Tuple2('ぐんぶ', '軍部'),
			_Utils_Tuple2('くんりん', '君臨'),
			_Utils_Tuple2('け', 'け'),
			_Utils_Tuple2('けあな', '毛穴'),
			_Utils_Tuple2('けいい', '経緯'),
			_Utils_Tuple2('けいえん', '敬遠'),
			_Utils_Tuple2('けいかい', '警戒'),
			_Utils_Tuple2('けいかいしん', '警戒心'),
			_Utils_Tuple2('けいかん', '景観'),
			_Utils_Tuple2('けいき', '刑期'),
			_Utils_Tuple2('けいき', '契機'),
			_Utils_Tuple2('けいご', '警護'),
			_Utils_Tuple2('けいこう', '蛍光'),
			_Utils_Tuple2('けいこうとう', '蛍光灯'),
			_Utils_Tuple2('けいこく', '渓谷'),
			_Utils_Tuple2('けいさい', '掲載'),
			_Utils_Tuple2('けいざいせいちょうりつ', '経済成長率'),
			_Utils_Tuple2('けいし', '警視'),
			_Utils_Tuple2('けいじさいばん', '刑事裁判'),
			_Utils_Tuple2('けいしちょう', '警視庁'),
			_Utils_Tuple2('けいしゃ', '傾斜'),
			_Utils_Tuple2('けいしょう', '継承'),
			_Utils_Tuple2('けいしょう', '軽症'),
			_Utils_Tuple2('けいしょう', '敬称'),
			_Utils_Tuple2('けいじょう', '形状'),
			_Utils_Tuple2('けいせい', '形勢'),
			_Utils_Tuple2('けいそく', '計測'),
			_Utils_Tuple2('けいそつ', '軽率'),
			_Utils_Tuple2('けいたい', '形態'),
			_Utils_Tuple2('けいだい', '境内'),
			_Utils_Tuple2('けいばつ', '刑罰'),
			_Utils_Tuple2('けいぶ', '警部'),
			_Utils_Tuple2('げいふう', '芸風'),
			_Utils_Tuple2('けいべつ', 'けいべつ'),
			_Utils_Tuple2('けいほう', '刑法'),
			_Utils_Tuple2('けいほう', '警報'),
			_Utils_Tuple2('げいめい', '芸名'),
			_Utils_Tuple2('けいゆ', '軽油'),
			_Utils_Tuple2('けいりゅう', '渓流'),
			_Utils_Tuple2('けいりょう', '計量'),
			_Utils_Tuple2('けいれつ', '系列'),
			_Utils_Tuple2('けいろ', '経路'),
			_Utils_Tuple2('けいろう', '敬老'),
			_Utils_Tuple2('けーじ', 'ケージ'),
			_Utils_Tuple2('けーぶる', 'ケーブル'),
			_Utils_Tuple2('けーぶるかー', 'ケーブルカー'),
			_Utils_Tuple2('けがれ', '汚れ'),
			_Utils_Tuple2('げきたい', '撃退'),
			_Utils_Tuple2('げきだん', '劇団'),
			_Utils_Tuple2('げきつい', '撃墜'),
			_Utils_Tuple2('げきてき', '劇的'),
			_Utils_Tuple2('げきとつ', '激突'),
			_Utils_Tuple2('げきやく', '劇薬'),
			_Utils_Tuple2('げきれい', '激励'),
			_Utils_Tuple2('げし', '夏至'),
			_Utils_Tuple2('けしさる', '消し去る'),
			_Utils_Tuple2('けじめ', 'けじめ'),
			_Utils_Tuple2('けしょうなおし', '化粧直し'),
			_Utils_Tuple2('けずりぶし', '削り節'),
			_Utils_Tuple2('けたたましい', 'けたたましい'),
			_Utils_Tuple2('けたちがい', '桁違い'),
			_Utils_Tuple2('けだま', '毛玉'),
			_Utils_Tuple2('げだん', '下段'),
			_Utils_Tuple2('けつ', '穴'),
			_Utils_Tuple2('けついん', '欠員'),
			_Utils_Tuple2('けつえん', '血縁'),
			_Utils_Tuple2('けっかく', '結核'),
			_Utils_Tuple2('けっかん', '欠陥'),
			_Utils_Tuple2('けっかん', '血管'),
			_Utils_Tuple2('げっけい', '月経'),
			_Utils_Tuple2('けっこう', '血行'),
			_Utils_Tuple2('けつごう', '結合'),
			_Utils_Tuple2('げっこう', '月光'),
			_Utils_Tuple2('けっさい', '決済'),
			_Utils_Tuple2('けっさく', '傑作'),
			_Utils_Tuple2('けっさん', '決算'),
			_Utils_Tuple2('けつじょ', '欠如'),
			_Utils_Tuple2('けっしょう', '結晶'),
			_Utils_Tuple2('げっそり', 'げっそり'),
			_Utils_Tuple2('げっつー', 'ゲッツー'),
			_Utils_Tuple2('げっぷ', 'げっぷ'),
			_Utils_Tuple2('げっぷ', '月賦'),
			_Utils_Tuple2('けっぺき', '潔癖'),
			_Utils_Tuple2('けつべつ', '決別'),
			_Utils_Tuple2('けつぼう', '欠乏'),
			_Utils_Tuple2('けなす', 'けなす'),
			_Utils_Tuple2('けぬき', '毛抜き'),
			_Utils_Tuple2('けねん', '懸念'),
			_Utils_Tuple2('けはい', '気配'),
			_Utils_Tuple2('けみかる', 'ケミカル'),
			_Utils_Tuple2('けむし', '毛虫'),
			_Utils_Tuple2('けむたい', '煙たい'),
			_Utils_Tuple2('けむる', '煙る'),
			_Utils_Tuple2('けもの', '獣'),
			_Utils_Tuple2('けらい', '家来'),
			_Utils_Tuple2('げらげら', 'げらげら'),
			_Utils_Tuple2('けり', 'けり'),
			_Utils_Tuple2('げりら', 'ゲリラ'),
			_Utils_Tuple2('げるまん', 'ゲルマン'),
			_Utils_Tuple2('げろ', 'げろ'),
			_Utils_Tuple2('けろっと', 'けろっと'),
			_Utils_Tuple2('けわしい', '険しい'),
			_Utils_Tuple2('けん', '圏'),
			_Utils_Tuple2('けん', '間'),
			_Utils_Tuple2('けん', '剣'),
			_Utils_Tuple2('げん', '言'),
			_Utils_Tuple2('げん', '源'),
			_Utils_Tuple2('げんあん', '原案'),
			_Utils_Tuple2('けんい', '権威'),
			_Utils_Tuple2('げんえき', '原液'),
			_Utils_Tuple2('けんお', '嫌悪'),
			_Utils_Tuple2('けんおん', '検温'),
			_Utils_Tuple2('げんか', '原価'),
			_Utils_Tuple2('けんかい', '見解'),
			_Utils_Tuple2('げんかい', '厳戒'),
			_Utils_Tuple2('げんかく', '厳格'),
			_Utils_Tuple2('げんきゅう', '言及'),
			_Utils_Tuple2('けんきょ', '謙虚'),
			_Utils_Tuple2('けんきょ', '検挙'),
			_Utils_Tuple2('けんきょ', '謙虚'),
			_Utils_Tuple2('けんぎょう', '兼業'),
			_Utils_Tuple2('げんきょう', '元凶'),
			_Utils_Tuple2('けんけい', '県警'),
			_Utils_Tuple2('げんけい', '原形'),
			_Utils_Tuple2('げんけい', '原型'),
			_Utils_Tuple2('けんげん', '権限'),
			_Utils_Tuple2('げんご', '原語'),
			_Utils_Tuple2('げんごう', '元号'),
			_Utils_Tuple2('げんこうはん', '現行犯'),
			_Utils_Tuple2('けんこく', '建国'),
			_Utils_Tuple2('げんこく', '原告'),
			_Utils_Tuple2('げんこつ', 'げん骨'),
			_Utils_Tuple2('けんざい', '健在'),
			_Utils_Tuple2('けんざい', '顕在'),
			_Utils_Tuple2('けんざい', '建材'),
			_Utils_Tuple2('げんさくしゃ', '原作者'),
			_Utils_Tuple2('けんさつ', '検察'),
			_Utils_Tuple2('けんさつかん', '検察官'),
			_Utils_Tuple2('けんさつちょう', '検察庁'),
			_Utils_Tuple2('けんざん', '剣山'),
			_Utils_Tuple2('けんじ', '検事'),
			_Utils_Tuple2('けんじ', '堅持'),
			_Utils_Tuple2('げんし', '原始'),
			_Utils_Tuple2('げんし', '原子'),
			_Utils_Tuple2('けんしき', '見識'),
			_Utils_Tuple2('げんしじだい', '原始時代'),
			_Utils_Tuple2('げんしてき', '原始的'),
			_Utils_Tuple2('げんしゅ', '元首'),
			_Utils_Tuple2('げんじゅうみん', '原住民'),
			_Utils_Tuple2('げんしゅく', '厳粛'),
			_Utils_Tuple2('けんしゅつ', '検出'),
			_Utils_Tuple2('げんしょ', '原書'),
			_Utils_Tuple2('けんしょう', '懸賞'),
			_Utils_Tuple2('けんしょう', '検証'),
			_Utils_Tuple2('けんじょう', '献上'),
			_Utils_Tuple2('けんじょうしゃ', '健常者'),
			_Utils_Tuple2('げんしろ', '原子炉'),
			_Utils_Tuple2('けんしん', '献身'),
			_Utils_Tuple2('げんじん', '原人'),
			_Utils_Tuple2('けんすい', '懸垂'),
			_Utils_Tuple2('げんせん', '源泉'),
			_Utils_Tuple2('げんせん', '厳選'),
			_Utils_Tuple2('げんぜん', '厳然'),
			_Utils_Tuple2('げんせんちょうしゅう', '源泉徴収'),
			_Utils_Tuple2('げんそ', '元素'),
			_Utils_Tuple2('けんぞう', '建造'),
			_Utils_Tuple2('げんそう', '幻想'),
			_Utils_Tuple2('げんぞう', '現像'),
			_Utils_Tuple2('げんそうてき', '幻想的'),
			_Utils_Tuple2('げんそん', '現存'),
			_Utils_Tuple2('けんたいき', 'けん怠期'),
			_Utils_Tuple2('けんち', '見地'),
			_Utils_Tuple2('けんちじ', '県知事'),
			_Utils_Tuple2('けんちょ', '顕著'),
			_Utils_Tuple2('けんていしけん', '検定試験'),
			_Utils_Tuple2('けんとう', '健闘'),
			_Utils_Tuple2('げんなり', 'げんなり'),
			_Utils_Tuple2('げんに', '現に'),
			_Utils_Tuple2('げんばかんとく', '現場監督'),
			_Utils_Tuple2('げんばく', '原爆'),
			_Utils_Tuple2('けんびきょう', '顕微鏡'),
			_Utils_Tuple2('けんぶん', '見聞'),
			_Utils_Tuple2('げんぶん', '原文'),
			_Utils_Tuple2('げんみつ', '厳密'),
			_Utils_Tuple2('けんめい', '賢明'),
			_Utils_Tuple2('げんめつ', '幻滅'),
			_Utils_Tuple2('けんやく', '倹約'),
			_Utils_Tuple2('げんゆ', '原油'),
			_Utils_Tuple2('げんろん', '言論'),
			_Utils_Tuple2('ご', '御'),
			_Utils_Tuple2('ご', '碁'),
			_Utils_Tuple2('こあ', 'コア'),
			_Utils_Tuple2('こいくち', '濃い口'),
			_Utils_Tuple2('こいする', '恋'),
			_Utils_Tuple2('こいぶみ', '恋文'),
			_Utils_Tuple2('こいる', 'コイル'),
			_Utils_Tuple2('こう', '光'),
			_Utils_Tuple2('こう', '甲'),
			_Utils_Tuple2('こう', '公'),
			_Utils_Tuple2('こう', '公'),
			_Utils_Tuple2('こう', '溝'),
			_Utils_Tuple2('こう', '項'),
			_Utils_Tuple2('こう', '香'),
			_Utils_Tuple2('こう', '請う'),
			_Utils_Tuple2('こう', '功'),
			_Utils_Tuple2('こう', '行'),
			_Utils_Tuple2('こう', '膏'),
			_Utils_Tuple2('こう', '鋼'),
			_Utils_Tuple2('こう', '孔'),
			_Utils_Tuple2('こう', '候'),
			_Utils_Tuple2('ごう', '号'),
			_Utils_Tuple2('ごう', '業'),
			_Utils_Tuple2('こうあつ', '高圧'),
			_Utils_Tuple2('こうあん', '考案'),
			_Utils_Tuple2('こうあんいいんかい', '公安委員会'),
			_Utils_Tuple2('こうい', '更衣'),
			_Utils_Tuple2('こういき', '広域'),
			_Utils_Tuple2('こういしょう', '後遺症'),
			_Utils_Tuple2('こうう', '降雨'),
			_Utils_Tuple2('こううりょう', '降雨量'),
			_Utils_Tuple2('こうえん', '後援'),
			_Utils_Tuple2('こうおつ', '甲乙'),
			_Utils_Tuple2('こうか', '硬化'),
			_Utils_Tuple2('こうか', '高架'),
			_Utils_Tuple2('こうかい', '航海'),
			_Utils_Tuple2('こうがい', '口外'),
			_Utils_Tuple2('ごうかい', '豪快'),
			_Utils_Tuple2('ごうがい', '号外'),
			_Utils_Tuple2('ごうかん', '強姦'),
			_Utils_Tuple2('こうき', '高貴'),
			_Utils_Tuple2('ごうきゅう', '号泣'),
			_Utils_Tuple2('こうきょう', '好況'),
			_Utils_Tuple2('こうきょう', '交響'),
			_Utils_Tuple2('こうぎょう', '鉱業'),
			_Utils_Tuple2('こうきょうきょく', '交響曲'),
			_Utils_Tuple2('こうきょうだんたい', '公共団体'),
			_Utils_Tuple2('こうきょうほうそう', '公共放送'),
			_Utils_Tuple2('こうきん', '抗菌'),
			_Utils_Tuple2('こうぐ', '工具'),
			_Utils_Tuple2('こうけい', '後継'),
			_Utils_Tuple2('こうけん', '貢献'),
			_Utils_Tuple2('こうごう', '皇后'),
			_Utils_Tuple2('こうさく', '工作'),
			_Utils_Tuple2('こうさく', '耕作'),
			_Utils_Tuple2('こうさつ', '考察'),
			_Utils_Tuple2('こうさん', '降参'),
			_Utils_Tuple2('こうざん', '鉱山'),
			_Utils_Tuple2('こうざん', '高山'),
			_Utils_Tuple2('こうし', '行使'),
			_Utils_Tuple2('こうし', '公私'),
			_Utils_Tuple2('こうし', '格子'),
			_Utils_Tuple2('こうじ', '公示'),
			_Utils_Tuple2('こうしき', '硬式'),
			_Utils_Tuple2('こうしゅう', '口臭'),
			_Utils_Tuple2('こうしゅうは', '高周波'),
			_Utils_Tuple2('こうじょ', '控除'),
			_Utils_Tuple2('こうしょう', '高尚'),
			_Utils_Tuple2('こうしん', '交信'),
			_Utils_Tuple2('こうしんきょく', '行進曲'),
			_Utils_Tuple2('こうず', '構図'),
			_Utils_Tuple2('こうすい', '降水'),
			_Utils_Tuple2('こうずる', '講ずる'),
			_Utils_Tuple2('こうせい', '更正'),
			_Utils_Tuple2('こうせい', '厚生'),
			_Utils_Tuple2('こうせい', '攻勢'),
			_Utils_Tuple2('こうせいしょう', '厚生省'),
			_Utils_Tuple2('ごうせいせんい', '合成繊維'),
			_Utils_Tuple2('こうせいねんきん', '厚生年金'),
			_Utils_Tuple2('こうせいぶっしつ', '抗生物質'),
			_Utils_Tuple2('こうせつ', '降雪'),
			_Utils_Tuple2('ごうせつ', '豪雪'),
			_Utils_Tuple2('こうせん', '光線'),
			_Utils_Tuple2('こうそ', '控訴'),
			_Utils_Tuple2('こうそ', '酵素'),
			_Utils_Tuple2('こうそう', '抗争'),
			_Utils_Tuple2('こうそくりょく', '拘束力'),
			_Utils_Tuple2('こうたく', '光沢'),
			_Utils_Tuple2('ごうだつ', '強奪'),
			_Utils_Tuple2('こうだん', '公団'),
			_Utils_Tuple2('こうだんじゅうたく', '公団住宅'),
			_Utils_Tuple2('こうちく', '構築'),
			_Utils_Tuple2('こうちょう', '紅潮'),
			_Utils_Tuple2('こうてい', '皇帝'),
			_Utils_Tuple2('こうてい', '行程'),
			_Utils_Tuple2('こうてい', '工程'),
			_Utils_Tuple2('こうでん', '香典'),
			_Utils_Tuple2('こうとう', '高騰'),
			_Utils_Tuple2('こうどく', '購読'),
			_Utils_Tuple2('こうないえん', '口内炎'),
			_Utils_Tuple2('こうにんかいけいし', '公認会計士'),
			_Utils_Tuple2('こうはい', '荒廃'),
			_Utils_Tuple2('こうはい', '交配'),
			_Utils_Tuple2('こうばい', '購買'),
			_Utils_Tuple2('こうばいりょく', '購買力'),
			_Utils_Tuple2('こうばしい', '香ばしい'),
			_Utils_Tuple2('こうはん', '公判'),
			_Utils_Tuple2('こうはん', '広範'),
			_Utils_Tuple2('こうばん', '降板'),
			_Utils_Tuple2('こうひ', '工費'),
			_Utils_Tuple2('こうふ', '交付'),
			_Utils_Tuple2('こうふ', '公布'),
			_Utils_Tuple2('こうふく', '降伏'),
			_Utils_Tuple2('こうぶつ', '鉱物'),
			_Utils_Tuple2('こうぼ', '酵母'),
			_Utils_Tuple2('こうぼ', '公募'),
			_Utils_Tuple2('こうほう', '広報'),
			_Utils_Tuple2('こうぼう', '工房'),
			_Utils_Tuple2('ごうほう', '合法'),
			_Utils_Tuple2('ごうほうてき', '合法的'),
			_Utils_Tuple2('こうまん', '高慢'),
			_Utils_Tuple2('ごうまん', 'ごう慢'),
			_Utils_Tuple2('こうみょう', '巧妙'),
			_Utils_Tuple2('こうみん', '公民'),
			_Utils_Tuple2('こうむる', 'こうむる'),
			_Utils_Tuple2('こうもり', 'こうもり'),
			_Utils_Tuple2('こうもん', '肛門'),
			_Utils_Tuple2('こうやく', '公約'),
			_Utils_Tuple2('こうら', '甲羅'),
			_Utils_Tuple2('こうらく', '行楽'),
			_Utils_Tuple2('こうり', '小売り'),
			_Utils_Tuple2('ごうり', '合理'),
			_Utils_Tuple2('ごうりしゅぎ', '合理主義'),
			_Utils_Tuple2('ごうりてき', '合理的'),
			_Utils_Tuple2('こうりゃく', '攻略'),
			_Utils_Tuple2('こうりょ', '考慮'),
			_Utils_Tuple2('こうりょう', '香料'),
			_Utils_Tuple2('こうりょく', '効力'),
			_Utils_Tuple2('こうりん', '後輪'),
			_Utils_Tuple2('こうれい', '恒例'),
			_Utils_Tuple2('ごうれい', '号令'),
			_Utils_Tuple2('こうろ', '航路'),
			_Utils_Tuple2('こうろう', '功労'),
			_Utils_Tuple2('こうろん', '口論'),
			_Utils_Tuple2('ごえい', '護衛'),
			_Utils_Tuple2('こえがわり', '声変わり'),
			_Utils_Tuple2('こえだ', '小枝'),
			_Utils_Tuple2('こえる', '肥える'),
			_Utils_Tuple2('ごーじゃす', 'ゴージャス'),
			_Utils_Tuple2('こーてぃんぐ', 'コーティング'),
			_Utils_Tuple2('こーなーきっく', 'コーナーキック'),
			_Utils_Tuple2('ごかく', '互角'),
			_Utils_Tuple2('こかげ', '木陰'),
			_Utils_Tuple2('ごがつにんぎょう', '五月人形'),
			_Utils_Tuple2('こがらし', '木枯らし'),
			_Utils_Tuple2('こかん', '股間'),
			_Utils_Tuple2('ごかん', '五感'),
			_Utils_Tuple2('ごきげん', '御機嫌'),
			_Utils_Tuple2('こきざみ', '小刻み'),
			_Utils_Tuple2('こきゃく', '顧客'),
			_Utils_Tuple2('こきゅうき', '呼吸器'),
			_Utils_Tuple2('こきゅうこんなん', '呼吸困難'),
			_Utils_Tuple2('こく', '刻'),
			_Utils_Tuple2('こく', 'こく'),
			_Utils_Tuple2('こく', '酷'),
			_Utils_Tuple2('こく', '石'),
			_Utils_Tuple2('ごくい', '極意'),
			_Utils_Tuple2('こくいん', '刻印'),
			_Utils_Tuple2('こくさい', '国債'),
			_Utils_Tuple2('こくさいじょうやく', '国際条約'),
			_Utils_Tuple2('こくさいしょく', '国際色'),
			_Utils_Tuple2('こくさいほう', '国際法'),
			_Utils_Tuple2('こくし', '酷使'),
			_Utils_Tuple2('こくせい', '国勢'),
			_Utils_Tuple2('こくぜい', '国税'),
			_Utils_Tuple2('こくぜいちょう', '国税庁'),
			_Utils_Tuple2('こくせいちょうさ', '国勢調査'),
			_Utils_Tuple2('こくそ', '告訴'),
			_Utils_Tuple2('こくち', '告知'),
			_Utils_Tuple2('こくどう', '国道'),
			_Utils_Tuple2('こくはつ', '告発'),
			_Utils_Tuple2('ごくひ', '極秘'),
			_Utils_Tuple2('こくべつ', '告別'),
			_Utils_Tuple2('こくべつしき', '告別式'),
			_Utils_Tuple2('こくぼう', '国防'),
			_Utils_Tuple2('こくみんしゅけん', '国民主権'),
			_Utils_Tuple2('こくみんそうせいさん', '国民総生産'),
			_Utils_Tuple2('こくみんとうひょう', '国民投票'),
			_Utils_Tuple2('こくみんねんきん', '国民年金'),
			_Utils_Tuple2('ごくらく', '極楽'),
			_Utils_Tuple2('こけ', 'こけ'),
			_Utils_Tuple2('こげ', '焦げ'),
			_Utils_Tuple2('こけい', '固形'),
			_Utils_Tuple2('こけし', 'こけし'),
			_Utils_Tuple2('こげつく', '焦げ付く'),
			_Utils_Tuple2('こげめ', '焦げ目'),
			_Utils_Tuple2('ごげん', '語源'),
			_Utils_Tuple2('ここち', '心地'),
			_Utils_Tuple2('ここちよい', '心地よい'),
			_Utils_Tuple2('こごと', '小言'),
			_Utils_Tuple2('こころあたり', '心当たり'),
			_Utils_Tuple2('こころえ', '心得'),
			_Utils_Tuple2('こころえる', '心得る'),
			_Utils_Tuple2('こころがける', '心掛ける'),
			_Utils_Tuple2('こころがまえ', '心構え'),
			_Utils_Tuple2('こころくばり', '心配り'),
			_Utils_Tuple2('こころざし', '志'),
			_Utils_Tuple2('こころざす', '志す'),
			_Utils_Tuple2('こころづかい', '心遣い'),
			_Utils_Tuple2('こころづよい', '心強い'),
			_Utils_Tuple2('こころない', '心無い'),
			_Utils_Tuple2('こころまち', '心待ち'),
			_Utils_Tuple2('こころみ', '試み'),
			_Utils_Tuple2('こころみに', '試みに'),
			_Utils_Tuple2('こころもち', '心持ち'),
			_Utils_Tuple2('こころよい', '快い'),
			_Utils_Tuple2('ここん', '古今'),
			_Utils_Tuple2('ここんとうざい', '古今東西'),
			_Utils_Tuple2('ごさ', '誤差'),
			_Utils_Tuple2('ごさい', '後妻'),
			_Utils_Tuple2('こさめ', '小雨'),
			_Utils_Tuple2('こし', '故紙'),
			_Utils_Tuple2('こじ', '孤児'),
			_Utils_Tuple2('こじ', '誇示'),
			_Utils_Tuple2('こじいん', '孤児院'),
			_Utils_Tuple2('こしかけ', '腰掛け'),
			_Utils_Tuple2('こしかける', '腰掛ける'),
			_Utils_Tuple2('こじき', 'こじき'),
			_Utils_Tuple2('ごしごし', 'ごしごし'),
			_Utils_Tuple2('こしつ', '固執'),
			_Utils_Tuple2('ごしっぷ', 'ゴシップ'),
			_Utils_Tuple2('こしぬけ', '腰抜け'),
			_Utils_Tuple2('ごしゅうしょうさま', '御愁傷様'),
			_Utils_Tuple2('ごじゅん', '語順'),
			_Utils_Tuple2('こしょう', '呼称'),
			_Utils_Tuple2('こじれる', 'こじれる'),
			_Utils_Tuple2('こじん', '故人'),
			_Utils_Tuple2('こす', 'こす'),
			_Utils_Tuple2('こすれる', '擦れる'),
			_Utils_Tuple2('こせき', '戸籍'),
			_Utils_Tuple2('こせきとうほん', '戸籍謄本'),
			_Utils_Tuple2('こぞう', '小僧'),
			_Utils_Tuple2('こそく', 'こそく'),
			_Utils_Tuple2('ごそごそ', 'ごそごそ'),
			_Utils_Tuple2('こだい', '誇大'),
			_Utils_Tuple2('こだかい', '小高い'),
			_Utils_Tuple2('ごたごた', 'ごたごた'),
			_Utils_Tuple2('こだち', '木立ち'),
			_Utils_Tuple2('こだま', '木霊'),
			_Utils_Tuple2('こだわり', 'こだわり'),
			_Utils_Tuple2('こだわる', 'こだわる'),
			_Utils_Tuple2('こつ', '骨'),
			_Utils_Tuple2('こつ', '骨'),
			_Utils_Tuple2('ごつい', 'ごつい'),
			_Utils_Tuple2('こっかく', '骨格'),
			_Utils_Tuple2('こっかけんりょく', '国家権力'),
			_Utils_Tuple2('こっかこうむいん', '国家公務員'),
			_Utils_Tuple2('こっくぴっと', 'コックピット'),
			_Utils_Tuple2('こっけい', 'こっけい'),
			_Utils_Tuple2('ごっこ', 'ごっこ'),
			_Utils_Tuple2('こっこう', '国交'),
			_Utils_Tuple2('こっこく', '刻々'),
			_Utils_Tuple2('ごつごつ', 'ごつごつ'),
			_Utils_Tuple2('ごったがえす', 'ごった返す'),
			_Utils_Tuple2('ごっちゃ', 'ごっちゃ'),
			_Utils_Tuple2('こってり', 'こってり'),
			_Utils_Tuple2('ごっどふァーざー', 'ゴッドファーザー'),
			_Utils_Tuple2('こづれ', '子連れ'),
			_Utils_Tuple2('こていかんねん', '固定観念'),
			_Utils_Tuple2('こていしさん', '固定資産'),
			_Utils_Tuple2('こていしさんぜい', '固定資産税'),
			_Utils_Tuple2('こてん', '古典'),
			_Utils_Tuple2('ごてん', '御殿'),
			_Utils_Tuple2('こてんてき', '古典的'),
			_Utils_Tuple2('こと', '異'),
			_Utils_Tuple2('こと', '琴'),
			_Utils_Tuple2('こどう', '鼓動'),
			_Utils_Tuple2('ことかく', '事欠く'),
			_Utils_Tuple2('ことこと', 'ことこと'),
			_Utils_Tuple2('ことごと', '事事'),
			_Utils_Tuple2('ことごとく', 'ことごとく'),
			_Utils_Tuple2('ことさら', '殊更'),
			_Utils_Tuple2('ことたりる', '事足りる'),
			_Utils_Tuple2('ことぶき', '寿'),
			_Utils_Tuple2('こなす', 'こなす'),
			_Utils_Tuple2('こねる', 'こねる'),
			_Utils_Tuple2('このましい', '好ましい'),
			_Utils_Tuple2('このみ', '木の実'),
			_Utils_Tuple2('こばか', '小馬鹿'),
			_Utils_Tuple2('こばしり', '小走り'),
			_Utils_Tuple2('こばむ', '拒む'),
			_Utils_Tuple2('こばると', 'コバルト'),
			_Utils_Tuple2('こばん', '小判'),
			_Utils_Tuple2('ごばん', '碁盤'),
			_Utils_Tuple2('ごび', '語尾'),
			_Utils_Tuple2('こびりつく', 'こびり付く'),
			_Utils_Tuple2('こびる', 'こびる'),
			_Utils_Tuple2('こぶ', 'こぶ'),
			_Utils_Tuple2('ごふく', '呉服'),
			_Utils_Tuple2('こぶし', 'こぶし'),
			_Utils_Tuple2('こぶとり', '小太り'),
			_Utils_Tuple2('こぶら', 'コブラ'),
			_Utils_Tuple2('こぶり', '小振り'),
			_Utils_Tuple2('こふん', '古墳'),
			_Utils_Tuple2('こぶん', '子分'),
			_Utils_Tuple2('ごほうび', 'ご褒美'),
			_Utils_Tuple2('こま', 'こま'),
			_Utils_Tuple2('ごまあえ', 'ごまあえ'),
			_Utils_Tuple2('こまか', '細か'),
			_Utils_Tuple2('こまぎれ', '細切れ'),
			_Utils_Tuple2('こまく', '鼓膜'),
			_Utils_Tuple2('こまやか', '細やか'),
			_Utils_Tuple2('こまわり', '小回り'),
			_Utils_Tuple2('こみあげる', '込み上げる'),
			_Utils_Tuple2('こみみ', '小耳'),
			_Utils_Tuple2('こめかみ', 'こめかみ'),
			_Utils_Tuple2('こめつぶ', '米粒'),
			_Utils_Tuple2('こめんてーたー', 'コメンテーター'),
			_Utils_Tuple2('ごもく', '五目'),
			_Utils_Tuple2('こもち', '子持ち'),
			_Utils_Tuple2('こもる', '籠もる'),
			_Utils_Tuple2('こもん', '顧問'),
			_Utils_Tuple2('こや', '小屋'),
			_Utils_Tuple2('こやく', '子役'),
			_Utils_Tuple2('こやす', '肥やす'),
			_Utils_Tuple2('こゆう', '固有'),
			_Utils_Tuple2('こゆき', '小雪'),
			_Utils_Tuple2('ごよう', '御用'),
			_Utils_Tuple2('こようしゃ', '雇用者'),
			_Utils_Tuple2('こよみ', '暦'),
			_Utils_Tuple2('こらい', '古来'),
			_Utils_Tuple2('こらえる', 'こらえる'),
			_Utils_Tuple2('こらしめる', '懲らしめる'),
			_Utils_Tuple2('こらす', 'こらす'),
			_Utils_Tuple2('こり', '凝り'),
			_Utils_Tuple2('こりー', 'コリー'),
			_Utils_Tuple2('こりかたまる', '凝り固まる'),
			_Utils_Tuple2('こりる', '懲りる'),
			_Utils_Tuple2('ごりん', '五輪'),
			_Utils_Tuple2('こる', '凝る'),
			_Utils_Tuple2('これみよがし', 'これみよがし'),
			_Utils_Tuple2('これら', 'コレラ'),
			_Utils_Tuple2('ごろ', 'ゴロ'),
			_Utils_Tuple2('ごろ', '語呂'),
			_Utils_Tuple2('ころがりこむ', '転がり込む'),
			_Utils_Tuple2('ごろく', '語録'),
			_Utils_Tuple2('ころしや', '殺し屋'),
			_Utils_Tuple2('ころも', '衣'),
			_Utils_Tuple2('こわけ', '小分け'),
			_Utils_Tuple2('ごわごわ', 'ごわごわ'),
			_Utils_Tuple2('こわだか', '声高'),
			_Utils_Tuple2('こんいん', '婚姻'),
			_Utils_Tuple2('こんがり', 'こんがり'),
			_Utils_Tuple2('こんがん', '懇願'),
			_Utils_Tuple2('こんき', '根気'),
			_Utils_Tuple2('こんきょ', '根拠'),
			_Utils_Tuple2('こんげん', '根源'),
			_Utils_Tuple2('こんこん', 'こんこん'),
			_Utils_Tuple2('こんざい', '混在'),
			_Utils_Tuple2('こんじき', '金色'),
			_Utils_Tuple2('こんじゃく', '今昔'),
			_Utils_Tuple2('こんじょう', '根性'),
			_Utils_Tuple2('こんぜつ', '根絶'),
			_Utils_Tuple2('こんせぷと', 'コンセプト'),
			_Utils_Tuple2('こんたん', '魂胆'),
			_Utils_Tuple2('こんだん', '懇談'),
			_Utils_Tuple2('こんちゅう', '昆虫'),
			_Utils_Tuple2('こんてい', '根底'),
			_Utils_Tuple2('こんてな', 'コンテナ'),
			_Utils_Tuple2('こんでんすみるく', 'コンデンスミルク'),
			_Utils_Tuple2('こんどーむ', 'コンドーム'),
			_Utils_Tuple2('こんとらすと', 'コントラスト'),
			_Utils_Tuple2('こんとろーらー', 'コントローラー'),
			_Utils_Tuple2('こんにゅう', '混入'),
			_Utils_Tuple2('こんぱす', 'コンパス'),
			_Utils_Tuple2('こんびなーと', 'コンビナート'),
			_Utils_Tuple2('こんぶ', '昆布'),
			_Utils_Tuple2('こんぷれっくす', 'コンプレックス'),
			_Utils_Tuple2('こんべやー', 'コンベヤー'),
			_Utils_Tuple2('こんぽ', 'コンポ'),
			_Utils_Tuple2('こんぽう', '梱包'),
			_Utils_Tuple2('こんもり', 'こんもり'),
			_Utils_Tuple2('こんよく', '混浴'),
			_Utils_Tuple2('こんわく', '困惑'),
			_Utils_Tuple2('さ', 'さ'),
			_Utils_Tuple2('ざ', '座'),
			_Utils_Tuple2('ざ', '座'),
			_Utils_Tuple2('さーきっと', 'サーキット'),
			_Utils_Tuple2('さーど', 'サード'),
			_Utils_Tuple2('さーばー', 'サーバー'),
			_Utils_Tuple2('さーぶ', 'サーブ'),
			_Utils_Tuple2('さい', '差異'),
			_Utils_Tuple2('ざい', '財'),
			_Utils_Tuple2('ざい', '財'),
			_Utils_Tuple2('ざい', '材'),
			_Utils_Tuple2('ざい', '罪'),
			_Utils_Tuple2('ざいあく', '罪悪'),
			_Utils_Tuple2('さいえん', '菜園'),
			_Utils_Tuple2('ざいかい', '財界'),
			_Utils_Tuple2('さいかいはつ', '再開発'),
			_Utils_Tuple2('さいかく', '才覚'),
			_Utils_Tuple2('さいき', '再起'),
			_Utils_Tuple2('さいきん', '細菌'),
			_Utils_Tuple2('さいく', '細工'),
			_Utils_Tuple2('さいけつ', '採血'),
			_Utils_Tuple2('さいげつ', '歳月'),
			_Utils_Tuple2('さいげん', '際限'),
			_Utils_Tuple2('ざいげん', '財源'),
			_Utils_Tuple2('ざいこ', '在庫'),
			_Utils_Tuple2('さいこうさい', '最高裁'),
			_Utils_Tuple2('さいこうさいばんしょ', '最高裁判所'),
			_Utils_Tuple2('さいこうび', '最後尾'),
			_Utils_Tuple2('さいさん', '採算'),
			_Utils_Tuple2('さいしゅ', '採取'),
			_Utils_Tuple2('さいしょうげんど', '最小限度'),
			_Utils_Tuple2('ざいせい', '財政'),
			_Utils_Tuple2('さいせいき', '最盛期'),
			_Utils_Tuple2('さいせん', '再選'),
			_Utils_Tuple2('さいぜん', '最善'),
			_Utils_Tuple2('さいたく', '採択'),
			_Utils_Tuple2('ざいたく', '在宅'),
			_Utils_Tuple2('さいだん', '裁断'),
			_Utils_Tuple2('さいだん', '祭壇'),
			_Utils_Tuple2('ざいだん', '財団'),
			_Utils_Tuple2('ざいちゅう', '在中'),
			_Utils_Tuple2('さいてん', '祭典'),
			_Utils_Tuple2('さいどかー', 'サイドカー'),
			_Utils_Tuple2('さいどぶれーき', 'サイドブレーキ'),
			_Utils_Tuple2('さいなん', '災難'),
			_Utils_Tuple2('さいにゅう', '歳入'),
			_Utils_Tuple2('ざいにん', '罪人'),
			_Utils_Tuple2('さいにんしき', '再認識'),
			_Utils_Tuple2('さいばい', '栽培'),
			_Utils_Tuple2('ざいばつ', '財閥'),
			_Utils_Tuple2('さいばん', '裁判'),
			_Utils_Tuple2('さいばんかん', '裁判官'),
			_Utils_Tuple2('さいばんしょ', '裁判所'),
			_Utils_Tuple2('さいぶ', '細部'),
			_Utils_Tuple2('さいぶん', '細分'),
			_Utils_Tuple2('さいぶんか', '細分化'),
			_Utils_Tuple2('さいほう', '裁縫'),
			_Utils_Tuple2('さいぼう', '細胞'),
			_Utils_Tuple2('さいまつ', '歳末'),
			_Utils_Tuple2('さいみん', '催眠'),
			_Utils_Tuple2('さいむ', '債務'),
			_Utils_Tuple2('ざいむ', '財務'),
			_Utils_Tuple2('さいわい', '幸い'),
			_Utils_Tuple2('さいわい', '幸い'),
			_Utils_Tuple2('さえぎる', '遮る'),
			_Utils_Tuple2('さえる', 'さえる'),
			_Utils_Tuple2('さお', '竿'),
			_Utils_Tuple2('さかえる', '栄える'),
			_Utils_Tuple2('さがく', '差額'),
			_Utils_Tuple2('さかさま', '逆様'),
			_Utils_Tuple2('さがしあてる', '捜し当てる'),
			_Utils_Tuple2('さかずき', '杯'),
			_Utils_Tuple2('さかのぼる', 'さかのぼる'),
			_Utils_Tuple2('さぎ', '詐欺'),
			_Utils_Tuple2('さきおくり', '先送り'),
			_Utils_Tuple2('さきがけ', '先駆け'),
			_Utils_Tuple2('さきがける', '先駆ける'),
			_Utils_Tuple2('さきごろ', '先ごろ'),
			_Utils_Tuple2('さきざき', '先々'),
			_Utils_Tuple2('さきだつ', '先立つ'),
			_Utils_Tuple2('さきみだれる', '咲き乱れる'),
			_Utils_Tuple2('さきゆき', '先行き'),
			_Utils_Tuple2('さく', '策'),
			_Utils_Tuple2('さく', 'さく'),
			_Utils_Tuple2('さくげん', '削減'),
			_Utils_Tuple2('さくさく', 'さくさく'),
			_Utils_Tuple2('さくらぜんせん', '桜前線'),
			_Utils_Tuple2('さぐり', '探り'),
			_Utils_Tuple2('さくりゃく', '策略'),
			_Utils_Tuple2('さける', '裂ける'),
			_Utils_Tuple2('ざこ', 'ざこ'),
			_Utils_Tuple2('さこく', '鎖国'),
			_Utils_Tuple2('ささい', 'ささい'),
			_Utils_Tuple2('ささげる', 'ささげる'),
			_Utils_Tuple2('ささめごと', 'ささめごと'),
			_Utils_Tuple2('ささやか', 'ささやか'),
			_Utils_Tuple2('ささやき', 'ささやき'),
			_Utils_Tuple2('ささやく', 'ささやく'),
			_Utils_Tuple2('さしいれ', '差し入れ'),
			_Utils_Tuple2('さしいれる', '差し入れる'),
			_Utils_Tuple2('さしおさえ', '差し押さえ'),
			_Utils_Tuple2('さしおさえる', '差し押さえる'),
			_Utils_Tuple2('さしかえる', '差し替える'),
			_Utils_Tuple2('ざしき', '座敷'),
			_Utils_Tuple2('さしこみ', '差し込み'),
			_Utils_Tuple2('さしこむ', '差し込む'),
			_Utils_Tuple2('さしだす', '差し出す'),
			_Utils_Tuple2('さしつかえ', '差し支え'),
			_Utils_Tuple2('さしつかえる', '差し支える'),
			_Utils_Tuple2('さしとめる', '差し止める'),
			_Utils_Tuple2('さしのべる', '差し伸べる'),
			_Utils_Tuple2('さしば', '差し歯'),
			_Utils_Tuple2('さしひき', '差し引き'),
			_Utils_Tuple2('さしひく', '差し引く'),
			_Utils_Tuple2('ざしょう', '座礁'),
			_Utils_Tuple2('さす', '挿す'),
			_Utils_Tuple2('さずかる', '授かる'),
			_Utils_Tuple2('さずける', '授ける'),
			_Utils_Tuple2('させん', '左遷'),
			_Utils_Tuple2('ざぜん', '座禅'),
			_Utils_Tuple2('さそいこむ', '誘い込む'),
			_Utils_Tuple2('さだか', '定か'),
			_Utils_Tuple2('さだまる', '定まる'),
			_Utils_Tuple2('さだめる', '定める'),
			_Utils_Tuple2('ざだん', '座談'),
			_Utils_Tuple2('ざだんかい', '座談会'),
			_Utils_Tuple2('ざちょう', '座長'),
			_Utils_Tuple2('さっかく', '錯覚'),
			_Utils_Tuple2('ざつがく', '雑学'),
			_Utils_Tuple2('さつき', '五月'),
			_Utils_Tuple2('さっきゅう', '早急'),
			_Utils_Tuple2('さっきん', '殺菌'),
			_Utils_Tuple2('ざっきん', '雑菌'),
			_Utils_Tuple2('さっくす', 'サックス'),
			_Utils_Tuple2('ざっくばらん', 'ざっくばらん'),
			_Utils_Tuple2('ざっくり', 'ざっくり'),
			_Utils_Tuple2('さっこん', '昨今'),
			_Utils_Tuple2('さっし', 'サッシ'),
			_Utils_Tuple2('さっし', '察し'),
			_Utils_Tuple2('さっし', '冊子'),
			_Utils_Tuple2('さっしょう', '殺傷'),
			_Utils_Tuple2('さつじんき', '殺人鬼'),
			_Utils_Tuple2('さっする', '察する'),
			_Utils_Tuple2('ざっそう', '雑草'),
			_Utils_Tuple2('ざった', '雑多'),
			_Utils_Tuple2('さっち', '察知'),
			_Utils_Tuple2('さっちゅう', '殺虫'),
			_Utils_Tuple2('さっちゅうざい', '殺虫剤'),
			_Utils_Tuple2('さっとう', '殺到'),
			_Utils_Tuple2('ざつよう', '雑用'),
			_Utils_Tuple2('さてい', '査定'),
			_Utils_Tuple2('さては', 'さては'),
			_Utils_Tuple2('さと', '里'),
			_Utils_Tuple2('さとおや', '里親'),
			_Utils_Tuple2('さとがえり', '里帰り'),
			_Utils_Tuple2('さとす', '諭す'),
			_Utils_Tuple2('さとり', '悟り'),
			_Utils_Tuple2('さとる', '悟る'),
			_Utils_Tuple2('さばいばる', 'サバイバル'),
			_Utils_Tuple2('さばく', '裁く'),
			_Utils_Tuple2('さび', 'さび'),
			_Utils_Tuple2('ざひょう', '座標'),
			_Utils_Tuple2('さびる', 'さびる'),
			_Utils_Tuple2('さぶ', 'サブ'),
			_Utils_Tuple2('さふァいあ', 'サファイア'),
			_Utils_Tuple2('さほう', '作法'),
			_Utils_Tuple2('さぼてん', 'サボテン'),
			_Utils_Tuple2('さほど', 'さほど'),
			_Utils_Tuple2('さま', '様'),
			_Utils_Tuple2('さまがわり', '様変わり'),
			_Utils_Tuple2('さみだれ', 'さみだれ'),
			_Utils_Tuple2('さむざむ', '寒々'),
			_Utils_Tuple2('さも', '然も'),
			_Utils_Tuple2('ざゆうのめい', '座右の銘'),
			_Utils_Tuple2('さよう', '作用'),
			_Utils_Tuple2('さよく', '左翼'),
			_Utils_Tuple2('ざら', 'ざら'),
			_Utils_Tuple2('さらう', 'さらう'),
			_Utils_Tuple2('さらきん', 'サラ金'),
			_Utils_Tuple2('さらさら', 'さらさら'),
			_Utils_Tuple2('さらさら', 'さらさら'),
			_Utils_Tuple2('ざらざら', 'ざらざら'),
			_Utils_Tuple2('さらす', 'さらす'),
			_Utils_Tuple2('さらば', 'さらば'),
			_Utils_Tuple2('さりー', 'サリー'),
			_Utils_Tuple2('さりげない', 'さりげない'),
			_Utils_Tuple2('さわ', '沢'),
			_Utils_Tuple2('ざわめく', 'ざわめく'),
			_Utils_Tuple2('さわる', '障る'),
			_Utils_Tuple2('さわん', '左腕'),
			_Utils_Tuple2('さん', '酸'),
			_Utils_Tuple2('さん', '酸'),
			_Utils_Tuple2('ざん', '残'),
			_Utils_Tuple2('さんか', '酸化'),
			_Utils_Tuple2('さんかいき', '三回忌'),
			_Utils_Tuple2('さんがく', '山岳'),
			_Utils_Tuple2('ざんがく', '残額'),
			_Utils_Tuple2('さんぎいん', '参議院'),
			_Utils_Tuple2('さんぎいんぎいん', '参議院議員'),
			_Utils_Tuple2('さんきゃく', '三脚'),
			_Utils_Tuple2('ざんぎゃく', '残虐'),
			_Utils_Tuple2('さんきゅう', '産休'),
			_Utils_Tuple2('さんぎょうはいきぶつ', '産業廃棄物'),
			_Utils_Tuple2('さんけつ', '酸欠'),
			_Utils_Tuple2('さんけんぶんりつ', '三権分立'),
			_Utils_Tuple2('さんご', '産後'),
			_Utils_Tuple2('さんご', 'さんご'),
			_Utils_Tuple2('さんご', '三五'),
			_Utils_Tuple2('ざんこく', '残酷'),
			_Utils_Tuple2('さんさい', '山菜'),
			_Utils_Tuple2('さんさく', '散策'),
			_Utils_Tuple2('さんざん', '散々'),
			_Utils_Tuple2('さんざん', '散々'),
			_Utils_Tuple2('さんじゅう', '三重'),
			_Utils_Tuple2('さんしゅつ', '産出'),
			_Utils_Tuple2('さんしん', '三振'),
			_Utils_Tuple2('ざんしん', '斬新'),
			_Utils_Tuple2('さんせい', '酸性'),
			_Utils_Tuple2('さんせいう', '酸性雨'),
			_Utils_Tuple2('さんせいけん', '参政権'),
			_Utils_Tuple2('さんせん', '参戦'),
			_Utils_Tuple2('さんそう', '山荘'),
			_Utils_Tuple2('ざんぞう', '残像'),
			_Utils_Tuple2('ざんぞん', '残存'),
			_Utils_Tuple2('さんどぺーぱー', 'サンドペーパー'),
			_Utils_Tuple2('さんとら', 'サントラ'),
			_Utils_Tuple2('さんにゅう', '参入'),
			_Utils_Tuple2('さんぱい', '参拝'),
			_Utils_Tuple2('さんぱつ', '散髪'),
			_Utils_Tuple2('ざんぱん', '残飯'),
			_Utils_Tuple2('さんび', '賛美'),
			_Utils_Tuple2('さんぴ', '賛否'),
			_Utils_Tuple2('さんぷ', '散布'),
			_Utils_Tuple2('さんぷりんぐ', 'サンプリング'),
			_Utils_Tuple2('さんまいめ', '三枚目'),
			_Utils_Tuple2('さんみ', '酸味'),
			_Utils_Tuple2('さんみゃく', '山脈'),
			_Utils_Tuple2('さんめん', '三面'),
			_Utils_Tuple2('さんらん', '産卵'),
			_Utils_Tuple2('さんりゅう', '三流'),
			_Utils_Tuple2('ざんりゅう', '残留'),
			_Utils_Tuple2('さんるい', '三塁'),
			_Utils_Tuple2('さんれつ', '参列'),
			_Utils_Tuple2('し', '士'),
			_Utils_Tuple2('し', '志'),
			_Utils_Tuple2('し', '師'),
			_Utils_Tuple2('じ', '児'),
			_Utils_Tuple2('じ', 'じ'),
			_Utils_Tuple2('しあげ', '仕上げ'),
			_Utils_Tuple2('しあげる', '仕上げる'),
			_Utils_Tuple2('しあん', '思案'),
			_Utils_Tuple2('しいく', '飼育'),
			_Utils_Tuple2('しーくれっと', 'シークレット'),
			_Utils_Tuple2('しいて', '強いて'),
			_Utils_Tuple2('しーど', 'シード'),
			_Utils_Tuple2('しいる', '強いる'),
			_Utils_Tuple2('しいん', '死因'),
			_Utils_Tuple2('しうち', '仕打ち'),
			_Utils_Tuple2('じえい', '自衛'),
			_Utils_Tuple2('じぇっと', 'ジェット'),
			_Utils_Tuple2('しぇる', 'シェル'),
			_Utils_Tuple2('しぇるたー', 'シェルター'),
			_Utils_Tuple2('じぇんだー', 'ジェンダー'),
			_Utils_Tuple2('しお', '潮'),
			_Utils_Tuple2('しおかぜ', '潮風'),
			_Utils_Tuple2('しおから', '塩辛'),
			_Utils_Tuple2('しおけ', '塩気'),
			_Utils_Tuple2('しおざけ', '塩ざけ'),
			_Utils_Tuple2('しおづけ', '塩漬け'),
			_Utils_Tuple2('しおやき', '塩焼き'),
			_Utils_Tuple2('しか', '市価'),
			_Utils_Tuple2('じか', '時価'),
			_Utils_Tuple2('しかい', '視界'),
			_Utils_Tuple2('しがい', '市街'),
			_Utils_Tuple2('じがい', '自害'),
			_Utils_Tuple2('しかえし', '仕返し'),
			_Utils_Tuple2('しかく', '刺客'),
			_Utils_Tuple2('じかくしょうじょう', '自覚症状'),
			_Utils_Tuple2('しかけ', '仕掛け'),
			_Utils_Tuple2('しかける', '仕掛ける'),
			_Utils_Tuple2('しかと', 'しかと'),
			_Utils_Tuple2('じかに', 'じかに'),
			_Utils_Tuple2('しがみつく', 'しがみ付く'),
			_Utils_Tuple2('しがん', '志願'),
			_Utils_Tuple2('しき', '指揮'),
			_Utils_Tuple2('じき', '磁気'),
			_Utils_Tuple2('しきい', '敷居'),
			_Utils_Tuple2('しぎかい', '市議会'),
			_Utils_Tuple2('しきかく', '色覚'),
			_Utils_Tuple2('しきさい', '色彩'),
			_Utils_Tuple2('しきそ', '色素'),
			_Utils_Tuple2('しきたり', '仕来り'),
			_Utils_Tuple2('しきち', '敷地'),
			_Utils_Tuple2('しきちょう', '色調'),
			_Utils_Tuple2('じきてーぷ', '磁気テープ'),
			_Utils_Tuple2('しきてん', '式典'),
			_Utils_Tuple2('じきに', '直に'),
			_Utils_Tuple2('しきべつ', '識別'),
			_Utils_Tuple2('しきもう', '色盲'),
			_Utils_Tuple2('しきゅう', '子宮'),
			_Utils_Tuple2('しきゅう', '四球'),
			_Utils_Tuple2('しきゅう', '死球'),
			_Utils_Tuple2('じきゅう', '自給'),
			_Utils_Tuple2('じきゅう', '持久'),
			_Utils_Tuple2('じきゅうじそく', '自給自足'),
			_Utils_Tuple2('じきゅうりょく', '持久力'),
			_Utils_Tuple2('しぎょう', '始業'),
			_Utils_Tuple2('じぎょうか', '事業家'),
			_Utils_Tuple2('しきょく', '支局'),
			_Utils_Tuple2('しきり', '仕切り'),
			_Utils_Tuple2('しきる', '仕切る'),
			_Utils_Tuple2('じく', '軸'),
			_Utils_Tuple2('しぐさ', 'しぐさ'),
			_Utils_Tuple2('しくしく', 'しくしく'),
			_Utils_Tuple2('しくはっく', '四苦八苦'),
			_Utils_Tuple2('しぐれ', 'しぐれ'),
			_Utils_Tuple2('しげしげ', 'しげしげ'),
			_Utils_Tuple2('しけつ', '止血'),
			_Utils_Tuple2('じけつ', '自決'),
			_Utils_Tuple2('しげみ', '茂み'),
			_Utils_Tuple2('しける', 'しける'),
			_Utils_Tuple2('しげる', '茂る'),
			_Utils_Tuple2('じげん', '次元'),
			_Utils_Tuple2('しけんかん', '試験管'),
			_Utils_Tuple2('しご', '死語'),
			_Utils_Tuple2('しこう', '嗜好'),
			_Utils_Tuple2('しこう', '志向'),
			_Utils_Tuple2('しこう', '施行'),
			_Utils_Tuple2('しこう', '試行'),
			_Utils_Tuple2('じこう', '時効'),
			_Utils_Tuple2('じこう', '事項'),
			_Utils_Tuple2('しこうさくご', '試行錯誤'),
			_Utils_Tuple2('じごうじとく', '自業自得'),
			_Utils_Tuple2('しごき', 'しごき'),
			_Utils_Tuple2('しこたま', 'しこたま'),
			_Utils_Tuple2('しごとがら', '仕事柄'),
			_Utils_Tuple2('しこむ', '仕込む'),
			_Utils_Tuple2('しこり', '痼'),
			_Utils_Tuple2('しさ', '示唆'),
			_Utils_Tuple2('しさい', '司祭'),
			_Utils_Tuple2('しざい', '資材'),
			_Utils_Tuple2('じざい', '自在'),
			_Utils_Tuple2('しさく', '施策'),
			_Utils_Tuple2('しさく', '思索'),
			_Utils_Tuple2('しさつ', '視察'),
			_Utils_Tuple2('しさんか', '資産家'),
			_Utils_Tuple2('しし', 'しし'),
			_Utils_Tuple2('ししつ', '資質'),
			_Utils_Tuple2('ししゃ', '試写'),
			_Utils_Tuple2('ししゃごにゅう', '四捨五入'),
			_Utils_Tuple2('じしゅ', '自主'),
			_Utils_Tuple2('しじゅう', '始終'),
			_Utils_Tuple2('じしゅく', '自粛'),
			_Utils_Tuple2('ししょう', '師匠'),
			_Utils_Tuple2('しじょう', '試乗'),
			_Utils_Tuple2('しじょう', '至上'),
			_Utils_Tuple2('しじょう', '史上'),
			_Utils_Tuple2('ししん', '指針'),
			_Utils_Tuple2('じす', 'ＪＩＳ'),
			_Utils_Tuple2('しすう', '指数'),
			_Utils_Tuple2('しずく', '滴'),
			_Utils_Tuple2('しずけさ', '静けさ'),
			_Utils_Tuple2('しすたー', 'シスター'),
			_Utils_Tuple2('じする', '辞する'),
			_Utils_Tuple2('しせい', '市政'),
			_Utils_Tuple2('しせき', '史跡'),
			_Utils_Tuple2('しせつ', '私設'),
			_Utils_Tuple2('しせつ', '使節'),
			_Utils_Tuple2('じぜん', '慈善'),
			_Utils_Tuple2('しそうか', '思想家'),
			_Utils_Tuple2('じそく', '自足'),
			_Utils_Tuple2('じそんしん', '自尊心'),
			_Utils_Tuple2('したあじ', '下味'),
			_Utils_Tuple2('じたい', '辞退'),
			_Utils_Tuple2('じたい', '字体'),
			_Utils_Tuple2('じだいさくご', '時代錯誤'),
			_Utils_Tuple2('したう', '慕う'),
			_Utils_Tuple2('したうけ', '下請け'),
			_Utils_Tuple2('したうち', '舌打ち'),
			_Utils_Tuple2('したがえる', '従える'),
			_Utils_Tuple2('したじ', '下地'),
			_Utils_Tuple2('したじゅんび', '下準備'),
			_Utils_Tuple2('したしらべ', '下調べ'),
			_Utils_Tuple2('したたか', 'したたか'),
			_Utils_Tuple2('したづみ', '下積み'),
			_Utils_Tuple2('したてる', '仕立てる'),
			_Utils_Tuple2('したどり', '下取り'),
			_Utils_Tuple2('したみ', '下見'),
			_Utils_Tuple2('じちかい', '自治会'),
			_Utils_Tuple2('しちや', '質屋'),
			_Utils_Tuple2('しちゅう', '支柱'),
			_Utils_Tuple2('しちょう', '視聴'),
			_Utils_Tuple2('しちょうかく', '視聴覚'),
			_Utils_Tuple2('じつ', '実'),
			_Utils_Tuple2('じついん', '実印'),
			_Utils_Tuple2('じつえん', '実演'),
			_Utils_Tuple2('しっかん', '疾患'),
			_Utils_Tuple2('しつかん', '質感'),
			_Utils_Tuple2('しっき', '漆器'),
			_Utils_Tuple2('しつぎ', '質疑'),
			_Utils_Tuple2('じっきょう', '実況'),
			_Utils_Tuple2('じっきょうほうそう', '実況放送'),
			_Utils_Tuple2('しっく', 'シック'),
			_Utils_Tuple2('しっくり', 'しっくり'),
			_Utils_Tuple2('しつけ', 'しつけ'),
			_Utils_Tuple2('しつける', 'しつける'),
			_Utils_Tuple2('じっけん', '実権'),
			_Utils_Tuple2('じっけんだい', '実験台'),
			_Utils_Tuple2('しっこう', '執行'),
			_Utils_Tuple2('しっこうゆうよ', '執行猶予'),
			_Utils_Tuple2('じっしつ', '実質'),
			_Utils_Tuple2('じっしつてき', '実質的'),
			_Utils_Tuple2('じっしゃ', '実写'),
			_Utils_Tuple2('じっしょう', '実証'),
			_Utils_Tuple2('じつじょう', '実情'),
			_Utils_Tuple2('しっしん', 'しっしん'),
			_Utils_Tuple2('じっせん', '実戦'),
			_Utils_Tuple2('じっせん', '実践'),
			_Utils_Tuple2('じっせんてき', '実践的'),
			_Utils_Tuple2('しっそ', '質素'),
			_Utils_Tuple2('しっそう', '疾走'),
			_Utils_Tuple2('しっそう', '失そう'),
			_Utils_Tuple2('じつぞう', '実像'),
			_Utils_Tuple2('じつぞん', '実存'),
			_Utils_Tuple2('しったい', '失態'),
			_Utils_Tuple2('じったい', '実態'),
			_Utils_Tuple2('じったい', '実体'),
			_Utils_Tuple2('じっち', '実地'),
			_Utils_Tuple2('しつてき', '質的'),
			_Utils_Tuple2('しっと', 'しっと'),
			_Utils_Tuple2('しっとり', 'しっとり'),
			_Utils_Tuple2('じっぴ', '実費'),
			_Utils_Tuple2('しっぴつ', '執筆'),
			_Utils_Tuple2('しっぷ', '湿布'),
			_Utils_Tuple2('じっぷ', '実父'),
			_Utils_Tuple2('じつぼ', '実母'),
			_Utils_Tuple2('しつむ', '執務'),
			_Utils_Tuple2('じつむ', '実務'),
			_Utils_Tuple2('しつめい', '失明'),
			_Utils_Tuple2('しつよう', '執よう'),
			_Utils_Tuple2('してい', '師弟'),
			_Utils_Tuple2('しでかす', '仕出かす'),
			_Utils_Tuple2('してん', '支点'),
			_Utils_Tuple2('じどうせいぎょ', '自動制御'),
			_Utils_Tuple2('しどうようりょう', '指導要領'),
			_Utils_Tuple2('しとしと', 'しとしと'),
			_Utils_Tuple2('しなう', 'しなう'),
			_Utils_Tuple2('しなうす', '品薄'),
			_Utils_Tuple2('しなやか', 'しなやか'),
			_Utils_Tuple2('しなん', '至難'),
			_Utils_Tuple2('しにせ', 'しにせ'),
			_Utils_Tuple2('しにめ', '死に目'),
			_Utils_Tuple2('じぬし', '地主'),
			_Utils_Tuple2('しのうこうしょう', '士農工商'),
			_Utils_Tuple2('しのぐ', 'しのぐ'),
			_Utils_Tuple2('しのび', '忍び'),
			_Utils_Tuple2('しのびこむ', '忍び込む'),
			_Utils_Tuple2('しのびよる', '忍び寄る'),
			_Utils_Tuple2('じばいせき', '自賠責'),
			_Utils_Tuple2('しばかり', '芝刈り'),
			_Utils_Tuple2('じばく', '自爆'),
			_Utils_Tuple2('しばしば', 'しばしば'),
			_Utils_Tuple2('じはだ', '地肌'),
			_Utils_Tuple2('じはつ', '自発'),
			_Utils_Tuple2('じばら', '自腹'),
			_Utils_Tuple2('しばり', '縛り'),
			_Utils_Tuple2('しはん', '師範'),
			_Utils_Tuple2('じばん', '地盤'),
			_Utils_Tuple2('じひ', '慈悲'),
			_Utils_Tuple2('じびき', '字引'),
			_Utils_Tuple2('じひつ', '自筆'),
			_Utils_Tuple2('しひょう', '指標'),
			_Utils_Tuple2('じふ', '自負'),
			_Utils_Tuple2('しぶい', '渋い'),
			_Utils_Tuple2('しぶしぶ', '渋々'),
			_Utils_Tuple2('しぶる', '渋る'),
			_Utils_Tuple2('しへい', '紙幣'),
			_Utils_Tuple2('じべた', '地べた'),
			_Utils_Tuple2('しほう', '司法'),
			_Utils_Tuple2('じほう', '時報'),
			_Utils_Tuple2('しほうしけん', '司法試験'),
			_Utils_Tuple2('しほうしょし', '司法書士'),
			_Utils_Tuple2('しほうはっぽう', '四方八方'),
			_Utils_Tuple2('しぼり', '絞り'),
			_Utils_Tuple2('しぼりこむ', '絞り込む'),
			_Utils_Tuple2('しまい', '仕舞い'),
			_Utils_Tuple2('じまえ', '自前'),
			_Utils_Tuple2('しみこむ', '染み込む'),
			_Utils_Tuple2('じみち', '地道'),
			_Utils_Tuple2('しみゅれーしょん', 'シミュレーション'),
			_Utils_Tuple2('じみる', '染みる'),
			_Utils_Tuple2('しむける', '仕向ける'),
			_Utils_Tuple2('じめい', '自明'),
			_Utils_Tuple2('しめた', 'しめた'),
			_Utils_Tuple2('しめつ', '死滅'),
			_Utils_Tuple2('じめつ', '自滅'),
			_Utils_Tuple2('しめつける', '締め付ける'),
			_Utils_Tuple2('しめる', 'しめる'),
			_Utils_Tuple2('しも', '霜'),
			_Utils_Tuple2('じもと', '地元'),
			_Utils_Tuple2('しもばしら', '霜柱'),
			_Utils_Tuple2('じもん', '自問'),
			_Utils_Tuple2('じもんじとう', '自問自答'),
			_Utils_Tuple2('じゃ', '蛇'),
			_Utils_Tuple2('じゃーじー', 'ジャージー'),
			_Utils_Tuple2('しゃか', '釈迦'),
			_Utils_Tuple2('じゃがー', 'ジャガー'),
			_Utils_Tuple2('しゃかいふっき', '社会復帰'),
			_Utils_Tuple2('しゃかいほけん', '社会保険'),
			_Utils_Tuple2('しゃかいほしょう', '社会保障'),
			_Utils_Tuple2('しゃかんきょり', '車間距離'),
			_Utils_Tuple2('しゃきしゃき', 'しゃきしゃき'),
			_Utils_Tuple2('しゃくち', '借地'),
			_Utils_Tuple2('じゃぐち', '蛇口'),
			_Utils_Tuple2('しゃくど', '尺度'),
			_Utils_Tuple2('しゃくはち', '尺八'),
			_Utils_Tuple2('しゃくほう', '釈放'),
			_Utils_Tuple2('しゃくや', '借家'),
			_Utils_Tuple2('しゃくよう', '借用'),
			_Utils_Tuple2('しゃげき', '射撃'),
			_Utils_Tuple2('しゃけん', '車検'),
			_Utils_Tuple2('しゃこう', '社交'),
			_Utils_Tuple2('しゃこうじれい', '社交辞令'),
			_Utils_Tuple2('しゃこうだんす', '社交ダンス'),
			_Utils_Tuple2('しゃさつ', '射殺'),
			_Utils_Tuple2('じゃじゃうま', 'じゃじゃ馬'),
			_Utils_Tuple2('しゃしゅ', '車種'),
			_Utils_Tuple2('しゃじょう', '車上'),
			_Utils_Tuple2('しゃせい', '写生'),
			_Utils_Tuple2('しゃそう', '車窓'),
			_Utils_Tuple2('しゃたい', '車体'),
			_Utils_Tuple2('しゃだん', '遮断'),
			_Utils_Tuple2('しゃだんき', '遮断機'),
			_Utils_Tuple2('じゃっかん', '若干'),
			_Utils_Tuple2('しゃっとあうと', 'シャットアウト'),
			_Utils_Tuple2('じゃどう', '邪道'),
			_Utils_Tuple2('しゃどー', 'シャドー'),
			_Utils_Tuple2('しゃふつ', '煮沸'),
			_Utils_Tuple2('しゃぶる', 'しゃぶる'),
			_Utils_Tuple2('じゃらじゃら', 'じゃらじゃら'),
			_Utils_Tuple2('じゃり', '砂利'),
			_Utils_Tuple2('しゃれい', '謝礼'),
			_Utils_Tuple2('しゃれる', 'しゃれる'),
			_Utils_Tuple2('じゃんる', 'ジャンル'),
			_Utils_Tuple2('しゅ', '首'),
			_Utils_Tuple2('しゅ', '主'),
			_Utils_Tuple2('じゅ', '樹'),
			_Utils_Tuple2('しゅいろ', '朱色'),
			_Utils_Tuple2('しゅう', '集'),
			_Utils_Tuple2('じゅう', '重'),
			_Utils_Tuple2('じゅう', '獣'),
			_Utils_Tuple2('じゅう', '重'),
			_Utils_Tuple2('じゅうおう', '縦横'),
			_Utils_Tuple2('しゅうぎ', '祝儀'),
			_Utils_Tuple2('しゅうぎいん', '衆議院'),
			_Utils_Tuple2('しゅうぎいんぎいん', '衆議院議員'),
			_Utils_Tuple2('じゅうけいしょう', '重軽傷'),
			_Utils_Tuple2('しゅうげき', '襲撃'),
			_Utils_Tuple2('しゅうけつ', '終結'),
			_Utils_Tuple2('じゅうけつ', '充血'),
			_Utils_Tuple2('じゅうこう', '重厚'),
			_Utils_Tuple2('じゅうこうぎょう', '重工業'),
			_Utils_Tuple2('しゅうし', '終止'),
			_Utils_Tuple2('じゅうじ', '従事'),
			_Utils_Tuple2('しゅうしふ', '終止符'),
			_Utils_Tuple2('しゅうしゅく', '収縮'),
			_Utils_Tuple2('しゅうじゅく', '習熟'),
			_Utils_Tuple2('じゅうじゅん', '従順'),
			_Utils_Tuple2('しゅうしょく', '修飾'),
			_Utils_Tuple2('しゅうしん', '終身'),
			_Utils_Tuple2('しゅうしん', '就寝'),
			_Utils_Tuple2('しゅうじん', '囚人'),
			_Utils_Tuple2('じゅうしん', '重心'),
			_Utils_Tuple2('しゅうしんけい', '終身刑'),
			_Utils_Tuple2('しゅうしんこよう', '終身雇用'),
			_Utils_Tuple2('しゅうしんこようせい', '終身雇用制'),
			_Utils_Tuple2('じゅうぜい', '重税'),
			_Utils_Tuple2('しゅうぜん', '修繕'),
			_Utils_Tuple2('じゅうそく', '充足'),
			_Utils_Tuple2('じゅうぞく', '従属'),
			_Utils_Tuple2('じゅうだん', '銃弾'),
			_Utils_Tuple2('しゅうち', '周知'),
			_Utils_Tuple2('しゅうちしん', 'しゅう恥心'),
			_Utils_Tuple2('しゅうちゃく', '執着'),
			_Utils_Tuple2('しゅうと', 'しゅうと'),
			_Utils_Tuple2('しゅうとう', '周到'),
			_Utils_Tuple2('しゅうとめ', 'しゅうとめ'),
			_Utils_Tuple2('じゅうなんたいそう', '柔軟体操'),
			_Utils_Tuple2('じゅうにぶん', '十二分'),
			_Utils_Tuple2('しゅうは', '宗派'),
			_Utils_Tuple2('しゅうは', '周波'),
			_Utils_Tuple2('しゅうはい', '集配'),
			_Utils_Tuple2('しゅうはすう', '周波数'),
			_Utils_Tuple2('しゅうばん', '終盤'),
			_Utils_Tuple2('じゅうみんぜい', '住民税'),
			_Utils_Tuple2('しゅうやく', '集約'),
			_Utils_Tuple2('しゅうゆう', '周遊'),
			_Utils_Tuple2('しゅうよう', '収容'),
			_Utils_Tuple2('じゅうらい', '従来'),
			_Utils_Tuple2('しゅうらく', '集落'),
			_Utils_Tuple2('じゅうりょう', '十両'),
			_Utils_Tuple2('しゅえい', '守衛'),
			_Utils_Tuple2('じゅえき', '受益'),
			_Utils_Tuple2('しゅがん', '主眼'),
			_Utils_Tuple2('しゅぎょう', '修行'),
			_Utils_Tuple2('しゅぎょう', '修業'),
			_Utils_Tuple2('じゅきょう', '儒教'),
			_Utils_Tuple2('しゅくじ', '祝辞'),
			_Utils_Tuple2('しゅくしゃく', '縮尺'),
			_Utils_Tuple2('じゅくすい', '熟睡'),
			_Utils_Tuple2('じゅくする', '熟する'),
			_Utils_Tuple2('じゅくせい', '熟成'),
			_Utils_Tuple2('じゅくち', '熟知'),
			_Utils_Tuple2('じゅくどく', '熟読'),
			_Utils_Tuple2('じゅくねん', '熟年'),
			_Utils_Tuple2('しゅくめい', '宿命'),
			_Utils_Tuple2('じゅくれん', '熟練'),
			_Utils_Tuple2('しゅけん', '主権'),
			_Utils_Tuple2('しゅご', '守護'),
			_Utils_Tuple2('しゅこう', '趣向'),
			_Utils_Tuple2('しゅごう', '酒豪'),
			_Utils_Tuple2('しゅし', '趣旨'),
			_Utils_Tuple2('しゅし', '主旨'),
			_Utils_Tuple2('しゅし', '種子'),
			_Utils_Tuple2('じゅし', '樹脂'),
			_Utils_Tuple2('しゅじゅ', '種々'),
			_Utils_Tuple2('じゅじゅ', '授受'),
			_Utils_Tuple2('しゅしん', '主審'),
			_Utils_Tuple2('じゅしんき', '受信機'),
			_Utils_Tuple2('じゅず', '数珠'),
			_Utils_Tuple2('じゅせい', '受精'),
			_Utils_Tuple2('しゅせき', '主席'),
			_Utils_Tuple2('しゅぞく', '種族'),
			_Utils_Tuple2('しゅたい', '主体'),
			_Utils_Tuple2('しゅだいか', '主題歌'),
			_Utils_Tuple2('じゅだく', '受諾'),
			_Utils_Tuple2('じゅつ', '術'),
			_Utils_Tuple2('しゅっか', '出荷'),
			_Utils_Tuple2('しゅっけ', '出家'),
			_Utils_Tuple2('しゅっこう', '出港'),
			_Utils_Tuple2('しゅっこう', '出航'),
			_Utils_Tuple2('しゅっし', '出資'),
			_Utils_Tuple2('しゅっしょ', '出所'),
			_Utils_Tuple2('しゅっしょう', '出生'),
			_Utils_Tuple2('しゅっしょうりつ', '出生率'),
			_Utils_Tuple2('しゅってい', '出廷'),
			_Utils_Tuple2('しゅっとう', '出頭'),
			_Utils_Tuple2('しゅつば', '出馬'),
			_Utils_Tuple2('しゅっぴ', '出費'),
			_Utils_Tuple2('しゅっぴん', '出品'),
			_Utils_Tuple2('しゅつぼつ', '出没'),
			_Utils_Tuple2('しゅどう', '主導'),
			_Utils_Tuple2('しゅどうけん', '主導権'),
			_Utils_Tuple2('じゅどうてき', '受動的'),
			_Utils_Tuple2('しゅび', '守備'),
			_Utils_Tuple2('しゅひん', '主賓'),
			_Utils_Tuple2('しゅべつ', '種別'),
			_Utils_Tuple2('しゅほう', '手法'),
			_Utils_Tuple2('じゅもく', '樹木'),
			_Utils_Tuple2('じゅもん', 'じゅ文'),
			_Utils_Tuple2('じゅよ', '授与'),
			_Utils_Tuple2('じゅり', '受理'),
			_Utils_Tuple2('じゅりつ', '樹立'),
			_Utils_Tuple2('しゅりょう', '狩猟'),
			_Utils_Tuple2('じゅりょう', '受領'),
			_Utils_Tuple2('じゅれい', '樹齢'),
			_Utils_Tuple2('しゅわん', '手腕'),
			_Utils_Tuple2('しゅん', '旬'),
			_Utils_Tuple2('じゅん', '純'),
			_Utils_Tuple2('じゅん', '準'),
			_Utils_Tuple2('じゅんあい', '純愛'),
			_Utils_Tuple2('じゅんかい', '巡回'),
			_Utils_Tuple2('じゅんかつ', '潤滑'),
			_Utils_Tuple2('じゅんかつゆ', '潤滑油'),
			_Utils_Tuple2('しゅんぎく', '春菊'),
			_Utils_Tuple2('じゅんさ', '巡査'),
			_Utils_Tuple2('しゅんじ', '瞬時'),
			_Utils_Tuple2('じゅんじ', '順次'),
			_Utils_Tuple2('しゅんじゅう', '春秋'),
			_Utils_Tuple2('じゅんずる', '準ずる'),
			_Utils_Tuple2('じゅんせい', '純正'),
			_Utils_Tuple2('じゅんど', '純度'),
			_Utils_Tuple2('じゅんとう', '順当'),
			_Utils_Tuple2('じゅんのう', '順応'),
			_Utils_Tuple2('じゅんぱく', '純白'),
			_Utils_Tuple2('しゅんぱつりょく', '瞬発力'),
			_Utils_Tuple2('しょ', '書'),
			_Utils_Tuple2('しょ', '諸'),
			_Utils_Tuple2('しょいん', '書院'),
			_Utils_Tuple2('じょいんと', 'ジョイント'),
			_Utils_Tuple2('しょう', '商'),
			_Utils_Tuple2('しょう', 'しょう'),
			_Utils_Tuple2('しよう', '仕様'),
			_Utils_Tuple2('じょう', '錠'),
			_Utils_Tuple2('じょう', '錠'),
			_Utils_Tuple2('じょうか', '浄化'),
			_Utils_Tuple2('しょうかい', '照会'),
			_Utils_Tuple2('しょうがい', '生涯'),
			_Utils_Tuple2('しょうがいきょういく', '生涯教育'),
			_Utils_Tuple2('しょうがいちし', '傷害致死'),
			_Utils_Tuple2('しょうがいほけん', '傷害保険'),
			_Utils_Tuple2('しょうかく', '昇格'),
			_Utils_Tuple2('じょうかそう', '浄化槽'),
			_Utils_Tuple2('じょうきせん', '蒸気船'),
			_Utils_Tuple2('しょうきゃく', '焼却'),
			_Utils_Tuple2('しょうぐん', '将軍'),
			_Utils_Tuple2('しょうけいもじ', '象形文字'),
			_Utils_Tuple2('しょうげき', '衝撃'),
			_Utils_Tuple2('しょうけん', '証券'),
			_Utils_Tuple2('しょうこう', '焼香'),
			_Utils_Tuple2('しょうごう', '称号'),
			_Utils_Tuple2('じょうこう', '乗降'),
			_Utils_Tuple2('しょうこうぐん', '症候群'),
			_Utils_Tuple2('しょうさん', '勝算'),
			_Utils_Tuple2('しょうさん', '称賛'),
			_Utils_Tuple2('じょうしゅう', '常習'),
			_Utils_Tuple2('じょうしょうきりゅう', '上昇気流'),
			_Utils_Tuple2('しょうじる', '生じる'),
			_Utils_Tuple2('しょうしん', '昇進'),
			_Utils_Tuple2('じょうすい', '浄水'),
			_Utils_Tuple2('しょうする', '称する'),
			_Utils_Tuple2('しょうずる', '生ずる'),
			_Utils_Tuple2('じょうせい', '情勢'),
			_Utils_Tuple2('しょうそ', '勝訴'),
			_Utils_Tuple2('しょうぞう', '肖像'),
			_Utils_Tuple2('じょうそう', '上層'),
			_Utils_Tuple2('じょうぞう', '醸造'),
			_Utils_Tuple2('しょうぞうが', '肖像画'),
			_Utils_Tuple2('しょうそく', '消息'),
			_Utils_Tuple2('じょうたい', '上体'),
			_Utils_Tuple2('しょうだく', '承諾'),
			_Utils_Tuple2('じょうちゅう', '常駐'),
			_Utils_Tuple2('じょうちょ', '情緒'),
			_Utils_Tuple2('しょうちょう', '省庁'),
			_Utils_Tuple2('しょうちょうてき', '象徴的'),
			_Utils_Tuple2('じょうと', '譲渡'),
			_Utils_Tuple2('しょうとう', '消灯'),
			_Utils_Tuple2('しょうどう', '衝動'),
			_Utils_Tuple2('じょうにんりじこく', '常任理事国'),
			_Utils_Tuple2('じょうはつ', '蒸発'),
			_Utils_Tuple2('じょうび', '常備'),
			_Utils_Tuple2('しょうひょう', '商標'),
			_Utils_Tuple2('しょうふ', '娼婦'),
			_Utils_Tuple2('じょうぶつ', '成仏'),
			_Utils_Tuple2('しょうほう', '商法'),
			_Utils_Tuple2('しょうぼうしょ', '消防署'),
			_Utils_Tuple2('しょうぼうちょう', '消防庁'),
			_Utils_Tuple2('じょうみゃく', '静脈'),
			_Utils_Tuple2('じょうむ', '常務'),
			_Utils_Tuple2('じょうむ', '乗務'),
			_Utils_Tuple2('じょうむいん', '乗務員'),
			_Utils_Tuple2('しょうめつ', '消滅'),
			_Utils_Tuple2('しょうもう', '消耗'),
			_Utils_Tuple2('しょうもうひん', '消耗品'),
			_Utils_Tuple2('じょうもんじだい', '縄文時代'),
			_Utils_Tuple2('じょうやく', '条約'),
			_Utils_Tuple2('じょうりく', '上陸'),
			_Utils_Tuple2('しょうりつ', '勝率'),
			_Utils_Tuple2('しょうりとうしゅ', '勝利投手'),
			_Utils_Tuple2('じょうりゅう', '蒸留'),
			_Utils_Tuple2('じょうるり', '浄るり'),
			_Utils_Tuple2('しょうれい', '奨励'),
			_Utils_Tuple2('じょうれい', '条例'),
			_Utils_Tuple2('じょうれん', '常連'),
			_Utils_Tuple2('じょがくせい', '女学生'),
			_Utils_Tuple2('じょがっこう', '女学校'),
			_Utils_Tuple2('しょかん', '書簡'),
			_Utils_Tuple2('じょかんとく', '助監督'),
			_Utils_Tuple2('しょき', '書記'),
			_Utils_Tuple2('しょきゅう', '初球'),
			_Utils_Tuple2('じょきょ', '除去'),
			_Utils_Tuple2('じょきょく', '序曲'),
			_Utils_Tuple2('じょきん', '除菌'),
			_Utils_Tuple2('しょくぎょうびょう', '職業病'),
			_Utils_Tuple2('しょくする', '食する'),
			_Utils_Tuple2('しょくつう', '食通'),
			_Utils_Tuple2('しょくにく', '食肉'),
			_Utils_Tuple2('しょくはつ', '触発'),
			_Utils_Tuple2('しょくひんてんかぶつ', '食品添加物'),
			_Utils_Tuple2('しょくぶつにんげん', '植物人間'),
			_Utils_Tuple2('しょくぶつゆ', '植物油'),
			_Utils_Tuple2('しょくみん', '植民'),
			_Utils_Tuple2('しょくみんち', '植民地'),
			_Utils_Tuple2('しょくむ', '職務'),
			_Utils_Tuple2('しょくん', '諸君'),
			_Utils_Tuple2('しょけい', '処刑'),
			_Utils_Tuple2('じょげん', '助言'),
			_Utils_Tuple2('じょこう', '徐行'),
			_Utils_Tuple2('しょこく', '諸国'),
			_Utils_Tuple2('しょさい', '書斎'),
			_Utils_Tuple2('しょざい', '所在'),
			_Utils_Tuple2('しょざいち', '所在地'),
			_Utils_Tuple2('しょしき', '書式'),
			_Utils_Tuple2('じょしつ', '除湿'),
			_Utils_Tuple2('しょじょ', '処女'),
			_Utils_Tuple2('しょじょさく', '処女作'),
			_Utils_Tuple2('しょする', '処する'),
			_Utils_Tuple2('じょせいほるもん', '女性ホルモン'),
			_Utils_Tuple2('しょせき', '書籍'),
			_Utils_Tuple2('じょせき', '除籍'),
			_Utils_Tuple2('じょせつ', '除雪'),
			_Utils_Tuple2('しょせん', 'しょせん'),
			_Utils_Tuple2('しょぞう', '所蔵'),
			_Utils_Tuple2('じょそう', '女装'),
			_Utils_Tuple2('じょそう', '助走'),
			_Utils_Tuple2('じょそう', '除草'),
			_Utils_Tuple2('しょたい', '所帯'),
			_Utils_Tuple2('しょだな', '書棚'),
			_Utils_Tuple2('しょだん', '初段'),
			_Utils_Tuple2('しょち', '処置'),
			_Utils_Tuple2('じょちゅう', '女中'),
			_Utils_Tuple2('しょっかく', '触覚'),
			_Utils_Tuple2('しょっと', 'ショット'),
			_Utils_Tuple2('じょてい', '女帝'),
			_Utils_Tuple2('しょとう', '諸島'),
			_Utils_Tuple2('しょとう', '初等'),
			_Utils_Tuple2('しょとく', '所得'),
			_Utils_Tuple2('しょとくぜい', '所得税'),
			_Utils_Tuple2('しょばつ', '処罰'),
			_Utils_Tuple2('しょはん', '初版'),
			_Utils_Tuple2('じょばん', '序盤'),
			_Utils_Tuple2('しょぶん', '処分'),
			_Utils_Tuple2('しょほう', '処方'),
			_Utils_Tuple2('しょほうせん', '処方せん'),
			_Utils_Tuple2('しょぼしょぼ', 'しょぼしょぼ'),
			_Utils_Tuple2('しょぼん', 'しょぼん'),
			_Utils_Tuple2('じょめい', '除名'),
			_Utils_Tuple2('しょや', '初夜'),
			_Utils_Tuple2('じょれつ', '序列'),
			_Utils_Tuple2('しょんぼり', 'しょんぼり'),
			_Utils_Tuple2('じらい', '地雷'),
			_Utils_Tuple2('しらがぞめ', '白髪染め'),
			_Utils_Tuple2('しらける', '白ける'),
			_Utils_Tuple2('しらたき', '白滝'),
			_Utils_Tuple2('しりごみ', '尻込み'),
			_Utils_Tuple2('しりこん', 'シリコン'),
			_Utils_Tuple2('じりじり', 'じりじり'),
			_Utils_Tuple2('しりぞける', '退ける'),
			_Utils_Tuple2('じりつしんけい', '自律神経'),
			_Utils_Tuple2('しりょ', '思慮'),
			_Utils_Tuple2('じりょく', '磁力'),
			_Utils_Tuple2('しりんだー', 'シリンダー'),
			_Utils_Tuple2('しるもの', '汁物'),
			_Utils_Tuple2('しれい', '司令'),
			_Utils_Tuple2('じれい', '辞令'),
			_Utils_Tuple2('じれい', '事例'),
			_Utils_Tuple2('しれいかん', '司令官'),
			_Utils_Tuple2('しれつ', 'し烈'),
			_Utils_Tuple2('しれん', '試練'),
			_Utils_Tuple2('じれんま', 'ジレンマ'),
			_Utils_Tuple2('しわざ', '仕業'),
			_Utils_Tuple2('しわしわ', 'しわしわ'),
			_Utils_Tuple2('じわじわ', 'じわじわ'),
			_Utils_Tuple2('じん', '陣'),
			_Utils_Tuple2('じん', '刃'),
			_Utils_Tuple2('じんいてき', '人為的'),
			_Utils_Tuple2('じんえい', '陣営'),
			_Utils_Tuple2('じんか', '人家'),
			_Utils_Tuple2('しんかい', '深海'),
			_Utils_Tuple2('しんがい', '侵害'),
			_Utils_Tuple2('しんがお', '新顔'),
			_Utils_Tuple2('しんかん', '新刊'),
			_Utils_Tuple2('しんぎ', '審議'),
			_Utils_Tuple2('しんぎ', '真偽'),
			_Utils_Tuple2('じんぎ', '仁義'),
			_Utils_Tuple2('しんきょう', '心境'),
			_Utils_Tuple2('しんくう', '真空'),
			_Utils_Tuple2('じんぐう', '神宮'),
			_Utils_Tuple2('じんくす', 'ジンクス'),
			_Utils_Tuple2('しんくろ', 'シンクロ'),
			_Utils_Tuple2('しんけいすいじゃく', '神経衰弱'),
			_Utils_Tuple2('しんけん', '親権'),
			_Utils_Tuple2('しんこう', '信仰'),
			_Utils_Tuple2('しんこう', '侵攻'),
			_Utils_Tuple2('しんこう', '振興'),
			_Utils_Tuple2('しんこう', '新興'),
			_Utils_Tuple2('しんこくか', '深刻化'),
			_Utils_Tuple2('しんしゅ', '新酒'),
			_Utils_Tuple2('しんじゅう', '心中'),
			_Utils_Tuple2('しんしゅく', '伸縮'),
			_Utils_Tuple2('しんじょう', '信条'),
			_Utils_Tuple2('しんじょう', '心情'),
			_Utils_Tuple2('じんじょう', '尋常'),
			_Utils_Tuple2('しんすい', '浸水'),
			_Utils_Tuple2('しんずい', '真髄'),
			_Utils_Tuple2('しんせい', '神聖'),
			_Utils_Tuple2('しんせさいざー', 'シンセサイザー'),
			_Utils_Tuple2('しんぜん', '親善'),
			_Utils_Tuple2('しんそう', '真相'),
			_Utils_Tuple2('しんそう', '深層'),
			_Utils_Tuple2('じんぞう', '人造'),
			_Utils_Tuple2('じんぞうにんげん', '人造人間'),
			_Utils_Tuple2('じんそく', '迅速'),
			_Utils_Tuple2('しんそこ', '心底'),
			_Utils_Tuple2('じんだい', '甚大'),
			_Utils_Tuple2('しんちょう', '新調'),
			_Utils_Tuple2('しんちんたいしゃ', '新陳代謝'),
			_Utils_Tuple2('じんつう', '陣痛'),
			_Utils_Tuple2('しんてん', '進展'),
			_Utils_Tuple2('しんでん', '神殿'),
			_Utils_Tuple2('しんてんち', '新天地'),
			_Utils_Tuple2('しんと', '信徒'),
			_Utils_Tuple2('しんと', '新都'),
			_Utils_Tuple2('しんと', 'しんと'),
			_Utils_Tuple2('じんと', 'じんと'),
			_Utils_Tuple2('しんどい', 'しんどい'),
			_Utils_Tuple2('しんとう', '神道'),
			_Utils_Tuple2('しんとう', '浸透'),
			_Utils_Tuple2('しんどう', '振動'),
			_Utils_Tuple2('しんどう', '震動'),
			_Utils_Tuple2('じんどう', '人道'),
			_Utils_Tuple2('じんどうてき', '人道的'),
			_Utils_Tuple2('じんどる', '陣取る'),
			_Utils_Tuple2('しんなー', 'シンナー'),
			_Utils_Tuple2('しんなり', 'しんなり'),
			_Utils_Tuple2('しんにゅう', '侵入'),
			_Utils_Tuple2('しんにん', '信任'),
			_Utils_Tuple2('しんばる', 'シンバル'),
			_Utils_Tuple2('しんぴ', '神秘'),
			_Utils_Tuple2('しんぴてき', '神秘的'),
			_Utils_Tuple2('しんふぜん', '心不全'),
			_Utils_Tuple2('じんぶんかがく', '人文科学'),
			_Utils_Tuple2('しんぼう', '辛抱'),
			_Utils_Tuple2('じんぼう', '人望'),
			_Utils_Tuple2('しんまい', '新米'),
			_Utils_Tuple2('しんみ', '親身'),
			_Utils_Tuple2('じんみゃく', '人脈'),
			_Utils_Tuple2('しんみり', 'しんみり'),
			_Utils_Tuple2('しんようじゅ', '針葉樹'),
			_Utils_Tuple2('しんり', '真理'),
			_Utils_Tuple2('しんりゃく', '侵略'),
			_Utils_Tuple2('しんりょう', '診療'),
			_Utils_Tuple2('しんりょく', '新緑'),
			_Utils_Tuple2('しんれい', '心霊'),
			_Utils_Tuple2('しんれいげんしょう', '心霊現象'),
			_Utils_Tuple2('す', '巣'),
			_Utils_Tuple2('す', '素'),
			_Utils_Tuple2('すあし', '素足'),
			_Utils_Tuple2('ずあん', '図案'),
			_Utils_Tuple2('すいあげる', '吸い上げる'),
			_Utils_Tuple2('すいあつ', '水圧'),
			_Utils_Tuple2('すいい', '水位'),
			_Utils_Tuple2('すいい', '推移'),
			_Utils_Tuple2('すいがら', '吸い殻'),
			_Utils_Tuple2('すいぎん', '水銀'),
			_Utils_Tuple2('すいげん', '水源'),
			_Utils_Tuple2('すいこう', '遂行'),
			_Utils_Tuple2('すいさつ', '推察'),
			_Utils_Tuple2('すいさん', '水産'),
			_Utils_Tuple2('すいさんぶつ', '水産物'),
			_Utils_Tuple2('ずいじ', '随時'),
			_Utils_Tuple2('すいじゃく', '衰弱'),
			_Utils_Tuple2('すいしん', '推進'),
			_Utils_Tuple2('すいそ', '水素'),
			_Utils_Tuple2('すいそう', '水槽'),
			_Utils_Tuple2('すいそう', '吹奏'),
			_Utils_Tuple2('すいそうがく', '吹奏楽'),
			_Utils_Tuple2('すいそく', '推測'),
			_Utils_Tuple2('すいたい', '衰退'),
			_Utils_Tuple2('すいだす', '吸い出す'),
			_Utils_Tuple2('すいてき', '水滴'),
			_Utils_Tuple2('すいでん', '水田'),
			_Utils_Tuple2('すいとる', '吸い取る'),
			_Utils_Tuple2('すいばく', '水爆'),
			_Utils_Tuple2('ずいひつ', '随筆'),
			_Utils_Tuple2('ずいぶん', '随分'),
			_Utils_Tuple2('すいぼつ', '水没'),
			_Utils_Tuple2('すいろん', '推論'),
			_Utils_Tuple2('すいんぐ', 'スイング'),
			_Utils_Tuple2('すうこう', '崇高'),
			_Utils_Tuple2('ずうずうしい', 'ずうずうしい'),
			_Utils_Tuple2('すうはい', '崇拝'),
			_Utils_Tuple2('すえおき', '据え置き'),
			_Utils_Tuple2('すえる', '据える'),
			_Utils_Tuple2('すか', 'スカ'),
			_Utils_Tuple2('ずかい', '図解'),
			_Utils_Tuple2('すかいらいん', 'スカイライン'),
			_Utils_Tuple2('すかうと', 'スカウト'),
			_Utils_Tuple2('すがお', '素顔'),
			_Utils_Tuple2('すかし', '透かし'),
			_Utils_Tuple2('すかすか', 'すかすか'),
			_Utils_Tuple2('すがすがしい', 'すがすがしい'),
			_Utils_Tuple2('ずがら', '図柄'),
			_Utils_Tuple2('すがる', 'すがる'),
			_Utils_Tuple2('すきかって', '好き勝手'),
			_Utils_Tuple2('すきこのむ', '好き好む'),
			_Utils_Tuple2('すきずき', '好き好き'),
			_Utils_Tuple2('ずきん', '頭きん'),
			_Utils_Tuple2('ずく', 'ずく'),
			_Utils_Tuple2('すくう', 'すくう'),
			_Utils_Tuple2('すくすく', 'すくすく'),
			_Utils_Tuple2('すくりゅー', 'スクリュー'),
			_Utils_Tuple2('ずけずけ', 'ずけずけ'),
			_Utils_Tuple2('すける', '透ける'),
			_Utils_Tuple2('すごすご', 'すごすご'),
			_Utils_Tuple2('すこぶる', '頗る'),
			_Utils_Tuple2('すこやか', '健やか'),
			_Utils_Tuple2('すさまじい', 'すさまじい'),
			_Utils_Tuple2('ずさん', 'ずさん'),
			_Utils_Tuple2('すじ', '筋'),
			_Utils_Tuple2('すじにく', '筋肉'),
			_Utils_Tuple2('すじみち', '筋道'),
			_Utils_Tuple2('すすき', 'すすき'),
			_Utils_Tuple2('すすぎ', 'すすぎ'),
			_Utils_Tuple2('すすぐ', 'すすぐ'),
			_Utils_Tuple2('すずむし', '鈴虫'),
			_Utils_Tuple2('すずり', '硯'),
			_Utils_Tuple2('すする', 'すする'),
			_Utils_Tuple2('すそ', 'すそ'),
			_Utils_Tuple2('すたーたー', 'スターター'),
			_Utils_Tuple2('すたすた', 'すたすた'),
			_Utils_Tuple2('すだつ', '巣立つ'),
			_Utils_Tuple2('すためん', 'スタメン'),
			_Utils_Tuple2('すだれ', 'すだれ'),
			_Utils_Tuple2('すたれる', '廃れる'),
			_Utils_Tuple2('すたんす', 'スタンス'),
			_Utils_Tuple2('すたんだーど', 'スタンダード'),
			_Utils_Tuple2('すたんばい', 'スタンバイ'),
			_Utils_Tuple2('すづけ', '酢漬け'),
			_Utils_Tuple2('すっと', 'すっと'),
			_Utils_Tuple2('すっぱり', 'すっぱり'),
			_Utils_Tuple2('すっぽり', 'すっぽり'),
			_Utils_Tuple2('すで', '素手'),
			_Utils_Tuple2('すてーたす', 'ステータス'),
			_Utils_Tuple2('すてんれす', 'ステンレス'),
			_Utils_Tuple2('すと', 'スト'),
			_Utils_Tuple2('すとらいかー', 'ストライカー'),
			_Utils_Tuple2('すとらいき', 'ストライキ'),
			_Utils_Tuple2('すとらいく', 'ストライク'),
			_Utils_Tuple2('すとりっぷ', 'ストリップ'),
			_Utils_Tuple2('すとろぼ', 'ストロボ'),
			_Utils_Tuple2('すなあらし', '砂嵐'),
			_Utils_Tuple2('すなっく', 'スナック'),
			_Utils_Tuple2('すなっぷ', 'スナップ'),
			_Utils_Tuple2('すね', 'すね'),
			_Utils_Tuple2('ずのう', '頭脳'),
			_Utils_Tuple2('すのもの', '酢の物'),
			_Utils_Tuple2('すぱーく', 'スパーク'),
			_Utils_Tuple2('すぱいく', 'スパイク'),
			_Utils_Tuple2('すばこ', '巣箱'),
			_Utils_Tuple2('ずばずば', 'ずばずば'),
			_Utils_Tuple2('すはだ', '素肌'),
			_Utils_Tuple2('ずばぬける', 'ずば抜ける'),
			_Utils_Tuple2('すばやい', '素早い'),
			_Utils_Tuple2('ずばり', 'ずばり'),
			_Utils_Tuple2('すぴーか', 'スピーカー'),
			_Utils_Tuple2('すぴっつ', 'スピッツ'),
			_Utils_Tuple2('すぴりっと', 'スピリット'),
			_Utils_Tuple2('すぴん', 'スピン'),
			_Utils_Tuple2('すふぃんくす', 'スフィンクス'),
			_Utils_Tuple2('すぶり', '素振り'),
			_Utils_Tuple2('すべ', 'すべ'),
			_Utils_Tuple2('すぺあ', 'スペア'),
			_Utils_Tuple2('すべらす', '滑らす'),
			_Utils_Tuple2('すべりこむ', '滑り込む'),
			_Utils_Tuple2('ずぼら', 'ずぼら'),
			_Utils_Tuple2('すます', '澄ます'),
			_Utils_Tuple2('すまっしゅ', 'スマッシュ'),
			_Utils_Tuple2('すみ', '炭'),
			_Utils_Tuple2('すみ', '墨'),
			_Utils_Tuple2('すみきる', '澄み切る'),
			_Utils_Tuple2('すみずみ', '隅々'),
			_Utils_Tuple2('すみつく', '住み着く'),
			_Utils_Tuple2('すみっこ', '隅っこ'),
			_Utils_Tuple2('すみび', '炭火'),
			_Utils_Tuple2('すみやか', '速やか'),
			_Utils_Tuple2('すむ', '澄む'),
			_Utils_Tuple2('ずめん', '図面'),
			_Utils_Tuple2('すやき', '素焼き'),
			_Utils_Tuple2('すやすや', 'すやすや'),
			_Utils_Tuple2('すらいだー', 'スライダー'),
			_Utils_Tuple2('すらいでぃんぐ', 'スライディング'),
			_Utils_Tuple2('すらいど', 'スライド'),
			_Utils_Tuple2('ずらす', 'ずらす'),
			_Utils_Tuple2('すらり', 'すらり'),
			_Utils_Tuple2('ずらり', 'ずらり'),
			_Utils_Tuple2('すらりと', 'すらりと'),
			_Utils_Tuple2('ずらりと', 'ずらりと'),
			_Utils_Tuple2('すらんぷ', 'スランプ'),
			_Utils_Tuple2('すり', '刷り'),
			_Utils_Tuple2('ずりおちる', 'ずり落ちる'),
			_Utils_Tuple2('すりきれる', '擦り切れる'),
			_Utils_Tuple2('すりつぶす', 'すりつぶす'),
			_Utils_Tuple2('する', '刷る'),
			_Utils_Tuple2('する', 'する'),
			_Utils_Tuple2('する', '擦る'),
			_Utils_Tuple2('ずる', 'ずる'),
			_Utils_Tuple2('ずるずる', 'ずるずる'),
			_Utils_Tuple2('するめ', 'するめ'),
			_Utils_Tuple2('ずれ', 'ずれ'),
			_Utils_Tuple2('すれすれ', '擦れ擦れ'),
			_Utils_Tuple2('すれちがい', 'すれ違い'),
			_Utils_Tuple2('すれる', '擦れる'),
			_Utils_Tuple2('すろーがん', 'スローガン'),
			_Utils_Tuple2('すろーぷ', 'スロープ'),
			_Utils_Tuple2('すろっと', 'スロット'),
			_Utils_Tuple2('ずんぐり', 'ずんぐり'),
			_Utils_Tuple2('ずんずん', 'ずんずん'),
			_Utils_Tuple2('すんぜん', '寸前'),
			_Utils_Tuple2('すんだん', '寸断'),
			_Utils_Tuple2('すんなり', 'すんなり'),
			_Utils_Tuple2('すんぽう', '寸法'),
			_Utils_Tuple2('せい', '制'),
			_Utils_Tuple2('せい', '聖'),
			_Utils_Tuple2('せい', '勢'),
			_Utils_Tuple2('せい', '精'),
			_Utils_Tuple2('せい', '聖'),
			_Utils_Tuple2('せいあつ', '制圧'),
			_Utils_Tuple2('せいい', '誠意'),
			_Utils_Tuple2('せいか', '聖歌'),
			_Utils_Tuple2('せいか', '製菓'),
			_Utils_Tuple2('せいか', '生花'),
			_Utils_Tuple2('せいか', '青果'),
			_Utils_Tuple2('せいかい', '政界'),
			_Utils_Tuple2('せいき', '正規'),
			_Utils_Tuple2('せいき', '性器'),
			_Utils_Tuple2('せいぎ', '正義'),
			_Utils_Tuple2('せいぎかん', '正義感'),
			_Utils_Tuple2('せいきまつ', '世紀末'),
			_Utils_Tuple2('せいぎょ', '制御'),
			_Utils_Tuple2('せいきょう', '生協'),
			_Utils_Tuple2('せいきょう', '盛況'),
			_Utils_Tuple2('せいけつ', '清潔'),
			_Utils_Tuple2('せいこう', '性交'),
			_Utils_Tuple2('せいこう', '精巧'),
			_Utils_Tuple2('せいこうい', '性行為'),
			_Utils_Tuple2('せいさい', '制裁'),
			_Utils_Tuple2('せいさんがく', '生産額'),
			_Utils_Tuple2('せいし', '精子'),
			_Utils_Tuple2('せいし', '製紙'),
			_Utils_Tuple2('せいじつ', '誠実'),
			_Utils_Tuple2('せいじゃく', '静寂'),
			_Utils_Tuple2('ぜいしゅう', '税収'),
			_Utils_Tuple2('せいしゅく', '静粛'),
			_Utils_Tuple2('せいじゅく', '成熟'),
			_Utils_Tuple2('せいじょう', '清浄'),
			_Utils_Tuple2('せいしんびょう', '精神病'),
			_Utils_Tuple2('せいじんびょう', '成人病'),
			_Utils_Tuple2('せいず', '製図'),
			_Utils_Tuple2('せいする', '制する'),
			_Utils_Tuple2('せいぜい', 'せいぜい'),
			_Utils_Tuple2('ぜいせい', '税制'),
			_Utils_Tuple2('せいそく', '生息'),
			_Utils_Tuple2('せいたい', '声帯'),
			_Utils_Tuple2('せいたい', '生態'),
			_Utils_Tuple2('せいだい', '盛大'),
			_Utils_Tuple2('せいたいけい', '生態系'),
			_Utils_Tuple2('せいたん', '生誕'),
			_Utils_Tuple2('せいち', '聖地'),
			_Utils_Tuple2('せいつう', '精通'),
			_Utils_Tuple2('せいてい', '制定'),
			_Utils_Tuple2('せいてき', '性的'),
			_Utils_Tuple2('せいてつ', '製鉄'),
			_Utils_Tuple2('せいとう', '正統'),
			_Utils_Tuple2('せいとう', '正当'),
			_Utils_Tuple2('せいとう', '政党'),
			_Utils_Tuple2('せいどう', '青銅'),
			_Utils_Tuple2('せいとうせいじ', '政党政治'),
			_Utils_Tuple2('せいとうは', '正統派'),
			_Utils_Tuple2('せいとうぼうえい', '正当防衛'),
			_Utils_Tuple2('せいにく', '精肉'),
			_Utils_Tuple2('せいは', '制覇'),
			_Utils_Tuple2('せいはつ', '整髪'),
			_Utils_Tuple2('せいほう', '製法'),
			_Utils_Tuple2('せいまい', '精米'),
			_Utils_Tuple2('せいみつ', '精密'),
			_Utils_Tuple2('せいみつきかい', '精密機械'),
			_Utils_Tuple2('ぜいむ', '税務'),
			_Utils_Tuple2('ぜいむしょ', '税務署'),
			_Utils_Tuple2('せいめい', '声明'),
			_Utils_Tuple2('せいめいはんだん', '姓名判断'),
			_Utils_Tuple2('せいやく', '制約'),
			_Utils_Tuple2('せいやく', '誓約'),
			_Utils_Tuple2('せいよく', '性欲'),
			_Utils_Tuple2('せいりてき', '生理的'),
			_Utils_Tuple2('せいりゃく', '政略'),
			_Utils_Tuple2('せいりゅう', '清流'),
			_Utils_Tuple2('せいりょう', '声量'),
			_Utils_Tuple2('せいりょう', '清涼'),
			_Utils_Tuple2('せいりょく', '精力'),
			_Utils_Tuple2('せいりょくてき', '精力的'),
			_Utils_Tuple2('せいれいしていとし', '政令指定都市'),
			_Utils_Tuple2('せいろん', '正論'),
			_Utils_Tuple2('せかいかん', '世界観'),
			_Utils_Tuple2('せかす', 'せかす'),
			_Utils_Tuple2('せがむ', 'せがむ'),
			_Utils_Tuple2('せき', '隻'),
			_Utils_Tuple2('せきがいせん', '赤外線'),
			_Utils_Tuple2('せきこむ', 'せき込む'),
			_Utils_Tuple2('せきさい', '積載'),
			_Utils_Tuple2('せきしょ', '関所'),
			_Utils_Tuple2('せきずい', 'せき髄'),
			_Utils_Tuple2('せきひ', '石碑'),
			_Utils_Tuple2('せきわけ', '関脇'),
			_Utils_Tuple2('せくしょん', 'セクション'),
			_Utils_Tuple2('せけん', '世間'),
			_Utils_Tuple2('せけんてき', '世間的'),
			_Utils_Tuple2('せこい', 'せこい'),
			_Utils_Tuple2('せしゅう', '世襲'),
			_Utils_Tuple2('せすじ', '背筋'),
			_Utils_Tuple2('ぜせい', '是正'),
			_Utils_Tuple2('せそう', '世相'),
			_Utils_Tuple2('せたい', '世帯'),
			_Utils_Tuple2('せたいぬし', '世帯主'),
			_Utils_Tuple2('せだん', 'セダン'),
			_Utils_Tuple2('せつ', '節'),
			_Utils_Tuple2('せつ', '節'),
			_Utils_Tuple2('ぜつえん', '絶縁'),
			_Utils_Tuple2('せっかい', '石灰'),
			_Utils_Tuple2('せっかい', '切開'),
			_Utils_Tuple2('せっかち', 'せっかち'),
			_Utils_Tuple2('せっき', '石器'),
			_Utils_Tuple2('せっく', '節句'),
			_Utils_Tuple2('ぜっく', '絶句'),
			_Utils_Tuple2('せっくす', 'セックス'),
			_Utils_Tuple2('ぜっけい', '絶景'),
			_Utils_Tuple2('ぜっさん', '絶賛'),
			_Utils_Tuple2('せつじつ', '切実'),
			_Utils_Tuple2('せっしゅ', '接種'),
			_Utils_Tuple2('せっしゅ', '摂取'),
			_Utils_Tuple2('せっしょう', '折衝'),
			_Utils_Tuple2('せっしょく', '接触'),
			_Utils_Tuple2('せったー', 'セッター'),
			_Utils_Tuple2('ぜつだい', '絶大'),
			_Utils_Tuple2('ぜったいち', '絶対値'),
			_Utils_Tuple2('せつだん', '切断'),
			_Utils_Tuple2('せっちゃくざい', '接着剤'),
			_Utils_Tuple2('せつど', '節度'),
			_Utils_Tuple2('せっとう', '窃盗'),
			_Utils_Tuple2('せつない', '切ない'),
			_Utils_Tuple2('せつに', '切に'),
			_Utils_Tuple2('ぜっぱん', '絶版'),
			_Utils_Tuple2('ぜっぴん', '絶品'),
			_Utils_Tuple2('せっぷく', '切腹'),
			_Utils_Tuple2('せっぷん', 'せっぷん'),
			_Utils_Tuple2('ぜっぺき', '絶壁'),
			_Utils_Tuple2('ぜつみょう', '絶妙'),
			_Utils_Tuple2('ぜつめつ', '絶滅'),
			_Utils_Tuple2('せつもん', '設問'),
			_Utils_Tuple2('ぜに', '銭'),
			_Utils_Tuple2('せばまる', '狭まる'),
			_Utils_Tuple2('せばめる', '狭める'),
			_Utils_Tuple2('せぱれーつ', 'セパレーツ'),
			_Utils_Tuple2('せめ', '責め'),
			_Utils_Tuple2('せめたてる', '責め立てる'),
			_Utils_Tuple2('せり', '競り'),
			_Utils_Tuple2('せれもにー', 'セレモニー'),
			_Utils_Tuple2('せろはん', 'セロハン'),
			_Utils_Tuple2('せん', '腺'),
			_Utils_Tuple2('ぜん', '膳'),
			_Utils_Tuple2('ぜん', '禅'),
			_Utils_Tuple2('ぜん', '膳'),
			_Utils_Tuple2('せんい', '繊維'),
			_Utils_Tuple2('ぜんか', '前科'),
			_Utils_Tuple2('せんかん', '戦艦'),
			_Utils_Tuple2('せんきょ', '占拠'),
			_Utils_Tuple2('せんぎょ', '鮮魚'),
			_Utils_Tuple2('せんきょう', '戦況'),
			_Utils_Tuple2('せんきょうし', '宣教師'),
			_Utils_Tuple2('せんこく', '宣告'),
			_Utils_Tuple2('せんごく', '戦国'),
			_Utils_Tuple2('せんさい', '繊細'),
			_Utils_Tuple2('せんざい', '潜在'),
			_Utils_Tuple2('せんざいてき', '潜在的'),
			_Utils_Tuple2('せんさく', 'せん索'),
			_Utils_Tuple2('せんさばんべつ', '千差万別'),
			_Utils_Tuple2('せんし', '戦士'),
			_Utils_Tuple2('せんしゅうがっこう', '専修学校'),
			_Utils_Tuple2('せんしゅけん', '選手権'),
			_Utils_Tuple2('せんしゅつ', '選出'),
			_Utils_Tuple2('せんじゅつ', '戦術'),
			_Utils_Tuple2('ぜんじゅつ', '前述'),
			_Utils_Tuple2('せんしょう', '戦勝'),
			_Utils_Tuple2('せんしょう', '先勝'),
			_Utils_Tuple2('せんじょう', '洗浄'),
			_Utils_Tuple2('せんしょく', '染色'),
			_Utils_Tuple2('せんしょくたい', '染色体'),
			_Utils_Tuple2('ぜんしん', '前身'),
			_Utils_Tuple2('せんすい', '潜水'),
			_Utils_Tuple2('せんすいかん', '潜水艦'),
			_Utils_Tuple2('ぜんせい', '前世'),
			_Utils_Tuple2('ぜんせい', '全盛'),
			_Utils_Tuple2('せんせいじゅつ', '占星術'),
			_Utils_Tuple2('せんせん', '戦線'),
			_Utils_Tuple2('せんせん', '宣戦'),
			_Utils_Tuple2('せんせんふこく', '宣戦布告'),
			_Utils_Tuple2('ぜんそく', 'ぜん息'),
			_Utils_Tuple2('せんち', 'センチ'),
			_Utils_Tuple2('せんち', '戦地'),
			_Utils_Tuple2('ぜんち', '全治'),
			_Utils_Tuple2('せんちめんたる', 'センチメンタル'),
			_Utils_Tuple2('せんちゃ', '煎茶'),
			_Utils_Tuple2('ぜんちょう', '前兆'),
			_Utils_Tuple2('せんて', '先手'),
			_Utils_Tuple2('ぜんてい', '前提'),
			_Utils_Tuple2('せんてんてき', '先天的'),
			_Utils_Tuple2('せんど', '鮮度'),
			_Utils_Tuple2('ぜんと', '前途'),
			_Utils_Tuple2('せんとう', '戦闘'),
			_Utils_Tuple2('せんどう', '先導'),
			_Utils_Tuple2('せんとうき', '戦闘機'),
			_Utils_Tuple2('せんとらる', 'セントラル'),
			_Utils_Tuple2('ぜんにちせい', '全日制'),
			_Utils_Tuple2('せんにゅう', '潜入'),
			_Utils_Tuple2('せんにゅうかん', '先入観'),
			_Utils_Tuple2('ぜんにん', '善人'),
			_Utils_Tuple2('せんのう', '洗脳'),
			_Utils_Tuple2('せんばい', '専売'),
			_Utils_Tuple2('せんぱく', '船舶'),
			_Utils_Tuple2('せんばつ', '選抜'),
			_Utils_Tuple2('せんぱつとうしゅ', '先発投手'),
			_Utils_Tuple2('せんぷう', '旋風'),
			_Utils_Tuple2('せんぷく', '潜伏'),
			_Utils_Tuple2('せんべつ', 'せん別'),
			_Utils_Tuple2('せんぽう', '戦法'),
			_Utils_Tuple2('せんぼつ', '戦没'),
			_Utils_Tuple2('せんむ', '専務'),
			_Utils_Tuple2('ぜんめつ', '全滅'),
			_Utils_Tuple2('せんゆう', '占有'),
			_Utils_Tuple2('ぜんよう', '全容'),
			_Utils_Tuple2('せんりつ', '旋律'),
			_Utils_Tuple2('せんりひん', '戦利品'),
			_Utils_Tuple2('せんりゃく', '戦略'),
			_Utils_Tuple2('せんりゅう', '川柳'),
			_Utils_Tuple2('せんりょう', '占領'),
			_Utils_Tuple2('せんりょう', '染料'),
			_Utils_Tuple2('ぜんりょう', '善良'),
			_Utils_Tuple2('せんりょく', '戦力'),
			_Utils_Tuple2('ぜんりん', '前輪'),
			_Utils_Tuple2('せんれい', '洗礼'),
			_Utils_Tuple2('せんれん', '洗練'),
			_Utils_Tuple2('そ', '素'),
			_Utils_Tuple2('そう', '葬'),
			_Utils_Tuple2('そう', '創'),
			_Utils_Tuple2('そう', '装'),
			_Utils_Tuple2('そう', '奏'),
			_Utils_Tuple2('そう', '倉'),
			_Utils_Tuple2('そう', '槽'),
			_Utils_Tuple2('そう', '叢'),
			_Utils_Tuple2('ぞうえん', '造園'),
			_Utils_Tuple2('そうおう', '相応'),
			_Utils_Tuple2('そうかい', '爽快'),
			_Utils_Tuple2('そうかつ', '総括'),
			_Utils_Tuple2('そうかん', '創刊'),
			_Utils_Tuple2('ぞうかん', '増刊'),
			_Utils_Tuple2('ぞうきいしょく', '臓器移植'),
			_Utils_Tuple2('そうぎしゃ', '葬儀社'),
			_Utils_Tuple2('ぞうきばやし', '雑木林'),
			_Utils_Tuple2('そうきゅう', '送球'),
			_Utils_Tuple2('そうぐう', '遭遇'),
			_Utils_Tuple2('ぞうげ', '象牙'),
			_Utils_Tuple2('そうげい', '送迎'),
			_Utils_Tuple2('ぞうけい', '造形'),
			_Utils_Tuple2('そうけん', '送検'),
			_Utils_Tuple2('ぞうご', '造語'),
			_Utils_Tuple2('そうごのりいれ', '相互乗り入れ'),
			_Utils_Tuple2('そうさい', '総裁'),
			_Utils_Tuple2('そうさい', '相殺'),
			_Utils_Tuple2('そうざん', '早産'),
			_Utils_Tuple2('そうしつ', '喪失'),
			_Utils_Tuple2('そうじて', '総じて'),
			_Utils_Tuple2('そうじゅう', '操縦'),
			_Utils_Tuple2('そうしょう', '総称'),
			_Utils_Tuple2('そうしょく', '装飾'),
			_Utils_Tuple2('ぞうしょく', '増殖'),
			_Utils_Tuple2('ぞうしん', '増進'),
			_Utils_Tuple2('ぞうすい', '雑炊'),
			_Utils_Tuple2('ぞうせい', '造成'),
			_Utils_Tuple2('そうせいじ', '双生児'),
			_Utils_Tuple2('そうぞくぜい', '相続税'),
			_Utils_Tuple2('そうたい', '相対'),
			_Utils_Tuple2('そうたい', '総体'),
			_Utils_Tuple2('そうだい', '壮大'),
			_Utils_Tuple2('そうたいてき', '相対的'),
			_Utils_Tuple2('ぞうちく', '増築'),
			_Utils_Tuple2('そうちゃく', '装着'),
			_Utils_Tuple2('そうちょう', '総長'),
			_Utils_Tuple2('そうで', '総出'),
			_Utils_Tuple2('そうてい', '想定'),
			_Utils_Tuple2('ぞうてい', '贈呈'),
			_Utils_Tuple2('そうてん', '争点'),
			_Utils_Tuple2('そうでん', '送電'),
			_Utils_Tuple2('ぞうとう', '贈答'),
			_Utils_Tuple2('ぞうとうひん', '贈答品'),
			_Utils_Tuple2('そうなん', '遭難'),
			_Utils_Tuple2('そうねん', '壮年'),
			_Utils_Tuple2('そうば', '相場'),
			_Utils_Tuple2('そうび', '装備'),
			_Utils_Tuple2('ぞうふく', '増幅'),
			_Utils_Tuple2('そうほう', '双方'),
			_Utils_Tuple2('そうむ', '総務'),
			_Utils_Tuple2('ぞうよ', '贈与'),
			_Utils_Tuple2('ぞうり', '草履'),
			_Utils_Tuple2('そうりょ', '僧侶'),
			_Utils_Tuple2('そえもの', '添え物'),
			_Utils_Tuple2('そえる', '添える'),
			_Utils_Tuple2('そーす', 'ソース'),
			_Utils_Tuple2('そがい', '阻害'),
			_Utils_Tuple2('ぞく', '俗'),
			_Utils_Tuple2('そくい', '即位'),
			_Utils_Tuple2('そくざ', '即座'),
			_Utils_Tuple2('そくざに', '即座に'),
			_Utils_Tuple2('そくしつ', '側室'),
			_Utils_Tuple2('ぞくしょう', '俗称'),
			_Utils_Tuple2('そくしん', '促進'),
			_Utils_Tuple2('そくせき', '即席'),
			_Utils_Tuple2('ぞくせつ', '俗説'),
			_Utils_Tuple2('そくばく', '束縛'),
			_Utils_Tuple2('そげき', '狙撃'),
			_Utils_Tuple2('そこそこ', 'そこそこ'),
			_Utils_Tuple2('そこなう', '損なう'),
			_Utils_Tuple2('そこなし', '底無し'),
			_Utils_Tuple2('そこねる', '損ねる'),
			_Utils_Tuple2('そこら', 'そこら'),
			_Utils_Tuple2('そし', '阻止'),
			_Utils_Tuple2('そしょう', '訴訟'),
			_Utils_Tuple2('そそう', '粗相'),
			_Utils_Tuple2('そそぐ', '注ぐ'),
			_Utils_Tuple2('そだい', '粗大'),
			_Utils_Tuple2('そだてあげる', '育て上げる'),
			_Utils_Tuple2('そち', '措置'),
			_Utils_Tuple2('そつう', '疎通'),
			_Utils_Tuple2('そっきょう', '即興'),
			_Utils_Tuple2('そっきん', '側近'),
			_Utils_Tuple2('そっけつ', '即決'),
			_Utils_Tuple2('そっこう', '即効'),
			_Utils_Tuple2('そっこう', '速攻'),
			_Utils_Tuple2('そっこうせい', '即効性'),
			_Utils_Tuple2('そっこく', '即刻'),
			_Utils_Tuple2('そっせん', '率先'),
			_Utils_Tuple2('そで', '袖'),
			_Utils_Tuple2('そてー', 'ソテー'),
			_Utils_Tuple2('そでぐち', '袖口'),
			_Utils_Tuple2('そとづら', '外面'),
			_Utils_Tuple2('そなえ', '供え'),
			_Utils_Tuple2('そなえ', '備え'),
			_Utils_Tuple2('そなえつけ', '備え付け'),
			_Utils_Tuple2('そなえもの', '供え物'),
			_Utils_Tuple2('そなえる', '備える'),
			_Utils_Tuple2('そなえる', '供える'),
			_Utils_Tuple2('そなわる', '備わる'),
			_Utils_Tuple2('その', '園'),
			_Utils_Tuple2('そのくせ', 'そのくせ'),
			_Utils_Tuple2('そのもの', '其の物'),
			_Utils_Tuple2('そばかす', 'そばかす'),
			_Utils_Tuple2('そぶり', '素振り'),
			_Utils_Tuple2('そぼく', '素朴'),
			_Utils_Tuple2('そまつ', '粗末'),
			_Utils_Tuple2('そまる', '染まる'),
			_Utils_Tuple2('そむく', '背く'),
			_Utils_Tuple2('そむりえ', 'ソムリエ'),
			_Utils_Tuple2('そめ', '染め'),
			_Utils_Tuple2('そめあげる', '染め上げる'),
			_Utils_Tuple2('そめもの', '染め物'),
			_Utils_Tuple2('そもそも', 'そもそも'),
			_Utils_Tuple2('そらす', 'そらす'),
			_Utils_Tuple2('そらみみ', '空耳'),
			_Utils_Tuple2('そり', 'そり'),
			_Utils_Tuple2('そる', '反る'),
			_Utils_Tuple2('それる', 'それる'),
			_Utils_Tuple2('そろ', 'ソロ'),
			_Utils_Tuple2('そろい', 'そろい'),
			_Utils_Tuple2('そろばん', 'そろばん'),
			_Utils_Tuple2('ぞろめ', 'ぞろ目'),
			_Utils_Tuple2('そんげん', '尊厳'),
			_Utils_Tuple2('そんげんし', '尊厳死'),
			_Utils_Tuple2('そんしょう', '損傷'),
			_Utils_Tuple2('そんする', '損する'),
			_Utils_Tuple2('ぞんぶん', '存分'),
			_Utils_Tuple2('だ', '打'),
			_Utils_Tuple2('だ', '打'),
			_Utils_Tuple2('たーる', 'タール'),
			_Utils_Tuple2('たい', '耐'),
			_Utils_Tuple2('たい', '帯'),
			_Utils_Tuple2('たいあん', '大安'),
			_Utils_Tuple2('だいいっせん', '第一線'),
			_Utils_Tuple2('たいか', '退化'),
			_Utils_Tuple2('たいか', '大家'),
			_Utils_Tuple2('たいか', '耐火'),
			_Utils_Tuple2('たいが', '大河'),
			_Utils_Tuple2('たいがい', '対外'),
			_Utils_Tuple2('たいがい', '大概'),
			_Utils_Tuple2('たいき', '待機'),
			_Utils_Tuple2('だいぎし', '代議士'),
			_Utils_Tuple2('だいきち', '大吉'),
			_Utils_Tuple2('たいきゅう', '耐久'),
			_Utils_Tuple2('たいきょ', '退去'),
			_Utils_Tuple2('たいこう', '対抗'),
			_Utils_Tuple2('たいこうしゃ', '対向車'),
			_Utils_Tuple2('たいこく', '大国'),
			_Utils_Tuple2('だいごみ', 'だいごみ'),
			_Utils_Tuple2('たいさ', '大佐'),
			_Utils_Tuple2('たいさん', '退散'),
			_Utils_Tuple2('たいじ', '胎児'),
			_Utils_Tuple2('たいじ', '退治'),
			_Utils_Tuple2('だいじぇすと', 'ダイジェスト'),
			_Utils_Tuple2('たいしゃ', '代謝'),
			_Utils_Tuple2('たいしゃく', '貸借'),
			_Utils_Tuple2('たいしゅう', '体臭'),
			_Utils_Tuple2('たいしゅう', '大衆'),
			_Utils_Tuple2('たいしゅうか', '大衆化'),
			_Utils_Tuple2('たいしゅうてき', '大衆的'),
			_Utils_Tuple2('たいしょ', '対処'),
			_Utils_Tuple2('たいしょう', '大賞'),
			_Utils_Tuple2('たいしょう', '大将'),
			_Utils_Tuple2('だいしょう', '代償'),
			_Utils_Tuple2('たいしん', '耐震'),
			_Utils_Tuple2('たいすい', '耐水'),
			_Utils_Tuple2('たいせい', '体勢'),
			_Utils_Tuple2('たいせい', '態勢'),
			_Utils_Tuple2('たいそう', '大層'),
			_Utils_Tuple2('たいそう', '大層'),
			_Utils_Tuple2('だいそれた', '大それた'),
			_Utils_Tuple2('たいだ', '怠惰'),
			_Utils_Tuple2('だいだ', '代打'),
			_Utils_Tuple2('だいたい', '代替'),
			_Utils_Tuple2('たいちょう', '隊長'),
			_Utils_Tuple2('だいちょう', '台帳'),
			_Utils_Tuple2('だいちょう', '大腸'),
			_Utils_Tuple2('だいちょうきん', '大腸菌'),
			_Utils_Tuple2('たいとう', '台頭'),
			_Utils_Tuple2('だいなし', '台無し'),
			_Utils_Tuple2('だいなまいと', 'ダイナマイト'),
			_Utils_Tuple2('だいなみっく', 'ダイナミック'),
			_Utils_Tuple2('たいにん', '退任'),
			_Utils_Tuple2('たいねつ', '耐熱'),
			_Utils_Tuple2('たいねつがらす', '耐熱ガラス'),
			_Utils_Tuple2('だいの', '大の'),
			_Utils_Tuple2('たいのう', '滞納'),
			_Utils_Tuple2('だいのじ', '大の字'),
			_Utils_Tuple2('だいぶ', 'ダイブ'),
			_Utils_Tuple2('たいべつ', '大別'),
			_Utils_Tuple2('だいべん', '代弁'),
			_Utils_Tuple2('たいほう', '大砲'),
			_Utils_Tuple2('たいぼく', '大木'),
			_Utils_Tuple2('だいほん', '台本'),
			_Utils_Tuple2('たいま', '大麻'),
			_Utils_Tuple2('たいまつ', 'たいまつ'),
			_Utils_Tuple2('たいまん', '怠慢'),
			_Utils_Tuple2('だいみょう', '大名'),
			_Utils_Tuple2('だいもく', '題目'),
			_Utils_Tuple2('たいよ', '貸与'),
			_Utils_Tuple2('たいようけい', '太陽系'),
			_Utils_Tuple2('たいようれき', '太陽暦'),
			_Utils_Tuple2('だいりせき', '大理石'),
			_Utils_Tuple2('たいりょう', '大漁'),
			_Utils_Tuple2('たいる', 'タイル'),
			_Utils_Tuple2('だいれくと', 'ダイレクト'),
			_Utils_Tuple2('たうえ', '田植え'),
			_Utils_Tuple2('だうと', 'ダウト'),
			_Utils_Tuple2('だうんじゃけっと', 'ダウンジャケット'),
			_Utils_Tuple2('だえき', 'だ液'),
			_Utils_Tuple2('たえま', '絶え間'),
			_Utils_Tuple2('たえる', '絶える'),
			_Utils_Tuple2('たえる', '耐える'),
			_Utils_Tuple2('だえん', 'だ円'),
			_Utils_Tuple2('たか', '高'),
			_Utils_Tuple2('たか', 'たか'),
			_Utils_Tuple2('たかい', '他界'),
			_Utils_Tuple2('たかだい', '高台'),
			_Utils_Tuple2('たかだか', '高々'),
			_Utils_Tuple2('だがっき', '打楽器'),
			_Utils_Tuple2('たかとび', '高跳び'),
			_Utils_Tuple2('たかな', '高菜'),
			_Utils_Tuple2('たかね', '高値'),
			_Utils_Tuple2('たかぶる', '高ぶる'),
			_Utils_Tuple2('たかまり', '高まり'),
			_Utils_Tuple2('たかまる', '高まる'),
			_Utils_Tuple2('たかめる', '高める'),
			_Utils_Tuple2('たがやす', '耕す'),
			_Utils_Tuple2('たかる', 'たかる'),
			_Utils_Tuple2('たき', '多岐'),
			_Utils_Tuple2('たきぎ', '薪'),
			_Utils_Tuple2('たきこむ', '炊き込む'),
			_Utils_Tuple2('たきしーど', 'タキシード'),
			_Utils_Tuple2('だきゅう', '打球'),
			_Utils_Tuple2('だきょう', '妥協'),
			_Utils_Tuple2('たぐ', 'タグ'),
			_Utils_Tuple2('たぐい', 'たぐい'),
			_Utils_Tuple2('だくおん', '濁音'),
			_Utils_Tuple2('たくじしょ', '託児所'),
			_Utils_Tuple2('たくじょう', '卓上'),
			_Utils_Tuple2('たくする', '託する'),
			_Utils_Tuple2('たくち', '宅地'),
			_Utils_Tuple2('だくてん', '濁点'),
			_Utils_Tuple2('たくましい', 'たくましい'),
			_Utils_Tuple2('たくみ', '巧み'),
			_Utils_Tuple2('たくみ', 'たくみ'),
			_Utils_Tuple2('たくらむ', '企む'),
			_Utils_Tuple2('たくわえる', '蓄える'),
			_Utils_Tuple2('たけ', '丈'),
			_Utils_Tuple2('だげき', '打撃'),
			_Utils_Tuple2('たさい', '多彩'),
			_Utils_Tuple2('ださく', '駄作'),
			_Utils_Tuple2('たし', '足し'),
			_Utils_Tuple2('だし', '出し'),
			_Utils_Tuple2('たしつ', '多湿'),
			_Utils_Tuple2('だしゃ', '打者'),
			_Utils_Tuple2('だじゃれ', 'だじゃれ'),
			_Utils_Tuple2('たじゅう', '多重'),
			_Utils_Tuple2('たしゅたよう', '多種多様'),
			_Utils_Tuple2('だしん', '打診'),
			_Utils_Tuple2('だすう', '打数'),
			_Utils_Tuple2('たずさわる', '携わる'),
			_Utils_Tuple2('だせい', '惰性'),
			_Utils_Tuple2('だせき', '打席'),
			_Utils_Tuple2('だそく', '蛇足'),
			_Utils_Tuple2('たたき', 'たたき'),
			_Utils_Tuple2('たたきこむ', 'たたきこむ'),
			_Utils_Tuple2('たたきだす', 'たたき出す'),
			_Utils_Tuple2('ただす', '正す'),
			_Utils_Tuple2('たたずむ', 'たたずむ'),
			_Utils_Tuple2('だだっぴろい', 'だだっ広い'),
			_Utils_Tuple2('ただよう', '漂う'),
			_Utils_Tuple2('たち', '太刀'),
			_Utils_Tuple2('たち', '質'),
			_Utils_Tuple2('たちあい', '立ち会い'),
			_Utils_Tuple2('たちあう', '立ち会う'),
			_Utils_Tuple2('たちあがり', '立ち上がり'),
			_Utils_Tuple2('たちきる', '断ち切る'),
			_Utils_Tuple2('たちならぶ', '立ち並ぶ'),
			_Utils_Tuple2('たちのき', '立ち退き'),
			_Utils_Tuple2('たちのく', '立ち退く'),
			_Utils_Tuple2('たちのぼる', '立ち上る'),
			_Utils_Tuple2('たちはだかる', '立ちはだかる'),
			_Utils_Tuple2('たちむかう', '立ち向かう'),
			_Utils_Tuple2('たちよる', '立ち寄る'),
			_Utils_Tuple2('たつ', '断つ'),
			_Utils_Tuple2('たつ', '起つ'),
			_Utils_Tuple2('だつい', '脱衣'),
			_Utils_Tuple2('だっかい', '脱会'),
			_Utils_Tuple2('だっかん', '奪還'),
			_Utils_Tuple2('たっくる', 'タックル'),
			_Utils_Tuple2('だっこ', '抱っこ'),
			_Utils_Tuple2('だっしゅ', 'ダッシュ'),
			_Utils_Tuple2('だっしゅう', '脱臭'),
			_Utils_Tuple2('だっしゅつ', '脱出'),
			_Utils_Tuple2('だっしょく', '脱色'),
			_Utils_Tuple2('たつじん', '達人'),
			_Utils_Tuple2('だっすい', '脱水'),
			_Utils_Tuple2('だつぜい', '脱税'),
			_Utils_Tuple2('だっせん', '脱線'),
			_Utils_Tuple2('だっそう', '脱走'),
			_Utils_Tuple2('だったい', '脱退'),
			_Utils_Tuple2('たっぷ', 'タップ'),
			_Utils_Tuple2('だつぼう', '脱帽'),
			_Utils_Tuple2('だつもう', '脱毛'),
			_Utils_Tuple2('だつらく', '脱落'),
			_Utils_Tuple2('たて', '盾'),
			_Utils_Tuple2('たてうり', '建て売り'),
			_Utils_Tuple2('たてかえ', '立て替え'),
			_Utils_Tuple2('たてなおす', '立て直す'),
			_Utils_Tuple2('だてん', '打点'),
			_Utils_Tuple2('だとう', '妥当'),
			_Utils_Tuple2('だとう', '打倒'),
			_Utils_Tuple2('たどりつく', 'たどり着く'),
			_Utils_Tuple2('たどる', 'たどる'),
			_Utils_Tuple2('だに', 'だに'),
			_Utils_Tuple2('たにんぎょうぎ', '他人行儀'),
			_Utils_Tuple2('たねあかし', '種明かし'),
			_Utils_Tuple2('たねうま', '種馬'),
			_Utils_Tuple2('たねん', '多年'),
			_Utils_Tuple2('たのみこむ', '頼み込む'),
			_Utils_Tuple2('たば', '束'),
			_Utils_Tuple2('だは', '打破'),
			_Utils_Tuple2('たばねる', '束ねる'),
			_Utils_Tuple2('たび', '足袋'),
			_Utils_Tuple2('たびかさなる', '度重なる'),
			_Utils_Tuple2('たびじ', '旅路'),
			_Utils_Tuple2('だぶつく', 'だぶつく'),
			_Utils_Tuple2('だぶる', 'ダブる'),
			_Utils_Tuple2('だぶるす', 'ダブルス'),
			_Utils_Tuple2('だぶるぷれー', 'ダブルプレー'),
			_Utils_Tuple2('だぼく', '打撲'),
			_Utils_Tuple2('たま', '弾'),
			_Utils_Tuple2('たましい', '魂'),
			_Utils_Tuple2('だまりこくる', '黙りこくる'),
			_Utils_Tuple2('だまりこむ', '黙り込む'),
			_Utils_Tuple2('たまる', '堪る'),
			_Utils_Tuple2('たみ', '民'),
			_Utils_Tuple2('ため', '溜め'),
			_Utils_Tuple2('ためこむ', 'ため込む'),
			_Utils_Tuple2('ためらう', 'ためらう'),
			_Utils_Tuple2('ためん', '多面'),
			_Utils_Tuple2('ためんてき', '多面的'),
			_Utils_Tuple2('たもくてき', '多目的'),
			_Utils_Tuple2('たよう', '多様'),
			_Utils_Tuple2('たよう', '多様'),
			_Utils_Tuple2('たようか', '多様化'),
			_Utils_Tuple2('たら', '鱈'),
			_Utils_Tuple2('だらく', '堕落'),
			_Utils_Tuple2('だらしない', 'だらしない'),
			_Utils_Tuple2('たらす', '垂らす'),
			_Utils_Tuple2('たらたら', 'たらたら'),
			_Utils_Tuple2('だらだら', 'だらだら'),
			_Utils_Tuple2('だりつ', '打率'),
			_Utils_Tuple2('たる', 'たる'),
			_Utils_Tuple2('だるま', 'だるま'),
			_Utils_Tuple2('たるむ', 'たるむ'),
			_Utils_Tuple2('たれ', '垂れ'),
			_Utils_Tuple2('たれる', '垂れる'),
			_Utils_Tuple2('たん', 'たん'),
			_Utils_Tuple2('たん', '端'),
			_Utils_Tuple2('たん', '炭'),
			_Utils_Tuple2('たん', '譚'),
			_Utils_Tuple2('たん', '反'),
			_Utils_Tuple2('だん', '暖'),
			_Utils_Tuple2('だん', '弾'),
			_Utils_Tuple2('だんあつ', '弾圧'),
			_Utils_Tuple2('たんいつ', '単一'),
			_Utils_Tuple2('たんいつ', '単一'),
			_Utils_Tuple2('たんか', '単価'),
			_Utils_Tuple2('たんか', '短歌'),
			_Utils_Tuple2('だんがい', '断崖'),
			_Utils_Tuple2('たんきゅう', '探求'),
			_Utils_Tuple2('たんきゅう', '探究'),
			_Utils_Tuple2('だんけつ', '団結'),
			_Utils_Tuple2('だんげん', '断言'),
			_Utils_Tuple2('だんこ', '断固'),
			_Utils_Tuple2('だんごう', '談合'),
			_Utils_Tuple2('たんごのせっく', '端午の節句'),
			_Utils_Tuple2('だんさ', '段差'),
			_Utils_Tuple2('たんさんがす', '炭酸ガス'),
			_Utils_Tuple2('たんさんすい', '炭酸水'),
			_Utils_Tuple2('だんじ', '男児'),
			_Utils_Tuple2('たんしゃ', '単車'),
			_Utils_Tuple2('たんしゅく', '短縮'),
			_Utils_Tuple2('だんじょどうけん', '男女同権'),
			_Utils_Tuple2('たんしん', '単身'),
			_Utils_Tuple2('たんしんふにん', '単身赴任'),
			_Utils_Tuple2('たんすい', '淡水'),
			_Utils_Tuple2('だんすい', '断水'),
			_Utils_Tuple2('たんすいかぶつ', '炭水化物'),
			_Utils_Tuple2('たんすう', '単数'),
			_Utils_Tuple2('だんぜつ', '断絶'),
			_Utils_Tuple2('たんせん', '単線'),
			_Utils_Tuple2('だんぜん', '断然'),
			_Utils_Tuple2('たんそ', '炭素'),
			_Utils_Tuple2('だんそう', '断層'),
			_Utils_Tuple2('だんそんじょひ', '男尊女卑'),
			_Utils_Tuple2('たんたい', '単体'),
			_Utils_Tuple2('たんたん', '淡々'),
			_Utils_Tuple2('たんち', '探知'),
			_Utils_Tuple2('たんちょう', '単調'),
			_Utils_Tuple2('たんちょう', '短調'),
			_Utils_Tuple2('だんでぃー', 'ダンディー'),
			_Utils_Tuple2('たんてき', '端的'),
			_Utils_Tuple2('だんどり', '段取り'),
			_Utils_Tuple2('だんねつ', '断熱'),
			_Utils_Tuple2('たんねん', '丹念'),
			_Utils_Tuple2('だんねん', '断念'),
			_Utils_Tuple2('たんのう', '堪能'),
			_Utils_Tuple2('たんぱく', 'たんぱく'),
			_Utils_Tuple2('たんぱく', '淡白'),
			_Utils_Tuple2('たんぱつ', '単発'),
			_Utils_Tuple2('たんばりん', 'タンバリン'),
			_Utils_Tuple2('だんぷ', 'ダンプ'),
			_Utils_Tuple2('だんべる', 'ダンベル'),
			_Utils_Tuple2('たんぺん', '短編'),
			_Utils_Tuple2('だんぺん', '断片'),
			_Utils_Tuple2('だんぺんてき', '断片的'),
			_Utils_Tuple2('たんぽ', '担保'),
			_Utils_Tuple2('たんぽぽ', 'たんぽぽ'),
			_Utils_Tuple2('たんまつ', '端末'),
			_Utils_Tuple2('だんめん', '断面'),
			_Utils_Tuple2('だんらん', '団らん'),
			_Utils_Tuple2('たんれん', '鍛練'),
			_Utils_Tuple2('だんわ', '談話'),
			_Utils_Tuple2('ち', '値'),
			_Utils_Tuple2('ちぇろ', 'チェロ'),
			_Utils_Tuple2('ちか', '地価'),
			_Utils_Tuple2('ちかい', '誓い'),
			_Utils_Tuple2('ちかう', '誓う'),
			_Utils_Tuple2('ちかく', '知覚'),
			_Utils_Tuple2('ちがく', '地学'),
			_Utils_Tuple2('ちかちか', 'ちかちか'),
			_Utils_Tuple2('ちかづく', '近づく'),
			_Utils_Tuple2('ちからぞえ', '力添え'),
			_Utils_Tuple2('ちぎる', 'ちぎる'),
			_Utils_Tuple2('ちぎれる', 'ちぎれる'),
			_Utils_Tuple2('ちく', '築'),
			_Utils_Tuple2('ちくさん', '畜産'),
			_Utils_Tuple2('ちくしょう', '畜生'),
			_Utils_Tuple2('ちくせき', '蓄積'),
			_Utils_Tuple2('ちくちく', 'ちくちく'),
			_Utils_Tuple2('ちくび', '乳首'),
			_Utils_Tuple2('ちくり', 'ちくり'),
			_Utils_Tuple2('ちくわ', '竹輪'),
			_Utils_Tuple2('ちさい', '地裁'),
			_Utils_Tuple2('ちし', '致死'),
			_Utils_Tuple2('ちしきじん', '知識人'),
			_Utils_Tuple2('ちしつ', '地質'),
			_Utils_Tuple2('ちそう', '地層'),
			_Utils_Tuple2('ちたい', '地帯'),
			_Utils_Tuple2('ちたん', 'チタン'),
			_Utils_Tuple2('ちち', '乳'),
			_Utils_Tuple2('ちぢまる', '縮まる'),
			_Utils_Tuple2('ちぢみ', '縮み'),
			_Utils_Tuple2('ちぢむ', '縮む'),
			_Utils_Tuple2('ちぢめる', '縮める'),
			_Utils_Tuple2('ちぢれる', '縮れる'),
			_Utils_Tuple2('ちつじょ', '秩序'),
			_Utils_Tuple2('ちっそ', '窒素'),
			_Utils_Tuple2('ちっそく', '窒息'),
			_Utils_Tuple2('ちっぽけ', 'ちっぽけ'),
			_Utils_Tuple2('ちなむ', 'ちなむ'),
			_Utils_Tuple2('ちのう', '知能'),
			_Utils_Tuple2('ちのうしすう', '知能指数'),
			_Utils_Tuple2('ちび', 'ちび'),
			_Utils_Tuple2('ちびちび', 'ちびちび'),
			_Utils_Tuple2('ちひょう', '地表'),
			_Utils_Tuple2('ちびる', 'ちびる'),
			_Utils_Tuple2('ちほうぎょうせい', '地方行政'),
			_Utils_Tuple2('ちほうこうきょうだんたい', '地方公共団体'),
			_Utils_Tuple2('ちほうさいばんしょ', '地方裁判所'),
			_Utils_Tuple2('ちほうじち', '地方自治'),
			_Utils_Tuple2('ちほうじちたい', '地方自治体'),
			_Utils_Tuple2('ちまた', 'ちまた'),
			_Utils_Tuple2('ちみつ', '緻密'),
			_Utils_Tuple2('ちめいてき', '致命的'),
			_Utils_Tuple2('ちめいど', '知名度'),
			_Utils_Tuple2('ちゃーたー', 'チャーター'),
			_Utils_Tuple2('ちゃかい', '茶会'),
			_Utils_Tuple2('ちゃくじつ', '着実'),
			_Utils_Tuple2('ちゃくしゅ', '着手'),
			_Utils_Tuple2('ちゃくしょく', '着色'),
			_Utils_Tuple2('ちゃくち', '着地'),
			_Utils_Tuple2('ちゃくちゃく', '着々'),
			_Utils_Tuple2('ちゃくにん', '着任'),
			_Utils_Tuple2('ちゃくよう', '着用'),
			_Utils_Tuple2('ちゃち', 'ちゃち'),
			_Utils_Tuple2('ちゃっか', '着火'),
			_Utils_Tuple2('ちゃっこう', '着工'),
			_Utils_Tuple2('ちゃのま', '茶の間'),
			_Utils_Tuple2('ちゃばしら', '茶柱'),
			_Utils_Tuple2('ちゃぶだい', 'ちゃぶ台'),
			_Utils_Tuple2('ちやほや', 'ちやほや'),
			_Utils_Tuple2('ちゃら', 'ちゃら'),
			_Utils_Tuple2('ちゃりてぃー', 'チャリティー'),
			_Utils_Tuple2('ちゃんぽん', 'ちゃんぽん'),
			_Utils_Tuple2('ちゅうおうぎんこう', '中央銀行'),
			_Utils_Tuple2('ちゅうかい', '仲介'),
			_Utils_Tuple2('ちゅうがえり', '宙返り'),
			_Utils_Tuple2('ちゅうかんしょく', '中間色'),
			_Utils_Tuple2('ちゅうかんほうこく', '中間報告'),
			_Utils_Tuple2('ちゅうけいほうそう', '中継放送'),
			_Utils_Tuple2('ちゅうけん', '中堅'),
			_Utils_Tuple2('ちゅうげん', '中元'),
			_Utils_Tuple2('ちゅうざい', '駐在'),
			_Utils_Tuple2('ちゅうざいしょ', '駐在所'),
			_Utils_Tuple2('ちゅうじつ', '忠実'),
			_Utils_Tuple2('ちゅうしゃく', '注釈'),
			_Utils_Tuple2('ちゅうしゅつ', '抽出'),
			_Utils_Tuple2('ちゅうしょう', '中傷'),
			_Utils_Tuple2('ちゅうしょうてき', '抽象的'),
			_Utils_Tuple2('ちゅうすう', '中枢'),
			_Utils_Tuple2('ちゅうせい', '中性'),
			_Utils_Tuple2('ちゅうせい', '忠誠'),
			_Utils_Tuple2('ちゅうせい', '中世'),
			_Utils_Tuple2('ちゅうぜつ', '中絶'),
			_Utils_Tuple2('ちゅうだん', '中断'),
			_Utils_Tuple2('ちゅうちょ', 'ちゅうちょ'),
			_Utils_Tuple2('ちゅうどく', '中毒'),
			_Utils_Tuple2('ちゅうとはんぱ', '中途半端'),
			_Utils_Tuple2('ちゅーなー', 'チューナー'),
			_Utils_Tuple2('ちゅうにゅう', '注入'),
			_Utils_Tuple2('ちゅーにんぐ', 'チューニング'),
			_Utils_Tuple2('ちゅうばん', '中盤'),
			_Utils_Tuple2('ちゅうぼう', 'ちゅう房'),
			_Utils_Tuple2('ちゅうりゃく', '中略'),
			_Utils_Tuple2('ちょ', '著'),
			_Utils_Tuple2('ちょいちょい', 'ちょいちょい'),
			_Utils_Tuple2('ちょう', '調'),
			_Utils_Tuple2('ちょう', '腸'),
			_Utils_Tuple2('ちょうい', '弔意'),
			_Utils_Tuple2('ちょういん', '調印'),
			_Utils_Tuple2('ちょうえき', '懲役'),
			_Utils_Tuple2('ちょうおんぱ', '超音波'),
			_Utils_Tuple2('ちょうかいめんしょく', '懲戒免職'),
			_Utils_Tuple2('ちょうかく', '聴覚'),
			_Utils_Tuple2('ちょうきょう', '調教'),
			_Utils_Tuple2('ちょうこう', '兆候'),
			_Utils_Tuple2('ちょうごう', '調合'),
			_Utils_Tuple2('ちょうし', 'ちょうし'),
			_Utils_Tuple2('ちょうしゃ', '庁舎'),
			_Utils_Tuple2('ちょうじゃ', '長者'),
			_Utils_Tuple2('ちょうしゅ', '聴取'),
			_Utils_Tuple2('ちょうしゅう', '徴収'),
			_Utils_Tuple2('ちょうしゅう', '聴衆'),
			_Utils_Tuple2('ちょうじん', '超人'),
			_Utils_Tuple2('ちょうそん', '町村'),
			_Utils_Tuple2('ちょうだい', '長大'),
			_Utils_Tuple2('ちょうたつ', '調達'),
			_Utils_Tuple2('ちょうちょう', '長調'),
			_Utils_Tuple2('ちょうちん', 'ちょうちん'),
			_Utils_Tuple2('ちょうてい', '調停'),
			_Utils_Tuple2('ちょうにん', '町人'),
			_Utils_Tuple2('ちょうねくたい', 'ちょうネクタイ'),
			_Utils_Tuple2('ちょうのうりょく', '超能力'),
			_Utils_Tuple2('ちょうはつ', '挑発'),
			_Utils_Tuple2('ちょうばつ', '懲罰'),
			_Utils_Tuple2('ちょうふく', '重複'),
			_Utils_Tuple2('ちょうへい', '徴兵'),
			_Utils_Tuple2('ちょうへん', '長編'),
			_Utils_Tuple2('ちょうほう', '重宝'),
			_Utils_Tuple2('ちょうむすび', 'ちょう結び'),
			_Utils_Tuple2('ちょうりつ', '調律'),
			_Utils_Tuple2('ちょうわ', '調和'),
			_Utils_Tuple2('ちょく', '直'),
			_Utils_Tuple2('ちょくえい', '直営'),
			_Utils_Tuple2('ちょくげき', '直撃'),
			_Utils_Tuple2('ちょくし', '直視'),
			_Utils_Tuple2('ちょくしゃ', '直射'),
			_Utils_Tuple2('ちょくせつぜい', '直接税'),
			_Utils_Tuple2('ちょくぞく', '直属'),
			_Utils_Tuple2('ちょくちょく', 'ちょくちょく'),
			_Utils_Tuple2('ちょくやく', '直訳'),
			_Utils_Tuple2('ちょくりゅう', '直流'),
			_Utils_Tuple2('ちょくれつ', '直列'),
			_Utils_Tuple2('ちょこちょこ', 'ちょこちょこ'),
			_Utils_Tuple2('ちょさく', '著作'),
			_Utils_Tuple2('ちょさくけん', '著作権'),
			_Utils_Tuple2('ちょさくぶつ', '著作物'),
			_Utils_Tuple2('ちょすい', '貯水'),
			_Utils_Tuple2('ちょぞう', '貯蔵'),
			_Utils_Tuple2('ちょちく', '貯蓄'),
			_Utils_Tuple2('ちょっか', '直下'),
			_Utils_Tuple2('ちょっかん', '直感'),
			_Utils_Tuple2('ちょっかん', '直観'),
			_Utils_Tuple2('ちょっかんてき', '直観的'),
			_Utils_Tuple2('ちょっきゅう', '直球'),
			_Utils_Tuple2('ちょっけい', '直径'),
			_Utils_Tuple2('ちょっけつ', '直結'),
			_Utils_Tuple2('ちょっぷ', 'チョップ'),
			_Utils_Tuple2('ちょめい', '著名'),
			_Utils_Tuple2('ちょろい', 'ちょろい'),
			_Utils_Tuple2('ちょろちょろ', 'ちょろちょろ'),
			_Utils_Tuple2('ちょんまげ', 'ちょんまげ'),
			_Utils_Tuple2('ちらし', '散らし'),
			_Utils_Tuple2('ちらす', '散らす'),
			_Utils_Tuple2('ちらちら', 'ちらちら'),
			_Utils_Tuple2('ちらつく', 'ちらつく'),
			_Utils_Tuple2('ちらばる', '散らばる'),
			_Utils_Tuple2('ちらほら', 'ちらほら'),
			_Utils_Tuple2('ちり', 'ちり'),
			_Utils_Tuple2('ちりちり', 'ちりちり'),
			_Utils_Tuple2('ちりめん', 'ちりめん'),
			_Utils_Tuple2('ちりょく', '知力'),
			_Utils_Tuple2('ちるど', 'チルド'),
			_Utils_Tuple2('ちん', '珍'),
			_Utils_Tuple2('ちんぎん', '賃金'),
			_Utils_Tuple2('ちんげんさい', 'チンゲン菜'),
			_Utils_Tuple2('ちんこ', 'ちんこ'),
			_Utils_Tuple2('ちんじゅつ', '陳述'),
			_Utils_Tuple2('ちんたい', '沈滞'),
			_Utils_Tuple2('ちんちゃく', '沈着'),
			_Utils_Tuple2('ちんちょう', '珍重'),
			_Utils_Tuple2('ちんつう', '鎮痛'),
			_Utils_Tuple2('ちんぴら', 'ちんぴら'),
			_Utils_Tuple2('ちんぷんかんぷん', 'ちんぷんかんぷん'),
			_Utils_Tuple2('ちんぼつ', '沈没'),
			_Utils_Tuple2('ちんみ', '珍味'),
			_Utils_Tuple2('ちんもく', '沈黙'),
			_Utils_Tuple2('ちんれつ', '陳列'),
			_Utils_Tuple2('つい', '追'),
			_Utils_Tuple2('ついきゅう', '追及'),
			_Utils_Tuple2('ついきゅう', '追求'),
			_Utils_Tuple2('ついせき', '追跡'),
			_Utils_Tuple2('ついせきちょうさ', '追跡調査'),
			_Utils_Tuple2('ついとう', '追悼'),
			_Utils_Tuple2('ついとつ', '追突'),
			_Utils_Tuple2('ついほう', '追放'),
			_Utils_Tuple2('ついやす', '費やす'),
			_Utils_Tuple2('ついらく', '墜落'),
			_Utils_Tuple2('つう', '通'),
			_Utils_Tuple2('つうかん', '痛感'),
			_Utils_Tuple2('つうこく', '通告'),
			_Utils_Tuple2('つうさん', '通算'),
			_Utils_Tuple2('つうしょう', '通称'),
			_Utils_Tuple2('つうしんえいせい', '通信衛星'),
			_Utils_Tuple2('つうしんしゃ', '通信社'),
			_Utils_Tuple2('つうしんせい', '通信制'),
			_Utils_Tuple2('つうしんぼ', '通信簿'),
			_Utils_Tuple2('つうしんもう', '通信網'),
			_Utils_Tuple2('つうずる', '通ずる'),
			_Utils_Tuple2('つうせつ', '痛切'),
			_Utils_Tuple2('つうたつ', '通達'),
			_Utils_Tuple2('つうちひょう', '通知表'),
			_Utils_Tuple2('つうちぼ', '通知簿'),
			_Utils_Tuple2('つーりんぐ', 'ツーリング'),
			_Utils_Tuple2('つーる', 'ツール'),
			_Utils_Tuple2('つえ', 'つえ'),
			_Utils_Tuple2('つかいこむ', '使い込む'),
			_Utils_Tuple2('つかいもの', '使い物'),
			_Utils_Tuple2('つかえる', '仕える'),
			_Utils_Tuple2('つかさどる', 'つかさどる'),
			_Utils_Tuple2('つかのま', 'つかの間'),
			_Utils_Tuple2('つかる', '浸かる'),
			_Utils_Tuple2('つかる', '漬かる'),
			_Utils_Tuple2('つき', 'つき'),
			_Utils_Tuple2('つぎ', '継ぎ'),
			_Utils_Tuple2('つきあわせる', '突き合わせる'),
			_Utils_Tuple2('つきそう', '付き添う'),
			_Utils_Tuple2('つきだす', '突き出す'),
			_Utils_Tuple2('つきっきり', '付きっ切り'),
			_Utils_Tuple2('つきつめる', '突き詰める'),
			_Utils_Tuple2('つきでる', '突き出る'),
			_Utils_Tuple2('つきとめる', '突き止める'),
			_Utils_Tuple2('つきぬける', '突き抜ける'),
			_Utils_Tuple2('つきびと', '付き人'),
			_Utils_Tuple2('つきやぶる', '突き破る'),
			_Utils_Tuple2('つきる', '尽きる'),
			_Utils_Tuple2('つぐ', '継ぐ'),
			_Utils_Tuple2('つくす', '尽くす'),
			_Utils_Tuple2('つくだに', 'つくだ煮'),
			_Utils_Tuple2('つくづく', 'つくづく'),
			_Utils_Tuple2('つぐない', '償い'),
			_Utils_Tuple2('つぐなう', '償う'),
			_Utils_Tuple2('つくり', '作り'),
			_Utils_Tuple2('つくりもの', '作り物'),
			_Utils_Tuple2('つくろう', '繕う'),
			_Utils_Tuple2('つげ', '告げ'),
			_Utils_Tuple2('づけ', '漬け'),
			_Utils_Tuple2('つけあわせ', '付け合わせ'),
			_Utils_Tuple2('つけこむ', '漬け込む'),
			_Utils_Tuple2('つける', 'つける'),
			_Utils_Tuple2('つげる', '告げる'),
			_Utils_Tuple2('っこ', 'っこ'),
			_Utils_Tuple2('つたう', '伝う'),
			_Utils_Tuple2('つたえきく', '伝え聞く'),
			_Utils_Tuple2('つつ', '筒'),
			_Utils_Tuple2('つっかかる', '突っ掛かる'),
			_Utils_Tuple2('つっこみ', '突っ込み'),
			_Utils_Tuple2('つつしむ', '慎む'),
			_Utils_Tuple2('つつぬけ', '筒抜け'),
			_Utils_Tuple2('つっぱしる', '突っ走る'),
			_Utils_Tuple2('つっぱる', '突っ張る'),
			_Utils_Tuple2('つづり', 'つづり'),
			_Utils_Tuple2('つづる', 'つづる'),
			_Utils_Tuple2('つて', 'つて'),
			_Utils_Tuple2('つど', '都度'),
			_Utils_Tuple2('つどい', '集い'),
			_Utils_Tuple2('つどう', '集う'),
			_Utils_Tuple2('つとまる', '務まる'),
			_Utils_Tuple2('つとめにん', '勤め人'),
			_Utils_Tuple2('つな', '綱'),
			_Utils_Tuple2('つなぎ', 'つなぎ'),
			_Utils_Tuple2('つね', '常'),
			_Utils_Tuple2('つねづね', '常々'),
			_Utils_Tuple2('つの', '角'),
			_Utils_Tuple2('つのる', '募る'),
			_Utils_Tuple2('つば', '唾'),
			_Utils_Tuple2('つばさ', '翼'),
			_Utils_Tuple2('つばめ', 'つばめ'),
			_Utils_Tuple2('つぶ', '粒'),
			_Utils_Tuple2('つぶ', '粒'),
			_Utils_Tuple2('つぶさに', 'つぶさに'),
			_Utils_Tuple2('つぶし', '潰し'),
			_Utils_Tuple2('つぶす', '潰す'),
			_Utils_Tuple2('つぶつぶ', '粒々'),
			_Utils_Tuple2('つぶやく', 'つぶやく'),
			_Utils_Tuple2('つぶる', 'つぶる'),
			_Utils_Tuple2('つぼ', 'つぼ'),
			_Utils_Tuple2('つぼ', '坪'),
			_Utils_Tuple2('つぼ', '坪'),
			_Utils_Tuple2('つま', 'つま'),
			_Utils_Tuple2('つまむ', 'つまむ'),
			_Utils_Tuple2('つまり', '詰まり'),
			_Utils_Tuple2('つみき', '積み木'),
			_Utils_Tuple2('つみたてる', '積み立てる'),
			_Utils_Tuple2('つむ', '摘む'),
			_Utils_Tuple2('つめ', '詰め'),
			_Utils_Tuple2('づめ', '詰め'),
			_Utils_Tuple2('つめかける', '詰め掛ける'),
			_Utils_Tuple2('つや', '通夜'),
			_Utils_Tuple2('つや', '艶'),
			_Utils_Tuple2('つやつや', 'つやつや'),
			_Utils_Tuple2('つら', '面'),
			_Utils_Tuple2('つらなる', '連なる'),
			_Utils_Tuple2('つらぬきとおす', '貫き通す'),
			_Utils_Tuple2('つらぬく', '貫く'),
			_Utils_Tuple2('つらねる', '連ねる'),
			_Utils_Tuple2('つりあげる', 'つり上げる'),
			_Utils_Tuple2('つりさげる', 'つり下げる'),
			_Utils_Tuple2('つりせん', '釣り銭'),
			_Utils_Tuple2('つる', 'つる'),
			_Utils_Tuple2('つる', '鶴'),
			_Utils_Tuple2('つる', 'つる'),
			_Utils_Tuple2('つるす', 'つるす'),
			_Utils_Tuple2('つれこ', '連れ子'),
			_Utils_Tuple2('つわり', 'つわり'),
			_Utils_Tuple2('つんと', 'つんと'),
			_Utils_Tuple2('つんのめる', 'つんのめる'),
			_Utils_Tuple2('て', '手'),
			_Utils_Tuple2('てあたり', '手当たり'),
			_Utils_Tuple2('てあて', '手当て'),
			_Utils_Tuple2('てい', '弟'),
			_Utils_Tuple2('でぃーぜる', 'ディーゼル'),
			_Utils_Tuple2('でぃーぜるえんじん', 'ディーゼルエンジン'),
			_Utils_Tuple2('でぃーぜるしゃ', 'ディーゼル車'),
			_Utils_Tuple2('てぃーぴーおー', 'ＴＰＯ'),
			_Utils_Tuple2('でぃーらー', 'ディーラー'),
			_Utils_Tuple2('ていき', '提起'),
			_Utils_Tuple2('ていぎ', '定義'),
			_Utils_Tuple2('ていきよきん', '定期預金'),
			_Utils_Tuple2('ていきんり', '低金利'),
			_Utils_Tuple2('ていけい', '提携'),
			_Utils_Tuple2('ていけつ', '締結'),
			_Utils_Tuple2('ていけつあつ', '低血圧'),
			_Utils_Tuple2('ていげん', '提言'),
			_Utils_Tuple2('ていこく', '帝国'),
			_Utils_Tuple2('ていさい', '体裁'),
			_Utils_Tuple2('ていじ', '呈示'),
			_Utils_Tuple2('ていしせい', '低姿勢'),
			_Utils_Tuple2('ていじせい', '定時制'),
			_Utils_Tuple2('ていしゅかんぱく', '亭主関白'),
			_Utils_Tuple2('ていしょう', '提唱'),
			_Utils_Tuple2('でいすい', '泥水'),
			_Utils_Tuple2('ていすう', '定数'),
			_Utils_Tuple2('でぃすこ', 'ディスコ'),
			_Utils_Tuple2('でぃすぷれー', 'ディスプレー'),
			_Utils_Tuple2('ていせい', '訂正'),
			_Utils_Tuple2('ていせつ', '定説'),
			_Utils_Tuple2('ていそ', '提訴'),
			_Utils_Tuple2('ていぞく', '低俗'),
			_Utils_Tuple2('ていたく', '邸宅'),
			_Utils_Tuple2('ていちゃく', '定着'),
			_Utils_Tuple2('ていちょう', '丁重'),
			_Utils_Tuple2('でぃてーる', 'ディテール'),
			_Utils_Tuple2('ていねんせい', '定年制'),
			_Utils_Tuple2('ていはく', '停泊'),
			_Utils_Tuple2('ていばん', '定番'),
			_Utils_Tuple2('ていひょう', '定評'),
			_Utils_Tuple2('でぃふぇんす', 'ディフェンス'),
			_Utils_Tuple2('ていへん', '底辺'),
			_Utils_Tuple2('ていぼう', '堤防'),
			_Utils_Tuple2('ていめい', '低迷'),
			_Utils_Tuple2('ていれい', '定例'),
			_Utils_Tuple2('てうち', '手打ち'),
			_Utils_Tuple2('でーたつうしん', 'データ通信'),
			_Utils_Tuple2('てーぴんぐ', 'テーピング'),
			_Utils_Tuple2('でーもん', 'デーモン'),
			_Utils_Tuple2('ておくれ', '手遅れ'),
			_Utils_Tuple2('ておし', '手押し'),
			_Utils_Tuple2('てがかり', '手掛かり'),
			_Utils_Tuple2('てがける', '手掛ける'),
			_Utils_Tuple2('でかせぎ', '出稼ぎ'),
			_Utils_Tuple2('でかでか', 'でかでか'),
			_Utils_Tuple2('てがら', '手柄'),
			_Utils_Tuple2('てき', '滴'),
			_Utils_Tuple2('てきい', '敵意'),
			_Utils_Tuple2('てきかく', '適格'),
			_Utils_Tuple2('てきぎ', '適宜'),
			_Utils_Tuple2('てきごう', '適合'),
			_Utils_Tuple2('てきざい', '適材'),
			_Utils_Tuple2('てきし', '敵視'),
			_Utils_Tuple2('てきしょ', '適所'),
			_Utils_Tuple2('てきせい', '適性'),
			_Utils_Tuple2('てきせい', '適正'),
			_Utils_Tuple2('てきせいけんさ', '適性検査'),
			_Utils_Tuple2('できだか', '出来高'),
			_Utils_Tuple2('てきち', '敵地'),
			_Utils_Tuple2('てきはつ', '摘発'),
			_Utils_Tuple2('てきれいき', '適齢期'),
			_Utils_Tuple2('てぐち', '手口'),
			_Utils_Tuple2('でこぼこ', '凸凹'),
			_Utils_Tuple2('てごろ', '手頃'),
			_Utils_Tuple2('てごわい', '手ごわい'),
			_Utils_Tuple2('てさき', '手先'),
			_Utils_Tuple2('でさき', '出先'),
			_Utils_Tuple2('てさぐり', '手探り'),
			_Utils_Tuple2('てさげ', '手提げ'),
			_Utils_Tuple2('てざわり', '手触り'),
			_Utils_Tuple2('でし', '弟子'),
			_Utils_Tuple2('でじたるしんごう', 'デジタル信号'),
			_Utils_Tuple2('でしゃばる', '出しゃばる'),
			_Utils_Tuple2('てじゅん', '手順'),
			_Utils_Tuple2('てじょう', '手錠'),
			_Utils_Tuple2('てすう', '手数'),
			_Utils_Tuple2('てそう', '手相'),
			_Utils_Tuple2('てだすけ', '手助け'),
			_Utils_Tuple2('てだて', '手立て'),
			_Utils_Tuple2('でたらめ', 'でたらめ'),
			_Utils_Tuple2('てぢか', '手近'),
			_Utils_Tuple2('てちがい', '手違い'),
			_Utils_Tuple2('てっかい', '撤回'),
			_Utils_Tuple2('てつがくてき', '哲学的'),
			_Utils_Tuple2('てつき', '手付き'),
			_Utils_Tuple2('でっき', 'デッキ'),
			_Utils_Tuple2('てっきょ', '撤去'),
			_Utils_Tuple2('てっきり', 'てっきり'),
			_Utils_Tuple2('てつけ', '手付け'),
			_Utils_Tuple2('てつけきん', '手付け金'),
			_Utils_Tuple2('てっけん', '鉄拳'),
			_Utils_Tuple2('てっこう', '鉄鋼'),
			_Utils_Tuple2('てっこつ', '鉄骨'),
			_Utils_Tuple2('でっさん', 'デッサン'),
			_Utils_Tuple2('てつじん', '鉄人'),
			_Utils_Tuple2('てっする', '徹する'),
			_Utils_Tuple2('てっそく', '鉄則'),
			_Utils_Tuple2('てったい', '撤退'),
			_Utils_Tuple2('てってい', '徹底'),
			_Utils_Tuple2('てっていてき', '徹底的'),
			_Utils_Tuple2('でっどひーと', 'デッドヒート'),
			_Utils_Tuple2('てっぱい', '撤廃'),
			_Utils_Tuple2('でっぱる', '出っ張る'),
			_Utils_Tuple2('てつぶん', '鉄分'),
			_Utils_Tuple2('てっぺん', 'てっぺん'),
			_Utils_Tuple2('てつぼう', '鉄棒'),
			_Utils_Tuple2('てっぽう', '鉄砲'),
			_Utils_Tuple2('てつわん', '鉄腕'),
			_Utils_Tuple2('てどり', '手取り'),
			_Utils_Tuple2('てなおし', '手直し'),
			_Utils_Tuple2('でなおす', '出直す'),
			_Utils_Tuple2('てならい', '手習い'),
			_Utils_Tuple2('てなんと', 'テナント'),
			_Utils_Tuple2('てのこう', '手の甲'),
			_Utils_Tuple2('てば', '手羽'),
			_Utils_Tuple2('てばやい', '手早い'),
			_Utils_Tuple2('でびる', 'デビル'),
			_Utils_Tuple2('でふれ', 'デフレ'),
			_Utils_Tuple2('てふろん', 'テフロン'),
			_Utils_Tuple2('てほん', '手本'),
			_Utils_Tuple2('でま', 'デマ'),
			_Utils_Tuple2('でまど', '出窓'),
			_Utils_Tuple2('てまどる', '手間取る'),
			_Utils_Tuple2('でむく', '出向く'),
			_Utils_Tuple2('てもち', '手持ち'),
			_Utils_Tuple2('でゅえっと', 'デュエット'),
			_Utils_Tuple2('てらこや', '寺子屋'),
			_Utils_Tuple2('てらしあわせる', '照らし合わせる'),
			_Utils_Tuple2('てらす', 'テラス'),
			_Utils_Tuple2('てり', '照り'),
			_Utils_Tuple2('でりけーと', 'デリケート'),
			_Utils_Tuple2('てるてるぼうず', 'てるてる坊主'),
			_Utils_Tuple2('てれびじょん', 'テレビジョン'),
			_Utils_Tuple2('でん', '田'),
			_Utils_Tuple2('でん', '殿'),
			_Utils_Tuple2('でんえん', '田園'),
			_Utils_Tuple2('てんか', '添加'),
			_Utils_Tuple2('てんか', '天下'),
			_Utils_Tuple2('てんか', '転嫁'),
			_Utils_Tuple2('でんがく', '田楽'),
			_Utils_Tuple2('てんかん', '転換'),
			_Utils_Tuple2('てんかんき', '転換期'),
			_Utils_Tuple2('てんき', '転機'),
			_Utils_Tuple2('でんき', '伝記'),
			_Utils_Tuple2('でんきょく', '電極'),
			_Utils_Tuple2('てんぐ', 'てんぐ'),
			_Utils_Tuple2('てんくう', '天空'),
			_Utils_Tuple2('でんげき', '電撃'),
			_Utils_Tuple2('てんこう', '転向'),
			_Utils_Tuple2('てんさい', '天災'),
			_Utils_Tuple2('てんさく', '添削'),
			_Utils_Tuple2('てんじ', '点字'),
			_Utils_Tuple2('でんしけんびきょう', '電子顕微鏡'),
			_Utils_Tuple2('でんじは', '電磁波'),
			_Utils_Tuple2('てんしゃ', '転写'),
			_Utils_Tuple2('でんじゅ', '伝授'),
			_Utils_Tuple2('てんしゅかく', '天守閣'),
			_Utils_Tuple2('てんしゅつ', '転出'),
			_Utils_Tuple2('てんじょう', '添乗'),
			_Utils_Tuple2('でんしょう', '伝承'),
			_Utils_Tuple2('てんじょういん', '添乗員'),
			_Utils_Tuple2('てんしょく', '天職'),
			_Utils_Tuple2('てんしょん', 'テンション'),
			_Utils_Tuple2('てんしん', '転身'),
			_Utils_Tuple2('でんしん', '電信'),
			_Utils_Tuple2('てんずる', '転ずる'),
			_Utils_Tuple2('てんせい', '天性'),
			_Utils_Tuple2('てんたい', '天体'),
			_Utils_Tuple2('てんち', '天地'),
			_Utils_Tuple2('てんで', 'てんで'),
			_Utils_Tuple2('てんてき', '天敵'),
			_Utils_Tuple2('でんてつ', '電鉄'),
			_Utils_Tuple2('てんてん', '転々'),
			_Utils_Tuple2('でんと', 'でんと'),
			_Utils_Tuple2('でんどう', '伝導'),
			_Utils_Tuple2('てんのうせい', '天皇制'),
			_Utils_Tuple2('てんのうへいか', '天皇陛下'),
			_Utils_Tuple2('てんばい', '転売'),
			_Utils_Tuple2('てんばつ', '天罰'),
			_Utils_Tuple2('てんびん', '天びん'),
			_Utils_Tuple2('てんぷ', '添付'),
			_Utils_Tuple2('てんぽ', '店舗'),
			_Utils_Tuple2('てんぼう', '展望'),
			_Utils_Tuple2('てんめつ', '点滅'),
			_Utils_Tuple2('てんもん', '天文'),
			_Utils_Tuple2('てんもんがく', '天文学'),
			_Utils_Tuple2('てんよう', '転用'),
			_Utils_Tuple2('でんらい', '伝来'),
			_Utils_Tuple2('てんらん', '展覧'),
			_Utils_Tuple2('と', '斗'),
			_Utils_Tuple2('どあい', '度合い'),
			_Utils_Tuple2('とある', 'とある'),
			_Utils_Tuple2('といかえす', '問い返す'),
			_Utils_Tuple2('といかける', '問い掛ける'),
			_Utils_Tuple2('といき', '吐息'),
			_Utils_Tuple2('とう', '党'),
			_Utils_Tuple2('とう', '党'),
			_Utils_Tuple2('とう', '塔'),
			_Utils_Tuple2('とう', '糖'),
			_Utils_Tuple2('とう', '筒'),
			_Utils_Tuple2('とう', '当'),
			_Utils_Tuple2('とう', '党'),
			_Utils_Tuple2('とう', '刀'),
			_Utils_Tuple2('どういん', '動員'),
			_Utils_Tuple2('とうえい', '投影'),
			_Utils_Tuple2('とうか', '投下'),
			_Utils_Tuple2('どうか', '同化'),
			_Utils_Tuple2('どうか', '銅貨'),
			_Utils_Tuple2('とうかい', '倒壊'),
			_Utils_Tuple2('とうかい', '東海'),
			_Utils_Tuple2('とうかいどう', '東海道'),
			_Utils_Tuple2('とうかん', '投函'),
			_Utils_Tuple2('どうがん', '童顔'),
			_Utils_Tuple2('とうき', '登記'),
			_Utils_Tuple2('とうき', '陶器'),
			_Utils_Tuple2('とうき', '投棄'),
			_Utils_Tuple2('とうぎ', '討議'),
			_Utils_Tuple2('とうきゅう', '投球'),
			_Utils_Tuple2('とうきゅう', '等級'),
			_Utils_Tuple2('とうきょく', '当局'),
			_Utils_Tuple2('どうくつ', '洞窟'),
			_Utils_Tuple2('とうげ', '峠'),
			_Utils_Tuple2('とうげい', '陶芸'),
			_Utils_Tuple2('とうけつ', '凍結'),
			_Utils_Tuple2('とうこう', '投稿'),
			_Utils_Tuple2('とうごう', '統合'),
			_Utils_Tuple2('どうこう', '動向'),
			_Utils_Tuple2('どうこう', '瞳孔'),
			_Utils_Tuple2('とうさい', '搭載'),
			_Utils_Tuple2('とうさく', '盗作'),
			_Utils_Tuple2('とうし', '投資'),
			_Utils_Tuple2('とうし', '透視'),
			_Utils_Tuple2('どうし', '同志'),
			_Utils_Tuple2('とうじき', '陶磁器'),
			_Utils_Tuple2('とうじしゃ', '当事者'),
			_Utils_Tuple2('とうしゅ', '党首'),
			_Utils_Tuple2('とうしゅ', '投手'),
			_Utils_Tuple2('とうしゅう', '踏襲'),
			_Utils_Tuple2('とうしょ', '投書'),
			_Utils_Tuple2('どうじょう', '道場'),
			_Utils_Tuple2('とうしん', '等身'),
			_Utils_Tuple2('とうずる', '投ずる'),
			_Utils_Tuple2('どうずる', '動ずる'),
			_Utils_Tuple2('とうせい', '統制'),
			_Utils_Tuple2('どうせい', '同棲'),
			_Utils_Tuple2('どうせいあい', '同性愛'),
			_Utils_Tuple2('とうせき', '投石'),
			_Utils_Tuple2('とうそう', '闘争'),
			_Utils_Tuple2('どうそう', '同窓'),
			_Utils_Tuple2('どうぞう', '銅像'),
			_Utils_Tuple2('とうぞく', '盗賊'),
			_Utils_Tuple2('どうぞく', '同族'),
			_Utils_Tuple2('とうたつ', '到達'),
			_Utils_Tuple2('とうち', '当地'),
			_Utils_Tuple2('とうち', '統治'),
			_Utils_Tuple2('とうちょう', '登頂'),
			_Utils_Tuple2('とうちょう', '盗聴'),
			_Utils_Tuple2('どうちょう', '同調'),
			_Utils_Tuple2('とうてい', '到底'),
			_Utils_Tuple2('とうとい', '尊い'),
			_Utils_Tuple2('とうとう', '等々'),
			_Utils_Tuple2('とうとぶ', '尊ぶ'),
			_Utils_Tuple2('どうなが', '胴長'),
			_Utils_Tuple2('どうにも', 'どうにも'),
			_Utils_Tuple2('とうにゅう', '投入'),
			_Utils_Tuple2('とうにょう', '糖尿'),
			_Utils_Tuple2('とうにょうびょう', '糖尿病'),
			_Utils_Tuple2('とうにん', '当人'),
			_Utils_Tuple2('とうはつ', '頭髪'),
			_Utils_Tuple2('どうはん', '同伴'),
			_Utils_Tuple2('とうひ', '逃避'),
			_Utils_Tuple2('とうぶん', '糖分'),
			_Utils_Tuple2('とうべん', '答弁'),
			_Utils_Tuple2('とうほう', '当方'),
			_Utils_Tuple2('とうぼう', '逃亡'),
			_Utils_Tuple2('どうみゃく', '動脈'),
			_Utils_Tuple2('どうみゃくこうか', '動脈硬化'),
			_Utils_Tuple2('どうめい', '同盟'),
			_Utils_Tuple2('とうめん', '当面'),
			_Utils_Tuple2('どうやら', 'どうやら'),
			_Utils_Tuple2('とうゆ', '灯油'),
			_Utils_Tuple2('どうよう', '童謡'),
			_Utils_Tuple2('どうよう', '動揺'),
			_Utils_Tuple2('とうらい', '到来'),
			_Utils_Tuple2('どうりょく', '動力'),
			_Utils_Tuple2('とうるい', '盗塁'),
			_Utils_Tuple2('とうろくしょうひょう', '登録商標'),
			_Utils_Tuple2('とうわく', '当惑'),
			_Utils_Tuple2('とおざかる', '遠ざかる'),
			_Utils_Tuple2('とおざける', '遠ざける'),
			_Utils_Tuple2('とおで', '遠出'),
			_Utils_Tuple2('どーぴんぐ', 'ドーピング'),
			_Utils_Tuple2('とおぼえ', '遠吠え'),
			_Utils_Tuple2('とおまわし', '遠回し'),
			_Utils_Tuple2('とおめ', '遠目'),
			_Utils_Tuple2('とおりがかり', '通り掛かり'),
			_Utils_Tuple2('とおりこす', '通り越す'),
			_Utils_Tuple2('とーん', 'トーン'),
			_Utils_Tuple2('とかげ', 'トカゲ'),
			_Utils_Tuple2('とがめる', 'とがめる'),
			_Utils_Tuple2('とがる', 'とがる'),
			_Utils_Tuple2('どき', '土器'),
			_Utils_Tuple2('ときおり', '時折'),
			_Utils_Tuple2('ときたまご', '溶き卵'),
			_Utils_Tuple2('どぎつい', 'どぎつい'),
			_Utils_Tuple2('ときはなす', '解き放す'),
			_Utils_Tuple2('どぎまぎ', 'どぎまぎ'),
			_Utils_Tuple2('ときめき', 'ときめき'),
			_Utils_Tuple2('ときめく', 'ときめく'),
			_Utils_Tuple2('どきょう', '度胸'),
			_Utils_Tuple2('ときょうそう', '徒競走'),
			_Utils_Tuple2('とぎれる', '途切れる'),
			_Utils_Tuple2('とく', '説く'),
			_Utils_Tuple2('とく', '徳'),
			_Utils_Tuple2('とぐ', '研ぐ'),
			_Utils_Tuple2('とくい', '特異'),
			_Utils_Tuple2('どくがす', '毒ガス'),
			_Utils_Tuple2('どくさい', '独裁'),
			_Utils_Tuple2('とくさく', '得策'),
			_Utils_Tuple2('とくさつ', '特撮'),
			_Utils_Tuple2('どくじ', '独自'),
			_Utils_Tuple2('とくしゅ', '特殊'),
			_Utils_Tuple2('とくしゅ', '特殊'),
			_Utils_Tuple2('とくしゅせい', '特殊性'),
			_Utils_Tuple2('とくしゅほうじん', '特殊法人'),
			_Utils_Tuple2('どくせい', '毒性'),
			_Utils_Tuple2('どくぜつ', '毒舌'),
			_Utils_Tuple2('どくせん', '独占'),
			_Utils_Tuple2('どくそう', '独走'),
			_Utils_Tuple2('どくそう', '独創'),
			_Utils_Tuple2('どくそうてき', '独創的'),
			_Utils_Tuple2('どくだん', '独断'),
			_Utils_Tuple2('とぐち', '戸口'),
			_Utils_Tuple2('とくちゅう', '特注'),
			_Utils_Tuple2('とくてん', '特典'),
			_Utils_Tuple2('とくはいん', '特派員'),
			_Utils_Tuple2('とくひつ', '特筆'),
			_Utils_Tuple2('とくほん', '読本'),
			_Utils_Tuple2('とくめい', '匿名'),
			_Utils_Tuple2('とくやく', '特約'),
			_Utils_Tuple2('とくゆう', '特有'),
			_Utils_Tuple2('どくりつこく', '独立国'),
			_Utils_Tuple2('とげ', 'とげ'),
			_Utils_Tuple2('とけあう', '溶け合う'),
			_Utils_Tuple2('とけこむ', '溶け込む'),
			_Utils_Tuple2('とげる', '遂げる'),
			_Utils_Tuple2('どける', 'どける'),
			_Utils_Tuple2('とこう', '渡航'),
			_Utils_Tuple2('どこそこ', 'どこそこ'),
			_Utils_Tuple2('とことん', 'とことん'),
			_Utils_Tuple2('とこのま', '床の間'),
			_Utils_Tuple2('ところてん', 'ところてん'),
			_Utils_Tuple2('どさくさ', 'どさくさ'),
			_Utils_Tuple2('とざす', '閉ざす'),
			_Utils_Tuple2('どじ', 'どじ'),
			_Utils_Tuple2('としおいる', '年老いる'),
			_Utils_Tuple2('としご', '年子'),
			_Utils_Tuple2('としこっか', '都市国家'),
			_Utils_Tuple2('としごろ', '年頃'),
			_Utils_Tuple2('どしどし', 'どしどし'),
			_Utils_Tuple2('どしゃ', '土砂'),
			_Utils_Tuple2('とじょう', '途上'),
			_Utils_Tuple2('どじょう', '土壌'),
			_Utils_Tuple2('とじょうこく', '途上国'),
			_Utils_Tuple2('どすう', '度数'),
			_Utils_Tuple2('どせい', '土星'),
			_Utils_Tuple2('とそう', '塗装'),
			_Utils_Tuple2('どだい', '土台'),
			_Utils_Tuple2('とだえる', '途絶える'),
			_Utils_Tuple2('どたばた', 'どたばた'),
			_Utils_Tuple2('とたんに', '途端に'),
			_Utils_Tuple2('とちがら', '土地柄'),
			_Utils_Tuple2('とっか', '特価'),
			_Utils_Tuple2('とっき', '突起'),
			_Utils_Tuple2('とっきゅう', '特級'),
			_Utils_Tuple2('とっきょ', '特許'),
			_Utils_Tuple2('どっきんぐ', 'ドッキング'),
			_Utils_Tuple2('とつぐ', '嫁ぐ'),
			_Utils_Tuple2('どっく', 'ドック'),
			_Utils_Tuple2('とっくり', '徳利'),
			_Utils_Tuple2('とっけん', '特権'),
			_Utils_Tuple2('どっこいしょ', 'どっこいしょ'),
			_Utils_Tuple2('とっこう', '特攻'),
			_Utils_Tuple2('とっこう', '特効'),
			_Utils_Tuple2('とっこうやく', '特効薬'),
			_Utils_Tuple2('とっさ', 'とっさ'),
			_Utils_Tuple2('どっさり', 'どっさり'),
			_Utils_Tuple2('とつじょ', '突如'),
			_Utils_Tuple2('どっちみち', 'どっちみち'),
			_Utils_Tuple2('とっちめる', 'とっちめる'),
			_Utils_Tuple2('とっておき', '取って置き'),
			_Utils_Tuple2('とっとと', 'とっとと'),
			_Utils_Tuple2('とつにゅう', '突入'),
			_Utils_Tuple2('とっぱ', '突破'),
			_Utils_Tuple2('とっぱつ', '突発'),
			_Utils_Tuple2('とっぱつてき', '突発的'),
			_Utils_Tuple2('とっぷう', '突風'),
			_Utils_Tuple2('どっぷり', 'どっぷり'),
			_Utils_Tuple2('とどこおる', '滞る'),
			_Utils_Tuple2('とどまる', 'とどまる'),
			_Utils_Tuple2('とどめる', 'とどめる'),
			_Utils_Tuple2('どなー', 'ドナー'),
			_Utils_Tuple2('となえる', '唱える'),
			_Utils_Tuple2('どなべ', '土鍋'),
			_Utils_Tuple2('となりあう', '隣り合う'),
			_Utils_Tuple2('との', '殿'),
			_Utils_Tuple2('とのさま', '殿様'),
			_Utils_Tuple2('とばく', '賭博'),
			_Utils_Tuple2('とびいり', '飛び入り'),
			_Utils_Tuple2('とびかう', '飛び交う'),
			_Utils_Tuple2('とびきり', '飛び切り'),
			_Utils_Tuple2('とびこえる', '飛び越える'),
			_Utils_Tuple2('とびこす', '飛び越す'),
			_Utils_Tuple2('とびたつ', '飛び立つ'),
			_Utils_Tuple2('とびちる', '飛び散る'),
			_Utils_Tuple2('とびつく', '飛び付く'),
			_Utils_Tuple2('どひょう', '土俵'),
			_Utils_Tuple2('とほう', '途方'),
			_Utils_Tuple2('どぼく', '土木'),
			_Utils_Tuple2('とぼしい', '乏しい'),
			_Utils_Tuple2('とぼとぼ', 'とぼとぼ'),
			_Utils_Tuple2('とまどい', '戸惑い'),
			_Utils_Tuple2('とまどう', '戸惑う'),
			_Utils_Tuple2('とみ', '富'),
			_Utils_Tuple2('とむ', '富む'),
			_Utils_Tuple2('ともあれ', 'ともあれ'),
			_Utils_Tuple2('ともかく', 'ともかく'),
			_Utils_Tuple2('ともす', 'ともす'),
			_Utils_Tuple2('ともども', '共々'),
			_Utils_Tuple2('とやかく', 'とやかく'),
			_Utils_Tuple2('とらい', '渡来'),
			_Utils_Tuple2('どらいば', 'ドライバー'),
			_Utils_Tuple2('とらえる', '捉える'),
			_Utils_Tuple2('とらえる', '捕らえる'),
			_Utils_Tuple2('とらっく', 'トラック'),
			_Utils_Tuple2('とらっぷ', 'トラップ'),
			_Utils_Tuple2('どらふと', 'ドラフト'),
			_Utils_Tuple2('どらむかん', 'ドラム缶'),
			_Utils_Tuple2('どらむすこ', 'どら息子'),
			_Utils_Tuple2('どらやき', 'どら焼き'),
			_Utils_Tuple2('とらわれる', '捕らわれる'),
			_Utils_Tuple2('とらんじすた', 'トランジスタ'),
			_Utils_Tuple2('とりあつかい', '取り扱い'),
			_Utils_Tuple2('とりあつかう', '取り扱う'),
			_Utils_Tuple2('とりお', 'トリオ'),
			_Utils_Tuple2('とりおこなう', '執り行う'),
			_Utils_Tuple2('とりかえし', '取り返し'),
			_Utils_Tuple2('とりきめ', '取り決め'),
			_Utils_Tuple2('とりきめる', '取り決める'),
			_Utils_Tuple2('とりこ', 'とりこ'),
			_Utils_Tuple2('とりこわす', '取り壊す'),
			_Utils_Tuple2('とりさげる', '取り下げる'),
			_Utils_Tuple2('とりさる', '取り去る'),
			_Utils_Tuple2('とりしきる', '取り仕切る'),
			_Utils_Tuple2('とりしまり', '取り締まり'),
			_Utils_Tuple2('とりしまる', '取り締まる'),
			_Utils_Tuple2('とりしらべ', '取り調べ'),
			_Utils_Tuple2('とりそろえる', '取りそろえる'),
			_Utils_Tuple2('とりたて', '取り立て'),
			_Utils_Tuple2('とりちがえる', '取り違える'),
			_Utils_Tuple2('とりつく', '取り付く'),
			_Utils_Tuple2('とりつける', '取り付ける'),
			_Utils_Tuple2('とりにく', '鳥肉'),
			_Utils_Tuple2('とりのこす', '取り残す'),
			_Utils_Tuple2('とりのぞく', '取り除く'),
			_Utils_Tuple2('とりはからう', '取り計らう'),
			_Utils_Tuple2('とりはずす', '取り外す'),
			_Utils_Tuple2('とりはだ', '鳥肌'),
			_Utils_Tuple2('どりぶる', 'ドリブル'),
			_Utils_Tuple2('とりぶん', '取り分'),
			_Utils_Tuple2('とりまく', '取り巻く'),
			_Utils_Tuple2('とりやめる', '取りやめる'),
			_Utils_Tuple2('とりょう', '塗料'),
			_Utils_Tuple2('とりよせる', '取り寄せる'),
			_Utils_Tuple2('どりる', 'ドリル'),
			_Utils_Tuple2('とりわけ', '取り分け'),
			_Utils_Tuple2('とりわける', '取り分ける'),
			_Utils_Tuple2('とる', '執る'),
			_Utils_Tuple2('とるねーど', 'トルネード'),
			_Utils_Tuple2('どれい', '奴隷'),
			_Utils_Tuple2('とれーど', 'トレード'),
			_Utils_Tuple2('とれーらー', 'トレーラー'),
			_Utils_Tuple2('とろい', 'とろい'),
			_Utils_Tuple2('どろー', 'ドロー'),
			_Utils_Tuple2('とろける', 'とろける'),
			_Utils_Tuple2('とろとろ', 'とろとろ'),
			_Utils_Tuple2('どろどろ', 'どろどろ'),
			_Utils_Tuple2('とろふぃー', 'トロフィー'),
			_Utils_Tuple2('とろろ', 'とろろ'),
			_Utils_Tuple2('とろんぼーん', 'トロンボーン'),
			_Utils_Tuple2('どわすれ', '度忘れ'),
			_Utils_Tuple2('どん', 'どん'),
			_Utils_Tuple2('どんかん', '鈍感'),
			_Utils_Tuple2('どんでんがえし', 'どんでん返し'),
			_Utils_Tuple2('とんと', 'とんと'),
			_Utils_Tuple2('とんや', '問屋'),
			_Utils_Tuple2('どんよく', '貪欲'),
			_Utils_Tuple2('どんより', 'どんより'),
			_Utils_Tuple2('ないかく', '内閣'),
			_Utils_Tuple2('ないかく', '内角'),
			_Utils_Tuple2('ないかくそうりだいじん', '内閣総理大臣'),
			_Utils_Tuple2('ないしょく', '内職'),
			_Utils_Tuple2('ないしん', '内心'),
			_Utils_Tuple2('ないしんしょ', '内申書'),
			_Utils_Tuple2('ないそう', '内装'),
			_Utils_Tuple2('ないぞう', '内蔵'),
			_Utils_Tuple2('ないぞう', '内臓'),
			_Utils_Tuple2('ないたー', 'ナイター'),
			_Utils_Tuple2('ないちんげーる', 'ナイチンゲール'),
			_Utils_Tuple2('ないてい', '内定'),
			_Utils_Tuple2('ないみつ', '内密'),
			_Utils_Tuple2('ないや', '内野'),
			_Utils_Tuple2('ないやあんだ', '内野安打'),
			_Utils_Tuple2('ないやしゅ', '内野手'),
			_Utils_Tuple2('ないらん', '内乱'),
			_Utils_Tuple2('なえ', '苗'),
			_Utils_Tuple2('なえぎ', '苗木'),
			_Utils_Tuple2('なおかつ', 'なおかつ'),
			_Utils_Tuple2('なかじき', '中敷き'),
			_Utils_Tuple2('ながしこむ', '流し込む'),
			_Utils_Tuple2('ながつづき', '長続き'),
			_Utils_Tuple2('ながなが', '長々'),
			_Utils_Tuple2('なかにも', '中にも'),
			_Utils_Tuple2('ながらく', '長らく'),
			_Utils_Tuple2('なぎさ', 'なぎさ'),
			_Utils_Tuple2('なぐさめ', '慰め'),
			_Utils_Tuple2('なくなく', '泣く泣く'),
			_Utils_Tuple2('なげかける', '投げ掛ける'),
			_Utils_Tuple2('なげかわしい', '嘆かわしい'),
			_Utils_Tuple2('なげき', '嘆き'),
			_Utils_Tuple2('なげく', '嘆く'),
			_Utils_Tuple2('なこうど', '仲人'),
			_Utils_Tuple2('なごむ', '和む'),
			_Utils_Tuple2('なごやか', '和やか'),
			_Utils_Tuple2('なごり', '名残'),
			_Utils_Tuple2('なさけ', '情け'),
			_Utils_Tuple2('なさけない', '情けない'),
			_Utils_Tuple2('なざし', '名指し'),
			_Utils_Tuple2('なしとげる', '成し遂げる'),
			_Utils_Tuple2('なじみ', 'なじみ'),
			_Utils_Tuple2('なじむ', 'なじむ'),
			_Utils_Tuple2('なぞる', 'なぞる'),
			_Utils_Tuple2('なた', 'なた'),
			_Utils_Tuple2('なだれ', '雪崩'),
			_Utils_Tuple2('なついん', 'なつ印'),
			_Utils_Tuple2('なつく', '懐く'),
			_Utils_Tuple2('なでしこ', 'なでしこ'),
			_Utils_Tuple2('なとりうむ', 'ナトリウム'),
			_Utils_Tuple2('なにかと', '何かと'),
			_Utils_Tuple2('なにごと', '何事'),
			_Utils_Tuple2('なにしろ', '何しろ'),
			_Utils_Tuple2('なにせ', '何せ'),
			_Utils_Tuple2('なにとぞ', '何とぞ'),
			_Utils_Tuple2('なにひとつ', '何一つ'),
			_Utils_Tuple2('なにぶん', '何分'),
			_Utils_Tuple2('なにぶん', '何分'),
			_Utils_Tuple2('なにもの', '何物'),
			_Utils_Tuple2('なにやら', '何やら'),
			_Utils_Tuple2('なのりでる', '名乗り出る'),
			_Utils_Tuple2('なのる', '名乗る'),
			_Utils_Tuple2('なべぞこ', '鍋底'),
			_Utils_Tuple2('なまぐさい', '生臭い'),
			_Utils_Tuple2('なまじ', 'なまじ'),
			_Utils_Tuple2('なまじっか', 'なまじっか'),
			_Utils_Tuple2('なます', 'なます'),
			_Utils_Tuple2('なまなましい', '生々しい'),
			_Utils_Tuple2('なまり', 'なまり'),
			_Utils_Tuple2('なまり', '鉛'),
			_Utils_Tuple2('なまる', 'なまる'),
			_Utils_Tuple2('なみかぜ', '波風'),
			_Utils_Tuple2('なみだぐむ', '涙ぐむ'),
			_Utils_Tuple2('なみなみ', 'なみなみ'),
			_Utils_Tuple2('なめくじ', 'なめくじ'),
			_Utils_Tuple2('なめらか', '滑らか'),
			_Utils_Tuple2('なめる', 'なめる'),
			_Utils_Tuple2('なやましい', '悩ましい'),
			_Utils_Tuple2('なやます', '悩ます'),
			_Utils_Tuple2('ならう', '倣う'),
			_Utils_Tuple2('ならす', '慣らす'),
			_Utils_Tuple2('ならびに', '並びに'),
			_Utils_Tuple2('なり', '鳴り'),
			_Utils_Tuple2('なり', 'なり'),
			_Utils_Tuple2('なりあがる', '成り上がる'),
			_Utils_Tuple2('なりたち', '成り立ち'),
			_Utils_Tuple2('なりたつ', '成り立つ'),
			_Utils_Tuple2('なりゆき', '成り行き'),
			_Utils_Tuple2('なりわい', 'なりわい'),
			_Utils_Tuple2('なると', 'ナルト'),
			_Utils_Tuple2('なれーたー', 'ナレーター'),
			_Utils_Tuple2('なれそめ', 'なれ初め'),
			_Utils_Tuple2('なれなれしい', 'なれなれしい'),
			_Utils_Tuple2('なわ', '縄'),
			_Utils_Tuple2('なわばり', '縄張り'),
			_Utils_Tuple2('なん', '難'),
			_Utils_Tuple2('なんい', '難易'),
			_Utils_Tuple2('なんいど', '難易度'),
			_Utils_Tuple2('なんかい', '難解'),
			_Utils_Tuple2('なんかん', '難関'),
			_Utils_Tuple2('なんこう', '軟こう'),
			_Utils_Tuple2('なんこう', '難航'),
			_Utils_Tuple2('なんこつ', '軟骨'),
			_Utils_Tuple2('なんしき', '軟式'),
			_Utils_Tuple2('なんせんす', 'ナンセンス'),
			_Utils_Tuple2('なんたいどうぶつ', '軟体動物'),
			_Utils_Tuple2('なんちょう', '難聴'),
			_Utils_Tuple2('なんてん', '難点'),
			_Utils_Tuple2('なんど', '難度'),
			_Utils_Tuple2('なんぱ', '難破'),
			_Utils_Tuple2('なんばーぷれーと', 'ナンバープレート'),
			_Utils_Tuple2('なんべい', '南米'),
			_Utils_Tuple2('なんぼ', '何ぼ'),
			_Utils_Tuple2('なんぼくせんそう', '南北戦争'),
			_Utils_Tuple2('にかよう', '似通う'),
			_Utils_Tuple2('にがり', 'にがり'),
			_Utils_Tuple2('にがわらい', '苦笑い'),
			_Utils_Tuple2('にきび', 'にきび'),
			_Utils_Tuple2('にきょくか', '二極化'),
			_Utils_Tuple2('にくせい', '肉声'),
			_Utils_Tuple2('にくたらしい', '憎たらしい'),
			_Utils_Tuple2('にぐん', '二軍'),
			_Utils_Tuple2('にげこむ', '逃げ込む'),
			_Utils_Tuple2('にげだす', '逃げ出す'),
			_Utils_Tuple2('にごす', '濁す'),
			_Utils_Tuple2('にこちん', 'ニコチン'),
			_Utils_Tuple2('にこみ', '煮込み'),
			_Utils_Tuple2('にこむ', '煮込む'),
			_Utils_Tuple2('にごる', '濁る'),
			_Utils_Tuple2('にざかな', '煮魚'),
			_Utils_Tuple2('にしび', '西日'),
			_Utils_Tuple2('にじむ', 'にじむ'),
			_Utils_Tuple2('にじゅうこうぞう', '二重構造'),
			_Utils_Tuple2('にじる', '煮汁'),
			_Utils_Tuple2('にす', 'ニス'),
			_Utils_Tuple2('にせい', '二世'),
			_Utils_Tuple2('にせる', '似せる'),
			_Utils_Tuple2('にだい', '荷台'),
			_Utils_Tuple2('にだす', '煮出す'),
			_Utils_Tuple2('にたつ', '煮立つ'),
			_Utils_Tuple2('にちや', '日夜'),
			_Utils_Tuple2('にちようだいく', '日曜大工'),
			_Utils_Tuple2('にっきん', '日勤'),
			_Utils_Tuple2('にづくり', '荷造り'),
			_Utils_Tuple2('にっこり', 'にっこり'),
			_Utils_Tuple2('にっしょう', '日照'),
			_Utils_Tuple2('にっぽう', '日報'),
			_Utils_Tuple2('につめる', '煮詰める'),
			_Utils_Tuple2('にないて', '担い手'),
			_Utils_Tuple2('になう', '担う'),
			_Utils_Tuple2('ににんさんきゃく', '二人三脚'),
			_Utils_Tuple2('にのうで', '二の腕'),
			_Utils_Tuple2('にぶる', '鈍る'),
			_Utils_Tuple2('にぼし', '煮干し'),
			_Utils_Tuple2('にまめ', '煮豆'),
			_Utils_Tuple2('にめんせい', '二面性'),
			_Utils_Tuple2('にやにや', 'にやにや'),
			_Utils_Tuple2('にやり', 'にやり'),
			_Utils_Tuple2('にゅあんす', 'ニュアンス'),
			_Utils_Tuple2('にゅうえき', '乳液'),
			_Utils_Tuple2('にゅうか', '入荷'),
			_Utils_Tuple2('にゅうこう', '入港'),
			_Utils_Tuple2('にゅうさつ', '入札'),
			_Utils_Tuple2('にゅうさん', '乳酸'),
			_Utils_Tuple2('にゅうさんきん', '乳酸菌'),
			_Utils_Tuple2('にゅうし', '乳歯'),
			_Utils_Tuple2('にゅうじ', '乳児'),
			_Utils_Tuple2('にゅうしゅ', '入手'),
			_Utils_Tuple2('にゅうしゅつりょく', '入出力'),
			_Utils_Tuple2('にゅうしょう', '入賞'),
			_Utils_Tuple2('にゅうせき', '入籍'),
			_Utils_Tuple2('にゅうたい', '入隊'),
			_Utils_Tuple2('にゅうだん', '入団'),
			_Utils_Tuple2('にゅーとん', 'ニュートン'),
			_Utils_Tuple2('にゅうもん', '入門'),
			_Utils_Tuple2('にゅうようじ', '乳幼児'),
			_Utils_Tuple2('にょう', '尿'),
			_Utils_Tuple2('にりんしゃ', '二輪車'),
			_Utils_Tuple2('にるい', '二塁'),
			_Utils_Tuple2('にわか', 'にわか'),
			_Utils_Tuple2('にわき', '庭木'),
			_Utils_Tuple2('にわさき', '庭先'),
			_Utils_Tuple2('にんい', '任意'),
			_Utils_Tuple2('にんいほけん', '任意保険'),
			_Utils_Tuple2('にんか', '認可'),
			_Utils_Tuple2('にんき', '任期'),
			_Utils_Tuple2('にんぎょ', '人魚'),
			_Utils_Tuple2('にんぎょうげき', '人形劇'),
			_Utils_Tuple2('にんげんぞう', '人間像'),
			_Utils_Tuple2('にんげんみ', '人間味'),
			_Utils_Tuple2('にんしき', '認識'),
			_Utils_Tuple2('にんしょう', '人称'),
			_Utils_Tuple2('にんじょう', '人情'),
			_Utils_Tuple2('にんじょうみ', '人情味'),
			_Utils_Tuple2('にんしん', '妊娠'),
			_Utils_Tuple2('にんそう', '人相'),
			_Utils_Tuple2('にんたい', '忍耐'),
			_Utils_Tuple2('にんち', '認知'),
			_Utils_Tuple2('にんてい', '認定'),
			_Utils_Tuple2('にんむ', '任務'),
			_Utils_Tuple2('にんめい', '任命'),
			_Utils_Tuple2('ぬいめ', '縫い目'),
			_Utils_Tuple2('ぬーど', 'ヌード'),
			_Utils_Tuple2('ぬか', 'ぬか'),
			_Utils_Tuple2('ぬかす', '抜かす'),
			_Utils_Tuple2('ぬきだす', '抜き出す'),
			_Utils_Tuple2('ぬきとる', '抜き取る'),
			_Utils_Tuple2('ぬぐう', '拭う'),
			_Utils_Tuple2('ぬくぬく', 'ぬくぬく'),
			_Utils_Tuple2('ぬけげ', '抜け毛'),
			_Utils_Tuple2('ぬけだす', '抜け出す'),
			_Utils_Tuple2('ぬけでる', '抜け出る'),
			_Utils_Tuple2('ぬげる', '脱げる'),
			_Utils_Tuple2('ぬし', '主'),
			_Utils_Tuple2('ぬのじ', '布地'),
			_Utils_Tuple2('ぬま', '沼'),
			_Utils_Tuple2('ぬめり', 'ぬめり'),
			_Utils_Tuple2('ぬらす', 'ぬらす'),
			_Utils_Tuple2('ぬり', '塗り'),
			_Utils_Tuple2('ぬりたくる', '塗りたくる'),
			_Utils_Tuple2('ぬりたて', '塗り立て'),
			_Utils_Tuple2('ぬるぬる', 'ぬるぬる'),
			_Utils_Tuple2('ねいろ', '音色'),
			_Utils_Tuple2('ねおん', 'ネオン'),
			_Utils_Tuple2('ねが', 'ネガ'),
			_Utils_Tuple2('ねがいでる', '願い出る'),
			_Utils_Tuple2('ねがてぃぶ', 'ネガティブ'),
			_Utils_Tuple2('ねぐせ', '寝癖'),
			_Utils_Tuple2('ねこぜ', '猫背'),
			_Utils_Tuple2('ねごと', '寝言'),
			_Utils_Tuple2('ねころがる', '寝転がる'),
			_Utils_Tuple2('ねころぶ', '寝転ぶ'),
			_Utils_Tuple2('ねじ', 'ねじ'),
			_Utils_Tuple2('ねた', 'ねた'),
			_Utils_Tuple2('ねたむ', 'ねたむ'),
			_Utils_Tuple2('ねっきょう', '熱狂'),
			_Utils_Tuple2('ねっく', 'ネック'),
			_Utils_Tuple2('ねづく', '根付く'),
			_Utils_Tuple2('ねっする', '熱する'),
			_Utils_Tuple2('ねつぞう', 'ねつ造'),
			_Utils_Tuple2('ねったい', '熱帯'),
			_Utils_Tuple2('ねったいぎょ', '熱帯魚'),
			_Utils_Tuple2('ねっとう', '熱湯'),
			_Utils_Tuple2('ねつぼう', '熱望'),
			_Utils_Tuple2('ねづよい', '根強い'),
			_Utils_Tuple2('ねつりょう', '熱量'),
			_Utils_Tuple2('ねどこ', '寝床'),
			_Utils_Tuple2('ねばり', '粘り'),
			_Utils_Tuple2('ねばりづよい', '粘り強い'),
			_Utils_Tuple2('ねばる', '粘る'),
			_Utils_Tuple2('ねぶかい', '根深い'),
			_Utils_Tuple2('ねぶくろ', '寝袋'),
			_Utils_Tuple2('ねぼける', 'ねぼける'),
			_Utils_Tuple2('ねむたい', '眠たい'),
			_Utils_Tuple2('ねらい', '狙い'),
			_Utils_Tuple2('ねる', '練る'),
			_Utils_Tuple2('ねわざ', '寝技'),
			_Utils_Tuple2('ねん', '念'),
			_Utils_Tuple2('ねんいり', '念入り'),
			_Utils_Tuple2('ねんが', '年賀'),
			_Utils_Tuple2('ねんかん', '年鑑'),
			_Utils_Tuple2('ねんがん', '念願'),
			_Utils_Tuple2('ねんぐ', '年貢'),
			_Utils_Tuple2('ねんごう', '年号'),
			_Utils_Tuple2('ねんこうじょれつ', '年功序列'),
			_Utils_Tuple2('ねんざ', '捻挫'),
			_Utils_Tuple2('ねんしき', '年式'),
			_Utils_Tuple2('ねんしき', '年式'),
			_Utils_Tuple2('ねんじゅうぎょうじ', '年中行事'),
			_Utils_Tuple2('ねんしょう', '燃焼'),
			_Utils_Tuple2('ねんちゃく', '粘着'),
			_Utils_Tuple2('ねんちょう', '年長'),
			_Utils_Tuple2('ねんど', '粘土'),
			_Utils_Tuple2('ねんとう', '念頭'),
			_Utils_Tuple2('ねんぶつ', '念仏'),
			_Utils_Tuple2('ねんぽう', '年俸'),
			_Utils_Tuple2('ねんまく', '粘膜'),
			_Utils_Tuple2('ねんれいそう', '年齢層'),
			_Utils_Tuple2('のいず', 'ノイズ'),
			_Utils_Tuple2('のいろーぜ', 'ノイローゼ'),
			_Utils_Tuple2('のうきょう', '農協'),
			_Utils_Tuple2('のうこう', '農耕'),
			_Utils_Tuple2('のうこう', '濃厚'),
			_Utils_Tuple2('のうこつ', '納骨'),
			_Utils_Tuple2('のうさく', '農作'),
			_Utils_Tuple2('のうさんぶつ', '農産物'),
			_Utils_Tuple2('のうし', '脳死'),
			_Utils_Tuple2('のうしゅく', '濃縮'),
			_Utils_Tuple2('のうぜい', '納税'),
			_Utils_Tuple2('のうそっちゅう', '脳卒中'),
			_Utils_Tuple2('のうち', '農地'),
			_Utils_Tuple2('のうど', '濃度'),
			_Utils_Tuple2('のうのう', 'のうのう'),
			_Utils_Tuple2('のうは', '脳波'),
			_Utils_Tuple2('のうひん', '納品'),
			_Utils_Tuple2('のうふ', '農夫'),
			_Utils_Tuple2('のうみそ', '脳みそ'),
			_Utils_Tuple2('のうみつ', '濃密'),
			_Utils_Tuple2('のうむ', '濃霧'),
			_Utils_Tuple2('のうやく', '農薬'),
			_Utils_Tuple2('のうりん', '農林'),
			_Utils_Tuple2('のうりんすいさんしょう', '農林水産省'),
			_Utils_Tuple2('のがす', '逃す'),
			_Utils_Tuple2('のがれる', '逃れる'),
			_Utils_Tuple2('のき', '軒'),
			_Utils_Tuple2('のぎく', '野菊'),
			_Utils_Tuple2('のきなみ', '軒並み'),
			_Utils_Tuple2('のく', 'のく'),
			_Utils_Tuple2('のける', 'のける'),
			_Utils_Tuple2('のこぎり', 'のこぎり'),
			_Utils_Tuple2('のさばる', 'のさばる'),
			_Utils_Tuple2('のし', 'のし'),
			_Utils_Tuple2('のぞきこむ', 'のぞきこむ'),
			_Utils_Tuple2('のそのそ', 'のそのそ'),
			_Utils_Tuple2('のぞましい', '望ましい'),
			_Utils_Tuple2('のぞむ', '臨む'),
			_Utils_Tuple2('のっかる', '乗っかる'),
			_Utils_Tuple2('のっける', '乗っける'),
			_Utils_Tuple2('のっぺらぼう', 'のっぺらぼう'),
			_Utils_Tuple2('のっぽ', 'のっぽ'),
			_Utils_Tuple2('ののしる', '罵る'),
			_Utils_Tuple2('のばなし', '野放し'),
			_Utils_Tuple2('のはら', '野原'),
			_Utils_Tuple2('のびなやむ', '伸び悩む'),
			_Utils_Tuple2('のびのび', '延び延び'),
			_Utils_Tuple2('のびりつ', '伸び率'),
			_Utils_Tuple2('のぶ', 'ノブ'),
			_Utils_Tuple2('のべ', '延べ'),
			_Utils_Tuple2('のみ', 'のみ'),
			_Utils_Tuple2('のみこむ', '飲み込む'),
			_Utils_Tuple2('のみねーと', 'ノミネート'),
			_Utils_Tuple2('のめりこむ', 'のめり込む'),
			_Utils_Tuple2('のらねこ', '野良猫'),
			_Utils_Tuple2('のり', 'のり'),
			_Utils_Tuple2('のりあい', '乗り合い'),
			_Utils_Tuple2('のりあげる', '乗り上げる'),
			_Utils_Tuple2('のりあわせる', '乗り合わせる'),
			_Utils_Tuple2('のりいれ', '乗り入れ'),
			_Utils_Tuple2('のりいれる', '乗り入れる'),
			_Utils_Tuple2('のりきる', '乗り切る'),
			_Utils_Tuple2('のりくむ', '乗り組む'),
			_Utils_Tuple2('のりこえる', '乗り越える'),
			_Utils_Tuple2('のりごこち', '乗り心地'),
			_Utils_Tuple2('のりこむ', '乗り込む'),
			_Utils_Tuple2('のりだす', '乗り出す'),
			_Utils_Tuple2('のりつぐ', '乗り継ぐ'),
			_Utils_Tuple2('のりまわす', '乗り回す'),
			_Utils_Tuple2('のろい', '呪い'),
			_Utils_Tuple2('のろい', 'のろい'),
			_Utils_Tuple2('のろう', '呪う'),
			_Utils_Tuple2('のろのろ', 'のろのろ'),
			_Utils_Tuple2('のんき', 'のんき'),
			_Utils_Tuple2('は', '波'),
			_Utils_Tuple2('は', '派'),
			_Utils_Tuple2('は', '派'),
			_Utils_Tuple2('は', '波'),
			_Utils_Tuple2('ぱあ', 'ぱあ'),
			_Utils_Tuple2('ぱーかっしょん', 'パーカッション'),
			_Utils_Tuple2('はあく', '把握'),
			_Utils_Tuple2('ばーじょん', 'バージョン'),
			_Utils_Tuple2('ばーじん', 'バージン'),
			_Utils_Tuple2('ぱーそなりてぃー', 'パーソナリティー'),
			_Utils_Tuple2('ぱーそなる', 'パーソナル'),
			_Utils_Tuple2('ぱーつ', 'パーツ'),
			_Utils_Tuple2('ばーてん', 'バーテン'),
			_Utils_Tuple2('ばーなー', 'バーナー'),
			_Utils_Tuple2('はーぷ', 'ハープ'),
			_Utils_Tuple2('ばーべる', 'バーベル'),
			_Utils_Tuple2('はい', '廃'),
			_Utils_Tuple2('はい', '排'),
			_Utils_Tuple2('はい', '灰'),
			_Utils_Tuple2('はい', '肺'),
			_Utils_Tuple2('はい', '敗'),
			_Utils_Tuple2('はい', '杯'),
			_Utils_Tuple2('はいあん', '廃案'),
			_Utils_Tuple2('はいいん', '敗因'),
			_Utils_Tuple2('はいえな', 'ハイエナ'),
			_Utils_Tuple2('ばいお', 'バイオ'),
			_Utils_Tuple2('ぱいおにあ', 'パイオニア'),
			_Utils_Tuple2('ばいかい', '媒介'),
			_Utils_Tuple2('はいがす', '排ガス'),
			_Utils_Tuple2('はいかつりょう', '肺活量'),
			_Utils_Tuple2('はいから', 'ハイカラ'),
			_Utils_Tuple2('はいかん', '配管'),
			_Utils_Tuple2('はいき', '排気'),
			_Utils_Tuple2('はいき', '廃棄'),
			_Utils_Tuple2('はいきがす', '排気ガス'),
			_Utils_Tuple2('ばいきゃく', '売却'),
			_Utils_Tuple2('はいきゅう', '配給'),
			_Utils_Tuple2('はいきりょう', '排気量'),
			_Utils_Tuple2('はいぐう', '配偶'),
			_Utils_Tuple2('はいぐうしゃ', '配偶者'),
			_Utils_Tuple2('はいけい', '背景'),
			_Utils_Tuple2('はいけい', '拝啓'),
			_Utils_Tuple2('はいご', '背後'),
			_Utils_Tuple2('はいこう', '廃校'),
			_Utils_Tuple2('はいごう', '配合'),
			_Utils_Tuple2('はいし', '廃止'),
			_Utils_Tuple2('はいしゃ', '敗者'),
			_Utils_Tuple2('はいしゃ', '廃車'),
			_Utils_Tuple2('はいしゃく', '拝借'),
			_Utils_Tuple2('ばいしゃく', '媒酌'),
			_Utils_Tuple2('はいじゃっく', 'ハイジャック'),
			_Utils_Tuple2('ばいしゅう', '買収'),
			_Utils_Tuple2('はいしゅつ', '排出'),
			_Utils_Tuple2('ばいしゅん', '売春'),
			_Utils_Tuple2('ばいしゅんふ', '売春婦'),
			_Utils_Tuple2('はいじょ', '排除'),
			_Utils_Tuple2('ばいしょう', '賠償'),
			_Utils_Tuple2('はいしょく', '配色'),
			_Utils_Tuple2('はいじん', '俳人'),
			_Utils_Tuple2('はいすい', '排水'),
			_Utils_Tuple2('はいすいこう', '排水溝'),
			_Utils_Tuple2('ばいすう', '倍数'),
			_Utils_Tuple2('はいせき', '排斥'),
			_Utils_Tuple2('はいせつ', '排泄'),
			_Utils_Tuple2('はいせん', '配線'),
			_Utils_Tuple2('はいせん', '敗戦'),
			_Utils_Tuple2('はいそ', '敗訴'),
			_Utils_Tuple2('はいぞく', '配属'),
			_Utils_Tuple2('はいたい', '敗退'),
			_Utils_Tuple2('ばいたい', '媒体'),
			_Utils_Tuple2('はいち', '配置'),
			_Utils_Tuple2('はいはい', 'はいはい'),
			_Utils_Tuple2('ばいぱす', 'バイパス'),
			_Utils_Tuple2('はいばん', '廃盤'),
			_Utils_Tuple2('はいび', '配備'),
			_Utils_Tuple2('はいびすかす', 'ハイビスカス'),
			_Utils_Tuple2('はいひん', '廃品'),
			_Utils_Tuple2('はいふ', '配付'),
			_Utils_Tuple2('ぱいぷ', 'パイプ'),
			_Utils_Tuple2('ぱいぷやく', 'パイプ役'),
			_Utils_Tuple2('はいふん', 'ハイフン'),
			_Utils_Tuple2('はいぶん', '配分'),
			_Utils_Tuple2('はいぼく', '敗北'),
			_Utils_Tuple2('はいめん', '背面'),
			_Utils_Tuple2('はいやく', '配役'),
			_Utils_Tuple2('はいらいと', 'ハイライト'),
			_Utils_Tuple2('はいらん', '排卵'),
			_Utils_Tuple2('はいりこむ', '入り込む'),
			_Utils_Tuple2('ばいりつ', '倍率'),
			_Utils_Tuple2('はいりょ', '配慮'),
			_Utils_Tuple2('はいれつ', '配列'),
			_Utils_Tuple2('はう', 'はう'),
			_Utils_Tuple2('ぱうだー', 'パウダー'),
			_Utils_Tuple2('ばうんど', 'バウンド'),
			_Utils_Tuple2('はえる', '映える'),
			_Utils_Tuple2('はおり', '羽織'),
			_Utils_Tuple2('はおる', '羽織る'),
			_Utils_Tuple2('はかい', '破壊'),
			_Utils_Tuple2('はかいし', '墓石'),
			_Utils_Tuple2('はかいてき', '破壊的'),
			_Utils_Tuple2('ばかげる', '馬鹿げる'),
			_Utils_Tuple2('はがす', '剥がす'),
			_Utils_Tuple2('はかどる', 'はかどる'),
			_Utils_Tuple2('はかば', '墓場'),
			_Utils_Tuple2('ばかばかしい', '馬鹿馬鹿しい'),
			_Utils_Tuple2('はかま', 'はかま'),
			_Utils_Tuple2('はかまいり', '墓参り'),
			_Utils_Tuple2('ばかやろう', '馬鹿野郎'),
			_Utils_Tuple2('ばからしい', '馬鹿らしい'),
			_Utils_Tuple2('はかり', 'はかり'),
			_Utils_Tuple2('はかる', '図る'),
			_Utils_Tuple2('はがれる', '剥がれる'),
			_Utils_Tuple2('はき', '破棄'),
			_Utils_Tuple2('はきすてる', '吐き捨てる'),
			_Utils_Tuple2('はきだす', '吐き出す'),
			_Utils_Tuple2('はきだめ', '掃きだめ'),
			_Utils_Tuple2('はきゅう', '波及'),
			_Utils_Tuple2('はきょく', '破局'),
			_Utils_Tuple2('はく', '博'),
			_Utils_Tuple2('はく', '拍'),
			_Utils_Tuple2('はぐ', '剥ぐ'),
			_Utils_Tuple2('ばくおん', '爆音'),
			_Utils_Tuple2('ばくが', '麦芽'),
			_Utils_Tuple2('はくがい', '迫害'),
			_Utils_Tuple2('はぐくむ', '育む'),
			_Utils_Tuple2('ばくげき', '爆撃'),
			_Utils_Tuple2('はくしき', '博識'),
			_Utils_Tuple2('はくしゃ', '拍車'),
			_Utils_Tuple2('はくしゃく', '伯爵'),
			_Utils_Tuple2('はくしょ', '白書'),
			_Utils_Tuple2('ばくしょう', '爆笑'),
			_Utils_Tuple2('はくしょく', '白色'),
			_Utils_Tuple2('ばくぜん', '漠然'),
			_Utils_Tuple2('ばくだい', 'ばく大'),
			_Utils_Tuple2('はくだつ', '剥奪'),
			_Utils_Tuple2('ばくてりあ', 'バクテリア'),
			_Utils_Tuple2('はくねつ', '白熱'),
			_Utils_Tuple2('ばくは', '爆破'),
			_Utils_Tuple2('ぱくぱく', 'ぱくぱく'),
			_Utils_Tuple2('ばくふ', '幕府'),
			_Utils_Tuple2('ばくふう', '爆風'),
			_Utils_Tuple2('ばくまつ', '幕末'),
			_Utils_Tuple2('はくらん', '博覧'),
			_Utils_Tuple2('はくらんかい', '博覧会'),
			_Utils_Tuple2('ぱくり', 'ぱくり'),
			_Utils_Tuple2('はくりきこ', '薄力粉'),
			_Utils_Tuple2('はくりょく', '迫力'),
			_Utils_Tuple2('ぱくる', 'ぱくる'),
			_Utils_Tuple2('はぐるま', '歯車'),
			_Utils_Tuple2('はぐれる', 'はぐれる'),
			_Utils_Tuple2('ばくろ', '暴露'),
			_Utils_Tuple2('はげむ', '励む'),
			_Utils_Tuple2('ばけもの', '化け物'),
			_Utils_Tuple2('はげる', '剥げる'),
			_Utils_Tuple2('はげる', 'はげる'),
			_Utils_Tuple2('ばける', '化ける'),
			_Utils_Tuple2('はけん', '派遣'),
			_Utils_Tuple2('はごいた', '羽子板'),
			_Utils_Tuple2('はこいり', '箱入り'),
			_Utils_Tuple2('ばさばさ', 'ばさばさ'),
			_Utils_Tuple2('ぱさぱさ', 'ぱさぱさ'),
			_Utils_Tuple2('はじきだす', 'はじき出す'),
			_Utils_Tuple2('はじく', 'はじく'),
			_Utils_Tuple2('はじける', 'はじける'),
			_Utils_Tuple2('ぱしふぃっく', 'パシフィック'),
			_Utils_Tuple2('はしゃぐ', 'はしゃぐ'),
			_Utils_Tuple2('はしやすめ', '箸休め'),
			_Utils_Tuple2('はしりぬける', '走り抜ける'),
			_Utils_Tuple2('はじる', '恥じる'),
			_Utils_Tuple2('はしわたし', '橋渡し'),
			_Utils_Tuple2('はすう', '端数'),
			_Utils_Tuple2('ばすたぶ', 'バスタブ'),
			_Utils_Tuple2('ぱすてる', 'パステル'),
			_Utils_Tuple2('はずむ', '弾む'),
			_Utils_Tuple2('はぜ', 'はぜ'),
			_Utils_Tuple2('ばせい', '罵声'),
			_Utils_Tuple2('ぱせり', 'パセリ'),
			_Utils_Tuple2('はそん', '破損'),
			_Utils_Tuple2('はた', '端'),
			_Utils_Tuple2('ぱたー', 'パター'),
			_Utils_Tuple2('はだあれ', '肌荒れ'),
			_Utils_Tuple2('はださむい', '肌寒い'),
			_Utils_Tuple2('はたす', '果たす'),
			_Utils_Tuple2('ばたばた', 'ばたばた'),
			_Utils_Tuple2('ぱたぱた', 'ぱたぱた'),
			_Utils_Tuple2('ばたふらい', 'バタフライ'),
			_Utils_Tuple2('はたまた', 'はたまた'),
			_Utils_Tuple2('はたらきかける', '働き掛ける'),
			_Utils_Tuple2('はたらきざかり', '働き盛り'),
			_Utils_Tuple2('はたらきて', '働き手'),
			_Utils_Tuple2('はたん', '破綻'),
			_Utils_Tuple2('はち', '鉢'),
			_Utils_Tuple2('はちうえ', '鉢植え'),
			_Utils_Tuple2('ばちがい', '場違い'),
			_Utils_Tuple2('ぱちくり', 'ぱちくり'),
			_Utils_Tuple2('はちまき', '鉢巻き'),
			_Utils_Tuple2('はちょう', '波長'),
			_Utils_Tuple2('ばつ', '罰'),
			_Utils_Tuple2('はついく', '発育'),
			_Utils_Tuple2('はつえん', '発煙'),
			_Utils_Tuple2('はつが', '発芽'),
			_Utils_Tuple2('はっかー', 'ハッカー'),
			_Utils_Tuple2('はっかく', '発覚'),
			_Utils_Tuple2('はっき', '発揮'),
			_Utils_Tuple2('ばっくぐらうんど', 'バックグラウンド'),
			_Utils_Tuple2('はっくつ', '発掘'),
			_Utils_Tuple2('ばっくなんばー', 'バックナンバー'),
			_Utils_Tuple2('ばっくみらー', 'バックミラー'),
			_Utils_Tuple2('ばつぐん', '抜群'),
			_Utils_Tuple2('はっけつびょう', '白血病'),
			_Utils_Tuple2('はっけん', '発券'),
			_Utils_Tuple2('はつげんりょく', '発言力'),
			_Utils_Tuple2('はっこう', '発酵'),
			_Utils_Tuple2('はっこう', '発光'),
			_Utils_Tuple2('ばっさい', '伐採'),
			_Utils_Tuple2('ばっさり', 'ばっさり'),
			_Utils_Tuple2('はっしゃ', '発射'),
			_Utils_Tuple2('はっしょう', '発症'),
			_Utils_Tuple2('はっしょう', '発祥'),
			_Utils_Tuple2('はっしょく', '発色'),
			_Utils_Tuple2('はっしん', '発進'),
			_Utils_Tuple2('ばっすい', '抜粋'),
			_Utils_Tuple2('はっする', 'ハッスル'),
			_Utils_Tuple2('はっする', '発する'),
			_Utils_Tuple2('ばっする', '罰する'),
			_Utils_Tuple2('はっせい', '発声'),
			_Utils_Tuple2('ばっそく', '罰則'),
			_Utils_Tuple2('ばったー', 'バッター'),
			_Utils_Tuple2('ばったーぼっくす', 'バッターボックス'),
			_Utils_Tuple2('はったり', 'はったり'),
			_Utils_Tuple2('ぱったり', 'ぱったり'),
			_Utils_Tuple2('ばっち', 'バッチ'),
			_Utils_Tuple2('はっちゅう', '発注'),
			_Utils_Tuple2('ぱっちわーく', 'パッチワーク'),
			_Utils_Tuple2('ばってき', 'ばってき'),
			_Utils_Tuple2('ばってりー', 'バッテリー'),
			_Utils_Tuple2('はつでん', '発電'),
			_Utils_Tuple2('ぱっと', 'パット'),
			_Utils_Tuple2('はつどう', '発動'),
			_Utils_Tuple2('はつねつ', '発熱'),
			_Utils_Tuple2('はつばしょ', '初場所'),
			_Utils_Tuple2('はっぴ', '法被'),
			_Utils_Tuple2('はつびょう', '発病'),
			_Utils_Tuple2('ばっふァろー', 'バッファロー'),
			_Utils_Tuple2('はっぽう', '発砲'),
			_Utils_Tuple2('はっぽう', '八方'),
			_Utils_Tuple2('はっぽう', '発泡'),
			_Utils_Tuple2('はっぽうすちろーる', '発泡スチロール'),
			_Utils_Tuple2('はつらつ', 'はつらつ'),
			_Utils_Tuple2('はて', '果て'),
			_Utils_Tuple2('はて', 'はて'),
			_Utils_Tuple2('はてる', '果てる'),
			_Utils_Tuple2('はどう', '波動'),
			_Utils_Tuple2('はどめ', '歯止め'),
			_Utils_Tuple2('はな', 'はな'),
			_Utils_Tuple2('はな', 'ハナ'),
			_Utils_Tuple2('はなげ', '鼻毛'),
			_Utils_Tuple2('はなざかり', '花盛り'),
			_Utils_Tuple2('はなしこむ', '話し込む'),
			_Utils_Tuple2('はなつ', '放つ'),
			_Utils_Tuple2('はなはだ', '甚だ'),
			_Utils_Tuple2('はなはだしい', '甚だしい'),
			_Utils_Tuple2('はなばなしい', '華々しい'),
			_Utils_Tuple2('はなやぐ', '華やぐ'),
			_Utils_Tuple2('はなれ', '離れ'),
			_Utils_Tuple2('はなわ', '花輪'),
			_Utils_Tuple2('はにかみ', 'はにかみ'),
			_Utils_Tuple2('はにかむ', 'はにかむ'),
			_Utils_Tuple2('はね', '跳ね'),
			_Utils_Tuple2('ばね', 'ばね'),
			_Utils_Tuple2('はねあがる', '跳ね上がる'),
			_Utils_Tuple2('はねかえす', '跳ね返す'),
			_Utils_Tuple2('はねつける', 'はね付ける'),
			_Utils_Tuple2('はねる', 'はねる'),
			_Utils_Tuple2('はねる', '跳ねる'),
			_Utils_Tuple2('ぱねる', 'パネル'),
			_Utils_Tuple2('ぱのらま', 'パノラマ'),
			_Utils_Tuple2('ばば', 'ばば'),
			_Utils_Tuple2('ははかた', '母方'),
			_Utils_Tuple2('はばつ', '派閥'),
			_Utils_Tuple2('はびこる', 'はびこる'),
			_Utils_Tuple2('ぱふぉーまんす', 'パフォーマンス'),
			_Utils_Tuple2('ぱぶりっく', 'パブリック'),
			_Utils_Tuple2('ばぶる', 'バブル'),
			_Utils_Tuple2('はへん', '破片'),
			_Utils_Tuple2('はまき', '葉巻'),
			_Utils_Tuple2('はまる', 'はまる'),
			_Utils_Tuple2('はみんぐ', 'ハミング'),
			_Utils_Tuple2('はめ', '羽目'),
			_Utils_Tuple2('はめつ', '破滅'),
			_Utils_Tuple2('はめる', 'はめる'),
			_Utils_Tuple2('はもん', '波紋'),
			_Utils_Tuple2('はや', '早'),
			_Utils_Tuple2('はやがわり', '早変わり'),
			_Utils_Tuple2('はやばん', '早番'),
			_Utils_Tuple2('はやぶさ', 'はやぶさ'),
			_Utils_Tuple2('はやまる', '早まる'),
			_Utils_Tuple2('はやめる', '早める'),
			_Utils_Tuple2('はやめる', '速める'),
			_Utils_Tuple2('ばら', 'ばら'),
			_Utils_Tuple2('ぱらしゅーと', 'パラシュート'),
			_Utils_Tuple2('はらす', '晴らす'),
			_Utils_Tuple2('ばらす', 'ばらす'),
			_Utils_Tuple2('はらだたしい', '腹立たしい'),
			_Utils_Tuple2('はらだち', '腹立ち'),
			_Utils_Tuple2('ばらつき', 'ばらつき'),
			_Utils_Tuple2('ばらつく', 'ばらつく'),
			_Utils_Tuple2('ばらにく', 'ばら肉'),
			_Utils_Tuple2('はらのなか', '腹の中'),
			_Utils_Tuple2('ぱらぱら', 'ぱらぱら'),
			_Utils_Tuple2('ばらまく', 'ばらまく'),
			_Utils_Tuple2('はらむ', 'はらむ'),
			_Utils_Tuple2('はらわた', 'はらわた'),
			_Utils_Tuple2('はり', '張り'),
			_Utils_Tuple2('ばり', '張り'),
			_Utils_Tuple2('ばりえーしょん', 'バリエーション'),
			_Utils_Tuple2('はりかえる', '張り替える'),
			_Utils_Tuple2('はりがね', '針金'),
			_Utils_Tuple2('ばりかん', 'バリカン'),
			_Utils_Tuple2('ばりき', '馬力'),
			_Utils_Tuple2('ばりけーど', 'バリケード'),
			_Utils_Tuple2('はりだす', '張り出す'),
			_Utils_Tuple2('はりつける', '張り付ける'),
			_Utils_Tuple2('ぱりっと', 'ぱりっと'),
			_Utils_Tuple2('ばりばり', 'ばりばり'),
			_Utils_Tuple2('ぱりぱり', 'ぱりぱり'),
			_Utils_Tuple2('はりめぐらす', '張り巡らす'),
			_Utils_Tuple2('ばりゅー', 'バリュー'),
			_Utils_Tuple2('ばるーん', 'バルーン'),
			_Utils_Tuple2('はるか', '遥か'),
			_Utils_Tuple2('はるか', '遥か'),
			_Utils_Tuple2('はるさめ', '春雨'),
			_Utils_Tuple2('ぱるぷ', 'パルプ'),
			_Utils_Tuple2('はるめく', '春めく'),
			_Utils_Tuple2('はれ', '腫れ'),
			_Utils_Tuple2('はれあがる', '晴れ上がる'),
			_Utils_Tuple2('はれがましい', '晴れがましい'),
			_Utils_Tuple2('はれつ', '破裂'),
			_Utils_Tuple2('はれもの', '腫れ物'),
			_Utils_Tuple2('ばれりーな', 'バレリーナ'),
			_Utils_Tuple2('ぱろでぃー', 'パロディー'),
			_Utils_Tuple2('ばろめーたー', 'バロメーター'),
			_Utils_Tuple2('はん', '版'),
			_Utils_Tuple2('はん', '版'),
			_Utils_Tuple2('ばん', '盤'),
			_Utils_Tuple2('ばん', 'バン'),
			_Utils_Tuple2('はんえい', '繁栄'),
			_Utils_Tuple2('はんおん', '半音'),
			_Utils_Tuple2('はんかい', '半壊'),
			_Utils_Tuple2('ばんがい', '番外'),
			_Utils_Tuple2('はんかがい', '繁華街'),
			_Utils_Tuple2('はんかく', '半角'),
			_Utils_Tuple2('はんきょう', '反響'),
			_Utils_Tuple2('ぱんく', 'パンク'),
			_Utils_Tuple2('はんぐる', 'ハングル'),
			_Utils_Tuple2('はんけい', '半径'),
			_Utils_Tuple2('はんげき', '反撃'),
			_Utils_Tuple2('はんけつ', '判決'),
			_Utils_Tuple2('はんげん', '半減'),
			_Utils_Tuple2('はんこう', '犯行'),
			_Utils_Tuple2('ばんこん', '晩婚'),
			_Utils_Tuple2('はんじ', '判事'),
			_Utils_Tuple2('ばんじ', '万事'),
			_Utils_Tuple2('はんしゃ', '反射'),
			_Utils_Tuple2('ばんしゃく', '晩酌'),
			_Utils_Tuple2('はんじゅく', '半熟'),
			_Utils_Tuple2('はんじょう', '繁盛'),
			_Utils_Tuple2('はんしょく', '繁殖'),
			_Utils_Tuple2('ばんぜん', '万全'),
			_Utils_Tuple2('ばんそう', '伴奏'),
			_Utils_Tuple2('ばんそうこう', 'ばんそうこう'),
			_Utils_Tuple2('はんたー', 'ハンター'),
			_Utils_Tuple2('ばんちょう', '番長'),
			_Utils_Tuple2('ばんづけ', '番付'),
			_Utils_Tuple2('はんで', 'ハンデ'),
			_Utils_Tuple2('はんてい', '判定'),
			_Utils_Tuple2('はんてぃんぐ', 'ハンティング'),
			_Utils_Tuple2('はんてん', '反転'),
			_Utils_Tuple2('ばんと', 'バント'),
			_Utils_Tuple2('はんどう', '反動'),
			_Utils_Tuple2('はんどうたい', '半導体'),
			_Utils_Tuple2('ぱんとまいむ', 'パントマイム'),
			_Utils_Tuple2('ばんにん', '番人'),
			_Utils_Tuple2('はんにんまえ', '半人前'),
			_Utils_Tuple2('ばんねん', '晩年'),
			_Utils_Tuple2('ばんのう', '万能'),
			_Utils_Tuple2('はんぱ', '半端'),
			_Utils_Tuple2('ばんばん', 'ばんばん'),
			_Utils_Tuple2('ぱんぱん', 'パンパン'),
			_Utils_Tuple2('はんぷく', '反復'),
			_Utils_Tuple2('ひ', '比'),
			_Utils_Tuple2('ひ', '被'),
			_Utils_Tuple2('び', '尾'),
			_Utils_Tuple2('び', '鼻'),
			_Utils_Tuple2('ひあそび', '火遊び'),
			_Utils_Tuple2('ぴあのせん', 'ピアノ線'),
			_Utils_Tuple2('ひいき', 'ひいき'),
			_Utils_Tuple2('びいしき', '美意識'),
			_Utils_Tuple2('びーず', 'ビーズ'),
			_Utils_Tuple2('びーむ', 'ビーム'),
			_Utils_Tuple2('ひえこみ', '冷え込み'),
			_Utils_Tuple2('ひえこむ', '冷え込む'),
			_Utils_Tuple2('ひえしょう', '冷え性'),
			_Utils_Tuple2('びえん', '鼻炎'),
			_Utils_Tuple2('びおら', 'ビオラ'),
			_Utils_Tuple2('びか', '美化'),
			_Utils_Tuple2('ひがいもうそう', '被害妄想'),
			_Utils_Tuple2('ひかえ', '控え'),
			_Utils_Tuple2('ひかえしつ', '控え室'),
			_Utils_Tuple2('ひかえめ', '控えめ'),
			_Utils_Tuple2('ひかえる', '控える'),
			_Utils_Tuple2('ひかく', '皮革'),
			_Utils_Tuple2('びがく', '美学'),
			_Utils_Tuple2('ひがみ', 'ひがみ'),
			_Utils_Tuple2('ひかりごうせい', '光合成'),
			_Utils_Tuple2('ひかりつうしん', '光通信'),
			_Utils_Tuple2('ひかりふァいばー', '光ファイバー'),
			_Utils_Tuple2('ひかん', '悲観'),
			_Utils_Tuple2('ひがん', '彼岸'),
			_Utils_Tuple2('ひかんてき', '悲観的'),
			_Utils_Tuple2('ひきあう', '引き合う'),
			_Utils_Tuple2('ひきあげ', '引き上げ'),
			_Utils_Tuple2('ひきいる', '率いる'),
			_Utils_Tuple2('ひきいれる', '引き入れる'),
			_Utils_Tuple2('ひきおこす', '引き起こす'),
			_Utils_Tuple2('ひきがね', '引き金'),
			_Utils_Tuple2('ひきぎわ', '引き際'),
			_Utils_Tuple2('ひきこむ', '引き込む'),
			_Utils_Tuple2('ひきさがる', '引き下がる'),
			_Utils_Tuple2('ひきさく', '引き裂く'),
			_Utils_Tuple2('ひきさげる', '引き下げる'),
			_Utils_Tuple2('ひきしまる', '引き締まる'),
			_Utils_Tuple2('ひきしめる', '引き締める'),
			_Utils_Tuple2('ひきたてる', '引き立てる'),
			_Utils_Tuple2('ひきつぐ', '引き継ぐ'),
			_Utils_Tuple2('ひきつける', '引き付ける'),
			_Utils_Tuple2('ひきつづく', '引き続く'),
			_Utils_Tuple2('ひきでもの', '引き出物'),
			_Utils_Tuple2('ひきとり', '引き取り'),
			_Utils_Tuple2('ひきとる', '引き取る'),
			_Utils_Tuple2('ひきぬく', '引き抜く'),
			_Utils_Tuple2('ひきはなす', '引き離す'),
			_Utils_Tuple2('ひきゃく', '飛脚'),
			_Utils_Tuple2('ひきょう', 'ひきょう'),
			_Utils_Tuple2('ひきよせる', '引き寄せる'),
			_Utils_Tuple2('ひきわたし', '引き渡し'),
			_Utils_Tuple2('ひきわたす', '引き渡す'),
			_Utils_Tuple2('ひく', 'ひく'),
			_Utils_Tuple2('ぴくぴく', 'ぴくぴく'),
			_Utils_Tuple2('ひげき', '悲劇'),
			_Utils_Tuple2('ひけつ', '否決'),
			_Utils_Tuple2('ひけつ', '秘けつ'),
			_Utils_Tuple2('ひける', '引ける'),
			_Utils_Tuple2('ひけんしゃ', '被験者'),
			_Utils_Tuple2('ひこう', '非行'),
			_Utils_Tuple2('ひこく', '被告'),
			_Utils_Tuple2('ひこぼし', 'ひこ星'),
			_Utils_Tuple2('ひごろ', '日ごろ'),
			_Utils_Tuple2('ひさい', '被災'),
			_Utils_Tuple2('ひさしい', '久しい'),
			_Utils_Tuple2('ひさびさ', '久々'),
			_Utils_Tuple2('ひざまくら', '膝枕'),
			_Utils_Tuple2('ひさん', '悲惨'),
			_Utils_Tuple2('ひじかけ', '肘掛け'),
			_Utils_Tuple2('ひしゃたい', '被写体'),
			_Utils_Tuple2('ひじゅう', '比重'),
			_Utils_Tuple2('ひじゅん', '批准'),
			_Utils_Tuple2('ひじょう', '非情'),
			_Utils_Tuple2('びしょう', '微笑'),
			_Utils_Tuple2('ひじょうきん', '非常勤'),
			_Utils_Tuple2('ひじょうしき', '非常識'),
			_Utils_Tuple2('びしょく', '美食'),
			_Utils_Tuple2('びじょん', 'ビジョン'),
			_Utils_Tuple2('ひすてりっく', 'ヒステリック'),
			_Utils_Tuple2('ぴすとん', 'ピストン'),
			_Utils_Tuple2('びせいぶつ', '微生物'),
			_Utils_Tuple2('ひそか', 'ひそか'),
			_Utils_Tuple2('ひそむ', '潜む'),
			_Utils_Tuple2('ひそめる', '潜める'),
			_Utils_Tuple2('ひだ', 'ひだ'),
			_Utils_Tuple2('ひだい', '肥大'),
			_Utils_Tuple2('ひたす', '浸す'),
			_Utils_Tuple2('ひたすら', 'ひたすら'),
			_Utils_Tuple2('ぴたっと', 'ぴたっと'),
			_Utils_Tuple2('ひだね', '火種'),
			_Utils_Tuple2('ひたひた', 'ひたひた'),
			_Utils_Tuple2('ひたむき', 'ひたむき'),
			_Utils_Tuple2('ひたる', '浸る'),
			_Utils_Tuple2('びちく', '備蓄'),
			_Utils_Tuple2('ぴちぴち', 'ぴちぴち'),
			_Utils_Tuple2('びちゃびちゃ', 'びちゃびちゃ'),
			_Utils_Tuple2('ひつ', '筆'),
			_Utils_Tuple2('ひつう', '悲痛'),
			_Utils_Tuple2('ひっかく', '引っ掻く'),
			_Utils_Tuple2('ひっかける', '引っ掛ける'),
			_Utils_Tuple2('ひつぎ', 'ひつぎ'),
			_Utils_Tuple2('ひづけ', '日づけ'),
			_Utils_Tuple2('ひっけん', '必見'),
			_Utils_Tuple2('ひっこぬく', '引っこ抜く'),
			_Utils_Tuple2('ひっこみ', '引っ込み'),
			_Utils_Tuple2('ひっこむ', '引っ込む'),
			_Utils_Tuple2('ひっこめる', '引っ込める'),
			_Utils_Tuple2('ひっさつ', '必殺'),
			_Utils_Tuple2('ひっし', '必至'),
			_Utils_Tuple2('ひつじゅ', '必需'),
			_Utils_Tuple2('ひっしょう', '必勝'),
			_Utils_Tuple2('びっしり', 'びっしり'),
			_Utils_Tuple2('ひっす', '必須'),
			_Utils_Tuple2('ひっせき', '筆跡'),
			_Utils_Tuple2('ひつぜん', '必然'),
			_Utils_Tuple2('ひつぜんてき', '必然的'),
			_Utils_Tuple2('ひつだん', '筆談'),
			_Utils_Tuple2('ぴっち', 'ピッチ'),
			_Utils_Tuple2('ひっちはいく', 'ヒッチハイク'),
			_Utils_Tuple2('ぴっちゃー', 'ピッチャー'),
			_Utils_Tuple2('ぴっちんぐ', 'ピッチング'),
			_Utils_Tuple2('ひってき', '匹敵'),
			_Utils_Tuple2('ひっと', 'ヒット'),
			_Utils_Tuple2('びっと', 'ビット'),
			_Utils_Tuple2('ひっとう', '筆頭'),
			_Utils_Tuple2('びてき', '美的'),
			_Utils_Tuple2('ひでり', '日照り'),
			_Utils_Tuple2('ひでん', '秘伝'),
			_Utils_Tuple2('ひとあたり', '人当たり'),
			_Utils_Tuple2('ひといき', '一息'),
			_Utils_Tuple2('びどう', '微動'),
			_Utils_Tuple2('ひとがき', '人垣'),
			_Utils_Tuple2('ひとがら', '人柄'),
			_Utils_Tuple2('びとく', '美徳'),
			_Utils_Tuple2('ひとくふう', '一工夫'),
			_Utils_Tuple2('ひとごと', 'ひとごと'),
			_Utils_Tuple2('ひとざと', '人里'),
			_Utils_Tuple2('ひとさま', '人様'),
			_Utils_Tuple2('ひとじち', '人質'),
			_Utils_Tuple2('ひとすじ', '一筋'),
			_Utils_Tuple2('ひとたび', '一度'),
			_Utils_Tuple2('ひとなみ', '人波'),
			_Utils_Tuple2('ひとまかせ', '人任せ'),
			_Utils_Tuple2('ひとまず', 'ひとまず'),
			_Utils_Tuple2('ひとまとめ', 'ひとまとめ'),
			_Utils_Tuple2('ひとむかし', '一昔'),
			_Utils_Tuple2('ひとめ', '一目'),
			_Utils_Tuple2('ひとめ', '人目'),
			_Utils_Tuple2('ひどり', '日取り'),
			_Utils_Tuple2('ひとりあるき', '独り歩き'),
			_Utils_Tuple2('ひとりじめ', '独り占め'),
			_Utils_Tuple2('ひとりだち', '独り立ち'),
			_Utils_Tuple2('ひとりでに', '独りでに'),
			_Utils_Tuple2('ひとりもの', '独り者'),
			_Utils_Tuple2('ひな', 'ひな'),
			_Utils_Tuple2('ひなた', 'ひなた'),
			_Utils_Tuple2('ひなん', '非難'),
			_Utils_Tuple2('ひねくれる', 'ひねくれる'),
			_Utils_Tuple2('ひねる', 'ひねる'),
			_Utils_Tuple2('ひのくるま', '火の車'),
			_Utils_Tuple2('ひのたま', '火の玉'),
			_Utils_Tuple2('ひのようじん', '火の用心'),
			_Utils_Tuple2('ひばく', '被爆'),
			_Utils_Tuple2('びはだ', '美肌'),
			_Utils_Tuple2('ひばな', '火花'),
			_Utils_Tuple2('ひはん', '批判'),
			_Utils_Tuple2('ひはんてき', '批判的'),
			_Utils_Tuple2('ひび', 'ひび'),
			_Utils_Tuple2('ひびき', '響き'),
			_Utils_Tuple2('ひひょう', '批評'),
			_Utils_Tuple2('びびる', 'びびる'),
			_Utils_Tuple2('びひん', '備品'),
			_Utils_Tuple2('びぶらーと', 'ビブラート'),
			_Utils_Tuple2('ひぼう', 'ひぼう'),
			_Utils_Tuple2('ひぼし', '日干し'),
			_Utils_Tuple2('ひめ', '姫'),
			_Utils_Tuple2('ひめる', '秘める'),
			_Utils_Tuple2('ひもの', '干物'),
			_Utils_Tuple2('ひや', '冷や'),
			_Utils_Tuple2('ひやく', '飛躍'),
			_Utils_Tuple2('ひゃくじゅうのおう', '百獣の王'),
			_Utils_Tuple2('ひやくてき', '飛躍的'),
			_Utils_Tuple2('ひやむぎ', '冷や麦'),
			_Utils_Tuple2('ひややか', '冷ややか'),
			_Utils_Tuple2('ひややっこ', '冷やっこ'),
			_Utils_Tuple2('ひゆ', '比喩'),
			_Utils_Tuple2('ひゅーず', 'ヒューズ'),
			_Utils_Tuple2('ひゅーまにずむ', 'ヒューマニズム'),
			_Utils_Tuple2('ひゅーまん', 'ヒューマン'),
			_Utils_Tuple2('ひょいと', 'ひょいと'),
			_Utils_Tuple2('ひょう', '票'),
			_Utils_Tuple2('ひょう', '評'),
			_Utils_Tuple2('ひょう', '票'),
			_Utils_Tuple2('ひょう', '票'),
			_Utils_Tuple2('ひょう', 'ひょう'),
			_Utils_Tuple2('ひょうが', '氷河'),
			_Utils_Tuple2('びょうげんきん', '病原菌'),
			_Utils_Tuple2('ひょうご', '標語'),
			_Utils_Tuple2('ひょうこう', '標高'),
			_Utils_Tuple2('ひょうさつ', '表札'),
			_Utils_Tuple2('ひょうざん', '氷山'),
			_Utils_Tuple2('ひょうし', '拍子'),
			_Utils_Tuple2('ひょうじゅんか', '標準化'),
			_Utils_Tuple2('ひょうしょう', '表彰'),
			_Utils_Tuple2('びょうしん', '秒針'),
			_Utils_Tuple2('ひょうたん', 'ひょうたん'),
			_Utils_Tuple2('ひょうちゃく', '漂着'),
			_Utils_Tuple2('ひょうてい', '評定'),
			_Utils_Tuple2('ひょうてき', '標的'),
			_Utils_Tuple2('びょうとう', '病棟'),
			_Utils_Tuple2('ひょうはく', '漂白'),
			_Utils_Tuple2('ひょうひ', '表皮'),
			_Utils_Tuple2('びょうぶ', 'びょうぶ'),
			_Utils_Tuple2('びょうま', '病魔'),
			_Utils_Tuple2('ひょうめい', '表明'),
			_Utils_Tuple2('ひょうめんせき', '表面積'),
			_Utils_Tuple2('びょうよみ', '秒読み'),
			_Utils_Tuple2('ひょうり', '表裏'),
			_Utils_Tuple2('ひょうりゅう', '漂流'),
			_Utils_Tuple2('ひょうろん', '評論'),
			_Utils_Tuple2('ひよけ', '日よけ'),
			_Utils_Tuple2('ひよこ', 'ひよこ'),
			_Utils_Tuple2('ひょっこり', 'ひょっこり'),
			_Utils_Tuple2('ひょっと', 'ひょっと'),
			_Utils_Tuple2('ひより', '日より'),
			_Utils_Tuple2('びら', 'びら'),
			_Utils_Tuple2('ひらきなおる', '開き直る'),
			_Utils_Tuple2('ひらべったい', '平べったい'),
			_Utils_Tuple2('ひらめき', 'ひらめき'),
			_Utils_Tuple2('ひらめく', 'ひらめく'),
			_Utils_Tuple2('ひりき', '非力'),
			_Utils_Tuple2('ひりつ', '比率'),
			_Utils_Tuple2('ひりょう', '肥料'),
			_Utils_Tuple2('びりょう', '微量'),
			_Utils_Tuple2('ひる', 'ヒル'),
			_Utils_Tuple2('ひる', '干る'),
			_Utils_Tuple2('ひるがえす', '翻す'),
			_Utils_Tuple2('ひるがえる', '翻る'),
			_Utils_Tuple2('ひるさがり', '昼下がり'),
			_Utils_Tuple2('ひろう', '披露'),
			_Utils_Tuple2('ひんい', '品位'),
			_Utils_Tuple2('ひんかく', '品格'),
			_Utils_Tuple2('びんかん', '敏感'),
			_Utils_Tuple2('ひんこん', '貧困'),
			_Utils_Tuple2('ひんじゃく', '貧弱'),
			_Utils_Tuple2('ひんしゅ', '品種'),
			_Utils_Tuple2('ひんしゅかいりょう', '品種改良'),
			_Utils_Tuple2('ひんしゅく', 'ひんしゅく'),
			_Utils_Tuple2('びんじょう', '便乗'),
			_Utils_Tuple2('ひんせい', '品性'),
			_Utils_Tuple2('ぴんせっと', 'ピンセット'),
			_Utils_Tuple2('ひんそう', '貧相'),
			_Utils_Tuple2('びんてーじ', 'ビンテージ'),
			_Utils_Tuple2('ひんど', '頻度'),
			_Utils_Tuple2('ぴんと', 'ピント'),
			_Utils_Tuple2('ひんぱつ', '頻発'),
			_Utils_Tuple2('ひんぱん', '頻繁'),
			_Utils_Tuple2('ひんぱん', '頻繁'),
			_Utils_Tuple2('ふ', '府'),
			_Utils_Tuple2('ふ', '負'),
			_Utils_Tuple2('ふ', '布'),
			_Utils_Tuple2('ふァーむ', 'ファーム'),
			_Utils_Tuple2('ぶあい', '歩合'),
			_Utils_Tuple2('ふァいたー', 'ファイター'),
			_Utils_Tuple2('ふァいとまねー', 'ファイトマネー'),
			_Utils_Tuple2('ふァいばー', 'ファイバー'),
			_Utils_Tuple2('ふァいんだー', 'ファインダー'),
			_Utils_Tuple2('ふァうる', 'ファウル'),
			_Utils_Tuple2('ふァんたじー', 'ファンタジー'),
			_Utils_Tuple2('ふァんたすてぃっく', 'ファンタスティック'),
			_Utils_Tuple2('ふァんでーしょん', 'ファンデーション'),
			_Utils_Tuple2('ふい', '不意'),
			_Utils_Tuple2('ぶい', '部位'),
			_Utils_Tuple2('ぶいあいぴー', 'ＶＩＰ'),
			_Utils_Tuple2('ふぃーばー', 'フィーバー'),
			_Utils_Tuple2('ふぃーるど', 'フィールド'),
			_Utils_Tuple2('ふぃぎゅあ', 'フィギュア'),
			_Utils_Tuple2('ふぃくしょん', 'フィクション'),
			_Utils_Tuple2('ふぃじかる', 'フィジカル'),
			_Utils_Tuple2('ぶいねっく', 'Ｖネック'),
			_Utils_Tuple2('ぶいよん', 'ブイヨン'),
			_Utils_Tuple2('ふう', '風'),
			_Utils_Tuple2('ふう', '封'),
			_Utils_Tuple2('ふうあつ', '風圧'),
			_Utils_Tuple2('ふういん', '封印'),
			_Utils_Tuple2('ぶーいんぐ', 'ブーイング'),
			_Utils_Tuple2('ふうか', '風化'),
			_Utils_Tuple2('ぶーけ', 'ブーケ'),
			_Utils_Tuple2('ふうさ', '封鎖'),
			_Utils_Tuple2('ふうしゅう', '風習'),
			_Utils_Tuple2('ふうしょ', '封書'),
			_Utils_Tuple2('ぶーす', 'ブース'),
			_Utils_Tuple2('ふうすい', '風水'),
			_Utils_Tuple2('ふうずる', '封ずる'),
			_Utils_Tuple2('ふうぞく', '風俗'),
			_Utils_Tuple2('ふうちょう', '風潮'),
			_Utils_Tuple2('ふうど', '風土'),
			_Utils_Tuple2('ふうぶつ', '風物'),
			_Utils_Tuple2('ふうぶつし', '風物詩'),
			_Utils_Tuple2('ふうぼう', '風貌'),
			_Utils_Tuple2('ふうみ', '風味'),
			_Utils_Tuple2('ぶーめらん', 'ブーメラン'),
			_Utils_Tuple2('ふうりゅう', '風流'),
			_Utils_Tuple2('ふえ', '笛'),
			_Utils_Tuple2('ふえいせい', '不衛生'),
			_Utils_Tuple2('ふぇーどあうと', 'フェードアウト'),
			_Utils_Tuple2('ふえて', '不得手'),
			_Utils_Tuple2('ふぇにっくす', 'フェニックス'),
			_Utils_Tuple2('ふぇると', 'フェルト'),
			_Utils_Tuple2('ふぇろもん', 'フェロモン'),
			_Utils_Tuple2('ふぉーまっと', 'フォーマット'),
			_Utils_Tuple2('ふぉーまるどれす', 'フォーマルドレス'),
			_Utils_Tuple2('ふぉーめーしょん', 'フォーメーション'),
			_Utils_Tuple2('ふぉわーど', 'フォワード'),
			_Utils_Tuple2('ふか', '付加'),
			_Utils_Tuple2('ふかいり', '深入り'),
			_Utils_Tuple2('ふかかい', '不可解'),
			_Utils_Tuple2('ぶかつ', '部活'),
			_Utils_Tuple2('ふかぶか', '深々'),
			_Utils_Tuple2('ふかぶかと', '深々と'),
			_Utils_Tuple2('ふかみ', '深み'),
			_Utils_Tuple2('ふき', 'ふき'),
			_Utils_Tuple2('ぶき', '武器'),
			_Utils_Tuple2('ふきあげる', '吹き上げる'),
			_Utils_Tuple2('ふきかえ', '吹き替え'),
			_Utils_Tuple2('ふきかえす', '吹き返す'),
			_Utils_Tuple2('ふきげん', '不機嫌'),
			_Utils_Tuple2('ふきこむ', '吹き込む'),
			_Utils_Tuple2('ふきそ', '不起訴'),
			_Utils_Tuple2('ふきだす', '吹き出す'),
			_Utils_Tuple2('ふきつ', '不吉'),
			_Utils_Tuple2('ふきとばす', '吹き飛ばす'),
			_Utils_Tuple2('ふきとぶ', '吹き飛ぶ'),
			_Utils_Tuple2('ふきとる', '拭き取る'),
			_Utils_Tuple2('ふきぬけ', '吹き抜け'),
			_Utils_Tuple2('ぶきみ', '不気味'),
			_Utils_Tuple2('ふきょう', '布教'),
			_Utils_Tuple2('ふきょか', '不許可'),
			_Utils_Tuple2('ふきんこう', '不均衡'),
			_Utils_Tuple2('ふきんしん', '不謹慎'),
			_Utils_Tuple2('ふく', '噴く'),
			_Utils_Tuple2('ふくげん', '復元'),
			_Utils_Tuple2('ふくごう', '複合'),
			_Utils_Tuple2('ふくさんぶつ', '副産物'),
			_Utils_Tuple2('ふくし', '福祉'),
			_Utils_Tuple2('ふくしき', '複式'),
			_Utils_Tuple2('ふくしゃ', '複写'),
			_Utils_Tuple2('ふくしゅう', '復しゅう'),
			_Utils_Tuple2('ふくじゅう', '服従'),
			_Utils_Tuple2('ふくしょく', '服飾'),
			_Utils_Tuple2('ふくする', '服する'),
			_Utils_Tuple2('ふくせい', '複製'),
			_Utils_Tuple2('ふくとしん', '副都心'),
			_Utils_Tuple2('ぶくぶく', 'ぶくぶく'),
			_Utils_Tuple2('ふくぶくろ', '福袋'),
			_Utils_Tuple2('ふくみみ', '福耳'),
			_Utils_Tuple2('ふくめん', '覆面'),
			_Utils_Tuple2('ふくよか', '膨よか'),
			_Utils_Tuple2('ふくらはぎ', 'ふくらはぎ'),
			_Utils_Tuple2('ふくらます', '膨らます'),
			_Utils_Tuple2('ふくらみ', '膨らみ'),
			_Utils_Tuple2('ふくらむ', '膨らむ'),
			_Utils_Tuple2('ふくれあがる', '膨れ上がる'),
			_Utils_Tuple2('ふくれっつら', 'ふくれっ面'),
			_Utils_Tuple2('ふくれる', '膨れる'),
			_Utils_Tuple2('ふけ', 'ふけ'),
			_Utils_Tuple2('ぶけ', '武家'),
			_Utils_Tuple2('ふけい', '父兄'),
			_Utils_Tuple2('ふけつ', '不潔'),
			_Utils_Tuple2('ふける', '老ける'),
			_Utils_Tuple2('ふけんぜん', '不健全'),
			_Utils_Tuple2('ふごう', '富豪'),
			_Utils_Tuple2('ふこくきょうへい', '富国強兵'),
			_Utils_Tuple2('ふさ', '房'),
			_Utils_Tuple2('ふさい', '負債'),
			_Utils_Tuple2('ふさがる', '塞がる'),
			_Utils_Tuple2('ふさぐ', '塞ぐ'),
			_Utils_Tuple2('ふし', '節'),
			_Utils_Tuple2('ふし', '父子'),
			_Utils_Tuple2('ふじ', '藤'),
			_Utils_Tuple2('ぶし', '武士'),
			_Utils_Tuple2('ぶしどう', '武士道'),
			_Utils_Tuple2('ふしめ', '節目'),
			_Utils_Tuple2('ぶじゅつ', '武術'),
			_Utils_Tuple2('ふじゅん', '不純'),
			_Utils_Tuple2('ぶしょ', '部署'),
			_Utils_Tuple2('ふしょう', '負傷'),
			_Utils_Tuple2('ふしょう', '不詳'),
			_Utils_Tuple2('ふじょう', '浮上'),
			_Utils_Tuple2('ぶしょう', '武将'),
			_Utils_Tuple2('ぶしょう', '無精'),
			_Utils_Tuple2('ふしょうじ', '不祥事'),
			_Utils_Tuple2('ぶしょうひげ', '無精ひげ'),
			_Utils_Tuple2('ぶじょく', '侮辱'),
			_Utils_Tuple2('ふしん', '不審'),
			_Utils_Tuple2('ふしん', '不振'),
			_Utils_Tuple2('ふしん', '不信'),
			_Utils_Tuple2('ふしんにん', '不信任'),
			_Utils_Tuple2('ぶすう', '部数'),
			_Utils_Tuple2('ふすま', 'ふすま'),
			_Utils_Tuple2('ふぜい', '風情'),
			_Utils_Tuple2('ふせっせい', '不摂生'),
			_Utils_Tuple2('ふせる', '伏せる'),
			_Utils_Tuple2('ぶそう', '武装'),
			_Utils_Tuple2('ぶぞく', '部族'),
			_Utils_Tuple2('ぶたい', '部隊'),
			_Utils_Tuple2('ふたまた', '二股'),
			_Utils_Tuple2('ふだん', '不断'),
			_Utils_Tuple2('ふち', '縁'),
			_Utils_Tuple2('ぷち', 'プチ'),
			_Utils_Tuple2('ぶちあける', 'ぶち明ける'),
			_Utils_Tuple2('ぶちこむ', 'ぶち込む'),
			_Utils_Tuple2('ふちどり', '縁取り'),
			_Utils_Tuple2('ふちゃく', '付着'),
			_Utils_Tuple2('ぶつ', '仏'),
			_Utils_Tuple2('ふっき', '復帰'),
			_Utils_Tuple2('ふっきゅう', '復旧'),
			_Utils_Tuple2('ぶつぎり', 'ぶつ切り'),
			_Utils_Tuple2('ふっきれる', '吹っ切れる'),
			_Utils_Tuple2('ふっきん', '腹筋'),
			_Utils_Tuple2('ふっく', 'フック'),
			_Utils_Tuple2('ぶっけん', '物件'),
			_Utils_Tuple2('ふっこう', '復興'),
			_Utils_Tuple2('ふつごう', '不都合'),
			_Utils_Tuple2('ぶっさん', '物産'),
			_Utils_Tuple2('ぶっし', '物資'),
			_Utils_Tuple2('ぶっしつ', '物質'),
			_Utils_Tuple2('ぶっしつてき', '物質的'),
			_Utils_Tuple2('ぶったい', '物体'),
			_Utils_Tuple2('ぶつだん', '仏壇'),
			_Utils_Tuple2('ふってん', '沸点'),
			_Utils_Tuple2('ふっとう', '沸騰'),
			_Utils_Tuple2('ぶつぶつこうかん', '物々交換'),
			_Utils_Tuple2('ぶつりゅう', '物流'),
			_Utils_Tuple2('ふてい', '不定'),
			_Utils_Tuple2('ぷでぃんぐ', 'プディング'),
			_Utils_Tuple2('ふてき', '不適'),
			_Utils_Tuple2('ふてきとう', '不適当'),
			_Utils_Tuple2('ふとう', '不当'),
			_Utils_Tuple2('ふどう', '不動'),
			_Utils_Tuple2('ふとうめい', '不透明'),
			_Utils_Tuple2('ふとくてい', '不特定'),
			_Utils_Tuple2('ふところ', '懐'),
			_Utils_Tuple2('ぶなん', '無難'),
			_Utils_Tuple2('ふにん', '不妊'),
			_Utils_Tuple2('ふにん', '赴任'),
			_Utils_Tuple2('ふにんしょう', '不妊症'),
			_Utils_Tuple2('ふのう', '不能'),
			_Utils_Tuple2('ふはい', '腐敗'),
			_Utils_Tuple2('ふはつ', '不発'),
			_Utils_Tuple2('ふび', '不備'),
			_Utils_Tuple2('ふひょう', '不評'),
			_Utils_Tuple2('ふふく', '不服'),
			_Utils_Tuple2('ふへん', '普遍'),
			_Utils_Tuple2('ふへんてき', '普遍的'),
			_Utils_Tuple2('ふほうこうい', '不法行為'),
			_Utils_Tuple2('ふまえる', '踏まえる'),
			_Utils_Tuple2('ふみいれる', '踏み入れる'),
			_Utils_Tuple2('ふみきる', '踏み切る'),
			_Utils_Tuple2('ふみこむ', '踏み込む'),
			_Utils_Tuple2('ふみだい', '踏み台'),
			_Utils_Tuple2('ふみたおす', '踏み倒す'),
			_Utils_Tuple2('ふみだす', '踏み出す'),
			_Utils_Tuple2('ふみんしょう', '不眠症'),
			_Utils_Tuple2('ふめん', '譜面'),
			_Utils_Tuple2('ふもう', '不毛'),
			_Utils_Tuple2('ふもと', 'ふもと'),
			_Utils_Tuple2('ふもん', '不問'),
			_Utils_Tuple2('ふやける', 'ふやける'),
			_Utils_Tuple2('ふゆう', '富裕'),
			_Utils_Tuple2('ふよう', '扶養'),
			_Utils_Tuple2('ふらいんぐ', 'フライング'),
			_Utils_Tuple2('ぶらうんかん', 'ブラウン管'),
			_Utils_Tuple2('ぷらかーど', 'プラカード'),
			_Utils_Tuple2('ぶらく', '部落'),
			_Utils_Tuple2('ぷらぐ', 'プラグ'),
			_Utils_Tuple2('ぶらさがる', 'ぶら下がる'),
			_Utils_Tuple2('ぶらさげる', 'ぶら下げる'),
			_Utils_Tuple2('ふらすとれーしょん', 'フラストレーション'),
			_Utils_Tuple2('ぷらちな', 'プラチナ'),
			_Utils_Tuple2('ふらっぐ', 'フラッグ'),
			_Utils_Tuple2('ぶらっくりすと', 'ブラックリスト'),
			_Utils_Tuple2('ふらっと', 'ふらっと'),
			_Utils_Tuple2('ぷらねたりうむ', 'プラネタリウム'),
			_Utils_Tuple2('ぷらもでる', 'プラモデル'),
			_Utils_Tuple2('ぶらりと', 'ぶらりと'),
			_Utils_Tuple2('ぶらんく', 'ブランク'),
			_Utils_Tuple2('ぶらんこ', 'ぶらんこ'),
			_Utils_Tuple2('ふらんちゃいず', 'フランチャイズ'),
			_Utils_Tuple2('ぶらんでー', 'ブランデー'),
			_Utils_Tuple2('ぷらんにんぐ', 'プランニング'),
			_Utils_Tuple2('ふり', '振り'),
			_Utils_Tuple2('ぶり', '振り'),
			_Utils_Tuple2('ふりー', 'フリー'),
			_Utils_Tuple2('ふりかけ', '振り掛け'),
			_Utils_Tuple2('ふりきる', '振り切る'),
			_Utils_Tuple2('ぷりずむ', 'プリズム'),
			_Utils_Tuple2('ふりそそぐ', '降り注ぐ'),
			_Utils_Tuple2('ふりだし', '振り出し'),
			_Utils_Tuple2('ふりつけ', '振り付け'),
			_Utils_Tuple2('ふりはらう', '振り払う'),
			_Utils_Tuple2('ぷりぷり', 'ぷりぷり'),
			_Utils_Tuple2('ふりまわす', '振り回す'),
			_Utils_Tuple2('ぶりょく', '武力'),
			_Utils_Tuple2('ふりる', 'フリル'),
			_Utils_Tuple2('ふりわける', '振り分ける'),
			_Utils_Tuple2('ふりん', '不倫'),
			_Utils_Tuple2('ぶる', 'ぶる'),
			_Utils_Tuple2('ぶるい', '部類'),
			_Utils_Tuple2('ふるう', '振るう'),
			_Utils_Tuple2('ぶるーす', 'ブルース'),
			_Utils_Tuple2('ふるーと', 'フルート'),
			_Utils_Tuple2('ふるすぴーど', 'フルスピード'),
			_Utils_Tuple2('ぶるどーざー', 'ブルドーザー'),
			_Utils_Tuple2('ぷるとにうむ', 'プルトニウム'),
			_Utils_Tuple2('ふるはうす', 'フルハウス'),
			_Utils_Tuple2('ふるびる', '古びる'),
			_Utils_Tuple2('ふるぼける', '古ぼける'),
			_Utils_Tuple2('ふるまう', '振る舞う'),
			_Utils_Tuple2('ふるめかしい', '古めかしい'),
			_Utils_Tuple2('ふれあい', '触れ合い'),
			_Utils_Tuple2('ぶれい', '無礼'),
			_Utils_Tuple2('ぶれいこう', '無礼講'),
			_Utils_Tuple2('ぷれーおふ', 'プレーオフ'),
			_Utils_Tuple2('ぶれーく', 'ブレーク'),
			_Utils_Tuple2('ふれーず', 'フレーズ'),
			_Utils_Tuple2('ぷれーぼーい', 'プレーボーイ'),
			_Utils_Tuple2('ぶれーん', 'ブレーン'),
			_Utils_Tuple2('ぷれーん', 'プレーン'),
			_Utils_Tuple2('ぶれざー', 'ブレザー'),
			_Utils_Tuple2('ぷれみあむ', 'プレミアム'),
			_Utils_Tuple2('ふれる', '振れる'),
			_Utils_Tuple2('ぶれる', 'ぶれる'),
			_Utils_Tuple2('ふれんちとーすと', 'フレンチトースト'),
			_Utils_Tuple2('ぶれんど', 'ブレンド'),
			_Utils_Tuple2('ふろうしゃ', '浮浪者'),
			_Utils_Tuple2('ぶろー', 'ブロー'),
			_Utils_Tuple2('ふろおけ', '風呂おけ'),
			_Utils_Tuple2('ふろーずん', 'フローズン'),
			_Utils_Tuple2('ふろく', '付録'),
			_Utils_Tuple2('ぷろぐらみんぐ', 'プログラミング'),
			_Utils_Tuple2('ぷろじぇくとちーむ', 'プロジェクトチーム'),
			_Utils_Tuple2('ふろしき', '風呂敷'),
			_Utils_Tuple2('ぷろだくしょん', 'プロダクション'),
			_Utils_Tuple2('ぷろてくたー', 'プロテクター'),
			_Utils_Tuple2('ぷろてすたんと', 'プロテスタント'),
			_Utils_Tuple2('ぷろでゅーさー', 'プロデューサー'),
			_Utils_Tuple2('ぷろでゅーす', 'プロデュース'),
			_Utils_Tuple2('ぷろぱん', 'プロパン'),
			_Utils_Tuple2('ぷろぱんがす', 'プロパンガス'),
			_Utils_Tuple2('ぷろぺら', 'プロペラ'),
			_Utils_Tuple2('ぷろぽーしょん', 'プロポーション'),
			_Utils_Tuple2('ぷろろーぐ', 'プロローグ'),
			_Utils_Tuple2('ふろん', 'フロン'),
			_Utils_Tuple2('ふろんがす', 'フロンガス'),
			_Utils_Tuple2('ふろんとがらす', 'フロントガラス'),
			_Utils_Tuple2('ふわり', 'ふわり'),
			_Utils_Tuple2('ふん', 'ふん'),
			_Utils_Tuple2('ふん', 'ふん'),
			_Utils_Tuple2('ふんか', '噴火'),
			_Utils_Tuple2('ぶんか', '分化'),
			_Utils_Tuple2('ふんがい', '憤慨'),
			_Utils_Tuple2('ぶんかいさん', '文化遺産'),
			_Utils_Tuple2('ぶんかざい', '文化財'),
			_Utils_Tuple2('ぶんかじん', '文化人'),
			_Utils_Tuple2('ぶんかつ', '分割'),
			_Utils_Tuple2('ふんき', '奮起'),
			_Utils_Tuple2('ぶんき', '分岐'),
			_Utils_Tuple2('ぶんきてん', '分岐点'),
			_Utils_Tuple2('ぶんぎょう', '分業'),
			_Utils_Tuple2('ぶんげい', '文芸'),
			_Utils_Tuple2('ぶんげいさくひん', '文芸作品'),
			_Utils_Tuple2('ぶんけん', '文献'),
			_Utils_Tuple2('ぶんこ', '文庫'),
			_Utils_Tuple2('ふんさい', '粉砕'),
			_Utils_Tuple2('ぶんさん', '分散'),
			_Utils_Tuple2('ぶんし', '分子'),
			_Utils_Tuple2('ふんしつ', '紛失'),
			_Utils_Tuple2('ふんしゃ', '噴射'),
			_Utils_Tuple2('ふんしゅつ', '噴出'),
			_Utils_Tuple2('ぶんしょ', '文書'),
			_Utils_Tuple2('ぶんじょう', '分譲'),
			_Utils_Tuple2('ぶんすう', '分数'),
			_Utils_Tuple2('ぶんせき', '分析'),
			_Utils_Tuple2('ふんそう', '紛争'),
			_Utils_Tuple2('ぶんだん', '分断'),
			_Utils_Tuple2('ぶんつう', '文通'),
			_Utils_Tuple2('ふんとう', '奮闘'),
			_Utils_Tuple2('ぶんどき', '分度器'),
			_Utils_Tuple2('ふんどし', 'ふんどし'),
			_Utils_Tuple2('ぶんぱい', '分配'),
			_Utils_Tuple2('ふんぱつ', '奮発'),
			_Utils_Tuple2('ふんばる', '踏ん張る'),
			_Utils_Tuple2('ぶんぴつ', '分泌'),
			_Utils_Tuple2('ぶんぷ', '分布'),
			_Utils_Tuple2('ぶんぶん', 'ぶんぶん'),
			_Utils_Tuple2('ぷんぷん', 'ぷんぷん'),
			_Utils_Tuple2('ぶんべん', '分娩'),
			_Utils_Tuple2('ふんまつ', '粉末'),
			_Utils_Tuple2('ぶんみゃく', '文脈'),
			_Utils_Tuple2('ぶんめいかいか', '文明開化'),
			_Utils_Tuple2('ぶんり', '分離'),
			_Utils_Tuple2('ぶんれつ', '分裂'),
			_Utils_Tuple2('へ', '屁'),
			_Utils_Tuple2('へい', 'へい'),
			_Utils_Tuple2('へい', '塀'),
			_Utils_Tuple2('へい', '兵'),
			_Utils_Tuple2('へいあん', '平安'),
			_Utils_Tuple2('へいあん', '平安'),
			_Utils_Tuple2('へいおん', '平穏'),
			_Utils_Tuple2('へいか', '陛下'),
			_Utils_Tuple2('へいがい', '弊害'),
			_Utils_Tuple2('へいがん', '併願'),
			_Utils_Tuple2('へいき', '兵器'),
			_Utils_Tuple2('へいきんち', '平均値'),
			_Utils_Tuple2('へいこう', '並行'),
			_Utils_Tuple2('へいこう', '閉口'),
			_Utils_Tuple2('へいごう', '併合'),
			_Utils_Tuple2('へいさ', '閉鎖'),
			_Utils_Tuple2('へいし', '兵士'),
			_Utils_Tuple2('へいせい', '平静'),
			_Utils_Tuple2('へいたん', '平坦'),
			_Utils_Tuple2('へいち', '平地'),
			_Utils_Tuple2('へいよう', '併用'),
			_Utils_Tuple2('へいりょく', '兵力'),
			_Utils_Tuple2('へいれつ', '並列'),
			_Utils_Tuple2('へいわじょうやく', '平和条約'),
			_Utils_Tuple2('ぺいんと', 'ペイント'),
			_Utils_Tuple2('べーきんぐぱうだー', 'ベーキングパウダー'),
			_Utils_Tuple2('ぺーすと', 'ペースト'),
			_Utils_Tuple2('ぺーぱーどらいばー', 'ペーパードライバー'),
			_Utils_Tuple2('へき', '壁'),
			_Utils_Tuple2('へきが', '壁画'),
			_Utils_Tuple2('へくたーる', 'ヘクタール'),
			_Utils_Tuple2('べくとる', 'ベクトル'),
			_Utils_Tuple2('へこみ', 'へこみ'),
			_Utils_Tuple2('へこむ', 'へこむ'),
			_Utils_Tuple2('べすとせらー', 'ベストセラー'),
			_Utils_Tuple2('へたくそ', '下手くそ'),
			_Utils_Tuple2('へだたり', '隔たり'),
			_Utils_Tuple2('べたつく', 'べた付く'),
			_Utils_Tuple2('へだてる', '隔てる'),
			_Utils_Tuple2('ぺたぺた', 'ぺたぺた'),
			_Utils_Tuple2('べっかく', '別格'),
			_Utils_Tuple2('べっこ', '別個'),
			_Utils_Tuple2('べっさつ', '別冊'),
			_Utils_Tuple2('べっし', '蔑視'),
			_Utils_Tuple2('べったり', 'べったり'),
			_Utils_Tuple2('ぺったり', 'ぺったり'),
			_Utils_Tuple2('べっと', '別途'),
			_Utils_Tuple2('へっどこーち', 'ヘッドコーチ'),
			_Utils_Tuple2('へっどらいと', 'ヘッドライト'),
			_Utils_Tuple2('べつわく', '別枠'),
			_Utils_Tuple2('へどろ', 'へどろ'),
			_Utils_Tuple2('ぺなるてぃー', 'ペナルティー'),
			_Utils_Tuple2('ぺにす', 'ペニス'),
			_Utils_Tuple2('べにやいた', 'ベニヤ板'),
			_Utils_Tuple2('ぺぱーみんと', 'ペパーミント'),
			_Utils_Tuple2('へびーきゅう', 'ヘビー級'),
			_Utils_Tuple2('へま', 'へま'),
			_Utils_Tuple2('へやわり', '部屋割り'),
			_Utils_Tuple2('へりうむ', 'ヘリウム'),
			_Utils_Tuple2('へる', '経る'),
			_Utils_Tuple2('へるつ', 'ヘルツ'),
			_Utils_Tuple2('べれー', 'ベレー'),
			_Utils_Tuple2('べろ', 'べろ'),
			_Utils_Tuple2('へん', '片'),
			_Utils_Tuple2('へん', '片'),
			_Utils_Tuple2('へん', '片'),
			_Utils_Tuple2('べん', '弁'),
			_Utils_Tuple2('へんい', '変異'),
			_Utils_Tuple2('べんかい', '弁解'),
			_Utils_Tuple2('へんかきゅう', '変化球'),
			_Utils_Tuple2('へんかく', '変革'),
			_Utils_Tuple2('べんがく', '勉学'),
			_Utils_Tuple2('へんかん', '返還'),
			_Utils_Tuple2('べんき', '便器'),
			_Utils_Tuple2('べんぎ', '便宜'),
			_Utils_Tuple2('へんきょく', '編曲'),
			_Utils_Tuple2('へんけん', '偏見'),
			_Utils_Tuple2('べんご', '弁護'),
			_Utils_Tuple2('べんざ', '便座'),
			_Utils_Tuple2('へんさい', '返済'),
			_Utils_Tuple2('へんさち', '偏差値'),
			_Utils_Tuple2('へんしつ', '変質'),
			_Utils_Tuple2('へんしゅう', '編集'),
			_Utils_Tuple2('べんしょう', '弁償'),
			_Utils_Tuple2('へんしょく', '偏食'),
			_Utils_Tuple2('へんせい', '編成'),
			_Utils_Tuple2('へんせん', '変遷'),
			_Utils_Tuple2('へんそく', '変則'),
			_Utils_Tuple2('へんそく', '変速'),
			_Utils_Tuple2('へんたい', '変態'),
			_Utils_Tuple2('ぺんたごん', 'ペンタゴン'),
			_Utils_Tuple2('ぺんち', 'ペンチ'),
			_Utils_Tuple2('べんちゃー', 'ベンチャー'),
			_Utils_Tuple2('べんつう', '便通'),
			_Utils_Tuple2('へんどう', '変動'),
			_Utils_Tuple2('ぺんねーむ', 'ペンネーム'),
			_Utils_Tuple2('ぺんふれんど', 'ペンフレンド'),
			_Utils_Tuple2('へんぼう', '変貌'),
			_Utils_Tuple2('へんよう', '変容'),
			_Utils_Tuple2('べんろん', '弁論'),
			_Utils_Tuple2('ほ', '帆'),
			_Utils_Tuple2('ほあん', '保安'),
			_Utils_Tuple2('ほいく', '保育'),
			_Utils_Tuple2('ぼいこっと', 'ボイコット'),
			_Utils_Tuple2('ぼいすれこーだー', 'ボイスレコーダー'),
			_Utils_Tuple2('ほいっぷ', 'ホイップ'),
			_Utils_Tuple2('ぼいらー', 'ボイラー'),
			_Utils_Tuple2('ぼいん', 'ぼ印'),
			_Utils_Tuple2('ほう', '宝'),
			_Utils_Tuple2('ぼう', '某'),
			_Utils_Tuple2('ぼう', '房'),
			_Utils_Tuple2('ほうあん', '法案'),
			_Utils_Tuple2('ほうい', '方位'),
			_Utils_Tuple2('ほうえい', '放映'),
			_Utils_Tuple2('ぼうえい', '防衛'),
			_Utils_Tuple2('ぼうえいちょう', '防衛庁'),
			_Utils_Tuple2('ぼうえきまさつ', '貿易摩擦'),
			_Utils_Tuple2('ぼうえん', '望遠'),
			_Utils_Tuple2('ぼうえんきょう', '望遠鏡'),
			_Utils_Tuple2('ほうおう', '法王'),
			_Utils_Tuple2('ぼうおん', '防音'),
			_Utils_Tuple2('ほうか', '放火'),
			_Utils_Tuple2('ほうが', '邦画'),
			_Utils_Tuple2('ぼうか', '防火'),
			_Utils_Tuple2('ほうかい', '崩壊'),
			_Utils_Tuple2('ぼうがい', '妨害'),
			_Utils_Tuple2('ほうがく', '邦楽'),
			_Utils_Tuple2('ほうかつ', '包括'),
			_Utils_Tuple2('ぼうかん', '傍観'),
			_Utils_Tuple2('ほうき', '放棄'),
			_Utils_Tuple2('ほうき', '法規'),
			_Utils_Tuple2('ぼうきゃく', '忘却'),
			_Utils_Tuple2('ぼうぎょ', '防御'),
			_Utils_Tuple2('ぼうぎょりつ', '防御率'),
			_Utils_Tuple2('ぼうぐ', '防具'),
			_Utils_Tuple2('ぼうげん', '暴言'),
			_Utils_Tuple2('ほうけんしゃかい', '封建社会'),
			_Utils_Tuple2('ほうけんせいど', '封建制度'),
			_Utils_Tuple2('ほうけんてき', '封建的'),
			_Utils_Tuple2('ほうこ', '宝庫'),
			_Utils_Tuple2('ぼうご', '防護'),
			_Utils_Tuple2('ほうこう', '芳香'),
			_Utils_Tuple2('ほうさく', '豊作'),
			_Utils_Tuple2('ほうさく', '方策'),
			_Utils_Tuple2('ほうし', '奉仕'),
			_Utils_Tuple2('ほうじ', '法事'),
			_Utils_Tuple2('ほうしき', '方式'),
			_Utils_Tuple2('ほうしゃ', '放射'),
			_Utils_Tuple2('ほうしゃせん', '放射線'),
			_Utils_Tuple2('ほうしゃのう', '放射能'),
			_Utils_Tuple2('ほうしゅう', '報酬'),
			_Utils_Tuple2('ぼうしゅう', '防臭'),
			_Utils_Tuple2('ほうしゅつ', '放出'),
			_Utils_Tuple2('ほうじる', '報じる'),
			_Utils_Tuple2('ほうしん', '放心'),
			_Utils_Tuple2('ほうじん', '邦人'),
			_Utils_Tuple2('ほうじん', '法人'),
			_Utils_Tuple2('ぼうず', '坊主'),
			_Utils_Tuple2('ぼうずあたま', '坊主頭'),
			_Utils_Tuple2('ほうずる', '報ずる'),
			_Utils_Tuple2('ぼうぜん', 'ぼう然'),
			_Utils_Tuple2('ぼうそうぞく', '暴走族'),
			_Utils_Tuple2('ほうそく', '法則'),
			_Utils_Tuple2('ぼうだい', '膨大'),
			_Utils_Tuple2('ぼうたかとび', '棒高跳び'),
			_Utils_Tuple2('ぼうだち', '棒立ち'),
			_Utils_Tuple2('ぼうだん', '防弾'),
			_Utils_Tuple2('ほうち', '放置'),
			_Utils_Tuple2('ぼうちゅうざい', '防虫剤'),
			_Utils_Tuple2('ぼうちょう', '傍聴'),
			_Utils_Tuple2('ぼうちょう', '膨張'),
			_Utils_Tuple2('ほうてい', '法廷'),
			_Utils_Tuple2('ほうていしき', '方程式'),
			_Utils_Tuple2('ほうてん', '法典'),
			_Utils_Tuple2('ほうでん', '放電'),
			_Utils_Tuple2('ぼうと', '暴徒'),
			_Utils_Tuple2('ぼうとう', '冒頭'),
			_Utils_Tuple2('ぼうどう', '暴動'),
			_Utils_Tuple2('ほうどうきかん', '報道機関'),
			_Utils_Tuple2('ほうのう', '奉納'),
			_Utils_Tuple2('ぼうはてい', '防波堤'),
			_Utils_Tuple2('ほうび', '褒美'),
			_Utils_Tuple2('ぼうび', '防備'),
			_Utils_Tuple2('ぼうふ', '防腐'),
			_Utils_Tuple2('ほうふく', '報復'),
			_Utils_Tuple2('ほうべん', '方便'),
			_Utils_Tuple2('ほうぼく', '放牧'),
			_Utils_Tuple2('ほうむ', '法務'),
			_Utils_Tuple2('ほうむしょう', '法務省'),
			_Utils_Tuple2('ほうむる', '葬る'),
			_Utils_Tuple2('ほうよう', '抱擁'),
			_Utils_Tuple2('ぼうらく', '暴落'),
			_Utils_Tuple2('ほうりこむ', '放り込む'),
			_Utils_Tuple2('ほうりだす', '放り出す'),
			_Utils_Tuple2('ほうりゅう', '放流'),
			_Utils_Tuple2('ぼうりょくだん', '暴力団'),
			_Utils_Tuple2('ほうれい', '法令'),
			_Utils_Tuple2('ほうわ', '飽和'),
			_Utils_Tuple2('ぼーいすかうと', 'ボーイスカウト'),
			_Utils_Tuple2('ぼーかりすと', 'ボーカリスト'),
			_Utils_Tuple2('ぼーかる', 'ボーカル'),
			_Utils_Tuple2('ぽーたぶる', 'ポータブル'),
			_Utils_Tuple2('ほおん', '保温'),
			_Utils_Tuple2('ぽか', 'ぽか'),
			_Utils_Tuple2('ほかく', '捕獲'),
			_Utils_Tuple2('ほがらか', '朗らか'),
			_Utils_Tuple2('ほかん', '保管'),
			_Utils_Tuple2('ぼき', '簿記'),
			_Utils_Tuple2('ぼきゃぶらりー', 'ボキャブラリー'),
			_Utils_Tuple2('ほきゅう', '補給'),
			_Utils_Tuple2('ほきょう', '補強'),
			_Utils_Tuple2('ぼきん', '募金'),
			_Utils_Tuple2('ほくい', '北緯'),
			_Utils_Tuple2('ぼくさー', 'ボクサー'),
			_Utils_Tuple2('ほぐす', 'ほぐす'),
			_Utils_Tuple2('ぼくちく', '牧畜'),
			_Utils_Tuple2('ほくほく', 'ほくほく'),
			_Utils_Tuple2('ほぐれる', 'ほぐれる'),
			_Utils_Tuple2('ぼけ', 'ぼけ'),
			_Utils_Tuple2('ほげい', '捕鯨'),
			_Utils_Tuple2('ぼける', 'ぼける'),
			_Utils_Tuple2('ほけんきん', '保険金'),
			_Utils_Tuple2('ほご', '保護'),
			_Utils_Tuple2('ほこうしゃてんごく', '歩行者天国'),
			_Utils_Tuple2('ほごしゃ', '保護者'),
			_Utils_Tuple2('ぼこぼこ', 'ぼこぼこ'),
			_Utils_Tuple2('ほこり', '誇り'),
			_Utils_Tuple2('ほこる', '誇る'),
			_Utils_Tuple2('ほさ', '補佐'),
			_Utils_Tuple2('ぼさぼさ', 'ぼさぼさ'),
			_Utils_Tuple2('ほし', '干し'),
			_Utils_Tuple2('ほじ', '保持'),
			_Utils_Tuple2('ぼし', '干し'),
			_Utils_Tuple2('ぼしかてい', '母子家庭'),
			_Utils_Tuple2('ぽじしょん', 'ポジション'),
			_Utils_Tuple2('ほしつ', '保湿'),
			_Utils_Tuple2('ほしゃく', '保釈'),
			_Utils_Tuple2('ほしゅ', '保守'),
			_Utils_Tuple2('ほしゅ', '捕手'),
			_Utils_Tuple2('ほしゅう', '補修'),
			_Utils_Tuple2('ほじゅう', '補充'),
			_Utils_Tuple2('ほしゅてき', '保守的'),
			_Utils_Tuple2('ほしょう', '保障'),
			_Utils_Tuple2('ほしょう', '補償'),
			_Utils_Tuple2('ほしょうきん', '保証金'),
			_Utils_Tuple2('ほじょきん', '補助金'),
			_Utils_Tuple2('ほすてす', 'ホステス'),
			_Utils_Tuple2('ほすと', 'ホスト'),
			_Utils_Tuple2('ぼすとんばっぐ', 'ボストンバッグ'),
			_Utils_Tuple2('ほせい', '補正'),
			_Utils_Tuple2('ぼせい', '母性'),
			_Utils_Tuple2('ほぜん', '保全'),
			_Utils_Tuple2('ほそう', '舗装'),
			_Utils_Tuple2('ほそく', '補足'),
			_Utils_Tuple2('ほそみ', '細身'),
			_Utils_Tuple2('ほそめ', '細目'),
			_Utils_Tuple2('ほそめる', '細める'),
			_Utils_Tuple2('ほそる', '細る'),
			_Utils_Tuple2('ぽたーじゅ', 'ポタージュ'),
			_Utils_Tuple2('ぼたい', '母体'),
			_Utils_Tuple2('ぽたり', 'ぽたり'),
			_Utils_Tuple2('ぼたん', 'ぼたん'),
			_Utils_Tuple2('ぼち', '墓地'),
			_Utils_Tuple2('ぼちぼち', 'ぼちぼち'),
			_Utils_Tuple2('ほちょう', '歩調'),
			_Utils_Tuple2('ほちょうき', '補聴器'),
			_Utils_Tuple2('ぼつ', '没'),
			_Utils_Tuple2('ほっかい', '北海'),
			_Utils_Tuple2('ぽっかり', 'ぽっかり'),
			_Utils_Tuple2('ほっきょく', '北極'),
			_Utils_Tuple2('ほっく', 'ホック'),
			_Utils_Tuple2('ほっさ', '発作'),
			_Utils_Tuple2('ほっさてき', '発作的'),
			_Utils_Tuple2('ぼっしゅう', '没収'),
			_Utils_Tuple2('ほっする', '欲する'),
			_Utils_Tuple2('ぼっする', '没する'),
			_Utils_Tuple2('ほっそく', '発足'),
			_Utils_Tuple2('ほっそり', 'ほっそり'),
			_Utils_Tuple2('ほったらかし', 'ほったらかし'),
			_Utils_Tuple2('ほったらかす', 'ほったらかす'),
			_Utils_Tuple2('ほったん', '発端'),
			_Utils_Tuple2('ぼっとう', '没頭'),
			_Utils_Tuple2('ぼっぱつ', '勃発'),
			_Utils_Tuple2('ぽっぷ', 'ポップ'),
			_Utils_Tuple2('ぽつぽつ', 'ぽつぽつ'),
			_Utils_Tuple2('ほつれる', 'ほつれる'),
			_Utils_Tuple2('ほと', 'ほと'),
			_Utils_Tuple2('ほどう', '補導'),
			_Utils_Tuple2('ほどく', 'ほどく'),
			_Utils_Tuple2('ほとけ', '仏'),
			_Utils_Tuple2('ほとけさま', '仏様'),
			_Utils_Tuple2('ほどける', 'ほどける'),
			_Utils_Tuple2('ほどこす', '施す'),
			_Utils_Tuple2('ほどとおい', '程遠い'),
			_Utils_Tuple2('ぽとふ', 'ポトフ'),
			_Utils_Tuple2('ほとほと', 'ほとほと'),
			_Utils_Tuple2('ほとぼり', 'ほとぼり'),
			_Utils_Tuple2('ほとり', 'ほとり'),
			_Utils_Tuple2('ぼとるきーぷ', 'ボトルキープ'),
			_Utils_Tuple2('ぼにゅう', '母乳'),
			_Utils_Tuple2('ほにゅうるい', '哺乳類'),
			_Utils_Tuple2('ほねおり', '骨折り'),
			_Utils_Tuple2('ほねぐみ', '骨組み'),
			_Utils_Tuple2('ほのお', '炎'),
			_Utils_Tuple2('ほのか', 'ほのか'),
			_Utils_Tuple2('ほのぼの', 'ほのぼの'),
			_Utils_Tuple2('ほほえましい', 'ほほえましい'),
			_Utils_Tuple2('ほも', 'ホモ'),
			_Utils_Tuple2('ぼやぼや', 'ぼやぼや'),
			_Utils_Tuple2('ほよう', '保養'),
			_Utils_Tuple2('ほら', '洞'),
			_Utils_Tuple2('ほり', '堀'),
			_Utils_Tuple2('ほり', '彫り'),
			_Utils_Tuple2('ぽりえすてる', 'ポリエステル'),
			_Utils_Tuple2('ほりおこす', '掘り起こす'),
			_Utils_Tuple2('ほりさげる', '掘り下げる'),
			_Utils_Tuple2('ぽりしー', 'ポリシー'),
			_Utils_Tuple2('ほりだす', '掘り出す'),
			_Utils_Tuple2('ほる', '彫る'),
			_Utils_Tuple2('ほるだー', 'ホルダー'),
			_Utils_Tuple2('ぼると', 'ボルト'),
			_Utils_Tuple2('ぼると', 'ボルト'),
			_Utils_Tuple2('ぽるの', 'ポルノ'),
			_Utils_Tuple2('ほるん', 'ホルン'),
			_Utils_Tuple2('ほれこむ', 'ほれ込む'),
			_Utils_Tuple2('ほれる', 'ほれる'),
			_Utils_Tuple2('ぼろ', 'ぼろ'),
			_Utils_Tuple2('ぼろい', 'ぼろい'),
			_Utils_Tuple2('ほろびる', '滅びる'),
			_Utils_Tuple2('ほろぶ', '滅ぶ'),
			_Utils_Tuple2('ほろぼす', '滅ぼす'),
			_Utils_Tuple2('ぽろぽろ', 'ぽろぽろ'),
			_Utils_Tuple2('ほんい', '本意'),
			_Utils_Tuple2('ほんかく', '本格'),
			_Utils_Tuple2('ほんかくか', '本格化'),
			_Utils_Tuple2('ほんかくてき', '本格的'),
			_Utils_Tuple2('ほんきょ', '本拠'),
			_Utils_Tuple2('ほんぎょう', '本業'),
			_Utils_Tuple2('ほんけ', '本家'),
			_Utils_Tuple2('ぼんさい', '盆栽'),
			_Utils_Tuple2('ほんし', '本紙'),
			_Utils_Tuple2('ほんしつ', '本質'),
			_Utils_Tuple2('ほんしつてき', '本質的'),
			_Utils_Tuple2('ほんしょう', '本性'),
			_Utils_Tuple2('ほんしょく', '本職'),
			_Utils_Tuple2('ほんせき', '本籍'),
			_Utils_Tuple2('ほんせん', '本線'),
			_Utils_Tuple2('ほんたい', '本体'),
			_Utils_Tuple2('ほんだい', '本題'),
			_Utils_Tuple2('ほんど', '本土'),
			_Utils_Tuple2('ぼんど', 'ボンド'),
			_Utils_Tuple2('ぼんねっと', 'ボンネット'),
			_Utils_Tuple2('ほんのう', '本能'),
			_Utils_Tuple2('ぼんのう', '煩悩'),
			_Utils_Tuple2('ほんのうてき', '本能的'),
			_Utils_Tuple2('ほんのり', 'ほんのり'),
			_Utils_Tuple2('ほんば', '本場'),
			_Utils_Tuple2('ほんぶ', '本部'),
			_Utils_Tuple2('ほんぺん', '本編'),
			_Utils_Tuple2('ほんぽう', '奔放'),
			_Utils_Tuple2('ぼんぼん', 'ボンボン'),
			_Utils_Tuple2('ぽんぽん', 'ぽんぽん'),
			_Utils_Tuple2('ほんまつてんとう', '本末転倒'),
			_Utils_Tuple2('ほんるい', '本塁'),
			_Utils_Tuple2('ほんるいだ', '本塁打'),
			_Utils_Tuple2('ま', '魔'),
			_Utils_Tuple2('まーけてぃんぐ', 'マーケティング'),
			_Utils_Tuple2('まーち', 'マーチ'),
			_Utils_Tuple2('まーまれーど', 'マーマレード'),
			_Utils_Tuple2('まい', '舞'),
			_Utils_Tuple2('まいあがる', '舞い上がる'),
			_Utils_Tuple2('まいくろ', 'マイクロ'),
			_Utils_Tuple2('まいそう', '埋葬'),
			_Utils_Tuple2('まいなー', 'マイナー'),
			_Utils_Tuple2('まいぼつ', '埋没'),
			_Utils_Tuple2('まいり', '参り'),
			_Utils_Tuple2('まいんど', 'マインド'),
			_Utils_Tuple2('まう', '舞う'),
			_Utils_Tuple2('まうすぴーす', 'マウスピース'),
			_Utils_Tuple2('まえおき', '前置き'),
			_Utils_Tuple2('まえまえ', '前々'),
			_Utils_Tuple2('まえむき', '前向き'),
			_Utils_Tuple2('まおう', '魔王'),
			_Utils_Tuple2('まかい', '魔界'),
			_Utils_Tuple2('まかす', '任す'),
			_Utils_Tuple2('まかなう', '賄う'),
			_Utils_Tuple2('まかふしぎ', 'まか不思議'),
			_Utils_Tuple2('まき', '巻き'),
			_Utils_Tuple2('まき', '巻'),
			_Utils_Tuple2('まきあげる', '巻き上げる'),
			_Utils_Tuple2('まきおこす', '巻き起こす'),
			_Utils_Tuple2('まきこむ', '巻き込む'),
			_Utils_Tuple2('まきぞえ', '巻き添え'),
			_Utils_Tuple2('まきゅう', '魔球'),
			_Utils_Tuple2('まぎらわしい', '紛らわしい'),
			_Utils_Tuple2('まぎれ', '紛れ'),
			_Utils_Tuple2('まぎれこむ', '紛れ込む'),
			_Utils_Tuple2('まぎれる', '紛れる'),
			_Utils_Tuple2('まぎわ', '間際'),
			_Utils_Tuple2('まく', 'まく'),
			_Utils_Tuple2('まく', '膜'),
			_Utils_Tuple2('まくあけ', '幕開け'),
			_Utils_Tuple2('まくうち', '幕内'),
			_Utils_Tuple2('まくる', 'まくる'),
			_Utils_Tuple2('まけいぬ', '負け犬'),
			_Utils_Tuple2('まけこす', '負け越す'),
			_Utils_Tuple2('まごころ', '真心'),
			_Utils_Tuple2('まごつく', 'まごつく'),
			_Utils_Tuple2('まこと', 'まこと'),
			_Utils_Tuple2('まごのて', '孫の手'),
			_Utils_Tuple2('まさしく', '正しく'),
			_Utils_Tuple2('まさつ', '摩擦'),
			_Utils_Tuple2('まさゆめ', '正夢'),
			_Utils_Tuple2('まさる', '勝る'),
			_Utils_Tuple2('まじえる', '交える'),
			_Utils_Tuple2('まして', 'まして'),
			_Utils_Tuple2('まじょ', '魔女'),
			_Utils_Tuple2('ましょう', '魔性'),
			_Utils_Tuple2('まじわり', '交わり'),
			_Utils_Tuple2('まじわる', '交わる'),
			_Utils_Tuple2('ますい', '麻酔'),
			_Utils_Tuple2('ますから', 'マスカラ'),
			_Utils_Tuple2('ますこっと', 'マスコット'),
			_Utils_Tuple2('ますと', 'マスト'),
			_Utils_Tuple2('まずまず', 'まずまず'),
			_Utils_Tuple2('また', '股'),
			_Utils_Tuple2('まだい', '真だい'),
			_Utils_Tuple2('またがる', 'またがる'),
			_Utils_Tuple2('またぐ', 'またぐ'),
			_Utils_Tuple2('またした', '股下'),
			_Utils_Tuple2('またたく', '瞬く'),
			_Utils_Tuple2('まだむ', 'マダム'),
			_Utils_Tuple2('またもや', '又もや'),
			_Utils_Tuple2('まちうける', '待ち受ける'),
			_Utils_Tuple2('まちかまえる', '待ち構える'),
			_Utils_Tuple2('まちこがれる', '待ち焦がれる'),
			_Utils_Tuple2('まちどおしい', '待ち遠しい'),
			_Utils_Tuple2('まちなみ', '町並み'),
			_Utils_Tuple2('まちまち', 'まちまち'),
			_Utils_Tuple2('まっき', '末期'),
			_Utils_Tuple2('まっくらやみ', '真っ暗闇'),
			_Utils_Tuple2('まっこう', '真っ向'),
			_Utils_Tuple2('まっさつ', '抹殺'),
			_Utils_Tuple2('まっしょう', '抹消'),
			_Utils_Tuple2('まったん', '末端'),
			_Utils_Tuple2('まっと', 'マット'),
			_Utils_Tuple2('まっとう', 'まっとう'),
			_Utils_Tuple2('まっとれす', 'マットレス'),
			_Utils_Tuple2('まつび', '末尾'),
			_Utils_Tuple2('まっぷたつ', '真っ二つ'),
			_Utils_Tuple2('まつる', '祭る'),
			_Utils_Tuple2('まと', '的'),
			_Utils_Tuple2('まどごし', '窓越し'),
			_Utils_Tuple2('まとはずれ', '的外れ'),
			_Utils_Tuple2('まとも', 'まとも'),
			_Utils_Tuple2('まどわく', '窓枠'),
			_Utils_Tuple2('まどわす', '惑わす'),
			_Utils_Tuple2('まなざし', 'まなざし'),
			_Utils_Tuple2('まぬがれる', '免れる'),
			_Utils_Tuple2('まぬけ', '間抜け'),
			_Utils_Tuple2('まねーじめんと', 'マネージメント'),
			_Utils_Tuple2('まねき', '招き'),
			_Utils_Tuple2('まねきねこ', '招き猫'),
			_Utils_Tuple2('まのあたり', '目の当たり'),
			_Utils_Tuple2('まひ', 'まひ'),
			_Utils_Tuple2('まふぃあ', 'マフィア'),
			_Utils_Tuple2('まぶす', 'まぶす'),
			_Utils_Tuple2('まぶた', 'まぶた'),
			_Utils_Tuple2('まほう', '魔法'),
			_Utils_Tuple2('まぼろし', '幻'),
			_Utils_Tuple2('ままはは', 'まま母'),
			_Utils_Tuple2('まみず', '真水'),
			_Utils_Tuple2('まみれる', 'まみれる'),
			_Utils_Tuple2('まめ', 'まめ'),
			_Utils_Tuple2('まもの', '魔物'),
			_Utils_Tuple2('まもりがみ', '守り神'),
			_Utils_Tuple2('まよけ', '魔よけ'),
			_Utils_Tuple2('まりあ', 'マリア'),
			_Utils_Tuple2('まりね', 'マリネ'),
			_Utils_Tuple2('まる', '丸'),
			_Utils_Tuple2('まる', '丸'),
			_Utils_Tuple2('まるきり', 'まるきり'),
			_Utils_Tuple2('まるごと', '丸ごと'),
			_Utils_Tuple2('まるた', '丸太'),
			_Utils_Tuple2('まるだし', '丸出し'),
			_Utils_Tuple2('まるっこい', '丸っこい'),
			_Utils_Tuple2('まるまる', '丸々'),
			_Utils_Tuple2('まるまる', '丸まる'),
			_Utils_Tuple2('まるまる', '丸々'),
			_Utils_Tuple2('まるみ', '丸み'),
			_Utils_Tuple2('まるみえ', '丸見え'),
			_Utils_Tuple2('まるめる', '丸める'),
			_Utils_Tuple2('まろやか', 'まろやか'),
			_Utils_Tuple2('まわし', '回し'),
			_Utils_Tuple2('まわし', '回し'),
			_Utils_Tuple2('まんえん', 'まんえん'),
			_Utils_Tuple2('まんき', '満期'),
			_Utils_Tuple2('まんきつ', '満喫'),
			_Utils_Tuple2('まんさい', '満載'),
			_Utils_Tuple2('まんざい', '漫才'),
			_Utils_Tuple2('まんせい', '慢性'),
			_Utils_Tuple2('まんたん', '満タン'),
			_Utils_Tuple2('まんてん', '満天'),
			_Utils_Tuple2('まんねり', 'マンネリ'),
			_Utils_Tuple2('まんびょう', '万病'),
			_Utils_Tuple2('まんほーる', 'マンホール'),
			_Utils_Tuple2('まんまと', 'まんまと'),
			_Utils_Tuple2('まんようしゅう', '万葉集'),
			_Utils_Tuple2('まんるい', '満塁'),
			_Utils_Tuple2('み', 'み'),
			_Utils_Tuple2('みあたる', '見当たる'),
			_Utils_Tuple2('みあわせる', '見合わせる'),
			_Utils_Tuple2('みうける', '見受ける'),
			_Utils_Tuple2('みうごき', '身動き'),
			_Utils_Tuple2('みうり', '身売り'),
			_Utils_Tuple2('みえ', 'みえ'),
			_Utils_Tuple2('みえかくれ', '見え隠れ'),
			_Utils_Tuple2('みおさめ', '見納め'),
			_Utils_Tuple2('みおとす', '見落とす'),
			_Utils_Tuple2('みおとり', '見劣り'),
			_Utils_Tuple2('みかい', '未開'),
			_Utils_Tuple2('みかいけつ', '未解決'),
			_Utils_Tuple2('みかえす', '見返す'),
			_Utils_Tuple2('みかえり', '見返り'),
			_Utils_Tuple2('みかける', '見掛ける'),
			_Utils_Tuple2('みがら', '身柄'),
			_Utils_Tuple2('みがわり', '身代わり'),
			_Utils_Tuple2('みかん', '未完'),
			_Utils_Tuple2('みき', '幹'),
			_Utils_Tuple2('みきる', '見切る'),
			_Utils_Tuple2('みきわめる', '見極める'),
			_Utils_Tuple2('みくだす', '見下す'),
			_Utils_Tuple2('みぐるしい', '見苦しい'),
			_Utils_Tuple2('みくろ', 'ミクロ'),
			_Utils_Tuple2('みこす', '見越す'),
			_Utils_Tuple2('みこみ', '見込み'),
			_Utils_Tuple2('みこむ', '見込む'),
			_Utils_Tuple2('みこん', '未婚'),
			_Utils_Tuple2('みさいる', 'ミサイル'),
			_Utils_Tuple2('みさかい', '見境'),
			_Utils_Tuple2('みさき', '岬'),
			_Utils_Tuple2('みじめ', '惨め'),
			_Utils_Tuple2('みしん', 'ミシン'),
			_Utils_Tuple2('みじん', 'みじん'),
			_Utils_Tuple2('みす', 'ミス'),
			_Utils_Tuple2('みずあめ', '水あめ'),
			_Utils_Tuple2('みすい', '未遂'),
			_Utils_Tuple2('みすえる', '見据える'),
			_Utils_Tuple2('みずかげん', '水加減'),
			_Utils_Tuple2('みずきり', '水切り'),
			_Utils_Tuple2('みずけ', '水気'),
			_Utils_Tuple2('みすごす', '見過ごす'),
			_Utils_Tuple2('みずしげん', '水資源'),
			_Utils_Tuple2('みずしょうばい', '水商売'),
			_Utils_Tuple2('みずたき', '水炊き'),
			_Utils_Tuple2('みすてる', '見捨てる'),
			_Utils_Tuple2('みずに', '水煮'),
			_Utils_Tuple2('みずのあわ', '水の泡'),
			_Utils_Tuple2('みずびたし', '水浸し'),
			_Utils_Tuple2('みずべ', '水辺'),
			_Utils_Tuple2('みずまし', '水増し'),
			_Utils_Tuple2('みずまわり', '水回り'),
			_Utils_Tuple2('みずむし', '水虫'),
			_Utils_Tuple2('みずわり', '水割り'),
			_Utils_Tuple2('みせかける', '見せ掛ける'),
			_Utils_Tuple2('みせば', '見せ場'),
			_Utils_Tuple2('みせもの', '見世物'),
			_Utils_Tuple2('みせる', '魅せる'),
			_Utils_Tuple2('みぜん', '未然'),
			_Utils_Tuple2('みぞ', '溝'),
			_Utils_Tuple2('みぞおち', 'みぞおち'),
			_Utils_Tuple2('みたす', '満たす'),
			_Utils_Tuple2('みだす', '乱す'),
			_Utils_Tuple2('みたてる', '見立てる'),
			_Utils_Tuple2('みだれる', '乱れる'),
			_Utils_Tuple2('みち', '未知'),
			_Utils_Tuple2('みちがえる', '見違える'),
			_Utils_Tuple2('みちかけ', '満ち欠け'),
			_Utils_Tuple2('みちしるべ', '道しるべ'),
			_Utils_Tuple2('みちすう', '未知数'),
			_Utils_Tuple2('みちすじ', '道筋'),
			_Utils_Tuple2('みちづれ', '道連れ'),
			_Utils_Tuple2('みちばた', '道端'),
			_Utils_Tuple2('みちびく', '導く'),
			_Utils_Tuple2('みちる', '満ちる'),
			_Utils_Tuple2('みつ', '蜜'),
			_Utils_Tuple2('みつ', '密'),
			_Utils_Tuple2('みつ', '満つ'),
			_Utils_Tuple2('みっしつ', '密室'),
			_Utils_Tuple2('みっしゅう', '密集'),
			_Utils_Tuple2('みっしょん', 'ミッション'),
			_Utils_Tuple2('みっせつ', '密接'),
			_Utils_Tuple2('みっちゃく', '密着'),
			_Utils_Tuple2('みつど', '密度'),
			_Utils_Tuple2('みつば', '三つ葉'),
			_Utils_Tuple2('みっぷう', '密封'),
			_Utils_Tuple2('みっぺい', '密閉'),
			_Utils_Tuple2('みつもり', '見積もり'),
			_Utils_Tuple2('みつもる', '見積もる'),
			_Utils_Tuple2('みつりん', '密林'),
			_Utils_Tuple2('みでぃあむ', 'ミディアム'),
			_Utils_Tuple2('みとおし', '見通し'),
			_Utils_Tuple2('みとおす', '見通す'),
			_Utils_Tuple2('みとどける', '見届ける'),
			_Utils_Tuple2('みなげ', '身投げ'),
			_Utils_Tuple2('みなごろし', '皆殺し'),
			_Utils_Tuple2('みなす', '見なす'),
			_Utils_Tuple2('みなもと', '源'),
			_Utils_Tuple2('みならい', '見習い'),
			_Utils_Tuple2('みならう', '見習う'),
			_Utils_Tuple2('みぬく', '見抜く'),
			_Utils_Tuple2('みねらる', 'ミネラル'),
			_Utils_Tuple2('みのう', '未納'),
			_Utils_Tuple2('みのがす', '見逃す'),
			_Utils_Tuple2('みのまわり', '身の回り'),
			_Utils_Tuple2('みのる', '実る'),
			_Utils_Tuple2('みばえ', '見栄え'),
			_Utils_Tuple2('みはからう', '見計らう'),
			_Utils_Tuple2('みはらい', '未払い'),
			_Utils_Tuple2('みはらし', '見晴らし'),
			_Utils_Tuple2('みはらす', '見晴らす'),
			_Utils_Tuple2('みはる', '見張る'),
			_Utils_Tuple2('みぶり', '身振り'),
			_Utils_Tuple2('みぼうじん', '未亡人'),
			_Utils_Tuple2('みまう', '見舞う'),
			_Utils_Tuple2('みみうち', '耳打ち'),
			_Utils_Tuple2('みみざわり', '耳障り'),
			_Utils_Tuple2('みみより', '耳寄り'),
			_Utils_Tuple2('みもと', '身元'),
			_Utils_Tuple2('みもの', '見物'),
			_Utils_Tuple2('みゃく', '脈'),
			_Utils_Tuple2('みやこ', '都'),
			_Utils_Tuple2('みやぶる', '見破る'),
			_Utils_Tuple2('みょう', '妙'),
			_Utils_Tuple2('みょう', '妙'),
			_Utils_Tuple2('みより', '身寄り'),
			_Utils_Tuple2('みりおねあ', 'ミリオネア'),
			_Utils_Tuple2('みりおん', 'ミリオン'),
			_Utils_Tuple2('みりょう', '魅了'),
			_Utils_Tuple2('みりん', 'みりん'),
			_Utils_Tuple2('みるみる', '見る見る'),
			_Utils_Tuple2('みれん', '未練'),
			_Utils_Tuple2('みれんがましい', '未練がましい'),
			_Utils_Tuple2('みわける', '見分ける'),
			_Utils_Tuple2('みんえい', '民営'),
			_Utils_Tuple2('みんか', '民家'),
			_Utils_Tuple2('みんかん', '民間'),
			_Utils_Tuple2('みんげい', '民芸'),
			_Utils_Tuple2('みんげいひん', '民芸品'),
			_Utils_Tuple2('みんじ', '民事'),
			_Utils_Tuple2('みんじさいばん', '民事裁判'),
			_Utils_Tuple2('みんじそしょう', '民事訴訟'),
			_Utils_Tuple2('みんしゅ', '民主'),
			_Utils_Tuple2('みんしゅう', '民衆'),
			_Utils_Tuple2('みんしゅしゅぎ', '民主主義'),
			_Utils_Tuple2('みんしゅてき', '民主的'),
			_Utils_Tuple2('みんぞく', '民俗'),
			_Utils_Tuple2('みんぞくがく', '民俗学'),
			_Utils_Tuple2('みんち', 'ミンチ'),
			_Utils_Tuple2('みんぽう', '民法'),
			_Utils_Tuple2('みんぽう', '民放'),
			_Utils_Tuple2('みんよう', '民謡'),
			_Utils_Tuple2('みんわ', '民話'),
			_Utils_Tuple2('むいしき', '無意識'),
			_Utils_Tuple2('むーん', 'ムーン'),
			_Utils_Tuple2('むーんらいと', 'ムーンライト'),
			_Utils_Tuple2('むえん', '無縁'),
			_Utils_Tuple2('むえん', '無縁'),
			_Utils_Tuple2('むおん', '無音'),
			_Utils_Tuple2('むかいかぜ', '向かい風'),
			_Utils_Tuple2('むかえいれる', '迎え入れる'),
			_Utils_Tuple2('むき', '無期'),
			_Utils_Tuple2('むき', '無機'),
			_Utils_Tuple2('むきしつ', '無機質'),
			_Utils_Tuple2('むきず', '無傷'),
			_Utils_Tuple2('むきちょうえき', '無期懲役'),
			_Utils_Tuple2('むきめい', '無記名'),
			_Utils_Tuple2('むきん', '無菌'),
			_Utils_Tuple2('むく', 'むく'),
			_Utils_Tuple2('むくいる', '報いる'),
			_Utils_Tuple2('むくみ', 'むくみ'),
			_Utils_Tuple2('むくむ', 'むくむ'),
			_Utils_Tuple2('むこ', '婿'),
			_Utils_Tuple2('むこう', '無効'),
			_Utils_Tuple2('むざい', '無罪'),
			_Utils_Tuple2('むさくい', '無作為'),
			_Utils_Tuple2('むさべつ', '無差別'),
			_Utils_Tuple2('むさぼる', '貪る'),
			_Utils_Tuple2('むざん', '無残'),
			_Utils_Tuple2('むし', '蒸し'),
			_Utils_Tuple2('むし', '蒸し'),
			_Utils_Tuple2('むじ', '無地'),
			_Utils_Tuple2('むしかえす', '蒸し返す'),
			_Utils_Tuple2('むしき', '蒸し器'),
			_Utils_Tuple2('むしくい', '虫食い'),
			_Utils_Tuple2('むじつ', '無実'),
			_Utils_Tuple2('むしぶろ', '蒸しぶろ'),
			_Utils_Tuple2('むしゃ', '武者'),
			_Utils_Tuple2('むしやき', '蒸し焼き'),
			_Utils_Tuple2('むじゃき', '無邪気'),
			_Utils_Tuple2('むしゃくしゃ', 'むしゃくしゃ'),
			_Utils_Tuple2('むじゅうりょく', '無重力'),
			_Utils_Tuple2('むじゅん', '矛盾'),
			_Utils_Tuple2('むしょう', '無償'),
			_Utils_Tuple2('むじょう', '無情'),
			_Utils_Tuple2('むじょうけんこうふく', '無条件降伏'),
			_Utils_Tuple2('むしょうに', '無性に'),
			_Utils_Tuple2('むしよけ', '虫よけ'),
			_Utils_Tuple2('むしょぞく', '無所属'),
			_Utils_Tuple2('むじるし', '無印'),
			_Utils_Tuple2('むしんけい', '無神経'),
			_Utils_Tuple2('むすび', '結び'),
			_Utils_Tuple2('むすびつき', '結び付き'),
			_Utils_Tuple2('むすびつく', '結び付く'),
			_Utils_Tuple2('むすびめ', '結び目'),
			_Utils_Tuple2('むせん', '無線'),
			_Utils_Tuple2('むせんつうしん', '無線通信'),
			_Utils_Tuple2('むぞうさ', '無造作'),
			_Utils_Tuple2('むち', '無知'),
			_Utils_Tuple2('むちゃ', '無茶'),
			_Utils_Tuple2('むちゃくちゃ', '無茶苦茶'),
			_Utils_Tuple2('むっつり', 'むっつり'),
			_Utils_Tuple2('むてき', '無敵'),
			_Utils_Tuple2('むとうは', '無党派'),
			_Utils_Tuple2('むないた', '胸板'),
			_Utils_Tuple2('むなしい', 'むなしい'),
			_Utils_Tuple2('むね', '旨'),
			_Utils_Tuple2('むねん', '無念'),
			_Utils_Tuple2('むのう', '無能'),
			_Utils_Tuple2('むのうりょく', '無能力'),
			_Utils_Tuple2('むぼう', '無謀'),
			_Utils_Tuple2('むぼうび', '無防備'),
			_Utils_Tuple2('むめんきょ', '無免許'),
			_Utils_Tuple2('むやみ', 'むやみ'),
			_Utils_Tuple2('むよう', '無用'),
			_Utils_Tuple2('むらがる', '群がる'),
			_Utils_Tuple2('むらす', '蒸らす'),
			_Utils_Tuple2('むれ', '群れ'),
			_Utils_Tuple2('むれる', '蒸れる'),
			_Utils_Tuple2('むれる', '群れる'),
			_Utils_Tuple2('むろん', '無論'),
			_Utils_Tuple2('め', 'め'),
			_Utils_Tuple2('めい', '明'),
			_Utils_Tuple2('めいあん', '明暗'),
			_Utils_Tuple2('めいうん', '命運'),
			_Utils_Tuple2('めいおうせい', '冥王星'),
			_Utils_Tuple2('めいかい', '明快'),
			_Utils_Tuple2('めいがら', '銘柄'),
			_Utils_Tuple2('めいき', '明記'),
			_Utils_Tuple2('めいぎ', '名義'),
			_Utils_Tuple2('めいげん', '名言'),
			_Utils_Tuple2('めいげん', '明言'),
			_Utils_Tuple2('めいごさん', 'めいごさん'),
			_Utils_Tuple2('めいさい', '明細'),
			_Utils_Tuple2('めいじ', '明示'),
			_Utils_Tuple2('めいじいしん', '明治維新'),
			_Utils_Tuple2('めいしょう', '名称'),
			_Utils_Tuple2('めいずる', '命ずる'),
			_Utils_Tuple2('めいせい', '名声'),
			_Utils_Tuple2('めいにち', '命日'),
			_Utils_Tuple2('めいはく', '明白'),
			_Utils_Tuple2('めいばん', '名盤'),
			_Utils_Tuple2('めいふく', '冥福'),
			_Utils_Tuple2('めいめい', '命名'),
			_Utils_Tuple2('めいめい', '銘々'),
			_Utils_Tuple2('めいもく', '名目'),
			_Utils_Tuple2('めいもん', '名門'),
			_Utils_Tuple2('めいよ', '名誉'),
			_Utils_Tuple2('めいりょう', '明りょう'),
			_Utils_Tuple2('めおと', 'めおと'),
			_Utils_Tuple2('めか', 'メカ'),
			_Utils_Tuple2('めが', 'メガ'),
			_Utils_Tuple2('めかくし', '目隠し'),
			_Utils_Tuple2('めがしら', '目頭'),
			_Utils_Tuple2('めかにずむ', 'メカニズム'),
			_Utils_Tuple2('めかにっく', 'メカニック'),
			_Utils_Tuple2('めがへるつ', 'メガヘルツ'),
			_Utils_Tuple2('めがみ', '女神'),
			_Utils_Tuple2('めぐみ', '恵み'),
			_Utils_Tuple2('めぐらす', '巡らす'),
			_Utils_Tuple2('めぐり', '巡り'),
			_Utils_Tuple2('めぐりあう', '巡り会う'),
			_Utils_Tuple2('めくる', 'めくる'),
			_Utils_Tuple2('めぐる', '巡る'),
			_Utils_Tuple2('めげる', 'めげる'),
			_Utils_Tuple2('めさき', '目先'),
			_Utils_Tuple2('めざましい', '目覚ましい'),
			_Utils_Tuple2('めざめ', '目覚め'),
			_Utils_Tuple2('めざわり', '目障り'),
			_Utils_Tuple2('めしつかい', '召し使い'),
			_Utils_Tuple2('めじり', '目尻'),
			_Utils_Tuple2('めじろ', '目白'),
			_Utils_Tuple2('めす', 'メス'),
			_Utils_Tuple2('めす', '召す'),
			_Utils_Tuple2('めたりっく', 'メタリック'),
			_Utils_Tuple2('めたる', 'メタル'),
			_Utils_Tuple2('めちゃ', '目茶'),
			_Utils_Tuple2('めっき', 'めっき'),
			_Utils_Tuple2('めつき', '目付き'),
			_Utils_Tuple2('めっせんじゃー', 'メッセンジャー'),
			_Utils_Tuple2('めつぼう', '滅亡'),
			_Utils_Tuple2('めど', '目処'),
			_Utils_Tuple2('めとろのーむ', 'メトロノーム'),
			_Utils_Tuple2('めのたま', '目の玉'),
			_Utils_Tuple2('めばえる', '芽生える'),
			_Utils_Tuple2('めぶんりょう', '目分量'),
			_Utils_Tuple2('めまい', 'めまい'),
			_Utils_Tuple2('めまぐるしい', '目まぐるしい'),
			_Utils_Tuple2('めもり', '目盛り'),
			_Utils_Tuple2('めもりある', 'メモリアル'),
			_Utils_Tuple2('めらにん', 'メラニン'),
			_Utils_Tuple2('めりっと', 'メリット'),
			_Utils_Tuple2('めろでぃー', 'メロディー'),
			_Utils_Tuple2('めん', '面'),
			_Utils_Tuple2('めん', '免'),
			_Utils_Tuple2('めんえき', '免疫'),
			_Utils_Tuple2('めんかい', '面会'),
			_Utils_Tuple2('めんざい', '免罪'),
			_Utils_Tuple2('めんしき', '面識'),
			_Utils_Tuple2('めんじょ', '免除'),
			_Utils_Tuple2('めんじょう', '免状'),
			_Utils_Tuple2('めんしょく', '免職'),
			_Utils_Tuple2('めんする', '面する'),
			_Utils_Tuple2('めんつ', 'メンツ'),
			_Utils_Tuple2('めんてなんす', 'メンテナンス'),
			_Utils_Tuple2('めんぼう', '綿棒'),
			_Utils_Tuple2('めんみつ', '綿密'),
			_Utils_Tuple2('も', '喪'),
			_Utils_Tuple2('も', '藻'),
			_Utils_Tuple2('もう', '網'),
			_Utils_Tuple2('もう', '猛'),
			_Utils_Tuple2('もうい', '猛威'),
			_Utils_Tuple2('もうける', '設ける'),
			_Utils_Tuple2('もうしいれる', '申し入れる'),
			_Utils_Tuple2('もうしたて', '申し立て'),
			_Utils_Tuple2('もうしたてる', '申し立てる'),
			_Utils_Tuple2('もうしで', '申し出'),
			_Utils_Tuple2('もうしでる', '申し出る'),
			_Utils_Tuple2('もうしぶん', '申し分'),
			_Utils_Tuple2('もうじゅう', '猛獣'),
			_Utils_Tuple2('もうしょ', '猛暑'),
			_Utils_Tuple2('もうそう', '妄想'),
			_Utils_Tuple2('もうだ', '猛打'),
			_Utils_Tuple2('もうちょい', 'もうちょい'),
			_Utils_Tuple2('もうとう', '毛頭'),
			_Utils_Tuple2('もうどうけん', '盲導犬'),
			_Utils_Tuple2('もうひつ', '毛筆'),
			_Utils_Tuple2('もうまく', '網膜'),
			_Utils_Tuple2('もうもく', '盲目'),
			_Utils_Tuple2('もうら', '網羅'),
			_Utils_Tuple2('もうれつ', '猛烈'),
			_Utils_Tuple2('もえつきる', '燃え尽きる'),
			_Utils_Tuple2('もーしょん', 'モーション'),
			_Utils_Tuple2('もーたーかー', 'モーターカー'),
			_Utils_Tuple2('もーたーしょー', 'モーターショー'),
			_Utils_Tuple2('もーど', 'モード'),
			_Utils_Tuple2('もか', 'モカ'),
			_Utils_Tuple2('もがく', 'もがく'),
			_Utils_Tuple2('もぎ', '模擬'),
			_Utils_Tuple2('もぎとる', 'もぎ取る'),
			_Utils_Tuple2('もくげき', '目撃'),
			_Utils_Tuple2('もくせい', '木星'),
			_Utils_Tuple2('もくたん', '木炭'),
			_Utils_Tuple2('もくてきいしき', '目的意識'),
			_Utils_Tuple2('もくにん', '黙認'),
			_Utils_Tuple2('もくば', '木馬'),
			_Utils_Tuple2('もくもく', '黙々'),
			_Utils_Tuple2('もぐり', '潜り'),
			_Utils_Tuple2('もぐる', '潜る'),
			_Utils_Tuple2('もけい', '模型'),
			_Utils_Tuple2('もざいく', 'モザイク'),
			_Utils_Tuple2('もさく', '模索'),
			_Utils_Tuple2('もしか', 'もしか'),
			_Utils_Tuple2('もしくは', 'もしくは'),
			_Utils_Tuple2('もじばん', '文字盤'),
			_Utils_Tuple2('もしゃ', '模写'),
			_Utils_Tuple2('もしゅ', '喪主'),
			_Utils_Tuple2('もすく', 'モスク'),
			_Utils_Tuple2('もたもた', 'もたもた'),
			_Utils_Tuple2('もたらす', 'もたらす'),
			_Utils_Tuple2('もたれる', 'もたれる'),
			_Utils_Tuple2('もち', '持ち'),
			_Utils_Tuple2('もちあじ', '持ち味'),
			_Utils_Tuple2('もちあわせる', '持ち合わせる'),
			_Utils_Tuple2('もちいえ', '持ち家'),
			_Utils_Tuple2('もちーふ', 'モチーフ'),
			_Utils_Tuple2('もちかける', '持ち掛ける'),
			_Utils_Tuple2('もちきり', '持ち切り'),
			_Utils_Tuple2('もちこす', '持ち越す'),
			_Utils_Tuple2('もちにげ', '持ち逃げ'),
			_Utils_Tuple2('もちば', '持ち場'),
			_Utils_Tuple2('もちべーしょん', 'モチベーション'),
			_Utils_Tuple2('もちゅう', '喪中'),
			_Utils_Tuple2('もっか', '目下'),
			_Utils_Tuple2('もっこう', '木工'),
			_Utils_Tuple2('もってのほか', 'もってのほか'),
			_Utils_Tuple2('もっとー', 'モットー'),
			_Utils_Tuple2('もっとも', 'もっとも'),
			_Utils_Tuple2('もっとも', 'もっとも'),
			_Utils_Tuple2('もっぱら', '専ら'),
			_Utils_Tuple2('もてあます', '持て余す'),
			_Utils_Tuple2('もてなす', 'もてなす'),
			_Utils_Tuple2('もてはやす', 'もてはやす'),
			_Utils_Tuple2('もでるちぇんじ', 'モデルチェンジ'),
			_Utils_Tuple2('もどかしい', 'もどかしい'),
			_Utils_Tuple2('もとせん', '元栓'),
			_Utils_Tuple2('もとで', '元手'),
			_Utils_Tuple2('もとより', 'もとより'),
			_Utils_Tuple2('もにたー', 'モニター'),
			_Utils_Tuple2('もにたー', 'モニター'),
			_Utils_Tuple2('ものいい', '物言い'),
			_Utils_Tuple2('ものうり', '物売り'),
			_Utils_Tuple2('ものかげ', '物陰'),
			_Utils_Tuple2('ものがたる', '物語る'),
			_Utils_Tuple2('ものくろ', 'モノクロ'),
			_Utils_Tuple2('ものごころ', '物心'),
			_Utils_Tuple2('ものしずか', '物静か'),
			_Utils_Tuple2('ものずき', '物好き'),
			_Utils_Tuple2('ものの', '物の'),
			_Utils_Tuple2('もはや', 'もはや'),
			_Utils_Tuple2('もはん', '模範'),
			_Utils_Tuple2('もふく', '喪服'),
			_Utils_Tuple2('もほう', '模倣'),
			_Utils_Tuple2('もめる', 'もめる'),
			_Utils_Tuple2('ももたろう', '桃太郎'),
			_Utils_Tuple2('もものせっく', '桃の節句'),
			_Utils_Tuple2('もよおし', '催し'),
			_Utils_Tuple2('もよおす', '催す'),
			_Utils_Tuple2('もらす', '漏らす'),
			_Utils_Tuple2('もりあわせ', '盛り合わせ'),
			_Utils_Tuple2('もりこむ', '盛り込む'),
			_Utils_Tuple2('もりもり', 'もりもり'),
			_Utils_Tuple2('もれ', '漏れ'),
			_Utils_Tuple2('もれる', '漏れる'),
			_Utils_Tuple2('もろい', 'もろい'),
			_Utils_Tuple2('もろもろ', 'もろもろ'),
			_Utils_Tuple2('もんしょう', '紋章'),
			_Utils_Tuple2('もんすたー', 'モンスター'),
			_Utils_Tuple2('もんぜん', '門前'),
			_Utils_Tuple2('もんたーじゅ', 'モンタージュ'),
			_Utils_Tuple2('もんだいいしき', '問題意識'),
			_Utils_Tuple2('もんどう', '問答'),
			_Utils_Tuple2('もんぶかがくしょう', '文部科学省'),
			_Utils_Tuple2('もんぶしょう', '文部省'),
			_Utils_Tuple2('や', '矢'),
			_Utils_Tuple2('やおちょう', '八百長'),
			_Utils_Tuple2('やかた', '館'),
			_Utils_Tuple2('やききる', '焼き切る'),
			_Utils_Tuple2('やきつけ', '焼き付け'),
			_Utils_Tuple2('やきつける', '焼き付ける'),
			_Utils_Tuple2('やきはらう', '焼き払う'),
			_Utils_Tuple2('やきもき', 'やきもき'),
			_Utils_Tuple2('やきもの', '焼き物'),
			_Utils_Tuple2('やくがら', '役柄'),
			_Utils_Tuple2('やくざい', '薬剤'),
			_Utils_Tuple2('やくざいし', '薬剤師'),
			_Utils_Tuple2('やくしょく', '役職'),
			_Utils_Tuple2('やくどころ', '役所'),
			_Utils_Tuple2('やくにん', '役人'),
			_Utils_Tuple2('やくば', '役場'),
			_Utils_Tuple2('やくぶつ', '薬物'),
			_Utils_Tuple2('やくぶん', '訳文'),
			_Utils_Tuple2('やくみ', '薬味'),
			_Utils_Tuple2('やけあと', '焼け跡'),
			_Utils_Tuple2('やさき', '矢先'),
			_Utils_Tuple2('やし', 'やし'),
			_Utils_Tuple2('やじ', '野次'),
			_Utils_Tuple2('やしき', '屋敷'),
			_Utils_Tuple2('やしなう', '養う'),
			_Utils_Tuple2('やしゅ', '野手'),
			_Utils_Tuple2('やすあがり', '安上がり'),
			_Utils_Tuple2('やすね', '安値'),
			_Utils_Tuple2('やすやす', 'やすやす'),
			_Utils_Tuple2('やすらぎ', '安らぎ'),
			_Utils_Tuple2('やすり', 'やすり'),
			_Utils_Tuple2('やせい', '野生'),
			_Utils_Tuple2('やたら', 'やたら'),
			_Utils_Tuple2('やちょう', '野鳥'),
			_Utils_Tuple2('やっかい', '厄介'),
			_Utils_Tuple2('やっつける', 'やっつける'),
			_Utils_Tuple2('やつら', 'やつら'),
			_Utils_Tuple2('やつれる', 'やつれる'),
			_Utils_Tuple2('やとう', '野党'),
			_Utils_Tuple2('やどや', '宿屋'),
			_Utils_Tuple2('やどる', '宿る'),
			_Utils_Tuple2('やなぎ', '柳'),
			_Utils_Tuple2('やに', 'やに'),
			_Utils_Tuple2('やばん', '野蛮'),
			_Utils_Tuple2('やぼ', '野暮'),
			_Utils_Tuple2('やぼったい', '野暮ったい'),
			_Utils_Tuple2('やまがた', '山形'),
			_Utils_Tuple2('やまぐに', '山国'),
			_Utils_Tuple2('やまごえ', '山越え'),
			_Utils_Tuple2('やまざと', '山里'),
			_Utils_Tuple2('やまなみ', '山並み'),
			_Utils_Tuple2('やまねこ', '山猫'),
			_Utils_Tuple2('やまのて', '山の手'),
			_Utils_Tuple2('やまびこ', '山びこ'),
			_Utils_Tuple2('やまぶきいろ', '山吹色'),
			_Utils_Tuple2('やまほど', '山ほど'),
			_Utils_Tuple2('やまわけ', '山分け'),
			_Utils_Tuple2('やみ', '闇'),
			_Utils_Tuple2('やむ', '病む'),
			_Utils_Tuple2('ややこしい', 'ややこしい'),
			_Utils_Tuple2('やらかす', 'やらかす'),
			_Utils_Tuple2('やり', 'やり'),
			_Utils_Tuple2('やりがい', 'やりがい'),
			_Utils_Tuple2('やりくり', 'やりくり'),
			_Utils_Tuple2('やるき', 'やる気'),
			_Utils_Tuple2('やれ', 'やれ'),
			_Utils_Tuple2('やろう', '野郎'),
			_Utils_Tuple2('やわらぐ', '和らぐ'),
			_Utils_Tuple2('やわらげる', '和らげる'),
			_Utils_Tuple2('やんちゃ', 'やんちゃ'),
			_Utils_Tuple2('やんわり', 'やんわり'),
			_Utils_Tuple2('ゆいごん', '遺言'),
			_Utils_Tuple2('ゆいごんしょ', '遺言書'),
			_Utils_Tuple2('ゆいのう', '結納'),
			_Utils_Tuple2('ゆう', '結う'),
			_Utils_Tuple2('ゆうい', '優位'),
			_Utils_Tuple2('ゆういぎ', '有意義'),
			_Utils_Tuple2('ゆううつ', 'ゆううつ'),
			_Utils_Tuple2('ゆうえき', '有益'),
			_Utils_Tuple2('ゆうえつ', '優越'),
			_Utils_Tuple2('ゆうえつかん', '優越感'),
			_Utils_Tuple2('ゆうが', '優雅'),
			_Utils_Tuple2('ゆうかい', '誘拐'),
			_Utils_Tuple2('ゆうかん', '勇敢'),
			_Utils_Tuple2('ゆうぎ', '遊戯'),
			_Utils_Tuple2('ゆうきぶつ', '有機物'),
			_Utils_Tuple2('ゆうきゅう', '有給'),
			_Utils_Tuple2('ゆうきゅうきゅうか', '有給休暇'),
			_Utils_Tuple2('ゆうぐう', '優遇'),
			_Utils_Tuple2('ゆうけんしゃ', '有権者'),
			_Utils_Tuple2('ゆうごう', '融合'),
			_Utils_Tuple2('ゆうざい', '有罪'),
			_Utils_Tuple2('ゆうし', '融資'),
			_Utils_Tuple2('ゆうし', '有志'),
			_Utils_Tuple2('ゆうしゃ', '勇者'),
			_Utils_Tuple2('ゆうじゅうふだん', '優柔不断'),
			_Utils_Tuple2('ゆうしょくじんしゅ', '有色人種'),
			_Utils_Tuple2('ゆうすう', '有数'),
			_Utils_Tuple2('ゆうずう', '融通'),
			_Utils_Tuple2('ゆうすずみ', '夕涼み'),
			_Utils_Tuple2('ゆーすほすてる', 'ユースホステル'),
			_Utils_Tuple2('ゆうする', '有する'),
			_Utils_Tuple2('ゆうせい', '優勢'),
			_Utils_Tuple2('ゆうせい', '郵政'),
			_Utils_Tuple2('ゆうせい', '郵政'),
			_Utils_Tuple2('ゆうせいしょう', '郵政省'),
			_Utils_Tuple2('ゆうせん', '有線'),
			_Utils_Tuple2('ゆうたい', '優待'),
			_Utils_Tuple2('ゆうだい', '雄大'),
			_Utils_Tuple2('ゆうてん', '融点'),
			_Utils_Tuple2('ゆうどう', '誘導'),
			_Utils_Tuple2('ゆうどく', '有毒'),
			_Utils_Tuple2('ゆうふく', '裕福'),
			_Utils_Tuple2('ゆうべん', '雄弁'),
			_Utils_Tuple2('ゆうぼう', '有望'),
			_Utils_Tuple2('ゆうぼく', '遊牧'),
			_Utils_Tuple2('ゆうほどう', '遊歩道'),
			_Utils_Tuple2('ゆーもらす', 'ユーモラス'),
			_Utils_Tuple2('ゆうやみ', '夕闇'),
			_Utils_Tuple2('ゆうゆう', '悠々'),
			_Utils_Tuple2('ゆうよ', '猶予'),
			_Utils_Tuple2('ゆうらん', '遊覧'),
			_Utils_Tuple2('ゆうりょ', '憂慮'),
			_Utils_Tuple2('ゆうりょくしゃ', '有力者'),
			_Utils_Tuple2('ゆうれつ', '優劣'),
			_Utils_Tuple2('ゆうわく', '誘惑'),
			_Utils_Tuple2('ゆえ', '故'),
			_Utils_Tuple2('ゆえに', '故に'),
			_Utils_Tuple2('ゆかした', '床下'),
			_Utils_Tuple2('ゆがみ', 'ゆがみ'),
			_Utils_Tuple2('ゆがむ', 'ゆがむ'),
			_Utils_Tuple2('ゆかり', 'ゆかり'),
			_Utils_Tuple2('ゆきおんな', '雪女'),
			_Utils_Tuple2('ゆきどけ', '雪解け'),
			_Utils_Tuple2('ゆきみ', '雪見'),
			_Utils_Tuple2('ゆくすえ', '行く末'),
			_Utils_Tuple2('ゆくて', '行く手'),
			_Utils_Tuple2('ゆくゆく', 'ゆくゆく'),
			_Utils_Tuple2('ゆさぶる', '揺さぶる'),
			_Utils_Tuple2('ゆし', '油脂'),
			_Utils_Tuple2('ゆず', 'ゆず'),
			_Utils_Tuple2('ゆすぐ', 'ゆすぐ'),
			_Utils_Tuple2('ゆずり', '譲り'),
			_Utils_Tuple2('ゆずりうける', '譲り受ける'),
			_Utils_Tuple2('ゆずりわたす', '譲り渡す'),
			_Utils_Tuple2('ゆする', '揺する'),
			_Utils_Tuple2('ゆだねる', 'ゆだねる'),
			_Utils_Tuple2('ゆだや', 'ユダヤ'),
			_Utils_Tuple2('ゆだやきょう', 'ユダヤ教'),
			_Utils_Tuple2('ゆだやじん', 'ユダヤ人'),
			_Utils_Tuple2('ゆちゃく', '癒着'),
			_Utils_Tuple2('ゆでん', '油田'),
			_Utils_Tuple2('ゆどおし', '湯通し'),
			_Utils_Tuple2('ゆとり', 'ゆとり'),
			_Utils_Tuple2('ゆにせっくす', 'ユニセックス'),
			_Utils_Tuple2('ゆにふぉーむ', 'ユニフォーム'),
			_Utils_Tuple2('ゆびおり', '指折り'),
			_Utils_Tuple2('ゆびおりかぞえる', '指折り数える'),
			_Utils_Tuple2('ゆみ', '弓'),
			_Utils_Tuple2('ゆみなり', '弓なり'),
			_Utils_Tuple2('ゆめにも', '夢にも'),
			_Utils_Tuple2('ゆらい', '由来'),
			_Utils_Tuple2('ゆらぐ', '揺らぐ'),
			_Utils_Tuple2('ゆらす', '揺らす'),
			_Utils_Tuple2('ゆり', 'ゆり'),
			_Utils_Tuple2('ゆるし', '許し'),
			_Utils_Tuple2('ゆるむ', '緩む'),
			_Utils_Tuple2('ゆるめる', '緩める'),
			_Utils_Tuple2('ゆるやか', '緩やか'),
			_Utils_Tuple2('よ', '世'),
			_Utils_Tuple2('よう', '葉'),
			_Utils_Tuple2('よう', '洋'),
			_Utils_Tuple2('よう', '葉'),
			_Utils_Tuple2('よういく', '養育'),
			_Utils_Tuple2('よういしゅうとう', '用意周到'),
			_Utils_Tuple2('よういん', '要因'),
			_Utils_Tuple2('ようえき', '溶液'),
			_Utils_Tuple2('ようかい', '溶解'),
			_Utils_Tuple2('ようかい', '妖怪'),
			_Utils_Tuple2('ようがさ', '洋傘'),
			_Utils_Tuple2('ようかん', '洋館'),
			_Utils_Tuple2('ようがん', '溶岩'),
			_Utils_Tuple2('ようぎ', '容疑'),
			_Utils_Tuple2('ようぎしゃ', '容疑者'),
			_Utils_Tuple2('ようけん', '要件'),
			_Utils_Tuple2('ようご', '養護'),
			_Utils_Tuple2('ようこう', '要項'),
			_Utils_Tuple2('ようごがっこう', '養護学校'),
			_Utils_Tuple2('ようさい', '洋裁'),
			_Utils_Tuple2('ようし', '養子'),
			_Utils_Tuple2('ようしえんぐみ', '養子縁組'),
			_Utils_Tuple2('ようしき', '様式'),
			_Utils_Tuple2('ようしゃ', '容赦'),
			_Utils_Tuple2('ようじょ', '養女'),
			_Utils_Tuple2('ようしょう', '幼少'),
			_Utils_Tuple2('ようしょく', '養殖'),
			_Utils_Tuple2('ようじん', '要人'),
			_Utils_Tuple2('ようする', '要する'),
			_Utils_Tuple2('ようするに', '要するに'),
			_Utils_Tuple2('ようせい', '養成'),
			_Utils_Tuple2('ようせい', '要請'),
			_Utils_Tuple2('ようせい', '妖精'),
			_Utils_Tuple2('ようせき', '容積'),
			_Utils_Tuple2('ようそ', '要素'),
			_Utils_Tuple2('ようそう', '様相'),
			_Utils_Tuple2('ようち', '用地'),
			_Utils_Tuple2('ようちゅう', '幼虫'),
			_Utils_Tuple2('ようと', '用途'),
			_Utils_Tuple2('ようとん', '養豚'),
			_Utils_Tuple2('ようにん', '容認'),
			_Utils_Tuple2('ようは', '要は'),
			_Utils_Tuple2('ようひん', '洋品'),
			_Utils_Tuple2('ようぶん', '養分'),
			_Utils_Tuple2('ようぼう', '要望'),
			_Utils_Tuple2('ようま', '洋間'),
			_Utils_Tuple2('ようりょう', '要領'),
			_Utils_Tuple2('よか', '余暇'),
			_Utils_Tuple2('よきょう', '余興'),
			_Utils_Tuple2('よぎる', 'よぎる'),
			_Utils_Tuple2('よく', '浴'),
			_Utils_Tuple2('よく', '翼'),
			_Utils_Tuple2('よくし', '抑止'),
			_Utils_Tuple2('よくじょう', '浴場'),
			_Utils_Tuple2('よくせい', '抑制'),
			_Utils_Tuple2('よくそう', '浴槽'),
			_Utils_Tuple2('よくぼう', '欲望'),
			_Utils_Tuple2('よくよく', 'よくよく'),
			_Utils_Tuple2('よくよく', '翌々'),
			_Utils_Tuple2('よげん', '予言'),
			_Utils_Tuple2('よこじま', '横じま'),
			_Utils_Tuple2('よこづな', '横綱'),
			_Utils_Tuple2('よこどり', '横取り'),
			_Utils_Tuple2('よこなぐり', '横殴り'),
			_Utils_Tuple2('よこはば', '横幅'),
			_Utils_Tuple2('よこめ', '横目'),
			_Utils_Tuple2('よしあし', '善しあし'),
			_Utils_Tuple2('よしよし', 'よしよし'),
			_Utils_Tuple2('よせ', '寄席'),
			_Utils_Tuple2('よせい', '余生'),
			_Utils_Tuple2('よせつける', '寄せ付ける'),
			_Utils_Tuple2('よせなべ', '寄せ鍋'),
			_Utils_Tuple2('よそ', 'よそ'),
			_Utils_Tuple2('よそおい', '装い'),
			_Utils_Tuple2('よそおう', '装う'),
			_Utils_Tuple2('よたよた', 'よたよた'),
			_Utils_Tuple2('よだれ', 'よだれ'),
			_Utils_Tuple2('よだん', '余談'),
			_Utils_Tuple2('よち', '余地'),
			_Utils_Tuple2('よち', '予知'),
			_Utils_Tuple2('よちょきん', '預貯金'),
			_Utils_Tuple2('よちよち', 'よちよち'),
			_Utils_Tuple2('よとう', '与党'),
			_Utils_Tuple2('よなき', '夜泣き'),
			_Utils_Tuple2('よなべ', '夜なべ'),
			_Utils_Tuple2('よにげ', '夜逃げ'),
			_Utils_Tuple2('よねつ', '余熱'),
			_Utils_Tuple2('よはく', '余白'),
			_Utils_Tuple2('よびおこす', '呼び起こす'),
			_Utils_Tuple2('よびぐん', '予備軍'),
			_Utils_Tuple2('よびこむ', '呼び込む'),
			_Utils_Tuple2('よびちしき', '予備知識'),
			_Utils_Tuple2('よびもどす', '呼び戻す'),
			_Utils_Tuple2('よぼうせっしゅ', '予防接種'),
			_Utils_Tuple2('よほど', '余程'),
			_Utils_Tuple2('よぼよぼ', 'よぼよぼ'),
			_Utils_Tuple2('よみあげる', '読み上げる'),
			_Utils_Tuple2('よみかえす', '読み返す'),
			_Utils_Tuple2('よみがえる', 'よみがえる'),
			_Utils_Tuple2('よみきり', '読み切り'),
			_Utils_Tuple2('よみこむ', '読み込む'),
			_Utils_Tuple2('よめいり', '嫁入り'),
			_Utils_Tuple2('よめいりどうぐ', '嫁入り道具'),
			_Utils_Tuple2('よりかかる', '寄り掛かる'),
			_Utils_Tuple2('よりそう', '寄り添う'),
			_Utils_Tuple2('よりょく', '余力'),
			_Utils_Tuple2('よれよれ', 'よれよれ'),
			_Utils_Tuple2('よろい', 'よろい'),
			_Utils_Tuple2('よろこばしい', '喜ばしい'),
			_Utils_Tuple2('よろず', 'よろず'),
			_Utils_Tuple2('よろよろ', 'よろよろ'),
			_Utils_Tuple2('よろん', '世論'),
			_Utils_Tuple2('よわたり', '世渡り'),
			_Utils_Tuple2('よわみ', '弱み'),
			_Utils_Tuple2('よわむし', '弱虫'),
			_Utils_Tuple2('よわよわしい', '弱々しい'),
			_Utils_Tuple2('よわる', '弱る'),
			_Utils_Tuple2('らーど', 'ラード'),
			_Utils_Tuple2('らいせ', '来世'),
			_Utils_Tuple2('らいせんす', 'ライセンス'),
			_Utils_Tuple2('らいひん', '来賓'),
			_Utils_Tuple2('らいぶはうす', 'ライブハウス'),
			_Utils_Tuple2('らいふらいん', 'ライフライン'),
			_Utils_Tuple2('らいふる', 'ライフル'),
			_Utils_Tuple2('らいほう', '来訪'),
			_Utils_Tuple2('らうんじ', 'ラウンジ'),
			_Utils_Tuple2('らうんど', 'ラウンド'),
			_Utils_Tuple2('らくえん', '楽園'),
			_Utils_Tuple2('らくがき', '落書き'),
			_Utils_Tuple2('らくご', '落語'),
			_Utils_Tuple2('らくごか', '落語家'),
			_Utils_Tuple2('らくさつ', '落札'),
			_Utils_Tuple2('らくしょう', '楽勝'),
			_Utils_Tuple2('らくせき', '落石'),
			_Utils_Tuple2('らくせん', '落選'),
			_Utils_Tuple2('らくてん', '楽天'),
			_Utils_Tuple2('らくてんてき', '楽天的'),
			_Utils_Tuple2('らくよう', '落葉'),
			_Utils_Tuple2('らくようじゅ', '落葉樹'),
			_Utils_Tuple2('らじこん', 'ラジコン'),
			_Utils_Tuple2('らしんばん', '羅針盤'),
			_Utils_Tuple2('らち', '拉致'),
			_Utils_Tuple2('らっか', '落下'),
			_Utils_Tuple2('らっかせい', '落花生'),
			_Utils_Tuple2('らっかん', '楽観'),
			_Utils_Tuple2('らっかんてき', '楽観的'),
			_Utils_Tuple2('らっきょう', 'らっきょう'),
			_Utils_Tuple2('らっぱ', 'らっぱ'),
			_Utils_Tuple2('らてん', 'ラテン'),
			_Utils_Tuple2('らてんご', 'ラテン語'),
			_Utils_Tuple2('らりー', 'ラリー'),
			_Utils_Tuple2('られつ', '羅列'),
			_Utils_Tuple2('らん', '乱'),
			_Utils_Tuple2('らん', '蘭'),
			_Utils_Tuple2('らん', '乱'),
			_Utils_Tuple2('らん', 'ラン'),
			_Utils_Tuple2('らんおう', '卵黄'),
			_Utils_Tuple2('らんそう', '卵巣'),
			_Utils_Tuple2('らんだむ', 'ランダム'),
			_Utils_Tuple2('らんち', 'ランチ'),
			_Utils_Tuple2('らんとう', '乱闘'),
			_Utils_Tuple2('らんぱく', '卵白'),
			_Utils_Tuple2('らんよう', '乱用'),
			_Utils_Tuple2('り', '利'),
			_Utils_Tuple2('り', '里'),
			_Utils_Tuple2('り', '裏'),
			_Utils_Tuple2('りあくしょん', 'リアクション'),
			_Utils_Tuple2('りありてぃー', 'リアリティー'),
			_Utils_Tuple2('りある', 'リアル'),
			_Utils_Tuple2('りーぐ', 'リーグ'),
			_Utils_Tuple2('りーぐせん', 'リーグ戦'),
			_Utils_Tuple2('りきし', '力士'),
			_Utils_Tuple2('りきせつ', '力説'),
			_Utils_Tuple2('りきっど', 'リキッド'),
			_Utils_Tuple2('りきゅーる', 'リキュール'),
			_Utils_Tuple2('りきりょう', '力量'),
			_Utils_Tuple2('りくぐん', '陸軍'),
			_Utils_Tuple2('りくじょう', '陸上'),
			_Utils_Tuple2('りくち', '陸地'),
			_Utils_Tuple2('りくつ', '理屈'),
			_Utils_Tuple2('りくるーと', 'リクルート'),
			_Utils_Tuple2('りこーる', 'リコール'),
			_Utils_Tuple2('りざーぶ', 'リザーブ'),
			_Utils_Tuple2('りじ', '理事'),
			_Utils_Tuple2('りしゅう', '履修'),
			_Utils_Tuple2('りじゅん', '利潤'),
			_Utils_Tuple2('りせい', '理性'),
			_Utils_Tuple2('りたいあ', 'リタイア'),
			_Utils_Tuple2('りだつ', '離脱'),
			_Utils_Tuple2('りちぎ', '律儀'),
			_Utils_Tuple2('りちゃくりく', '離着陸'),
			_Utils_Tuple2('りつあん', '立案'),
			_Utils_Tuple2('りっこうほ', '立候補'),
			_Utils_Tuple2('りっしゅう', '立秋'),
			_Utils_Tuple2('りっしゅん', '立春'),
			_Utils_Tuple2('りっしょう', '立証'),
			_Utils_Tuple2('りったい', '立体'),
			_Utils_Tuple2('りったいてき', '立体的'),
			_Utils_Tuple2('りっち', '立地'),
			_Utils_Tuple2('りっちじょうけん', '立地条件'),
			_Utils_Tuple2('りっぽう', '立方'),
			_Utils_Tuple2('りっぽう', '立法'),
			_Utils_Tuple2('りとう', '離島'),
			_Utils_Tuple2('りとう', '離党'),
			_Utils_Tuple2('りねん', '理念'),
			_Utils_Tuple2('りはーさる', 'リハーサル'),
			_Utils_Tuple2('りばうんど', 'リバウンド'),
			_Utils_Tuple2('りはつ', '理髪'),
			_Utils_Tuple2('りはつてん', '理髪店'),
			_Utils_Tuple2('りはびり', 'リハビリ'),
			_Utils_Tuple2('りふぉーむ', 'リフォーム'),
			_Utils_Tuple2('りべーと', 'リベート'),
			_Utils_Tuple2('りみっと', 'リミット'),
			_Utils_Tuple2('りむじん', 'リムジン'),
			_Utils_Tuple2('りめん', '裏面'),
			_Utils_Tuple2('りゃく', '略'),
			_Utils_Tuple2('りゃくしき', '略式'),
			_Utils_Tuple2('りゃくしょう', '略称'),
			_Utils_Tuple2('りゃくす', '略す'),
			_Utils_Tuple2('りゃくず', '略図'),
			_Utils_Tuple2('りゃくだつ', '略奪'),
			_Utils_Tuple2('りゃくれき', '略歴'),
			_Utils_Tuple2('りゅう', '竜'),
			_Utils_Tuple2('りゅうい', '留意'),
			_Utils_Tuple2('りゅういき', '流域'),
			_Utils_Tuple2('りゅうぎ', '流儀'),
			_Utils_Tuple2('りゅうさん', '硫酸'),
			_Utils_Tuple2('りゅうざん', '流産'),
			_Utils_Tuple2('りゅうし', '粒子'),
			_Utils_Tuple2('りゅうしゅつ', '流出'),
			_Utils_Tuple2('りゅうちょう', '流ちょう'),
			_Utils_Tuple2('りゅうどう', '流動'),
			_Utils_Tuple2('りゅうどうせい', '流動性'),
			_Utils_Tuple2('りゅうねん', '留年'),
			_Utils_Tuple2('りゅうは', '流派'),
			_Utils_Tuple2('りゅうひょう', '流氷'),
			_Utils_Tuple2('りゅーまち', 'リューマチ'),
			_Utils_Tuple2('りょう', '領'),
			_Utils_Tuple2('りょう', '涼'),
			_Utils_Tuple2('りょう', '両'),
			_Utils_Tuple2('りよう', '理容'),
			_Utils_Tuple2('りょういき', '領域'),
			_Utils_Tuple2('りようかち', '利用価値'),
			_Utils_Tuple2('りょうきょく', '両極'),
			_Utils_Tuple2('りょうけ', '両家'),
			_Utils_Tuple2('りょうじかん', '領事館'),
			_Utils_Tuple2('りょうしき', '良識'),
			_Utils_Tuple2('りょうしつ', '良質'),
			_Utils_Tuple2('りょうしゅ', '領主'),
			_Utils_Tuple2('りょうしゅう', '領収'),
			_Utils_Tuple2('りょうしょう', '了承'),
			_Utils_Tuple2('りょうせい', '良性'),
			_Utils_Tuple2('りょうたん', '両端'),
			_Utils_Tuple2('りょうてい', '料亭'),
			_Utils_Tuple2('りょうど', '領土'),
			_Utils_Tuple2('りょうほう', '療法'),
			_Utils_Tuple2('りょうやく', '良薬'),
			_Utils_Tuple2('りょうよう', '療養'),
			_Utils_Tuple2('りょかく', '旅客'),
			_Utils_Tuple2('りょかくき', '旅客機'),
			_Utils_Tuple2('りょくおうしょく', '緑黄色'),
			_Utils_Tuple2('りょくち', '緑地'),
			_Utils_Tuple2('りりーす', 'リリース'),
			_Utils_Tuple2('りりーふ', 'リリーフ'),
			_Utils_Tuple2('りりつ', '利率'),
			_Utils_Tuple2('りれき', '履歴'),
			_Utils_Tuple2('りろん', '理論'),
			_Utils_Tuple2('りろんてき', '理論的'),
			_Utils_Tuple2('りん', '輪'),
			_Utils_Tuple2('りんかい', '臨海'),
			_Utils_Tuple2('りんかく', '輪郭'),
			_Utils_Tuple2('りんきおうへん', '臨機応変'),
			_Utils_Tuple2('りんじゅう', '臨終'),
			_Utils_Tuple2('りんしょう', '臨床'),
			_Utils_Tuple2('りんせつ', '隣接'),
			_Utils_Tuple2('りんどう', '林道'),
			_Utils_Tuple2('りんり', '倫理'),
			_Utils_Tuple2('りんりてき', '倫理的'),
			_Utils_Tuple2('るあー', 'ルアー'),
			_Utils_Tuple2('るい', '塁'),
			_Utils_Tuple2('るいけい', '累計'),
			_Utils_Tuple2('るいじ', '類似'),
			_Utils_Tuple2('るいすい', '類推'),
			_Utils_Tuple2('るいせき', '累積'),
			_Utils_Tuple2('るーきー', 'ルーキー'),
			_Utils_Tuple2('るーじゅ', 'ルージュ'),
			_Utils_Tuple2('るーず', 'ルーズ'),
			_Utils_Tuple2('るーつ', 'ルーツ'),
			_Utils_Tuple2('るーれっと', 'ルーレット'),
			_Utils_Tuple2('るねさんす', 'ルネサンス'),
			_Utils_Tuple2('るびー', 'ルビー'),
			_Utils_Tuple2('るぽ', 'ルポ'),
			_Utils_Tuple2('るぽるたーじゅ', 'ルポルタージュ'),
			_Utils_Tuple2('れい', '霊'),
			_Utils_Tuple2('れいあうと', 'レイアウト'),
			_Utils_Tuple2('れいか', '零下'),
			_Utils_Tuple2('れいかん', '霊感'),
			_Utils_Tuple2('れいきゃく', '冷却'),
			_Utils_Tuple2('れいこく', '冷酷'),
			_Utils_Tuple2('れいさい', '零細'),
			_Utils_Tuple2('れいじょう', '礼状'),
			_Utils_Tuple2('れいせん', '冷戦'),
			_Utils_Tuple2('れいぜん', '霊前'),
			_Utils_Tuple2('れいそう', '礼装'),
			_Utils_Tuple2('れいぞう', '冷蔵'),
			_Utils_Tuple2('れいちょうるい', '霊長類'),
			_Utils_Tuple2('れいはい', '礼拝'),
			_Utils_Tuple2('れいぷ', 'レイプ'),
			_Utils_Tuple2('れいふく', '礼服'),
			_Utils_Tuple2('れいめん', '冷麺'),
			_Utils_Tuple2('れーさー', 'レーサー'),
			_Utils_Tuple2('れーざー', 'レーザー'),
			_Utils_Tuple2('れき', '暦'),
			_Utils_Tuple2('れきだい', '歴代'),
			_Utils_Tuple2('れぎゅらー', 'レギュラー'),
			_Utils_Tuple2('れくりえーしょん', 'レクリエーション'),
			_Utils_Tuple2('れげえ', 'レゲエ'),
			_Utils_Tuple2('れこーだー', 'レコーダー'),
			_Utils_Tuple2('れこーでぃんぐ', 'レコーディング'),
			_Utils_Tuple2('れじゃーさんぎょう', 'レジャー産業'),
			_Utils_Tuple2('れず', 'レズ'),
			_Utils_Tuple2('れすらー', 'レスラー'),
			_Utils_Tuple2('れすりんぐ', 'レスリング'),
			_Utils_Tuple2('れっかー', 'レッカー'),
			_Utils_Tuple2('れっきょ', '列挙'),
			_Utils_Tuple2('れってる', 'レッテル'),
			_Utils_Tuple2('れっとう', '劣等'),
			_Utils_Tuple2('れっとうかん', '劣等感'),
			_Utils_Tuple2('れとろ', 'レトロ'),
			_Utils_Tuple2('れぱーとりー', 'レパートリー'),
			_Utils_Tuple2('れぷりか', 'レプリカ'),
			_Utils_Tuple2('れぼりゅーしょん', 'レボリューション'),
			_Utils_Tuple2('れんが', 'れんが'),
			_Utils_Tuple2('れんけい', '連携'),
			_Utils_Tuple2('れんけつ', '連結'),
			_Utils_Tuple2('れんこ', '連呼'),
			_Utils_Tuple2('れんこう', '連行'),
			_Utils_Tuple2('れんごう', '連合'),
			_Utils_Tuple2('れんごうこく', '連合国'),
			_Utils_Tuple2('れんさ', '連鎖'),
			_Utils_Tuple2('れんさい', '連載'),
			_Utils_Tuple2('れんしゃ', '連射'),
			_Utils_Tuple2('れんせん', '連戦'),
			_Utils_Tuple2('れんそう', '連想'),
			_Utils_Tuple2('れんだ', '連打'),
			_Utils_Tuple2('れんたい', '連帯'),
			_Utils_Tuple2('れんぱ', '連覇'),
			_Utils_Tuple2('れんぱい', '連敗'),
			_Utils_Tuple2('れんぱつ', '連発'),
			_Utils_Tuple2('れんぽう', '連邦'),
			_Utils_Tuple2('れんめい', '連名'),
			_Utils_Tuple2('れんめい', '連盟'),
			_Utils_Tuple2('れんりつ', '連立'),
			_Utils_Tuple2('ろ', '露'),
			_Utils_Tuple2('ろう', '労'),
			_Utils_Tuple2('ろうきゅうか', '老朽化'),
			_Utils_Tuple2('ろうさい', '労災'),
			_Utils_Tuple2('ろうでん', '漏電'),
			_Utils_Tuple2('ろうどうきじゅんほう', '労働基準法'),
			_Utils_Tuple2('ろうどうくみあい', '労働組合'),
			_Utils_Tuple2('ろうどうしょう', '労働省'),
			_Utils_Tuple2('ろうどうじょうけん', '労働条件'),
			_Utils_Tuple2('ろうどく', '朗読'),
			_Utils_Tuple2('ろうにゃくなんにょ', '老若男女'),
			_Utils_Tuple2('ろうにん', '浪人'),
			_Utils_Tuple2('ろうば', '老婆'),
			_Utils_Tuple2('ろうひ', '浪費'),
			_Utils_Tuple2('ろうほう', '朗報'),
			_Utils_Tuple2('ろうや', 'ろう屋'),
			_Utils_Tuple2('ろうりょく', '労力'),
			_Utils_Tuple2('ろーかる', 'ローカル'),
			_Utils_Tuple2('ろーしょん', 'ローション'),
			_Utils_Tuple2('ろーすと', 'ロースト'),
			_Utils_Tuple2('ろーたりー', 'ロータリー'),
			_Utils_Tuple2('ろーてーしょん', 'ローテーション'),
			_Utils_Tuple2('ろーらー', 'ローラー'),
			_Utils_Tuple2('ろかた', '路肩'),
			_Utils_Tuple2('ろくに', 'ろくに'),
			_Utils_Tuple2('ろけ', 'ロケ'),
			_Utils_Tuple2('ろこつ', '露骨'),
			_Utils_Tuple2('ろじ', '路地'),
			_Utils_Tuple2('ろしゅつ', '露出'),
			_Utils_Tuple2('ろっくんろーる', 'ロックンロール'),
			_Utils_Tuple2('ろっぽう', '六法'),
			_Utils_Tuple2('ろっぽうぜんしょ', '六法全書'),
			_Utils_Tuple2('ろてん', '露店'),
			_Utils_Tuple2('ろまん', 'ロマン'),
			_Utils_Tuple2('ろまんす', 'ロマンス'),
			_Utils_Tuple2('ろまんちっく', 'ロマンチック'),
			_Utils_Tuple2('ろめん', '路面'),
			_Utils_Tuple2('ろん', '論'),
			_Utils_Tuple2('ろんがい', '論外'),
			_Utils_Tuple2('ろんぎ', '論議'),
			_Utils_Tuple2('ろんじゅつ', '論述'),
			_Utils_Tuple2('ろんじる', '論じる'),
			_Utils_Tuple2('ろんずる', '論ずる'),
			_Utils_Tuple2('ろんせつ', '論説'),
			_Utils_Tuple2('ろんそう', '論争'),
			_Utils_Tuple2('わーぷ', 'ワープ'),
			_Utils_Tuple2('わーぷろ', 'ワープロ'),
			_Utils_Tuple2('わいぱー', 'ワイパー'),
			_Utils_Tuple2('わいやれす', 'ワイヤレス'),
			_Utils_Tuple2('わいるど', 'ワイルド'),
			_Utils_Tuple2('わいろ', 'わいろ'),
			_Utils_Tuple2('わいわい', 'わいわい'),
			_Utils_Tuple2('わおん', '和音'),
			_Utils_Tuple2('わか', '和歌'),
			_Utils_Tuple2('わかい', '和解'),
			_Utils_Tuple2('わかいもの', '若い者'),
			_Utils_Tuple2('わかがえり', '若返り'),
			_Utils_Tuple2('わかれめ', '分かれ目'),
			_Utils_Tuple2('わき', '脇'),
			_Utils_Tuple2('わきあがる', '湧き上がる'),
			_Utils_Tuple2('わきが', 'わきが'),
			_Utils_Tuple2('わきまえる', 'わきまえる'),
			_Utils_Tuple2('わぎり', '輪切り'),
			_Utils_Tuple2('わく', '枠'),
			_Utils_Tuple2('わくぐみ', '枠組み'),
			_Utils_Tuple2('わくせい', '惑星'),
			_Utils_Tuple2('わくちん', 'ワクチン'),
			_Utils_Tuple2('わけめ', '分け目'),
			_Utils_Tuple2('わご', '和語'),
			_Utils_Tuple2('わごん', 'ワゴン'),
			_Utils_Tuple2('わざ', '技'),
			_Utils_Tuple2('わざわい', '災い'),
			_Utils_Tuple2('わざわざ', 'わざわざ'),
			_Utils_Tuple2('わし', '和紙'),
			_Utils_Tuple2('わし', 'わし'),
			_Utils_Tuple2('わじゅつ', '話術'),
			_Utils_Tuple2('わずらう', '患う'),
			_Utils_Tuple2('わずらわしい', '煩わしい'),
			_Utils_Tuple2('わせいえいご', '和製英語'),
			_Utils_Tuple2('わそう', '和装'),
			_Utils_Tuple2('わだかまり', 'わだかまり'),
			_Utils_Tuple2('わたし', '渡し'),
			_Utils_Tuple2('わたり', '渡り'),
			_Utils_Tuple2('わたりあるく', '渡り歩く'),
			_Utils_Tuple2('わたりどり', '渡り鳥'),
			_Utils_Tuple2('わっくす', 'ワックス'),
			_Utils_Tuple2('わっしょい', 'わっしょい'),
			_Utils_Tuple2('わな', 'わな'),
			_Utils_Tuple2('わび', 'わび'),
			_Utils_Tuple2('わびる', 'わびる'),
			_Utils_Tuple2('わぶん', '和文'),
			_Utils_Tuple2('わへい', '和平'),
			_Utils_Tuple2('わめく', 'わめく'),
			_Utils_Tuple2('わやく', '和訳'),
			_Utils_Tuple2('わら', 'わら'),
			_Utils_Tuple2('わり', '割'),
			_Utils_Tuple2('わりあいに', '割合に'),
			_Utils_Tuple2('わりあて', '割り当て'),
			_Utils_Tuple2('わりあてる', '割り当てる'),
			_Utils_Tuple2('わりきる', '割り切る'),
			_Utils_Tuple2('わりこむ', '割り込む'),
			_Utils_Tuple2('わりだか', '割高'),
			_Utils_Tuple2('わりだす', '割り出す'),
			_Utils_Tuple2('わりびき', '割引'),
			_Utils_Tuple2('わりやす', '割安'),
			_Utils_Tuple2('わるぎ', '悪気'),
			_Utils_Tuple2('わるつ', 'ワルツ'),
			_Utils_Tuple2('われ', '我'),
			_Utils_Tuple2('われ', '割れ'),
			_Utils_Tuple2('われさきに', '我先に'),
			_Utils_Tuple2('われめ', '割れ目'),
			_Utils_Tuple2('わんたん', 'ワンタン'),
			_Utils_Tuple2('わんぱたーん', 'ワンパターン'),
			_Utils_Tuple2('わんまん', 'ワンマン'),
			_Utils_Tuple2('わんりょく', '腕力')
		]));
var $author$project$Dictionary$find = function (yomi) {
	var result = A2($elm$core$Dict$get, yomi, $author$project$Dictionary$joukyu);
	if (result.$ === 'Just') {
		var word = result.a;
		return _List_fromArray(
			[word]);
	} else {
		return _List_Nil;
	}
};
var $elm$core$Debug$log = _Debug_log;
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Pittan$fromDictionary = F2(
	function (conf, cell) {
		var row = A2(
			$elm$core$Debug$log,
			'row',
			A2(
				$elm$core$List$sortBy,
				function ($) {
					return $.x;
				},
				A2(
					$elm$core$List$filter,
					function (p) {
						return _Utils_eq(p.y, cell.y);
					},
					conf)));
		var xMax = A2(
			$elm$core$Debug$log,
			'max',
			A2(
				$elm$core$Basics$max,
				cell.x,
				A2(
					$elm$core$Maybe$withDefault,
					cell.x,
					$elm$core$List$maximum(
						A2(
							$elm$core$List$map,
							function ($) {
								return $.x;
							},
							row)))));
		var xHigh = A2(
			$elm$core$Debug$log,
			'right',
			A2(
				$elm$core$List$filter,
				function (i) {
					return _Utils_eq(
						(i - cell.x) + 1,
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (p) {
									return (_Utils_cmp(cell.x, p.x) < 1) && (_Utils_cmp(p.x, i) < 1);
								},
								row)));
				},
				A2($elm$core$List$range, cell.x, xMax)));
		var xMin = A2(
			$elm$core$Debug$log,
			'min',
			A2(
				$elm$core$Basics$min,
				cell.x,
				A2(
					$elm$core$Maybe$withDefault,
					cell.x,
					$elm$core$List$minimum(
						A2(
							$elm$core$List$map,
							function ($) {
								return $.x;
							},
							row)))));
		var xLow = A2(
			$elm$core$Debug$log,
			'left',
			A2(
				$elm$core$List$filter,
				function (i) {
					return _Utils_eq(
						(cell.x - i) + 1,
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (p) {
									return (_Utils_cmp(i, p.x) < 1) && (_Utils_cmp(p.x, cell.x) < 1);
								},
								row)));
				},
				A2($elm$core$List$range, xMin, cell.x)));
		var hWordAt = function (range) {
			return $author$project$Dictionary$find(
				$elm$core$String$concat(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.c;
						},
						$elm$core$List$concat(
							A2(
								$elm$core$List$map,
								function (x) {
									return A2(
										$elm$core$List$filter,
										function (p) {
											return _Utils_eq(p.x, x);
										},
										row);
								},
								range)))));
		};
		var hRanges = A2(
			$elm$core$Debug$log,
			'range',
			$elm$core$List$concat(
				A2(
					$elm$core$List$map,
					function (start) {
						return A2(
							$elm$core$List$map,
							function (end) {
								return A2($elm$core$List$range, start, end);
							},
							xHigh);
					},
					xLow)));
		var hWords = A2(
			$elm$core$Debug$log,
			'horizontal',
			A3(
				$elm$core$List$foldl,
				F2(
					function (range, cells) {
						return _Utils_ap(
							cells,
							hWordAt(range));
					}),
				_List_Nil,
				A2(
					$elm$core$List$filter,
					function (range) {
						return $elm$core$List$length(range) > 1;
					},
					hRanges)));
		var col = A2(
			$elm$core$List$sortBy,
			function ($) {
				return $.y;
			},
			A2(
				$elm$core$List$filter,
				function (p) {
					return _Utils_eq(p.x, cell.x);
				},
				conf));
		var vWordAt = function (range) {
			return $author$project$Dictionary$find(
				$elm$core$String$concat(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.c;
						},
						$elm$core$List$concat(
							A2(
								$elm$core$List$map,
								function (y) {
									return A2(
										$elm$core$List$filter,
										function (p) {
											return _Utils_eq(p.y, y);
										},
										col);
								},
								range)))));
		};
		var yMax = A2(
			$elm$core$Basics$max,
			cell.y,
			A2(
				$elm$core$Maybe$withDefault,
				cell.y,
				$elm$core$List$maximum(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.y;
						},
						col))));
		var yHigh = A2(
			$elm$core$Debug$log,
			'below',
			A2(
				$elm$core$List$filter,
				function (j) {
					return _Utils_eq(
						(j - cell.y) + 1,
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (p) {
									return (_Utils_cmp(cell.y, p.y) < 1) && (_Utils_cmp(p.y, j) < 1);
								},
								col)));
				},
				A2($elm$core$List$range, cell.y, yMax)));
		var yMin = A2(
			$elm$core$Basics$min,
			cell.y,
			A2(
				$elm$core$Maybe$withDefault,
				cell.y,
				$elm$core$List$minimum(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.y;
						},
						col))));
		var yLow = A2(
			$elm$core$Debug$log,
			'above',
			A2(
				$elm$core$List$filter,
				function (j) {
					return _Utils_eq(
						(cell.y - j) + 1,
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (p) {
									return (_Utils_cmp(j, p.y) < 1) && (_Utils_cmp(p.y, cell.y) < 1);
								},
								col)));
				},
				A2($elm$core$List$range, yMin, cell.y)));
		var vRanges = A2(
			$elm$core$Debug$log,
			'range',
			$elm$core$List$concat(
				A2(
					$elm$core$List$map,
					function (start) {
						return A2(
							$elm$core$List$map,
							function (end) {
								return A2($elm$core$List$range, start, end);
							},
							yHigh);
					},
					yLow)));
		var vWords = A2(
			$elm$core$Debug$log,
			'vertical',
			A3(
				$elm$core$List$foldl,
				F2(
					function (range, cells) {
						return _Utils_ap(
							cells,
							vWordAt(range));
					}),
				_List_Nil,
				A2(
					$elm$core$List$filter,
					function (range) {
						return $elm$core$List$length(range) > 1;
					},
					vRanges)));
		return _Utils_ap(hWords, vWords);
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Pittan$initConf = _List_fromArray(
	[
		A5($author$project$Pittan$Piece, 0, 3, 2, 'は', true),
		A5($author$project$Pittan$Piece, 1, 4, 2, 'あ', true),
		A5($author$project$Pittan$Piece, 2, 5, 2, 'と', true)
	]);
var $author$project$Pittan$initialCandidates = _List_fromArray(
	[
		_List_fromArray(
		['あ', 'あ', 'あ', 'ぁ', 'い', 'ぃ', 'う', 'ぅ', 'え', 'ぇ', 'お', 'ぉ', 'か', 'き', 'く', 'け', 'こ', 'さ', 'し', 'す', 'せ', 'そ', 'た', 'ち', 'つ', 'っ', 'て', 'と', 'な', 'に', 'ぬ', 'ね', 'の', 'は', 'ひ', 'ふ', 'へ', 'ほ', 'ま', 'み', 'む', 'め', 'も', 'や', 'ゃ', 'ゆ', 'ゅ', 'よ', 'ょ', 'ら', 'り', 'る', 'れ', 'ろ', 'わ', 'ん', 'が', 'ぎ', 'ぐ', 'げ', 'ご', 'ざ', 'じ', 'ず', 'ぜ', 'ぞ', 'だ', 'ぢ', 'づ', 'で', 'ど', 'ば', 'び', 'ぶ', 'べ', 'ぼ', 'ぱ', 'ぴ', 'ぷ', 'ぺ', 'ぽ']),
		_List_fromArray(
		['い', 'ろ', 'は', 'に', 'ほ', 'へ', 'と', 'ち', 'り', 'ぬ', 'る', 'を', 'わ', 'か', 'よ', 'た', 'れ', 'そ', 'つ', 'ね', 'な', 'ら', 'ん', 'う', 'の', 'お', 'く', 'や', 'ま', 'け', 'ふ', 'こ', 'え', 'て', 'あ', 'さ', 'き', 'ゆ', 'め', 'み', 'し', 'よ', 'ひ', 'も', 'せ', 'す', 'ん']),
		_List_fromArray(
		['あ', 'あ', 'あ', 'あ', 'か', 'か', 'か', 'さ', 'さ', 'さ', 'た', 'た', 'た', 'な', 'な', 'な', 'は', 'は', 'は', 'ま', 'ま', 'ま', 'や', 'や', 'や', 'ら', 'ら', 'ら', 'わ', 'わ', 'わ'])
	]);
var $author$project$Pittan$initialConfs = _List_fromArray(
	[
		_List_fromArray(
		[
			A5($author$project$Pittan$Piece, 0, 3, 2, 'は', true),
			A5($author$project$Pittan$Piece, 1, 4, 2, 'あ', true),
			A5($author$project$Pittan$Piece, 2, 5, 2, 'と', true)
		]),
		_List_fromArray(
		[
			A5($author$project$Pittan$Piece, 0, 3, 1, 'い', true),
			A5($author$project$Pittan$Piece, 0, 9, 1, 'ろ', true)
		]),
		_List_fromArray(
		[
			A5($author$project$Pittan$Piece, 0, 6, 1, 'あ', true)
		])
	]);
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Pittan$playSound = _Platform_outgoingPort('playSound', $elm$core$Basics$identity);
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $author$project$Pittan$unit = 60;
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $author$project$Pittan$valid = F2(
	function (conf, cell) {
		var row = A2(
			$elm$core$Debug$log,
			'row',
			A2(
				$elm$core$List$sortBy,
				function ($) {
					return $.x;
				},
				A2(
					$elm$core$List$filter,
					function (p) {
						return _Utils_eq(p.y, cell.y);
					},
					conf)));
		var xMax = A2(
			$elm$core$Debug$log,
			'max',
			A2(
				$elm$core$Basics$max,
				cell.x,
				A2(
					$elm$core$Maybe$withDefault,
					cell.x,
					$elm$core$List$maximum(
						A2(
							$elm$core$List$map,
							function ($) {
								return $.x;
							},
							row)))));
		var xHigh = A2(
			$elm$core$Debug$log,
			'right',
			A2(
				$elm$core$List$filter,
				function (i) {
					return _Utils_eq(
						(i - cell.x) + 1,
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (p) {
									return (_Utils_cmp(cell.x, p.x) < 1) && (_Utils_cmp(p.x, i) < 1);
								},
								row)));
				},
				A2($elm$core$List$range, cell.x, xMax)));
		var xMin = A2(
			$elm$core$Debug$log,
			'min',
			A2(
				$elm$core$Basics$min,
				cell.x,
				A2(
					$elm$core$Maybe$withDefault,
					cell.x,
					$elm$core$List$minimum(
						A2(
							$elm$core$List$map,
							function ($) {
								return $.x;
							},
							row)))));
		var xLow = A2(
			$elm$core$Debug$log,
			'left',
			A2(
				$elm$core$List$filter,
				function (i) {
					return _Utils_eq(
						(cell.x - i) + 1,
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (p) {
									return (_Utils_cmp(i, p.x) < 1) && (_Utils_cmp(p.x, cell.x) < 1);
								},
								row)));
				},
				A2($elm$core$List$range, xMin, cell.x)));
		var hWordAt = function (range) {
			return A2(
				$elm$core$Dict$member,
				$elm$core$String$concat(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.c;
						},
						$elm$core$List$concat(
							A2(
								$elm$core$List$map,
								function (x) {
									return A2(
										$elm$core$List$filter,
										function (p) {
											return _Utils_eq(p.x, x);
										},
										row);
								},
								range)))),
				$author$project$Dictionary$joukyu);
		};
		var hRanges = A2(
			$elm$core$Debug$log,
			'range',
			$elm$core$List$concat(
				A2(
					$elm$core$List$map,
					function (start) {
						return A2(
							$elm$core$List$map,
							function (end) {
								return A2($elm$core$List$range, start, end);
							},
							xHigh);
					},
					xLow)));
		var hWords = A2(
			$elm$core$Debug$log,
			'horizontal',
			A3(
				$elm$core$List$foldl,
				F2(
					function (range, cells) {
						return hWordAt(range) ? _Utils_ap(
							cells,
							_List_fromArray(
								[
									A2(
									$elm$core$List$map,
									function (x) {
										return A2($author$project$Pittan$Cell, x, cell.y);
									},
									range)
								])) : cells;
					}),
				_List_Nil,
				A2(
					$elm$core$List$filter,
					function (range) {
						return $elm$core$List$length(range) > 1;
					},
					hRanges)));
		var col = A2(
			$elm$core$List$sortBy,
			function ($) {
				return $.y;
			},
			A2(
				$elm$core$List$filter,
				function (p) {
					return _Utils_eq(p.x, cell.x);
				},
				conf));
		var vWordAt = function (range) {
			return A2(
				$elm$core$Dict$member,
				$elm$core$String$concat(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.c;
						},
						$elm$core$List$concat(
							A2(
								$elm$core$List$map,
								function (y) {
									return A2(
										$elm$core$List$filter,
										function (p) {
											return _Utils_eq(p.y, y);
										},
										col);
								},
								range)))),
				$author$project$Dictionary$joukyu);
		};
		var yMax = A2(
			$elm$core$Basics$max,
			cell.y,
			A2(
				$elm$core$Maybe$withDefault,
				cell.y,
				$elm$core$List$maximum(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.y;
						},
						col))));
		var yHigh = A2(
			$elm$core$Debug$log,
			'below',
			A2(
				$elm$core$List$filter,
				function (j) {
					return _Utils_eq(
						(j - cell.y) + 1,
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (p) {
									return (_Utils_cmp(cell.y, p.y) < 1) && (_Utils_cmp(p.y, j) < 1);
								},
								col)));
				},
				A2($elm$core$List$range, cell.y, yMax)));
		var yMin = A2(
			$elm$core$Basics$min,
			cell.y,
			A2(
				$elm$core$Maybe$withDefault,
				cell.y,
				$elm$core$List$minimum(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.y;
						},
						col))));
		var yLow = A2(
			$elm$core$Debug$log,
			'above',
			A2(
				$elm$core$List$filter,
				function (j) {
					return _Utils_eq(
						(cell.y - j) + 1,
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (p) {
									return (_Utils_cmp(j, p.y) < 1) && (_Utils_cmp(p.y, cell.y) < 1);
								},
								col)));
				},
				A2($elm$core$List$range, yMin, cell.y)));
		var vRanges = A2(
			$elm$core$Debug$log,
			'range',
			$elm$core$List$concat(
				A2(
					$elm$core$List$map,
					function (start) {
						return A2(
							$elm$core$List$map,
							function (end) {
								return A2($elm$core$List$range, start, end);
							},
							yHigh);
					},
					yLow)));
		var vWords = A2(
			$elm$core$Debug$log,
			'vertical',
			A3(
				$elm$core$List$foldl,
				F2(
					function (range, cells) {
						return vWordAt(range) ? _Utils_ap(
							cells,
							_List_fromArray(
								[
									A2(
									$elm$core$List$map,
									function (y) {
										return A2($author$project$Pittan$Cell, cell.x, y);
									},
									range)
								])) : cells;
					}),
				_List_Nil,
				A2(
					$elm$core$List$filter,
					function (range) {
						return $elm$core$List$length(range) > 1;
					},
					vRanges)));
		return A2(
			$elm$core$List$filter,
			function (range) {
				return $elm$core$List$length(range) > 1;
			},
			_Utils_ap(hWords, vWords));
	});
var $author$project$Pittan$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'PDown':
				var id = msg.a;
				var pos = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							moving: $elm$core$Maybe$Just(id),
							nowAt: pos,
							startedAt: pos
						}),
					$elm$core$Platform$Cmd$none);
			case 'PMove':
				var pos = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{nowAt: pos}),
					$elm$core$Platform$Cmd$none);
			case 'PUp':
				var pos = msg.a;
				var y = $elm$core$Basics$floor(pos.y / $author$project$Pittan$unit);
				var x = $elm$core$Basics$floor(pos.x / $author$project$Pittan$unit);
				var removeChar = F2(
					function (c, list) {
						return (model.gameId !== 1) ? $elm$core$List$sort(
							A2(
								$elm$core$List$append,
								A2(
									$elm$core$List$drop,
									1,
									A2(
										$elm$core$List$filter,
										function (d) {
											return _Utils_eq(c, d);
										},
										list)),
								A2(
									$elm$core$List$filter,
									function (d) {
										return !_Utils_eq(c, d);
									},
									list))) : A2(
							$elm$core$List$append,
							A2(
								$elm$core$List$drop,
								1,
								A2(
									$elm$core$List$filter,
									function (d) {
										return _Utils_eq(c, d);
									},
									list)),
							A2(
								$elm$core$List$filter,
								function (d) {
									return !_Utils_eq(c, d);
								},
								list));
					});
				var putChars = A2(
					$elm$core$List$map,
					function ($) {
						return $.c;
					},
					model.conf);
				var onCell = A2(
					$elm$core$List$member,
					A2($author$project$Pittan$Cell, x, y),
					model.board);
				var newConf = onCell ? A2(
					$elm$core$List$map,
					function (p) {
						var _v1 = model.moving;
						if (_v1.$ === 'Just') {
							var id = _v1.a;
							return _Utils_eq(id, p.id) ? _Utils_update(
								p,
								{used: true, x: x, y: y}) : p;
						} else {
							return p;
						}
					},
					model.conf) : A2(
					$elm$core$List$filter,
					function (p) {
						return !_Utils_eq(
							p.id,
							A2($elm$core$Maybe$withDefault, -1, model.moving));
					},
					model.conf);
				var newWords = A2(
					$author$project$Pittan$fromDictionary,
					newConf,
					A2($author$project$Pittan$Cell, x, y));
				var newlyAddedChar = A2(
					$elm$core$Maybe$withDefault,
					A5($author$project$Pittan$Piece, -1, x, y, '', false),
					$elm$core$List$head(
						A2(
							$elm$core$List$filter,
							function (p) {
								return _Utils_eq(p.x, x) && _Utils_eq(p.y, y);
							},
							newConf))).c;
				var makeWord = A2(
					$author$project$Pittan$valid,
					newConf,
					A2($author$project$Pittan$Cell, x, y));
				var initialChars = A2(
					$elm$core$List$map,
					function ($) {
						return $.c;
					},
					$author$project$Pittan$initConf);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							candidates: ($elm$core$List$length(makeWord) > 0) ? A2(removeChar, newlyAddedChar, model.candidates) : model.candidates,
							completed: _Utils_eq(
								$elm$core$List$length(model.conf),
								$elm$core$List$length(model.board)),
							conf: ($elm$core$List$length(makeWord) > 0) ? newConf : A2(
								$elm$core$List$filter,
								function (p) {
									return !_Utils_eq(
										p.id,
										A2($elm$core$Maybe$withDefault, -1, model.moving));
								},
								model.conf),
							foundWords: _Utils_ap(model.foundWords, newWords),
							moving: $elm$core$Maybe$Nothing,
							newWordsAt: makeWord,
							nowAt: {x: 0, y: 0},
							startedAt: {x: 0, y: 0}
						}),
					_Utils_eq(makeWord, _List_Nil) ? $elm$core$Platform$Cmd$none : $author$project$Pittan$playSound(
						$elm$json$Json$Encode$bool(true)));
			case 'GenPiece':
				var p = msg.a;
				var pos = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							conf: A2($elm$core$List$cons, p, model.conf),
							moving: $elm$core$Maybe$Just(p.id),
							nowAt: pos,
							startedAt: pos
						}),
					$elm$core$Platform$Cmd$none);
			case 'CursorPlus':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cursor: (model.cursor < 0) ? (model.cursor + 5) : model.cursor
						}),
					$elm$core$Platform$Cmd$none);
			case 'CursorMinus':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cursor: (_Utils_cmp(
								model.cursor,
								-$elm$core$List$length(model.candidates)) > 0) ? (model.cursor - 5) : model.cursor
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var id = msg.a;
				var newConf = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$head(
						A2($elm$core$List$drop, id, $author$project$Pittan$initialConfs)));
				var newCandidate = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$head(
						A2($elm$core$List$drop, id, $author$project$Pittan$initialCandidates)));
				var newBoard = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$head(
						A2($elm$core$List$drop, id, $author$project$Pittan$boards)));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{board: newBoard, candidates: newCandidate, conf: newConf, gameId: id}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Pittan$PMove = function (a) {
	return {$: 'PMove', a: a};
};
var $author$project$Pittan$PUp = function (a) {
	return {$: 'PUp', a: a};
};
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $andrewMacmurray$elm_simple_animation$Internal$Animation$duration_ = function (_v0) {
	var d = _v0.a;
	return d;
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $andrewMacmurray$elm_simple_animation$Internal$Animation$joinWith = function (f) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$map(f),
		$elm$core$String$concat);
};
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$escapedChars_ = $elm$core$Set$fromList(
	_List_fromArray(
		[
			_Utils_chr('.'),
			_Utils_chr(' '),
			_Utils_chr(','),
			_Utils_chr('#'),
			_Utils_chr('$'),
			_Utils_chr('%'),
			_Utils_chr('('),
			_Utils_chr(')'),
			_Utils_chr('&'),
			_Utils_chr(';'),
			_Utils_chr(':'),
			_Utils_chr('\"'),
			_Utils_chr('\''),
			_Utils_chr('*'),
			_Utils_chr('~'),
			_Utils_chr('!'),
			_Utils_chr('@'),
			_Utils_chr('^'),
			_Utils_chr('+'),
			_Utils_chr('='),
			_Utils_chr('/'),
			_Utils_chr('?'),
			_Utils_chr('>'),
			_Utils_chr('<'),
			_Utils_chr('['),
			_Utils_chr(']'),
			_Utils_chr('{'),
			_Utils_chr('}'),
			_Utils_chr('|'),
			_Utils_chr('`')
		]));
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm$core$Basics$not = _Basics_not;
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$escapedChars = function (c) {
	return !A2($elm$core$Set$member, c, $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$escapedChars_);
};
var $elm$core$String$filter = _String_filter;
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$escape = $elm$core$String$filter($andrewMacmurray$elm_simple_animation$Internal$Animation$Property$escapedChars);
var $elm$core$Basics$round = _Basics_round;
var $andrewMacmurray$elm_simple_animation$Internal$Transform$rounded = F2(
	function (n, val) {
		return $elm$core$String$fromInt(
			$elm$core$Basics$round(val) * n);
	});
var $andrewMacmurray$elm_simple_animation$Internal$Transform$name = function (t) {
	switch (t.$) {
		case 'Translate':
			switch (t.a.$) {
				case 'Y':
					var y_ = t.a.a;
					return 'y' + A2($andrewMacmurray$elm_simple_animation$Internal$Transform$rounded, 1, y_);
				case 'X':
					var x_ = t.a.a;
					return 'x' + A2($andrewMacmurray$elm_simple_animation$Internal$Transform$rounded, 1, x_);
				default:
					var _v1 = t.a;
					var x_ = _v1.a;
					var y_ = _v1.b;
					return 'x' + (A2($andrewMacmurray$elm_simple_animation$Internal$Transform$rounded, 1, x_) + ('y' + A2($andrewMacmurray$elm_simple_animation$Internal$Transform$rounded, 1, y_)));
			}
		case 'Rotate':
			var r_ = t.a;
			return 'r' + A2($andrewMacmurray$elm_simple_animation$Internal$Transform$rounded, 1, r_);
		default:
			var x_ = t.a;
			var y_ = t.b;
			return 'sx' + (A2($andrewMacmurray$elm_simple_animation$Internal$Transform$rounded, 100, x_) + ('sy' + A2($andrewMacmurray$elm_simple_animation$Internal$Transform$rounded, 100, y_)));
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$rounded = F2(
	function (n, val) {
		return $elm$core$String$fromInt(
			$elm$core$Basics$round(val * n));
	});
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$name = function (prop) {
	switch (prop.$) {
		case 'Opacity':
			var n = prop.a;
			return 'o' + A2($andrewMacmurray$elm_simple_animation$Internal$Animation$Property$rounded, 100, n);
		case 'Transform':
			var t = prop.a;
			return $andrewMacmurray$elm_simple_animation$Internal$Transform$name(t);
		default:
			var n = prop.a;
			var p = prop.b;
			return _Utils_ap(
				$andrewMacmurray$elm_simple_animation$Internal$Animation$Property$escape(n),
				$andrewMacmurray$elm_simple_animation$Internal$Animation$Property$escape(p));
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$frameName = function (_v0) {
	var dur = _v0.a;
	var props = _v0.b;
	return 'f' + ($elm$core$String$fromInt(
		$elm$core$Basics$round(dur)) + A2($andrewMacmurray$elm_simple_animation$Internal$Animation$joinWith, $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$name, props));
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$framesNames = $andrewMacmurray$elm_simple_animation$Internal$Animation$joinWith($andrewMacmurray$elm_simple_animation$Internal$Animation$frameName);
var $andrewMacmurray$elm_simple_animation$Internal$Animation$frames_ = function (_v0) {
	var f = _v0.c;
	return f;
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$isEmpty = function (anim) {
	return !$andrewMacmurray$elm_simple_animation$Internal$Animation$duration_(anim);
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$iterationName = function (i) {
	if (i.$ === 'Loop') {
		return 'infinite';
	} else {
		var count = i.a;
		return 'count-' + $elm$core$String$fromInt(count);
	}
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $andrewMacmurray$elm_simple_animation$Internal$Ease$toString = function (e) {
	switch (e.$) {
		case 'Cubic':
			var a = e.a;
			var b = e.b;
			var c = e.c;
			var d = e.d;
			return 'cubic-bezier(' + (A2(
				$elm$core$String$join,
				',',
				_List_fromArray(
					[
						$elm$core$String$fromFloat(a),
						$elm$core$String$fromFloat(b),
						$elm$core$String$fromFloat(c),
						$elm$core$String$fromFloat(d)
					])) + ')');
		case 'Linear':
			return 'linear';
		case 'Ease':
			return 'ease';
		case 'EaseIn':
			return 'ease-in';
		case 'EaseOut':
			return 'ease-out';
		default:
			return 'ease-in-out';
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$optionName = function (o) {
	switch (o.$) {
		case 'Delay':
			var n = o.a;
			return 'd' + $elm$core$String$fromInt(n);
		case 'Ease':
			var ease = o.a;
			return $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$escape(
				$andrewMacmurray$elm_simple_animation$Internal$Ease$toString(ease));
		case 'Iteration':
			var i = o.a;
			return $andrewMacmurray$elm_simple_animation$Internal$Animation$iterationName(i);
		case 'Yoyo':
			return 'yoyo';
		default:
			return 'rev';
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$optionNames = $andrewMacmurray$elm_simple_animation$Internal$Animation$joinWith($andrewMacmurray$elm_simple_animation$Internal$Animation$optionName);
var $andrewMacmurray$elm_simple_animation$Internal$Animation$rawOptions_ = function (_v0) {
	var o = _v0.b;
	return o;
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$name_ = function (anim) {
	return $andrewMacmurray$elm_simple_animation$Internal$Animation$isEmpty(anim) ? 'anim-empty' : ('anim-' + ($elm$core$String$fromInt(
		$andrewMacmurray$elm_simple_animation$Internal$Animation$duration_(anim)) + ($andrewMacmurray$elm_simple_animation$Internal$Animation$optionNames(
		$andrewMacmurray$elm_simple_animation$Internal$Animation$rawOptions_(anim)) + $andrewMacmurray$elm_simple_animation$Internal$Animation$framesNames(
		$andrewMacmurray$elm_simple_animation$Internal$Animation$frames_(anim)))));
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $andrewMacmurray$elm_simple_animation$Internal$Unit$ms = function (n) {
	return $elm$core$String$fromInt(n) + 'ms';
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$animationDuration = function (anim) {
	return $andrewMacmurray$elm_simple_animation$Internal$Unit$ms(
		$andrewMacmurray$elm_simple_animation$Internal$Animation$duration_(anim));
};
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
var $andrewMacmurray$elm_simple_animation$Internal$Animation$collectOption = F2(
	function (o, opts) {
		switch (o.$) {
			case 'Delay':
				var ms = o.a;
				return _Utils_update(
					opts,
					{
						delay: $elm$core$Maybe$Just(ms)
					});
			case 'Iteration':
				var i = o.a;
				return _Utils_update(
					opts,
					{
						iteration: $elm$core$Maybe$Just(i)
					});
			case 'Ease':
				var e = o.a;
				return _Utils_update(
					opts,
					{
						timingFunction: $elm$core$Maybe$Just(e)
					});
			case 'Yoyo':
				return _Utils_update(
					opts,
					{isYoyo: true});
			default:
				return _Utils_update(
					opts,
					{reversed: true});
		}
	});
var $andrewMacmurray$elm_simple_animation$Internal$Animation$defaults = {delay: $elm$core$Maybe$Nothing, isYoyo: false, iteration: $elm$core$Maybe$Nothing, reversed: false, timingFunction: $elm$core$Maybe$Nothing};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Count = function (a) {
	return {$: 'Count', a: a};
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Loop = {$: 'Loop'};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$iterationForYoyo = function (opts) {
	var _v0 = opts.iteration;
	if (_v0.$ === 'Just') {
		if (_v0.a.$ === 'Loop') {
			var _v1 = _v0.a;
			return $andrewMacmurray$elm_simple_animation$Internal$Animation$Loop;
		} else {
			var n = _v0.a.a;
			return $andrewMacmurray$elm_simple_animation$Internal$Animation$Count(n * 2);
		}
	} else {
		return $andrewMacmurray$elm_simple_animation$Internal$Animation$Count(2);
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$normalise = function (opts) {
	return opts.isYoyo ? _Utils_update(
		opts,
		{
			iteration: $elm$core$Maybe$Just(
				$andrewMacmurray$elm_simple_animation$Internal$Animation$iterationForYoyo(opts))
		}) : opts;
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$options_ = A2(
	$elm$core$Basics$composeR,
	$andrewMacmurray$elm_simple_animation$Internal$Animation$rawOptions_,
	A2(
		$elm$core$Basics$composeR,
		A2($elm$core$List$foldl, $andrewMacmurray$elm_simple_animation$Internal$Animation$collectOption, $andrewMacmurray$elm_simple_animation$Internal$Animation$defaults),
		$andrewMacmurray$elm_simple_animation$Internal$Animation$normalise));
var $andrewMacmurray$elm_simple_animation$Internal$Animation$renderDirection = function (d) {
	switch (d.$) {
		case 'Alternate_':
			return 'alternate';
		case 'Reverse_':
			return 'reverse';
		default:
			return 'alternate-reverse';
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$renderIteration = function (i) {
	if (i.$ === 'Loop') {
		return 'infinite';
	} else {
		var count = i.a;
		return $elm$core$String$fromInt(count);
	}
};
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
var $andrewMacmurray$elm_simple_animation$Internal$Animation$renderOption = F2(
	function (name, toProp) {
		return $elm$core$Maybe$map(
			function (x) {
				return name + (': ' + toProp(x));
			});
	});
var $andrewMacmurray$elm_simple_animation$Internal$Animation$AlternateReverse_ = {$: 'AlternateReverse_'};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Alternate_ = {$: 'Alternate_'};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Reverse_ = {$: 'Reverse_'};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$toDirection = function (opts) {
	return (opts.isYoyo && opts.reversed) ? $elm$core$Maybe$Just($andrewMacmurray$elm_simple_animation$Internal$Animation$AlternateReverse_) : (opts.reversed ? $elm$core$Maybe$Just($andrewMacmurray$elm_simple_animation$Internal$Animation$Reverse_) : (opts.isYoyo ? $elm$core$Maybe$Just($andrewMacmurray$elm_simple_animation$Internal$Animation$Alternate_) : $elm$core$Maybe$Nothing));
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$renderOptions_ = function (opts) {
	return _List_fromArray(
		[
			A3($andrewMacmurray$elm_simple_animation$Internal$Animation$renderOption, 'animation-delay', $andrewMacmurray$elm_simple_animation$Internal$Unit$ms, opts.delay),
			A3($andrewMacmurray$elm_simple_animation$Internal$Animation$renderOption, 'animation-timing-function', $andrewMacmurray$elm_simple_animation$Internal$Ease$toString, opts.timingFunction),
			A3($andrewMacmurray$elm_simple_animation$Internal$Animation$renderOption, 'animation-iteration-count', $andrewMacmurray$elm_simple_animation$Internal$Animation$renderIteration, opts.iteration),
			A3(
			$andrewMacmurray$elm_simple_animation$Internal$Animation$renderOption,
			'animation-direction',
			$andrewMacmurray$elm_simple_animation$Internal$Animation$renderDirection,
			$andrewMacmurray$elm_simple_animation$Internal$Animation$toDirection(opts))
		]);
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$renderOptions = A2(
	$elm$core$Basics$composeR,
	$andrewMacmurray$elm_simple_animation$Internal$Animation$options_,
	A2(
		$elm$core$Basics$composeR,
		$andrewMacmurray$elm_simple_animation$Internal$Animation$renderOptions_,
		$elm$core$List$filterMap($elm$core$Basics$identity)));
var $andrewMacmurray$elm_simple_animation$Internal$Animation$classProperties = function (anim) {
	return A2(
		$elm$core$String$join,
		';\n',
		A2(
			$elm$core$List$append,
			_List_fromArray(
				[
					'animation-name: ' + $andrewMacmurray$elm_simple_animation$Internal$Animation$name_(anim),
					'animation-duration: ' + $andrewMacmurray$elm_simple_animation$Internal$Animation$animationDuration(anim),
					'animation-fill-mode: both'
				]),
			$andrewMacmurray$elm_simple_animation$Internal$Animation$renderOptions(anim)));
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$classDefinition_ = function (anim) {
	return '.' + ($andrewMacmurray$elm_simple_animation$Internal$Animation$name_(anim) + ('{\n' + ($andrewMacmurray$elm_simple_animation$Internal$Animation$classProperties(anim) + '\n};')));
};
var $andrewMacmurray$elm_simple_animation$Internal$Unit$pc = function (n) {
	return $elm$core$String$fromFloat(n) + '%';
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$filterMaybes = $elm$core$List$filterMap($elm$core$Basics$identity);
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$getProp = function (f) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$filterMap(f),
		$elm$core$List$head);
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$opacity_ = function (p) {
	if (p.$ === 'Opacity') {
		var n = p.a;
		return $elm$core$Maybe$Just(
			'opacity:' + $elm$core$String$fromFloat(n));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$raw_ = function (p) {
	if (p.$ === 'Raw') {
		var k = p.a;
		var v = p.b;
		return $elm$core$Maybe$Just(k + (':' + v));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$collectTransforms = A2(
	$elm$core$List$foldl,
	F2(
		function (val, acc) {
			if (val.$ === 'Transform') {
				var t = val.a;
				return A2($elm$core$List$cons, t, acc);
			} else {
				return acc;
			}
		}),
	_List_Nil);
var $andrewMacmurray$elm_simple_animation$Internal$Transform$combine = F2(
	function (transform, combined) {
		switch (transform.$) {
			case 'Rotate':
				var n = transform.a;
				return _Utils_update(
					combined,
					{
						rotate: $elm$core$Maybe$Just(n)
					});
			case 'ScaleXY':
				var x_ = transform.a;
				var y_ = transform.b;
				return _Utils_update(
					combined,
					{
						scale: $elm$core$Maybe$Just(
							_Utils_Tuple2(x_, y_))
					});
			default:
				switch (transform.a.$) {
					case 'XY':
						var _v1 = transform.a;
						var x_ = _v1.a;
						var y_ = _v1.b;
						return _Utils_update(
							combined,
							{
								xy: $elm$core$Maybe$Just(
									_Utils_Tuple2(x_, y_))
							});
					case 'X':
						var n = transform.a.a;
						return _Utils_update(
							combined,
							{
								x: $elm$core$Maybe$Just(n)
							});
					default:
						var n = transform.a.a;
						return _Utils_update(
							combined,
							{
								y: $elm$core$Maybe$Just(n)
							});
				}
		}
	});
var $andrewMacmurray$elm_simple_animation$Internal$Transform$empty = {rotate: $elm$core$Maybe$Nothing, scale: $elm$core$Maybe$Nothing, x: $elm$core$Maybe$Nothing, xy: $elm$core$Maybe$Nothing, y: $elm$core$Maybe$Nothing};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$render_ = function (f) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$Maybe$map(f),
		$elm$core$Maybe$withDefault(''));
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$deg = function (n) {
	return $elm$core$String$fromFloat(n) + 'deg';
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$join = $elm$core$String$join('');
var $andrewMacmurray$elm_simple_animation$Internal$Transform$rotate_ = function (n) {
	return $andrewMacmurray$elm_simple_animation$Internal$Transform$join(
		_List_fromArray(
			[
				'rotate(',
				$andrewMacmurray$elm_simple_animation$Internal$Transform$deg(n),
				')'
			]));
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$scale_ = function (_v0) {
	var x_ = _v0.a;
	var y_ = _v0.b;
	return $andrewMacmurray$elm_simple_animation$Internal$Transform$join(
		_List_fromArray(
			[
				'scale(',
				$elm$core$String$fromFloat(x_),
				',',
				$elm$core$String$fromFloat(y_),
				')'
			]));
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$px = function (n) {
	return $elm$core$String$fromFloat(n) + 'px';
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$translateX_ = function (n) {
	return $andrewMacmurray$elm_simple_animation$Internal$Transform$join(
		_List_fromArray(
			[
				'translateX(',
				$andrewMacmurray$elm_simple_animation$Internal$Transform$px(n),
				')'
			]));
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$translateY_ = function (n) {
	return $andrewMacmurray$elm_simple_animation$Internal$Transform$join(
		_List_fromArray(
			[
				'translateY(',
				$andrewMacmurray$elm_simple_animation$Internal$Transform$px(n),
				')'
			]));
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$translate_ = function (_v0) {
	var x_ = _v0.a;
	var y_ = _v0.b;
	return $andrewMacmurray$elm_simple_animation$Internal$Transform$join(
		_List_fromArray(
			[
				'translate(',
				$andrewMacmurray$elm_simple_animation$Internal$Transform$px(x_),
				',',
				$andrewMacmurray$elm_simple_animation$Internal$Transform$px(y_),
				')'
			]));
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$render = function (combined) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeR, $elm$core$String$isEmpty, $elm$core$Basics$not),
			_List_fromArray(
				[
					A2($andrewMacmurray$elm_simple_animation$Internal$Transform$render_, $andrewMacmurray$elm_simple_animation$Internal$Transform$translate_, combined.xy),
					A2($andrewMacmurray$elm_simple_animation$Internal$Transform$render_, $andrewMacmurray$elm_simple_animation$Internal$Transform$translateX_, combined.x),
					A2($andrewMacmurray$elm_simple_animation$Internal$Transform$render_, $andrewMacmurray$elm_simple_animation$Internal$Transform$translateY_, combined.y),
					A2($andrewMacmurray$elm_simple_animation$Internal$Transform$render_, $andrewMacmurray$elm_simple_animation$Internal$Transform$scale_, combined.scale),
					A2($andrewMacmurray$elm_simple_animation$Internal$Transform$render_, $andrewMacmurray$elm_simple_animation$Internal$Transform$rotate_, combined.rotate)
				])));
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$toString = A2(
	$elm$core$Basics$composeR,
	A2($elm$core$List$foldl, $andrewMacmurray$elm_simple_animation$Internal$Transform$combine, $andrewMacmurray$elm_simple_animation$Internal$Transform$empty),
	$andrewMacmurray$elm_simple_animation$Internal$Transform$render);
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$transform_ = function (props) {
	var _v0 = $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$collectTransforms(props);
	if (!_v0.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var transforms = _v0;
		return $elm$core$Maybe$Just(
			'transform:' + $andrewMacmurray$elm_simple_animation$Internal$Transform$toString(transforms));
	}
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$render = function (props) {
	return A2(
		$elm$core$String$join,
		';',
		$andrewMacmurray$elm_simple_animation$Internal$Animation$Property$filterMaybes(
			$elm$core$List$concat(
				_List_fromArray(
					[
						A2($elm$core$List$map, $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$raw_, props),
						_List_fromArray(
						[
							A2($andrewMacmurray$elm_simple_animation$Internal$Animation$Property$getProp, $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$opacity_, props),
							$andrewMacmurray$elm_simple_animation$Internal$Animation$Property$transform_(props)
						])
					]))));
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$renderFrame = function (_v0) {
	var percent = _v0.a;
	var properties = _v0.b;
	return $andrewMacmurray$elm_simple_animation$Internal$Unit$pc(percent) + ('{' + ($andrewMacmurray$elm_simple_animation$Internal$Animation$Property$render(properties) + ';}'));
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$keyframes_ = A2(
	$elm$core$Basics$composeR,
	$andrewMacmurray$elm_simple_animation$Internal$Animation$frames_,
	A2(
		$elm$core$Basics$composeR,
		$elm$core$List$map($andrewMacmurray$elm_simple_animation$Internal$Animation$renderFrame),
		$elm$core$String$join('\n')));
var $andrewMacmurray$elm_simple_animation$Internal$Animation$keyframesAnimation_ = function (anim) {
	return '@keyframes ' + ($andrewMacmurray$elm_simple_animation$Internal$Animation$name_(anim) + ('{' + ($andrewMacmurray$elm_simple_animation$Internal$Animation$keyframes_(anim) + '}')));
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$stylesheet_ = function (anim) {
	return $andrewMacmurray$elm_simple_animation$Internal$Animation$keyframesAnimation_(anim) + ('\n' + $andrewMacmurray$elm_simple_animation$Internal$Animation$classDefinition_(anim));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Animated$toStylesheet_ = function (anim) {
	return A3(
		$elm$html$Html$node,
		'style',
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(
				$andrewMacmurray$elm_simple_animation$Internal$Animation$stylesheet_(anim))
			]));
};
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Animated$node = F5(
	function (options, node_, anim, attrs, els) {
		return A2(
			node_,
			A2(
				$elm$core$List$cons,
				options._class(
					$andrewMacmurray$elm_simple_animation$Internal$Animation$name_(anim)),
				attrs),
			A2(
				$elm$core$List$cons,
				$andrewMacmurray$elm_simple_animation$Simple$Animation$Animated$toStylesheet_(anim),
				els));
	});
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Animated$svg = $andrewMacmurray$elm_simple_animation$Simple$Animation$Animated$node;
var $author$project$Pittan$animatedSvg = $andrewMacmurray$elm_simple_animation$Simple$Animation$Animated$svg(
	{_class: $elm$svg$Svg$Attributes$class});
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $author$project$Pittan$animatedCover = $author$project$Pittan$animatedSvg($elm$svg$Svg$g);
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$Pittan$cellView = function (cell) {
	return A2(
		$elm$svg$Svg$rect,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$x(
				$elm$core$String$fromInt($author$project$Pittan$unit * cell.x)),
				$elm$svg$Svg$Attributes$y(
				$elm$core$String$fromInt($author$project$Pittan$unit * cell.y)),
				$elm$svg$Svg$Attributes$width(
				$elm$core$String$fromInt($author$project$Pittan$unit)),
				$elm$svg$Svg$Attributes$height(
				$elm$core$String$fromInt($author$project$Pittan$unit)),
				$elm$svg$Svg$Attributes$stroke('black'),
				$elm$svg$Svg$Attributes$strokeWidth('2px'),
				$elm$svg$Svg$Attributes$fill('pink')
			]),
		_List_Nil);
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$Opacity = function (a) {
	return {$: 'Opacity', a: a};
};
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Property$opacity = $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$Opacity;
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$Transform = function (a) {
	return {$: 'Transform', a: a};
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$ScaleXY = F2(
	function (a, b) {
		return {$: 'ScaleXY', a: a, b: b};
	});
var $andrewMacmurray$elm_simple_animation$Internal$Transform$scaleXY = $andrewMacmurray$elm_simple_animation$Internal$Transform$ScaleXY;
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Property$scale = function (n) {
	return $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$Transform(
		A2($andrewMacmurray$elm_simple_animation$Internal$Transform$scaleXY, n, n));
};
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Property$scaleXY = F2(
	function (x_, y_) {
		return $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$Transform(
			A2($andrewMacmurray$elm_simple_animation$Internal$Transform$scaleXY, x_, y_));
	});
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Step = F2(
	function (a, b) {
		return {$: 'Step', a: a, b: b};
	});
var $andrewMacmurray$elm_simple_animation$Simple$Animation$step = $andrewMacmurray$elm_simple_animation$Simple$Animation$Step;
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Stepped = function (a) {
	return {$: 'Stepped', a: a};
};
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Animation = F3(
	function (a, b, c) {
		return {$: 'Animation', a: a, b: b, c: c};
	});
var $andrewMacmurray$elm_simple_animation$Internal$Animation$Frame = F2(
	function (a, b) {
		return {$: 'Frame', a: a, b: b};
	});
var $andrewMacmurray$elm_simple_animation$Simple$Animation$duration = $andrewMacmurray$elm_simple_animation$Internal$Animation$duration_;
var $elm$core$Basics$ge = _Utils_ge;
var $andrewMacmurray$elm_simple_animation$Simple$Animation$adjustCompleteWait = F2(
	function (anim, timePassed) {
		var duration_ = $andrewMacmurray$elm_simple_animation$Simple$Animation$duration(anim);
		return ((duration_ - timePassed) >= 1) ? duration_ : (timePassed + 1);
	});
var $andrewMacmurray$elm_simple_animation$Simple$Animation$frameProps = function (_v0) {
	var props = _v0.b;
	return props;
};
var $andrewMacmurray$elm_simple_animation$Simple$Animation$accumDuration = F2(
	function (step_, curr) {
		switch (step_.$) {
			case 'Step':
				var d = step_.a;
				return d + curr;
			case 'Wait':
				var d = step_.a;
				return d + curr;
			default:
				var anim = step_.a;
				return A2($andrewMacmurray$elm_simple_animation$Simple$Animation$adjustCompleteWait, anim, curr);
		}
	});
var $andrewMacmurray$elm_simple_animation$Simple$Animation$totalDuration = A2($elm$core$List$foldl, $andrewMacmurray$elm_simple_animation$Simple$Animation$accumDuration, 0);
var $andrewMacmurray$elm_simple_animation$Simple$Animation$toFrames = F2(
	function (firstFrame, steps_) {
		var percentPerMs = 100 / $andrewMacmurray$elm_simple_animation$Simple$Animation$totalDuration(steps_);
		var getFrame = F2(
			function (f, _v2) {
				var n = _v2.a;
				var xs = _v2.b;
				var cur = _v2.c;
				switch (f.$) {
					case 'Step':
						var d = f.a;
						var props = f.b;
						return _Utils_Tuple3(
							n + d,
							_Utils_ap(
								xs,
								_List_fromArray(
									[cur])),
							A2($andrewMacmurray$elm_simple_animation$Internal$Animation$Frame, percentPerMs * (n + d), props));
					case 'Wait':
						var d = f.a;
						return _Utils_Tuple3(
							n + d,
							_Utils_ap(
								xs,
								_List_fromArray(
									[cur])),
							A2(
								$andrewMacmurray$elm_simple_animation$Internal$Animation$Frame,
								percentPerMs * (n + d),
								$andrewMacmurray$elm_simple_animation$Simple$Animation$frameProps(cur)));
					default:
						var d = f.a;
						var dur = A2($andrewMacmurray$elm_simple_animation$Simple$Animation$adjustCompleteWait, d, n);
						return _Utils_Tuple3(
							dur,
							_Utils_ap(
								xs,
								_List_fromArray(
									[cur])),
							A2(
								$andrewMacmurray$elm_simple_animation$Internal$Animation$Frame,
								percentPerMs * dur,
								$andrewMacmurray$elm_simple_animation$Simple$Animation$frameProps(cur)));
				}
			});
		return function (_v0) {
			var xs = _v0.b;
			var curr = _v0.c;
			return _Utils_ap(
				xs,
				_List_fromArray(
					[curr]));
		}(
			A3(
				$elm$core$List$foldl,
				getFrame,
				_Utils_Tuple3(
					0,
					_List_Nil,
					A2($andrewMacmurray$elm_simple_animation$Internal$Animation$Frame, 0, firstFrame)),
				steps_));
	});
var $andrewMacmurray$elm_simple_animation$Simple$Animation$toAnimation = function (_v0) {
	var s = _v0.a;
	return A3(
		$andrewMacmurray$elm_simple_animation$Internal$Animation$Animation,
		$andrewMacmurray$elm_simple_animation$Simple$Animation$totalDuration(s.steps),
		s.options,
		A2($andrewMacmurray$elm_simple_animation$Simple$Animation$toFrames, s.startAt, s.steps));
};
var $andrewMacmurray$elm_simple_animation$Simple$Animation$steps = F2(
	function (_v0, steps_) {
		var options = _v0.options;
		var startAt = _v0.startAt;
		return $andrewMacmurray$elm_simple_animation$Simple$Animation$toAnimation(
			$andrewMacmurray$elm_simple_animation$Simple$Animation$Stepped(
				{options: options, startAt: startAt, steps: steps_}));
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $andrewMacmurray$elm_simple_animation$Internal$Transform$Translate = function (a) {
	return {$: 'Translate', a: a};
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$X = function (a) {
	return {$: 'X', a: a};
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$x = A2($elm$core$Basics$composeL, $andrewMacmurray$elm_simple_animation$Internal$Transform$Translate, $andrewMacmurray$elm_simple_animation$Internal$Transform$X);
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Property$x = A2($elm$core$Basics$composeL, $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$Transform, $andrewMacmurray$elm_simple_animation$Internal$Transform$x);
var $andrewMacmurray$elm_simple_animation$Internal$Transform$Y = function (a) {
	return {$: 'Y', a: a};
};
var $andrewMacmurray$elm_simple_animation$Internal$Transform$y = A2($elm$core$Basics$composeL, $andrewMacmurray$elm_simple_animation$Internal$Transform$Translate, $andrewMacmurray$elm_simple_animation$Internal$Transform$Y);
var $andrewMacmurray$elm_simple_animation$Simple$Animation$Property$y = A2($elm$core$Basics$composeL, $andrewMacmurray$elm_simple_animation$Internal$Animation$Property$Transform, $andrewMacmurray$elm_simple_animation$Internal$Transform$y);
var $author$project$Pittan$coverAnimation = function (ranges) {
	var yMin = function (range) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$minimum(
				A2(
					$elm$core$List$map,
					function ($) {
						return $.y;
					},
					range)));
	};
	var yMax = function (range) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$maximum(
				A2(
					$elm$core$List$map,
					function ($) {
						return $.y;
					},
					range)));
	};
	var xMin = function (range) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$minimum(
				A2(
					$elm$core$List$map,
					function ($) {
						return $.x;
					},
					range)));
	};
	var xMax = function (range) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$maximum(
				A2(
					$elm$core$List$map,
					function ($) {
						return $.x;
					},
					range)));
	};
	var oneStep = function (range) {
		return A2(
			$andrewMacmurray$elm_simple_animation$Simple$Animation$step,
			500,
			_List_fromArray(
				[
					$andrewMacmurray$elm_simple_animation$Simple$Animation$Property$x(
					$author$project$Pittan$unit * xMin(range)),
					$andrewMacmurray$elm_simple_animation$Simple$Animation$Property$y(
					$author$project$Pittan$unit * yMin(range)),
					A2(
					$andrewMacmurray$elm_simple_animation$Simple$Animation$Property$scaleXY,
					(xMax(range) - xMin(range)) + 1,
					(yMax(range) - yMin(range)) + 1)
				]));
	};
	return A2(
		$andrewMacmurray$elm_simple_animation$Simple$Animation$steps,
		{
			options: _List_Nil,
			startAt: _List_fromArray(
				[
					$andrewMacmurray$elm_simple_animation$Simple$Animation$Property$scale(0),
					$andrewMacmurray$elm_simple_animation$Simple$Animation$Property$opacity(0)
				])
		},
		_Utils_ap(
			A2($elm$core$List$map, oneStep, ranges),
			_List_fromArray(
				[
					A2(
					$andrewMacmurray$elm_simple_animation$Simple$Animation$step,
					500,
					_List_fromArray(
						[
							$andrewMacmurray$elm_simple_animation$Simple$Animation$Property$scale(0)
						]))
				])));
};
var $elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var $author$project$Pittan$boardView = function (model) {
	return A2(
		$elm$svg$Svg$g,
		_List_Nil,
		_Utils_ap(
			A2($elm$core$List$map, $author$project$Pittan$cellView, model.board),
			_List_fromArray(
				[
					A3(
					$author$project$Pittan$animatedCover,
					$author$project$Pittan$coverAnimation(model.newWordsAt),
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$rect,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$width(
									$elm$core$String$fromInt($author$project$Pittan$unit)),
									$elm$svg$Svg$Attributes$height(
									$elm$core$String$fromInt($author$project$Pittan$unit)),
									$elm$svg$Svg$Attributes$fill('red'),
									$elm$svg$Svg$Attributes$fillOpacity('0.3'),
									$elm$svg$Svg$Attributes$stroke('yellow'),
									$elm$svg$Svg$Attributes$strokeWidth('5px')
								]),
							_List_Nil)
						]))
				])));
};
var $author$project$Pittan$CursorMinus = {$: 'CursorMinus'};
var $author$project$Pittan$CursorPlus = {$: 'CursorPlus'};
var $author$project$Pittan$GenPiece = F2(
	function (a, b) {
		return {$: 'GenPiece', a: a, b: b};
	});
var $elm$svg$Svg$Attributes$clipPath = _VirtualDom_attribute('clip-path');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fontSize = _VirtualDom_attribute('font-size');
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
var $elm$svg$Svg$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$defaultOptions = {preventDefault: true, stopPropagation: false};
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$Event = F5(
	function (pointerType, pointer, pointerId, isPrimary, contactDetails) {
		return {contactDetails: contactDetails, isPrimary: isPrimary, pointer: pointer, pointerId: pointerId, pointerType: pointerType};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$ContactDetails = F5(
	function (width, height, pressure, tiltX, tiltY) {
		return {height: height, pressure: pressure, tiltX: tiltX, tiltY: tiltY, width: width};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$map5 = _Json_map5;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$contactDetailsDecoder = A6(
	$elm$json$Json$Decode$map5,
	$mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$ContactDetails,
	A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'pressure', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'tiltX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'tiltY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event = F6(
	function (keys, button, clientPos, offsetPos, pagePos, screenPos) {
		return {button: button, clientPos: clientPos, keys: keys, offsetPos: offsetPos, pagePos: pagePos, screenPos: screenPos};
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton = {$: 'BackButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton = {$: 'ErrorButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton = {$: 'ForwardButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton = {$: 'MainButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton = {$: 'MiddleButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton = {$: 'SecondButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId = function (id) {
	switch (id) {
		case 0:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton;
		case 1:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton;
		case 2:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton;
		case 3:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton;
		case 4:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton;
		default:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton;
	}
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder = A2(
	$elm$json$Json$Decode$map,
	$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId,
	A2($elm$json$Json$Decode$field, 'button', $elm$json$Json$Decode$int));
var $mpizenberg$elm_pointer_events$Internal$Decode$clientPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$Keys = F3(
	function (alt, ctrl, shift) {
		return {alt: alt, ctrl: ctrl, shift: shift};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $mpizenberg$elm_pointer_events$Internal$Decode$keys = A4(
	$elm$json$Json$Decode$map3,
	$mpizenberg$elm_pointer_events$Internal$Decode$Keys,
	A2($elm$json$Json$Decode$field, 'altKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'ctrlKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'shiftKey', $elm$json$Json$Decode$bool));
var $elm$json$Json$Decode$map6 = _Json_map6;
var $mpizenberg$elm_pointer_events$Internal$Decode$offsetPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$pagePos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$screenPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'screenX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'screenY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder = A7($elm$json$Json$Decode$map6, $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event, $mpizenberg$elm_pointer_events$Internal$Decode$keys, $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder, $mpizenberg$elm_pointer_events$Internal$Decode$clientPos, $mpizenberg$elm_pointer_events$Internal$Decode$offsetPos, $mpizenberg$elm_pointer_events$Internal$Decode$pagePos, $mpizenberg$elm_pointer_events$Internal$Decode$screenPos);
var $elm$json$Json$Decode$string = _Json_decodeString;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$MouseType = {$: 'MouseType'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$PenType = {$: 'PenType'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$TouchType = {$: 'TouchType'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$stringToPointerType = function (str) {
	switch (str) {
		case 'pen':
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$PenType;
		case 'touch':
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$TouchType;
		default:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$MouseType;
	}
};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$pointerTypeDecoder = A2($elm$json$Json$Decode$map, $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$stringToPointerType, $elm$json$Json$Decode$string);
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$eventDecoder = A6(
	$elm$json$Json$Decode$map5,
	$mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$Event,
	A2($elm$json$Json$Decode$field, 'pointerType', $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$pointerTypeDecoder),
	$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder,
	A2($elm$json$Json$Decode$field, 'pointerId', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'isPrimary', $elm$json$Json$Decode$bool),
	$mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$contactDetailsDecoder);
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onWithOptions = F3(
	function (event, options, tag) {
		return A2(
			$elm$html$Html$Events$custom,
			event,
			A2(
				$elm$json$Json$Decode$map,
				function (ev) {
					return {
						message: tag(ev),
						preventDefault: options.preventDefault,
						stopPropagation: options.stopPropagation
					};
				},
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$eventDecoder));
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onDown = A2($mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onWithOptions, 'pointerdown', $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$defaultOptions);
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $author$project$Pittan$candView = function (model) {
	var letterView = F2(
		function (i, c) {
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$transform(
						'translate (0' + (',' + ($elm$core$String$fromInt((i + model.cursor) * $author$project$Pittan$unit) + ')'))),
						$elm$svg$Svg$Attributes$clipPath('url(#candClip)'),
						$mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onDown(
						function (event) {
							return $author$project$Pittan$GenPiece(
								A5(
									$author$project$Pittan$Piece,
									$elm$core$List$length(model.conf),
									1,
									(i + model.cursor) + 1,
									c,
									true))(
								{x: event.pointer.offsetPos.a, y: event.pointer.offsetPos.b});
						})
					]),
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$rect,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$width(
								$elm$core$String$fromInt($author$project$Pittan$unit)),
								$elm$svg$Svg$Attributes$height(
								$elm$core$String$fromInt($author$project$Pittan$unit)),
								$elm$svg$Svg$Attributes$fill('yellow'),
								$elm$svg$Svg$Attributes$fillOpacity('0.1'),
								$elm$svg$Svg$Attributes$stroke('black'),
								$elm$svg$Svg$Attributes$strokeWidth('3px'),
								$elm$svg$Svg$Attributes$clipPath('url(#candClip)')
							]),
						_List_Nil),
						A2(
						$elm$svg$Svg$text_,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$x(
								$elm$core$String$fromInt(($author$project$Pittan$unit / 4) | 0)),
								$elm$svg$Svg$Attributes$y(
								$elm$core$String$fromInt(((2 * $author$project$Pittan$unit) / 3) | 0)),
								$elm$svg$Svg$Attributes$fontSize(
								$elm$core$String$fromInt(($author$project$Pittan$unit / 2) | 0)),
								$elm$svg$Svg$Attributes$stroke('black'),
								$elm$svg$Svg$Attributes$clipPath('url(#candClip)')
							]),
						_List_fromArray(
							[
								$elm$svg$Svg$text(c)
							]))
					]));
		});
	return A2(
		$elm$svg$Svg$g,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$transform(
				'translate(60,' + ($elm$core$String$fromInt($author$project$Pittan$unit) + ')'))
			]),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$clipPath('url(#candClip)')
					]),
				A2($elm$core$List$indexedMap, letterView, model.candidates)),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d(
						'M 0 -5 l ' + ($elm$core$String$fromInt($author$project$Pittan$unit) + (' 0 l ' + ($elm$core$String$fromInt(((-$author$project$Pittan$unit) / 2) | 0) + (' ' + ($elm$core$String$fromInt(((-$author$project$Pittan$unit) / 2) | 0) + ' z')))))),
						$elm$svg$Svg$Attributes$fill('black'),
						$elm$svg$Svg$Events$onClick($author$project$Pittan$CursorPlus)
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d(
						'M 0 ' + ($elm$core$String$fromInt((8 * $author$project$Pittan$unit) + 5) + (' l ' + ($elm$core$String$fromInt($author$project$Pittan$unit) + (' 0 l ' + ($elm$core$String$fromInt(((-$author$project$Pittan$unit) / 2) | 0) + (' ' + ($elm$core$String$fromInt(($author$project$Pittan$unit / 2) | 0) + ' z')))))))),
						$elm$svg$Svg$Attributes$fill('black'),
						$elm$svg$Svg$Events$onClick($author$project$Pittan$CursorMinus)
					]),
				_List_Nil)
			]));
};
var $elm$svg$Svg$clipPath = $elm$svg$Svg$trustedNode('clipPath');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$html$Html$li = _VirtualDom_node('li');
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onMove = A2($mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onWithOptions, 'pointermove', $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$defaultOptions);
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onUp = A2($mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onWithOptions, 'pointerup', $mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$defaultOptions);
var $author$project$Pittan$PDown = F2(
	function (a, b) {
		return {$: 'PDown', a: a, b: b};
	});
var $author$project$Pittan$pieceView = F2(
	function (piece, model) {
		var msg = piece.used ? _List_Nil : _List_fromArray(
			[
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onDown(
				function (event) {
					return A2(
						$author$project$Pittan$PDown,
						piece.id,
						{x: event.pointer.offsetPos.a, y: event.pointer.offsetPos.b});
				})
			]);
		var dy = function () {
			var _v1 = model.moving;
			if (_v1.$ === 'Nothing') {
				return 0;
			} else {
				var id = _v1.a;
				return _Utils_eq(id, piece.id) ? (model.nowAt.y - model.startedAt.y) : 0;
			}
		}();
		var dx = function () {
			var _v0 = model.moving;
			if (_v0.$ === 'Nothing') {
				return 0;
			} else {
				var id = _v0.a;
				return _Utils_eq(id, piece.id) ? (model.nowAt.x - model.startedAt.x) : 0;
			}
		}();
		var dstring = 'translate(' + ($elm$core$String$fromFloat((piece.x * $author$project$Pittan$unit) + dx) + (', ' + ($elm$core$String$fromFloat((piece.y * $author$project$Pittan$unit) + dy) + ')')));
		return A2(
			$elm$svg$Svg$g,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$transform(dstring)
					]),
				msg),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromInt($author$project$Pittan$unit)),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromInt($author$project$Pittan$unit)),
							$elm$svg$Svg$Attributes$fill('red'),
							$elm$svg$Svg$Attributes$fillOpacity('0.4'),
							$elm$svg$Svg$Attributes$stroke('black')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$text_,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromInt(($author$project$Pittan$unit / 4) | 0)),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromInt(((2 * $author$project$Pittan$unit) / 3) | 0)),
							$elm$svg$Svg$Attributes$fontSize(
							$elm$core$String$fromInt(($author$project$Pittan$unit / 2) | 0)),
							$elm$svg$Svg$Attributes$stroke('black')
						]),
					_List_fromArray(
						[
							$elm$svg$Svg$text(piece.c)
						]))
				]));
	});
var $author$project$Pittan$NextGame = function (a) {
	return {$: 'NextGame', a: a};
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Pittan$radioButton1 = F2(
	function (gameId, label) {
		return A2(
			$elm$html$Html$label,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('radio'),
							$elm$html$Html$Attributes$name('games'),
							$elm$html$Html$Events$onClick(
							$author$project$Pittan$NextGame(gameId))
						]),
					_List_Nil),
					$elm$svg$Svg$text(label)
				]));
	});
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Pittan$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'touch-action', 'none')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2($author$project$Pittan$radioButton1, 0, 'ハート'),
						A2($author$project$Pittan$radioButton1, 1, 'いろは'),
						A2($author$project$Pittan$radioButton1, 2, 'A')
					])),
				((model.gameId >= 0) && (model.gameId < 3)) ? A2(
				$elm$svg$Svg$svg,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$width('800'),
						$elm$svg$Svg$Attributes$height('800'),
						$mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onMove(
						function (event) {
							return $author$project$Pittan$PMove(
								{x: event.pointer.offsetPos.a, y: event.pointer.offsetPos.b});
						}),
						$mpizenberg$elm_pointer_events$Html$Events$Extra$Pointer$onUp(
						function (event) {
							return $author$project$Pittan$PUp(
								{x: event.pointer.offsetPos.a, y: event.pointer.offsetPos.b});
						})
					]),
				_Utils_ap(
					_List_fromArray(
						[
							$author$project$Pittan$boardView(model)
						]),
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$clipPath,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$id('candClip')
									]),
								_List_fromArray(
									[
										A2(
										$elm$svg$Svg$rect,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$x('0'),
												$elm$svg$Svg$Attributes$y('0'),
												$elm$svg$Svg$Attributes$width(
												$elm$core$String$fromInt($author$project$Pittan$unit)),
												$elm$svg$Svg$Attributes$height(
												$elm$core$String$fromInt(8 * $author$project$Pittan$unit))
											]),
										_List_Nil),
										A2(
										$elm$svg$Svg$path,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$d('M 0 0 l 100 0 l 0 480 l -100 0 Z')
											]),
										_List_Nil)
									]))
							]),
						_Utils_ap(
							_List_fromArray(
								[
									$author$project$Pittan$candView(model)
								]),
							_Utils_ap(
								A2(
									$elm$core$List$map,
									function (p) {
										return A2($author$project$Pittan$pieceView, p, model);
									},
									model.conf),
								_List_fromArray(
									[
										A2(
										$elm$svg$Svg$text_,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$x('300'),
												$elm$svg$Svg$Attributes$y('200'),
												$elm$svg$Svg$Attributes$fontSize('170')
											]),
										_List_fromArray(
											[
												model.completed ? $elm$html$Html$text('💯') : $elm$html$Html$text('')
											]))
									])))))) : $elm$html$Html$text(''),
				A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$map,
					function (w) {
						return A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(w)
								]));
					},
					model.foundWords))
			]));
};
var $author$project$Pittan$main = $elm$browser$Browser$element(
	{init: $author$project$Pittan$init, subscriptions: $author$project$Pittan$subscriptions, update: $author$project$Pittan$update, view: $author$project$Pittan$view});
_Platform_export({'Pittan':{'init':$author$project$Pittan$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));