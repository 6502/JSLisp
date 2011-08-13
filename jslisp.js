/****************************************************************************\
******************************************************************************
**                                                                          **
**  Copyright (c) 2011 by Andrea Griffini                                   **
**                                                                          **
**  Permission is hereby granted, free of charge, to any person obtaining   **
**  a copy of this software and associated documentation files (the         **
**  "Software"), to deal in the Software without restriction, including     **
**  without limitation the rights to use, copy, modify, merge, publish,     **
**  distribute, sublicense, and/or sell copies of the Software, and to      **
**  permit persons to whom the Software is furnished to do so, subject to   **
**  the following conditions:                                               **
**                                                                          **
**  The above copyright notice and this permission notice shall be          **
**  included in all copies or substantial portions of the Software.         **
**                                                                          **
**  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         **
**  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      **
**  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                   **
**  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE  **
**  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION  **
**  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION   **
**  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.         **
**                                                                          **
******************************************************************************
\****************************************************************************/

/* node.js

var window = global;

exports.eval = function (x)
{
    return eval(f$$js_compile(f$$parse_value(x)));
}

*/

function f$$mangle(x)
{
    var res = "";
    for (var i=0; i<x.length; i++)
    {
        if (x[i] == '-')
        {
            res += '_';
        }
        else if ((x[i] >= 'a' && x[i] <= 'z') ||
                 (x[i] >= 'A' && x[i] <= 'Z'))
        {
            res += x[i];
        }
        else
        {
            res += ("$" + x.charCodeAt(i) + "$");
        }
    }
    return "$$" + res;
}

function f$$demangle(x)
{
    return x.substr(2)
        .replace(/_/g,"-")
        .replace(/(\$[0-9]+\$)/g,
                 function(s)
                 {
                     return String.fromCharCode(parseInt(s.substr(1, s.length-2)));
                 });
}

function Symbol(name)
{
    this.name = name;
}

function f$$intern(name)
{
    var mname = f$$mangle(name);
    var x = window["s" + mname];
    if (x == undefined)
    {
        x = window["s" + mname] = new Symbol(mname);
        if (name[0] == ':') window["d" + mname] = x;
    }
    return x;
}

function f$$numberp(x) { return (typeof x) == "number" || (x && x.constructor == Number) ? true : false; };
function f$$stringp(x) { return ((typeof x) == "string"); }
function f$$listp(x)   { return (x && x.constructor == Array)  ? true : false; }
function f$$symbolp(x) { return (x && x.constructor == Symbol) ? true : false; }

var constants = {'$$null':'null',
                 '$$true':'true',
                 '$$false':'false',
                 '$$undefined':'undefined',
                 '$$NaN':'NaN'};

var specials = {};
var lexmacros = {};
var lexsmacros = {};
var jscompile = {};

jscompile["$$js_code"] = function(x) { return x[1]; };

function f$$js_eval(x) { return eval(x); }

function f$$symbol_function(x) { return window["f" + x.name]; }
function f$$set_symbol_function(x, y) { return window["f" + x.name] = y; }

function f$$symbol_value(x) { return window["d" + x.name]; }
function f$$set_symbol_value(x, y) { return window["d" + x.name] = y; }

function f$$symbol_macro(x) { return window["m" + x.name]; }
function f$$set_symbol_macro(x, y) { return window["m" + x.name] = y; }

function f$$symbol_name(x) { return f$$demangle(x.name); }

jscompile["$$function"] = function(x)
{
    return "f" + x[1].name;
};

jscompile["$$if"] = function(x)
{
    return ("(" +
            f$$js_compile(x[1]) +
            "?" +
            f$$js_compile(x[2]) +
            ":" +
            f$$js_compile(x[3]) + ")");
};

jscompile["$$defvar"] = function(x)
{
    var v = x[1].name;
    specials[v] = true;
    return "(d" + v + " = ((window['d" + v + "']!=undefined)?d" + v + ":" + f$$js_compile(x[2]) + "))";
};

function implprogn(x)
{
    var res = "(";
    if (x.length == 0)
    {
        res += "null";
    }
    else
    {
        for (var i=0; i<x.length; i++)
        {
            if (i > 0) res += ",";
            res += f$$js_compile(x[i]);
        }
    }
    res += ")";
    return res;
}

jscompile["$$progn"] = function(x)
{
    return implprogn(x.slice(1));
};

function shadow_lexsmacro(n,s)
{
    if (lexsmacros[n])
    {
        s.push([n,lexsmacros[n]]);
        lexsmacros[n] = null;
    }
}

function unshadow_lexsmacros(s)
{
    for (var i=0; i<s.length; i++)
        lexsmacros[s[i][0]] = s[i][1];
}

jscompile["$$let"] = function(x)
{
    var lexsm = [];
    var spe = [];
    var res = "((function(";
    for (var i=0; i<x[1].length; i++)
    {
        if (i > 0) res += ",";
        var name = x[1][i][0].name;
        if (specials[name])
        {
            res += "sd" + name;
            spe.push(name);
        }
        else
        {
            res += "d" + name;
        }
        shadow_lexsmacro(name, lexsm);
    }
    res += "){";
    for (var i=0; i<spe.length; i++)
    {
        res += "var osd" + spe[i] + "=d" + spe[i] + ";";
        res += "d" + spe[i] + "=sd" + spe[i] + ";";
    }
    res += "var res=";
    res += implprogn(x.slice(2));
    unshadow_lexsmacros(lexsm);
    res += ";";
    for (var i=0; i<spe.length; i++)
    {
        res += "d" + spe[i] + "=osd" + spe[i] + ";";
    }
    res += "return res;})(";
    for (var i=0; i<x[1].length; i++)
    {
        if (i > 0) res += ",";
        res += f$$js_compile(x[1][i][1]);
    }
    res += "))";
    return res;
};

jscompile["$$lambda"] = function(x)
{
    var lexsm = [];
    var spe = [];
    var res = "(function(";
    var rest = null;
    var nargs = 0;
    for (var i=0; i<x[1].length; i++)
    {
        var v = x[1][i].name;
        if (v == "$$$38$rest" || v == "$$$38$body")
        {
            rest = x[1][i+1].name;
            shadow_lexsmacro(rest, lexsm);
        }
        else if (!rest)
        {
            if (i > 0) res += ",";
            if (specials[v])
            {
                res += "sd" + v;
                spe.push(v);
            }
            else
            {
                res += "d" + v;
            }
            nargs++;
            shadow_lexsmacro(v, lexsm);
        }
    }
    res += "){";
    for (var i=0; i<spe.length; i++)
    {
        res += "var osd" + spe[i] + "=d" + spe[i] + ";";
        res += "d" + spe[i] + "=sd" + spe[i] + ";";
    }
    if (rest)
    {
        if (specials[rest])
        {
            spe.push(rest);
            res += "var osd" + rest + "=d" + rest + ";";
        }
        else
        {
            res += "var ";
        }

        res += ("d" + rest + "=[]; "+
                "for (var $rest=" + nargs + "; $rest<arguments.length; $rest++){d" +
                rest + "[$rest-" + nargs + "] = arguments[$rest];}");
    }
    res += "var res=";
    res += implprogn(x.slice(2));
    unshadow_lexsmacros(lexsm);
    res += ";"
    for (var i=0; i<spe.length; i++)
    {
        res += "d" + spe[i] + "=osd" + spe[i] + ";";
    }
    res += "return res;})";
    return res;
};

function f$$logcount(x)
{
    var n = 0;
    while (x)
    {
        x &= x-1;
        n++;
    }
    return n;
}

function f$$list()
{
    var res = [];
    for (var i=0; i<arguments.length; i++)
        res.push(arguments[i]);
    return res;
}

jscompile["$$funcall"] = function(x)
{
    var res = f$$js_compile(x[1]) + "(";
    for (var i=2; i<x.length; i++)
    {
        if (i > 2) res += ",";
        res += f$$js_compile(x[i]);
    }
    return res + ")";
}

function f$$funcall()
{
    var args = [];
    for (var i=1; i<arguments.length; i++)
        args.push(arguments[i]);
    return arguments[0].apply(window, args);
}

jscompile["$$labels"] = function(x)
{
    // First hide all macros and lexical macros named as defined functions
    var lexm = [];
    var hmacros = [];
    for (var i=0; i<x[1].length; i++)
    {
        var v = x[1][i][0].name;
        lexm.push([v, lexmacros[v]]);
        hmacros.push([v, window["m"+v]]);
        lexmacros[v] = window["m"+v] = undefined;
    }

    var res = "((function(){";
    for (var i=0; i<x[1].length; i++)
    {
        var v = x[1][i][0].name;
        res += "var f" + v + "=function(";
        var args = x[1][i][1];
        for (var j=0; j<args.length; j++)
        {
            if (j > 0) res += ",";
            res += "d" + args[j].name;
        }
        res += "){return ";
        res += implprogn(x[1][i].slice(2));
        res += ";};";
    }
    res += "return ";
    res += implprogn(x.slice(2));
    res += ";})())";

    // Restored hidden macros
    for (var i=0; i<lexm.length; i++)
        lexmacros[lexm[i][0]] = lexm[i][1];
    for (var i=0; i<hmacros.length; i++)
        window["m"+hmacros[i][0]] = hmacros[i][1];
    return res;
}

jscompile["$$dotimes"] = function(x)
{
    var lexsm = [];
    var v = x[1][0].name;
    shadow_lexsmacro(v, lexsm);
    if (specials[v])
    {
        var res = ("(function($$dotimes_count){var osd" + v + "=d" + v + ";for(d" + v +
                   "=0; d" + v + "<$$dotimes_count; ++d" + v + "){");
        for (var j=2; j<x.length; j++)
        {
            res += f$$js_compile(x[j]) + ";";
        }
        unshadow_lexsmacros(lexsm);
        res += "};d" + v + "=osd" + v + ";return null;})(" + f$$js_compile(x[1][1]) + ")";
    }
    else
    {
        var res = ("(function($$dotimes_count){for(var d" + v +
                   "=0; d" + v + "<$$dotimes_count; ++d" + v + "){");
        for (var j=2; j<x.length; j++)
        {
            res += f$$js_compile(x[j]) + ";";
        }
        unshadow_lexsmacros(lexsm);
        res += "}return null;})(" + f$$js_compile(x[1][1]) + ")";
    }
    return res;
}

jscompile["$$dolist"] = function(x)
{
    var lexsm = [];
    var v = x[1][0].name;
    shadow_lexsmacro(v, lexsm);
    if (specials[v])
    {
        var res = ("(function($$dolist_L){var osd" + v + "=d" + v + ";for(var $$i=0; $$i < $$dolist_L.length; ++$$i){" +
                   "d" + v + " = $$dolist_L[$$i]; ");
        for (var j=2; j<x.length; j++)
        {
            res += f$$js_compile(x[j]) + ";";
        }
        unshadow_lexsmacros(lexsm);
        res += "};d" + v + "=osd" + v + ";return null;})(" + f$$js_compile(x[1][1]) + ")";
    }
    else
    {
        var res = ("(function($$dolist_L){for(var $$i=0; $$i < $$dolist_L.length; ++$$i){" +
                   "var d" + v + " = $$dolist_L[$$i]; ");
        for (var j=2; j<x.length; j++)
        {
            res += f$$js_compile(x[j]) + ";";
        }
        unshadow_lexsmacros(lexsm);
        res += "}return null;})(" + f$$js_compile(x[1][1]) + ")";
    }
    return res;
}

jscompile["$$setq"] = function(x)
{
    if (f$$symbolp(x[1]))
    {
        return "(d" + x[1].name + "=" + f$$js_compile(x[2]) + ")";
    }
    else
    {
        return f$$js_compile([f$$intern("setf"), x[1], x[2]]);
    }
}

var lisp_literals = [];

jscompile["$$quote"] = function(x)
{
    if (f$$symbolp(x[1]))
        return "s" + x[1].name;
    if (f$$numberp(x[1]) || f$$stringp(x[1]))
        return JSON.stringify(x[1]);
    lisp_literals.push(x[1]);
    return "lisp_literals[" + (lisp_literals.length-1) + "]";
}

function f$$eval(x)
{
    return eval(f$$js_compile(x));
}

function f$$macroexpand_$49$(x)
{
    if (f$$listp(x) && f$$symbolp(x[0]) && window["m" + x[0].name])
        return  window["m" + x[0].name].apply(window, x.slice(1));
    return x;
}

jscompile["$$append"] = function(x)
{
    if (x.length == 1) return "[]";
    if (x.length == 2) return f$$js_compile(x[1]);
    var res = f$$js_compile(x[1]);
    for (var i=2; i<x.length; i++)
        res += ".concat(" + f$$js_compile(x[i]) + ")";
    return res;
}

function f$$apply(f, args)
{
    return f.apply(null, args);
}

jscompile["$$apply"] = function(x)
{
    var res = f$$js_compile(x[1]);
    return res + ".apply(null," + f$$js_compile(x[2]) + ")";
};

jscompile["$$and"] = function(x)
{
    if (x.length == 1)
        return "true";
    var res = "(";
    for (var i=1; i<x.length; i++)
    {
        if (i > 1) res += "&&";
        res += f$$js_compile(x[i]);
    }
    return res + ")";
};

jscompile["$$or"] = function(x)
{
    if (x.length == 1)
        return "false";
    var res = "(";
    for (var i=1; i<x.length; i++)
    {
        if (i > 1) res += "||";
        res += f$$js_compile(x[i]);
    }
    return res + ")";
};

jscompile["$$cond"] = function(x)
{
    var res = "(function(){";
    for (var i=1; i<x.length; i++)
    {
        if (i > 1)
            res += "else ";
        res += ("if (" + f$$js_compile(x[i][0]) +
                ") return " + implprogn(x[i].slice(1)) + ";");
    }
    return res + "})()";
};

jscompile["$$when"] = function(x)
{
    return ("(" + f$$js_compile(x[1]) + "?(" +
            implprogn(x.slice(2)) + "):null)");
};

jscompile["$$unless"] = function(x)
{
    return ("(" + f$$js_compile(x[1]) + "?null:(" +
            implprogn(x.slice(2)) + "))");
};

jscompile["$$do"] = function(x)
{
    var lexsm = [];
    var spe = [];
    var res = "(function(";
    for (var i=0; i<x[1].length; i++)
    {
        var v = x[1][i][0].name;
        if (i > 0) res += ",";
        if (specials[v])
        {
            res += "sd" + v;
            spe.push(v);
        }
        else
        {
            res += "d" + v;
        }
        shadow_lexsmacro(v, lexsm);
    }
    res += "){";
    for (var i=0; i<spe.length; i++)
    {
        res += "var osd" + spe[i] + "=d" + spe[i] + ";";
        res += "d" + spe[i] + "=sd" + spe[i] + ";";
    }
    res += "for(;;){";
    res += "if(" + f$$js_compile(x[2][0]) + "){";
    res += "var res=" + implprogn(x[2].slice(1)) + ";";
    for (var i=0; i<spe.length; i++)
    {
        res += "d" + spe[i] + "=osd" + spe[i] + ";";
    }
    res += "return res;}";
    res += implprogn(x.slice(3)) + ";";
    unshadow_lexsmacros(lexsm);
    for (var i=0; i<x[1].length; i++)
    {
        if (x[1][i].length == 3)
        {
            var v = x[1][i][0].name;
            res += "d" + v + "=(" + f$$js_compile(x[1][i][2]) + ");";
        }
    }
    res += "}})(";
    for (var i=0; i<x[1].length; i++)
    {
        if (i > 0) res += ",";
        res += f$$js_compile(x[1][i][1]);
    }
    return res + ")";
};

jscompile["$$macrolet"] = function(x)
{
    var oldm = [];
    for (var i=0; i<x[1].length; i++)
    {
        var name = x[1][i][0].name;
        var args = x[1][i][1];
        var body = x[1][i].slice(2);
        oldm.push([name, lexmacros[name]]);
        lexmacros[name] = eval(f$$js_compile([f$$intern("lambda"), args].concat(body)));
    }
    var res = implprogn(x.slice(2));
    for (var i=0; i<oldm.length; i++)
        lexmacros[oldm[i][0]] = oldm[i][1];
    return res;
};

jscompile["$$symbol_macrolet"] = function(x)
{
    var oldm = [];
    for (var i=0; i<x[1].length; i++)
    {
        var name = x[1][i][0].name;
        var value = f$$js_compile(x[1][i][1]);
        oldm.push([name, lexsmacros[name]]);
        lexsmacros[name] = value;
    }
    var res = implprogn(x.slice(2));
    for (var i=0; i<oldm.length; i++)
        lexsmacros[oldm[i][0]] = oldm[i][1];
    return res;
};

function f$$js_compile(x)
{
    if (f$$symbolp(x))
    {
        return lexsmacros[x.name] || constants[x.name] || "d" + x.name;
    }
    else if (f$$listp(x))
    {
        var f = x[0];
        if (f$$symbolp(f))
        {
            var wf = jscompile[f.name];
            if (wf && wf.constructor == Function)
            {
                return wf(x);
            }
            else if (lexmacros[f.name])
            {
                var macro_expansion = lexmacros[f.name].apply(window, x.slice(1));
                return f$$js_compile(macro_expansion);
            }
            else if (window["m" + f.name])
            {
                var macro_expansion = window["m" + f.name].apply(window, x.slice(1));
                return f$$js_compile(macro_expansion);
            }
            else
            {
                var res = "f" + f.name + "(";
                for (var i=1; i<x.length; i++)
                {
                    if (i > 1) res += ",";
                    res += f$$js_compile(x[i]);
                }
                res += ")";
                return res;
            }
        }
        else
        {
            throw "Invalid function call";
        }
    }
    else
    {
        try
        {
            return JSON.stringify(x);
        }
        catch(err)
        {
            return "<" + x.constructor.name + ">";
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

var d$$$42$spaces$42$ = " \t\r\n";
var d$$$42$stopchars$42$ = "()";

function f$$skip_spaces(src)
{
    while(true)
    {
        while (d$$$42$spaces$42$.indexOf(src()) != -1)
            src(1);
        if (src() == ';')
        {
            while (src() != undefined && src() != "\n")
                src(1);
        }
        else
        {
            break;
        }
    }
}

function f$$parse_spaced(src)
{
    f$$skip_spaces(src);
    return f$$f$$parse_value(src);
}

function f$$parse_stopping(c)
{
    return (c == undefined ||
            d$$$42$spaces$42$.indexOf(c) != -1 ||
            d$$$42$stopchars$42$.indexOf(c) !=-1);
}

function f$$parse_number_or_symbol(src)
{
    var res = "";
    if (src() == "-")
        res += src(1);
    while (src() >= "0" && src() <= "9")
        res += src(1);
    if (src() == ".")
    {
        res += src(1);
        while (src() >= "0" && src() <= "9")
            res += src(1);
    }
    if (res != "-" && f$$parse_stopping(src()))
        return parseFloat(res);
    while (!f$$parse_stopping(src()))
        res += src(1);
    return f$$intern(res);
}

function f$$parse_delimited_list(src, stop)
{
    var res = [];
    f$$skip_spaces(src);
    var oldstops = d$$$42$stopchars$42$;
    d$$$42$stopchars$42$ += stop;
    while (src() != undefined && src() != stop)
    {
        res.push(f$$parse_value(src));
        f$$skip_spaces(src);
    }
    d$$$42$stopchars$42$ = oldstops;
    if (src() != stop)
        throw JSON.stringify(stop) + " expected";
    src(1);
    return res;
}

function f$$reader_function(s)
{
    var i = 0;
    return function(n) {
        var c = s[i];
        i+=(n|0);
        return c;
    };
}

var readers = { "|": function(src)
                {
                    src(1);
                    var res = "";
                    while (src() != undefined && src() != '|')
                    {
                        if (src() == '\\') src(1);
                        res += src(1);
                    }
                    if (src(1) != '|') throw "'|' expected";
                    return f$$intern(res);
                },

                '"': function(src)
                {
                    src(1);
                    var res = "";
                    while (src() != undefined && src() != '"')
                    {
                        if (src() == '\\')
                        {
                            src(1);
                            var c = src(1);
                            if (c == "n") res += "\n";
                            else if (c == "b") res += "\b";
                            else if (c == "t") res += "\t";
                            else if (c == "n") res += "\n";
                            else if (c == "v") res += "\v";
                            else if (c == "f") res += "\f";
                            else if (c == "r") res += "\r";
                            else if (c == "0") {
                                var oct = 0;
                                while (src() >= "0" && src <= "7")
                                    oct = oct*8 + (src(1).charCodeAt(0) - 48);
                                res += String.fromCharCode(oct);
                            }
                            else if (c == "x") {
                                var hx1 = "0123456789ABCDEF".indexOf(src(1).toUpperCase());
                                var hx2 = "0123456789ABCDEF".indexOf(src(1).toUpperCase());
                                if (hx1 < 0 || hx2 < 0) throw "Invalid hex char escape";
                                res += String.fromCharCode(hx1*16 + hx2);
                            }
                            else if (c == "u")
                            {
                                hx = 0;
                                for (var i=0; i<4; i++)
                                {
                                    var d = "0123456789ABCDEF".indexOf(src(1).toUpperCase());
                                    if (d < 0) throw "Invalid unicode char escape";
                                    hx = hx*16 + d;
                                }
                                res += String.fromCharCode(hx);
                            }
                            else
                            {
                                res += c;
                            }
                        }
                        else
                        {
                            res += src(1);
                        }
                    }
                    if (src(1) != '"') throw "'\"' expected";
                    return res;
                },

                "'": function(src)
                {
                    src(1);
                    return [f$$intern("quote"), f$$parse_value(src)];
                },

                "`": function(src)
                {
                    src(1);
                    return [f$$intern("`"), f$$parse_value(src)];
                },

                ",": function(src)
                {
                    src(1);
                    if (src() == '@')
                    {
                        src(1);
                        return [f$$intern(",@"), f$$parse_value(src)];
                    }
                    else
                    {
                        return [f$$intern(","), f$$parse_value(src)];
                    }
                },

                "#": function(src)
                {
                    src(1);
                    if (src() == "'")
                    {
                        src(1);
                        return [f$$intern("function"), f$$parse_value(src)];
                    }
                    else if (src() == "\\")
                    {
                        src(1);
                        return src(1);
                    }
                    throw "Unsupported '#' combination";
                },

                "(": function(src)
                {
                    src(1);
                    return f$$parse_delimited_list(src, ")");
                },

                "0": f$$parse_number_or_symbol,
                "1": f$$parse_number_or_symbol,
                "2": f$$parse_number_or_symbol,
                "3": f$$parse_number_or_symbol,
                "4": f$$parse_number_or_symbol,
                "5": f$$parse_number_or_symbol,
                "6": f$$parse_number_or_symbol,
                "7": f$$parse_number_or_symbol,
                "8": f$$parse_number_or_symbol,
                "9": f$$parse_number_or_symbol,
                "-": f$$parse_number_or_symbol,

                " ": f$$parse_spaced,
                "\t": f$$parse_spaced,
                "\n": f$$parse_spaced,
                "\r": f$$parse_spaced,

                "default": function(src)
                {
                    var res = "";
                    while (!f$$parse_stopping(src()))
                    {
                        if (src() == "\\") src(1);
                        res += src(1);
                    }
                    if (res == "")
                        throw "Value expected";
                    return f$$intern(res);
                }
              };

function f$$parse_value(src)
{
    if (src.constructor == String)
        src = f$$reader_function(src);
    f$$skip_spaces(src);
    if (src() == undefined)
        throw "Value expected";
    return (readers[src()] || readers["default"])(src);
}

function f$$str_value(x)
{
    if (f$$symbolp(x))
    {
        return f$$demangle(x.name);
    }
    else if (f$$listp(x))
    {
        var res = "(";
        for (var i=0; i<x.length; i++)
        {
            if (i > 0) res += " ";
            res += f$$str_value(x[i]);
        }
        return res + ")";
    }
    else if (x && x.constructor == Function)
    {
        return "#CODE";
    }
    else if ((typeof x) == "undefined")
    {
        return "undefined";
    }
    else if ((typeof x) == "number" && isNaN(x))
    {
        return "NaN";
    }
    else
    {
        try
        {
            return JSON.stringify(x);
        }
        catch(err)
        {
            return "#<" + x.constructor.name + ">";
        }
    }
}

jscompile["$$not"] = function(x)
{
    return "!" + f$$js_compile(x[1]);
}

function f$$error(x) { throw x };

function f$$set_compile_function(name, body)
{
    jscompile[name.name] = body;
}

function f$$compile_function(name)
{
    return jscompile[name.name];
}

function f$$reader(ch)
{
    return readers[ch];
}

function f$$set_reader(ch, f)
{
    readers[ch] = f;
    return f;
}

function f$$load(src)
{
    if (src.constructor.name == "String")
    {
        var i = 0;
        var src_org = src;
        src = function(d) {
            d |= 0;
            i += d;
            return src_org[i-d];
        };
    }
    var nforms = 0;
    try
    {
        f$$skip_spaces(src);
        while (src())
        {
            var phase = "parsing";
            var form = f$$parse_value(src);
            ++nforms;
            phase = "compiling";
            js = f$$js_compile(form);
            phase = "executing";
            eval(js);
            f$$skip_spaces(src);
        }
    }
    catch(err)
    {
        throw "Error during boot (form=" + nforms + ", phase = " + phase + "):\n" + err;
    }
}
