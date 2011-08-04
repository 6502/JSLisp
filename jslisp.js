
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
    name = f$$mangle(name);
    var x = window["s" + name];
    if (x == undefined)
    {
        x = window["s" + name] = new Symbol(name);
    }
    return x;
}

function f$$listp(x)
{
    return x && x.constructor == Array;
}

function f$$symbolp(x)
{
    return x && x.constructor == Symbol;
}

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

jscompile["$$if"] = function(x)
{
    return ("(" +
            f$$js_compile(x[1]) +
            "?" +
            f$$js_compile(x[2]) +
            ":" +
            f$$js_compile(x[3]) + ")");
};

jscompile["$$length"] = function(x)
{
    return "(" + f$$js_compile(x[1]) + ").length";
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

function f$$$43$()
{
    var total = arguments[0];
    for (var i=1; i<arguments.length; i++)
        total += arguments[i];
    return total;
}

jscompile["$$$43$"] = function(x)
{
    var res = "(";
    for (var i=1; i<x.length; i++)
    {
        if (i > 1) res += "+";
        res += f$$js_compile(x[i]);
    }
    return res + ")";
};

function f$$_()
{
    if (arguments.length == 1)
    {
        return - arguments[0];
    }
    var total = arguments[0];
    for (var i=1; i<arguments.length; i++)
        total -= arguments[i];
    return total;
};

jscompile["$$_"] = function(x)
{
    var res = "(";
    if (x.length == 2)
    {
        res += "-" + f$$js_compile(x[1]);
    }
    else
    {
        for (var i=1; i<x.length; i++)
        {
            if (i > 1) res += "-";
            res += f$$js_compile(x[i]);
        }
    }
    return res + ")";
};

function f$$ash(x, y)
{
    return (y > 0 ? x << y : x >> -y);
}

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

function f$$slice(x, y, z)
{
    return x.slice(y, z);
}

(function(){
    var binops = ["*", "/",
                  ">", "<", ">=", "<=", ["==", "="], ["!=", "/="],
                  ["|", "logior"],
                  ["&", "logand"],
                  ["^", "logxor"]];
    for (var i=0; i<binops.length; i++)
    {
        var jsname = binops[i];
        var lispname = jsname;
        if (jsname.constructor == Array)
        {
            lispname = jsname[1];
            jsname = jsname[0];
        }

        jscompile[f$$mangle(lispname)] =
            eval("(function(x) { return \"(\" + f$$js_compile(x[1]) + \"" +
                 jsname +
                 "\" + f$$js_compile(x[2]) + \")\"; })");
        window["f" + f$$mangle(lispname)] =
            eval("(function(x, y) { return x " + jsname + " y; })");
    }
})();

jscompile["$$list"] = function(x)
{
    var res = "[";
    for (var i=1; i<x.length; i++)
    {
        if (i > 1) res += ",";
        res += f$$js_compile(x[i]);
    }
    return res + "]";
};

function f$$list()
{
    var res = [];
    for (var i=0; i<arguments.length; i++)
        res.push(arguments[i]);
    return res;
}

jscompile["$$aref"] = function(x)
{
    return f$$js_compile(x[1]) + "[" + f$$js_compile(x[2]) + "]";
};

function f$$aref(x, index)
{
    return x[index];
}

jscompile["$$set_aref"] = function(x)
{
    return "(" + f$$js_compile(x[1]) + "[" + f$$js_compile(x[2]) + "]=" + f$$js_compile(x[3]) + ")";
};

function f$$set_aref(x, index, value)
{
    x[index] = value;
    return value;
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
    var res = "((function(){";
    for (var i=0; i<x[1].length; i++)
    {
        res += "var f" + x[1][i][0].name + "=function(";
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
    if (x.length == 2) return f$$js_compile(x[1]) + ".slice()";
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

jscompile["$$rest"] = function(x)
{
    return f$$js_compile(x[1]) + ".slice(1)";
};

jscompile["$$reverse"] = function(x)
{
    return f$$js_compile(x[1]) + ".slice().reverse()";
};

jscompile["$$nreverse"] = function(x)
{
    return f$$js_compile(x[1]) + ".reverse()";
};

function f$$push(x, L)
{
    L.push(x);
    return x;
}

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

var spaces = " \t\r\n";
var stops = spaces + "()";

function f$$skip_spaces(src)
{
    while(true)
    {
        while (spaces.indexOf(src()) != -1)
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

function parseSpaced(src)
{
    f$$skip_spaces(src);
    return f$$f$$parse_value(src);
}

function parseNumberOrSymbol(src)
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
    if (res != "-" && (src() == undefined || stops.indexOf(src()) != -1))
        return parseFloat(res);
    while (src() != undefined && stops.indexOf(src()) == -1)
        res += src(1);
    return f$$intern(res);
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
                        return [f$$intern("symbol-function"), f$$parse_value(src)];
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
                    var res = [];
                    while (src() != undefined && src() != ")")
                    {
                        res.push(f$$parse_value(src)); f$$skip_spaces(src);
                    }
                    if (src(1) != ")") throw "')' expected";
                    return res;
                },

                "\"": function(src)
                {
                    src(1);
                    var res = "";
                    while (src() != undefined && src() != '"')
                    {
                        if (src() == '\\')
                        {
                            src(1);
                            if (src() == 'n')
                            {
                                res += '\n'; src(1);
                            }
                            else if (src() == 'r')
                            {
                                res += '\r'; src(1);
                            }
                            else if (src() == 't')
                            {
                                res += '\t'; src(1);
                            }
                            else
                            {
                                res += src(1);
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

                "|": function(src)
                {
                    src(1);
                    var res = "";
                    while (src() != undefined && src() != '|')
                    {
                        if (src() == '\\')
                        {
                            src(1);
                            if (src() == 'n')
                            {
                                res += '\n'; src(1);
                            }
                            else if (src() == 'r')
                            {
                                res += '\r'; src(1);
                            }
                            else if (src() == 't')
                            {
                                res += '\t'; src(1);
                            }
                            else
                            {
                                res += src(1);
                            }
                        }
                        else
                        {
                            res += src(1);
                        }
                    }
                    if (src(1) != '|') throw "'|' expected";
                    return f$$intern(res);
                },

                "0": parseNumberOrSymbol,
                "1": parseNumberOrSymbol,
                "2": parseNumberOrSymbol,
                "3": parseNumberOrSymbol,
                "4": parseNumberOrSymbol,
                "5": parseNumberOrSymbol,
                "6": parseNumberOrSymbol,
                "7": parseNumberOrSymbol,
                "8": parseNumberOrSymbol,
                "9": parseNumberOrSymbol,
                "-": parseNumberOrSymbol,

                " ": parseSpaced,
                "\t": parseSpaced,
                "\n": parseSpaced,
                "\r": parseSpaced,

                "default": function(src)
                {
                    var res = "";
                    while (src() != undefined && stops.indexOf(src()) == -1)
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
    {
        var s = src;
        var i = 0;
        src = function(n) { var c = s[i]; i+=(n|0); return c; };
    }
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
function f$$numberp(x) { return x && x.constructor == Number; };
function f$$stringp(x) { return x && x.constructor == String; };

function f$$set_compile_function(name, body)
{
    jscompile[f$$mangle(name)] = body;
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

var boot = ["(set-symbol-macro 'defmacro"+
            "   (lambda (name args &rest body)"+
            "     (list 'set-symbol-macro"+
            "           (list 'quote name)"+
            "           (append (list 'lambda args) body))))",

            "(defmacro defun (name args &rest body)"+
            "  (list 'set-symbol-function"+
            "        (list 'quote name)"+
            "        (append (list 'lambda args) body)))",

            "(defun bqconst (x)"+
            "  (if (listp x)"+
            "      (if (or (= (aref x 0) '\\,)"+
            "              (= (aref x 0) '\\`)"+
            "              (= (aref x 0) '\\,@))"+
            "          false"+
            "          (do ((i 0 (+ i 1)))"+
            "              ((or (> i (length x)) (not (bqconst (aref x i))))"+
            "               (> i (length x)))))"+
            "      true))",

            "(defun bquote (x)"+
            "  (cond"+
            "   ((or (numberp x) (stringp x) (= x null))"+
            "    x)"+
            "   ((bqconst x)"+
            "    (list 'quote x))"+
            "   ((listp x)"+
            "    (cond"+
            "     ((= (aref x 0) '\\`)"+
            "      (list '\\` (bquote (aref x 1))))"+
            "     ((= (aref x 0) '\\,)"+
            "      (aref x 1))"+
            "     ((= (aref x 0) '\\,@)"+
            "      (error \",@ must be used inside lists\"))"+
            "     (true"+
            "      (let ((res (list 'append))"+
            "            (clist (list 'list)))"+
            "        (dolist (el x)"+
            "          (cond"+
            "           ((and el (listp el) (= (aref el 0) '\\,@))"+
            "            (when (> (length clist) 1)"+
            "              (push clist res)"+
            "              (setq clist (list 'list)))"+
            "            (push (aref el 1) res))"+
            "           (true"+
            "            (push (bquote el) clist))))"+
            "        (when (> (length clist) 1)"+
            "          (push clist res))"+
            "        (if (> (length res) 2)"+
            "            res"+
            "            (aref res 1))))))"+
            "   (true (list 'quote x))))",

            "(defmacro |`| (x) (bquote x))",

            "(defmacro defmacro/f (name args &rest body)"+
            "    `(progn"+
            "        (defmacro ,name ,args ,@body)"+
            "        (eval `(defun ,',name ,',args"+
            "                      ,(apply (symbol-macro ',name) ',args)))))",

            "(defmacro/f first   (x) `(aref ,x 0))",
            "(defmacro/f second  (x) `(aref ,x 1))",
            "(defmacro/f third   (x) `(aref ,x 2))",
            "(defmacro/f fourth  (x) `(aref ,x 3))",
            "(defmacro/f fifth   (x) `(aref ,x 4))",
            "(defmacro/f sixth   (x) `(aref ,x 5))",
            "(defmacro/f seventh (x) `(aref ,x 6))",
            "(defmacro/f eighth  (x) `(aref ,x 7))",
            "(defmacro/f nineth  (x) `(aref ,x 8))",
            "(defmacro/f tenth   (x) `(aref ,x 9))",

            "(defmacro let* (bindings &rest body)"+
            "  (if (> (length bindings) 1)"+
            "     `(let (,(aref bindings 0))"+
            "        (let* ,(rest bindings) ,@body))"+
            "     `(let ,bindings ,@body)))",

            "(defmacro setf (place value)"+
            "  (cond"+
            "    ((symbolp place)"+
            "     `(setq ,place ,value))"+
            "    ((listp place)"+
            "     (let* ((f (first place))"+
            "            (sf (intern (+ \"set-\" (symbol-name f)))))"+
            "       (if (or (symbol-function sf) (symbol-macro sf))"+
            "           `(,sf ,@(rest place) ,value)"+
            "           (if (symbol-macro f)"+
            "               `(setf ,(macroexpand-1 place) ,value)"+
            "                (error \"Unsupported setf place\")))))"+
            "    (true (error \"Invalid setf place\"))))",

            "(defmacro incf (place inc)"+
            "  (if (= inc undefined) (setf inc 1))"+
            "  (cond"+
            "    ((symbolp place)"+
            "     `(setq ,place (+ ,place ,inc)))"+
            "    ((listp place)"+
            "     (let* ((f (first place))"+
            "            (sf (intern (+ \"inc-\" (symbol-name f)))))"+
            "       (if (or (symbol-function sf) (symbol-macro sf))"+
            "           `(,sf ,@(rest place) ,inc)"+
            "           (if (symbol-macro f)"+
            "               `(incf ,(macroexpand-1 place) ,inc)"+
            "                (error \"Unsupported decf place\")))))"+
            "    (true (error \"Invalid incf place\"))))",

            "(defmacro decf (place inc)"+
            "  (if (= inc undefined) (setf inc 1))"+
            "  (cond"+
            "    ((symbolp place)"+
            "     `(setq ,place (- ,place ,inc)))"+
            "    ((listp place)"+
            "     (let* ((f (first place))"+
            "            (sf (intern (+ \"dec-\" (symbol-name f)))))"+
            "       (if (or (symbol-function sf) (symbol-macro sf))"+
            "           `(,sf ,@(rest place) ,inc)"+
            "           (if (symbol-macro f)"+
            "               `(decf ,(macroexpand-1 place) ,inc)"+
            "                (error \"Unsupported decf place\")))))"+
            "    (true (error \"Invalid decf place\"))))",

            "(defun 1+ (x) (+ x 1))",
            "(defun 1- (x) (- x 1))",

            "(defmacro inc-aref (array index value)"+
            "  (let ((aa (gensym))"+
            "        (ix (gensym)))"+
            "    `(let ((,aa ,array)"+
            "           (,ix ,index))"+
            "       (setf (aref ,aa ,ix) (+ (aref ,aa ,ix) ,value)))))",

            "(defmacro dec-aref (array index value)"+
            "  (let ((aa (gensym))"+
            "        (ix (gensym)))"+
            "    `(let ((,aa ,array)"+
            "           (,ix ,index))"+
            "       (setf (aref ,aa ,ix) (- (aref ,aa ,ix) ,value)))))",

            "(defvar *gensym-count* 0)",
            "(defun gensym (prefix)"+
            "  (intern (+ \"G:\" (if prefix (+ prefix \"/\") \"\") (incf *gensym-count*))))",

            "(defmacro substr (x start count)"+
            "    `(js-code ,(+ \"(\" (js-compile x) \").substr(\""+
            "                  (js-compile start) \",\""+
            "                  (js-compile count) \")\")))",

            "(defmacro defstruct (name &rest fields)"+
            "    `(progn"+
            "        (defmacro/f ,name ,fields"+
            "            `(list"+
            "                ,'',name"+
            "                ,,@fields))"+
            "        (defun ,(intern (+ (symbol-name name) #\\?)) (self)"+
            "            (if (and (listp self) (= ',name (aref self 0))) true false))"+
            "        (defvar ,(intern (+ \"*\" (symbol-name name) \"-fields*\")) ',fields)"+
            "        ,@(let ((res (list))"+
            "                (index 1))"+
            "            (dolist (f fields)"+
            "                (let ((fn (intern (+ (symbol-name name) \"-\" (symbol-name f)))))"+
            "                    (push `(defmacro/f ,fn (self)"+
            "                            `(aref ,self ,,index)) res)"+
            "                    (incf index)))"+
            "            res)))",

            // JS object access/creation
            "(defmacro . (obj &rest fields)"+
            "    (let ((res (js-compile obj)))"+
            "        (dolist (x fields)"+
            "            (setf res (+ res \".\" (symbol-name x))))"+
            "        `(js-code ,res)))",

            "(defmacro set-. (obj &rest fields)"+
            "    (let ((res (js-compile obj)))"+
            "        (dolist (x (slice fields 0 (1- (length fields))))"+
            "            (setf res (+ res \".\" (symbol-name x))))"+
            "        (setf res (+ res \"=\" (js-compile (aref fields (1- (length fields))))))"+
            "        `(js-code ,res)))",

            // DOM
            "(setf document (js-code \"document\"))",
            "(setf window (js-code \"window\"))",
            "(defun get-element-by-id (id) (funcall (. document getElementById) id))",
            "(defun create-element (id) (funcall (. document createElement) id))",
            "(defun append-child (x child) (funcall (. x appendChild) child))",
            "(defun remove-child (x child) (funcall (. x removeChild) child))",
           ];

(function(){
    for (var i=0; i<boot.length; i++)
    {
        var x = f$$parse_value(boot[i]);
        eval(f$$js_compile(x));
    }
})();
