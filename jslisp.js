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

function stringify(x)
{
    return JSON.stringify(x).substr(0); // Opera bug
}

function Symbol(name, interned)
{
    this.name = name;
    this.interned = interned;

    this.toString = function () { return f$$demangle(this.name); };

    // Cells (used only for uninterned symbols)
    this.d = undefined;
    this.f = undefined;
    this.m = undefined;
}

function Namespace()
{
    this.vars = {};
    this.props = {};
    this.stack = [];

    this.add = function(name, value)
    {
        this.stack.push([name, this.vars[name], this.props[name]]);
        this.vars[name] = value;
        this.props[name] = {};
    };

    this.begin = function()
    {
        this.stack.push(false);
    };

    this.end = function()
    {
        if (this.stack.length == 0)
            throw new String("Internal error: Stack underflow in Namespace.end()");
        for (var x=this.stack.pop(); x; x=this.stack.pop())
        {
            this.vars[x[0]] = x[1];
            this.props[x[0]] = x[2];
            if (this.stack.length == 0)
                throw new String("Internal error: Stack underflow in Namespace.end()");
        }
    };

    return this;
}

var lisp_literals = [];

var constants = {'$$null':'null',
                 '$$true':'true',
                 '$$false':'false',
                 '$$undefined':'undefined',
                 '$$NaN':'NaN'};

var lexvar = new Namespace();
var lexfunc = new Namespace();
var lexmacro = new Namespace();
var lexsmacro = new Namespace();

var specials = {};
var jscompile = {};

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
f$$mangle.documentation = ("(mangle x:string) -> string\n" +
                           "Returns the javascript version of a lisp symbol name "+
                           "by quoting characters forbidden in javascript identifiers");

function deflisp(name, doc, f)
{
    window["f" + f$$mangle(name)] = f;
    f.documentation = doc;
}

function defcompile(name, doc, f)
{
    jscompile[f$$mangle(name)] = f;
    f.documentation = doc;
}

deflisp("documentation",
        "(documentation x:function) -> string/null\n" +
        "Returns the documentation string for function 'x' or null if there's no documentation.",
        function(x)
        {
            return x.documentation || null;
        });

deflisp("set-documentation",
        "(set-documentation x:function doc:string)\n" +
        "Sets the documentation string for function 'x' to 'doc'.",
        function(x, doc)
        {
            x.documentation = doc;
        });

deflisp("arglist",
        "(arglist function)\n" +
        "Returns the argument list for function 'x' or null if unknown.",
        function(x)
        {
            return x.arglist || null;
        });

deflisp("set-arglist",
        "(set-arglist function arglist)\n" +
        "Sets the argument list for function 'x' to 'arglist'.",
        function(x, arglist)
        {
            x.arglist = arglist;
        });

deflisp("demangle",
        "(demangle x:string) -> string\n" +
        "Returns a lisp name by decoding a javascript name produced by (mangle ...)",
        function(x)
        {
            return x.substr(2)
                .replace(/_/g,"-")
                .replace(/(\$[0-9]+\$)/g,
                         function(s)
                         {
                             return String.fromCharCode(parseInt(s.substr(1, s.length-2)));
                         });
        });

deflisp("intern",
        "(intern x:string) -> symbol\n" +
        "Create and returns an interned symbol with the specified name or just returns that symbol if " +
        "it has been already interned. If the name starts with a colon ':' character then the symbol value " +
        "cell of this symbol is also bound to the symbol itself.",
        function(name)
        {
            var mname = f$$mangle(name);
            var x = window["s" + mname];
            if (x == undefined)
            {
                x = window["s" + mname] = new Symbol(mname, true);
                if (name[0] == ':') window["d" + mname] = x;
            }
            return x;
        });

deflisp("number?", "(number? x) -> bool\nReturns true if and only if x is a number (including NaN)",
        function(x) { return (typeof x) == "number"; });

deflisp("string?", "(string? x) -> bool\nReturns true if and only if x is a string",
        function(x) { return (typeof x) == "string"; });

deflisp("list?", "(list? x) -> bool\nReturns true if and only if x is a list",
        function(x) { return (x && x.constructor == Array)  ? true : false; });

deflisp("symbol?", "(symbol? x) -> bool\nReturns true if and only if x is a symbol",
        function(x) { return (x && x.constructor == Symbol) ? true : false; });

defcompile("js-code",
           "(js-code x:string-literal)\n" +
           "Verbatim javascript code generation.",
           function(x) { return x[1]; });

deflisp("js-eval",
        "(js-eval x:string) -> result\n" +
        "Javascript evaluation of a string at runtime.",
        function(x) { return eval(x); });

deflisp("symbol-function",
        "(symbol-function x:symbol) -> function\n" +
        "Returns the function cell of a symbol or undefined if that function is not present. " +
        "Lookup doesn't consider lexical function definitions (e.g. (labels ...)).",
        function(x) { return x.interned ? window["f" + x.name] : x.d; });

deflisp("set-symbol-function",
        "(set-symbol-function x:symbol f:function)\n" +
        "Sets the function cell of a symbol to the specified function. It doesn't affect "+
        "lexical function definitions (e.g. (lables ...)).",
        function(x, y) { return x.interned ? (window["f" + x.name] = y) : (x.d = y); });

deflisp("symbol-value",
        "(symbol-value x:symbol) -> value)\n" +
        "Returns the current value cell of a symbol or undefined if that symbol has no value. " +
        "Lookup doesn't consider lexical symbols.",
        function(x) { return x.interned ? window["d" + x.name] : x.d; });

deflisp("set-symbol-value",
        "(set-symbol-value x:symbol y)\n" +
        "Sets the current value cell of a symbol. It doesn't affect lexical bindings.",
        function(x, y) { return x.interned ? (window["d" + x.name] = y) : (x.d = y); });

deflisp("symbol-macro",
        "(symbol-macro x:symbol) -> function\n" +
        "Returns the current macro expander function cell of a symbol or undefined if that " +
        "symbol has no macro expander function set. Lookup doesn't consider lexical macros.",
        function(x) { return x.interned ? window["m" + x.name] : x.m; });

deflisp("set-symbol-macro",
        "(set-symbol-macro x:symbol y:function)\n" +
        "Sets the macro expander function cell of a symbol. It doesn't affect lexical macros.",
        function(x, y) { return x.interned ? (window["m" + x.name] = y) : (x.m = y); });

deflisp("symbol-name",
        "(symbol-name x:symbol) -> string\n" +
        "Returns the lisp symbol name of a symbol as a string object.",
        function(x) { return f$$demangle(x.name); });

defcompile("if",
           "(if condition then-part [else-part])\n" +
           "Conditional evaluation form. Evaluates either then-part only or else-part only depending " +
           "on wether the evaluation of condition returned a true value or not.",
           function(x)
           {
               return ("(" +
                       f$$js_compile(x[1]) +
                       "?" +
                       f$$js_compile(x[2]) +
                       ":" +
                       f$$js_compile(x[3]) + ")");
           });

defcompile("defvar",
           "(defvar variable:symbol value)\n" +
           "Sets the value cell of variable only if is not currenly undefined, and also marks the " +
           "symbol as 'special' so that future value bindings on this symbol will always be dynamic " +
           "and not lexical.",
           function(x)
           {
               var v = x[1].name;
               specials[v] = "d" + v;
               return "(d" + v + " = ((window['d" + v + "']!=undefined)?d" + v + ":" + f$$js_compile(x[2]) + "))";
           });

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

defcompile("progn",
           "(progn form-1 form-2 ... form-n)\n" +
           "Evaluates all the forms in sequence, returning as value the value of the last one.",
           function(x)
           {
               return implprogn(x.slice(1));
           });

defcompile("let",
           "(let ((x1 v1)(x2 v2) ... (xn vn)) f1 f2 ... fn)\n" +
           "Evaluates a sequence of forms f1, f2 ... fn by first establishing lexical/dynamic bindings " +
           "for the variables x1=v1, x2=v2 ... xn=vn. The evaluation of the forms v1 ... vn " +
           "does /NOT/ consider the bindings that will be established by (let ...).",
           function(x)
           {
               lexvar.begin();
               lexsmacro.begin();

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
                       lexvar.add(name, "d" + name);
                       res += "d" + name;
                   }
                   lexsmacro.add(name, undefined);
               }
               res += "){";
               for (var i=0; i<spe.length; i++)
               {
                   res += "var osd" + spe[i] + "=d" + spe[i] + ";";
                   res += "d" + spe[i] + "=sd" + spe[i] + ";";
               }
               res += "var res=";
               res += implprogn(x.slice(2));

               lexsmacro.end();
               lexvar.end();

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
           });

defcompile("lambda",
           "(lambda (arg-1 ... arg-n) form-1 form-2 ... form-n)\n" +
           "Returns a function object that when called will lexically/dynamically bind " +
           "parameters to arg1, arg2, ... arg-n and that will evaluate form-1 form-2 " +
           "form-n in sequence returning the last evaluated form value as result",
           function(x)
           {
               lexvar.begin();
               lexsmacro.begin();
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
                       if (!specials[rest])
                           lexvar.add(rest, "d"+ rest);
                       lexsmacro.add(rest, undefined);
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
                           lexvar.add(v, "d" + v);
                       }
                       nargs++;
                       lexsmacro.add(v, undefined);
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

               lexvar.end();
               lexsmacro.end();

               res += ";"
               for (var i=0; i<spe.length; i++)
               {
                   res += "d" + spe[i] + "=osd" + spe[i] + ";";
               }
               res += "return res;})";
               return res;
           });

deflisp("logcount",
        "(logcount x:number) -> number\n" +
        "Returns the number of bits set to 1 in the binary representation of the integer number x.",
        function(x)
        {
            var n = 0;
            while (x)
            {
                x &= x-1;
                n++;
            }
            return n;
        });

deflisp("list",
        "(list x1 x2 ... xn) -> list\n" +
        "Returns the list containing the value of the expressions x1, x2 ... xn.",
        function()
        {
            var res = [];
            for (var i=0; i<arguments.length; i++)
                res.push(arguments[i]);
            return res;
        });

deflisp("funcall",
        "(funcall f x1 x2 ... xn)\n" +
        "Calls the function object f passing x1, x2, ... xn values as parameters.",
        function()
        {
            var args = [];
            for (var i=1; i<arguments.length; i++)
                args.push(arguments[i]);
            return arguments[0].apply(window, args);
        });

defcompile("labels",
           "(labels ((func1 (x1 x2 ... xn) f1 f2 .. fn)...) b1 b2 ... bn)\n" +
           "Excutes the body forms b1, b2 ... bn by first establishing a lexical binding for the " +
           "function names func1, func2 ... funcn. When compiling the body forms any macros defined outside " +
           "the (labels ...) form with names func1, func2, ... funcn will be ignored.",
           function(x)
           {
               // First hide all macros and lexical macros named as defined functions
               lexfunc.begin();
               lexmacro.begin();
               var hmacros = [];
               for (var i=0; i<x[1].length; i++)
               {
                   var v = x[1][i][0].name;
                   lexmacro.add(v, undefined);
                   hmacros.push([v, window["m" + v]]);
                   window["m" + v] = undefined;
               }

               var res = "((function(){";
               for (var i=0; i<x[1].length; i++)
               {
                   var v = x[1][i][0].name;
                   lexfunc.add(v, "f" + v);
                   res += "var f" + v + "=" +
                       f$$js_compile([f$$intern("lambda")].concat(x[1][i].slice(1))) + ";";
               }
               res += "return ";
               res += implprogn(x.slice(2));
               res += ";})())";

               lexfunc.end();
               lexmacro.end();
               // Restore hidden global macros
               for (var i=0; i<hmacros.length; i++)
                   window["m" + hmacros[i][0]] = hmacros[i][1];
               return res;
           });

defcompile("dotimes",
           "(dotimes (var count) f1 f2 ... fn)\n" +
           "Evaluates the body forms f1 f2 ... fn in sequence exactly 'count' times by setting " +
           "the dynamically/lexically bound variable 'var' to 0, 1, ... count-1 before each iteration. "+
           "The return value is null.",
           function(x)
           {
               lexsmacro.begin();
               var v = x[1][0].name;
               lexsmacro.add(v, undefined);
               if (specials[v])
               {
                   var res = ("(function($$dotimes_count){var osd" + v + "=d" + v + ";for(d" + v +
                              "=0; d" + v + "<$$dotimes_count; ++d" + v + "){");
                   for (var j=2; j<x.length; j++)
                   {
                       res += f$$js_compile(x[j]) + ";";
                   }
                   lexsmacro.end();
                   res += "};d" + v + "=osd" + v + ";return null;})(" + f$$js_compile(x[1][1]) + ")";
               }
               else
               {
                   lexvar.begin();
                   lexvar.add(v, "d" + v);
                   var res = ("(function($$dotimes_count){for(var d" + v +
                              "=0; d" + v + "<$$dotimes_count; ++d" + v + "){");
                   for (var j=2; j<x.length; j++)
                   {
                       res += f$$js_compile(x[j]) + ";";
                   }
                   lexvar.end();
                   lexsmacro.end();
                   res += "}return null;})(" + f$$js_compile(x[1][1]) + ")";
               }
               return res;
           });

defcompile("dolist",
           "(dolist (var x:list) f1 f2 ... fn)\n" +
           "Evaluates the body forms f1 f2 ... fn in sequence times by setting " +
           "the dynamically/lexically bound variable 'var' to next element of list 'x' each time. "+
           "The return value is null.",
           function(x)
           {
               lexsmacro.begin();
               var v = x[1][0].name;
               lexsmacro.add(v, undefined);
               if (specials[v])
               {
                   var res = ("(function($$dolist_L){var osd" + v + "=d" + v + ";for(var $$i=0; $$i < $$dolist_L.length; ++$$i){" +
                              "d" + v + " = $$dolist_L[$$i]; ");
                   for (var j=2; j<x.length; j++)
                   {
                       res += f$$js_compile(x[j]) + ";";
                   }
                   lexsmacro.end();
                   res += "};d" + v + "=osd" + v + ";return null;})(" + f$$js_compile(x[1][1]) + ")";
               }
               else
               {
                   lexvar.begin();
                   lexvar.add(v, "d" + v);
                   var res = ("(function($$dolist_L){for(var $$i=0; $$i < $$dolist_L.length; ++$$i){" +
                              "var d" + v + " = $$dolist_L[$$i]; ");
                   for (var j=2; j<x.length; j++)
                   {
                       res += f$$js_compile(x[j]) + ";";
                   }
                   lexvar.end();
                   lexsmacro.end();
                   res += "}return null;})(" + f$$js_compile(x[1][1]) + ")";
               }
               return res;
           });

defcompile("setq",
           "(setq name value)\n" +
           "Sets the current value of variable 'name'. When 'name' is currently bound to a symbol macro setq "+
           "is transformed in a corresponding (setf ...) form.",
           function(x)
           {
               if (f$$symbol$63$(x[1]))
               {
                   return "(d" + x[1].name + "=" + f$$js_compile(x[2]) + ")";
               }
               else
               {
                   return f$$js_compile([f$$intern("setf"), x[1], x[2]]);
               }
           });

defcompile("quote",
           "(quote x)\n" +
           "Returns the unevaluated x as result.",
           function(x)
           {
               if (f$$symbol$63$(x[1]) && x[1].interned)
                   return "s" + x[1].name;
               if (f$$number$63$(x[1]) || f$$string$63$(x[1]))
                   return stringify(x[1]);
               lisp_literals.push(x[1]);
               return "lisp_literals[" + (lisp_literals.length-1) + "]";
           });

deflisp("eval",
        "(eval x) -> result\n" +
        "Evaluates the expression x without considering lexical bindings.",
        function(x)
        {
            return eval(f$$js_compile(x));
        });

deflisp("macroexpand-1",
        "(macroexpand-1 x) -> result\n" +
        "Expands the macro call or symbol in x or returns x unaltered if " +
        "it's neither a macro invocation nor a macro symbol. Lexical macro bindings are NOT considered.",
        function(x)
        {
            if (f$$symbol$63$(x) && x.symbol_macro)
                x = x.symbol_macro;
            else if (f$$list$63$(x) && f$$symbol$63$(x[0]) && window["m" + x[0].name])
                x = window["m" + x[0].name].apply(window, x.slice(1));
            return x;
        });

deflisp("macroexpand",
        "(macroexpand x) -> result\n" +
        "Repeats macro expansion process of macroexpand-1 on x until no more expansions are possible.",
        function(x)
        {
            for (;;)
            {
                if (f$$symbol$63$(x) && x.symbol_macro)
                    x = x.symbol_macro;
                else if (f$$list$63$(x) && f$$symbol$63$(x[0]) && window["m" + x[0].name])
                    x = window["m" + x[0].name].apply(window, x.slice(1));
                else break;
            }
            return x;
        });

deflisp("append",
        "(append list-1 list-2 ... list-n\n"+
        "Returns a list obtained by concatenating all specified lists.",
        function()
        {
            res = [];
            for (var i=0; i<arguments.length; i++)
                res = res.concat(arguments[i]);
            return res;
        });

deflisp("apply",
        "(apply f args) -> result\n" +
        "Calls the function 'f' passing the list 'args' as arguments",
        function(f, args)
        {
            return f.apply(null, args);
        });

deflisp("lexical-macro",
        "(lexical-macro x) -> result\n" +
        "Returns the lexical macro function associated to symbol x if present or undefined otherwise",
        function(x)
        {
            return lexmacro.vars[x.name];
        });

deflisp("lexical-symbol-macro",
        "(lexical-symbol-macro x:symbol) -> result\n" +
        "Returns the lexical symbol-macro associated to symbol x if present or undefined otherwise",
        function(x)
        {
            return lexsmacro.vars[x.name];
        });

deflisp("lexical-function",
        "(lexical-function x:symbol) -> result\n" +
        "Returns the lexical function associated to symbol x if present or undefined otherwise",
        function(x)
        {
            return lexfunc.vars[x.name];
        });

defcompile("apply",
           "(apply f args)\n",
           "Calls the function 'f' passing the list 'args' as arguments",
           function(x)
           {
               var res = f$$js_compile(x[1]);
               return res + ".apply(null," + f$$js_compile(x[2]) + ")";
           });

defcompile("and",
           "(and x1 x2 ... xn)\n" +
           "Returns the value of last form 'xn' if all forms evaluate to logically true or otherwise " +
           "returns the first logically false result without evaluating subsequent forms.",
           function(x)
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
           });

defcompile("or",
           "(or x1 x2 ... xn)\n" +
           "Returns the value of the first form that evaluates to logically true without evaluating " +
           "subsequent forms, or otherwise returns the value of last form 'xn' if all of them evaluate " +
           "to logicall false.",
           function(x)
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
           });

defcompile("cond",
           "(cond ((t1 f1)(t2 f2)...(tn fn)))\n" +
           "Evaluates in sequence t1, t2 ... tn and returns the value of the first corresponding form " +
           "'f' when the value is logically true without evaluating subsequent conditions. " +
           "Returns null if no condition t evaluates to logically true",
           function(x)
           {
               var res = "(";
               for (var i=1; i<x.length; i++)
               {
                   if (i > 1)
                       res += ":";
                   res += (f$$js_compile(x[i][0]) + "?" +
                           implprogn(x[i].slice(1)));
               }
               return res + ":null)";
           });

defcompile("when",
           "(when condition f1 f2 ... fn)\n" +
           "If 'condition' evaluates to logically true evaluates the body forms f1, f2, ... fn in " +
           "sequence an returns the value of last evaluated form, otherwise returns null without " +
           "evaluating any of the body forms.",
           function(x)
           {
               return ("(" + f$$js_compile(x[1]) + "?" +
                       implprogn(x.slice(2)) + ":null)");
           });

defcompile("unless",
           "(unless condition f1 f2 ... fn)\n" +
           "If 'condition' evaluates to logically false evaluates the body forms f1, f2, ... fn in " +
           "sequence an returns the value of last evaluated form, otherwise returns null without " +
           "evaluating any of the body forms.",
           function(x)
           {
               return ("(" + f$$js_compile(x[1]) + "?null:(" +
                       implprogn(x.slice(2)) + "))");
           });

defcompile("do",
           "(do ((v1 init1 [inc1])...)(exit-test res1 res2 ...) b1 b2 ...)\n" +
           "Loops over the body forms b1 b2 ... bn by first establishing a lexical/dynamic binding " +
           "v1=init1, v2=init2, ... and by assigning the value of the increment forms inc1 to v1, " +
           "inc2 to v2 ... where they are present after each iteration. " +
           "Before entering each loop iteration the exit-test form is evaluated and if logically true the " +
           "iteration is not performed and the result forms res1, res2 ... are evaluated in sequence "+
           "with the value of last of them being used as the final result of the (do ...) form.",
           function(x)
           {
               lexsmacro.begin();
               lexvar.begin();
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
                       lexvar.add(v, "d" + v);
                   }
                   lexsmacro.add(v, undefined);
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
               for (var i=0; i<x[1].length; i++)
               {
                   if (x[1][i].length == 3)
                   {
                       var v = x[1][i][0].name;
                       res += "d" + v + "=(" + f$$js_compile(x[1][i][2]) + ");";
                   }
               }
               res += "}})(";
               lexsmacro.end();
               lexvar.end();
               for (var i=0; i<x[1].length; i++)
               {
                   if (i > 0) res += ",";
                   res += f$$js_compile(x[1][i][1]);
               }
               return res + ")";
           });

defcompile("macrolet",
           "(macrolet ((m1 (x1 x2 ...) b1 b2 ...) ...) body1 body2 ... bodyn)\n" +
           "Evaluates the body forms body1, body2, ... bodyn that are compiled by first installing " +
           "the lexical macros m1, m2 ... mn. Global macros accessible with (symbol-macro x) are not " +
           "affected by these local definitions.",
           function(x)
           {
               lexmacro.begin();
               for (var i=0; i<x[1].length; i++)
               {
                   var name = x[1][i][0].name;
                   var args = x[1][i][1];
                   var body = x[1][i].slice(2);
                   lexmacro.add(name, eval(f$$js_compile([f$$intern("lambda"), args].concat(body))));
               }
               var res = implprogn(x.slice(2));
               lexmacro.end();
               return res;
           });

defcompile("symbol-macrolet",
           "(symbol-macrolet ((x1 def1)(x2 def2)...) body1 body2 ... bodyn)\n" +
           "Evaluates the body forms body1, body2 ... bodyn that are compiled by first installing "+
           "the lexical symbol macros x1 x2 ... xn.",
           function(x)
           {
               lexsmacro.begin();
               for (var i=0; i<x[1].length; i++)
               {
                   var name = x[1][i][0].name;
                   var value = x[1][i][1];
                   lexsmacro.add(name, value);
               }
               var res = implprogn(x.slice(2));
               lexsmacro.end();
               return res;
           });

deflisp("warning",
        "(warning msg)\n" +
        "Function called by the compiler to emit warnings about possible logical errors in the compiled code.",
        function(msg)
        {
            f$$display("WARNING: " + msg.replace(/\$\$[a-zA-Z_0-9\$]*/g, f$$demangle));
        });

var d$$$42$error_location$42$ = null;

function erl(x, f)
{
    try
    {
        return f()
    }
    catch (err)
    {
        if (!err.location)
            err.location = [];
        err.location.push(x);
        throw err;
    }
}

var d$$$42$declarations$42$ = [];

deflisp("js-compile",
        "(js-compile x) -> string\n" +
        "Returns a string containing Javascript code that when evaluated in javascript will perform the " +
        "evaluation of the passed form 'x'.",
        function(x)
        {
            if (f$$symbol$63$(x))
            {
                if (lexsmacro.vars[x.name])
                    return f$$js_compile(lexsmacro.vars[x.name]);

                if (lexvar.vars[x.name])
                    return lexvar.vars[x.name];

                if (x.symbol_macro)
                    return f$$js_compile(x.symbol_macro);

                var v = (specials[x.name] ||
                         constants[x.name]);
                if ((typeof v) == "undefined")
                {
                    if ((typeof window["d" + x.name]) == "undefined")
                        f$$warning("Undefined variable " + x.name);
                    v = "d" + x.name;
                    if (x.constant &&
                        ((typeof window["d" + x.name]) == "string" ||
                         (typeof window["d" + x.name]) == "number"))
                        v = stringify(window["d" + x.name]);
                }
                return v;
            }
            else if (f$$list$63$(x) && f$$symbol$63$(x[0]) && x[0].name == "$$declare")
            {
                d$$$42$declarations$42$.push(x);
            }
            else if (f$$list$63$(x))
            {
                try {
                    var decl = d$$$42$declarations$42$.length;

                    var wrapper = function(r) {
                        return r;
                    };
                    if (x.location)
                    {
                        wrapper = function(r) {
                            return ("erl(" +
                                    stringify(x.location) +
                                    ", function(){return(" +
                                    r + ");})");
                        };
                    }
                    var f = x[0];
                    if (f$$symbol$63$(f))
                    {
                        var wf = jscompile[f.name];
                        if (wf && wf.constructor == Function)
                        {
                            return wrapper(wf(x));
                        }
                        else if (lexmacro.vars[f.name])
                        {
                            var macro_expansion = lexmacro.vars[f.name].apply(window, x.slice(1));
                            return wrapper(f$$js_compile(macro_expansion));
                        }
                        else if (window["m" + f.name])
                        {
                            var macro_expansion = window["m" + f.name].apply(window, x.slice(1));
                            return wrapper(f$$js_compile(macro_expansion));
                        }
                        else
                        {
                            var gf = window["f" + f.name];
                            if (!lexfunc.vars[f.name])
                            {
                                if (!gf)
                                {
                                    f$$warning("Undefined function " + f.name);
                                }
                                else if (gf.arglist)
                                {
                                    var caf = window["f$$static_check_args"];
                                    if (caf && caf!=42)
                                        caf(x, gf.arglist);
                                }
                            }

                            var res = "f" + f.name + "(";
                            for (var i=1; i<x.length; i++)
                            {
                                if (i > 1) res += ",";
                                res += f$$js_compile(x[i]);
                            }
                            res += ")";
                            return wrapper(res);
                        }
                    }
                    else
                    {
                        throw new String("Invalid function call");
                    }
                }
                finally
                {
                    d$$$42$declarations$42$.length = decl;
                }
            }
            else if ((typeof x) == "undefined")
            {
                return "undefined";
            }
            else
            {
                try
                {
                    return stringify(x);
                }
                catch(err)
                {
                    return "<" + x.constructor.name + ">";
                }
            }
        });

///////////////////////////////////////////////////////////////////////////////////////////

var d$$$42$spaces$42$ = " \t\r\n";
var d$$$42$stopchars$42$ = "()\"";

deflisp("skip-spaces",
        "(skip-spaces src)\n" +
        "Keeps consuming characters from the char source 'src' until it's exhausted or " +
        "until the current character is not included in *spaces*.",
        function(src)
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
        });

deflisp("parse-spaced",
        "(parse-spaced src) -> value\n" +
        "Parses a value from a character source 'src' by first skipping any spacing character.",
        function(src)
        {
            f$$skip_spaces(src);
            return f$$f$$parse_value(src);
        });

deflisp("parse-stopping",
        "(parse-stopping x:char) -> bool\n" +
        "True if symbol parsing should stop before character 'x' because x is undefined or " +
        "it's listed in *stopchars*.",
        function f$$parse_stopping(c)
        {
            return (c == undefined ||
                    d$$$42$spaces$42$.indexOf(c) != -1 ||
                    d$$$42$stopchars$42$.indexOf(c) !=-1);
        });

deflisp("parse-number-or-symbol",
        "(parse-number-or-symbol src) -> value\n" +
        "Parses a number or a symbol from character source 'src' depending on if after the number "+
        "the next character is a stop character.",
        function(src)
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
        });

deflisp("parse-delimited-list",
        "(parse-delimited-list src stop) -> list\n" +
        "Parses a list of values from character source 'src' stopping when next character is 'stop' " +
        "also consuming this stopping character.",
        function(src, stop)
        {
            var res = [];
            f$$skip_spaces(src);
            if (src.location)
            {
                // copy source location info if available
                res.location = src.location.slice();
            }
            var oldstops = d$$$42$stopchars$42$;
            d$$$42$stopchars$42$ += stop;
            while (src() != undefined && src() != stop)
            {
                res.push(f$$parse_value(src));
                f$$skip_spaces(src);
            }
            d$$$42$stopchars$42$ = oldstops;
            if (src() != stop)
                throw new String(stringify(stop) + " expected");
            src(1);
            return res;
        });

deflisp("reader-function",
        "(reader-function x:string) -> character-source\n" +
        "Creates a character source function that will produce the content of the specified string.",
        function(s)
        {
            var i = 0;
            return function(n) {
                var c = s[i];
                i+=(n|0);
                return c;
            };
        });

var readers = { "|": function(src)
                {
                    src(1);
                    var res = "";
                    while (src() != undefined && src() != '|')
                    {
                        if (src() == '\\') src(1);
                        res += src(1);
                    }
                    if (src(1) != '|') throw new String("'|' expected");
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
                                if (hx1 < 0 || hx2 < 0) throw new String("Invalid hex char escape");
                                res += String.fromCharCode(hx1*16 + hx2);
                            }
                            else if (c == "u")
                            {
                                hx = 0;
                                for (var i=0; i<4; i++)
                                {
                                    var d = "0123456789ABCDEF".indexOf(src(1).toUpperCase());
                                    if (d < 0) throw new String("Invalid unicode char escape");
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
                    if (src(1) != '"') throw new String("'\"' expected");
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
                    else if (src() == ".")
                    {
                        src(1);
                        return f$$eval(f$$parse_value(src));
                    }
                    throw new String("Unsupported '#' combination");
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
                        throw new String("Value expected");
                    return f$$intern(res);
                }
              };

deflisp("parse-value",
        "(parse-value src) -> value\n" +
        "Parses a value from the given character source or string.",
        function(src)
        {
            if (src.constructor == String)
                src = f$$reader_function(src);
            f$$skip_spaces(src);
            if (src() == undefined)
                throw new String("Value expected");
            return (readers[src()] || readers["default"])(src);
        });

deflisp("str-value",
        "(str-value x) -> string\n" +
        "Computes a string representation of the value x",
        function(x, circle_print)
        {
            if (circle_print == undefined)
                circle_print = [];
            if (f$$symbol$63$(x))
            {
                return f$$demangle(x.name);
            }
            else if (f$$list$63$(x))
            {
                if (x.length == 2 && f$$symbol$63$(x[0]))
                {
                    if (x[0].name == "$$quote")
                        return "'" + f$$str_value(x[1], circle_print);
                    if (x[0].name == "$$function" &&
                        f$$symbol$63$(x[1]))
                        return "#'" + f$$demangle(x[1].name);
                }
                if (circle_print.indexOf(x) != -1)
                    return "#" + circle_print.indexOf(x);
                circle_print.push(x);
                var res = "(";
                for (var i=0; i<x.length; i++)
                {
                    if (i > 0) res += " ";
                    res += f$$str_value(x[i], circle_print);
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
                    return stringify(x);
                }
                catch(err)
                {
                    return "#<" + x.constructor.name + ">";
                }
            }
        });

deflisp("set-compile-specialization",
        "(set-compile-specialization x:symbol function)\n" +
        "Installs a new compiler specialization for forms starting with the specified symbol.",
        function(name, body)
        {
            jscompile[name.name] = body;
        });

deflisp("compile-specialization",
        "(compile-specialization x:symbol) -> function\n" +
        "Returns the current compile specialization for forms starting with the specified symbol or " +
        "undefined if there's no such a compiler specialization.",
        function(name)
        {
            return jscompile[name.name];
        });

deflisp("reader",
        "(reader x:character) -> function\n" +
        "Returns current reading function associated to the specified character or undefined if there's " +
        "no reading function associated with it.",
        function(ch)
        {
            return readers[ch];
        });

deflisp("set-reader",
        "(set-reader x:character f:function)\n" +
        "Sets a new reading function associated to the specified character that will be called by (parse-value src) " +
        "when character x is met as current character in 'src'. The function will be called  passing the character " +
        "source src as argument.",
        function(ch, f)
        {
            readers[ch] = f;
            return f;
        });

deflisp("load",
        "(load src &optional name)\n" +
        "Parses, compiles and evaluates all forms in the character source or string 'src' " +
        "one at a time in sequence. If name is passed and src is a string then source location information is attached to each parsed list.",
        function f$$load(src, name)
        {
            if (f$$string$63$(src))
            {
                var i = 0;
                var src_org = src;
                src = function(d) {
                    d |= 0;
                    if (name && d && src_org[i] == "\n")
                    {
                        src.location[1]++;
                        src.location[2] = 0;
                    }
                    i += d;
                    if (name)
                        src.location[2] += d;
                    return src_org[i-d];
                };
                if (name)
                    src.location = [name, 1, 1];
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
                    var js = f$$js_compile(form);
                    phase = "executing";
                    eval(js);
                    f$$skip_spaces(src);
                }
            }
            catch(err)
            {
                var werr = new String("Error during load (form=" + nforms + ", phase = " + phase + "):\n" +
                                      err + "\n" +
                                      ((phase == "executing" || phase == "compiling") ?
                                       f$$macroexpand_$49$(f$$str_value(form)) : ""));
                werr.location = err.location;
                throw werr;
            }
        });

deflisp("http",
        "(http verb url data [success-function [failure-function]]) -> result\n" +
        "Executes the specified http request (\"GET\" or \"POST\") for the specified url " +
        "either asynchronously (if success-function is specified) or synchronously if no " +
        "callback is specified. The success function if specified will be passed " +
        "the content, the url and the request object. The failure function if specified " +
        "will be passed the url and the request status code in case of an error.",
        function(verb, url, data, onSuccess, onFail)
        {
            var req = new XMLHttpRequest();
            if (onSuccess)
            {
                req.onreadystatechange = function()
                {
                    if (req.readyState == 4) {
                        if (req.status == 200) {
                            onSuccess(req.responseText, url, req);
                        }
                        else
                        {
                            if (onFail)
                                onFail(url, req.status);
                            else
                                throw new String("Ajax request error (url=" +
                                                 url +
                                                 ", status=" +
                                                 req.status + ")");
                        }
                    }
                }
            }
            req.open(verb, url, !!onSuccess);
            req.send(data);
            return onSuccess ? req : req.responseText;
        });

deflisp("http-get",
        "(http-get url [success-function [failure-function]]) -> result\n" +
        "Acquires the specified resource Executes " +
        "either asynchronously (if success-function is specified) or synchronously if no " +
        "callback is specified. The success function if specified will be passed " +
        "the content, the url and the request object. The failure function if specified " +
        "will be passed the url and the request status code in case of an error.",
        function(url, onSuccess, onFailure)
        {
            return f$$http("GET", url, null, onSuccess, onFailure);
        });