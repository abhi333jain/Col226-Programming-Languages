CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "eval3.sml";
use "typechecker.sml";
use "Combined.yacc.sig";
use "Combined.yacc.sml";
use "Combined.lex.sml";
use "load-Combined.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
