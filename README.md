# haskcalc

calc2 implements a straightforward maths expression parser using the Parsec library. Notably it constructs the full grammar dynamically, based on a list of operators and their precedences, which allows arbitrary operator precedence and sequencing (although deep nesting like this slows the parser down).

UParser implements a simple parser combinator library, like Parsec. The underlying implementation is different (and consequently very slow) - the interface is mostly the same, but parsers never eat tokens on failure.

calc3 reimplements calc2 using UParser. Many of the operators were removed, because UParser gets extremely slow with more nesting, but it would finish (eventually) with the full operator list.

irpnM implements a straightforward interactive RPN (Reverse Polish Notation, i.e., stack-based) calculator.
