## Translator

Utilized racket for easy parsing of LC syntax and translated over to javascript

Easy to do with two phases 1) parse and 2) translate

Parse takes in an sexpr and creates an ast out of it that we pass into translate that takes said ast and creates a tring of valid js out of it for us to use.

these steps are combined in the function `top-translate`

