# mathematica-prolog-interpreter
A prolog interpreter in mathematica for final project in advanced topics in software engineering (02360651)

## What do I currently have?
1) A working tokenizer. Gets input, and transfers it to known tokens. `cd tokenizer && tokenizer.wl`
2) A working parser. Get the output from the tokenizer, and creates an AST. `cd parser && EBNFParser.wl.wl`
3) A working dictionary creator. Get the output from the parser, and use facts and rules in a dictionary. `cd dictionary && dictionaryCreator.wl`
4) A working renamer. Replaces the names of variables to prevent collision. `cd renamer && renamer.wl`
5) A working unify. Makes substitutions to help reslove conflicts. `cd unify && unify.wl`

## Next to come👀:
1) Continue to solve.
   - Errors needed to be fixed.
     1) cont list [a|Z]. Problem is in unify, it doesn't know how to do that.
     2) case of placeholder _.
     3) case like this: listlist([X,Y], C).
2) Full implementation:
     1) Get input from a in.pl file.
     2) EVAL.
     3) Print results to out.pl file.
