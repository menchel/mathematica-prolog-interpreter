# mathematica-prolog-interpreter
A prolog interpreter in mathematica for final project in advanced topics in software engineering (02360651)

## What do I currently have?
1) A working tokenizer. Gets input, and transfers it to known tokens. `cd tokenizer && tokenizer.wl`
2) A working parser. Get the output from the tokenizer, and creates an AST. `cd parser && EBNFParser.wl.wl`
3) A working dictionary creator. Get the output from the parser, and use facts and rules in a dictionary. `cd dictionary && dictionaryCreator.wl`
4) A working renamer. Replaces the names of variables to prevent collision. `cd renamer && renamer.wl`
5) A working unify. Makes substitutions to help reslove conflicts. `cd unify && unify.wl`

## Next to come👀:
1) Create a resolver to solve goals.
2) Full REPL:🤩
       - READ- get lines from a .pl file.
       - EVAL- use the resolver function to process (after tokenizer+parser).
       - PRINT- print to the screen the output.
       - LOOP- continue to read.
