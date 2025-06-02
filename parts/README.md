# interpreter parts
1) A working tokenizer. Gets input, and transfers it to known tokens. `cd tokenizer && tokenizer.wl`
2) A working parser. Get the output from the tokenizer, and creates an AST. `cd parser && EBNFParser.wl.wl`
3) A working dictionary creator. Get the output from the parser, and use facts and rules in a dictionary. `cd dictionary && dictionaryCreator.wl`
4) A working renamer. Replaces the names of variables to prevent collision. `cd renamer && renamer.wl`
5) A working unify. Makes substitutions to help reslove conflicts. `cd unify && unify.wl`
6) A working query reslover. Takes queries and returns whether they are true,false or the variable assignment to make them true. `cd resolver && queryResolver.wl`
