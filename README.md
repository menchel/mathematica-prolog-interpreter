# mathematica-prolog-interpreter
A prolog interpreter in mathematica for end course project in advanced topics in software engineering (02360651)

## What do I currently have?
1) A working tokenizer. Gets input, and transfers it to known tokens. `cd tokenizer && tokenizer.wl`
2) A working parser. Get the output from the tokenizer, and creates an AST. `cd parser && parser.wl`
3) A visual aid to see the parser output. `cd parser && visualAider.wl`

## nect to come👀:
1) Full eval implementation.
     - get the parsed contect.
     - If fact:
         - search correctness.👍
     - If rule:
          - add to the envieroment. 🏞️
2) Full REPL:🤩
       - READ- get lines from a .pl file.
       - EVAL- use the eval function to process (after tokenizer+parser).
       - PRINT- print to the screen the output.
       - LOOP- continue to read.
