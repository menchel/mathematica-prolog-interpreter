# mathematica-prolog-interpreter
A prolog interpreter in mathematica for final project in advanced topics in software engineering (02360651)🔥

# repository structure 🏗️
- interpreter ⚙️
    1) Holds the interpreter code. ⁉️
    2) Holds the system design. 💻
- parts🎓
    - Holds each interpreter part individualy. ▶️
- system_tests 🧪
    - Holds checked tests for the interpreter. 🙂

## interpreter interface 🗺️
The interpreter pakacge holds 2 functions:

### interpret
- this function is the actual interpreter. Calling her will interpret the code in in.pl, and give the output in out.pl. Any errors will appear in the notebook.
### setDepth
- this function set the depth of the recursion in the interpreter. Needed since:
  1) Infinite loops are possible.
  2) Mathematica has a recursion limit.

## How to run? 🏃
- Use the code from `cd interpreter/code && interpreter.wl` in a wolfram notebook. 💡
- This code is a package ready to use.💻
- Write your code in a file called in.pl in the same folder as the notebook. ✍️
- Create a new file named out.pl in the same folder as your notebook. 🤔
- Run the function interpret[] from the package. 🏃‍♂️
- The results will be in the file out.pl. 😄

## Some tested cases 🧪
### simple_facts_tests :accessibility:
- Test that checks that we take care of facts as needed.
### test_length 🔥
- Test to check recursive rule. Specifically, length of a list.
### test_list 🗒️
- Test to check some basic list operations.
### test_piano 🎹
- Test to check some operations on piano numbers.
### test_start ▶️
- Test to check some complex combinations.
### test_tree 🎄
- Test to check some tree predicates.
### test_cricle ⚫
- test to check behaviour with infinite solutions.
