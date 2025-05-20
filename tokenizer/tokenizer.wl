(* main tokenizer function *)
tokenCreator[inputCode_String] := Module[{tokens},

  tokens = StringCases[
    inputCode,
    {
     (* go over all cases *)
      s : RegularExpression["\\s+"] :> Nothing,
      s : RegularExpression["[a-z][a-zA-Z0-9_]*"] :> {"Atom", s},
      s : RegularExpression["[A-Z_][a-zA-Z0-9_]*"] :> {"Variable", s},
      s : RegularExpression["\\d+(?:\\.\\d+)?"] :> {"Number", s},
      s : RegularExpression["'(?:[^']|'')*'"] :> {"String", s},

      (* special cases, mostly language built in signs *)
      ":-" :> {"ColonDash", ":-"},
      "." :> {"Dot", "."},
      "," :> {"Comma", ","},
      "(" :> {"LParen", "("},
      ")" :> {"RParen", ")"},
      "[" :> {"LBracket", "["},
      "]" :> {"RBracket", "]"},
      "|" :> {"Bar", "|"}
    }
  ];
  tokens
];

(* input handler *)
tokenizeFileLines[fileName_String] := Module[{lines, tokensPerLine},
  lines = Import[fileName, "Lines"];
  tokensPerLine = tokenCreator /@ lines;
  
  Do[
    Print["Line ", i, ": ", tokensPerLine[[i]]],
    {i, Length[lines]}
  ];
  
  tokensPerLine  (* Return tokens for all lines *)
];
tokenizeFileLines[(* path to file *)];
