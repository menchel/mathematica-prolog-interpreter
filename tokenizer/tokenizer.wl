tokenCreator[inputCode_String] := Module[{tokens},

  tokens = StringCases[
    inputCode,
    {
      s : RegularExpression["\\s+"] :> Nothing,
      s : RegularExpression["[a-z][a-zA-Z0-9_]*"] :> {"Atom", s},
      s : RegularExpression["[A-Z_][a-zA-Z0-9_]*"] :> {"Variable", s},
      s : RegularExpression["\\d+(?:\\.\\d+)?"] :> {"Number", s},
      s : RegularExpression["'(?:[^']|'')*'"] :> {"String", s},
      
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
