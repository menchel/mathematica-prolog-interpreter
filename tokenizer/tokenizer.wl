(* main tokenizer function *)
tokenCreator[inputCode_String] := Module[{tokens},

  tokens = StringCases[
    inputCode,
    {
     (* go over all cases *)
      s : RegularExpression["true"] :> {"true",s},
      s : RegularExpression["false"] :> {"false",s},
      s : RegularExpression["_"] :> {"placeHolder",s},
      s : RegularExpression["?-"] :> {"query",s},
      s : RegularExpression[";"] :> {"or",s},
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
      "|" :> {"Bar", "|"},
      "+" :> {"Negation","+"}
    }
  ];
  tokens
];
