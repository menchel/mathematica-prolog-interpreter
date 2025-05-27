(* printer *)
ClearAll[printAST]

(* Define colors for different node types *)
astColor[type_] := Switch[type,
  "Program", Blue,
  "ClauseList", Purple,
  "Clause" | "Fact", Darker[Blue],
  "Query", RGBColor[0.4, 0.2, 1],
  "PredicateList", Darker[Green],
  "Predicate", Green,
  "TermList", Darker[Orange],
  "Term", Orange,
  "List" | "ListCons", RGBColor[0.7, 0.5, 0],
  "TailVar" | "TailBar", Brown,
  "Boolean", Pink,
  "OrList", Magenta,
  "Not", Red,
  _, Black
]

printAST[tree_, indent_: 0] := Module[
  {prefix, nodeType, children, color},
  
  (* Indent with arrows *)
  prefix = StringJoin[Table["------", {indent}]];
  nodeType = tree["type"];
  children = tree["children"];
  color = astColor[nodeType];
  
  Print[Style[prefix <> nodeType, color]];
  
  (* Recursively print children *)
  Do[
    Which[
      AssociationQ[child],
        printAST[child, indent + 1],
      ListQ[child] && AllTrue[child, AssociationQ],
        Do[printAST[subchild, indent + 1], {subchild, child}],
      True,
        Print[prefix <> "------" <> ToString[child]]
    ],
    {child, children}
  ];
];
