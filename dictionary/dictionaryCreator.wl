(* ============ dictionary ============= *)
(*
	The dictionary creator gets the parsed facts and rules from the parser
	, and creates a dictionary of [functor][arity] for future use
*)
(* ==================================== *)

(* building a database for all the facts and rules *)
processClauses[clauses_Association] := Module[
  {facts = clauses["Facts"], rules = clauses["Rules"], dict = <||>},
  
  (* helper inner func *)
  getKey[currClause_] := Module[{head, arguments},
    Which[
      (* rule *)
      Head[currClause] === Rule,
        head = currClause[[1, "head"]];
        arguments = currClause[[1, "arguments"]],
      (* fact *)
      True,
        head = currClause["head"];
        arguments = currClause["arguments"]
    ];
    {head, Length[arguments]}
  ];
  
  (* add fact *)
  Do[
    Module[{key = getKey[fact]},
      If[KeyExistsQ[dict, key],
        AppendTo[dict[key, "facts"], fact],
        dict[key] = <|"facts" -> {fact}, "rules" -> {}|>
      ]
    ],
    {fact, facts}
  ];
  
  (* add rule *)
  Do[
    Module[{key = getKey[rule]},
      If[KeyExistsQ[dict, key],
        AppendTo[dict[key, "rules"], rule],
        dict[key] = <|"facts" -> {}, "rules" -> {rule}|>
      ]
    ],
    {rule, rules}
  ];
  
  dict
];
