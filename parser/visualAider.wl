(* used some internet resourses (and AI) to generate this code
   since I only used it to get a better view of my parser, and check if it is correct *)

ClearAll[toASTTree, visualizeAST]

(* Rule: head :- body. *)
toASTTree[Rule[head_, body_List]] :=
  Tree["Clause", {
    Tree["Head", {toASTTree[head]}],
    Tree["Body", toASTTree /@ body]
  }];

(* Predicate term: e.g. append([], Ys, Ys) *)
toASTTree[Predicate[name_, args_List]] :=
  Tree["Predicate",
    Prepend[toASTTree /@ args, Tree["Functor", {Tree["Atom: " <> name]}]]
  ];

(* Prolog list: [X|Y] -> PrologList[".", {X, Y}] *)
toASTTree[PrologList[".", {head_, tail_}]] :=
  Tree["List", {
    Tree["Head", {toASTTree[head]}],
    Tree["Tail", {toASTTree[tail]}]
  }];

(* Variable *)
toASTTree[s_String] /; StringMatchQ[s, RegularExpression["[A-Z_].*"]] :=
  Tree["Variable: " <> s];

(* Atom *)
toASTTree[s_String] /; StringMatchQ[s, RegularExpression["[a-z].*"]] :=
  Tree["Atom: " <> s];

(* Strings *)
toASTTree[s_String] /; StringStartsQ[s, "'"] :=
  Tree["String: " <> s];

(* Number *)
toASTTree[n_?NumericQ] :=
  Tree["Number: " <> ToString[n]];

(* Negation term: e.g. +p(X) or +true *)
toASTTree[Negation[term_]] :=
  Tree["Negation", {toASTTree[term]}];

(* PlaceHolder (_) *)
toASTTree[PlaceHolder[]] :=
  Tree["PlaceHolder: _"];

(* Fallback *)
toASTTree[other_] :=
  Tree["Unknown: " <> ToString[other]];

(* Top-level visualizer *)
visualizeAST[parsed_List] := 
  Tree["PrologProgram", toASTTree /@ parsed,
    TreeElementStyleFunction -> (
      Which[
        StringStartsQ[#2, "Clause"]     , Darker@Blue,
        StringStartsQ[#2, "Predicate"]  , Darker@Green,
        StringStartsQ[#2, "Functor"]    , Orange,
        StringStartsQ[#2, "Atom"]       , Black,
        StringStartsQ[#2, "Variable"]   , Purple,
        StringStartsQ[#2, "String"]     , Brown,
        StringStartsQ[#2, "Number"]     , Darker@Red,
        StringStartsQ[#2, "List"]       , DarkCyan,
        StringStartsQ[#2, "Negation"]   , Red,
        StringStartsQ[#2, "PlaceHolder"], DarkGray,
        StringStartsQ[#2, "Head"]       , Gray,
        StringStartsQ[#2, "Body"]       , Gray,
        True                            , Black
      ] &
    )
  ];
