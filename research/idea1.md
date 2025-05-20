#Simple code idea: 

(* Term representation *)
VariableQ[x_] := Head[x] === var;
CompoundQ[x_] := Head[x] === compound;
AtomQ[x_] := Head[x] === atom;

(* Unification *)
unify[t1_, t2_, subst_] := 
  With[{result = unify1[t1, t2, subst]}, 
   If[result === $Failed, $Failed, result]];

unify1[t1_, t2_, subst_] := 
  Which[
   t1 === t2, subst,
   VariableQ[t1], bindVar[t1, t2, subst],
   VariableQ[t2], bindVar[t2, t1, subst],
   CompoundQ[t1] && CompoundQ[t2] && Head[t1] === Head[t2] && 
    Length[t1] === Length[t2], 
   Fold[unify1, subst, Transpose[{List @@ t1, List @@ t2}]],
   True, $Failed
  ];

bindVar[var[name_], term_, subst_] := 
  If[MemberQ[Variables[term], var[name]], $Failed,
   subst /. {var[name] -> term}];

(* Database *)
assert[head_ :- body_] := 
  AppendTo[clauses, compound[":-", {head, body}]];
assert[fact_] := AppendTo[clauses, fact];

(* Resolution *)
resolve[goal_, subst_] := 
  Module[{unified, newGoals, newSubst},
   SelectFirst[
    Map[
     Function[clause,
      newSubst = unify[goal, 
        If[Head[clause] === compound && clause[[0]] === ":-", 
         clause[[1]], clause], subst];
      If[newSubst =!= $Failed,
       newGoals = 
        If[Head[clause] === compound && clause[[0]] === ":-", 
         clause[[2]], True];
       {newSubst, newGoals},
       $Failed]
      ],
     clauses
     ],
    # =!= $Failed &]
   ];

(* Top-level prove function *)
prove[goals_, subst_] := 
  Module[{result = resolve[First[goals], subst]},
   If[result === $Failed, $Failed,
    With[{newSubst = First[result], 
      remainingGoals = If[Length[goals] > 1, Rest[goals], {}], 
      newGoals = If[ListQ[Last[result]], Last[result], {Last[result]}]},
     If[Length[Join[newGoals, remainingGoals]] === 0, newSubst,
      prove[Join[newGoals, remainingGoals], newSubst]]
     ]]
   ];

(* Initialize database *)
clauses = {};

## Notes:
1) No handle of nested terms.
2) No total backtracking.
