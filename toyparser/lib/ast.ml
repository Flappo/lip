type ast =
    Const of int
  | Add of ast * ast
  | Min of ast * ast (* sottrazione *)

