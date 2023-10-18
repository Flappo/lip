type ast =
    Const of int
  | Add of ast * ast
  | Sub of ast * ast (* sottrazione *)
  | Mul of ast * ast (* moltiplicazione *)
  | Div of ast * ast (* divisione *)