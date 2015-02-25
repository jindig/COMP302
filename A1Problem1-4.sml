Control.Print.printDepth := 100;

datatype Mathexp =
      Num of int
    | Var of string
    | Neg of Mathexp
    | Add of Mathexp * Mathexp
    | Mul of Mathexp * Mathexp

fun simplify (eqn : Mathexp) : Mathexp =
case eqn of
      Neg exp =>        let
                            val e = simplify exp
                        in
                           case e of
                                 Num 0 => Num 0 (* Special case as the negative of zero is zero *)
                               | _ => Neg e (* Negative of anything else is just simplified anything else *)
                        end
    | Add (eq1, eq2) => let
                            val e1 = simplify eq1
                            val e2 = simplify eq2
                        in
                            case (e1, e2) of (* Handle the identity 0 + x = x and vice versa *)
                                  (Num 0, e2) => e2
                                | (e1, Num 0) => e1
                                | (_, _) => Add (e1, e2) (* If no identity, then put the simplified expression back together *)
                        end
    | Mul (eq1, eq2) => let
                            val e1 = simplify eq1
                            val e2 = simplify eq2
                        in
                            case (e1, e2) of
                                  (Num 0, e2) => Num 0 (* Handle the identity 0 * x = 0 and vice versa *)
                                | (e1, Num 0) => Num 0
                                | (Num 1, e2) => e2 (* Handle the identity 1 * x = x and vice versa *)
                                | (e1, Num 1) => e1
                                | (_, _) => Mul (e1, e2) (* If no identity, then put the simplified expression back together *)
                        end
    | _ => eqn (* Only other possibilities are Int and Var, which can't be simplified further by definition *)
    
fun diff (eqn : Mathexp, x : string) : Mathexp =
let
    fun diff_helper (eqn : Mathexp, x : string) : Mathexp =
    case eqn of
          Var s => if s = x then Num 1 else Var s (* The derivative of the variable we're taking the derivative of is 1, pass through other vars *)
        | Neg exp => Neg (diff_helper (exp, x)) (* Differentiate the inside then make it negative again *)
        | Mul (eq1, eq2) => let (* Product rule: f'(x)g(x) + f(x)g'(x) *)
                                val left = Mul (diff_helper (eq1, x), eq2)
                                val right = Mul (eq1, diff_helper(eq2, x))
                            in
                                Add (left, right)
                            end
        | Add (eq1, eq2) => Add (diff_helper (eq1, x), diff_helper (eq2, x)) (* Differentiate both sides and add them back together *)
        | Num i => Num 0 (* Derivative of a constant is zero by definition *)
in
    simplify (diff_helper (eqn, x)) (* Simplify things to make it easier for humans to read and test *)
end
