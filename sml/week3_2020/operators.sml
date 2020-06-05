(* 
    Pipe operator. 
    Simply to write code like "data |> fun" instead "fun data".
    Then you can "pipe" results to functions via partial application:
    
    fun square a b = a * b
    fun sum a b = a + b

    10 |> sum 10 |> square 2 // 40      
*)
fun op |> (x, f) = f x

(* apply operator *)
fun op $ (f, x) = f x

(* composition operator *)
fun op >> (f, g) x = g(f(x))

infix |>
infix $
infix >>