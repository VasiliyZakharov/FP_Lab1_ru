//вариант 8
let eps = 0.0001
let a = 0.0
let b = 2.0
let n = 10

let PrintTop =
    printfn "------------------------------------------------------------------"
    printfn "|  x  |  Builtin  | Smart Taylor | terms1 | Dumb Taylor | terms2 |"
    printfn "------------------------------------------------------------------"


let PrintTabl x B (S, t1) (D, t2) =
    printfn "| %.1f | %.5f  |   %.5f   | %6d |  %.5f   | %6d |" x B S t1 D t2
    printfn "------------------------------------------------------------------"


let f (x: float) =
    (1.0/(2.0 * x - 5.0))


let abs x =
    if x < 0.0 then 
        (-1.0) * x
    else x


let SmartTaylor eps x =
    let form (prev: float) (x: float) =
        prev * (2.0 * x / 5.0)

    let rec check (acc: float) (m: float) (n: int) =
        if (abs m) < eps then (acc, n - 1)
        else
            let next = form m x
            check (acc + m) next (n + 1)

    check 0.0 -0.2 1


let DumbTaylor (eps: float) (x: float) =
    let form (n: int) (x: float) =
        -((2.0**(float n - 1.0))*(x**(float n-1.0)))/(5.0**(float n))

    let rec check (acc: float) (n: int) =
        let k = form n x
        if (abs k) < eps then
            acc + k, n
        else 
            check (acc + k) (n + 1)

    check 0.0 1




let main = 
    PrintTop
    for i = 0 to n do
        let x = a+(float i)/(float n)*(b-a)
        PrintTabl x (f x) (SmartTaylor eps x) (DumbTaylor eps x)

        
main
