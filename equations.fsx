//вариант 8
open System


let PrintTop =
    printfn "------------------------------------"
    printfn "| Dichotomy | Iterations | Newthon |"
    printfn "------------------------------------"


let eps = 0.0001


let abs x =
    if x < 0.0 then 
        (-1.0) * x
    else x


let dichotomy f a b = 
    let rec cycle a b =
        let c = 0.5*(a + b)
        if abs (b - a) < eps then 
            c
        else
            if (f a) * (f c) < 0.0 then
                cycle a c
            else
                cycle c b
    cycle a b
            

let iterations phi x0 = 
    let rec cycle x =
        if (abs((phi x) - x) < eps) then
            phi x
        else
            cycle (phi x)
    cycle x0


let newthon f f' x0 = 
    let phi x = x - (f x) / (f' x)
    iterations phi x0        


let f1 x = 0.6 * 3.0**x - 2.3 * x - 3.0
let f2 x = x**2.0 - Math.Log(1.0 + x) - 3.0 
let f3 x = 2.0 * x * Math.Sin x - Math.Cos x

let f1' x = 0.6 * Math.Log 3.0 * 3.0 ** x - 2.3
let f2' x = (2.0 * x**2.0 + 2.0 * x - 1.0) / (1.0 + x)
let f3' x = 3.0 * Math.Sin x + 2.0*Math.Cos x

let phi1 x = x - (f1 x) / (f1' x)
let phi2 x = x - (f2 x) / (f2' x)
let phi3 x = x - (f3 x) / (f3' x)


let main = 
    PrintTop
    printfn "| %9.5f | %10.5f |%8.5f |" (dichotomy f1 2.0 3.0) (iterations phi1 2.5) (newthon f1 f1' 2.5)
    printfn "------------------------------------"
    printfn "| %9.5f | %10.5f |%8.5f |" (dichotomy f2 2.0 3.0) (iterations phi2 2.5) (newthon f2 f2' 2.5)
    printfn "------------------------------------"
    printfn "| %9.5f | %10.5f |%8.5f |" (dichotomy f3 0.4 1.0) (iterations phi3 0.7) (newthon f3 f3' 0.7)
    printfn "------------------------------------"
