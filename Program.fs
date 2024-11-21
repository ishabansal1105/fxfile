let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]
printfn "Original Salaries: %A" salaries

let highIncomeSalaries = salaries |> List.filter (fun salary -> salary > 100000)


let calculateTax salary =
    match salary with
    | s when s <= 49020 -> s * 15 / 100
    | s when s <= 98040 -> s * 205 / 1000
    | s when s <= 151978 -> s * 26 / 100
    | s when s <= 216511 -> s * 29 / 100
    | _ -> salary * 33 / 100

let taxes = salaries |> List.map calculateTax



let updatedLowSalaries = 
    salaries 
    |> List.map (fun salary -> if salary < 49020 then salary + 20000 else salary)


let midRangeSalariesSum = 
    salaries
    |> List.filter (fun salary -> salary >= 50000 && salary <= 100000)
    |> List.fold (+) 0

printfn "High-Income Salaries: %A" highIncomeSalaries
printfn "Original Salaries: %A" salaries
printfn "Taxes for All Salaries: %A" taxes
printfn "Original Salaries: %A" salaries
printfn "Updated Low Salaries: %A" updatedLowSalaries
printfn "Original Salaries: %A" salaries
printfn "Sum of Mid-Range Salaries: %d" midRangeSalariesSum   



//Tail Recursion


let sumMultiplesOf3 n =

    let rec helper current accumulator =
        
        if current <= 0 then accumulator
        else
            
            helper (current - 3) (accumulator + current)
    
    
    helper n 0


let result = sumMultiplesOf3 27
printfn "Sum of multiples of 3 up to 27 is: %d" result