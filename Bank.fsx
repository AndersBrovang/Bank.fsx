/// A customer: (name, checkingAccounts, savingAccounts)
let customer1 = ("Bob", [100.0; 50.0], [200.0; 10.0])

/// Change one account in a list by index.
/// Uses List.mapi and applies changeFunction only to the matching index.
let changeAccount accountNumber changeFunction accounts =
    accounts
    |> List.mapi (fun position balance ->
        if position = accountNumber then changeFunction balance
        else balance)

/// Get an item by index (small replacement for List.item).
let rec listItem index list =
    if index = 0 then
        List.head list
    else
        listItem (index - 1) (List.tail list)

/// Add money to a checking account (amount is float).
let addToChecking accountNumber amount (name, checkings, savings) =
    let newCheckings = changeAccount accountNumber (fun balance -> balance + amount) checkings
    (name, newCheckings, savings)

/// Take money from a checking account if the balance is sufficient; otherwise leave unchanged.
let takeFromChecking accountNumber amount (name, checkings, savings) =
    let newCheckings =
        changeAccount accountNumber (fun balance ->
            if balance >= amount then balance - amount else balance) checkings
    (name, newCheckings, savings)

/// Add money to a saving account.
let addToSaving accountNumber amount (name, checkings, savings) =
    let newSavings = changeAccount accountNumber (fun balance -> balance + amount) savings
    (name, checkings, newSavings)

/// Take money from a saving account if the balance is sufficient; otherwise leave unchanged.
let takeFromSaving accountNumber amount (name, checkings, savings) =
    let newSavings =
        changeAccount accountNumber (fun balance ->
            if balance >= amount then balance - amount else balance) savings
    (name, checkings, newSavings)

/// Apply interest to all saving accounts for all customers.
/// Rate is given as e.g. 0.10 for 10%.
let applyInterestAll rate customers =
    customers
    |> List.map (fun (name, checkings, savings) ->
        let newSavings = List.map (fun balance -> balance + balance * rate) savings
        (name, checkings, newSavings))

/// Move money from a saving account to a checking account if there's sufficient balance.
/// If not enough balance, both accounts remain unchanged.
let moveSavingToChecking savingNumber checkingNumber amount (name, checkings, savings) =
    let savingBalance = listItem savingNumber savings
    if savingBalance < amount then
        (name, checkings, savings)   // not enough money
    else
        let newSavings = changeAccount savingNumber (fun balance -> balance - amount) savings
        let newCheckings = changeAccount checkingNumber (fun balance -> balance + amount) checkings
        (name, newCheckings, newSavings)

/// --------------------
/// Example usage & printing (kept from original)
/// --------------------
let customer1After =
    moveSavingToChecking 0 1 20.0 customer1

printfn "%A" customer1After
printfn "" // blank line

let customersAfterInterest = applyInterestAll 0.10 [customer1]
printfn "Interest example:"
printfn "Before: %A" customer1
printfn "After: %A" customersAfterInterest
