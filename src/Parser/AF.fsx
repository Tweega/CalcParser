#r "nuget: FSharpPlus"

open FSharpPlus.Lens // <- bring the lens operators in to scope
open FSharpPlus.Data

type Postcode = Postcode of string

type Address =
    { HouseNumber: string
      Postcode: Postcode }

type CreditCard =
    { Number: string
      Expiry: string
      Cvv: string
      Address: Address }

type User = { CreditCard: CreditCard }


module Address =
    // Lenses are usually named with a leading underscore
    let inline _postcode f address =
        // we pass a value (partToUpdate) and a transformer to f which delegates the running of the transformer to a Functor.  
            //This Functor may choose to not actually apply the transform, but simply return the value.  
            //The transform can produce anything, but in the context of a nested data structure it makes sense to return a  new parent instance.  


            
            //We can then pass the parent instance to a lens that updates the grandparent  with  that parent?
        // when called with view f is a function taking the 'part and returning an instance of Const<Postcode> which has a Map function (transform) that ignores the setter function and just returns  the 'part
        // when  called with setl f is a function taking a 'part and returning an instance of Identity<Postcode> which has a Map function that produces an Address
        let partToUpdate = address.Postcode
        let setter = fun postcode -> { address with Postcode = postcode }
        f partToUpdate <&> setter

let h = view Address._postcode { HouseNumber ="33"; Postcode = Postcode "SW1" }

let j = setl Address._postcode


module CreditCard =
    // We also usually just name after the property they point to
    let inline _address f card =
        f card.Address  // lifts value into a Functor - we now have (perhaps) an Identity<Address> - which implements Map
        <&> fun address -> { card with Address = address }  // here we call that Map function 

        //if using setl or view  we don't need to worry about the lifting function f - that will be supplied automatically

    // // we could get rid of some of the noise if we can pass our updater to a lifter
    // let updateCardAddress card  =
    //     fun address -> { card with Address = address }

    // let dodah (fba : 'b -> 'a)(fab: 'a -> 'b) f =
    //     fun (b: 'b) ->
    //         let a = fba b





module User =
    // The <&> is just an infix version of map
    let inline _creditCard f user =
        f user.CreditCard
        <&> fun card -> { user with CreditCard = card }

// this takes a user 
let setCreditCardPostcode postcode user =
    // (Postcode -> FSharpPlus.Data.Identity<Postcode>) -> User -> Identity<User>
    // composedLens takes a function that lifts postcode into a Functor
    // setl will supply Identity.Return - or something like that, view lifts to Const
    // that is effectively to say an object that encapsulates the function that updates the container
    // this mapping function will either run or not (in the case of Const the function is effectively ignored - theMap function returnsthe input).
    let composedLens = 
        (User._creditCard
        << CreditCard._address
        << Address._postcode)

    // We can use the .-> as an infix version of setl
    // setl takes the 
    let updatedUser =
        setl composedLens postcode user

    let updatedUser' = user |> (composedLens .->  postcode)
    
    user
    |> (User._creditCard
        << CreditCard._address
        << Address._postcode)
       .-> postcode // .-> is infix setl

    

// a lens gives us a way to update some part of a structure, given an instance of that structure
// it does this by taking a function taking an instance of that structure and 
//      locating the relevant part -- this is referred to as the getter
//      updating it -- this is the setter part of the operation.  
// The setter is a function that 
// returning the updated structure
// the lens needs to ultimately return an updated top-level structure
// for a deeply nested structure, this will mean providing a way to update all the parent objects up to the top
// this can be done 'manually' or more elegantly,  by chaining lenses together 
// 

let g() =
    let n = (box "5")
    match n with 
    | :? int as i -> printfn "Int %d " i
    | _ ->  printfn "None of the above"


