#r "nuget: FSharpPlus"

open FSharpPlus.Lens // <- bring the lens operators in to scope


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
        // we pass a value and a transformer to f which delegates the running of the transformer to a Functor.  
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

let g = lens


module CreditCard =
    // We also usually just name after the property they point to
    let inline _address f card =
        f card.Address
        <&> fun address -> { card with Address = address }

module User =
    // The <&> is just an infix version of map
    let inline _creditCard f user =
        f user.CreditCard
        <&> fun card -> { user with CreditCard = card }

let setCreditCardPostcode postcode user =
    // We can use the .-> as an infix version of setl
    user
    |> (User._creditCard
        << CreditCard._address
        << Address._postcode)
       .-> postcode

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




let setCreditCardPostcode2 postcode user =
    //User is our top level object
    //user
    // setl (User._creditCard
    //     << CreditCard._address
    //     << Address._postcode)
    //    postcode

    
    let b = setl (
        Address._postcode
        >> CreditCard._address
        >> User._creditCard
       postcode
    b user
let getCreditCardPostcode user =
    // We can use the ^. operator as an infix version of view
    user
    ^. (User._creditCard
        << CreditCard._address
        << Address._postcode)


