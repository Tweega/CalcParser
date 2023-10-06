#r "nuget: FSharpPlus"

open FSharpPlus.Lens // <- bring the lens operators in to scope
open FSharpPlus
open FSharpPlus.Data


type DataType =
    | Float
    | Integer
    | String
    | Date

type TypedValue = string * DataType

type AttributeNode = 
    {
        Name: string;
        Value: string;
        AttributeMap: Map<string, AttributeNode>
        DataType: DataType;
    }  

type ElementNode = 
    {
        Name: string;
        AttributeMap: Map<string, AttributeNode>
        ElementMap: Map<string, ElementNode>
    }  


let inline attributeValueLens attributeName = 
    let getter (maybeNode: option<ElementNode>) =
        match maybeNode with 
        | Some node -> 

            match Map.tryFind attributeName node.AttributeMap with
            | Some attr -> Some attr.Value
            | None -> None
        | None -> None
    let setter (maybeNode: option<ElementNode>) (newValue: string) = 
        match maybeNode with
        |Some node ->

            match Map.tryFind attributeName node.AttributeMap with
            | Some attr -> Some { node with AttributeMap = Map.add attributeName { attr with Value = newValue } node.AttributeMap }
            | _ -> None
        | None -> None
    lens getter setter


let sampleNode = 
    {
        Name = "SampleElement";
        AttributeMap = Map.ofList [("sampleAttr", { Name = "sampleAttr"; Value = "initialValue"; AttributeMap = Map.empty; DataType = String })];
        ElementMap = Map.empty;
    }

let lensForSampleAttr = attributeValueLens "sampleAttr"

let getValueThroughLens attrName s = view (attributeValueLens attrName) s
let setAttributeThroughLens attrName newValue (s:option<ElementNode>) = setl (attributeValueLens attrName) newValue s
  
let initialValue = getValueThroughLens "sampleAttr" sampleNode  // This should yield Some "initialValue"

let updatedNode = setAttributeThroughLens sampleNode "sampleAttr" (Some "newValue")

let newAttributeValue = getValueThroughLens updatedNode  // This should yield Some "newValue"



let inline elementLens elementName =
    let getter (node: ElementNode) =
        Map.tryFind elementName node.ElementMap
    let setter (node: ElementNode) (newChild: ElementNode option) = 
        match newChild with
        | Some child -> Some { node with ElementMap = Map.add elementName child node.ElementMap }
        | None -> Some node
    lens getter setter


//lenses are inline as they either return a container or a part depending on the Functor ie Const (view) or Identity (setl)
let inline _address f card =
        f card.Address
        <&> fun address -> { card with Address = address }


// the f function creates Functor from part
let inline _element elemName f (maybeElement:option<ElementNode>) =
    match maybeElement with
    | Some containerEl -> 
        match Map.tryFind elemName containerEl.ElementMap with
        | Some childEl -> 
            let functor = f childEl
            functor <&> fun childElem -> 
                let newMap = Map.add elemName elem containerEl.ElementMap
                
                Some {containerEl with ElementMap = newMap}
        | None -> None
    | None -> None

    
    
    
// Example of using these lenses:

let parentSampleNode = 
    {
        Name = "ParentElement";
        AttributeMap = Map.empty;
        ElementMap = Map.ofList [("ChildElement", sampleNode)]; // Using sampleNode from the previous example
    }

// Lens to navigate to "ChildElement"
let getElementThroughLens elementContainer elementName = view (elementLens elementName) elementContainer
let setElementThroughLens elementContainer elementName newElement = setl (elementLens elementName) newElement elementContainer

let jj elementContainer elementName  = 
    let ff = setElementThroughLens elementContainer elementName
    let gg = setAttributeThroughLens "some attr" "Some new value"
    let hh = ff >> gg
    let y = 3

    // let dd = 

let cc = ((setElementThroughLens) >=> setAttributeThroughLens)

let childLens = setl (elementLens "ChildElement")
let childLens2 = elementLens "ChildElement"

// Composing the lenses to access the nested attribute value inside "ChildElement"
let composedLens = childLens >=> lensForSampleAttr

let nestedInitialValue = view composedLens parentSampleNode // This should yield Some "initialValue"

let updatedParentNode = setl composedLens (Some "newNestedValue") parentSampleNode

let newNestedAttributeValue = view composedLens updatedParentNode // This should yield Some "newNestedValue"
