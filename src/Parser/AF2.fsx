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
  
let initialValue = getValueThroughLens "sampleAttr" (Some sampleNode)  // This should yield Some "initialValue"

let updatedNode = setAttributeThroughLens "sampleAttr" "newValue" (Some sampleNode)

let newAttributeValue = getValueThroughLens "sampleAttr" updatedNode  // This should yield Some "newValue"



let inline elementLens elementName =
    let getter (node: ElementNode) =
        Map.tryFind elementName node.ElementMap
    let setter (node: ElementNode) (newChild: ElementNode option) = 
        match newChild with
        | Some child -> Some { node with ElementMap = Map.add elementName child node.ElementMap }
        | None -> Some node
    lens getter setter


//lenses are inline as they either return a container or a part depending on the Functor ie Const (view) or Identity (setl)
// let inline _address f card =
//         f card.Address
//         <&> fun address -> { card with Address = address }


// the f function creates Functor from part
let inline _element elemName f (maybeElement:option<ElementNode>) =
    match maybeElement with
    | Some containerEl -> 
        match Map.tryFind elemName containerEl.ElementMap with
        | Some childEl -> 
            let functor = f childEl
            functor <&> fun childElem -> 
                let newMap = Map.add elemName childElem containerEl.ElementMap
                
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
    ff >> gg


// I want away to locate a node though the question of whether data binding is a better approach is still mute
// especially client side where there is only the one user

type UpdateFn = INode -> Node




