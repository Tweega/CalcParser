namespace Tweega.Shared
open Tweega.Shared.XFrameworkTypes
open Tweega.Shared.ClientStreamTypes

module StreamUtils =
    // this needs to be updated tbd tk - I doubt that it is meant to process function trees
    // we are grouping by type.provider so that we can batch jobs being done by the same builder
    // tbd tk - this is only temporary debug - function trees seem to have become the new StreamQuery
    type FunctionTreex = string
    let groupByStreamSource(streamRequests: list<FunctionTreex>) =
        //this function maps
            //FROM [{dataSourceA, tagA}; {dataSourceA tagB} {dataSourceB, tagC}] (ProviderName * TagAlias)
            // TO [{dsA, [TagA, tagB]} {dsB, [tagC]}] (ProviderName * list<TagAlias>)

            // but we may also have FunctionName * list<TypedStreamFunction>

            // i think we are supposed to be grouping on function name as dataSource was effectively a function
            // it depends on whether we can have a list of outputs from a set of function applications - it looks as though we can
            // so we group on function name so we would have function name / provider name
            // the ideal of this grouping is so that we can get the provider/ function to process a batch of requests at the same time
            // which makes sense in the case of a provider - which might be PI - in which case we want to be able to pass in all the tags
            // for a function we would have a batch of inputs - which is effectively a select into a model (asset hierarchy)
            // so we produce either {dsA, [TagA, tagB]} or functionName[[input set A; input set B...]]

            // we may actually need to review the process of specifying a query as I think the plan was to have a list of list of inputs
            // rather than a list of queries

            // there is no reason why we cannot specify inputs from different sources.
            // and our select statement into a model may identify nodes that specify different sources.

            // we originally imagined selecting a bunch of tags to have them pooled into a single buffer - but that is not so likely a scenario
            // and if we want that then we have a function that does that.

            // we group on function name which is either provider, or functionName
        // Seq.ofList streamRequests
        //     |> Seq.groupBy (fun (streamFunction) -> 
        //         match streamFunction with
        //         | StreamProvider (p, _t) -> p
        //         | StreamFunction (fn, _inputs) -> fn
        //     )
        //     |> List.ofSeq
        //     |> List.map(fun (streamSource, seqStreamFunction) ->
        //         (streamSource, (List.ofSeq seqStreamFunction))
        //     )

        [("SomeGroup", streamRequests)]