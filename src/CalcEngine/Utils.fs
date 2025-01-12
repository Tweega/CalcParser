namespace CalcEngine

open Tweega.Discovery.Shared.Types
open Tweega.Shared.Types
open Parser.ParserTypes

module Utils = 
    let determinePrecision (a: NumericValue) (b: NumericValue) : Number =
        // Determines the highest precision between two values
        match a, b with
        | NumericValue.Float64 _, _ | _, NumericValue.Float64 _ -> Number.Float64
        | NumericValue.Float32 _, _ | _, NumericValue.Float32 _ -> Number.Float32
        | NumericValue.Int64 _, _ | _, NumericValue.Int64 _ -> Number.Int64
        | NumericValue.Int32 _, _ | _, NumericValue.Int32 _ -> Number.Int32
        | NumericValue.Int16 _, _ | _, NumericValue.Int16 _ -> Number.Int16
        | NumericValue.Int8 _, _ | _, NumericValue.Int8 _ -> Number.Int8

    let castToOriginalPrecision (result: float) (precision: Number) : NumericValue =
        match precision with
        | Number.Float64 -> NumericValue.Float64 result
        | Number.Float32 -> NumericValue.Float32 (float32 result)
        | Number.Int64 -> NumericValue.Int64 (int64 result)
        | Number.Int32 -> NumericValue.Int32 (int32 result)
        | Number.Int16 -> NumericValue.Int16 (int16 result)
        | Number.Int8 -> NumericValue.Int8 (int8 result)

    let toFloat (value: NumericValue) : Result<float, string> =
        try 
            let i64 = 
                match value with
                | NumericValue.Float64 f -> f
                | NumericValue.Float32 f -> float f
                | NumericValue.Int64 i -> float i
                | NumericValue.Int32 i -> float i
                | NumericValue.Int16 i -> float i
                | NumericValue.Int8 i -> float i
            Ok i64
        with 
            | err -> Error err.Message


    let toInt64 (value: NumericValue) : Result<int64, string> =
        try 
            let i64 = 
                match value with
                | NumericValue.Float64 f -> int64 f //this could throw out of bounds error
                | NumericValue.Float32 f -> int64 f
                | NumericValue.Int64 i -> i
                | NumericValue.Int32 i -> int64 i
                | NumericValue.Int16 i -> int64 i
                | NumericValue.Int8 i -> int64 i
            Ok i64
        with 
            | err -> Error err.Message

    let monthToInt (month:string) =
        match month.ToLower() with
        | "jan" -> 1 | "feb" -> 2 | "mar" -> 3 | "apr" -> 4 | "may" -> 5 | "jun" -> 6
        | "jul" -> 7 | "aug" -> 8 | "sep" -> 9 | "oct" -> 10 | "nov" -> 11 | "dec" -> 12
        | _ -> failwith "Invalid month"


    
    
    let interpolateRVs(tsv1: TimeSeriesValue<ResolvedValue>, tsv2: TimeSeriesValue<ResolvedValue>, ts: Timestamp)
            : InterpolationResult<TimeSeriesValue<ResolvedValue> * bool> = 
            let canReleaseHeadBuffer = tsv2.Timestamp.Ticks = ts.Ticks
            match canReleaseHeadBuffer with 
            | true -> InterpolationResult.InterpolatedValue (tsv2, canReleaseHeadBuffer)
            | false ->
                let timeInterpolation = Tweega.Shared.Utils.percentageTimeElapsed(tsv1.Timestamp, tsv2.Timestamp, ts)
                match timeInterpolation with 
                | TimeInterpolation.Between percent ->
                    // only numeric values can be interpolated

                    // i need to pass back an indication of whether the top value in the buffer can be released yet.
                    match tsv1.Value, tsv2.Value with 
                    | ResolvedValue.Numeric nv1, ResolvedValue.Numeric nv2 ->
                    
                        let precision = determinePrecision nv1 nv2  //not making a special case for int64 here as interpolation a bit of guesswork anyway
                        match ((toFloat nv1), (toFloat nv2)) with 
                        | Ok floatA, Ok floatB  ->
                            let interpolatedValue = (floatA + (percent * floatB))
                            let nv = castToOriginalPrecision interpolatedValue precision
                            // let canReleaseHeadBuffer = 
                            
                            InterpolationResult.InterpolatedValue ({Timestamp = ts; Value = (ResolvedValue.Numeric nv)}, false)
                        | _ -> 
                            let msg = sprintf "Unable to cast numeric values to float: %A %A" nv1 nv2
                            InterpolationResult.Error msg
                    | _ -> (InterpolationResult.Error "Can only interpolate numeric values")
                
                | TimeInterpolation.AfterEnd ->
                    let msg = "Timestamp after end of end of time range.  This means either that the  orchestrator is not doing its job of ensuring that all timestamps are dealt with in sequence OR buffers are not being maintained properly"
                    (InterpolationResult.Error msg)
                | TimeInterpolation.BeforeStart ->
                    InterpolationResult.Wait
                | TimeInterpolation.DuplicateTimestamps ->
                    let msg = "Zero duration between timestamps. Unusual, but can happen if the data source publishes two values at the same time"
                    // again probably not an error.  we would need to remove duplicates from the buffer
                    Tweega.Shared.Utils.toConsole msg
                    InterpolationResult.DuplicateTimestamps
            


                