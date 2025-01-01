namespace Tweega.Shared
open Tweega.Shared.Types


type IDispatch =        
        abstract member dispatch startSpeechToText : unit -> IRecord.STTMsg
        abstract member stopSpeechToText : unit -> IRecord.STTMsg
