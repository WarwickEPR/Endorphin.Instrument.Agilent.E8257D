#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../Endorphin.Utilities/bin/Debug/Endorphin.Utilities.dll"
#r "../packages/log4net.2.0.5/lib/net45-full/log4net.dll"
#r "bin/Debug/Endorphin.Instrument.Agilent.E8257D.dll"


open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Agilent.E8257D

log4net.Config.BasicConfigurator.Configure ()


let sweepSettings =
    Sweep.Configure.frequencyStepSweepInHz 1.0e9<Hz> 2.0e9<Hz>
    |> Sweep.Configure.withPoints 100
    |> Sweep.Configure.withFixedPowerInDbm -1.0<dBm>
    |> Sweep.Configure.withDwellTime (Some (Duration_sec 0.01<s>))

try
    async {
        // open the keysight box - set the VISA access string you need here and timeout
        let! rfSource = RfSource.openInstrument "TCPIP::192.168.0.3::INSTR" 2000<ms>
        printfn "Opened instrument"

        do! Sweep.Apply.stepSweep rfSource sweepSettings

        // tidy up and close
        do! RfSource.closeInstrument rfSource}
    |> Async.RunSynchronously
with
    | :? InstrumentErrorException as exn -> printfn "Failed with instrument errors: %A" exn.Data0