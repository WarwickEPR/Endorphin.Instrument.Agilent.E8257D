// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#I "../../packages/"

#r "Endorphin.Core/lib/net452/Endorphin.Core.dll"
#r "Endorphin.Core.NationalInstruments/lib/net452/Endorphin.Core.NationalInstruments.dll"
#r "log4net/lib/net45-full/log4net.dll"
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
        let! instrument = IO.connect "TCPIP::192.168.0.3::INSTR" 2000<ms>
        printfn "Opened instrument"

        do! Sweep.Apply.stepSweep sweepSettings instrument

        // tidy up and close
        do! IO.disconnect instrument}
    |> Async.RunSynchronously
with
    | :? InstrumentErrorException as exn -> printfn "Failed with instrument errors: %A" exn.Data0