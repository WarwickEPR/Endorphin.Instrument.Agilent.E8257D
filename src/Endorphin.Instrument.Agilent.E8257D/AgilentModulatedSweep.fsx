// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#I "../../packages/"

#r "Endorphin.Core/lib/net452/Endorphin.Core.dll"
#r "Endorphin.Core.NationalInstruments/lib/net452/Endorphin.Core.NationalInstruments.dll"
#r "log4net/lib/net45-full/log4net.dll"
#r "bin/Debug/Endorphin.Instrument.Agilent.E8257D.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Agilent.E8257D

log4net.Config.BasicConfigurator.Configure ()

let dwell = 0.01<s>
let frequencyA = 1.234e9<Hz>
let frequencyB = 1.345e9<Hz>
let points = 500

let modulationFrequency = 2.0e4<Hz>
let modulationFn = Modulation.Configure.internalSineSourceInHzWithLfOutput modulationFrequency 0.1<V>
let modulationDepth = 60.0<Hz>
let modulation = FrequencyModulation (FM1, { Deviation = Frequency_Hz modulationDepth }, modulationFn)

let sweep = Sweep.Configure.frequencyStepSweepInHz frequencyA frequencyB
            |> Sweep.Configure.withPoints points
            |> Sweep.Configure.withFixedPowerInDbm -3.0<dBm>
            |> Sweep.Configure.withDwellTime (Some (Duration_sec dwell))
            |> Sweep.Configure.withStepTrigger (Some Bus)
            |> Sweep.Configure.withRetrace Off
            |> Sweep.StepSweep

let settings = { Sweep = sweep; Modulation = [ modulation ] }

try
    async {
        // open the keysight box - set the VISA access string you need here and timeout
        let! rfSource = RfSource.openInstrument "TCPIP::192.168.0.3::INSTR" 2000<ms>
        printfn "Opened instrument"

        do! RfSource.applySettings rfSource settings
        printfn "Applied settings"
        for i in 1..20 do
            printfn "Triggering"
            do! Sweep.Runtime.busTrigger rfSource
            do! Async.Sleep 8000

        // tidy up and close
        do! RfSource.closeInstrument rfSource}
    |> Async.RunSynchronously
with
    | :? InstrumentErrorException as exn -> printfn "Failed with instrument errors: %A" exn.Data0