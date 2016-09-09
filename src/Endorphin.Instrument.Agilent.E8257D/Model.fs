// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Agilent.E8257D

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

exception InstrumentErrorException of string seq
exception UnexpectedReplyException of string

/// Model of the possible configurations of an Agilent E8257D or similar RF source.
[<AutoOpen>]
module Model =
    [<AutoOpen>]
    module Quantities =
        /// An absolute amplitude of a signal, given as a float type with units of dBm.
        type Amplitude = Power_dBm of float<dBm> with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = sprintf "%e dBm" (match this with Power_dBm that -> float that)

        /// A frequency for a signal, given as a float type with units of Hz.
        type Frequency = Frequency_Hz of float<Hz> with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = sprintf "%e Hz" (match this with Frequency_Hz that -> float that)
        type Voltage = Voltage_V of float<V> with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = sprintf "%e VP" (match this with Voltage_V that -> float that)

        /// A phase for an I/Q signal, either in radians or degrees.
        type Phase =
            | Phase_rad of float<rad>
            | Phase_deg of float<deg>
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | Phase_rad that -> sprintf "%e RAD" (float that)
                    | Phase_deg that -> sprintf "%e DEG" (float that)

        /// A duration that something lasts for.
        type Duration = Duration_sec of float<s> with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = sprintf "%e s" (match this with Duration_sec that -> float that)
        /// A percentage of a total.
        type Percentage = Percentage of float<pct> with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = sprintf "%e PCT" (match this with Percentage that -> float that)
        /// A relative amplitude, measured in dB rather than dBm.
        type DecibelRatio = DecibelRatio of float<dB> with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = sprintf "%e dB" (match this with DecibelRatio that -> float that)

        /// Impedances that the machine can operate at.
        type Impedance =
            | Impedance_50Ohm
            | Impedance_600Ohm
            | Impedance_1MOhm
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | Impedance_50Ohm  -> "50"
                    | Impedance_600Ohm -> "600"
                    | Impedance_1MOhm  -> "1000000"

    [<AutoOpen>]
    module General =
        /// A direction of something, often a range in a sweep.
        type Direction = Up | Down with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | Up -> "UP"
                    | Down -> "DOWN"

        /// A state of coupling, either to AC or to DC.
        type Coupling = AC | DC with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | AC -> "AC"
                    | DC -> "DC"

        /// A toggle state, where something can either be On or Off.
        type OnOffState = On | Off with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | On -> "ON"
                    | Off -> "OFF"

        /// An automatic or manual state, for different types of control. with
        type AutoManualState = Auto | Manual with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | Auto -> "AUTO"
                    | Manual -> "MANUAL"

        /// Polarity of something, either Positive or Negative.
        type Polarity = Positive | Negative with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | Positive -> "POS"
                    | Negative -> "NEG"

        /// The shape of a function, for use in the function generator.
        type FunctionShape =
            | Sine
            | Triangle
            | Square
            | Ramp of polarity : Polarity
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | Sine     -> "SINE"
                    | Triangle -> "TRI"
                    | Square   -> "SQU"
                    | Ramp _   -> "RAMP"

        /// A state which can either be low or high.
        type LowHighState = Low | High with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | Low -> "LOW"
                    | High -> "HIGH"

    [<AutoOpen>]
    module Triggering =
        /// Where to source an external trigger from.
        type ExternalTriggerSource =
            | Trigger1
            | Trigger2
            | Pulse
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | Trigger1 -> "TRIG1"
                    | Trigger2 -> "TRIG2"
                    | Pulse    -> "PULSE"

        /// Where to source an internal trigger from.
        type InternalTriggerSource =
            | PulseVideo
            | PulseSync
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | PulseVideo -> "PVIDEO"
                    | PulseSync  -> "PSYNC"

        /// What type the trigger source should be.
        type TriggerSourceType =
            | ImmediateType
            | TriggerKeyType
            | BusType
            | ExternalType
            | InternalType
            | TimerType
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | ImmediateType  -> "IMM"
                    | TriggerKeyType -> "KEY"
                    | BusType        -> "BUS"
                    | ExternalType   -> "EXT"
                    | InternalType   -> "INT"
                    | TimerType      -> "TIM"

        /// A complete type of a trigger source.
        type TriggerSource =
            | Immediate
            | TriggerKey
            | Bus
            | External of source : ExternalTriggerSource * polarity : Polarity
            | Internal of source : InternalTriggerSource
            | Timer of period : Duration

        /// The type of trigger, either step or list.
        type TriggerType =
            | StepTrigger
            | ListTrigger

    [<AutoOpen>]
    module Modulation =
        // Unique Sources
        /// Which channel to listen for external input on.
        type ExternalInput =
            | EXT1
            | EXT2
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | EXT1 -> "EXT1"
                    | EXT2 -> "EXT2"

        /// Internal modulation source
        type InternalModulationSource = INT1 | INT2 with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | INT1 -> "INT1"
                    | INT2 -> "INT2"
        /// Which channel to use for the function generator.
        type FunctionGenerator = Function1 | Function2

        /// Which amplitude modulation path to use.
        type AmPath = AM1 | AM2 with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | AM1 -> "AM1"
                    | AM2 -> "AM2"
        /// Which frequency modulation path to use.
        type FmPath = FM1 | FM2 with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | FM1 -> "FM1"
                    | FM2 -> "FM2"

        /// A depth of something, either linear as a percentage, or exponential, as a decibel ratio.
        type Depth =
            | Linear of depth : Percentage
            | Exponential of depth : DecibelRatio

        /// The type of modulation depth - linear or exponential.
        type DepthType = LinearType | ExponentialType with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | LinearType -> "LIN"
                    | ExponentialType -> "EXP"

        type LfOutput = { PeakAmplitude : Voltage }

        /// Create a depth measured linearly as a percentage.
        let depthInPercentage (depth : Percentage) = Linear depth
        /// Create a depth measured exponentially as a decibel ratio.
        let depthInDecibels (depth : DecibelRatio) = Exponential depth

        /// Settings for amplitude modulation.
        type AmSettings = { Depth : Depth }
        /// Settings for frequency modulation.
        type FmSettings = { Deviation : Frequency }

        /// Settings for an external source.
        type ExternalSettings = {
            Coupling : Coupling
            Impedance : Impedance }

        /// Settings for a function generator source.
        type FunctionSettings = {
            Shape : FunctionShape
            Frequency : Frequency
            PhaseOffset : Phase }

        type InternalSourceSettings = {
            Shape : FunctionShape
            Frequency: Frequency
            LfOutput : LfOutput option }

        /// A source of a signal, either external or internal.
        type Source =
            | ExternalSource of port : ExternalInput * settings : ExternalSettings
            | InternalSource of source : InternalModulationSource * settings : InternalSourceSettings
            | InternalFunctionGenerator of generator : FunctionGenerator * settings : FunctionSettings

        /// Modulations have a set path, settings and source which will have its own settings
        type Modulation =
            | AmplitudeModulation of path : AmPath * settings : AmSettings * source : Source
            | FrequencyModulation of path : FmPath * settings : FmSettings * source : Source

        /// A list of modulations to apply as settings.
        type ModulationSettings = Modulation list

        /// Extract just the modulation channel from a Modulation
        type ModulationChannel =
            | AmChannel of path : AmPath
            | FmChannel of path : FmPath
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | AmChannel path -> SCPI.format path
                    | FmChannel path -> SCPI.format path

        /// Get the channel which is being modulated.
        let modulationChannel = function
            | AmplitudeModulation (path, _, _) -> AmChannel path
            | FrequencyModulation (path, _, _) -> FmChannel path

        /// The location of a source.
        type SourceProvider =
            | ExternalPort of port : ExternalInput
            | InternalPort of port : InternalModulationSource
            | InternalGenerator of generator : FunctionGenerator
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | ExternalPort EXT1 -> "EXT1"
                    | ExternalPort EXT2 -> "EXT2"
                    | InternalPort INT1 -> "INT1"
                    | InternalPort INT2 -> "INT2"
                    | InternalGenerator Function1 -> "FUNCTION1"
                    | InternalGenerator Function2 -> "FUNCTION2"

        /// Get the source of a modulation.
        let modulationSource = function
            | AmplitudeModulation (_, _, source)
            | FrequencyModulation (_, _, source) -> source

        /// Find the location which is providing a source.
        let sourceProvider = function
            | ExternalSource (port,_) -> ExternalPort port
            | InternalSource (port,_) -> InternalPort port
            | InternalFunctionGenerator (generator,_) -> InternalGenerator generator

    [<AutoOpen>]
    module Sweep =
        /// The mode to operate the sweep in, either fixed or swept.
        type SweepMode = Fixed | Swept with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                     | Fixed -> "FIX"
                     | Swept -> "LIST"

        /// The type of sweep to use, either list or step.
        type SweepType = List | Step with
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                     | List -> "LIST"
                     | Step -> "STEP"

        /// A range of values, with a begin value and an end value.
        type Range<'T> = { Start : 'T; Stop : 'T }
        /// Create a range between two values.
        let range a b = { Range.Start = a ; Range.Stop = b }

        /// A sweep through some set frequencies, either through a range of frequencies, or just at
        /// a constant frequency.
        type FrequencySweep =
            | FrequencySweep of range : Range<Frequency>
            | FixedFrequency of frequency : Frequency

        /// A sweep through some set amplitudes, either through a range of amplitudes, or just at
        /// a constant amplitude.
        type AmplitudeSweep =
            | AmplitudeSweep of range : Range<Amplitude>
            | FixedAmplitude of amplitude : Amplitude

        /// Settings for a sweep, encapsulating all options understandable by the machine.
        type SweepOptions = {
            Direction : Direction
            StepTrigger : TriggerSource option
            ListTrigger : TriggerSource option
            DwellTime : Duration option
            Retrace : OnOffState
            Continuous : OnOffState
            Mode : AutoManualState }

        /// A completely represented step sweep, including the frequency to sweep, the amplitude to
        /// sweep, how many points to sweep across, the spacings between them, and any associated
        /// options.
        type StepSweep = {
            Frequency : FrequencySweep
            Amplitude : AmplitudeSweep
            Points : int
            Options : SweepOptions }

        /// A sweep to run, either a full sweep, or a simple constant signal at a set frequency and
        /// amplitude.
        type Sweep =
            | NoSweep of frequency : Frequency * amplitude : Amplitude
            | StepSweep of sweep : StepSweep

        /// A sweep file that has been stored in the machine.
        type StoredSweep = StoredSweep of string

    [<AutoOpen>]
    module Route =
        /// Generic interface inherited by all other output signals.
        type ISignal = inherit SCPI.IScpiFormatable

        /// An output signal which is able to be sent through the user output BNCs.
        type IUserSignal = inherit ISignal
        /// An output signal which is able to be sent through the Sweep Out BNC.
        type ISweepOutSignal = inherit ISignal
        /// An output signal which is able to be sent through the Trigger1 or Trigger2 BNCs.
        type ITriggerSignal = inherit ISignal
        /// A signal which is either a marker channel, or no channel.
        type IMarkerSignal = inherit ISignal
        /// A signal input from one of the specifically user-controlled BNCs.
        type IUserBncSignal = inherit ISignal

        /// Type to denote that no output signal is being sent through a connector.
        type NoSignal =
            | NoSignal
            interface IUserSignal
            interface ISweepOutSignal
            interface ITriggerSignal
            interface IMarkerSignal
            interface IUserBncSignal
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = "NONE"

        /// A marker signal output.
        type UserSignalMarker =
            | RouteMarker1
            | RouteMarker2
            | RouteMarker3
            | RouteMarker4
            interface IUserSignal
            interface IMarkerSignal
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | RouteMarker1 -> "M1"
                    | RouteMarker2 -> "M2"
                    | RouteMarker3 -> "M3"
                    | RouteMarker4 -> "M4"

        /// The signal present on the 29th pin of the auxiliary connector.
        type UserSignalAux =
            | RouteAux29
            interface IUserSignal
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | RouteAux29 -> "AUX29"

        /// Output signals automatically generated by the system, all of which may be sent through
        /// the system output BNCs.
        type SystemSignalCommon =
            | RouteSourceSettled
            | RoutePulseVideo
            | RoutePulseSync
            | RouteSweptFunctionDone
            interface ISweepOutSignal
            interface ITriggerSignal
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | RouteSourceSettled -> "SETTLED"
                    | RoutePulseVideo -> "PVIDEO"
                    | RoutePulseSync -> "PSYNC"
                    | RouteSweptFunctionDone -> "SFDONE"

        /// A signal which may only be routed through the Sweep Out BNC.
        type SystemSignalSweepOut =
            | RouteSweepOut
            | RouteSweepRun
            interface ISweepOutSignal
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | RouteSweepOut -> "SWEEP"
                    | RouteSweepRun -> "SRUN"

        /// A signal which may only be routed through the two Trigger BNCs.
        type SystemSignalTrigger =
            | RouteSweepTriggerOut
            | RouteLxi
            | RoutePulseBnc
            | RouteOtherTrigger
            interface ITriggerSignal
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | RouteSweepTriggerOut -> "SWEEP"
                    | RouteLxi -> "LXI"
                    | RoutePulseBnc -> "PULSE"
                    | RouteOtherTrigger -> "TRIG"

        /// A signal coming from one of the user-controlled BNCs.
        type UserBnc =
            | RouteBasebandTrigger1
            | RouteBasebandTrigger2
            | RouteEvent1
            | RoutePatternTrigger
            interface IUserBncSignal
            interface SCPI.IScpiFormatable with
                member this.ToScpiString () = this |> function
                    | RouteBasebandTrigger1 -> "BBTRIGGER1"
                    | RouteBasebandTrigger2 -> "BBTRIGGER2"
                    | RouteEvent1  -> "EVENT1"
                    | RoutePatternTrigger -> "PTRIGGER"

        /// A complete set of output routings to write to the machine.
        type internal OutputRouting = {
            BbTrig1  : IUserSignal
            BbTrig2  : IUserSignal
            Event1   : IUserSignal
            PatTrig  : IUserSignal
            SweepOut : ISweepOutSignal
            Trig1    : ITriggerSignal
            Trig2    : ITriggerSignal }

        /// A complete set of input routings to write to the machine.
        type internal InputRouting = {
            PatTrig1 : IUserBncSignal
            PatTrig2 : IUserBncSignal }

        /// A set of internal routings for marker channels.  The same marker can be routed both
        /// internally and externally simultaneously.
        type internal InternalRouting = {
            AltAmplitude : IMarkerSignal
            AlcHold      : IMarkerSignal
            RfBlank      : IMarkerSignal }

        /// Polarities of the marker channels.
        type internal MarkerPolarities = {
            PolarityM1   : Polarity
            PolarityM2   : Polarity
            PolarityM3   : Polarity
            PolarityM4   : Polarity }

        /// A complete set of routings for the machine.
        type Routing = internal {
            Output           : OutputRouting
            Input            : InputRouting
            Internal         : InternalRouting
            MarkerPolarities : MarkerPolarities }

    /// A complete record of settings for the Agilent box, based on the sweep/modulation model.
    type RfSettings = {
        Sweep : Sweep.Sweep
        Modulation : Modulation list }