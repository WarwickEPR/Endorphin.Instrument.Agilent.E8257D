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
    module Instrument =
        /// An opened and connected RF source, which can have commands written to it.
        type RfSource = internal RfSource of Visa.Instrument

        /// A record of the identification information a device provides.
        type DeviceId = {
            Manufacturer : string
            ModelNumber : string
            SerialNumber : string
            Version : string }

        /// Model numbers that are recognised by the program.
        type ModelNumber = E8257D

        /// A returned error, including its code and the associated message.
        type Error = { Code : int ; Message : string }
   
    [<AutoOpen>]
    module Quantities =
        /// An absolute amplitude of a signal, given as a float type with units of dBm.
        type Amplitude = Power_dBm of float<dBm>
        /// A frequency for a signal, given as a float type with units of Hz.
        type Frequency = Frequency_Hz of float<Hz>

        /// A phase for an I/Q signal, either in radians or degrees.
        type Phase =
            | Phase_rad of float<rad>
            | Phase_deg of float<deg>
        /// A list of phases to cycle through.
        type PhaseCycle = internal PhaseCycle of Phase array

        /// A duration that something lasts for.
        type Duration = Duration_sec of float<s>
        /// A percentage of a total.
        type Percentage = Percentage of float<pct>
        /// A relative amplitude, measured in dB rather than dBm.
        type DecibelRatio = DecibelRatio of float<dB>
        
        /// Impedances that the machine can operate at.
        type Impedance =
            | Impedance_50Ohm
            | Impedance_600Ohm
            | Impedance_1MOhm

    [<AutoOpen>]
    module General = 
        /// A direction of something, often a range in a sweep.
        type Direction = Up | Down
        /// A state of coupling, either to AC or to DC.
        type Coupling = AC | DC
        /// A toggle state, where something can either be On or Off.
        type OnOffState = On | Off
        /// An automatic or manual state, for different types of control.
        type AutoManualState = Auto | Manual
        /// Polarity of something, either Positive or Negative.
        type Polarity = Positive | Negative
        /// The shape of a function, for use in the function generator.
        type FunctionShape =
            | Sine
            | Triangle
            | Square
            | Ramp of polarity : Polarity
        /// A state which can either be low or high.
        type LowHighState = Low | High


    [<AutoOpen>]
    module Triggering =
        /// Where to source an external trigger from.
        type ExternalTriggerSource =
            | Trigger1
            | Trigger2
            | Pulse

        /// Where to source an internal trigger from.
        type InternalTriggerSource =
            | PulseVideo
            | PulseSync

        /// What type the trigger source should be.
        type TriggerSourceType =
            | ImmediateType
            | TriggerKeyType
            | BusType
            | ExternalType
            | InternalType
            | TimerType

        /// A complete type of a trigger source.
        type TriggerSource =
            | Immediate
            | TriggerKey
            | Bus
            | External of source : ExternalTriggerSource * polarity : Polarity
            | Internal of source : InternalTriggerSource
            | Timer of period : Duration

        /// The type of trigger, either step or list.
        type TriggerType = StepTrigger | ListTrigger

    [<AutoOpen>]
    module Modulation =
        // Unique Sources
        /// Which channel to listen for external input on.
        type ExternalInput = EXT1 | EXT2
        /// Which channel to use for the function generator.
        type FunctionGenerator = Function1

        /// Which amplitude modulation path to use.
        type AmPath = AM1 | AM2
        /// Which frequency modulation path to use.
        type FmPath = FM1 | FM2

        /// A depth of something, either linear as a percentage, or exponential, as a decibel ratio.
        type Depth =
            | Linear of depth : Percentage
            | Exponential of depth : DecibelRatio

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

        /// A source of a signal, either external or internal.
        type Source = 
            | ExternalSource of port : ExternalInput * settings : ExternalSettings
            | InternalSource of generator : FunctionGenerator * settings : FunctionSettings

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
 
        /// Get the channel which is being modulated.
        let modulationChannel = function
            | AmplitudeModulation (path, _, _) -> AmChannel path
            | FrequencyModulation (path, _, _) -> FmChannel path

        /// The location of a source.
        type SourceProvider =
            | ExternalPort of port : ExternalInput
            | InternalGenerator of generator : FunctionGenerator

        /// Get the source of a modulation.
        let modulationSource = function
            | AmplitudeModulation (_, _, source)
            | FrequencyModulation (_, _, source) -> source

        /// Find the location which is providing a source.
        let sourceProvider = function
            | ExternalSource (port,_) -> ExternalPort port
            | InternalSource (generator,_) -> InternalGenerator generator

    [<AutoOpen>]
    module Sweep =
        /// The mode to operate the sweep in, either fixed or swept.
        type SweepMode = Fixed | Swept

        /// The type of sweep to use, either list or step.
        type SweepType = List | Step

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
        type ISignal = interface end

        /// An output signal which is able to be sent through the user output BNCs.
        type IUserSignal = interface inherit ISignal end
        /// An output signal which is able to be sent through the Sweep Out BNC.
        type ISweepOutSignal = interface inherit ISignal end
        /// An output signal which is able to be sent through the Trigger1 or Trigger2 BNCs.
        type ITriggerSignal = interface inherit ISignal end
        /// A signal which is either a marker channel, or no channel.
        type IMarkerSignal = interface inherit ISignal end
        /// A signal input from one of the specifically user-controlled BNCs.
        type IUserBncSignal = interface inherit ISignal end

        /// Type to denote that no output signal is being sent through a connector.
        type NoSignal =
            | NoSignal
            interface IUserSignal
            interface ISweepOutSignal
            interface ITriggerSignal
            interface IMarkerSignal
            interface IUserBncSignal

        /// A marker signal output.
        type UserSignalMarker =
            | RouteMarker1
            | RouteMarker2
            | RouteMarker3
            | RouteMarker4
            interface IUserSignal
            interface IMarkerSignal

        /// The signal present on the 29th pin of the auxiliary connector.
        type UserSignalAux =
            | RouteAux29
            interface IUserSignal

        /// Output signals automatically generated by the system, all of which may be sent through
        /// the system output BNCs.
        type SystemSignalCommon =
            | RouteSourceSettled
            | RoutePulseVideo
            | RoutePulseSync
            | RouteSweptFunctionDone
            interface ISweepOutSignal
            interface ITriggerSignal

        /// A signal which may only be routed through the Sweep Out BNC.
        type SystemSignalSweepOut =
            | RouteSweepOut
            | RouteSweepRun
            interface ISweepOutSignal

        /// A signal which may only be routed through the two Trigger BNCs.
        type SystemSignalTrigger =
            | RouteSweepTriggerOut
            | RouteLxi
            | RoutePulseBnc
            | RouteOtherTrigger
            interface ITriggerSignal

        /// A signal coming from one of the user-controlled BNCs.
        type UserBnc =
            | RouteBasebandTrigger1
            | RouteBasebandTrigger2
            | RouteEvent1
            | RoutePatternTrigger
            interface IUserBncSignal

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
        Sweep : Sweep
        Modulation : Modulation list }