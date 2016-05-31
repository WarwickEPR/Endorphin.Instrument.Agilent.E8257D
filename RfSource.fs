// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Agilent.E8257D

/// Command set of the RF instrument.
/// Implements functions to modify & query configuration.
/// Organised by subsystem mirroring the Agilent configuration.
module RfSource =
    open Sweep

    /// Open an RfSource given a VISA address and timeout in milliseconds.
    let openInstrument  = IO.Connect.openInstrument
    /// Close an RfSource instrument.
    let closeInstrument rfSource = async {
        do! IO.Connect.setLocal rfSource 
        do! IO.Connect.closeInstrument rfSource }
    /// Query the identity of an RfSource, and match the returned string against ones known
    /// to the program.
    let queryIdentity = IO.Identify.queryIdentity

    /// Apply a set of settings to the given RfSource machine.
    let applySettings rfSource settings = async {
        match settings.Sweep with
        | NoSweep (frequency,amplitude)
            -> do! setCarrierFrequency rfSource frequency
               do! setCarrierAmplitude rfSource amplitude
        | StepSweep sweep
            -> do! Sweep.Apply.stepSweep rfSource sweep
        do! Modulation.Apply.modulationSettings rfSource settings.Modulation }