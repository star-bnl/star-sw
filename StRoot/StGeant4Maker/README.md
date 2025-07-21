# StGeant4Maker Module

## Architecture

The `StGeant4Maker` module provides an interface to the Geant4 simulation toolkit within the STAR software framework, using the Virtual Monte Carlo (VMC) abstraction layer. This allows for flexible and detailed detector simulations, with the ability to use Geant4, Geant3, or a combination of both as physics engines.

The core architecture consists of the following components:

1.  **Main Controller (`StGeant4Maker`)**: This `StMaker` class is the central hub of the module. It initializes the simulation environment, including the detector geometry (loaded via AgML), the magnetic field, and the chosen physics engine(s). It drives the event loop, taking primary particles from an upstream generator (`StarPrimaryMaker`), and manages the final output.

2.  **VMC Interface (`StarVMCApplication`)**: This class implements the `TVirtualMCApplication` interface, acting as the bridge between the STAR framework and the generic VMC layer. It forwards calls for geometry construction, primary particle generation, and step-by-step event processing to `StGeant4Maker`.

3.  **Particle and Truth Management (`StMCParticleStack`)**: A custom particle stack that is central to the module's operation. It serves two purposes:
    *   It manages the `TParticle` stack required by the VMC for tracking.
    *   Crucially, it builds and maintains a parallel, persistent Monte Carlo truth record. This record consists of `StarMCParticle` and `StarMCVertex` objects, which form a complete history of all particles, their interactions, and their parent-daughter relationships. This truth tree is used to populate the final output tables.

4.  **Hit Production**:
    *   **`StSensitiveDetector`**: A generic sensitive detector class that can be attached to any active detector volume in the geometry.
    *   **`StHitCollection`**: When a particle passes through a sensitive volume, the corresponding `StSensitiveDetector` records the interaction. These interactions are stored in specialized `StHitCollection` objects (`StTrackerHitCollection` for trackers, `StCalorimeterHitCollection` for calorimeters).
    *   **`DetectorHit`**: The base class for a single simulated hit, with derived classes `TrackerHit` and `CalorimeterHit` storing detector-specific information.

5.  **Volume Identification**: To map hits to specific detector elements, a system of volume identifiers is used.
    *   **`AgML...VolumeId`**: A suite of small classes, one for each detector subsystem (e.g., `AgMLTpcVolumeId`, `AgMLEmcVolumeId`), responsible for calculating a unique integer ID for a sensitive volume based on its hierarchical position in the `TGeo` geometry.
    *   **`AgMLVolumeIdFactory`**: A factory that creates the correct `AgMLVolumeId` object for a given detector family name.

6.  **Output**: At the end of each event, `StGeant4Maker` iterates through the `StMCParticleStack`'s truth record and the various `StHitCollection`s to produce the standard STAR `g2t` tables (`g2t_track`, `g2t_vertex`, and detector-specific hit tables), which are then available for downstream reconstruction and analysis.

## Class Summary

### Core Components
*   `StGeant4Maker`: The main STAR maker that controls the Geant4 simulation.
*   `StarVMCApplication`: Implements the TVirtualMCApplication interface, bridging STAR and the VMC.
*   `StGeant4Application`: An empty, legacy TVirtualMCApplication required by the interface but superseded by `StarVMCApplication`.
*   `StarMagFieldAdaptor`: An adapter to allow the VMC to use the standard STAR magnetic field.
*   `GeometryUtils`: Helper functions to retrieve STAR-specific metadata (`AgMLExtension`) from the `TGeo` geometry.

### Particle and Truth Management
*   `StMCParticleStack`: A custom VMC stack that manages `TParticle`s and builds a parallel MC truth tree.
*   `StarMCParticle`: A container for a particle's full truth information, including its associated vertices and hits.
*   `StarMCVertex`: A container for a particle interaction or decay vertex, storing its position, parent, and daughters.

### Sensitive Detectors and Hits
*   `StSensitiveDetector`: The main class for a VMC sensitive detector in STAR.
*   `StHitCollection`: Abstract base class for hit collections.
*   `StTrackerHitCollection`: A collection of `TrackerHit` objects for tracking detectors.
*   `StCalorimeterHitCollection`: A collection of `CalorimeterHit` objects for calorimeters.
*   `DetectorHit`: Base class for a single simulated hit.
*   `TrackerHit`: A detailed hit in a tracking detector.
*   `CalorimeterHit`: A hit in a calorimeter, focused on energy deposition.

### Volume Identifiers
*   `AgMLVolumeIdFactory`: A factory that creates detector-specific volume identifier objects.
*   `AgMLBbcVolumeId`: Volume identifier for the Beam-Beam Counter (BBC).
*   `AgMLBTofVolumeId`: Volume identifier for the Barrel Time-of-Flight (BTOF).
*   `AgMLEEmcVolumeId`: Volume identifier for the Endcap Electromagnetic Calorimeter (EEMC).
*   `AgMLEmcVolumeId`: Volume identifier for the Barrel Electromagnetic Calorimeter (BEMC).
*   `AgMLEpdVolumeId`: Volume identifier for the Event Plane Detector (EPD).
*   `AgMLETofVolumeId`: Volume identifier for the Endcap Time-of-Flight (ETOF).
*   `AgMLFmsVolumeId`: Volume identifier for the Forward Meson Spectrometer (FMS).
*   `AgMLFstVolumeId`: Volume identifier for the Forward Silicon Tracker (FST).
*   `AgMLHcaVolumeId`: Volume identifier for the Forward Hadron Calorimeter (Hcal).
*   `AgMLMtdVolumeId`: Volume identifier for the Muon Telescope Detector (MTD).
*   `AgMLPreVolumeId`: Volume identifier for a generic preshower detector.
*   `AgMLStgVolumeId`: Volume identifier for the sTGC detector.
*   `AgMLTpcVolumeId`: Volume identifier for the Time Projection Chamber (TPC).
*   `AgMLVpdVolumeId`: Volume identifier for the Vertex Position Detector (VPD).
*   `AgMLWcaVolumeId`: Volume identifier for the Forward Hadron Calorimeter (Wcal).
