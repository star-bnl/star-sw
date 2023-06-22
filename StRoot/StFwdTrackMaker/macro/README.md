# StFwdTrackMaker
The `StFrwdTrackMaker` performs tracking with the forward silicon tracker (FST) and the forward sTGC tracker (FTT).
The tracking consists of several stages:

1. Load & sort space points from FST and FTT
2. Find seed tracks using FTT
3. Fit seed tracks using FTT + PV if available
4. Refit tracks with FST hits if  available
5. Fill StEvent structures

## Table of Contents

- Running the code
  - [Setup in Docker](#setup-in-docker)
  - [Setup on RCF](#setup-on-rcf)
  - [How to run tracking on simulation](#how-to-run-tracking-on-simulation)
  - [How to run tracking on data from DAQ files](#run-tracking-on-data-from-daq-files) 
- General info:
  - [Products and artifacts provided by the StFwdTrackMaker](#products-and-artifacts)
  - [XML configuration](#xml-configuration)
  - [Known Issues](#known-issues-in-the-tracking)
  - [Chain Options](#chain-options)


## Setup in Docker

this section is a WIP

## Setup on RCF

1. Checkout the code

    ```bash
    git clone --no-checkout git@github.com:jdbrice/star-sw-1.git
    cd star-sw-1
    git config core.sparseCheckout true

    touch .git/info/sparse-checkout

    echo "StRoot/StFst*" >> .git/info/sparse-checkout
    echo "StRoot/StFtt*" >> .git/info/sparse-checkout
    echo "StRoot/StFcs*" >> .git/info/sparse-checkout 
    # (or don't do this but drop "fcs" from BFC option in macro)
    echo "StRoot/StFwdTrackMaker/" >> .git/info/sparse-checkout
    echo "StRoot/StBFChain/" >> .git/info/sparse-checkout
    echo "StRoot/StEvent/" >> .git/info/sparse-checkout
    echo "StDb" >> .git/info/sparse-checkout
    echo "StarDb" >> .git/info/sparse-checkout

    git checkout fwd-tracking
    ```

2. initialize the environment:

    ```bash
    source StRoot/StFwdTrackMaker/macro/env.sh
    cons # build the code if not done already
    ```

    Note: this environment doesnt have the normal macro paths, so you might need to add (for DEV):

    ```sh
    gSystem->Load( "libStarRoot.so" );
    gROOT->SetMacroPath(".:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");
    ```

    to your macros if they load others (e.g. 'bfc.C').
    This will be fixed once the libraries are integrated into an official STAR library

3. Build the geometry cache

    ```bash
    ./StRoot/StFwdTrackMaker/macro/build_geom.C
    ```

4. Symlink macros for use. From project root

    ```sh
    $ ls 
    StarDb
    StDb
    StRoot
    ```

## How to run tracking on simulation

Sim link the `sim` macro directory at project root

```sh
ln -s StRoot/StFwdTrackMaker/macro/sim sim
```

There are three scripts that provide starsim + bfc:

1. Test track seed finder only (no track fitting)

    ```sh
    ./sim/run_batch_seed <JOB_ID>
    ```

    Uses the `seed.xml` configuration file

2. Test track fitting (with MC track finding)

    ```sh
    ./sim/run_batch_fast <JOB_ID>
    ```

    Uses the `fast_track.xml` configuration, check for exaxt options

3. Test track finding + fitting (analog to running on data) 

    ```sh
    ./sim/run_batch_full <JOB_ID>
    ```

    NOTE: "full" does not necessarily mean FST and PV are included, check the config file "full_track.xml"

## Run tracking on data from DAQ files

The Forward upgrade group has shared disk space at:

```sh
/gpfs01/star/pwg_tasks/FwdCalib/
```

Restored daq files can be found in the `DAQ` sub-directory (previously at my personal `/star/data03/pwg/jdb/FWD/daq/`).
Feel free to add (and organize) any restored DAQ files into this location.


In order to run on DAQ files, first sim link the `daq` macro directory at project root:

```sh
ln -s StRoot/StFwdTrackMaker/macro/daq daq
```

the run on a daq file named (or symlinked) `input.daq` with the config in `daq/daq_track.xml`:

```sh
./daq/daq_track.C >& LOG
```

or specifying the optional parameters:

```sh
root4star -b -q -l 'daq/daq_track.C( "path_to_input_file.daq", "path_to_config.xml", "dev2022" )'
```

the `daq_track.C` macro just loads necessary libraries and performs some setup to run the basic BFC chain:

```sh
in, dev2022, db, StEvent, MuDST, fcs, fst, ftt
```

Note: producing the event visualizations is very slow, so make sure to turn it off if you dont need it:

```cpp
fwdTrack->SetVisualize( false );
```

### Submitting jobs to condor

See the provided submission script: [StFwdTrackMaker/macro/daq/submit.xml](https://github.com/jdbrice/star-sw-1/blob/fwd-tracking/StRoot/StFwdTrackMaker/macro/daq/submit.xml)

You need to change:

1. Number of events to process  (first argument in)
   ```sh
   root4star -b -q -l 'daq/daq_track.C( 2, "'$INPUTFILE0'" )'
   ```
3. Filelist as input (and num files)
   ```xml
   <input URL="filelist:/gpfs01/star/pwg_tasks/FwdCalib/DAQ/zeroField_Alignment.lis" nFiles="1" />
   ```
5. output paths
   ```xml
    <stdout URL="file:/star/data03/pwg/jdb/scratch/log/log_$JOBID.log" />
    <stderr URL="file:/star/data03/pwg/jdb/scratch/log/err_$JOBID.err" />
    <output fromScratch="job_*.root" toURL="file:/star/data03/pwg/jdb/scratch/" />

    <Generator>
      <Location>/star/data03/pwg/jdb/scratch/gen</Location>
    </Generator>
   ```

Submit from the project root with:

```sh
star-submit daq/submit.xml
```

## Products and artifacts

The primary product of `StFwdTrackMaker` is the collection of `FwdTracks` stored in `StEvent/StFwdTrackCollection`. See the `StEvent/StFwdTrack.h` for the data included there. In principle it is everything needed for an analysis. And if not, feel free to make a pull request.

The `StFwdTrackMaker` can also be configured to output QA histograms. These are mostly used to debug the tracking itself:

```cpp
fwdTrack->SetGenerateHistograms( true );
```

The output filename is specified in the configuration with the top-level node:

```xml
<Output url="qa_histogram_file.root" />
```

The `StFwdTrackMaker` can also be configured to output a simple `ROOT TTree`. This is very useful for simple analysis or for investigating the performance of the tracking:

```cpp
fwdTrack->SetGenerateTree( true );
```

The TTree data members are defined by the `struct FwdTreeData` at the top of `StFwdTrackMaker.h`

```cpp
struct FwdTreeData {
  
    // hits from the sTGC;
    int fttN;
    vector<float> fttX, fttY, fttZ;
    vector<int> fttVolumeId;
    // Only avalaible for hits if MC
    vector<float> fttPt;
    vector<int> fttTrackId, fttVertexId;

    // todo: add FST hits

    // RC tracks
    int rcN;
    vector<float> rcPt, rcEta, rcPhi, rcQuality;
    vector<int> rcTrackId, rcNumFst, rcCharge;

    // MC Tracks
    int mcN;
    vector<float> mcPt, mcEta, mcPhi;
    vector<int> mcVertexId, mcCharge;

    // MC Level vertex info
    // maybe use also for TPC vertex if available in data
    int vmcN;
    vector<float> vmcX, vmcY, vmcZ;

    // track projections (those stored in StFwdTrack)
    int tprojN;
    vector<float> tprojX, tprojY, tprojZ;
    vector<int> tprojIdD, tprojIdT;

    // RAVE reco vertices
    int vrcN;
    vector<float> vrcX, vrcY, vrcZ;

    // Track hit deltas, useful for alignment checks
    int thdN;
    vector<float> thdX, thdY, thaZ;

    // Track finding computed criteria 
    // WIP, not filled as off 06-02-2022
    std::map<string, std::vector<float>> Crits;
    std::map<string, std::vector<int>> CritTrackIds;
};
```

Finally, the tracking code can produce visualization data in the [Wavefront OBJ](https://en.wikipedia.org/wiki/Wavefront_.obj_file) file format for 3D graphics. If you are interested ask me for more details.

# XML Configuration
The forward tracker can be configured via an XML configuration file.
Look at the examples in the `sim` and `daq` folders as well.

The basic structure is:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<config>
    <Output url="full_track.root" />
    <Geometry>fGeom.root</Geometry>
    <Source ... />
    <SiRasterizer r="..." phi="..." />
    
    <TrackFinder ... >
        ...
    </TrackFinder>
    
    <TrackFitter ... >
        ...
    </TrackFitter>
</config>
```

The file contains several parameters that can be configured:

- The name of the output QA file.

```xml
    <Output url="..." />
```

- The uncertaities on the FST hit points
  - SIMULATION: only used when running directly on GEANT hits
  - DATA: used to provide the uncertainties for track fitting

```xml
    <SiRasterizer r="..." phi="..." />
```

- Select various sources for the tracking data from ftt can be ["DATA", "GEANT"]. Default is data from StEvent, which can be data or fast/slow sim (since they write into fttCollection now)

```xml
    <Source ftt="..." />
```

The `<TrackFinder ... >` block is reqired for data. For simulation, if it is missing then MC track finding is performed i.e. perfect track seeds.
The `<TrackFinder ... >` block can contain:

- attribute, number of tracking iterations

```xml
    <TrackFinder nIterations="1" >
```

- `<Connector distance="..." />` : determines the distance between track seed hits. a value of 1 means hits must be found on adjacent sTGC planes to form a track seed. A value of 2 means that one plane can be skipped between points.

- `<SubsetNN active="..." min-hits-on-track="..." />`
  - `active="true|false"` : if `true` find subset of tracks that minimize the number of shared points on tracks.
  - `min-hits-on-track` minimum number of points on a track seed. Used even if `actiive="false"` to return valid track seeds.
- `<HitRemover active="true|false" />` : not implemented yet

Example:

```xml
<Connector distance="1"/>
<SubsetNN active="true" min-hits-on-track="3" />
<HitRemover active="false" />
```

- number of phi slices to perform the tracking in. More slices can speed up high mult events, but may also reduce efficiency of low pT tracks (large curvatre).

```xml
    <Iteration nPhiSlices="32" >
``` 

Note: there should be one `<Iteration>` block for each nmber given by `nIterations`. If you have less, then the last block is used for all remaining fit iterations

- `<Iteration ...>` blocks should contain
  - `<SegmentBuilder>` : Track finding CA parameters for two hit segments
  - `<ThreeHitSegments>` : Track finding CA parameters for three hit segments

For example:

```xml
<SegmentBuilder>
    <Criteria name="Crit2_RZRatio" min="0" max="1.20" />
    <Criteria name="Crit2_DeltaRho" min="-50" max="50.9"/>
    <Criteria name="Crit2_DeltaPhi" min="0" max="30.0" />
    <Criteria name="Crit2_StraightTrackRatio" min="0.01" max="5.85"/>
</SegmentBuilder>

<ThreeHitSegments>
    <Criteria name="Crit3_3DAngle" min="0" max="30" />
    <Criteria name="Crit3_PT" min="0" max="100" />
    <Criteria name="Crit3_ChangeRZRatio" min="0.8" max="1.21" />
    <Criteria name="Crit3_2DAngle" min="0" max="30" />
</ThreeHitSegments>
```

These provide somewhat loose but reasonable selection for tracks. They are likely to change. 

The `<TrackFitter ... >` block is reqired for data. If it is missing or the attribute `off="true"` is set, then track fitting will be skipped.
The `<TrackFitter ... >` block can contain:

- atribute `refitSi="true|false"` : controls whether or not tracks are refit with Si hits
- attribute : `mcSeed="true|false"` : only used for simulation. If true, then use the mc pt and eta values to seed the track fit (with some blurring).
- attribte `zeroB="true|false"` : if true, use zero field
- attribte `constB="true|false"` : if true, use constant 0.5 T field everywhere

  - `<Vertex ...>` block : controls vertex in track fitting
    - attribute `sigmaXY` : transverse uncertainty
    - attribute `sigmaZ` : longitudinal uncertainty
    - attribute `includeInFit="true|false"` : use primary vertex in track fit

```xml
<TrackFitter refitSi="true" mcSeed="false" zeroB="true"  >
    <Vertex sigmaXY="3" sigmaZ="100.0" includeInFit="true" smearMcVertex="false" />
</TrackFitter>
```

Note: in data if the `<Vertex ... includeInFit="true" />` tag is used and a primary vertex from the TPC is not available then the vertex position is set to (0,0,0) - so it can be used more like an ideal beam-line constraint in the fits.

## Known issues in the tracking

- Long distance projections fail due to a hard-coded condition in GenFit2 code, no work-around yet
  - Specifically effects the HCAL matching (projections to HCAL almost always fail)

## Chain Options

- `fwdTrack` : the forward track maker
- `fst` : Runs the FST offline chain
- `fcs` : Runs the FCS offline chain
- `ftt` : Runs the FTT offline chain
- `fwdTrack` : Runs the forward tracking, make sure it is after the detector chains.
- Simulation:
  - `fttFastSim` : Ftt fast simulator, produces space points directly
  - `fstFastSim` : Fst fast simulator, produces space points directly
  - `fcsSim` : Fcs simulator, produces raw data to be processed by standard offline chain

A basic chain for FWD detector reconstruction only:

```sh
in, dev2022, db, StEvent, MuDST, fcs, fst, ftt
```
