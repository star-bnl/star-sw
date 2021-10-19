# Changelog

## [SL21d_0] - future release on SL21d branch

### Enhancements

- Geometry update for 2022 (#173)
- StJetMaker: add idTruth to StJetTrack (#152)
- Use std::ostringstream in place of STARs ostrstream (#150)
  - Garbage in the output produced by StChain #165
  - StRoot/StUtilities/StMessage: fix regression from using the proper std::ostringstream::str (#168)
- Fix dependency of STAR geometry libraries on generated source files (#148)
- Update to FST geometry to add a simplified version of the geometry for tracking  (#147)
- Add test jobs to CI (#112, #106, #71)
- Change BTOF and VPD chain option names to something more descriptively useful (#84)
- Various changes to make code compatible with ROOT6 API (#47)
- Updates in CI (#75)
  - Build against ROOT6 in addition to ROOT5
  - Switch to multistage docker build; optimize base image
- Fix compiler errors and include previously excluded packages into CI build
  - StRoot/StSpinPool (#63)
  - StarVMC/GeoTestMaker (#72)
  - StRoot/StEEmcPool (#83)
  - StRoot/StFgtPool (#86, #93)
  - StRoot/StHighptPool (#87)
  - StRoot/StarGenerator/Kinematics (#96)
  - StRoot/StHbtMaker into CI build (#98)
- Introduce new StFcsCosmicMaker in StRoot/SpinPool (#58)

### Bug fixes

- fixed a logical error for the PicoVtxMod::Mtd in StPicoDstMaker.cxx (#163)

### Miscellaneous

- Fix file name conflicts on case insensitive systems (#48)
- Remove deprecated StRoot/StSpinMaker (#66)
- Cons build scripts cleanup (#99 #62)
- Add local DB parameters for TpcRS 26p5GeV_fixedTarget_2018 (#107, #88)
- Address compiler warning about set but not used variables in StRoot/StEEmcPool (#104)
- Update SCGL macro (#105)
- cons: Skip uncompilable package StShadowMaker (#69)

### Online

- Initial Implement of FST Online Monitor (#169)
- Update readers for stgc and fcs (#167)


## [SL21c_5] - 2021-10-08

### star-mcgen

- StarPythia8 path to XML data (#3)
- Fix initialization of Hijing interface (#5)


## [SL21c_4] - 2021-10-08

- fixed a logical error for the PicoVtxMod::Mtd in StPicoDstMaker.cxx (#163)


## [SL21c_3] - 2021-09-16

- let StBTofSimResParams.h to use existed timestamp in bfc chain, instead of re-define a new timestamp (#145)


## [SL21c_2] - 2021-08-24

- Add new BFC option for StPicoDstMaker: PicoVtxMtd (#116)


## [SL21c_1] - 2021-08-20

- Add local DB parameters for TpcRS 26p5GeV_fixedTarget_2018 (#107, #88)
- Change BTOF and VPD chain option names to something more descriptively useful (#84)
- StJetMaker: save additional TOF information to the jet trees (#79)
- TpcRS will need StEventUtilities lib for EbyET0 (#78)


## [SL21c_0] - 2021-07-29

First release after code transfer to Git

### Enhancements

- Employ initial CI build based on a Docker container with Spack environment (#37)
- Introduce run time switch for dN/dx calculations in StRoot/StdEdxY2Maker (#52)
- Improve pp and pA handling in StRoot/StBTofCalibMaker
- Update centrality bins in StRoot/StRefMultCorr
- Enhance cons scripts to allow more control over external dependencies location (#8)
- Introduce .github/CODEOWNERS
- Add new data member to mark all primary tracks in StPicoTrack (#34)
- Event-by-event T0 correction applied in StTpcHitMoverMaker (reconstruction)
  must by un-corrected in StTpcRSMaker (simulation) (#76)

### Bug fixes

- Fix pre Run 13 BTOF geometry by properly accounting valid trays and GEM-trays
- Fix memory leak introduced in SL21b (Issue #21)
- Fix BTOF tray indexing issue; Select params based on run number (#65)

### API changes

- Remove outdated `StAutoBrowse` and related public methods (#6):

      void StObject::Browse(TBrowser *tb)
      Bool_t StObject::IsFolder() const
      void StEmcDetector::Browse(TBrowser *b)
      void StEvent::Browse(TBrowser* b)

### Miscellaneous

- Multiple changes in StRoot/StTpcHitMaker

### star-mcgen

- Move pams/gen/idl from star-mcgen repo to star-sw


## [SL21b_v2] - 2021-04-02

### Enhancements

- Introduce StFwdTrackMaker package for track reconstruction with Forward
  Tracking System (FTS)
- Introduce fast simulators for FTS silicon-strip and small-strip Thin Gap
  Chamber (sTGC) detectors
- New code for handling Forward Calorimeter data and simulation (StRoot/StFcs*)

### Miscellaneous

- Unreviewed commits submitted to StRoot/StTpcDb


## [SL21b] - 2021-03-24

- Event-by-event T0 corrections for TPC
- Add new BFC option for selecting a vertex in pico events: PicoVtxVpdOrDefault
- Save MC track and MC vertex information in PicoDst
- Provide initial revision for new y2021a geometry


## [SL21a] - 2021-02-08

### Enhancements

- Update older geometries: y2018c, y2019b, and y2020b
- Updates to FST geometry (StarVMC/Geometry/FstmGeo/FstmGeo.xml)
- New code to support FCS commissioning
  - StEvent data structures, database, and online DAQ support for the new system
