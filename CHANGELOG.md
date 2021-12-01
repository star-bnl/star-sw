# Changelog

## SL21d_1 - 2021-12-01

### Enhancements

- FCS
  - Fcs mudst ([#200](https://github.com/star-bnl/star-sw/pull/200))
- sTGC
  - StEvent additions for sTGC (Ftt) detector ([#209](https://github.com/star-bnl/star-sw/pull/209))
- Online
  - Trigger group removed 5DSM from BBC crate, thus changing structure. ([#207](https://github.com/star-bnl/star-sw/pull/207))
  - StEvent StTriggerData2022 update associate with  ([#203](https://github.com/star-bnl/star-sw/pull/203))
  - updates to daq readers in prep for run 22 ([#186](https://github.com/star-bnl/star-sw/pull/186))

### Bug fixes

- EPD
  - Fix cut limits in EPD macro ([#188](https://github.com/star-bnl/star-sw/pull/188))
- FCS
  - StFcsDB makes no file unless debug is on ([#218](https://github.com/star-bnl/star-sw/pull/218))
- Online
  - Fix for pulser histogram in online plots ([#187](https://github.com/star-bnl/star-sw/pull/187))
- TPC
  - Move back to MagFactor instead of starMagOnl for pre 2012 data (issue #185) (#216)

- Minor fix: 2017 -> 2022 ([#219](https://github.com/star-bnl/star-sw/pull/219))

### Miscellaneous

- Initial Run 22 chains, including FTT and FCS ([#217](https://github.com/star-bnl/star-sw/pull/217))
- StRoot/Stv*: replace sys/types.h with stdint.h ([#211](https://github.com/star-bnl/star-sw/pull/211))
- Remove StRoot/RTS/include/FTP/FTPC_PADKEY.h ([#195](https://github.com/star-bnl/star-sw/pull/195))
- ci: Avoid conflict with latest tag when using cache to/from ([#198](https://github.com/star-bnl/star-sw/pull/198))
- Star tablelib ([#189](https://github.com/star-bnl/star-sw/pull/189))


## SL21d_0 - 2021-11-14

### Enhancements

- FCS
  - Updates from Tonko on StRoot/RTS/src/DAQ_FCS and TRG_FCS ([#179](https://github.com/star-bnl/star-sw/pull/179))
  - FCS db table fix (array size) ([#178](https://github.com/star-bnl/star-sw/pull/178))
  - Changing FCS Wcal and Hcal x positions in Geometry file based on run 21 survey ([#177](https://github.com/star-bnl/star-sw/pull/177))
  - Update readers for stgc and fcs ([#167](https://github.com/star-bnl/star-sw/pull/167))
  - Introduce new StFcsCosmicMaker in StRoot/SpinPool ([#58](https://github.com/star-bnl/star-sw/pull/58))
- FST
  - Initial Implement of FST Online Monitor ([#169](https://github.com/star-bnl/star-sw/pull/169))
  - Update to FST geometry to add a simplified version of the geometry for tracking  ([#147](https://github.com/star-bnl/star-sw/pull/147))
- TOF
  - Move histogram creation of reference pulser outside of daQA flag ([#182](https://github.com/star-bnl/star-sw/pull/182))
  - Add good event flag to ETOF headers and StPicoEvent ([#160](https://github.com/star-bnl/star-sw/pull/160))
- TPC
  - Tuning up TpcRS for AuAu19GeV 2019 ([#184](https://github.com/star-bnl/star-sw/pull/184))
  - Add fix for fixed Target run with broken yellowIntensity information ([#180](https://github.com/star-bnl/star-sw/pull/180))
  - Dedx for fixed target in 2019 ([#175](https://github.com/star-bnl/star-sw/pull/175))
  - Averaged magnet current to set magnetic field strength ([#170](https://github.com/star-bnl/star-sw/pull/170))
  - Introduce FXT chain option to enable anything FXT-specific ([#161](https://github.com/star-bnl/star-sw/pull/161))
  - Xianglei Zhu, 09/11/2021: FXT3p85_2018 and 09/14/2021: Isobar parameters ([#156](https://github.com/star-bnl/star-sw/pull/156))
- Online
  - Adding StTriggerData2022 for new v47 trigger data structure ([#172](https://github.com/star-bnl/star-sw/pull/172))
  - update 2021 trigger data format to version 0x47 ([#171](https://github.com/star-bnl/star-sw/pull/171))
  - StEvent/StTriggerData : Adding 3 access functions for revTick ([#183](https://github.com/star-bnl/star-sw/pull/183))
- Add Anti-H4 Lambda decay to Anti-He4 + pion- ([#176](https://github.com/star-bnl/star-sw/pull/176))
- Geometry update for 2022 ([#173](https://github.com/star-bnl/star-sw/pull/173))
- StJetMaker: add idTruth to StJetTrack ([#152](https://github.com/star-bnl/star-sw/pull/152))
- Use std::ostringstream in place of STARs ostrstream ([#150](https://github.com/star-bnl/star-sw/pull/150), [#165](https://github.com/star-bnl/star-sw/pull/165), [#168](https://github.com/star-bnl/star-sw/pull/168))
- Fix dependency of STAR geometry libraries on generated source files ([#148](https://github.com/star-bnl/star-sw/pull/148))
- Various changes to make code compatible with ROOT6 API ([#47](https://github.com/star-bnl/star-sw/pull/47))
- Updated CI
  - Build against ROOT6 in addition to ROOT5; switch to multistage docker build; optimize base image ([#75](https://github.com/star-bnl/star-sw/pull/75))
  - Add test jobs to CI ([#112](https://github.com/star-bnl/star-sw/pull/112), [#106](https://github.com/star-bnl/star-sw/pull/106), [#71](https://github.com/star-bnl/star-sw/pull/71))
  - Fix compiler errors and include previously excluded packages into CI build
    - StRoot/StSpinPool ([#63](https://github.com/star-bnl/star-sw/pull/63))
    - StarVMC/GeoTestMaker ([#72](https://github.com/star-bnl/star-sw/pull/72))
    - StRoot/StEEmcPool ([#83](https://github.com/star-bnl/star-sw/pull/83))
    - StRoot/StFgtPool ([#86](https://github.com/star-bnl/star-sw/pull/86), [#93](https://github.com/star-bnl/star-sw/pull/93))
    - StRoot/StHighptPool ([#87](https://github.com/star-bnl/star-sw/pull/87))
    - StRoot/StarGenerator/Kinematics ([#96](https://github.com/star-bnl/star-sw/pull/96))
    - StRoot/StHbtMaker ([#98](https://github.com/star-bnl/star-sw/pull/98))

### Miscellaneous

- Fix file name conflicts on case insensitive systems ([#48](https://github.com/star-bnl/star-sw/pull/48))
- Remove deprecated StRoot/StSpinMaker ([#66](https://github.com/star-bnl/star-sw/pull/66))
- Cons build scripts cleanup ([#99](https://github.com/star-bnl/star-sw/pull/99), [#62](https://github.com/star-bnl/star-sw/pull/62))
- Address compiler warning about set but not used variables in StRoot/StEEmcPool ([#104](https://github.com/star-bnl/star-sw/pull/104))
- Update SCGL macro ([#105](https://github.com/star-bnl/star-sw/pull/105))
- cons: Skip uncompilable package StShadowMaker ([#69](https://github.com/star-bnl/star-sw/pull/69))


## SL21c_5 - 2021-10-08

### star-mcgen

- StarPythia8 path to XML data ([#3](https://github.com/star-bnl/star-sw/pull/3))
- Fix initialization of Hijing interface ([#5](https://github.com/star-bnl/star-sw/pull/5))


## SL21c_4 - 2021-10-08

- fixed a logical error for the PicoVtxMod::Mtd in StPicoDstMaker.cxx ([#163](https://github.com/star-bnl/star-sw/pull/163))


## SL21c_3 - 2021-09-16

- let StBTofSimResParams.h to use existed timestamp in bfc chain, instead of re-define a new timestamp ([#145](https://github.com/star-bnl/star-sw/pull/145))


## SL21c_2 - 2021-08-24

- Add new BFC option for StPicoDstMaker: PicoVtxMtd ([#116](https://github.com/star-bnl/star-sw/pull/116))


## SL21c_1 - 2021-08-20

- Add local DB parameters for TpcRS 26p5GeV_fixedTarget_2018 ([#107](https://github.com/star-bnl/star-sw/pull/107), [#88](https://github.com/star-bnl/star-sw/pull/88))
- Change BTOF and VPD chain option names to something more descriptively useful ([#84](https://github.com/star-bnl/star-sw/pull/84))
- StJetMaker: save additional TOF information to the jet trees ([#79](https://github.com/star-bnl/star-sw/pull/79))
- TpcRS will need StEventUtilities lib for EbyET0 ([#78](https://github.com/star-bnl/star-sw/pull/78))


## SL21c_0 - 2021-07-29

First release after code transfer to Git

### Enhancements

- Employ initial CI build based on a Docker container with Spack environment ([#37](https://github.com/star-bnl/star-sw/pull/37))
- Introduce run time switch for dN/dx calculations in StRoot/StdEdxY2Maker ([#52](https://github.com/star-bnl/star-sw/pull/52))
- Improve pp and pA handling in StRoot/StBTofCalibMaker
- Update centrality bins in StRoot/StRefMultCorr
- Enhance cons scripts to allow more control over external dependencies location ([#8](https://github.com/star-bnl/star-sw/pull/8))
- Introduce .github/CODEOWNERS
- Add new data member to mark all primary tracks in StPicoTrack ([#34](https://github.com/star-bnl/star-sw/pull/34))
- Event-by-event T0 correction applied in StTpcHitMoverMaker (reconstruction)
  must by un-corrected in StTpcRSMaker (simulation) ([#76](https://github.com/star-bnl/star-sw/pull/76))

### Bug fixes

- Fix pre Run 13 BTOF geometry by properly accounting valid trays and GEM-trays
- Fix memory leak introduced in SL21b (Issue [#21](https://github.com/star-bnl/star-sw/pull/21))
- Fix BTOF tray indexing issue; Select params based on run number ([#65](https://github.com/star-bnl/star-sw/pull/65))

### API changes

- Remove outdated `StAutoBrowse` and related public methods ([#6](https://github.com/star-bnl/star-sw/pull/6)):

      void StObject::Browse(TBrowser *tb)
      Bool_t StObject::IsFolder() const
      void StEmcDetector::Browse(TBrowser *b)
      void StEvent::Browse(TBrowser* b)

### Miscellaneous

- Multiple changes in StRoot/StTpcHitMaker

### star-mcgen

- Move pams/gen/idl from star-mcgen repo to star-sw


## SL21b_v2 - 2021-04-02

### Enhancements

- Introduce StFwdTrackMaker package for track reconstruction with Forward
  Tracking System (FTS)
- Introduce fast simulators for FTS silicon-strip and small-strip Thin Gap
  Chamber (sTGC) detectors
- New code for handling Forward Calorimeter data and simulation (StRoot/StFcs*)

### Miscellaneous

- Unreviewed commits submitted to StRoot/StTpcDb


## SL21b - 2021-03-24

- Event-by-event T0 corrections for TPC
- Add new BFC option for selecting a vertex in pico events: PicoVtxVpdOrDefault
- Save MC track and MC vertex information in PicoDst
- Provide initial revision for new y2021a geometry


## SL21a - 2021-02-08

### Enhancements

- Update older geometries: y2018c, y2019b, and y2020b
- Updates to FST geometry (StarVMC/Geometry/FstmGeo/FstmGeo.xml)
- New code to support FCS commissioning
  - StEvent data structures, database, and online DAQ support for the new system
