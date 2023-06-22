# Changelog

## SL23d_0 - 2023-06-14

## What's Changed

* Added centrality procedure instructions by @zsweger in  ([#513](https://github.com/star-bnl/star-sw/pull/513))
* Assorted updates in OnlTools/Jevp for 2023 run by @jml985 in  ([#515](https://github.com/star-bnl/star-sw/pull/515))
* Remove unused file StRoot/StEvent/StDetectorId.inc by @plexoos in  ([#517](https://github.com/star-bnl/star-sw/pull/517))
* new RHICf library (only raw data saving part) by @ggfdsa10 in  ([#511](https://github.com/star-bnl/star-sw/pull/511))
* update CODEOWNERS for PWGTools directory by @starsdong in  ([#518](https://github.com/star-bnl/star-sw/pull/518))
* Auau200gev run19 strefmultcorr by @zsweger in  ([#514](https://github.com/star-bnl/star-sw/pull/514))
* added owners for a few subsystems by @starsdong in  ([#520](https://github.com/star-bnl/star-sw/pull/520))
* Summer 2023 updates by @jml985 in  ([#516](https://github.com/star-bnl/star-sw/pull/516))
* Automated run-by-run QA package (version-2) for STAR BES-II dataset QA. by @EllipticFlow in  ([#521](https://github.com/star-bnl/star-sw/pull/521))
* fix: remove ClassDef from StEpdGeom by @plexoos in  ([#522](https://github.com/star-bnl/star-sw/pull/522))
* Additon of StFwdTrack to StEvent, updates to StFwdTrackMaker to accomodate by @jdbrice in  ([#492](https://github.com/star-bnl/star-sw/pull/492))
* RHICf Sub-reconstruction tool (StRHICfPID) by @ggfdsa10 in  ([#519](https://github.com/star-bnl/star-sw/pull/519))
* StNbdFitMaker.cxx: update centrality calculation by @zsweger in  ([#512](https://github.com/star-bnl/star-sw/pull/512))
* New RHICf sub-reconstruction tool (StRHICfRecoPos) by @ggfdsa10 in  ([#524](https://github.com/star-bnl/star-sw/pull/524))
* Update to remove logging from itpc cluster finder by @jml985 in  ([#532](https://github.com/star-bnl/star-sw/pull/532))
* Edited mistake in params file by @zsweger in  ([#531](https://github.com/star-bnl/star-sw/pull/531))
* New RHICf sub-reconstruction tool (StRHICfRecoEnergy) by @ggfdsa10 in  ([#527](https://github.com/star-bnl/star-sw/pull/527))
* Star simulations 2023 ideal timestamp by @klendathu2k in  ([#534](https://github.com/star-bnl/star-sw/pull/534))
* Run23 prep by @genevb in  ([#536](https://github.com/star-bnl/star-sw/pull/536))
* run by run v3 with font removed by @ssedd1123 in  ([#526](https://github.com/star-bnl/star-sw/pull/526))
* Enable default STAR Spack environment by @plexoos in  ([#496](https://github.com/star-bnl/star-sw/pull/496))
* fix(env): clean up previously loaded module before activating new env by @plexoos in  ([#542](https://github.com/star-bnl/star-sw/pull/542))
* Fix for DAQ5k cluster finder dropping outer two RDOs of TPX bug by @jml985 in  ([#541](https://github.com/star-bnl/star-sw/pull/541))
* fix: load libiconv module into envs by @plexoos in  ([#543](https://github.com/star-bnl/star-sw/pull/543))
* Remove some functions in StRHICfFunction by @ggfdsa10 in  ([#533](https://github.com/star-bnl/star-sw/pull/533))
* RHICf Run and Event number added in StRHICfCollection by @ggfdsa10 in  ([#544](https://github.com/star-bnl/star-sw/pull/544))
* Updated default behavior of run-by-run QA according to agreement in QA meeting on 5/19 by @ssedd1123 in  ([#545](https://github.com/star-bnl/star-sw/pull/545))
* Add RHICf Run and Event number address in StRHICfDbMaker by @ggfdsa10 in  ([#546](https://github.com/star-bnl/star-sw/pull/546))
* Add RHICf Run and Event number in StMuRHICfxxx by @ggfdsa10 in  ([#548](https://github.com/star-bnl/star-sw/pull/548))
* Add RHICfRun and Event number in StRHICfRawHitMaker by @ggfdsa10 in  ([#551](https://github.com/star-bnl/star-sw/pull/551))
* Mask counters for production by @YannickSoehngen in  ([#550](https://github.com/star-bnl/star-sw/pull/550))
* Small additions to StFcsWaveformFitMaker and some macros for test level 6 by @dkapukchyan in  ([#549](https://github.com/star-bnl/star-sw/pull/549))

## New Contributors

* @zsweger made their first contribution in  ([#513](https://github.com/star-bnl/star-sw/pull/513))
* @ggfdsa10 made their first contribution in  ([#511](https://github.com/star-bnl/star-sw/pull/511))
* @EllipticFlow made their first contribution in  ([#521](https://github.com/star-bnl/star-sw/pull/521))
* @ssedd1123 made their first contribution in  ([#526](https://github.com/star-bnl/star-sw/pull/526))

**Full Changelog**: https://github.com/star-bnl/star-sw/compare/SL23b_0...SL23d_0


## SL23a_0 - 2023-01-15

## What's Changed

* ci: Bump starenv to v0.2.3 by @plexoos in  ([#399](https://github.com/star-bnl/star-sw/pull/399))
* Remove OnlTools/OnlinePlots by @plexoos in  ([#407](https://github.com/star-bnl/star-sw/pull/407))
* Separate out FCS cluster seed thresholds by @akioogawa in  ([#400](https://github.com/star-bnl/star-sw/pull/400))
* cons: Use `mysql_config` to set up external dependency for mysql by @plexoos in  ([#402](https://github.com/star-bnl/star-sw/pull/402))
* cons: Use xml2-config to set up external dependency by @plexoos in  ([#404](https://github.com/star-bnl/star-sw/pull/404))
* cons: Use pkg-config to set up external dependency on log4cxx by @plexoos in  ([#405](https://github.com/star-bnl/star-sw/pull/405))
* Assorted updates to StRoot/RTS, including `char *` issue by @jml985 in  ([#406](https://github.com/star-bnl/star-sw/pull/406))
* Update CODEOWNERS by @JohnTheBlindMilkman in  ([#414](https://github.com/star-bnl/star-sw/pull/414))
* Add issue template for bug reports by @plexoos in  ([#391](https://github.com/star-bnl/star-sw/pull/391))
* SpinPool Fcs pi0 finder fixing a crash by @akioogawa in  ([#412](https://github.com/star-bnl/star-sw/pull/412))
* Bug fix on tower Et calculation for the tower selection before jet finder by @zlchang in  ([#403](https://github.com/star-bnl/star-sw/pull/403))
* ci: Disable test throwing a `std::bad_alloc` by @plexoos in  ([#420](https://github.com/star-bnl/star-sw/pull/420))
* ci: Upgrade GitHub actions by @plexoos in  ([#419](https://github.com/star-bnl/star-sw/pull/419))
* Stargenerator Vertexing Options by @klendathu2k in  ([#408](https://github.com/star-bnl/star-sw/pull/408))
* Fix copy of eTOF good channel flags to PicoEvent by @PhilippWeidenkaff in  ([#416](https://github.com/star-bnl/star-sw/pull/416))
* ci: Switch to using specific version (v5) of test data container by @plexoos in  ([#418](https://github.com/star-bnl/star-sw/pull/418))
* Add scripts to load dependency packages via Environment Modules by @jdbrice in  ([#409](https://github.com/star-bnl/star-sw/pull/409))
* ci: Add test for FST chain with `st_fwd` stream data by @plexoos in  ([#421](https://github.com/star-bnl/star-sw/pull/421))
* Encode Geometry Tag w/in DB macros by @klendathu2k in  ([#422](https://github.com/star-bnl/star-sw/pull/422))
* [root6] Modify StRoot/StBFChain/BFC.C for ROOT6/cling by @klendathu2k in  ([#423](https://github.com/star-bnl/star-sw/pull/423))
* change for yuri by @jml985 in  ([#429](https://github.com/star-bnl/star-sw/pull/429))
* Avoid pointer cast to `time_t` from incompatible type by @plexoos in  ([#427](https://github.com/star-bnl/star-sw/pull/427))
* ci: Enable more tests using star-test-data:v6 by @plexoos in  ([#415](https://github.com/star-bnl/star-sw/pull/415))
* Fix GCC warning: forbidden conversion to `char*` by @plexoos in  ([#389](https://github.com/star-bnl/star-sw/pull/389))
* Change TPX gain loading interface from `fTpx->put(... to fTpx->gain_al` by @fisyak in  ([#417](https://github.com/star-bnl/star-sw/pull/417))
* ci: Delete untagged images after successful build by @plexoos in  ([#430](https://github.com/star-bnl/star-sw/pull/430))
* docker: Add latest tag to default build configuration by @plexoos in  ([#433](https://github.com/star-bnl/star-sw/pull/433))
* ci: Turn off CI for certain directories by @plexoos in  ([#434](https://github.com/star-bnl/star-sw/pull/434))
* new ETOF table requested by Yannick by @dmarkh in  ([#435](https://github.com/star-bnl/star-sw/pull/435))
* Add .devcontainer for GitHub codespaces by @plexoos in  ([#438](https://github.com/star-bnl/star-sw/pull/438))
* Set search paths for included files for ROOT C++ interpreter by @plexoos in  ([#441](https://github.com/star-bnl/star-sw/pull/441))
* [root6] StEvent/StEnumerations.cxx fix by @klendathu2k in  ([#440](https://github.com/star-bnl/star-sw/pull/440))
* Update to StFcsWaveformFitMaker to use new algorithm by @dkapukchyan in  ([#401](https://github.com/star-bnl/star-sw/pull/401))
* Add FST raw hits in StEvent and StMuDst by @techuan-huang in  ([#378](https://github.com/star-bnl/star-sw/pull/378))
* [geometry] Add y2023 first cut and y2022a production geometries by @klendathu2k in  ([#432](https://github.com/star-bnl/star-sw/pull/432))
* [root6] Set attributes on StrangeMuDstMaker in StBFChain, rather than using interpreter to configure by @klendathu2k in  ([#444](https://github.com/star-bnl/star-sw/pull/444))
* [root6] Eliminate crash when creating the MuDst by @klendathu2k in  ([#442](https://github.com/star-bnl/star-sw/pull/442))
* New RHICf table for Seunghwan by @dmarkh in  ([#446](https://github.com/star-bnl/star-sw/pull/446))
* [root6] Fix undeclared identifier 'chain' in StarDb/AgMLGeometry/Geometry...C macro by @plexoos in  ([#448](https://github.com/star-bnl/star-sw/pull/448))
* Adding eq4 for StTriggerData by @akioogawa in  ([#450](https://github.com/star-bnl/star-sw/pull/450))
* build-pull-request.yml: cancel previous workflows by @veprbl in  ([#453](https://github.com/star-bnl/star-sw/pull/453))
* Minimum viable pentagonal sTGC geometry with misalignment by @jdbrice in  ([#447](https://github.com/star-bnl/star-sw/pull/447))
* Update to trigger simulator to save intermediate jet patch ADCs and minor cleanup to the jet maker by @zlchang in  ([#424](https://github.com/star-bnl/star-sw/pull/424))
* build: Treat all warnings as errors by @plexoos in  ([#445](https://github.com/star-bnl/star-sw/pull/445))
* Remove StBFChain dependence in StarDb/AgMLGeometry/CreateGeometry.h by @klendathu2k in  ([#462](https://github.com/star-bnl/star-sw/pull/462))
* [root6] Address issues in TTable when interpreted by Cling by @plexoos in  ([#449](https://github.com/star-bnl/star-sw/pull/449))
* update TpcRS parameters for Run19 AuAu19.6 GeV embedding by @zhux97 in  ([#463](https://github.com/star-bnl/star-sw/pull/463))
* Revert "Remove StBFChain dependence in StarDb/AgMLGeometry/CreateGeometry.h (#462)" (partially) by @plexoos in  ([#468](https://github.com/star-bnl/star-sw/pull/468))
* docker: Exit build immediately with non-zero status by @plexoos in  ([#469](https://github.com/star-bnl/star-sw/pull/469))
* Hide include directive from ROOT5 interpreter by @klendathu2k in  ([#456](https://github.com/star-bnl/star-sw/pull/456))
* cons: Extend `SKIP_DIRS` to match arbitrary path by @plexoos in  ([#467](https://github.com/star-bnl/star-sw/pull/467))
* StGenericVertex: Clean up #include statements by @plexoos in  ([#443](https://github.com/star-bnl/star-sw/pull/443))
* Run XIX-XXII new dE/dx model and calibration by @fisyak in  ([#464](https://github.com/star-bnl/star-sw/pull/464))
* ci: Improve image tagging by @plexoos in  ([#474](https://github.com/star-bnl/star-sw/pull/474))
* StFst: Fix GCC 4.8.5 warnings by @plexoos in  ([#472](https://github.com/star-bnl/star-sw/pull/472))
* ci: Add more tests by @plexoos in  ([#473](https://github.com/star-bnl/star-sw/pull/473))
* StETofHitMaker: Fix unused variable warnings by @plexoos in  ([#471](https://github.com/star-bnl/star-sw/pull/471))
* Update clock jump correction, add new method for hit manipulation on counter level by @YannickSoehngen in  ([#457](https://github.com/star-bnl/star-sw/pull/457))

## New Contributors

* @JohnTheBlindMilkman made their first contribution in  ([#414](https://github.com/star-bnl/star-sw/pull/414))
* @dkapukchyan made their first contribution in  ([#401](https://github.com/star-bnl/star-sw/pull/401))

**Full Changelog**: https://github.com/star-bnl/star-sw/compare/SL22c_0...SL23a_0


## SL22c_0 - 2022-09-09

* ci: Expand tests with 2012, 2021, and 2022 data samples by @plexoos in  ([#371](https://github.com/star-bnl/star-sw/pull/371))
* A minor bug fix in StFcsDb::getDetFromName by @akioogawa in  ([#373](https://github.com/star-bnl/star-sw/pull/373))
* Fix misconfigured alignment correction in #358, change default start time in eTOF by @PhilippWeidenkaff in  ([#372](https://github.com/star-bnl/star-sw/pull/372))
* ci: Add test jobs with doEvents.C from StRoot/macros by @plexoos in  ([#374](https://github.com/star-bnl/star-sw/pull/374))
* Fix errrors reported by GCC 11.2.1 by @plexoos in  ([#366](https://github.com/star-bnl/star-sw/pull/366))
* ci: Enable tests for ROOT5 GCC11 environment by @plexoos in  ([#370](https://github.com/star-bnl/star-sw/pull/370))
* StMuDSTMaker: fixes for TChain::kBigNumber from ROOT6 by @veprbl in  ([#236](https://github.com/star-bnl/star-sw/pull/236))
* docs: Add section about containers by @plexoos in  ([#380](https://github.com/star-bnl/star-sw/pull/380))
* cons: Help agetof to find definition file by @plexoos in  ([#381](https://github.com/star-bnl/star-sw/pull/381))
* cons: Pass PYTHONPATH to build environment by @plexoos in  ([#382](https://github.com/star-bnl/star-sw/pull/382))
* New RHICf calibrations tables for Minho by @dmarkh in  ([#385](https://github.com/star-bnl/star-sw/pull/385))
* Add official centrality calibration for 14.6 GeV Run 19 by @nigmatkulov in  ([#384](https://github.com/star-bnl/star-sw/pull/384))
* Add Runtime Option to Ignore Air in Material Plots by @klendathu2k in  ([#386](https://github.com/star-bnl/star-sw/pull/386))
* [g4star] Miscellaneous updates to StarAgmlLib by @klendathu2k in  ([#308](https://github.com/star-bnl/star-sw/pull/308))
* Fix memory leaks when MuDst included with FWD data. Introduce a switch for FTT calibration mode (dump values to file) by @jdbrice in  ([#387](https://github.com/star-bnl/star-sw/pull/387))
* Removing old hardcoded alignment from StEtofMatchMaker.cxx by @PhilippWeidenkaff in  ([#390](https://github.com/star-bnl/star-sw/pull/390))
* Add Geant track info (g2t) to StFcsHit by @akioogawa in  ([#379](https://github.com/star-bnl/star-sw/pull/379))
* StRefMultCorr updated for standalone compilation by @nigmatkulov in  ([#388](https://github.com/star-bnl/star-sw/pull/388))
* Add calculation dX in TPC using local curvature by @fisyak in  ([#383](https://github.com/star-bnl/star-sw/pull/383))
* SpaceCharge distortion corrections for FXT by @genevb in  ([#393](https://github.com/star-bnl/star-sw/pull/393))


## SL22b_0 - 2022-06-22

* sTGC offline reconstruction chain / makers by @jdbrice in  ([#287](https://github.com/star-bnl/star-sw/pull/287))
* CODEOWNER update for StEvent/StETOF/EMC by @starsdong in  ([#334](https://github.com/star-bnl/star-sw/pull/334))
* docker: install the same version of pyparsing as on SDCC by @veprbl in  ([#264](https://github.com/star-bnl/star-sw/pull/264))
* Fixing pulser correction in production code by @PhilippWeidenkaff in  ([#335](https://github.com/star-bnl/star-sw/pull/335))
* Avoid onldb master usage when possible by @genevb in  ([#336](https://github.com/star-bnl/star-sw/pull/336))
* Fix the issues causing FST codes not running by @techuan-huang in  ([#338](https://github.com/star-bnl/star-sw/pull/338))
* Gstar part updates by @klendathu2k in  ([#342](https://github.com/star-bnl/star-sw/pull/342))
* eTOF Geometry changes based on technical drawings by @PhilippWeidenkaff in  ([#344](https://github.com/star-bnl/star-sw/pull/344))
* CODEOWNERS: Remove inactive maintainers by @plexoos in  ([#347](https://github.com/star-bnl/star-sw/pull/347))
* Enable shared linking of mysqlclient lib to root4stat by @plexoos in  ([#346](https://github.com/star-bnl/star-sw/pull/346))
* adding etof-alignment table by @YannickSoehngen in  ([#348](https://github.com/star-bnl/star-sw/pull/348))
* Add FST hits in MuDst by @techuan-huang in  ([#341](https://github.com/star-bnl/star-sw/pull/341))
* ci: Bump versions of GitHub action by @plexoos in  ([#350](https://github.com/star-bnl/star-sw/pull/350))
* Check for non-zero StEvent pointer before using it by @genevb in  ([#352](https://github.com/star-bnl/star-sw/pull/352))
* Switch to single Dockerfile with matrix using ROOT5 and ROOT6 star-spack environments by @plexoos in  ([#349](https://github.com/star-bnl/star-sw/pull/349))
* Build containers using star-spack environments by @plexoos in  ([#351](https://github.com/star-bnl/star-sw/pull/351))
* Fix memory issue in destruction of StBTofGeometry by @genevb in  ([#355](https://github.com/star-bnl/star-sw/pull/355))
* A small StFcsDb update by @akioogawa in  ([#354](https://github.com/star-bnl/star-sw/pull/354))
* Bump base image to v0.1.5 + nitpicking by @plexoos in  ([#356](https://github.com/star-bnl/star-sw/pull/356))
* Help cons figure out location of dependencies by @plexoos in  ([#357](https://github.com/star-bnl/star-sw/pull/357))
* dEdx Run XXI calibrations (1st pass) by @fisyak in  ([#353](https://github.com/star-bnl/star-sw/pull/353))
* bfc.C: fix for cling by @veprbl in  ([#278](https://github.com/star-bnl/star-sw/pull/278))
* MuDst corruption fix by @jdbrice in  ([#360](https://github.com/star-bnl/star-sw/pull/360))
* Refactor CI workflow files  by @plexoos in  ([#361](https://github.com/star-bnl/star-sw/pull/361))
* ci: avoid a duplicate step in build-containers.yml by @veprbl in  ([#364](https://github.com/star-bnl/star-sw/pull/364))
* Bump starenv to v0.1.6 with compiler as build argument by @plexoos in  ([#362](https://github.com/star-bnl/star-sw/pull/362))
* Transfer documentation from star-git-tools by @plexoos in  ([#367](https://github.com/star-bnl/star-sw/pull/367))
* genDst adaption for eTOF by @PhilippWeidenkaff in  ([#358](https://github.com/star-bnl/star-sw/pull/358))


## SL22a_0 - 2022-03-13

### Enhancements

- FCS
  - Adding option to read from MuDst in StFcsRawHitMaker ([#226](https://github.com/star-bnl/star-sw/pull/226))
  - Bug: Fix min/max default value mixup ([#221](https://github.com/star-bnl/star-sw/pull/221))
  - StMuDstMaker, init mFcsCollection to zero ([#222](https://github.com/star-bnl/star-sw/pull/222))
  - Fcs mudst ([#200](https://github.com/star-bnl/star-sw/pull/200))
- FST
  - added FST subsystem ([#242](https://github.com/star-bnl/star-sw/pull/242))
- sTGC
  - HOTFIX: Missing FttCollection in StEventClusteringHints.cxx ([#227](https://github.com/star-bnl/star-sw/pull/227))
  - StEvent to StMuDst for sTGC data ([#229](https://github.com/star-bnl/star-sw/pull/229))
  - StEvent additions for sTGC (Ftt) detector ([#209](https://github.com/star-bnl/star-sw/pull/209))
- TOF
  - Modifications to StBTofCalibMaker for FXT ([#241](https://github.com/star-bnl/star-sw/pull/241))
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
  - Move back to MagFactor instead of starMagOnl for pre 2012 data (issue [#185](https://github.com/star-bnl/star-sw/issues/185)) ([#216](https://github.com/star-bnl/star-sw/pull/216))

- Fix the charge of He5Lambda ([#239](https://github.com/star-bnl/star-sw/pull/239))
- Minor fix: 2017 -> 2022 ([#219](https://github.com/star-bnl/star-sw/pull/219))

### Miscellaneous

- starsim: make seed overflow into a fatal error ([#215](https://github.com/star-bnl/star-sw/pull/215))
- Update `gstar_part.g` ([#228](https://github.com/star-bnl/star-sw/pull/228))
- Initial Run 22 chains, including FTT and FCS ([#217](https://github.com/star-bnl/star-sw/pull/217))
- StRoot/StSpinPool: replace sys/types.h with stdint.h ([#224](https://github.com/star-bnl/star-sw/pull/224))
- StRoot/StTriggerUtilities: replace sys/types.h with stdint.h ([#212](https://github.com/star-bnl/star-sw/pull/212))
- StRoot/Stv: replace sys/types.h with stdint.h ([#211](https://github.com/star-bnl/star-sw/pull/211))
- Remove StRoot/RTS/include/FTP/FTPC_PADKEY.h ([#195](https://github.com/star-bnl/star-sw/pull/195))
- ci: Avoid conflict with latest tag when using cache to/from ([#198](https://github.com/star-bnl/star-sw/pull/198))
- Star tablelib ([#189](https://github.com/star-bnl/star-sw/pull/189))


* StEvent StFcsHit adding forgotten protection against bad inputs by @akioogawa in ([#252](https://github.com/star-bnl/star-sw/pull/252))
* Remove unused dependence on GEANT3 common blocks by @klendathu2k in ([#202](https://github.com/star-bnl/star-sw/pull/202))
* mgr/Conscript-standard: take into account agmlParser dependencies by @veprbl in ([#261](https://github.com/star-bnl/star-sw/pull/261))
* misc: replace sys/types.h with stdint.h by @veprbl in ([#234](https://github.com/star-bnl/star-sw/pull/234))
* FCS pi0 finder for Ecal by Xilin by @akioogawa in ([#250](https://github.com/star-bnl/star-sw/pull/250))
* mgr/Conscript-standard: fix build of StarVMC/StarGeometry with ROOT6 that doesn't ship ttables by @veprbl in ([#259](https://github.com/star-bnl/star-sw/pull/259))
* QA run22 + fcs by @genevb in ([#260](https://github.com/star-bnl/star-sw/pull/260))
* Reintroduce run time switch for dN/dx calculation by @fisyak in ([#268](https://github.com/star-bnl/star-sw/pull/268))
* StFmsBsQaMaker: fix invaild ClassDef by @veprbl in ([#256](https://github.com/star-bnl/star-sw/pull/256))
* Hot fix for FastOffline by @akioogawa in ([#271](https://github.com/star-bnl/star-sw/pull/271))
* Fst stevent by @jdbrice in ([#265](https://github.com/star-bnl/star-sw/pull/265))
* StRoot/StTof\*: fix inconsistent validAdc/validTdc declarations by @veprbl in ([#272](https://github.com/star-bnl/star-sw/pull/272))
* StBTofCalibMaker: fix variable shadowing by @veprbl in ([#273](https://github.com/star-bnl/star-sw/pull/273))
* StMiniMcMaker: use const reference to TString in setters by @veprbl in ([#257](https://github.com/star-bnl/star-sw/pull/257))
* Drawing rules for FCS histograms by @genevb in ([#276](https://github.com/star-bnl/star-sw/pull/276))
* StEEmcClusterMaker: avoid using operator++(bool&) by @veprbl in ([#274](https://github.com/star-bnl/star-sw/pull/274))
* StEvent StTriggerData2022 patch by @akioogawa in ([#284](https://github.com/star-bnl/star-sw/pull/284))
* Add makers and utilities for FST hit processing by @jdbrice in ([#266](https://github.com/star-bnl/star-sw/pull/266))
* Fix problems with Gating Grid simulation, add fix from Xianglei for i… by @fisyak in ([#281](https://github.com/star-bnl/star-sw/pull/281))
* Stabilize calculation of uncertainties for vertices extremely close to beam line by @kehw in ([#279](https://github.com/star-bnl/star-sw/pull/279))
* Rdo clean up by @fisyak in ([#293](https://github.com/star-bnl/star-sw/pull/293))
* Tuned T0offsets and iTPC cluster sizes for Run19 Au+Au 19.6 GeV embeding by @zhux97 in ([#290](https://github.com/star-bnl/star-sw/pull/290))
* Sync with RTS code by @jml985 in ([#288](https://github.com/star-bnl/star-sw/pull/288))
* Add K\*(892)+ and K\*(892)- to gstar_part.g by @klendathu2k in ([#286](https://github.com/star-bnl/star-sw/pull/286))
* Jevp Histogram Sync to reflect production code by @jml985 in ([#292](https://github.com/star-bnl/star-sw/pull/292))
* FCS Y position change based on measurements by @akioogawa in ([#291](https://github.com/star-bnl/star-sw/pull/291))
* fix for offline flag in itpc reader by @jml985 in ([#296](https://github.com/star-bnl/star-sw/pull/296))
* QA switch for FCS by @genevb in ([#298](https://github.com/star-bnl/star-sw/pull/298))
* Fix crash in StFcsPi0FinderForEcal with MuDst VPD vertex in FastOffli… by @akioogawa in ([#299](https://github.com/star-bnl/star-sw/pull/299))
* ci: Add tests for ROOT6 build  by @plexoos in ([#154](https://github.com/star-bnl/star-sw/pull/154))
* ci: grep output log to ensure there was no segfault by @veprbl in ([#300](https://github.com/star-bnl/star-sw/pull/300))
* Fst QA and Calibration Makers by @jdbrice in ([#282](https://github.com/star-bnl/star-sw/pull/282))
* Add Toshihiro Nonaka to the StRefMultCorr codeowner list by @nigmatkulov in ([#302](https://github.com/star-bnl/star-sw/pull/302))
* Fix EMC data in picoDst data in DAQ production by @starsdong in ([#295](https://github.com/star-bnl/star-sw/pull/295))
* Revert "Fix EMC data in picoDst data in DAQ production" by @veprbl in ([#304](https://github.com/star-bnl/star-sw/pull/304))
* Adding Flemming's TPC QA to fast-offline by @imooney in ([#307](https://github.com/star-bnl/star-sw/pull/307))
* Star geant4maker starvmc geometry macros by @klendathu2k in ([#306](https://github.com/star-bnl/star-sw/pull/306))
* [g4star] Add linked hit list to STGC by @klendathu2k in ([#305](https://github.com/star-bnl/star-sw/pull/305))
* new FTT detector and tables (Daniel) by @dmarkh in ([#310](https://github.com/star-bnl/star-sw/pull/310))
* Mu fcs hit clear by @jdbrice in ([#301](https://github.com/star-bnl/star-sw/pull/301))
* Misalignment updates FST and Simplified FST Geometry by @klendathu2k in ([#285](https://github.com/star-bnl/star-sw/pull/285))
* ci: run tests with MALLOC_CHECK_=3 by @veprbl in ([#312](https://github.com/star-bnl/star-sw/pull/312))
* CODEOWNERS update by @starsdong in ([#316](https://github.com/star-bnl/star-sw/pull/316))
* TriggerSimu + picoDst for BEMC data in daq->picoDst production by @starsdong in ([#314](https://github.com/star-bnl/star-sw/pull/314))
* Keep short tracks pointing to ETOF by @genevb in ([#320](https://github.com/star-bnl/star-sw/pull/320))
* Ignore StRoot/StBFChain/doc in Git by @plexoos in ([#321](https://github.com/star-bnl/star-sw/pull/321))
* Restore GetCVS() function in StTriggerSimuMaker by @genevb in ([#322](https://github.com/star-bnl/star-sw/pull/322))
* Tpc rdo mask fix by @fisyak in ([#319](https://github.com/star-bnl/star-sw/pull/319))
* Resolve compiler warnings in StFcsDb by @genevb in ([#323](https://github.com/star-bnl/star-sw/pull/323))
* Simu pp200 run14 pythia8 hfjets by @klendathu2k in ([#315](https://github.com/star-bnl/star-sw/pull/315))
* Resolved #324 by @klendathu2k in ([#325](https://github.com/star-bnl/star-sw/pull/325))


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
