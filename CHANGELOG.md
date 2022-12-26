# Changelog

## SL21c_8 - 2022-12-26

- [SL21c] Backport cons, CI, and tests from main to SL21c by @plexoos in  ([#465](https://github.com/star-bnl/star-sw/pull/465))

  - Moved the CI, tests, and build machinery from `main` (5424c348) in order to
    assemble Docker images at https://github.com/star-bnl/star-sw/pkgs/container/star-sw

- [SL21c] Backport TPC RS changes by @plexoos in  ([#466](https://github.com/star-bnl/star-sw/pull/466))


## SL21c_7 - 2022-02-02

- Backport the latest TpcRS code and Run19 parameters to SL21c ([#294](https://github.com/star-bnl/star-sw/pull/294))
- Backport PR [#281](https://github.com/star-bnl/star-sw/pull/281) to SL21c ([#289](https://github.com/star-bnl/star-sw/pull/289))


## SL21c_6 - 2021-12-15

- Modified TPC RS
  - Tuning up TpcRS for AuAu19GeV 2019 ([#184](https://github.com/star-bnl/star-sw/pull/184))
- Updated K0(892) mass and width according to the latest PDG values
  - Update gstar_part.g ([#228](https://github.com/star-bnl/star-sw/pull/228))
- Added new particle systems to StRoot/StarClassLibrary
  - Add Anti-H4 Lambda decay to Anti-He4 + pion- ([#176](https://github.com/star-bnl/star-sw/pull/176))
- Tests and docker images can be built now on this branch
  - Backport CI machinery from main branch ([#235](https://github.com/star-bnl/star-sw/pull/235))
- Resolved file name conflicts on case insensitive filesystems


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
