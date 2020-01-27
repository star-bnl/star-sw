/*!
 * \file StEnumerations.h
 */
/***************************************************************************
 *
 * $Id: StEnumerations.h,v 2.69 2020/01/27 21:28:31 genevb Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description: Enumerations for StEvent.
 *              Note that lots of code depend on this file and
 *              any change will force a recompilation of almost
 *              all of StEvent.
 *
 ***************************************************************************
 *
 * $Log: StEnumerations.h,v $
 * Revision 2.69  2020/01/27 21:28:31  genevb
 * Add short tracks toward ETOF
 *
 * Revision 2.68  2019/02/11 18:51:31  ullrich
 * Added EToF.
 *
 * Revision 2.67  2018/12/11 19:53:10  ullrich
 * Added RICHf.
 *
 * Revision 2.66  2018/04/30 23:18:10  smirnovd
 * [Cosmetic] Minor changes in various files
 *
 * - Renamed data member s/m/mMass/ in StikalmanTrack
 * - Changes in white space
 * - Return STAR code
 *
 * Revision 2.65  2018/03/27 02:40:12  genevb
 * Introduce kiTpcId
 *
 * Revision 2.64  2017/05/04 00:54:13  perev
 * Fts added
 *
 * Revision 2.63  2016/12/19 16:11:14  ullrich
 * Added FPost detector ID and StFpostConstants enums.
 *
 * Revision 2.62  2016/02/25 17:10:20  ullrich
 * Implemented detector() which is now a pure abstract method in StHit.
 *
 * Revision 2.61  2015/12/24 00:14:44  fisyak
 * Add GMT and SST Id and new dE/dx method
 *
 * Revision 2.60  2015/09/14 16:58:13  ullrich
 * Added items to StFmsDetectorId enumeration
 *
 * Revision 2.59  2015/09/01 18:29:01  ullrich
 * Changes due to adding StFpsSlat and interconnection between slats and points.
 *
 * Revision 2.58  2015/05/13 17:06:13  ullrich
 * Added hooks and interfaces to Sst detector (part of HFT).
 *
 * Revision 2.57  2015/02/14 18:57:24  ullrich
 * Big upgrade after adding StFmPoint and StFmsCluster.
 *
 * Revision 2.56  2014/04/10 14:35:26  ullrich
 * Add Ist constants.
 *
 * Revision 1.1.1.1  2013/07/23 14:13:30  fisyak
 *
 *
 * Revision 2.54  2013/07/16 14:29:03  fisyak
 * Restore mass fit tracks
 *
 * Revision 2.53  2013/05/07 19:30:21  jeromel
 * No change but add markers
 *
 * Revision 2.52  2013/04/10 19:15:52  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.50  2013/01/15 23:18:12  fisyak
 * Add more bits for track quality matching to EMC
 *
 * Revision 2.49  2013/01/08 18:56:03  ullrich
 * Changed value of kFgtNumTimeBins
 *
 * Revision 2.48  2012/11/05 18:18:45  ullrich
 * Add 3 new members to StFgtClusterSeedType.
 *
 * Revision 2.47  2012/09/16 21:34:48  fisyak
 * Add Tpc West Only and East only bits
 *
 * Revision 2.46  2012/07/30 14:41:52  ullrich
 * Added 2 new variables to enum StFgtClusterSeedType
 *
 * Revision 2.45  2012/07/21 03:31:37  perev
 *  Increase Track Range 300=>500
 *
 * Revision 2.44  2012/05/07 14:42:57  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.43  2012/04/27 01:32:13  perev
 * Tracking now is not TPC only
 *
 * Revision 2.42  2012/04/16 20:22:16  ullrich
 * Changes necessary to add Fgt package.
 *
 * Revision 2.41  2012/02/21 18:46:21  perev
 * max numeber of detid ==40
 *
 * Revision 2.40  2012/02/21 16:25:47  fisyak
 * add KFVertexFinder
 *
 * Revision 2.39  2012/01/24 03:00:42  perev
 * Etr detector added
 *
 * Revision 2.38  2011/04/25 21:25:09  ullrich
 * Modifications to hold MTD data.
 *
 * Revision 2.37  2009/11/23 22:22:25  ullrich
 * Minor cleanup performed and hooks for RPS added.
 *
 * Revision 2.36  2009/04/28 13:36:32  ullrich
 * Updated PWG enumarations.
 *
 * Revision 2.35  2009/04/06 19:23:53  ullrich
 * Add detector Ids for FPD East/West and FMS.
 *
 * Revision 2.34  2007/10/11 21:50:19  ullrich
 * Added new enums for PXL and IST detectors.
 *
 * Revision 2.33  2007/02/22 20:44:13  ullrich
 * Added l2DisplacedVertex to StL2AlgorithmId.
 *
 * Revision 2.32  2006/08/15 14:32:58  ullrich
 * Added kHpdId.
 *
 * Revision 2.31  2006/05/10 15:13:17  jeromel
 * Additional enum
 *
 * Revision 2.30  2006/05/04 19:05:05  ullrich
 * Added StL2TriggerResultType.
 *
 * Revision 2.29  2006/04/26 14:41:22  jeromel
 * doxygenized
 *
 * Revision 2.28  2006/04/25 23:20:31  ullrich
 * Added StPrimaryVertexOrder.
 *
 * Revision 2.27  2006/03/22 20:57:19  ullrich
 * Added StL2AlgorithmId enumeration.
 *
 * Revision 2.26  2006/03/12 17:00:15  jeromel
 * Added ppvNoCtbVertexFinder to hopefully clarify analysis
 *
 * Revision 2.25  2006/01/19 22:30:05  jeromel
 * kMaxId -> kMaxDetectorId
 *
 * Revision 2.24  2006/01/19 21:51:26  ullrich
 * Added new RnD detectors.
 *
 * Revision 2.23  2005/08/17 21:57:29  perev
 * kMaxId == max number of detectors+1
 *
 * Revision 2.22  2005/06/15 21:46:21  ullrich
 * Added StVertexFinderId enum.
 *
 * Revision 2.21  2004/11/03 18:30:07  ullrich
 * Added definitions for FTPC calibration vertices.
 *
 * Revision 2.20  2004/10/20 18:51:56  ullrich
 * Add enumerations from StDedxMethod.h StDetectorId.h StTrackMethod.h StVertexId.h.
 *
 * Revision 2.19  2004/10/14 19:59:42  ullrich
 * Added enum for EMC crate status.
 *
 * Revision 2.18  2004/04/29 14:05:00  fisyak
 * Add track finding method Id defines (for StTrackMethod.inc and sync. with StEnumerations)
 *
 * Revision 2.17  2002/04/18 23:22:41  jeromel
 * Changes for SVT 2 tables scheme.
 *
 * Revision 2.16  2002/02/27 18:56:27  ullrich
 * Added enum for l3 track finder.
 *
 * Revision 2.15  2002/02/25 19:32:03  ullrich
 * Added enums for RHIC ring to StBeamDirection.
 *
 * Revision 2.14  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.13  2001/04/05 04:00:35  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.12  2001/03/14 02:33:37  ullrich
 * Fixed typo.
 *
 * Revision 2.11  2001/03/14 02:30:20  ullrich
 * Added StPwg enum.
 *
 * Revision 2.10  2000/11/26 15:07:55  lasiuk
 * replace multiplyassigned bit for RICH
 *
 * Revision 2.9  2000/11/25 11:48:40  lasiuk
 * Modify the StRichHitFlags to account for ambiguities
 *
 * Revision 2.8  2000/11/01 16:42:19  lasiuk
 * add StRichHitFlag for PID info
 *
 * Revision 2.7  2000/09/28 10:57:13  ullrich
 * Added enums related to RICH PID.
 *
 * Revision 2.6  2000/08/28 16:28:45  ullrich
 * Added enum tpt and removed CINT pragmas.
 *
 * Revision 2.5  2000/08/28 17:08:58  didenko
 * get back revision 2.2
 *
 * Revision 2.2  1999/12/01 15:58:05  ullrich
 * New decoding for dst_track::method. New enum added.
 *
 * Revision 2.1  1999/11/15 18:48:16  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.0  1999/10/12 18:41:45  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StEnumerations_hh
#define StEnumerations_hh

// These include files contain definitions that are also used in
// Fortran code. The orginal definitions are implemented via pre-
// processor #define statements. For compatibility reasons we have
// to leave it as is for now.
#include "Rtypes.h"
#include "StDetectorDefinitions.h"
#include "StDedxDefinitions.h"
#include "StVertexDefinitions.h"
#include "StTrackDefinitions.h"

/*!
 * \enum StBeamDirection
 */
enum StBeamDirection {east   = 0,
                      yellow = 0,    // yellow beam is going west -> east
                      west   = 1,
                      blue   = 1};   // blue beam is going east -> west

/*!
 * \enum StBeamPolarizationAxis
 */
enum StBeamPolarizationAxis {transverse, longitudinal};

/*!
 * \enum StChargeSign
 */
enum StChargeSign {negative, positive};

/*!
 * \enum StDetectorId
 */
enum StDetectorId {kUnknownId   = kUnknownIdentifier,
                   kTpcId       = kTpcIdentifier,
                   kSvtId       = kSvtIdentifier,
                   kRichId      = kRichIdentifier,
                   kFtpcWestId  = kFtpcWestIdentifier,
                   kFtpcEastId  = kFtpcEastIdentifier,
                   kTofId       = kTofIdentifier,
                   kBTofId      = kTofIdentifier,
                   kCtbId       = kCtbIdentifier,
                   kSsdId       = kSsdIdentifier,
                   kBarrelEmcTowerId     = kBarrelEmcTowerIdentifier,
                   kBarrelEmcPreShowerId = kBarrelEmcPreShowerIdentifier,
                   kBarrelSmdEtaStripId  = kBarrelSmdEtaStripIdentifier,
                   kBarrelSmdPhiStripId  = kBarrelSmdPhiStripIdentifier,
                   kEndcapEmcTowerId     = kEndcapEmcTowerIdentifier,
                   kEndcapEmcPreShowerId = kEndcapEmcPreShowerIdentifier,
                   kEndcapSmdUStripId    = kEndcapSmdUStripIdentifier,
                   kEndcapSmdVStripId    = kEndcapSmdVStripIdentifier,
                   kZdcWestId   = kZdcWestIdentifier,
                   kZdcEastId   = kZdcEastIdentifier,
                   kMwpcWestId  = kMwpcWestIdentifier,
                   kMwpcEastId  = kMwpcEastIdentifier,
                   kTpcSsdId    = kTpcSsdIdentifier,
                   kTpcSvtId    = kTpcSvtIdentifier,
                   kTpcSsdSvtId = kTpcSsdSvtIdentifier,
                   kSsdSvtId    = kSsdSvtIdentifier,
                   kPhmdCpvId   = kPhmdCpvIdentifier,
                   kPhmdId      = kPhmdIdentifier,
                   kPxlId       = kPxlIdentifier,
                   kIstId       = kIstIdentifier,
                   kFgtId       = kFgtIdentifier,
                   kEtrId       = kEtrIdentifier,
                   kFpdWestId   = kFpdWestIdentifier,
                   kFpdEastId   = kFpdEastIdentifier, 
                   kFmsId       = kFmsIdentifier,  
                   kRpsId       = kRpsIdentifier,
                   kMtdId       = kMtdIdentifier,
                   kSstId       = kSstIdentifier,
                   kGmtId       = kGmtIdentifier,
                   kFtsId       = kFtsIdentifier,
                   kiTpcId      = kiTpcIdentifier,
                   kETofId      = kETofIdentifier,
                   kMaxDetectorId = 41};

/*!
 * \enum StTrackType
 */
enum StTrackType {global, primary, tpt, secondary, estGlobal, estPrimary, massFit, massFitAtVx};

/*!
 * \enum StTrackModel
 */
enum StTrackModel {helixModel, kalmanModel};

/*!
 * \enum StTrackFinderMethod
 */
enum StTrackFinderMethod {svtGrouper    = ksvtGrouperIdentifier,
                          svtStk        = ksvtStkIdentifier,
                          svtOther      = ksvtOtherIdentifier,
                          tpcStandard   = ktpcStandardIdentifier,
                          tpcOther      = ktpcOtherIdentifier,
                          ftpcConformal = kftpcConformalIdentifier,
                          ftpcCurrent   = kftpcCurrentIdentifier,
                          svtTpcSvm     = ksvtTpcSvmIdentifier,
                          svtTpcEst     = ksvtTpcEstIdentifier,
                          svtTpcPattern = ksvtTpcPatternIdentifier,
                          l3Standard    = kl3StandardIdentifier
};

/*!
 * \enum StDedxMethod
 */
enum StDedxMethod {kUndefinedMethodId        = kUndefinedMethodIdentifier,
                  kTruncatedMeanId           = kTruncatedMeanIdentifier,
                  kEnsembleTruncatedMeanId   = kEnsembleTruncatedMeanIdentifier,
                  kLikelihoodFitId           = kLikelihoodFitIdentifier,
                  kWeightedTruncatedMeanId   = kWeightedTruncatedMeanIdentifier,
		  kOtherMethodId             = kOtherMethodIdentifier,
                  kOtherMethodId2            = kOtherMethodIdentifier2};

/*!
 * \enum StTrackFittingMethod
 */
enum StTrackFittingMethod {kUndefinedFitterId         = kUndefinedFitterIdentifier,
                           kHelix2StepId              = kHelix2StepIdentifier,
                           kHelix3DId                 = kHelix3DIdentifier,
                           kKalmanFitId               = kKalmanFitIdentifier,
                           kLine2StepId               = kLine2StepIdentifier,
                           kLine3DId                  = kLine3DIdentifier,
                           kL3FitId                   = kL3FitIdentifier,
                           kITKalmanFitId             = kITKalmanFitIdentifier};
/*!
  ETrackStatusBits
 */
enum ETrackStatusBits {
  kCtbMatched      = BIT(14),   // track has CTB hit match
  kToFMatched      = BIT(15),   // track has ToF hit match
  kCtbNotMatched   = BIT(16),   // track has CTB prediction but no hit to match (veto)
  kToFNotMatched   = BIT(17),   // track has ToF prediction but no hit to match (veto)
  kBemcMatched     = BIT(18),   // track has     Bemc          hit match
  kEemcMatched     = BIT(19),   // track has     Eemc          hit match
  kBemcNotMatched  = BIT(20),   // track has     Bemc          prediction but no hit to match (veto)
  kEemcNotMatched  = BIT(21),   // track has             Eemc  prediction but no hit to match (veto)
  kDecayTrack      = BIT(22),   // track fit to decay vertex (V0, ...) 
  kPromptTrack     = BIT(23),   // track has prompt TPC hit: |209.4 - |z|| < 3 cm
  kPostXTrack      = BIT(24),   // Post Crossing Track, track which has >= 2 hit with wrong Z / Sector 
  kXMembrane       = BIT(25),   // track which has >= 2 hits from both sides of Membrane (z = 0)
  kShortTrack2EMC  = BIT(26),   // Short track pointing to EEMC
  kRejectedTrack   = BIT(27),   // track rejected by small no. of fit points (< 10)
  kWestTpcOnlyTrack= BIT(28),   // track has hits only from West Tpc (sector <= 12)
  kEastTpcOnlyTrack= BIT(29),   // track has hits only from East Tpc (sector >  12)
  kShortTrack2ETOF = BIT(30),   // Short track pointing to ETOF
  kEmcEnergyByte   =       0    // bits  0- 2: 1 ==> 0.3 < energy <=  0.6 Mip	 
  //                                           2 ==> 0.6 < energy <=  1.0 Hadron 
  // 	                                       3 ==> 1.0 < energy <=  4.0 Electon
  // 	                                       4 ==> 4.0 < energy <= 10.0 Tower	 
  // 	                                       5 ==>10.0 < energy <= 20.0 W	 
  // 	                                       6 ==>20.0 < energy <=100.0 Z	 
  // 	                                       7 ==>       energy > 100.0 ?       
};			     
/*!
 * \enum StVertexId
 */
enum StVertexId {kUndefinedVtxId   = kUndefinedVertexIdentifier,
                 kEventVtxId       = kEventVertexIdentifier,
		         kPrimaryVtxId     = kEventVertexIdentifier,
                 kV0VtxId          = kV0DecayIdentifier,
                 kXiVtxId          = kXiDecayIdentifier,
                 kKinkVtxId        = kKinkDecayIdentifier,
                 kOtherVtxId       = kOtherTypeIdentifier,
                 kFtpcEastCalVtxId = kFtpcEastCalibrationVertexIdentifier,
                 kFtpcWestCalVtxId = kFtpcWestCalibrationVertexIdentifier,
		         kBEAMConstrVtxId,
                 kRejectedVtxId};

/*!
 * \enum StRichPidFlag
 */
enum StRichPidFlag {eNoMip = 1,                                        /**< enum value eNoMip  */
                    eFastEnough = 2,                                   /**< enum value eFastEnough  */
                    eLightOnPadPlane = 4};

/*!
 * \enum StRichHitFlag
 */
enum StRichHitFlag {eDeconvoluted=1,                                   /**< enum value eDeconvoluted */
                    eMip=2,                                            /**< enum value eMip */
                    eSaturatedPad=4 ,                                  /**< enum value eSaturatedPad */
                    ePhotoElectron=8,                                  /**< enum value ePhotoElectron */
                    eAssociatedMip=16,                                 /**< enum value eAssociatedMip */
                    eMultiplyAssigned=32,                              /**< enum value eMultiplyAssigned */
                    eInAreaPi=64,                                      /**< enum value eInAreaPi */
                    eInAreaK=128,                                      /**< enum value eInAreaK */
                    eInAreap=256,                                      /**< enum value eInAreap */
                    eInConstantAnglePi=512,                            /**< enum value eInConstantAnglePi */
                    eInConstantAngleK=1024,                            /**< enum value eInConstantAngleK */
                    eInConstantAnglep=2048,                            /**< enum value eInConstantAnglep */
                    eInConstantAreaPi=4096,                            /**< enum value eInConstantAreaPi */
                    eInConstantAreaK=8192,                             /**< enum value eInConstantAreaK */
                    eInConstantAreap=16384,                            /**< enum value eInConstantAreap */
                    eInMultipleAreaPi=32768,                           /**< enum value eInMultipleAreaPi */
                    eInMultipleAreaK=65536,                            /**< enum value eInMultipleAreaK */
                    eInMultipleAreap=131072,                           /**< enum value eInMultipleAreap */
                    eInMultipleCAnglePi=262144,                        /**< enum value eInMultipleCAnglePi */
                    eInMultipleCAngleK=524288,                         /**< enum value eInMultipleCAngleK */
                    eInMultipleCAnglep=1048576,                        /**< enum value eInMultipleCAnglep */
                    eInMultipleCAreaPi=2097152,                        /**< enum value eInMultipleCAreaPi */
                    eInMultipleCAreaK=4194304,                         /**< enum value eInMultipleCAreaK */
                    eInMultipleCAreap=8388608,                         /**< enum value eInMultipleCAreap */
                    e1SigmaPi=16777216,                                /**< enum value e1SigmaPi */
                    e1SigmaK=33554432,                                 /**< enum value e1SigmaK */
                    e1Sigmap=67108864,                                 /**< enum value e1Sigmap */
                    e2SigmaPi=134217728,                               /**< enum value e2SigmaPi */
                    e2SigmaK=268435456,                                /**< enum value e2SigmaK */
                    e2Sigmap=536870912};

/*!
 * \enum StPwg
 */
enum StPwg         {generic,                                           /**< enum value generic */
                    ebye,                                              /**< enum value for ebye PWG (obsolete) */
                    hbt,                                               /**< enum value for hbt PWG (obsolete) */
                    highpt,                                            /**< enum value for highpt PWG (obsolete) */
                    pcoll,                                             /**< enum value for peripheral collision PWG */
                    upc = pcoll,                                       /**< alias for pcoll */
                    spectra,                                           /**< enum value for spectra PWG (obsolete) */
                    spin,                                              /**< enum value for spin PWG */
                    strangeness,                                       /**< enum value for strangeness PWG (obsolete) */
                    heavy,                                             /**< enum value for heavy flavor PWG */
                    bulkcorr,                                          /**< enum value for bulk correlation PWG */
                    jetcorr,                                           /**< enum value for jet correlation PWG */
                    lfspectra};                                        /**< enum value for light flavor spectra PWG */

/*!
 * \enum StEmcCrateStatus
 */
enum StEmcCrateStatus {crateUnknown       = 0,
                       crateNotPresent    = 1,
                       crateOK            = 2,
                       crateHeaderCorrupt = 3};

/*!
 * \enum StarMaxSize
 */
// maximal sizes of tracking part of STAR in cm (Victor)
enum StarMaxTrackRangeSize {kStarMaxTrackRangeR =  500, // including MTD
                            kStarMaxTrackRangeZ =  500,
                            kStarMinTrackRangeZ = -kStarMaxTrackRangeZ};

/*!
 * \enum StVertexFinderId
 */
enum StVertexFinderId { undefinedVertexFinder = 0,
                        lmvVertexFinder,
                        pplmvVertexFinder,
                        egrVertexFinder,
                        minuitVertexFinder,
                        ppvVertexFinder,
                        ppvNoCtbVertexFinder,
		    	        mcEventVertexFFinder,
			            KFVertexFinder};


/*!
 * \enum StL2AlgorithmId
 */
enum StL2AlgorithmId { l2Diagnostic = 0,
                       l2EmcCheck,
                       l2Jpsi,
                       l2Upsilon,
                       l2Dijet,
                       l2EmcPedestal,
                       l2Pi0Gamma,
                       l2DisplacedVertex};

/*!
 * \enum StPrimaryVertexOrder
 */
enum StPrimaryVertexOrder {
    orderByNumberOfDaughters = 0, /**< enum value for sorting based on NumberOfDaughters (default)  */
    orderByRanking                /**< enum value to switch ordering based on assigned rank         */
};


/*!
 * \enum StL2TriggerResultType
 */
enum StL2TriggerResultType {
    l2Trg2006BEMCGammaPi = 0,
    l2Trg2006BEMCGammaPiRandom,
    l2Trg2006EEMCGammaPi,
    l2Trg2006EEMCGammaPiRandom,
    l2Trg2006MonoJet,
    l2Trg2006DiJet,
    l2Trg2006RandomJet
};

/*!
 * \enum StFgtElecConsts
 */
// constants related to electric coordinates
enum StFgtElecConsts { 
    kFgtNumRdos = 2,                    // rdo in {1,2}
    kFgtNumArms = 6,                    // arm in 0-5, though 5 not used in run12.
    kFgtNumChannels = 128,              // channel in 0-127
    kFgtApvsPerAssembly = 12,           //
    kFgtMaxApvId=kFgtApvsPerAssembly*2, // covers 0-23 
    kFgtApvGap = 2,                     // i.e. apvs 10 & 11
    kFgtApvsPerOct = 5,
    kFgtApvsPerQuad = 10,
    kFgtApvsPerArm = 20,
    kFgtNumElecIds = kFgtNumChannels * kFgtApvsPerArm * kFgtNumArms * kFgtNumRdos  // elec id in 0 to kFgtNumElecIds-1
};

/*!
 * \enum StFgtPhysConsts
 */
// constants related to physical coordinates
enum StFgtPhysConsts {
    kFgtNumDiscs = 6,
    kFgtNumQuads = 4,
    kFgtNumOctantsPerDisc = 8,
    kFgtNumOctants = kFgtNumOctantsPerDisc*kFgtNumDiscs,
    kFgtNumLayers = 2,
    kFgtNumStrips = 720,
    kFgtNumGeoIds = kFgtNumQuads * kFgtNumDiscs * kFgtNumLayers * kFgtNumStrips,   // geoId in 0 to kFgtNumGeoIds-1
    kFgtNumPstripsPerOctant = 360,
    kFgtNumRstripsPerOctant = 280,
    kFgtLowerStripOctant = 'L',    // i.e. a strip is in octant "kFgtLowerStripOctant" if
    kFgtHigherStripOctant = 'S',   // the strip index is below the number of strips per octant
    // for that layer
    kFgtNumStripsPerDisc = kFgtNumQuads  * kFgtNumLayers * kFgtNumStrips // includes both planes, geoId for given disc will not exceed this range after common disc-offset is subtracted 
};

/*!
 * \enum StFgtGeneralConsts
 */
// unsorted constants
enum StFgtGeneralConsts {
    kFgtNumTimeBins = 15,
    kFgtMaxAdc = 4096,
};

/*!
 * \enum StFgtClusterSeed Type
 */
// cluster seed types
enum StFgtClusterSeedType {
    kFgtSeedTypeNo,
    kFgtDeadStrip,
    kFgtSeedType1,
    kFgtSeedType2,
    kFgtSeedType3,
    kFgtSeedType4,
    kFgtSeedType5,
    kFgtSeedTypeMax,
    kFgtClusterPart,
    kFgtNextToDeadGuy,
    kFgtClusterEndUp,
    kFgtClusterEndDown,
    kFgtStripShared,
    kFgtClusterTooBig,
    kFgtClusterSeedInSeaOfNoise,
    kFgtNextToCluster,
    kFgtKeepStrip
};

/** FPS/FMS/FHC/FPS/FPost detectorId */
enum StFmsDetectorId{
    kFpdNorthDetId=0,
    kFpdSouthDetId=1,
    kFpdNorthPrsDetId=2,
    kFpdSouthPrsDetId=3,
    kFpdNorthSMDVDetId=4,
    kFpdSouthSMDVDetId=5,
    kFpdNorthSMDHDetId=6,
    kFpdSouthSMDHDetId=7,
    kFmsNorthLargeDetId=8,
    kFmsSouthLargeDetId=9,
    kFmsNorthSmallDetId=10,
    kFmsSouthSmallDetId=11,
    kFhcNorthDetId=12,
    kFhcSouthDetId=13,
    kFpsDetId=14,
    kFpostDetId=15
};

/** Categorization of a FMS tower cluster as 1- or 2-photon, or "not sure". */
enum StFmsClusterCategory {
    kAmbiguousCluster = 0,  ///< Could be 1- or 2-photon, needs to be fitted
    k1PhotonCluster = 1,  ///< A cluster created by 1 photon
    k2PhotonCluster = 2,  ///< A cluster created by 2 photons
    kInvalidClusterCategory  ///< For default/unknown/error value
};  // enum StFmsClusterCategory

/** FPS basic constants */
enum StFpsConstants {
    kFpsNQuad=4,
    kFpsNLayer=3,
    kFpsNSlat=21,
    kFpsMaxSlat=252,
    kFpsNCandidate=4
};

/** FPost basic constants */
enum StFpostConstants {
    kFpostNQuad=2,
    kFpostNLayer=5,
    kFpostNSlat=43,
    kFpostMaxSlat=241,
    kFpostNCandidate=6
};

enum StRHICfConstants{
    kRHICfNtower=2,     // 0=small, 1=large                                                                                    
    kRHICfNplate=16,    // longitudinal segmentations      
    kRHICfNlayer=4,     // bar layers (longitudinal)                                                                           
    kRHICfNxy=2,        // 0=x, 1=y                                                                                            
    kRHICfNbarSmall=20, // bar# (0-19 for small)                                                                               
    kRHICfNbarLarge=40, // bar# (0-39 for large)
    kRHICfNcad=10,      // Local counts for DAQ
    kRHICfNtdc=256,     // TDC
    kRHICfNgpio=54      // Flags                                
};

/** For more IST related constants see StRoot/StIstUtil/StistConsts.h */
namespace StIstConsts
{
    const int kIstNumLadders = 24;          ///< 24 IST Ladders
    const int kIstNumSensorsPerLadder = 6;  ///< 6 sensor per one IST Ladder
    const int kIstNumSensors = 144;         ///< 144 sensors
    const int kIstNumPadsPerSensor = 768;   ///< 768 pads in each sensor
    const int kIstNumRowsPerSensor = 64;    ///< 64 rows in r-phi direction per each sensor
    const int kIstNumColumnsPerSensor = 12; ///< 12 columns in beam direction per each sensor
}

/*!
 * \enum StGmtElecConsts
 */
// constants related to electric coordinates
enum StGmtElecConsts {
    kGmtNumRdos = 1,                    // rdo in {1}
    kGmtNumArms = 2,                    // 1 arm in 2 halves (or groups) {0-1}
    kGmtNumChannels = 128,              // channel in 0-127
    kGmtApvsPerAssembly = 2,           //
    kGmtMaxApvId= 15,                  // we only use ARM channels 0-3 and 12-15
    kGmtApvGap = 8,                     // i.e. we use 0-3 and then 12-15
    kGmtApvsPerArm = 24, 
    kGmtNumElecIds = kGmtNumChannels * kGmtApvsPerArm * kGmtNumArms * kGmtNumRdos  // elec id in 0 to kGmtNumElecIds-1
};

/*!
 * \enum StGmtPhysConsts
 */
// constants related to physical coordinates
enum StGmtPhysConsts {
    kGmtNumModules = 8,
    kGmtNumLayers = 2,
    kGmtNumStrips = 128,  // one X layer and one Y layer
    kGmtNumConnectedStripsX = 126,  // 2 missing in X, 3 in Y
    kGmtNumConnectedStripsY = 125,  // 2 missing in X, 3 in Y
    kGmtNumGeoIds = kGmtNumModules * kGmtNumLayers * kGmtNumStrips,   // geoId in 0 to kGmtNumGeoIds-1
    kGmtNumPstripsPerModule = 128,  // "pads" which are the Local Y coordinates
    kGmtNumSstripsPerModule = 128,  // "strips" which are the Local X coordinates
    // for that layer
    kGmtNumStripsPerModule = kGmtNumStrips * kGmtNumLayers // accounts for X (strip) and Y (pad) layers
};

/*!
 * \enum StGmtGeneralConsts
 */
// unsorted constants
enum StGmtGeneralConsts {
    kGmtNumTimeBins = 15,
    kGmtNumTimeBinsForPed = 3,  // number of time bins to use for pedestal determination
    kGmtMaxAdc = 4096,
    kGmtPedCut = 3,   // pedestal subtracted threshold to temporarily tag a "hit" for 2nd pedestal pass
    kGmtHitCut = 5 //HERE 5  
};

const char *detectorNameById(StDetectorId id);
StDetectorId detectorIdByName(const char *name);
#endif
