/*!
 * \file StEnumerations.h
 */
/***************************************************************************
 *
 * $Id: StEnumerations.h,v 2.41 2012/02/21 18:46:21 perev Exp $
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
                   kMaxDetectorId = 40};

/*!
 * \enum StTrackType
 */
enum StTrackType {global, primary, tpt, secondary, estGlobal, estPrimary};

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
                  kOtherMethodId             = kOtherMethodIdentifier};

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
 * \enum StVertexId
 */
enum StVertexId {kUndefinedVtxId   = kUndefinedVertexIdentifier,
                 kEventVtxId       = kEventVertexIdentifier,
                 kV0VtxId          = kV0DecayIdentifier,
                 kXiVtxId          = kXiDecayIdentifier,
                 kKinkVtxId        = kKinkDecayIdentifier,
                 kOtherVtxId       = kOtherTypeIdentifier,
                 kFtpcEastCalVtxId = kFtpcEastCalibrationVertexIdentifier,
                 kFtpcWestCalVtxId = kFtpcWestCalibrationVertexIdentifier};

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
enum StarMaxTrackRangeSize {kStarMaxTrackRangeR = 200,
                            kStarMaxTrackRangeZ = 269};


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
enum StL2TriggerResultType {	l2Trg2006BEMCGammaPi = 0,
		         	l2Trg2006BEMCGammaPiRandom,
		         	l2Trg2006EEMCGammaPi,
		         	l2Trg2006EEMCGammaPiRandom,
		         	l2Trg2006MonoJet,
		         	l2Trg2006DiJet,
		         	l2Trg2006RandomJet};


const char *detectorNameById(StDetectorId id);
StDetectorId detectorIdByName(const char *name);
#endif
