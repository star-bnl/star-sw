/*!
 * \file StEnumerations.h
 *
 * Enumerations for StEvent.
 * Note that lots of code depend on this file and
 * any change will force a recompilation of almost
 * all of StEvent.
 */
/***************************************************************************
 *
 * $Id: StEnumerations.h,v 2.15 2002/02/25 19:32:03 ullrich Exp $
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

#include "StDetectorId.h"
#include "StVertexId.h"
#include "StDedxMethod.h"
#include "StTrackMethod.h"

/*!
 * \enum StBeamDirection 
 */
enum StBeamDirection {east = 0,
		      yellow = 0,  // yellow beam is going west -> east
		      west = 1,
		      blue = 1};   // blue beam is going east -> west

/*!
 * \enum StBeamPolarizationAxis 
 */
enum StBeamPolarizationAxis {transverse, longitudinal};

/*!
 * \enum StChargeSign 
 */
enum StChargeSign {negative, positive};

/*!
 * \enum StTrackType 
 */
enum StTrackType {global, primary, tpt, secondary};

/*!
 * \enum StTrackModel 
 */
enum StTrackModel {helixModel, kalmanModel};

/*!
 * \enum StTrackFinderMethod 
 */
enum StTrackFinderMethod { svtGrouper = 4,                             /**< enum value svtGrouper  */
                           svtStk,                                     /**< enum value svtStk */
                           svtOther,                                   /**< enum value svtOther */
                           tpcStandard,                                /**< enum value tpcStandard */
                           tpcOther,                                   /**< enum value tpcOther */
                           ftpcConformal,                              /**< enum value ftpcConformal */
                           svtTpcSvm,                                  /**< enum value svtTpcSvm */
                           svtTpcEst,                                  /**< enum value svtTpcEst */
                           svtTpcPattern };

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
                    ebye,                                              /**< enum value ebye */
                    hbt,                                               /**< enum value hbt */
                    highpt,                                            /**< enum value highpt */
                    pcoll,                                             /**< enum value pcoll */
                    spectra,                                           /**< enum value spectra */
                    spin,                                              /**< enum value spin */
                    strangeness};
#endif
