/***************************************************************************
 *
 * $Id: StEnumerations.h,v 2.10 2000/11/26 15:07:55 lasiuk Exp $
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

enum StBeamDirection {east, west};

enum StBeamPolarizationAxis {transverse, longitudinal};

enum StChargeSign {negative, positive};

enum StTrackType {global, primary, tpt, secondary};

enum StTrackModel {helixModel, kalmanModel};

enum StTrackFinderMethod { svtGrouper = 4,
			   svtStk,
			   svtOther, 
			   tpcStandard,
			   tpcOther,
			   ftpcConformal,
			   svtTpcSvm,
			   svtTpcEst,
			   svtTpcPattern };

enum StRichPidFlag {eNoMip = 1,
		    eFastEnough = 2,
		    eLightOnPadPlane = 4};

enum StRichHitFlag {eDeconvoluted=1,
		    eMip=2,
		    eSaturatedPad=4 ,
		    ePhotoElectron=8,
		    eAssociatedMip=16,
		    eMultiplyAssigned=32,
		    eInAreaPi=64,
		    eInAreaK=128,
		    eInAreap=256,
		    eInConstantAnglePi=512,
		    eInConstantAngleK=1024,
		    eInConstantAnglep=2048,
		    eInConstantAreaPi=4096,
		    eInConstantAreaK=8192,
		    eInConstantAreap=16384,
		    eInMultipleAreaPi=32768,
                    eInMultipleAreaK=65536,
                    eInMultipleAreap=131072,
                    eInMultipleCAnglePi=262144,
                    eInMultipleCAngleK=524288,
                    eInMultipleCAnglep=1048576,
                    eInMultipleCAreaPi=2097152,
                    eInMultipleCAreaK=4194304,
                    eInMultipleCAreap=8388608,
                    e1SigmaPi=16777216,
                    e1SigmaK=33554432,
                    e1Sigmap=67108864,
                    e2SigmaPi=134217728,
                    e2SigmaK=268435456,
                    e2Sigmap=536870912};
#endif
