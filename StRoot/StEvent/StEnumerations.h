/***************************************************************************
 *
 * $Id: StEnumerations.h,v 2.8 2000/11/01 16:42:19 lasiuk Exp $
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
 * Revision 2.8  2000/11/01 16:42:19  lasiuk
 * add StRichHitFlag for PID info
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
		    eMultiplyAssignedToRing=16,
		    eAssociatedMip=32,
		    e1SigmaPi=64,
		    e2SigmaPi=128,
		    eInConstantAreaPi=256,
		    eInAreaPi=512,
		    eAssignedToRingPi=1024,
		    e1SigmaK=2048,
		    e2SigmaK=4096,
		    eInConstantAngleK=1024,
		    eInAreaK=16384,
		    eAssignedToRingK=32768,
		    e1Sigmap=65536,
		    e2Sigmap=131072,
		    eInConstantAreap=262144,
		    eInAreap=524288,
		    eAssignedToRingp=1048576};
                    e2SigmaPi=134217728,
                    e2SigmaK=268435456,
                    e2Sigmap=536870912};
#endif
