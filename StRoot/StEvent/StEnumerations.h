/***************************************************************************
 *
 * $Id: StEnumerations.h,v 2.2 1999/12/01 15:58:05 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEnumerations.h,v $
 * Revision 2.2  1999/12/01 15:58:05  ullrich
 * New decoding for dst_track::method. New enum added.
 *
 * Revision 2.3  2000/08/17 00:12:18  ullrich
 * Added tpt to enum StTrackType
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
#ifdef __CINT__
#pragma link C++ enum StBeamDirection;
#pragma link C++ enum StBeamPolarizationAxis;
#pragma link C++ enum StChargeSign;
#pragma link C++ enum StDetectorId;
#pragma link C++ enum StVertexId;
#pragma link C++ enum StTrackType;
#pragma link C++ enum StTrackModel;
#pragma link C++ enum StDedxMethod;
#pragma link C++ enum StTrackFinderMethod;
#pragma link C++ enum StTrackFittingMethod;
#endif

#pragma link C++ enum StTrackFittingMethod;
#endif

 * New decoding for dst_track::method. New enum added.
 *
 * Revision 2.1  1999/11/15 18:48:16  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
enum StTrackType {global, primary, secondary};
 * Completely Revised for New Version
 *
enum StTrackType {global, primary, tpt, secondary};
#ifndef StEnumerations_hh
#define StEnumerations_hh
enum StTrackType {global, primary, secondary};
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
			   svtTpcSvm,
			   svtTpcEst,

		    eInConstantAreap=262144,
		    eInAreap=524288,
		    eAssignedToRingp=1048576};
                    e2SigmaPi=134217728,
                    e2SigmaK=268435456,
                    e2Sigmap=536870912};
#endif
