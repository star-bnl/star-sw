/***************************************************************************
 *
 * $Id: StEnumerations.h,v 1.4 1999/04/28 22:27:31 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEnumerations.h,v $
 * Revision 1.4  1999/04/28 22:27:31  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.4  1999/04/28 22:27:31  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.7  1999/02/26 14:02:26  ullrich
 * Changed pragmas.
 *
 * Revision 1.6  1999/02/26 14:01:17  ullrich
 * Removed enumeration StDetector (handled elsewhere).
 *
 * Revision 1.5  1999/02/24 02:58:30  ullrich
 * StDetector now reflects official STAR detector numbers.
 *
 * Revision 1.4  1999/02/24 01:55:34  genevb
 * Add Xi vertex type
 *
 * Revision 1.3  1999/02/18 15:40:06  ullrich
 * Added CINT link pragmas, detector enumeration changed but official
 * version still pending, added enum StTrackSign.
 *
 * Revision 1.2  1999/01/15 22:53:38  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.3  2000/08/17 00:12:18  ullrich
#ifndef StEnumeration_hh
#define StEnumeration_hh
 * Revision 2.9  2000/11/25 11:48:40  lasiuk
 * Modify the StRichHitFlags to account for ambiguities
 *
 * Revision 2.8  2000/11/01 16:42:19  lasiuk
#pragma link C++ enum StVertexType;
#pragma link C++ enum StTrackSign;
 * Revision 2.6  2000/08/28 16:28:45  ullrich
 * Added enum tpt and removed CINT pragmas.
#pragma link C++ enum StDetectorId;
#pragma link C++ enum StVertexId;
enum StBeamPolarizationAxis {sideways, vertical, longitudinal};
#endif
enum StVertexType {undefined, primary, kink,
		   twoBody, threeBody, nBody,
		   pileUpPrimary, V0, Xi};
#pragma link C++ enum StTrackFittingMethod;
enum StTrackSign {negativeTrack, positiveTrack};

 * New decoding for dst_track::method. New enum added.
 *
 * Revision 2.1  1999/11/15 18:48:16  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
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
