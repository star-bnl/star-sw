/***************************************************************************
 *
 * $Id: StEnumerations.h,v 1.2 1999/02/09 20:01:42 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEnumerations.h,v $
 * Revision 1.2  1999/02/09 20:01:42  fisyak
 * Import new Torre staff
 *
 * Added CINT link pragmas, detector enumeration changed but official
 * version still pending, added enum StTrackSign.
 *
 * Revision 1.2  1999/01/15 22:53:38  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.3  2000/08/17 00:12:18  ullrich
enum StDetector {tpc, svt, emc, smd, ftpc, tof, rich};
#pragma link C++ enum StVertexType;
#pragma link C++ enum StTrackSign;
 * Revision 2.6  2000/08/28 16:28:45  ullrich
 * Added enum tpt and removed CINT pragmas.
#pragma link C++ enum StDetectorId;
#pragma link C++ enum StVertexId;
enum StBeamPolarizationAxis {sideways, vertical, longitudinal};
		   pileUpPrimary, V0};
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
