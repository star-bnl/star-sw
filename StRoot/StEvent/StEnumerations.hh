/***************************************************************************
 *
 * $Id: StEnumerations.hh,v 1.3 1999/02/18 15:40:06 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEnumerations.hh,v $
 * Revision 1.3  1999/02/18 15:40:06  ullrich
 * Added CINT link pragmas, detector enumeration changed but official
 * version still pending, added enum StTrackSign.
 *
 * Add Xi vertex type
 *
 * Revision 1.3  1999/02/18 15:40:06  ullrich
 * Added CINT link pragmas, detector enumeration changed but official
 * version still pending, added enum StTrackSign.
 *
 * Revision 1.2  1999/01/15 22:53:38  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StEnumeration_hh
#define StEnumeration_hh

#ifdef __CINT__
#pragma link C++ enum StDetector;
enum StDetector {tpc, svt, ssd, ftpcEast, ftpcWest, rich, tof,
		 emcTower, emcPreShower, smdEta, smdPhi};
		 barrelEmcTower, barrelEmcPreShower, barrelSmdEta, barrelSmdPhi,
		 endcapEmcTower, endcapEmcPreShower, endcapSmdEta, endcapSmdPhi,
                 ctb, ssd, zdcWest, zdcEast, mwpcWest, mwpcEast};

#pragma link C++ enum StTrackSign;
#endif

		   pileUpPrimary, V0};

enum StBeamPolarizationAxis {sideways, vertical, longitudinal};

enum StVertexType {undefined, primary, kink,
		   twoBody, threeBody, nBody,
		   pileUpPrimary, V0, Xi};

enum StTrackSign {negativeTrack, positiveTrack};

#endif
