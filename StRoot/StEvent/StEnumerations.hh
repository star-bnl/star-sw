/***************************************************************************
 *
 * $Id: StEnumerations.hh,v 1.5 1999/02/24 02:58:30 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEnumerations.hh,v $
 * Revision 1.5  1999/02/24 02:58:30  ullrich
 * StDetector now reflects official STAR detector numbers.
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
 **************************************************************************/
#ifndef StEnumeration_hh
#define StEnumeration_hh

#ifdef __CINT__
#pragma link C++ enum StDetector;
#pragma link C++ enum StBeamDirection;
#pragma link C++ enum StBeamPolarizationAxis;
#pragma link C++ enum StVertexType;
enum StDetector {tpc = 1, svt, ftpcWest, ftpcEast, rich, tof,
		 barrelEmcTower, barrelEmcPreShower, barrelSmdEta, barrelSmdPhi,
		 endcapEmcTower, endcapEmcPreShower, endcapSmdEta, endcapSmdPhi,
                 ctb, ssd, zdcWest, zdcEast, mwpcWest, mwpcEast};

#pragma link C++ enum StTrackSign;
#endif

enum StBeamDirection {east, west};

enum StBeamPolarizationAxis {sideways, vertical, longitudinal};

enum StVertexType {undefined, primary, kink,
		   twoBody, threeBody, nBody,
		   pileUpPrimary, V0, Xi};

enum StTrackSign {negativeTrack, positiveTrack};

#endif
