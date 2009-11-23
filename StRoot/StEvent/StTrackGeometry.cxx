/***************************************************************************
 *
 * $Id: StTrackGeometry.cxx,v 2.7 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackGeometry.cxx,v $
 * Revision 2.7  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.6  2005/02/05 01:01:14  perev
 * test for zero momentum added
 *
 * Revision 2.5  2004/10/20 18:55:13  ullrich
 * Name of enum changed: StStarMaxR(Z) now StStarMaxTrackRangeR(Z).
 *
 * Revision 2.4  2004/10/17 03:35:10  perev
 * Error check improved
 *
 * Revision 2.3  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.2  1999/10/28 22:27:38  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:41  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StDetectorId.h"
#include "StTrackGeometry.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"

ClassImp(StTrackGeometry)

static const char rcsid[] = "$Id: StTrackGeometry.cxx,v 2.7 2009/11/23 16:34:07 fisyak Exp $";

StTrackGeometry::StTrackGeometry() {/* noop */}

StTrackGeometry::~StTrackGeometry() { /* noop */ }

int StTrackGeometry::bad() const
{
  StPhysicalHelixD hh = helix();
  int ierr=hh.bad();
  if (ierr) 				 return  1+100*ierr;
  if (fabs(hh.origin().z())  >kStarMaxTrackRangeZ) return 21;
  if (     hh.origin().perp()>kStarMaxTrackRangeR) return 31;
  if (momentum().mag() < 1.e-5                   ) return 41;
  return 0;
}
