/***************************************************************************
 *
 * $Id: StTrackGeometry.cxx,v 2.5 2004/10/20 18:55:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackGeometry.cxx,v $
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
#include "tables/St_dst_track_Table.h"
#include "StPhysicalHelixD.hh"

ClassImp(StTrackGeometry)

static const char rcsid[] = "$Id: StTrackGeometry.cxx,v 2.5 2004/10/20 18:55:13 ullrich Exp $";

StTrackGeometry::StTrackGeometry() {/* noop */}

StTrackGeometry::StTrackGeometry(const dst_track_st&)   {/* noop */}

StTrackGeometry::~StTrackGeometry() { /* noop */ }

int StTrackGeometry::bad() const
{
  StPhysicalHelixD hh = helix();
  int ierr=hh.bad();
  if (ierr) 				 return  1+100*ierr;
  if (fabs(hh.origin().z())  >kStarMaxTrackRangeZ) return 21;
  if (     hh.origin().perp()>kStarMaxTrackRangeR) return 31;
  return 0;
}
