/***************************************************************************
 *
 * $Id: StTrackGeometry.cxx,v 2.3 2003/10/30 20:07:32 perev Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackGeometry.cxx,v $
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
#include "StTrackGeometry.h"
#include "tables/St_dst_track_Table.h"
#include "StPhysicalHelixD.hh"

ClassImp(StTrackGeometry)

static const char rcsid[] = "$Id: StTrackGeometry.cxx,v 2.3 2003/10/30 20:07:32 perev Exp $";

StTrackGeometry::StTrackGeometry() {/* noop */}

StTrackGeometry::StTrackGeometry(const dst_track_st&)   {/* noop */}

StTrackGeometry::~StTrackGeometry() { /* noop */ }

int StTrackGeometry::bad() const
{
  StPhysicalHelixD hh = helix();
  if (!hh.valid()) return 1;
  return 0;	
}
