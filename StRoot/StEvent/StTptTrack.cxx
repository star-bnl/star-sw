/***************************************************************************
 *
 * $Id: StTptTrack.cxx,v 2.4 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, Aug 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTptTrack.cxx,v $
 * Revision 2.4  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.3  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.2  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2000/08/17 00:10:34  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTptTrack.h"
#include "StVertex.h"

ClassImp(StTptTrack)

static const char rcsid[] = "$Id: StTptTrack.cxx,v 2.4 2009/11/23 16:34:07 fisyak Exp $";

StTptTrack::StTptTrack() {/* noop */}

StTptTrack::~StTptTrack() {/* noop */}

StTrackType
StTptTrack::type() const { return tpt; }

const StVertex*
StTptTrack::vertex() const { return 0; }
