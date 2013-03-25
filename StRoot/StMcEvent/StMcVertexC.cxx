/***************************************************************************
 *
 * $Id: StMcVertexC.cxx,v 2.2 2013/03/25 23:48:05 perev Exp $
 *
 **************************************************************************/
#include <algorithm>
#ifndef ST_NO_NAMESPACES
using std::find;
#endif

#include "StMcVertexC.h"
#include "StMcTrack.hh"

static const char rcsid[] = "$Id: StMcVertexC.cxx,v 2.2 2013/03/25 23:48:05 perev Exp $";

int StMcVertexC::operator==(const StMcVertexC& v) const
{
    return (geantProcess() == v.geantProcess() &&
            position()     == v.position()     &&
            tof()          == v.tof()          &&
            key()          == v.key()
          ) ;
}

