/***************************************************************************
 *
 * $Id: StMcContainers.cc,v 2.0 1999/11/17 02:00:58 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Containers for StMcEvent objects
 *
 ***************************************************************************
 *
 * $Log: StMcContainers.cc,v $
 * Revision 2.0  1999/11/17 02:00:58  calderon
 * Completely revised for new StEvent
 *
 **************************************************************************/
#ifdef PERSISTENT

#include "StMcContainers.h"
#include "StMcSvtHit.h"
#include "StMcTpcHit.h"
#include "StMcFtpcHit.h"
#include "StMcTrack.h"
#include "StMcVertex.h"

StCollectionImp(McHit)
StCollectionImp(McSvtHit)
StCollectionImp(McTpcHit)
StCollectionImp(McFtpcHit)
StCollectionImp(McTrack)
StCollectionImp(McVertex)

#endif
