/***************************************************************************
 *
 * $Id: StContainers.cxx,v 2.1 1999/10/28 22:06:16 ullrich Exp $
 *
 * Author: Thomas Ullrich, Oct 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StContainers.cxx,v $
 * Revision 2.1  1999/10/28 22:06:16  ullrich
 * Initial Revision
 *
 * Revision 2.5  2000/05/22 21:42:41  ullrich
 * Add RICH related classes.
 *
 * Revision 2.4  2000/03/23 22:24:06  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 * Changes due to the addition of the EMC to StEvent
 *
 * Revision 2.2  2000/01/05 16:02:20  ullrich
 * SSD hits added to StEvent.
 *
#include "StTpcHit.h"
#include "StRichHit.h"
#include "StRichPixel.h"
#include "StRichPid.h"
#include "StRichCluster.h"
#include "StRichMCInfo.h"
#include "StEmcRawHit.h"
#include "StEmcCluster.h"
#include "StEmcPoint.h"
#include "StPrimaryTrack.h"
#include "StTrackNode.h"
#include "StTrackDetectorInfo.h"
#include "StV0Vertex.h"
#include "StXiVertex.h"
#include "StKinkVertex.h"
StCollectionImp(TpcHit)
StCollectionImp(FtpcHit)
StCollectionImp(RichCluster)
StCollectionImp(RichPixel)
StCollectionImp(RichPid)
StCollectionImp(RichMCInfo)
StCollectionImp(EmcRawHit)



StCollectionImp(TrackNode)
StCollectionImp(TrackPidTraits)
StCollectionImp(TrackDetectorInfo)
StCollectionImp(V0Vertex)
StCollectionImp(XiVertex)
StCollectionImp(KinkVertex)
