/***************************************************************************
 *
 * $Id: StContainers.cxx,v 2.6 2000/06/19 01:32:15 perev Exp $
 *
 * Author: Thomas Ullrich, Oct 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StContainers.cxx,v $
 * Revision 2.6  2000/06/19 01:32:15  perev
 * Thomas StEvent branches added
 *
 * Revision 2.7  2000/09/28 10:56:33  ullrich
 * Added Rich PID classes.
 *
 * Revision 2.6  2000/06/19 01:32:15  perev
 *  Thomas StEvent branches added
 *
 * Revision 2.5  2000/05/22 21:42:41  ullrich
 * Add RICH related classes.
 *
 * Revision 2.4  2000/03/23 22:24:06  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 * Revision 2.3  2000/02/23 17:35:54  ullrich
 * Changes due to the addition of the EMC to StEvent
 *
 * Revision 2.2  2000/01/05 16:02:20  ullrich
 * SSD hits added to StEvent.
 *
 * Revision 2.1  1999/10/28 22:06:16  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StContainers.h"
#include "StSvtHit.h"
#include "StSsdHit.h"
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
#include "StPrimaryVertex.h"
#include "StTrackNode.h"
#include "StTrackPidTraits.h"
#include "StTrackDetectorInfo.h"
#include "StV0Vertex.h"
#include "StXiVertex.h"
#include "StKinkVertex.h"

StCollectionImp(Object)
StCollectionImp(Hit)
StCollectionImp(PrimaryTrack)
StCollectionImp(PrimaryVertex)
StCollectionImp(SvtHit)
StCollectionImp(SsdHit)
StCollectionImp(TpcHit)
StCollectionImp(FtpcHit)
StCollectionImp(RichCluster)
StCollectionImp(RichPixel)
StCollectionImp(RichPid)
StCollectionImp(RichMCInfo)
StCollectionImp(EmcRawHit)
StCollectionImp(EmcCluster)
StCollectionImp(EmcPoint)
StCollectionImp(Track)
StCollectionImp(TrackNode)
StCollectionImp(TrackPidTraits)
StCollectionImp(TrackDetectorInfo)
StCollectionImp(V0Vertex)
StCollectionImp(XiVertex)
StCollectionImp(KinkVertex)
