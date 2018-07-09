/***************************************************************************
 *
 * $Id: StContainers.cxx,v 2.39 2018/07/09 14:54:37 ullrich Exp $
 *
 * Author: Thomas Ullrich, Oct 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StContainers.cxx,v $
 * Revision 2.39  2018/07/09 14:54:37  ullrich
 * Changed to add ETof.
 *
 * Revision 2.38  2018/02/08 17:36:26  ullrich
 * Changed for new EPD classes.
 *
 * Revision 2.37  2015/11/25 16:53:15  perev
 * Backward
 *
 * Revision 2.35  2015/10/02 19:48:53  ullrich
 * Added Rps tracks and points
 *
 * Revision 2.34  2015/09/01 18:29:01  ullrich
 * Changes due to adding StFpsSlat and interconnection between slats and points.
 *
 * Revision 2.33  2015/05/13 17:06:13  ullrich
 * Added hooks and interfaces to Sst detector (part of HFT).
 *
 * Revision 2.32  2015/02/14 18:57:24  ullrich
 * Big upgrade after adding StFmPoint and StFmsCluster.
 *
 * Revision 2.31  2014/04/10 16:00:12  jeromel
 * Changes to inlcude Ist structure (Thomas OK-ed / may revisit some comments)
 *
 * Revision 2.30  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 * Revision 2.28  2013/04/10 19:15:52  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.26  2013/03/05 14:42:45  ullrich
 * Added StPxl hits and Containers.
 *
 * Revision 2.25  2012/04/16 20:22:16  ullrich
 * Changes necessary to add Fgt package.
 *
 * Revision 2.24  2012/01/24 02:57:09  perev
 * Etr detector added
 *
 * Revision 2.23  2011/04/25 21:24:33  ullrich
 * Added containers for MTD.
 *
 * Revision 2.22  2011/02/01 19:47:36  ullrich
 * Added HLT branch and hooks.
 *
 * Revision 2.21  2010/01/08 22:43:44  ullrich
 * Updates needed to add StFmsCollection and related classes.
 *
 * Revision 2.20  2009/11/23 22:22:25  ullrich
 * Minor cleanup performed and hooks for RPS added.
 *
 * Revision 2.19  2008/12/24 22:15:53  fisyak
 * Add implementation for BTof containers
 *
 * Revision 2.18  2006/01/19 21:50:40  ullrich
 * Added RnD containers.
 *
 * Revision 2.17  2005/04/11 22:35:24  calderon
 * Tof Classes for Run 5.  Modifications and additions from Xin to
 * take care of new TOF daq and electronics.  Added StTofRawData and
 * modified containers and includes.
 *
 * Revision 2.16  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.15  2002/12/20 22:34:42  ullrich
 * Added containers for PMD hits and clusters.
 *
 * Revision 2.14  2001/12/01 15:39:19  ullrich
 * Added macros for StDetectorState.
 *
 * Revision 2.13  2001/11/10 23:53:22  ullrich
 * Added calibration vertices.
 *
 * Revision 2.12  2001/10/01 19:40:31  ullrich
 * Added StTofData.
 *
 * Revision 2.11  2001/08/02 01:27:08  ullrich
 * Added containers for StL3AlgorithmInfo.
 *
 * Revision 2.10  2001/04/16 20:29:45  ullrich
 * Added Tof containers.
 *
 * Revision 2.9  2001/03/14 02:29:16  ullrich
 * Added StPsd container.
 *
 * Revision 2.8  2000/11/25 11:52:21  lasiuk
 * addition of Rich Photon Info
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
#include "StCalibrationVertex.h"
#include "StContainers.h"
#include "StDetectorState.h"
#include "StEmcCluster.h"
#include "StEmcPoint.h"
#include "StEpdHit.h"
#include "StEmcRawHit.h"
#include "StEtrHit.h"
#include "StFgtHit.h"
#include "StFgtPoint.h"
#include "StFgtStrip.h"
#include "StFmsCluster.h"
#include "StFmsHit.h"
#include "StFmsPoint.h"
#include "StFpsSlat.h"
#include "StFtpcHit.h"
#include "StHltBEmcTowerHit.h"
#include "StHltBTofHit.h"
#include "StHltDiElectron.h"
#include "StHltHeavyFragment.h"
#include "StHltHighPt.h"
#include "StHltTrack.h"
#include "StHltTrackNode.h"
#include "StHltTriggerReason.h"
#include "StHltVpdHit.h"
#include "StIstHit.h"
#include "StKinkVertex.h"
#include "StL3AlgorithmInfo.h"
#include "StMtdHit.h"
#include "StMtdRawHit.h"
#include "StPhmdCluster.h"
#include "StPhmdHit.h"
#include "StPrimaryTrack.h"
#include "StPrimaryVertex.h"
#include "StPsd.h"
#include "StPxlHit.h"
#include "StRichCluster.h"
#include "StRichHit.h"
#include "StRichMCInfo.h"
#include "StRichPhotonInfo.h"
#include "StRichPid.h"
#include "StRichPixel.h"
#include "StRnDHit.h"
#include "StRpsCluster.h"
#include "StRpsTrack.h"
#include "StRpsTrackPoint.h"
#include "StSsdHit.h"
#include "StSstHit.h"
#include "StSvtHit.h"
#include "StTofCell.h"
#include "StTofData.h"
#include "StTofHit.h"
#include "StETofHit.h"
#include "StETofDigi.h"
#include "StTofRawData.h"
#include "StTofSlat.h"
#include "StTpcHit.h"
#include "StTrackDetectorInfo.h"
#include "StTrackNode.h"
#include "StTrackPidTraits.h"
#include "StV0Vertex.h"
#include "StXiVertex.h"

StCollectionImp(BTofHit)
StCollectionImp(BTofRawHit)
StCollectionImp(CalibrationVertex)
StCollectionImp(DetectorState)
StCollectionImp(EmcCluster)
StCollectionImp(EmcPoint)
StCollectionImp(EmcRawHit)
StCollectionImp(EpdHit)
StCollectionImp(EtrHit)
StCollectionImp(FgtHit)
StCollectionImp(FgtPoint)
StCollectionImp(FgtStrip)
StCollectionImp(FmsCluster)
StCollectionImp(FmsHit)
StCollectionImp(FmsPoint)
StCollectionImp(FpsSlat)
StCollectionImp(FtpcHit)
StCollectionImp(Hit)
StCollectionImp(HltBEmcTowerHit)
StCollectionImp(HltBTofHit)
StCollectionImp(HltDiElectron)
StCollectionImp(HltHeavyFragment)
StCollectionImp(HltHighPt)
StCollectionImp(HltTrack)
StCollectionImp(HltTrackNode)
StCollectionImp(HltTriggerReason)
StCollectionImp(HltVpdHit)
StCollectionImp(IstHit)
StCollectionImp(KinkVertex)
StCollectionImp(L3AlgorithmInfo)
StCollectionImp(MtdHit)
StCollectionImp(MtdRawHit)
StCollectionImp(Object)
StCollectionImp(PhmdCluster)
StCollectionImp(PhmdHit)
StCollectionImp(PrimaryTrack)
StCollectionImp(PrimaryVertex)
StCollectionImp(Psd)
StCollectionImp(PxlHit)
StCollectionImp(RichCluster)
StCollectionImp(RichHit)
StCollectionImp(RichMCInfo)
StCollectionImp(RichPhotonInfo)
StCollectionImp(RichPid)
StCollectionImp(RichPixel)
StCollectionImp(RnDHit)
StCollectionImp(RpsCluster)
StCollectionImp(RpsTrack)
StCollectionImp(RpsTrackPoint)
StCollectionImp(SsdHit)
StCollectionImp(SstHit)
StCollectionImp(SvtHit)
StCollectionImp(TofCell)
StCollectionImp(TofData)
StCollectionImp(TofHit)
StCollectionImp(ETofHit)
StCollectionImp(ETofDigi)
StCollectionImp(TofRawData)
StCollectionImp(TofSlat)
StCollectionImp(TpcHit)
StCollectionImp(Track)
StCollectionImp(TrackDetectorInfo)
StCollectionImp(TrackNode)
StCollectionImp(TrackPidTraits)
StCollectionImp(V0Vertex)
StCollectionImp(XiVertex)
