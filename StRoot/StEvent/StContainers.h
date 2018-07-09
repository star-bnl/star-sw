/***************************************************************************
 *
 * $Id: StContainers.h,v 2.42 2018/07/09 14:54:37 ullrich Exp $
 *
 * Author: Thomas Ullrich, Oct 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StContainers.h,v $
 * Revision 2.42  2018/07/09 14:54:37  ullrich
 * Changed to add ETof.
 *
 * Revision 2.41  2018/02/08 17:36:26  ullrich
 * Changed for new EPD classes.
 *
 * Revision 2.40  2015/11/25 16:53:36  perev
 * Backward
 *
 * Revision 2.38  2015/10/02 19:48:53  ullrich
 * Added Rps tracks and points
 *
 * Revision 2.37  2015/09/01 18:29:01  ullrich
 * Changes due to adding StFpsSlat and interconnection between slats and points.
 *
 * Revision 2.36  2015/05/13 17:06:13  ullrich
 * Added hooks and interfaces to Sst detector (part of HFT).
 *
 * Revision 2.35  2015/02/14 18:57:24  ullrich
 * Big upgrade after adding StFmPoint and StFmsCluster.
 *
 * Revision 2.34  2014/04/10 16:00:12  jeromel
 * Changes to inlcude Ist structure (Thomas OK-ed / may revisit some comments)
 *
 * Revision 2.33  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 * Revision 2.31  2013/04/10 19:15:52  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.29  2013/03/05 14:42:45  ullrich
 * Added StPxl hits and Containers.
 *
 * Revision 2.28  2012/04/16 20:22:16  ullrich
 * Changes necessary to add Fgt package.
 *
 * Revision 2.27  2012/01/24 02:57:05  perev
 * Etr detector added
 *
 * Revision 2.26  2011/04/25 21:24:33  ullrich
 * Added containers for MTD.
 *
 * Revision 2.25  2011/02/01 19:47:36  ullrich
 * Added HLT branch and hooks.
 *
 * Revision 2.24  2010/01/08 22:43:44  ullrich
 * Updates needed to add StFmsCollection and related classes.
 *
 * Revision 2.23  2009/11/23 22:22:25  ullrich
 * Minor cleanup performed and hooks for RPS added.
 *
 * Revision 2.22  2008/12/22 20:36:52  ullrich
 * Added hooks for new ToF (BTof)
 *
 * Revision 2.21  2006/01/19 21:50:40  ullrich
 * Added RnD containers.
 *
 * Revision 2.20  2005/04/11 22:35:25  calderon
 * Tof Classes for Run 5.  Modifications and additions from Xin to
 * take care of new TOF daq and electronics.  Added StTofRawData and
 * modified containers and includes.
 *
 * Revision 2.19  2004/03/01 17:44:37  fisyak
 * Add Print method
 *
 * Revision 2.18  2003/10/25 00:15:02  fisyak
 * remove StPhmdHit and StPhmdCluster added by mistake
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
 * Revision 2.11  2001/08/02 01:27:09  ullrich
 * Added containers for StL3AlgorithmInfo.
 *
 * Revision 2.10  2001/04/16 20:29:46  ullrich
 * Added Tof containers.
 *
 * Revision 2.9  2001/03/14 02:29:17  ullrich
 * Added StPsd container.
 *
 * Revision 2.8  2000/11/25 11:52:23  lasiuk
 * addition of Rich Photon Info
 *
 * Revision 2.7  2000/09/28 10:56:35  ullrich
 * Added Rich PID classes.
 *
 * Revision 2.6  2000/06/19 01:32:15  perev
 *  Thomas StEvent branches added
 *
 * Revision 2.5  2000/05/22 21:42:44  ullrich
 * Add RICH related classes.
 *
 * Revision 2.4  2000/03/23 22:24:06  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 * Revision 2.3  2000/02/23 17:35:57  ullrich
 * Changes due to the addition of the EMC to StEvent
 *
 * Revision 2.2  2000/01/05 16:02:23  ullrich
 * SSD hits added to StEvent.
 *
 * Revision 2.1  1999/10/28 22:06:19  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StContainers_hh
#define StContainers_hh
#include "StArray.h"

class StBTofHit;
class StBTofRawHit;
class StCalibrationVertex;
class StDetectorState;
class StEmcCluster;
class StEmcPoint;
class StEmcRawHit;
class StEpdHit;
class StEtrHit;
class StFgtHit;
class StFgtPoint;
class StFgtStrip;
class StFmsCluster;
class StFmsHit;
class StFmsPoint;
class StFpsSlat;
class StFtpcHit;
class StHit;
class StHltBEmcTowerHit;
class StHltBTofHit;
class StHltDiElectron;
class StHltHeavyFragment;
class StHltHighPt;
class StHltTrack;
class StHltTrackNode;
class StHltTriggerReason;
class StHltVpdHit;
class StIstHit;
class StKinkVertex;
class StL3AlgorithmInfo;
class StMtdHit;
class StMtdRawHit;
class StObject;
class StPhmdCluster;
class StPhmdCluster;
class StPhmdHit;
class StPhmdHit;
class StPrimaryTrack;
class StPrimaryVertex;
class StPsd;
class StPxlHit;
class StRichCluster;
class StRichHit;
class StRichMCInfo;
class StRichPhotonInfo;
class StRichPid;
class StRichPixel;
class StRnDHit;
class StRpsCluster;
class StRpsTrack;
class StRpsTrackPoint;
class StSsdHit;
class StSstHit;
class StSvtHit;
class StTofCell;
class StTofData;
class StTofHit;
class StETofHit;
class StETofDigi;
class StTofRawData;
class StTofSlat;
class StTpcHit;
class StTrack;
class StTrackDetectorInfo;
class StTrackNode;
class StTrackPidTraits;
class StV0Vertex;
class StXiVertex;

StCollectionDef(BTofHit)
StCollectionDef(BTofRawHit)
StCollectionDef(CalibrationVertex)
StCollectionDef(DetectorState)
StCollectionDef(EmcCluster)
StCollectionDef(EmcPoint)
StCollectionDef(EmcRawHit)
StCollectionDef(EpdHit)    // MAL
StCollectionDef(EtrHit)
StCollectionDef(FgtHit)
StCollectionDef(FgtPoint)
StCollectionDef(FgtStrip)
StCollectionDef(FmsCluster)
StCollectionDef(FmsHit)
StCollectionDef(FmsPoint)
StCollectionDef(FpsSlat)
StCollectionDef(FtpcHit)
StCollectionDef(Hit)
StCollectionDef(HltBEmcTowerHit)
StCollectionDef(HltBTofHit)
StCollectionDef(HltDiElectron)
StCollectionDef(HltHeavyFragment)
StCollectionDef(HltHighPt)
StCollectionDef(HltTrack)
StCollectionDef(HltTrackNode)
StCollectionDef(HltTriggerReason)
StCollectionDef(HltVpdHit)
StCollectionDef(IstHit)
StCollectionDef(KinkVertex)
StCollectionDef(L3AlgorithmInfo)
StCollectionDef(MtdHit)
StCollectionDef(MtdRawHit)
StCollectionDef(Object)
StCollectionDef(PhmdCluster)
StCollectionDef(PhmdHit)
StCollectionDef(PrimaryTrack)
StCollectionDef(PrimaryVertex)
StCollectionDef(Psd)
StCollectionDef(PxlHit)
StCollectionDef(RichCluster)
StCollectionDef(RichHit)
StCollectionDef(RichMCInfo)
StCollectionDef(RichPhotonInfo)
StCollectionDef(RichPid)
StCollectionDef(RichPixel)
StCollectionDef(RnDHit)
StCollectionDef(RpsCluster)
StCollectionDef(RpsTrack)
StCollectionDef(RpsTrackPoint)
StCollectionDef(SsdHit)
StCollectionDef(SstHit)
StCollectionDef(SvtHit)
StCollectionDef(TofCell)
StCollectionDef(TofData)
StCollectionDef(TofHit)
StCollectionDef(ETofHit)
StCollectionDef(ETofDigi)
StCollectionDef(TofRawData)
StCollectionDef(TofSlat)
StCollectionDef(TpcHit)
StCollectionDef(Track)
StCollectionDef(TrackDetectorInfo)
StCollectionDef(TrackNode)
StCollectionDef(TrackPidTraits)
StCollectionDef(V0Vertex)
StCollectionDef(XiVertex)

#endif
