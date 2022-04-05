//StvStEventFiller.h
/***************************************************************************
 *
 * $Id: StvStEventFiller.h,v 1.8 2015/11/11 01:52:57 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Mar 2002
 * Author: Victor Perev, Jun 2010
 ***************************************************************************
 *
 * $Log: StvStEventFiller.h,v $
 * Revision 1.8  2015/11/11 01:52:57  perev
 * Remove non used method setUseAux
 *
 * Revision 1.7  2015/06/10 17:28:08  perev
 * Print of used hits (from Sti) added
 *
 * Revision 1.6  2013/10/02 20:23:00  perev
 * kMinFitPoints 10 ==> 11 to be like Sti
 *
 * Revision 1.5  2012/10/21 22:57:22  perev
 * Add IdTruth into pulls
 *
 * Revision 1.4  2012/04/27 01:40:19  perev
 * Add konstant min hit number
 *
 * Revision 1.3  2012/04/10 22:41:54  perev
 * Cleanup
 *
 * Revision 1.2  2010/09/29 23:39:12  perev
 * Intereface fillPulls(...) chamnged
 *
 * Revision 1.1  2010/07/06 20:27:53  perev
 * Alpha version of Stv (Star Tracker Virtual)
 *
 * Revision 1.2  2010/07/03 16:27:15  perev
 * Last time name Stv
 *
 * Revision 1.1  2010/06/22 19:34:28  perev
 * EventFiller added
 *
 * Revision 2.24  2006/12/18 01:30:52  perev
 * fillPulls reorganized
 *
 *
 **************************************************************************/
#ifndef StvStEventFiller_HH
#define StvStEventFiller_HH
#include "StDetectorId.h"

enum ECuts { kMinFitPoints=11 };


class StEvent;
class StTrackNode;
class StTrackDetectorInfo;
class StvNode;
class StvHit;
class StTrack;
class StvPullEvent;
#include "StvEventFiller.h"
#include "StThreeVectorD.hh"
//class StHelix;
//class StHelixModel;
#include "StPhysicalHelixD.hh"

/*! \class StvStEventFiller
    StvStEventFiller is a utilitity class meant to properly convert StvTrack
    objects into StTrack (Global/Primary) objects and hang these on the StEvent
    Track-node.

    \author Manuel Calderon de la Barca Sanchez (Yale Software)
    Rewritten for StvVmc 
    \author Victor Perev (BNL)

 */
class StvStEventFiller : public StvEventFiller
{
public:
    StvStEventFiller();
    virtual ~StvStEventFiller();
    void fillEvent();
    void fillEventPrimaries();
    void fillDetectorInfo(StTrackDetectorInfo* detInfo, const StvTrack* kTrack,bool refCountIncr);
    void fillGeometry(StTrack* track, const StvTrack* kTrack, bool outer);
    //void fillTopologyMap(StTrack* track, const StvTrack* kTrack);
    void fillFitTraits(StTrack* track, const StvTrack* kTrack);
    void fillTrack(StTrack* track, const StvTrack* kTrack,StTrackDetectorInfo* detInfo );
    void fillDca(StTrack* track, const StvTrack* kTrack);
    void fillFlags(StTrack* track);
    double impactParameter(StTrack* strack , StThreeVectorD &vertexPosition);
    void setPullEvent(StvPullEvent *pe) 		{mPullEvent=pe;}
    void getAllPointCount(const StvTrack *track,int count[1][3]);
    const StvHit *GetHit(const StvNode *node) const;
private:
 void fillResHack(StHit *hh,const StvHit *stiHit, const StvNode *node);
 void fillPulls  (double len,StHit *hh,const StvHit *stiHit
                 ,const StvNode  *node
		 ,const StvTrack *track
		 ,int dets[1][3],int gloPri);
 void fillPulls  (const StvTrack *ktrack,const StTrack *stTrack,int gloPri);
 bool accept(const StvTrack* kTrack);
 void FillStHitErr(StHit *hh,const StvNode *node);

private:
    int mGloPri;		//0=filing global,1=filing primary
    int mTrackNumber;
    int mUsedHits[100];
    int mUsedGits[100];

    unsigned short mStvEncoded;


};

#endif
