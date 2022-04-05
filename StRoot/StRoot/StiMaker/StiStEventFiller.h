//StiStEventFiller.h
/***************************************************************************
 *
 * $Id: StiStEventFiller.h,v 2.28 2015/03/27 20:12:58 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Mar 2002
 ***************************************************************************
 *
 * $Log: StiStEventFiller.h,v $
 * Revision 2.28  2015/03/27 20:12:58  perev
 * Add printout of good track hits
 *
 * Revision 2.27  2015/03/21 02:16:53  perev
 * By Lidia request, addet printing number of used hits detector by detector
 * No any modification of any algorithmes
 *
 * Revision 2.26  2012/11/09 18:28:06  perev
 * fillpull development
 *
 * Revision 2.25  2012/05/07 14:56:14  fisyak
 * Add StKFVertexMaker
 *
 * Revision 2.24  2006/12/18 01:30:52  perev
 * fillPulls reorganized
 *
 * Revision 2.23  2006/08/29 22:18:37  fisyak
 * move filling of StTrackDetectorInfo into fillTrack
 *
 * Revision 2.22  2006/08/28 17:02:23  fisyak
 * Add +x11 short tracks pointing to EEMC, clean up StiDedxCalculator
 *
 * Revision 2.21  2006/06/16 21:29:17  perev
 * FillStHitErr method added and called
 *
 * Revision 2.20  2006/05/31 03:59:04  fisyak
 * Add Victor's dca track parameters, clean up
 *
 * Revision 2.19  2006/04/07 18:00:30  perev
 * Back to the latest Sti
 *
 * Revision 2.17  2005/12/08 00:03:07  perev
 * StiAux* mAux added
 *
 * Revision 2.16  2005/08/17 22:04:42  perev
 * PoinCount cleanup
 *
 * Revision 2.15  2005/07/20 17:34:29  perev
 * MultiVertex
 *
 * Revision 2.14  2005/04/11 17:42:53  perev
 * Temporary residuals saving added
 *
 * Revision 2.13  2005/02/07 18:34:17  fisyak
 * Add VMC dead material
 *
 * Revision 2.12  2004/10/26 06:45:41  perev
 * version V2V
 *
 * Revision 2.11  2004/10/14 02:21:35  calderon
 * Updated code in StTrackDetectorInfo, now only increment the reference count
 * for globals, not for primaries.  So fillTrackDetectorInfo changed to reflect
 * this.
 *
 * Revision 2.10  2004/08/06 22:23:29  calderon
 * Modified the code to use the setNumberOfxxxPoints(unsigned char,StDetectorId)
 * methods of StTrack, StTrackDetectorInfo, StTrackFitTraits, and to use
 * the maxPointCount(unsigned int detId) method of StiKalmanTrack.
 *
 * Revision 2.9  2004/07/07 19:33:48  calderon
 * Added method fillFlags.  Flags tpc, tpc+svt (globals and primaries) and flags -x02 tracks with less than 5 total fit points
 *
 * Revision 2.8  2004/03/31 00:27:29  calderon
 * Modifications for setting the fit points based on the chi2<chi2Max algorithm.
 * -Distinguish between points and fit points, so I added a function for each.
 * -Points is done as it was before, just counting the stHits for a given
 *  detector id.
 * -Fit points is done the same with the additional condition that each
 *  StiKalmanTrackNode has to satisfy the chi2 criterion.
 *
 * Revision 2.7  2004/03/23 23:12:36  calderon
 * Added an "accept" function to filter unwanted tracks from Sti into StEvent.
 * The current method just looks for tracks with a negative length, since
 * these were causing problems for the vertex finder (length was nan).  The
 * nan's have been trapped (one hopes!) in StiKalmanTrack, and for these
 * cases the return value is negative, so we can filter them out with a
 * simple length>0 condition.
 *
 * Revision 2.6  2004/01/27 23:40:47  calderon
 * The filling of the impactParameter() for global tracks is done now
 * only after finding the vertex.  The StPhysicalHelix::distance(StThreeVectorD)
 * method is used for both globals and primaries, the only difference is
 * where the helix is obtained:
 * - globals - helix from StTrack::geometry(), which was filled from the innermost
 *   hit node, which should be a hit at the time.
 * - primaries, helix from innermost hit node, which should be the vertex at the
 *   time it is called.
 *
 * Revision 2.5  2003/07/01 20:25:28  calderon
 * fillGeometry() - use node->getX(), as it should have been since the beginning
 * impactParameter() - always use the innermos hit node, not just for globals
 * removed extra variables which are no longer used.
 *
 * Revision 2.4  2003/04/25 21:41:18  andrewar
 * Added data memebers.
 *
 * Revision 2.3  2003/03/14 19:02:56  pruneau
 * various updates - DCA is a bitch
 *
 * Revision 2.2  2003/03/13 18:59:45  pruneau
 * various updates
 *
 * Revision 2.1  2003/01/22 21:12:16  calderon
 * Restored encoded method, uses enums but stores the value in constructor
 * as a data member so bit operations are only done once.
 * Fixed warnings.
 *
 * Revision 2.0  2002/12/04 16:51:01  pruneau
 * introducing version 2.0
 *
 * Revision 1.7  2002/08/22 21:46:00  pruneau
 * Made a fix to StiStEventFiller to remove calls to StHelix and StPhysicalHelix.
 * Currently there is one instance of StHelix used a calculation broker to
 * get helix parameters such as the distance of closest approach to the main
 * vertex.
 *
 * Revision 1.6  2002/08/19 19:33:01  pruneau
 * eliminated cout when unnecessary, made helix member of the EventFiller
 *
 * Revision 1.5  2002/08/12 21:39:57  calderon
 * Introduced fillPidTraits, which uses the values obtained from
 * Andrews brand new dEdxCalculator to create two instances of an
 * StTrackPidTraits object and pass it to the track being filled.
 *
 * Revision 1.4  2002/08/12 15:29:21  andrewar
 * Added dedx calculators
 *
 * Revision 1.3  2002/05/29 19:14:45  calderon
 * Filling of primaries, in
 * StiStEventFiller::fillEventPrimaries()
 *
 * Revision 1.2  2002/03/28 04:29:49  calderon
 * First test version of Filler
 * Currently fills only global tracks with the following characteristics
 * -Flag is set to 101, as most current global tracks are.  This is not strictly correct, as
 *  this flag is supposed to mean a tpc only track, so really need to check if the track has
 *  svt hits and then set it to the appropriate flag (501 or 601).
 * -Encoded method is set with bits 15 and 1 (starting from bit 0).  Bit 1 means Kalman fit.
 *  Bit 15 is an as-yet unused track-finding bit, which Thomas said ITTF could grab.
 * -Impact Parameter calculation is done using StHelix and the primary vertex from StEvent
 * -length is set using getTrackLength, which might still need tweaking
 * -possible points is currently set from getMaxPointCount which returns the total, and it is not
 *  what we need for StEvent, so this needs to be modified
 * -inner geometry (using the innermostHitNode -> Ben's transformer -> StPhysicalHelix -> StHelixModel)
 * -outer geometry, needs inside-out pass to obtain good parameters at outermostHitNode
 * -fit traits, still missing the probability of chi2
 * -topology map, filled from StuFixTopoMap once StDetectorInfo is properly set
 *
 * This version prints out lots of messages for debugging, should be more quiet
 * when we make progress.
 *
 **************************************************************************/
#ifndef StiStEventFiller_HH
#define StiStEventFiller_HH
#include <map>
using std::map;
#include "StDetectorId.h"
class StEvent;
class StTrackNode;
class StTrackDetectorInfo;
class StTrack;
class StiTrackContainer;
class StiTrack;
class StiKalmanTrack;
class StHelix;
class StHelixModel;
#include "StPhysicalHelixD.hh"
class StTrack2FastDetectorMatcher;
/*! \class StiStEventFiller
    StiStEventFiller is a utilitity class meant to properly convert StiKalmanTrack
    objects into StTrack (Global/Primary) objects and hang these on the StEvent
    Track-node.

    \author Manuel Calderon de la Barca Sanchez (Yale Software)
 */
class StiAux;
class StiPullEvent;
class StiStEventFiller
{
public:
    StiStEventFiller();
    void setUseAux(int aux=1)		{mUseAux=aux;}
    virtual ~StiStEventFiller();
    static StiStEventFiller *instance() {return fgStiStEventFiller;}
    void fillEvent(StEvent* e, StiTrackContainer* t);
    void fillEventPrimaries();
    void fillDetectorInfo(StTrackDetectorInfo* detInfo, StiKalmanTrack* kTrack,bool refCountIncr);
    void fillGeometry(StTrack* track, StiKalmanTrack* kTrack, bool outer);
    //void fillTopologyMap(StTrack* track, StiKalmanTrack* kTrack);
    void fillFitTraits(StTrack* track, StiKalmanTrack* kTrack);
    void fillTrack(StTrack* track, StiKalmanTrack* kTrack,StTrackDetectorInfo* detInfo );
    void fillDca(StTrack* track, StiKalmanTrack* kTrack);
    void fillFlags(StTrack* track);
    double impactParameter(StiKalmanTrack* kTrack, StThreeVectorD &vertexPosition);
    double impactParameter(StTrack* strack, StThreeVectorD &vertexPosition);
    void setPullEvent(StiPullEvent *pe) 		{mPullEvent=pe;}
 static map<StiKalmanTrack*, StTrackNode*> *Track2NodeMap() {return &mTrkNodeMap;}
 static map<StTrackNode*, StiKalmanTrack*> *Node2TrackMap() {return &mNodeTrkMap;}
private:
 void fillResHack(StHit *hh,const StiHit *stiHit, const StiKalmanTrackNode *node);
 void fillPulls  (StHit *hh,const StiHit *stiHit
                 ,const StiKalmanTrackNode *node
		 ,const StiKalmanTrack     *track
		 ,int dets[1][3],int gloPri);
 void fillPulls  (StiKalmanTrack *ktrack,const StGlobalTrack *gTrack,int gloPri);
 bool accept(StiKalmanTrack* kTrack);
 void FillStHitErr(StHit *hh,const StiKalmanTrackNode *node);
private:
    StEvent* mEvent;
    StiTrackContainer* mTrackStore;
    StiAux* mAux;
    StiPullEvent *mPullEvent;
    int mUseAux;
    int mGloPri;		//0=filing global,1=filing primary
    int mTrackNumber;
    int mUsedHits[100];
    int mUsedGits[100];
    static map<StiKalmanTrack*, StTrackNode*> mTrkNodeMap;
    static map<StTrackNode*, StiKalmanTrack*> mNodeTrkMap;
    unsigned short mStiEncoded;
    //helix parameters
    StThreeVectorD *originD;
    StPhysicalHelixD * physicalHelix;
    StTrack2FastDetectorMatcher *mFastDetectorMatcher;
    static StiStEventFiller* fgStiStEventFiller;
};

#endif
