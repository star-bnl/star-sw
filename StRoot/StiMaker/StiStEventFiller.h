//StiStEventFiller.h
/***************************************************************************
 *
 * $Id: StiStEventFiller.h,v 2.5 2003/07/01 20:25:28 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Mar 2002
 ***************************************************************************
 *
 * $Log: StiStEventFiller.h,v $
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
#include "Sti/StiDedxCalculator.h"
class StEvent;
class StTrackNode;
class StTrackDetectorInfo;
class StTrack;
class StiTrackContainer;
class StiTrack;
class StiKalmanTrack;
class StHelix;
class StHelixModel;
class StPhysicalHelixD;

/*! \class StiStEventFiller
    StiStEventFiller is a utilitity class meant to properly convert StiKalmanTrack
    objects into StTrack (Global/Primary) objects and hang these on the StEvent
    Track-node.

    \author Manuel Calderon de la Barca Sanchez (Yale Software)
 */
class StiStEventFiller
{
public:
    StiStEventFiller();
    virtual ~StiStEventFiller();
    StEvent* fillEvent(StEvent*, StiTrackContainer*);
    StEvent* fillEventPrimaries(StEvent*, StiTrackContainer*);
    void fillDetectorInfo(StTrackDetectorInfo* detInfo, StiKalmanTrack* kTrack);
    void fillGeometry(StTrack* track, StiKalmanTrack* kTrack, bool outer);
    //void fillTopologyMap(StTrack* track, StiKalmanTrack* kTrack);
    void fillFitTraits(StTrack* track, StiKalmanTrack* kTrack);
    void fillPidTraits(StTrack* track, StiKalmanTrack* kTrack);
    void filldEdxInfo(StiDedxCalculator&, StTrack* track, StiKalmanTrack* kTrack);
    void fillTrack(StTrack* track, StiKalmanTrack* kTrack);
    unsigned short encodedStEventFitPoints(StiKalmanTrack* kTrack); 
    float impactParameter(StiKalmanTrack* kTrack);
private:
    StEvent* mEvent;
    StiTrackContainer* mTrackStore;
    map<StiKalmanTrack*, StTrackNode*> mTrkNodeMap;

    StiDedxCalculator dEdxTpcCalculator;
    StiDedxCalculator dEdxSvtCalculator;

    unsigned short mStiEncoded;
    //helix parameters
    StThreeVectorD *originD;
    StPhysicalHelixD * physicalHelix;


};

#endif
