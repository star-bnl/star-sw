/***************************************************************************
 *
 * $Id: StMiniDstMaker.cxx,v 1.2 2000/10/16 19:35:41 ullrich Exp $
 *
 * Author: Thomas Ullrich, Oct 2000
 ***************************************************************************
 *
 * Description:  Example program to write miniDSTs based on StEvent.
 *               The miniDST will contain primary K+/K- tracks only.
 *
 ***************************************************************************
 *
 * $Log: StMiniDstMaker.cxx,v $
 * Revision 1.2  2000/10/16 19:35:41  ullrich
 * Updated to run on Sun/CC5.
 *
 * Revision 1.1  2000/10/13 19:26:17  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <typeinfo>
#include "StMiniDstMaker.h"
#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "SystemOfUnits.h"
#include "StGlobals.hh"
#include "BetheBloch.h"
#include "StParticleTypes.hh"
#include "kaonPid.hh"
#include "StEventScavenger.h"

static size_t acceptedEvents = 0;
static size_t acceptedKaons = 0;

ClassImp(StMiniDstMaker)

StMiniDstMaker::StMiniDstMaker(const Char_t *name) : StMaker(name)
{/* noop */ }

Int_t StMiniDstMaker::Init()
{
    return StMaker::Init();
}

Int_t StMiniDstMaker::Make()
{
    //
    //	Get StEvent
    //
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    if (!event) {
	cout << "StMiniDstMaker::Make(): not a valid StEvent: "
	     << (void*)event << endl;
	return kStOK;
    }
       
    //
    //  Filter out what seems to be crap, i.e. no primary vertex
    //
    if (!accept(event)) {
	cout << "StMiniDstMaker::Make(): event not accepted." << endl;
	delete event;
	return kStOK;
    }
    acceptedEvents++;

    
    //================================================================
    //
    //  Prepare the miniDST by removing everything we don't like.
    //  The rest happens automatically.
    //
    //================================================================
 
    StEventScavenger::removeEventSummary(event);
    StEventScavenger::removeSoftwareMonitor(event);
    StEventScavenger::removeTpcHitCollection(event);
    StEventScavenger::removeFtpcHitCollection(event);
    StEventScavenger::removeSvtHitCollection(event);
    StEventScavenger::removeSsdHitCollection(event);
    StEventScavenger::removeEmcCollection(event);
    StEventScavenger::removeRichCollection(event);
    // StEventScavenger::TriggerDetectorCollection(event);
    StEventScavenger::removeL3Trigger(event);
    StEventScavenger::removeV0Vertices(event);
    StEventScavenger::removeXiVertices(event);
    StEventScavenger::removeKinkVertices(event);

    //
    //  Select primary Kaons only
    //
    int nkaons = 0;
    StSPtrVecPrimaryTrack& theTracks = event->primaryVertex()->daughters();
    for (unsigned int i=0; i<theTracks.size(); i++)
	if (!accept(theTracks[i]))
	    StEventScavenger::remove(theTracks[i]);
	else
	    nkaons++;

    //
    //  Not interested in globals
    //
    StSPtrVecTrackNode& nodes = event->trackNodes();
    for (unsigned int k=0; k<nodes.size(); k++)
	 StEventScavenger::remove(nodes[k]->track(global));

    cout << "StMiniDstMaker::Make(): " << nkaons << " kaon candidates written." << endl;
    acceptedKaons += nkaons;

    //
    //  That's it!
    //  The IO maker will take care of the rest.
    //
    return kStOK;
}

bool StMiniDstMaker::accept(StEvent* event)
{
    if (!event->primaryVertex())
	cout << "StMiniDstMaker::accept(): no valid primary vertex." << endl;
    return event->primaryVertex();
}

bool StMiniDstMaker::accept(StTrack* track)
{    
    static kaonPid  pid;
    static StKaonPlus*  pkaon = StKaonPlus::instance();
    static StKaonMinus* nkaon = StKaonMinus::instance();

    if (track && track->flag() >= 0 && abs(track->geometry()->momentum()) < 0.8) {
            const StParticleDefinition* particle = track->pidTraits(pid);
	    if (particle == pkaon || particle == nkaon) return true;
    }
    return false;
}

Int_t StMiniDstMaker::Finish()
{
    cout << "**************************************************************************" << endl;
    cout << "*************************>> miniDST statistics <<*************************" << endl;
    cout << "**************************************************************************" << endl;
    cout << "===> total number of accepted event: " << acceptedEvents  << endl;
    cout << "===> total number of accepted kaons: " << acceptedKaons  << endl;
    cout << "**************************************************************************" << endl;
    return kStOK;
}
