/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.cxx,v 1.4 1999/07/19 14:24:07 hardtke Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when running
 *  root4star with StEventReaderMaker.
 *  It inherits from StHbtReaderMaker
 *
 *  Since this StHbtEventReader class gets its input from StEvent in root4star,
 *  it needs to know what chain has the StEventReaderMaker on it.  So you have
 *  to initialize (thru SetTheEventMaker()).
 *  Other StHbtEventReader classes (that might read ASCII files for example)
 *  would need other information, like the filename I guess, and so would
 *  have other private data members that they access.
 *
 ***************************************************************************
 *
 * $Log: StStandardHbtEventReader.cxx,v $
 * Revision 1.4  1999/07/19 14:24:07  hardtke
 * modifications to implement uDST
 *
 * Revision 1.3  1999/07/06 22:33:24  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.2  1999/06/29 17:50:28  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Reader/StStandardHbtEventReader.h"
#include "StChain/StChain.h"

// these StEvent files keep oscillating between ".hh" and ".h" files
// fortunately, they are used only here
/* .h files
   #include "StEvent.h"
   #include "StGlobalTrack.h"
   #include "StTpcDedxPid.h"
   #include "StDedxPid.h"
*/
#include "StEvent/StEvent.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTpcDedxPid.hh"
#include "StEvent/StDedxPid.hh"
//
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StEventMaker/StEventMaker.h"
#define HBT_B_FIELD 0.5*tesla

ClassImp(StStandardHbtEventReader)


//__________________
StStandardHbtEventReader::StStandardHbtEventReader(){
  /* no-op */
}
//__________________
//StStandardHbtEventReader::~StStandardHbtEventReader(){
//  /* no-op *//
//}
//__________________
StHbtEvent* StStandardHbtEventReader::ReturnHbtEvent(){

  cout << "StStandardHbtEventReader::ReturnHbtEvent" << endl;

  StEvent* rEvent = 0;

  /* Yuri put this "if 0" and DS stuff here, but it doesn't work
  #if 0
  rEvent = ((StEventReaderMaker*) mTheChain->Maker("events"))->event();
  #endif
  rEvent =  (StEvent *) GetInputDS("StEvent");
  */

  StEventMaker* tempMaker = (StEventMaker*) mTheEventMaker;

  rEvent = tempMaker->event();

  if (!rEvent){
    cout << "StStandardHbtEventReader - No StEvent!!! " << endl;
    return 0;
  }
  StHbtEvent* hbtEvent = new StHbtEvent;


  int mult = rEvent->trackCollection()->size();
  hbtEvent->SetNumberOfTracks(mult);
  hbtEvent->SetNumberOfGoodTracks(mult);  // same for now
  StHbtThreeVector vp = rEvent->primaryVertex()->position();
  hbtEvent->SetPrimVertPos(vp);
  
  StGlobalTrack* rTrack;
  cout << "StStandardHbtReader::ReturnHbtEvent - We have " << mult << " tracks to store - we skip tracks with nhits==0" << endl;
  // what the hell is this?  StHbtTrackCollection dummyTrackCollection;
  int icount=0;
  for (StTrackIterator iter=rEvent->trackCollection()->begin();
       iter!=rEvent->trackCollection()->end();iter++){
    //    cout << "Doing track number " << ++icount << endl;
    rTrack = *iter;
    int nhits = rTrack->numberOfTpcHits();
    //    cout << "nhits\t" << nhits << endl;
    if (nhits==0) {
      //      cout << "No hits -- skipping track (because it crashes otherwise)" << endl;
      continue;
    }

    //    cout << "Now getting the pidTraits" << endl;
    //StTrackPidTraits pidTraitsTemp = rTrack->pidTraits();
    //cout << " Got it"<<endl;

    const StDedxPid* tpcDedxPid = rTrack->pidTraits().tpcDedxPid();
    if (!tpcDedxPid) {
      cout << "No dEdx information - skipping track with " << nhits << " hits"<<endl;
      continue;
    }

    //    cout << "Getting readty to instantiate new StHbtTrack " << endl;

    StHbtTrack* hbtTrack = new StHbtTrack;

    //cout << "StHbtTrack instantiated " << endl;

    hbtTrack->SetNHits(nhits);

    float nsigpi = tpcDedxPid->numberOfSigma(0.139);
    //cout << "nsigpi\t\t" << nsigpi << endl;
    hbtTrack->SetNSigmaPion(nsigpi);
    float nsigk = tpcDedxPid->numberOfSigma(0.495);
    //cout << "nsigk\t\t\t" << nsigk << endl;
    hbtTrack->SetNSigmaKaon(nsigk);
    float nsigprot = tpcDedxPid->numberOfSigma(0.938);
    //cout << "nsigprot\t\t\t\t" << nsigprot << endl;
    hbtTrack->SetNSigmaProton(nsigprot);

    //    cout << "Nsig pion,kaon,proton : " << nsigpi << " " << nsigk << " " << nsigprot << endl;

    double pathlength = rTrack->helix().pathLength(vp);
    //cout << "pathlength\t" << pathlength << endl;
    StHbtThreeVector p = rTrack->helix().momentumAt(pathlength,HBT_B_FIELD);
    //cout << "p: " << p << endl;
    hbtTrack->SetP(p);

    float DCA = abs(rTrack->helix().at(pathlength)-vp);
    //cout << "DCA\t\t" << DCA << endl;
    //    hbtTrack->SetDCA(DCA);

    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    //cout << "pt\t\t\t" << pt << endl;
    //    hbtTrack->SetPt(pt);

    int charge = ((rTrack->helix().charge(HBT_B_FIELD)>0) ? 1 : -1);
    //cout << "charge\t\t\t\t" << charge << endl;
    hbtTrack->SetCharge(charge);
    
    //cout << "pushing..." <<endl;
    hbtEvent->TrackCollection()->push_back(hbtTrack);
  }
  return hbtEvent;
}
