/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.cxx,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
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
 *  to initialize (thru SetTheChain()).
 *  Other StHbtEventReader classes (that might read ASCII files for example)
 *  would need other information, like the filename I guess, and so would
 *  have other private data members that they access.
 *
 ***************************************************************************
 *
 * $Log: StStandardHbtEventReader.cxx,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Reader/StStandardHbtEventReader.h"
#include "StChain/StChain.h"
#include "StEvent/StEvent.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTpcDedxPid.hh"
#include "StEvent/StDedxPid.hh"
#include "StEventReaderMaker/StEventReaderMaker.h"
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
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
  rEvent = ((StEventReaderMaker*) mTheChain->Maker("events"))->event();
  if (!rEvent){
    cout << "StStandardHbtEventReader - No StEvent!!! " << endl;
    return 0;
  }
  StHbtEvent* hbtEvent = new StHbtEvent;


  int mult = rEvent->trackCollection()->size();
  cout << "StStandardHbtReader::ReturnHbtEvent() - mult, size() : "
       << mult << rEvent->trackCollection()->size() << endl;
  hbtEvent->SetMult(mult);
  StThreeVector<double> vp = rEvent->primaryVertex()->position();
  hbtEvent->SetPrimVertPos(vp);
  
  StGlobalTrack* rTrack;
  cout << "StStandardHbtReader::ReturnHbtEvent - We have " << mult << " tracks to store - we skip tracks with nhits==0" << endl;
  StHbtTrackCollection dummyTrackCollection;
  int icount=0;
  for (StTrackIterator iter=rEvent->trackCollection()->begin();
       iter!=rEvent->trackCollection()->end();iter++){
    //    cout << "Doing track number " << ++icount << endl;
    rTrack = *iter;
    int nhits = rTrack->numberOfTpcHits();
    //cout << "nhits\t" << nhits << endl;
    if (nhits==0) {
      //      cout << "No hits -- skipping track (because it crashes otherwise)" << endl;
      continue;
    }
    const StDedxPid* tpcDedxPid = rTrack->pidTraits().tpcDedxPid();
    if (!tpcDedxPid) {
      cout << "No dEdx information - skipping track with " << nhits << " hits"<<endl;
      continue;
    }

    StHbtTrack* hbtTrack = new StHbtTrack;
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

    double pathlength = rTrack->helix().pathLength(vp);
    //cout << "pathlength\t" << pathlength << endl;
    StThreeVector<double> p = rTrack->helix().momentumAt(pathlength,HBT_B_FIELD);
    //cout << "p: " << p << endl;
    hbtTrack->SetP(p);

    float DCA = abs(rTrack->helix().at(pathlength)-vp);
    //cout << "DCA\t\t" << DCA << endl;
    hbtTrack->SetDCA(DCA);

    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    //cout << "pt\t\t\t" << pt << endl;
    hbtTrack->SetPt(pt);

    int charge = ((rTrack->helix().charge(HBT_B_FIELD)>0) ? 1 : -1);
    //cout << "charge\t\t\t\t" << charge << endl;
    hbtTrack->SetCharge(charge);
    
    //cout << "pushing..." <<endl;
    hbtEvent->TrackCollection()->push_back(hbtTrack);
  }
  return hbtEvent;
}
