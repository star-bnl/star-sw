/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.cxx,v 1.9 1999/09/16 18:48:01 lisa Exp $
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
 * Revision 1.9  1999/09/16 18:48:01  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * Revision 1.8  1999/09/08 04:15:53  lisa
 * persistent microDST implementation tweaked to please fickle solaris details
 *
 * Revision 1.7  1999/09/03 22:39:17  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.6  1999/07/27 20:21:10  lisa
 * Franks fixes of StTrack and subsequent changes to particleCut and EventReader
 *
 * Revision 1.5  1999/07/24 16:24:25  lisa
 * adapt StHbtMaker to dev version of library - solaris still gives problems with strings
 *
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
#include "TOrdCollection.h"



// these StEvent files keep oscillating between ".hh" and ".h" files
// fortunately, they are used only here

#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StTpcDedxPid.h"
#include "StDedxPid.h"

/* .hh files
   #include "StEvent/StEvent.hh"
   #include "StEvent/StGlobalTrack.hh"
   #include "StEvent/StTpcDedxPid.hh"
   #include "StEvent/StDedxPid.hh"
*/
//
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StV0MiniDstMaker/StV0MiniDstMaker.h"  
#include "StV0MiniDstMaker/StV0MiniDst.hh"

#include "StEventMaker/StEventMaker.h"
#define HBT_B_FIELD 0.5*tesla

ClassImp(StStandardHbtEventReader)


//__________________
StStandardHbtEventReader::StStandardHbtEventReader(){
  mEventCut=0;
  mParticleCut=0;
  mReaderStatus = 0;  // "good"
  mV0=0;
}
//__________________
StStandardHbtEventReader::~StStandardHbtEventReader(){
  if (mEventCut) delete mEventCut;
  if (mParticleCut) delete mParticleCut;
}
//__________________
StHbtString StStandardHbtEventReader::Report(){
  StHbtString temp = "\n This is the StStandardHbtEventReader\n";
  temp += "---> EventCuts in Reader: ";
  if (mEventCut) {
    temp += mEventCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> ParticleCuts in Reader: ";
  if (mParticleCut) {
    temp += mParticleCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";
  return temp;
}
//__________________
StHbtEvent* StStandardHbtEventReader::ReturnHbtEvent(){

  cout << "StStandardHbtEventReader::ReturnHbtEvent" << endl;

  StEvent* rEvent = 0;

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
  
  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
  if (mEventCut){
    if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
      delete hbtEvent;
      return 0;
    }
  }


  StGlobalTrack* rTrack;
  cout << "StStandardHbtReader::ReturnHbtEvent - We have " << mult << " tracks to store - we skip tracks with nhits==0" << endl;


  // UNBELIEVABLY the star software cannot even handle its own iterators.  So the following line does not compile.
  //  for (StTrackIterator iter=rEvent->trackCollection()->begin();
  //    iter!=rEvent->trackCollection()->end();iter++){

  // gotta do it this way (incredibly)
  StTrackIterator iter=rEvent->trackCollection()->begin();
  for (int icount=0; icount<mult; icount++, iter++){

    rTrack = *iter;
    int nhits = rTrack->numberOfTpcHits();
    //    cout << "nhits\t" << nhits << endl;
    if (nhits==0) {
      //      cout << "No hits -- skipping track (because it crashes otherwise)" << endl;
      continue;
    }

    //cout << "Now getting the pidTraits" << endl;
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
    //cout << "Nsig pion,kaon,proton : " << nsigpi << " " << nsigk << " " << nsigprot << endl;
    
    float dEdx = rTrack->tpcDedx()->mean();
    //cout << "dEdx\t" << dEdx << endl; 
    hbtTrack->SetdEdx(dEdx);
    
    double pathlength = rTrack->helix().pathLength(vp);
    //cout << "pathlength\t" << pathlength << endl;
    StHbtThreeVector p = rTrack->helix().momentumAt(pathlength,HBT_B_FIELD);
    //cout << "p: " << p << endl;
    hbtTrack->SetP(p);

    StHbtThreeVector  DCAxyz = rTrack->helix().at(pathlength)-vp;
    //cout << "DCA\t\t" << DCAxyz << " " << DCAxyz.perp() << endl;
    hbtTrack->SetDCAxy( DCAxyz.perp() );
    hbtTrack->SetDCAz(  DCAxyz.z()  );

    hbtTrack->SetChiSquaredXY( rTrack->fitTraits().chiSquaredInXY() );
    hbtTrack->SetChiSquaredZ( rTrack->fitTraits().chiSquaredInPlaneZ() ); 

    StPhysicalHelixD&  helix = rTrack->helix();
    hbtTrack->SetHelix( helix );

     float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    //cout << "pt\t\t\t" << pt << endl;
    //hbtTrack->SetPt(pt);

    hbtTrack->SetPt(pt);

    int charge = ((rTrack->helix().charge(HBT_B_FIELD)>0) ? 1 : -1);
    //cout << "charge\t\t\t\t" << charge << endl;
    hbtTrack->SetCharge(charge);
    
    //cout << "pushing..." <<endl;

    // By now, all track-wise information has been extracted and stored in hbtTrack
    // see if it passes any front-loaded event cut
    if (mParticleCut){
      if (!(mParticleCut->Pass(hbtTrack))){                  // track failed - delete it and skip the push_back
	delete hbtTrack;
	continue;
      }
    }


    hbtEvent->TrackCollection()->push_back(hbtTrack);
  }




  //Now do v0 stuff


  //Pick up pointer v0 minidst maker
     StV0MiniDstMaker* v0Maker = (StV0MiniDstMaker *) mTheV0Maker;
       if( ! v0Maker ) {
	 cout << "Not doing v0 stuff" << endl;
	 return hbtEvent; 
       }
       //Get collection

      mCollection = v0Maker->GetCollection();
      int n_v0 =0;
      if( mCollection ){
        n_v0 = mCollection->GetSize();
	//Loop over all v0s in collection for this event

        for( int i=mV0; i<n_v0; i++){
        StV0MiniDst* v0FromMiniDst = (StV0MiniDst *) mCollection->At(i);
        v0FromMiniDst->UpdateV0();
        StHbtV0* hbtV0 = new StHbtV0;
        hbtV0->SetdecayLengthV0(v0FromMiniDst->decayLengthV0());
	hbtV0->SetdecayVertexV0(v0FromMiniDst->decayVertexV0());
	hbtV0->SetdcaV0Daughters(v0FromMiniDst->dcaV0Daughters());
	hbtV0->SetdcaV0ToPrimVertex(v0FromMiniDst->dcaV0ToPrimVertex());
        hbtV0->SetdcaPosToPrimVertex(v0FromMiniDst->dcaPosToPrimVertex());
        hbtV0->SetdcaNegToPrimVertex(v0FromMiniDst->dcaNegToPrimVertex());
        hbtV0->SetmomPos(v0FromMiniDst->momPos());
        hbtV0->SetmomNeg(v0FromMiniDst->momNeg());
        hbtV0->SettpcHitsPos(v0FromMiniDst->tpcHitsPos());
        hbtV0->SettpcHitsNeg(v0FromMiniDst->tpcHitsNeg());
        hbtV0->SetmomV0(v0FromMiniDst->momV0());
        hbtV0->SetalphaV0(v0FromMiniDst->alphaV0());
        hbtV0->SetptArmV0(v0FromMiniDst->ptArmV0());
        hbtV0->SeteLambda(v0FromMiniDst->eLambda());
        hbtV0->SeteK0Short(v0FromMiniDst->eK0Short());
        hbtV0->SetePosProton(v0FromMiniDst->ePosProton());
        hbtV0->SetePosPion(v0FromMiniDst->ePosPion());
        hbtV0->SeteNegPion(v0FromMiniDst->eNegPion());
        hbtV0->SeteNegProton(v0FromMiniDst->eNegProton());
        hbtV0->SetmassLambda(v0FromMiniDst->massLambda());
        hbtV0->SetmassAntiLambda(v0FromMiniDst->massAntiLambda());
        hbtV0->SetmassK0Short(v0FromMiniDst->massK0Short());
        hbtV0->SetrapLambda(v0FromMiniDst->rapLambda());
        hbtV0->SetrapK0Short(v0FromMiniDst->rapK0Short());
        hbtV0->SetcTauLambda(v0FromMiniDst->cTauLambda());
        hbtV0->SetcTauK0Short(v0FromMiniDst->cTauK0Short());
        hbtV0->SetptV0(v0FromMiniDst->ptV0());
        hbtV0->SetptotV0(v0FromMiniDst->ptotV0());
        hbtV0->SetptPos(v0FromMiniDst->ptPos());
        hbtV0->SetptotPos(v0FromMiniDst->ptotPos());
        hbtV0->SetptNeg(v0FromMiniDst->ptNeg());
        hbtV0->SetptotNeg(v0FromMiniDst->ptotNeg());

	hbtEvent->V0Collection()->push_back(hbtV0);
        }
	//Store total number of v0s in v0minidst so can start from there next time
        cout << "**** n_v0 = " << n_v0 << "**mV0"   << n_v0-mV0 << endl;        //  "       "
	mV0 =n_v0;
      }


  return hbtEvent;
}





















