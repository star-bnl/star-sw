/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.cxx,v 1.20 2000/04/26 14:55:45 rcwells Exp $
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
 * Revision 1.20  2000/04/26 14:55:45  rcwells
 * Fixed filling of NHitsPossible in StStandardHbtEventsReader
 *
 * Revision 1.19  2000/04/03 16:22:07  laue
 * some include files changed
 *
 * Revision 1.18  2000/02/26 19:06:12  laue
 * Some unnecessary includes removed.
 * StThreeVectorD replace by StHbtThreeVector.
 * StHbtBinaryReader now can derive output filename from StIOMaker
 *
 * Revision 1.17  2000/02/18 22:01:56  laue
 * Implementation of a collections of StHbtEventWriters.
 * We now can write multiple microDsts at a time.
 *
 * All readers can have front-loaded cuts now. For that reason some
 * functionality was moved from the specific readers to the base class
 *
 * Revision 1.16  2000/02/01 00:35:29  laue
 * namespaces and other little things (see Thomas CC5 migration page) changed
 * to run on the new Solaris Compiler CC5
 *
 * Revision 1.15  2000/01/25 17:35:27  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.14  1999/12/03 22:24:37  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.13  1999/11/24 21:56:05  laue
 * a typo fixed ; ClassDef() was splitted by an accidental carriage-return
 * ----------------------------------------------------------------------
 *
 * Revision 1.12  1999/09/28 15:06:06  didenko
 * Cleanup dependencies on non existing h-files
 *
 * Revision 1.11  1999/09/24 01:23:14  fisyak
 * Reduced Include Path
 *
 * Revision 1.10  1999/09/17 22:38:03  lisa
 * first full integration of V0s into StHbt framework
 *
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
#define HBT_BFIELD 0.5*tesla
 
#include "StHbtMaker/Reader/StStandardHbtEventReader.h"
#include "StChain.h"
#include "TOrdCollection.h"


// these StEvent files keep oscillating between ".hh" and ".h" files
// fortunatey, they are used only here

#include "StEvent.h"
#include "StGlobalTrack.h"
// <<<<< #include "StTpcDedxPid.h"
// <<<<< #include "StDedxPid.h"
#include "StTrackNode.h"
#include "StContainers.h"
#include "StPrimaryVertex.h"
#include "StVertex.h"
#include "StMeasuredPoint.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StTrackGeometry.h"

#include "StParticleTypes.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StHit.h"

//#include <typeinfo>
#include <math.h>


/* .hh files
//   #include "StEvent/StEvent.hh"
//   #include "StEvent/StGlobalTrack.hh"
//   #include "StEvent/StTpcDedxPid.hh"
//   #include "StEvent/StDedxPid.hh"
*/
//
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StV0MiniDstMaker/StV0MiniDstMaker.h"  
#include "StV0MiniDstMaker/StV0MiniDst.hh"

#include "StEventMaker/StEventMaker.h"

#ifdef __ROOT__
ClassImp(StStandardHbtEventReader)
#endif

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


//__________________
StStandardHbtEventReader::StStandardHbtEventReader(){
  mTheEventMaker=0;
  mTheV0Maker=0;
  mReaderStatus = 0;  // "good"
  mV0=0;


}
//__________________
StStandardHbtEventReader::~StStandardHbtEventReader(){
  if (mEventCut) delete mEventCut;
  if (mTrackCut) delete mTrackCut;
  if (mV0Cut) delete mV0Cut;


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
  temp += "\n---> TrackCuts in Reader: ";
  if (mTrackCut) {
    temp += mTrackCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> V0Cuts in Reader: ";
  if (mV0Cut) {
    temp += mV0Cut->Report();
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

  //int mult = rEvent->trackCollection()->size();
  int mult = rEvent->trackNodes().size();
  hbtEvent->SetNumberOfTracks(mult);
  hbtEvent->SetNumberOfGoodTracks(mult);  // same for now
  if ( rEvent->numberOfPrimaryVertices() != 1) {
    delete hbtEvent;
    return 0;
  }
  StHbtThreeVector vp = rEvent->primaryVertex()->position();
  hbtEvent->SetPrimVertPos(vp);
  cout << " primary vertex : " << vp << endl;
 
  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
  if (mEventCut){
    if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
      delete hbtEvent;
      return 0;
    }
  }

  StTrack* rTrack;
  cout << "StStandardHbtReader::ReturnHbtEvent - We have " << mult << " tracks to store - we skip tracks with nhits==0" << endl;

  StTpcDedxPidAlgorithm* PidAlgorithm = new StTpcDedxPidAlgorithm();

  if (!PidAlgorithm) cout << "Whoa!! No PidAlgorithm!! " << endl;

  // the following just point to particle definitions in StEvent
  StElectron* Electron = StElectron::instance();
  StPionPlus* Pion = StPionPlus::instance();
  StKaonPlus* Kaon = StKaonPlus::instance();
  StProton* Proton = StProton::instance();

  int iNoGlobal = 0;
  int iNoHits = 0;
  int iNoPidTraits = 0;
  int iFailedCut =0;
  int iNoBestGuess =0;
  // loop over all the tracks, accept only global
  for (unsigned long int icount=0; icount<(unsigned long int)mult; icount++){


    //cout << " track# " << icount << endl;
    rTrack = rEvent->trackNodes()[icount]->track(global);


    // don't make a hbtTrack if not a global track
    if (!rTrack) {
      iNoGlobal++;
      //cout << "No global track -- skipping track" << endl;
      continue;
    }


    // check number points in tpc
    int nhits = rTrack->detectorInfo()->numberOfPoints(kTpcId);
    //cout << "nhits\t" << nhits << endl;
    if (nhits==0) {
      iNoHits++;
      //cout << "No hits -- skipping track (because it crashes otherwise)" << endl;
      continue;
    }
    // get dedxPidTraits
    //cout << " number of pidTraits " << rTrack->pidTraits().size();
    //cout << " number of pidTraits for tpc: " << rTrack->pidTraits(kTpcId).size() << endl;
    StTrackPidTraits* trackPidTraits=0; 
    size_t iPidTraitsCounter=0;

    //for ( int ihit = 0; ihit < rTrack->detectorInfo()->hits(kTpcId).size(); ihit++) {
    //  cout << rTrack->detectorInfo()->hits(kTpcId)[ihit]->position() << endl;
    //}

    do {
      trackPidTraits = rTrack->pidTraits(kTpcId)[iPidTraitsCounter];
      iPidTraitsCounter++;
    } while (iPidTraitsCounter < rTrack->pidTraits(kTpcId).size() && (!trackPidTraits) );
    if (!trackPidTraits) {
      iNoPidTraits++;
      //cout << " No dEdx information from Tpc- skipping track with " << nhits << " hits"<< endl;
      continue;
    }
    // SIMPLE WAY  const StDedxPidTraits* dedxPidTraits = (const StDedxPidTraits*)trackPidTraits;
    // ULLRICH WAY...
#if defined(__SUNPRO_CC)
    const StDedxPidTraits *dedxPidTraits = dynamic_cast<const StDedxPidTraits*>((const StDedxPidTraits*)trackPidTraits);
#else
    const StDedxPidTraits *dedxPidTraits = dynamic_cast<const StDedxPidTraits*>(trackPidTraits);
#endif    



    // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
    // pointers to track and pidTraits are set 
    //cout << "look for best guess " << endl;
    StParticleDefinition* BestGuess = (StParticleDefinition*)rTrack->pidTraits(*PidAlgorithm);
    //    if (BestGuess) cout << "best guess for particle is " << BestGuess->name() << endl; //2dec9
    
    if (!BestGuess){
      iNoBestGuess++;
      continue;
    }



    //cout << " dE/dx = " << dedxPidTraits->mean() << endl;

    // get fitTraits
    StTrackFitTraits fitTraits = rTrack->fitTraits();
    //cout << " got fitTraits " << endl;

    //cout << "Getting readty to instantiate new StHbtTrack " << endl;


    // o.k., we got the track with all we need, let's create the StHbtTrack
    StHbtTrack* hbtTrack = new StHbtTrack;
    //cout << "StHbtTrack instantiated " << endl;


    
    hbtTrack->SetTrackId(rTrack->key());

    hbtTrack->SetNHits(nhits);
    int nHitsPoss = rTrack->numberOfPossiblePoints(kTpcId);
    hbtTrack->SetNHitsPossible(nHitsPoss);

    float nsige = PidAlgorithm->numberOfSigma(Electron);
    //cout << "nsigpe\t\t" << nsigpe << endl;
    hbtTrack->SetNSigmaElectron(nsige);

    float nsigpi = PidAlgorithm->numberOfSigma(Pion);
    //cout << "nsigpi\t\t" << nsigpi << endl;
    hbtTrack->SetNSigmaPion(nsigpi);

    float nsigk = PidAlgorithm->numberOfSigma(Kaon);
    //cout << "nsigk\t\t\t" << nsigk << endl;
    hbtTrack->SetNSigmaKaon(nsigk);

    float nsigprot = PidAlgorithm->numberOfSigma(Proton);
    //cout << "nsigprot\t\t\t\t" << nsigprot << endl;
    hbtTrack->SetNSigmaProton(nsigprot);



    //cout << "Nsig electron,pion,kaon,proton : " << nsige << " " << nsigpi << " " << nsigk << " " << nsigprot << endl;
    
    float dEdx = dedxPidTraits->mean();
    //cout << "dEdx\t" << dEdx << endl; 
    hbtTrack->SetdEdx(dEdx);
    
    double pathlength = rTrack->geometry()->helix().pathLength(vp);
    //cout << "pathlength\t" << pathlength << endl;
    StHbtThreeVector p = rTrack->geometry()->helix().momentumAt(pathlength,HBT_BFIELD);
    //cout << "p: " << p << endl;
    hbtTrack->SetP(p);



    StHbtThreeVector  DCAxyz = rTrack->geometry()->helix().at(pathlength)-vp;
    //cout << "DCA\t\t" << DCAxyz << " " << DCAxyz.perp() << endl;
    hbtTrack->SetDCAxy( DCAxyz.perp() );
    hbtTrack->SetDCAz(  DCAxyz.z()  );

    hbtTrack->SetChiSquaredXY( rTrack->fitTraits().chi2(0) );
    hbtTrack->SetChiSquaredZ( rTrack->fitTraits().chi2(1) ); 

    StPhysicalHelixD  helix = rTrack->geometry()->helix();
    hbtTrack->SetHelix( helix );

    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    //cout << "pt\t\t\t" << pt << endl;
    //hbtTrack->SetPt(pt);
    
    hbtTrack->SetPt(pt);
    
    int charge = (rTrack->geometry()->charge());
    //cout << "charge\t\t\t\t" << charge << endl;
    hbtTrack->SetCharge(charge);
    
    hbtTrack->SetTopologyMap( 0, rTrack->topologyMap().data(0) );
    hbtTrack->SetTopologyMap( 1, rTrack->topologyMap().data(1) );

    //cout << "pushing..." <<endl;

    
    // By now, all track-wise information has been extracted and stored in hbtTrack
    // see if it passes any front-loaded event cut
    if (mTrackCut){
      if (!(mTrackCut->Pass(hbtTrack))){                  // track failed - delete it and skip the push_back
	iFailedCut++;
	delete hbtTrack;
	continue;
      }
    }

    hbtEvent->TrackCollection()->push_back(hbtTrack);
  }
  delete PidAlgorithm;

  printf("%8i non-global tracks skipped \n",iNoGlobal);
  printf("%8i tracks skipped because of nHits=0 \n",iNoHits);
  printf("%8i tracks skipped because of not tpcPidTraits \n",iNoPidTraits);
  printf("%8i tracks failed the track cuts \n",iFailedCut);
  printf("%8i(%i) tracks pushed into collection \n",hbtEvent->TrackCollection()->size(),mult); 

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

	// By now, all track-wise information has been extracted and stored in hbtTrack
	// see if it passes any front-loaded event cut
	if (mV0Cut){
	  if (!(mV0Cut->Pass(hbtV0))){                  // track failed - delete it and skip the push_back
	    delete hbtV0;
	    continue;
	  }
	}


	hbtEvent->V0Collection()->push_back(hbtV0);
	} // end of loop over strangeness groups v0's
	//Store total number of v0s in v0minidst so can start from there next time
        cout << "**** n_v0 = " << n_v0 << "**mV0"   << n_v0-mV0 << endl;        //  "       "
	mV0 =n_v0;
      }

      // There might be event cuts that modify the collections of Tracks or V0 in the event.
      // These cuts have to be done after the event is built. That's why we have the event cut
      // at this point for the second time.
      // An example of this kind of cuts will be an cut that removes spit tracks from the event.
      if (mEventCut){
	if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
	  delete hbtEvent;
	  return 0;
	}
      }


  return hbtEvent;
}





















