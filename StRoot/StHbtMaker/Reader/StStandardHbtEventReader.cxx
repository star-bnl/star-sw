/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.cxx,v 1.26 2000/10/17 17:25:23 laue Exp $
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
 * Revision 1.26  2000/10/17 17:25:23  laue
 * Added the dE/dx information for v0s
 *
 * Revision 1.25  2000/08/31 22:32:37  laue
 * Readers updated for new StHbtEvent version 3.
 *
 * Revision 1.24  2000/07/16 21:14:45  laue
 * StStandardHbtEventReader modified to read primary tracks only
 *
 * Some unnecessary includes removed.
 * Changes from StV0MiniDst to StStrangeMuDst
 *
 * Revision 1.22  2000/06/08 16:12:11  laue
 * StStandardHbtEventReader.cxx: Topology map for V0 fixed
 * StHbtMcEventReader.cxx:       V0 updated
 *
 * Revision 1.21  2000/05/25 21:04:30  laue
 * StStandarsHbtEventReader updated for the new StStrangMuDstMaker
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
#include "StHbtMaker/Reader/StStandardHbtEventReader.h"
#include "StChain.h"


#include "StEvent.h"
#include "StEventTypes.h"
#include "StEventUtilities/StuRefMult.hh"
#include "StEventSummary.h"
#include "StGlobalTrack.h"
#include "StTrackNode.h"
#include "StContainers.h"
#include "StPrimaryVertex.h"
#include "StVertex.h"
#include "StMeasuredPoint.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StTrackGeometry.h"
#include "StTrackDetectorInfo.h"
#include "StParticleTypes.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StHit.h"
#include "StEventInfo.h"
#include <math.h>


#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"  
#include "StStrangeMuDstMaker/StV0MuDst.hh"

#include "StEventMaker/StEventMaker.h"


#include "StFlowTagMaker/StFlowTagMaker.h"
#include "tables/St_FlowTag_Table.h"


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
  mTheTagReader = 0;
  mReaderStatus = 0;  // "good"
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
  cout << " StStandardHbtEventReader::ReturnHbtEvent()" << endl;

  /////////////////
  // get StEvent //
  /////////////////
  StEvent* rEvent = 0;

  if (mTheEventMaker) {  // an event maker was specified in the macro
    StEventMaker* tempMaker = (StEventMaker*) mTheEventMaker;
    rEvent = tempMaker->event();
  }
  else { // no event maker was specified, we assume that an event.root file was read 
    cout << " read from event.root file " << endl;
    rEvent = (StEvent *) GetInputDS("StEvent");
    cout << " read from event.root file " << endl;
  }
  if (!rEvent){
    cout << " StStandardHbtEventReader::ReturnHbtEvent() - No StEvent!!! " << endl;
    return 0;
  }

  /*
  StEventSummary* summary = rEvent->summary();
  if (!summary){
    cout << " StStandardHbtEventReader::ReturnHbtEvent() - No StEventSummary!!! " << endl;
    return 0;
  }
  */

  // if this event has no tags, then return
  if (!mTheTagReader) {
    cout << " StStandardHbtEventReader::ReturnHbtEvent() -  no tag reader " << endl;
    return 0;
  }
  if (!mTheTagReader->EventMatch(rEvent->info()->runId() , rEvent->info()->id()) ) {
    cout << " StStandardHbtEventReader::ReturnHbtEvent() -  no tags for this event" << endl;
    return 0;
  }


  StHbtEvent* hbtEvent = new StHbtEvent;
  int mult = rEvent->trackNodes().size();

  if ( rEvent->numberOfPrimaryVertices() != 1) {
    cout << " StStandardHbtEventReader::ReturnHbtEvent() -  rEvent->numberOfPrimaryVertices()=" << 
      rEvent->numberOfPrimaryVertices() << endl;
    delete hbtEvent;
    return 0;
  }
  StHbtThreeVector vp = rEvent->primaryVertex()->position();
  hbtEvent->SetPrimVertPos(vp);
  cout << " StStandardHbtEventReader::ReturnHbtEvent() - primary vertex : " << vp << endl;
 
  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
  if (mEventCut){
    if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
      delete hbtEvent;
      return 0;
    }
  }

  StTrack* pTrack; // primary
  StTrack* gTrack; // global
  cout << "StStandardHbtReader::ReturnHbtEvent() - We have " << mult << " tracks to store - we skip tracks with nhits==0" << endl;

  StTpcDedxPidAlgorithm* PidAlgorithm = new StTpcDedxPidAlgorithm();

  if (!PidAlgorithm) cout << " StStandardHbtEventReader::ReturnHbtEvent() - Whoa!! No PidAlgorithm!! " << endl;

  // the following just point to particle definitions in StEvent
  StElectron* Electron = StElectron::instance();
  StPionPlus* Pion = StPionPlus::instance();
  StKaonPlus* Kaon = StKaonPlus::instance();
  StProton* Proton = StProton::instance();

  int iNoPrimary = 0;
  int iNoGlobal = 0;
  int iNoHits = 0;
  int iNoPidTraits = 0;
  int iFailedCut =0;
  int iNoBestGuess =0;
  int iBadFlag =0;
  int iPrimary = 0;
  int iGoodPrimary = 0;

  int isPrimary = 1;

  // loop over all the tracks, accept only global
  for (unsigned long int icount=0; icount<(unsigned long int)mult; icount++){
    pTrack = rEvent->trackNodes()[icount]->track(primary);
    if (pTrack) {
      iPrimary++;
      if (pTrack->flag()>=0) { 
	iGoodPrimary++;
      }
    }
  }

  
  hbtEvent->SetNumberOfTracks(iPrimary);
  hbtEvent->SetNumberOfGoodTracks(iGoodPrimary);
  hbtEvent->SetUncorrectedNumberOfPositivePrimaries(0);
  hbtEvent->SetUncorrectedNumberOfNegativePrimaries(uncorrectedNumberOfNegativePrimaries(*rEvent));
  hbtEvent->SetEventNumber(mTheTagReader->tag("mEventNumber"));    

  // reaction plane from tags 
  StHbtThreeVector a( mTheTagReader->tag("qxa",1), mTheTagReader->tag("qya",1),0);
  StHbtThreeVector b( mTheTagReader->tag("qxb",1), mTheTagReader->tag("qyb",1),0);
  float reactionPlane = (a+b).phi();
  float reactionPlaneError = a.angle(b);
  cout << " reactionPlane : " << reactionPlane/3.1415927*180.;
  cout << " reactionPlaneError : " << reactionPlaneError/3.1415927*180. << endl;
  hbtEvent->SetReactionPlane(reactionPlane);
  hbtEvent->SetReactionPlaneError(reactionPlaneError);
  

  for (unsigned long int icount=0; icount<(unsigned long int)mult; icount++){
    //cout << " track# " << icount << endl;
    pTrack = rEvent->trackNodes()[icount]->track(primary);
    gTrack = rEvent->trackNodes()[icount]->track(global);


    // don't make a hbtTrack if not a primary track
    if (!pTrack) {
      iNoPrimary++;
      isPrimary = -1;
      continue;
    }
    if (pTrack->flag() < 0) {
      iBadFlag++;
      cout << " Flag < 0 -- skipping track" << endl;
      continue;
    }
    if (!gTrack) {
      iNoGlobal++;
      cout <<  " Primary track, but no global track -- skipping track" << endl;
      continue;
    }


    // check number points in tpc
    int nhits = pTrack->detectorInfo()->numberOfPoints(kTpcId);
    //cout << "nhits\t" << nhits << endl;
    if (nhits==0) {
      iNoHits++;
      //cout << "No hits -- skipping track (because it crashes otherwise)" << endl;
      continue;
    }
    // get dedxPidTraits
    //cout << " number of pidTraits " << pTrack->pidTraits().size();
    //cout << " number of pidTraits for tpc: " << pTrack->pidTraits(kTpcId).size() << endl;
    StTrackPidTraits* trackPidTraits=0; 
    size_t iPidTraitsCounter=0;

    //for ( int ihit = 0; ihit < pTrack->detectorInfo()->hits(kTpcId).size(); ihit++) {
    //  cout << pTrack->detectorInfo()->hits(kTpcId)[ihit]->position() << endl;
    //}

    do {
      trackPidTraits = pTrack->pidTraits(kTpcId)[iPidTraitsCounter];
      iPidTraitsCounter++;
    } while (iPidTraitsCounter < pTrack->pidTraits(kTpcId).size() && (!trackPidTraits) );
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
    StParticleDefinition* BestGuess = (StParticleDefinition*)pTrack->pidTraits(*PidAlgorithm);
    //    if (BestGuess) cout << "best guess for particle is " << BestGuess->name() << endl; //2dec9
    
    if (!BestGuess){
      iNoBestGuess++;
      continue;
    }



    //cout << " dE/dx = " << dedxPidTraits->mean() << endl;

    // get fitTraits
    StTrackFitTraits fitTraits = pTrack->fitTraits();
    //cout << " got fitTraits " << endl;

    //cout << "Getting readty to instantiate new StHbtTrack " << endl;


    // o.k., we got the track with all we need, let's create the StHbtTrack
    StHbtTrack* hbtTrack = new StHbtTrack;
    //cout << "StHbtTrack instantiated " << endl;


    
    hbtTrack->SetTrackId(pTrack->key());

    hbtTrack->SetNHits(nhits);

    float nsige = PidAlgorithm->numberOfSigma(Electron);
    //cout << "nsige\t\t" << nsige << endl;
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
    
    double pathlength = pTrack->geometry()->helix().pathLength(vp);
    //cout << "pathlength\t" << pathlength << endl;
    StHbtThreeVector p = pTrack->geometry()->momentum();
    //cout << "p: " << p << endl;
    hbtTrack->SetP(p);



    StHbtThreeVector  DCAxyz = pTrack->geometry()->helix().at(pathlength)-vp;
    //cout << "DCA\t\t" << DCAxyz << " " << DCAxyz.perp() << endl;
    hbtTrack->SetDCAxy( DCAxyz.perp() );
    hbtTrack->SetDCAz(  DCAxyz.z()  );

    hbtTrack->SetChiSquaredXY( pTrack->fitTraits().chi2(0) );
    hbtTrack->SetChiSquaredZ( pTrack->fitTraits().chi2(1) ); 

    StPhysicalHelixD  helix = pTrack->geometry()->helix();
    hbtTrack->SetHelix( helix );

    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    //cout << "pt\t\t\t" << pt << endl;
    //hbtTrack->SetPt(pt);
    
    hbtTrack->SetPt(pt);   // flag secondary tracks
    
    int charge = (pTrack->geometry()->charge());
    //cout << "charge\t\t\t\t" << charge << endl;
    hbtTrack->SetCharge(charge);
    
    hbtTrack->SetTopologyMap( 0, gTrack->topologyMap().data(0) ); // take map from globals
    hbtTrack->SetTopologyMap( 1, gTrack->topologyMap().data(1) ); // take map from globals

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

  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i non-primary,        tracks skipped \n",iNoPrimary);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i non-global,         tracks skipped \n",iNoGlobal);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i no hits,            tracks skipped \n",iNoHits);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i no tpcPidTraits,    tracks skipped \n",iNoPidTraits);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i failed tracks cuts, track skipped \n",iFailedCut);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i bad flag,           track skipped \n",iBadFlag);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i(%i) tracks pushed into collection \n",hbtEvent->TrackCollection()->size(),	 mult); 

  //Now do v0 stuff


  //Pick up pointer v0 minidst maker
  //StV0MiniDstMaker* v0Maker = (StV0MiniDstMaker *) mTheV0Maker;
  StStrangeMuDstMaker* v0Maker = (StStrangeMuDstMaker *) mTheV0Maker;
  if( ! v0Maker ) {
    cout << " StStandardHbtEventReader::ReturnHbtEvent() - Not doing v0 stuff" << endl;
    return hbtEvent; 
  }
  //Get collection
  
  for( int i= 0; i < v0Maker->GetNV0(); i++){
    StV0MuDst* v0FromMuDst = v0Maker->GetV0(i);
    //v0FromMuDst->UpdateV0();
    StHbtV0* hbtV0 = new StHbtV0;
    hbtV0->SetdecayLengthV0(v0FromMuDst->decayLengthV0());
    hbtV0->SetdecayVertexV0X(v0FromMuDst->decayVertexV0X());
    hbtV0->SetdecayVertexV0Y(v0FromMuDst->decayVertexV0Y());
    hbtV0->SetdecayVertexV0Z(v0FromMuDst->decayVertexV0Z());
    hbtV0->SetdcaV0Daughters(v0FromMuDst->dcaV0Daughters());
    hbtV0->SetdcaV0ToPrimVertex(v0FromMuDst->dcaV0ToPrimVertex());
    hbtV0->SetdcaPosToPrimVertex(v0FromMuDst->dcaPosToPrimVertex());
    hbtV0->SetdcaNegToPrimVertex(v0FromMuDst->dcaNegToPrimVertex());
    hbtV0->SetmomPosX(v0FromMuDst->momPosX());
    hbtV0->SetmomPosY(v0FromMuDst->momPosY());
    hbtV0->SetmomPosZ(v0FromMuDst->momPosZ());
    hbtV0->SetmomNegX(v0FromMuDst->momNegX());
    hbtV0->SetmomNegY(v0FromMuDst->momNegY());
    hbtV0->SetmomNegZ(v0FromMuDst->momNegZ());
#ifdef STHBTDEBUG
    cout << " hist pos ";
    cout << v0FromMuDst->topologyMapPos().numberOfHits(kTpcId); 
    cout << " hist neg ";
    cout << v0FromMuDst->topologyMapNeg().numberOfHits(kTpcId) << endl;
#endif
    hbtV0->SettpcHitsPos(v0FromMuDst->topologyMapPos().numberOfHits(kTpcId));
    hbtV0->SettpcHitsNeg(v0FromMuDst->topologyMapNeg().numberOfHits(kTpcId));
    hbtV0->SetTrackTopologyMapPos(0,v0FromMuDst->topologyMapPos().data(0));
    hbtV0->SetTrackTopologyMapPos(1,v0FromMuDst->topologyMapPos().data(1));
    hbtV0->SetTrackTopologyMapNeg(0,v0FromMuDst->topologyMapNeg().data(0));
    hbtV0->SetTrackTopologyMapNeg(1,v0FromMuDst->topologyMapNeg().data(1));
    hbtV0->SetkeyPos(v0FromMuDst->keyPos());
    hbtV0->SetkeyNeg(v0FromMuDst->keyNeg());
#ifdef STHBTDEBUG
    cout << " keyPos " << v0FromMuDst->keyPos() << endl;
    cout << " keyNeg " << v0FromMuDst->keyNeg() << endl;
#endif
    hbtV0->SetmomV0X(v0FromMuDst->momV0X());
    hbtV0->SetmomV0Y(v0FromMuDst->momV0Y());
    hbtV0->SetmomV0Z(v0FromMuDst->momV0Z());
#ifdef STHBTDEBUG
    cout << " alpha  ";
    cout << v0FromMuDst->alphaV0();
    cout << " ptArm  ";
    cout << v0FromMuDst->ptArmV0() << endl;
#endif
    hbtV0->SetalphaV0(v0FromMuDst->alphaV0());
    hbtV0->SetptArmV0(v0FromMuDst->ptArmV0());
    hbtV0->SeteLambda(v0FromMuDst->eLambda());
    hbtV0->SeteK0Short(v0FromMuDst->eK0Short());
    hbtV0->SetePosProton(v0FromMuDst->ePosProton());
    hbtV0->SetePosPion(v0FromMuDst->ePosPion());
    hbtV0->SeteNegPion(v0FromMuDst->eNegPion());
    hbtV0->SeteNegProton(v0FromMuDst->eNegProton());
    hbtV0->SetmassLambda(v0FromMuDst->massLambda());
    hbtV0->SetmassAntiLambda(v0FromMuDst->massAntiLambda());
    hbtV0->SetmassK0Short(v0FromMuDst->massK0Short());
    hbtV0->SetrapLambda(v0FromMuDst->rapLambda());
    hbtV0->SetrapK0Short(v0FromMuDst->rapK0Short());
    hbtV0->SetcTauLambda(v0FromMuDst->cTauLambda());
    hbtV0->SetcTauK0Short(v0FromMuDst->cTauK0Short());
    hbtV0->SetptV0(v0FromMuDst->ptV0());
    hbtV0->SetptotV0(v0FromMuDst->ptotV0());
    hbtV0->SetptPos(v0FromMuDst->ptPos());
    hbtV0->SetptotPos(v0FromMuDst->ptotPos());
    hbtV0->SetptNeg(v0FromMuDst->ptNeg());
    hbtV0->SetptotNeg(v0FromMuDst->ptotNeg());
    hbtV0->SetdedxPos(v0FromMuDst->dedxPos());
    hbtV0->SetdedxNeg(v0FromMuDst->dedxNeg());

    
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
  //Store total number of v0s in v0Mudst so can start from there next time
  cout << " StStandardHbtEventReader::ReturnHbtEvent() - " << hbtEvent->V0Collection()->size();
  cout << " V0s pushed in collection " << endl;
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i(%i) V0s pushed into collection \n",
	 hbtEvent->V0Collection()->size(),
	 v0Maker->GetNV0());

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





















