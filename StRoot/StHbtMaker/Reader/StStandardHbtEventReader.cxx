/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.cxx,v 1.39 2003/05/16 21:30:18 magestro Exp $
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
 * Revision 1.39  2003/05/16 21:30:18  magestro
 * Removed obsolete include file
 *
 * Revision 1.38  2003/05/07 20:05:26  magestro
 * Removed StFlowTagMaker.h include
 *
 * Revision 1.37  2003/01/31 20:22:57  magestro
 * Small changes to eliminate compiler warnings
 *
 * Revision 1.36  2001/12/05 14:42:18  laue
 * updated for trigger(action)word and l3TriggerAlgorithm
 *
 * Revision 1.33  2001/06/21 19:18:42  laue
 * Modified Files: (to match the changed base classes)
 * 	StHbtAsciiReader.cxx StHbtAsciiReader.h
 * 	StHbtAssociationReader.cxx StHbtAssociationReader.h
 *  	StHbtBinaryReader.cxx StHbtBinaryReader.h
 *  	StHbtGstarTxtReader.cxx StHbtGstarTxtReader.h
 *  	StHbtStrangeMuDstEventReader.cxx
 *  	StHbtStrangeMuDstEventReader.h StStandardHbtEventReader.cxx
 * Added Files: new reader
 *  	StHbtTTreeReader.cxx StHbtTTreeReader.h
 *
 * Revision 1.32  2001/06/04 19:09:54  rcwells
 * Adding B-field, run number, and improved reaction plane functionality
 *
 * Revision 1.31  2001/05/25 23:24:01  lisa
 * Added in StHbtKink stuff
 *
 * Revision 1.30  2001/05/15 15:30:18  rcwells
 * Added magnetic field to StHbtEvent
 *
 * Revision 1.29  2001/04/25 18:08:16  perev
 * HPcorrs
 *
 * Revision 1.28  2001/02/08 22:38:26  laue
 * Reader can now switch between different track types: primary is default
 *
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
//#include "StuProbabilityPidAlgorithm.h" // new
#include <math.h>


#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StHbtMaker/Infrastructure/StHbtXiCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtKinkCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtXiCut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"  
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"

#include "StEventMaker/StEventMaker.h"

#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowAnalysisMaker/StFlowAnalysisMaker.h"
#include "StFlowMaker/StFlowSelection.h"

//#define STHBTDEBUG

#ifdef __ROOT__
ClassImp(StStandardHbtEventReader)
#endif

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


//__________________
StStandardHbtEventReader::StStandardHbtEventReader() : mTrackType(primary), mReadTracks(1), mReadV0s(1), mReadXis(1), mReadKinks(1) {
  mTheEventMaker=0;
  mTheV0Maker=0;
  mTheTagReader = 0;
  mReaderStatus = 0;  // "good"
  mFlowMaker = 0;
  mFlowAnalysisMaker = 0;
}
//__________________
StStandardHbtEventReader::~StStandardHbtEventReader(){
  if (mEventCut) delete mEventCut;
  if (mTrackCut) delete mTrackCut;
  if (mV0Cut) delete mV0Cut;
  if (mXiCut) delete mXiCut;
  if (mKinkCut) delete mKinkCut;
}
//__________________
StHbtString StStandardHbtEventReader::Report(){
  StHbtString temp = "\n This is the StStandardHbtEventReader\n";
  char ccc[100];
  sprintf(ccc," Track type is %d\n",mTrackType);
  temp += ccc;
  sprintf(ccc," mReadTracks is %d\n",mReadTracks);
  temp += ccc;
  sprintf(ccc," mReadV0s is %d\n",mReadV0s);
  temp += ccc;
  sprintf(ccc," mReadXis is %d\n",mReadXis);
  temp += ccc;
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
  temp += "\n---> XiCuts in Reader: ";
  if (mXiCut) {
    temp += mXiCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> KinkCuts in Reader: ";
  if (mKinkCut) {
    temp += mKinkCut->Report();
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
  if (!mTheTagReader) 
    cout << " StStandardHbtEventReader::ReturnHbtEvent() -  no tag reader " << endl;
  
  if (mTheTagReader) {
    cout << " StStandardHbtEventReader::ReturnHbtEvent() -  tag reader is switched on " << endl;    
    if (!mTheTagReader->EventMatch(rEvent->info()->runId() , rEvent->info()->id()) ) {
      cout << " StStandardHbtEventReader::ReturnHbtEvent() -  no tags for this event" << endl;
      return 0;
    }
  }
  

  StHbtEvent* hbtEvent = new StHbtEvent;
  unsigned int mult = rEvent->trackNodes().size();

  if ( rEvent->numberOfPrimaryVertices() != 1) {
    cout << " StStandardHbtEventReader::ReturnHbtEvent() -  rEvent->numberOfPrimaryVertices()=" << 
      rEvent->numberOfPrimaryVertices() << endl;
    delete hbtEvent;
    return 0;
  }
  StHbtThreeVector vp = rEvent->primaryVertex()->position();
  hbtEvent->SetPrimVertPos(vp);
  cout << " StStandardHbtEventReader::ReturnHbtEvent() - primary vertex : " << vp << endl;

  // Run number
  hbtEvent->SetRunNumber( rEvent->runId() );

  if ( rEvent->summary() ) {
    hbtEvent->SetMagneticField(rEvent->summary()->magneticField());
    cout <<" StStandardHbtEventReader - magnetic field " << hbtEvent->MagneticField() << endl;
    
  }
  else {
    cout << "StStandardHbtEventReader - no StEvent summary -> no magnetic field written to HbtEvent" << endl;
  }

  if ( rEvent->l0Trigger() ) {
    hbtEvent->SetTriggerWord(rEvent->l0Trigger()->triggerWord());
    hbtEvent->SetTriggerActionWord(rEvent->l0Trigger()->triggerActionWord());
  }
  else {
    cout << "StStandardHbtEventReader - no StEvent l0Trigger" << endl;
  }

  int nL3Processed,nL3Accept,nL3Build;
  unsigned int firedL3TriggerAlgorithm=0;
  if ( rEvent->l3Trigger() && rEvent->l3Trigger()->l3EventSummary()) {
    const StL3EventSummary* mL3EventSummary = rEvent->l3Trigger()->l3EventSummary();
      unsigned int nAlgorithms = mL3EventSummary->numberOfAlgorithms();
      cout << "Number of L3 algorithms for this run: " << nAlgorithms << endl;
      const StSPtrVecL3AlgorithmInfo& mL3AlgInfo = mL3EventSummary->algorithms();
      for (unsigned int i=0; i<nAlgorithms; i++) {
	int algId = mL3AlgInfo[i]->id();
	nL3Processed = mL3AlgInfo[i]->numberOfProcessedEvents();
	nL3Accept = mL3AlgInfo[i]->numberOfAcceptedEvents();
	nL3Build = mL3AlgInfo[i]->numberOfBuildEvents();
	if (mL3AlgInfo[i]->build()) cout << "**";
	cout << " alg id " << algId << ":\t #proc " << nL3Processed << "\t #accept " << nL3Accept << "\t #build " << nL3Build << endl;
      }
      
      // print triggered algorithms
      const StPtrVecL3AlgorithmInfo& mL3TriggerAlgInfo = mL3EventSummary->algorithmsAcceptingEvent();
      cout << "Number of L3 algorithms which triggered this event: " << mL3TriggerAlgInfo.size() << endl;
      cout << "triggered algorithms: ";
      for (unsigned int i=0; i<mL3TriggerAlgInfo.size(); i++) cout << mL3TriggerAlgInfo[i]->id() << "  ";
      cout << endl;

      firedL3TriggerAlgorithm=0;
      for (StPtrVecL3AlgorithmInfoConstIterator theIter = mL3TriggerAlgInfo.begin(); theIter != mL3TriggerAlgInfo.end(); ++theIter) {
	firedL3TriggerAlgorithm |= ( 1<<((*theIter)->id()) );
	cout << ((*theIter)->id()) << " " << firedL3TriggerAlgorithm << endl;
      }
      hbtEvent->SetL3TriggerAlgorithm(0,firedL3TriggerAlgorithm);
  }
  else {
    cout << "StStandardHbtEventReader - no StEvent l3Trigger" << endl;
  }
  cout <<  "StStandardHbtEventReader - done" << endl;


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
  StTrack* cTrack; // current track

  cout << "StStandardHbtReader::ReturnHbtEvent() - We have " << mult << " tracks to store - we skip tracks with nhits==0" << endl;

  StTpcDedxPidAlgorithm* PidAlgorithm = new StTpcDedxPidAlgorithm();

  if (!PidAlgorithm) cout << " StStandardHbtEventReader::ReturnHbtEvent() - Whoa!! No PidAlgorithm!! " << endl;

  // the following just point to particle definitions in StEvent
  //StElectron* Electron = StElectron::instance();
  //StPionPlus* Pion = StPionPlus::instance();
  //StKaonPlus* Kaon = StKaonPlus::instance();
  //StProton* Proton = StProton::instance();

  int iNoHits = 0;
  int iNoPidTraits = 0;
  int iFailedCut =0;
  int iNoBestGuess =0;
  int iBadFlag =0;
  int iTracks = 0;
  int iGoodTracks =0;
  int iWrongTrackType =0;

  // loop over all the tracks, accept only global
  for (unsigned int icount=0; icount<(unsigned int)mult; icount++){
    cTrack = rEvent->trackNodes()[icount]->track(mTrackType);
    if (cTrack) {
      iTracks++;
      if (cTrack->flag()>=0) iGoodTracks++;
#ifdef STHBTDEBUG
      cout << iTracks << " " << iGoodTracks << " : ";
      cout << cTrack->type() << " ";
      cout << cTrack->flag() << " ";
      cout << cTrack->key() << " ";
      cout << cTrack->impactParameter() << " ";
      cout << cTrack->numberOfPossiblePoints() << " ";
      cout << endl;
#endif 
    }
  }

  hbtEvent->SetNumberOfTracks(iTracks);
  hbtEvent->SetNumberOfGoodTracks(iGoodTracks);

  hbtEvent->SetUncorrectedNumberOfPositivePrimaries(uncorrectedNumberOfPositivePrimaries(*rEvent));
  hbtEvent->SetUncorrectedNumberOfNegativePrimaries(uncorrectedNumberOfNegativePrimaries(*rEvent));
  hbtEvent->SetEventNumber(rEvent->info()->id());
  
  if (rEvent->triggerDetectorCollection()) {
    StZdcTriggerDetector zdc = rEvent->triggerDetectorCollection()->zdc();
    hbtEvent->SetZdcAdcWest((int)zdc.adc(10)); // Zdc West sum attenuated
    hbtEvent->SetZdcAdcEast((int)zdc.adc(13)); // Zdc East sum attenuated
  }
  else {
    cout << "StStandardHbtEventReader::Return(...) - no triggerDetectorCollection " << endl;
  }

  /*
  // Randy commented this out since the RP is obtained from the FlowMaker now
  // Didn't remove in case someone found it useful
  if (mTheTagReader) {
    hbtEvent->SetEventNumber(mTheTagReader->tag("mEventNumber"));    
    // reaction plane from tags 
    StHbtThreeVector a( mTheTagReader->tag("qxa",1), mTheTagReader->tag("qya",1),0);
    StHbtThreeVector b( mTheTagReader->tag("qxb",1), mTheTagReader->tag("qyb",1),0);
    float reactionPlane = (a+b).phi();
    float reactionPlaneSubEventDifference = a.angle(b);
    cout << " reactionPlane : " << reactionPlane/3.1415927*180.;
    cout << " reactionPlaneSubEventDifference : " << reactionPlaneSubEventDifference/3.1415927*180. << endl;
    hbtEvent->SetReactionPlane(reactionPlane);
    hbtEvent->SetReactionPlaneSubEventDifference(reactionPlaneSubEventDifference);
  }
  */


  // For Aihong's pid
  //  StuProbabilityPidAlgorithm aihongsPid(*rEvent);
  if (mReadTracks) {
    for (unsigned int icount=0; icount<(unsigned int)mult; icount++){
#ifdef STHBTDEBUG
      cout << " track# " << icount;
      cout << "  -1- ";
#endif
      gTrack = rEvent->trackNodes()[icount]->track(global);
      pTrack = rEvent->trackNodes()[icount]->track(primary);
      cTrack = rEvent->trackNodes()[icount]->track(mTrackType);
      
      // don't make a hbtTrack if not a primary track
      if (!cTrack)            { iWrongTrackType++; continue; }
      if (cTrack->flag() < 0) { iBadFlag++; continue; }
      
      if (!gTrack) {
	cout << "StStandardHbtEventReader::Return(...) - no global track pointer !?! " << endl;
	continue;
      }
      
      // check number points in tpc
      if (!cTrack->detectorInfo()) {
	cout << "StStandardHbtEventReader::Return(...) - no detector info " << endl; 
	assert(0);
      }
      
      int nhits = cTrack->detectorInfo()->numberOfPoints(kTpcId);
      if (nhits==0) {
	iNoHits++;
#ifdef STHBTDEBUG
	cout << "No hits -- skipping track (because it crashes otherwise)" << endl;
#endif
	continue;
      }
      
      // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
      // pointers to track and pidTraits are set 
      StParticleDefinition* BestGuess = (StParticleDefinition*)cTrack->pidTraits(*PidAlgorithm);
      //    if (BestGuess) cout << "best guess for particle is " << BestGuess->name() << endl; //2dec9
      if (!BestGuess){
#ifdef STHBTDEBUG
	cout << " noGuess ";
#endif
	iNoBestGuess++;
	continue;
      }
      
#ifdef STHBTDEBUG
      cout << "Getting readty to instantiate new StHbtTrack " << endl;
#endif    
      
      // o.k., we got the track with all we need, let's create the StHbtTrack
      StHbtTrack* hbtTrack = new StHbtTrack(rEvent,cTrack);

      if (mTrackCut){
#ifdef STHBTDEBUG
      cout << " -cut- ";
      cout << endl;
#endif
	if (!(mTrackCut->Pass(hbtTrack))){                  // track failed - delete it and skip the push_back
	  iFailedCut++;
	  delete hbtTrack;
	  continue;
	}
      }
      
#ifdef STHBTDEBUG
      cout << " -p- ";
      cout << endl;
#endif
      hbtEvent->TrackCollection()->push_back(hbtTrack);
    }
  }
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i tracks of type %i           \n",iTracks,mTrackType);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i good tracks of type %i      \n",iGoodTracks,mTrackType);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i wrong type tracks \n",iWrongTrackType);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i no hits,            tracks skipped \n",iNoHits);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i no tpcPidTraits,    tracks skipped \n",iNoPidTraits);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i failed tracks cuts, track skipped \n",iFailedCut);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i bad flag,           track skipped \n",iBadFlag);
  printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i(%i) tracks pushed into collection \n",hbtEvent->TrackCollection()->size(),	 mult); 
  
  delete PidAlgorithm;
  //Now do v0 stuff
  
  //Pick up pointer v0 minidst maker
  //StV0MiniDstMaker* v0Maker = (StV0MiniDstMaker *) mTheV0Maker;
  StStrangeMuDstMaker* muDstMaker = (StStrangeMuDstMaker *) mTheV0Maker;

  /* **********************************************/
  /* now read the v0 from the StStrangeMuDstMaker */
  /* **********************************************/
  if( muDstMaker && mReadV0s ) {
    for( int i= 0; i < muDstMaker->GetNV0(); i++){
      StV0MuDst* v0FromMuDst = muDstMaker->GetV0(i);
      //v0FromMuDst->UpdateV0();
      StHbtV0* hbtV0 = new StHbtV0(*v0FromMuDst);
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
    printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i(%i) V0s pushed into collection \n",
	   hbtEvent->V0Collection()->size(),
	   muDstMaker->GetNV0());
  }

  /* **********************************************/
  /* now read the xi from the StStrangeMuDstMaker */
  /* **********************************************/
  if( muDstMaker && mReadXis ) {
    for( int i= 0; i < muDstMaker->GetNXi(); i++){
      StXiMuDst* xiFromMuDst = muDstMaker->GetXi(i);
      //xiFromMuDst->UpdateXi();
      StHbtXi* hbtXi = new StHbtXi(*xiFromMuDst);
      // By now, all track-wise information has been extracted and stored in hbtTrack
      // see if it passes any front-loaded event cut
      if (mXiCut){
	if (!(mXiCut->Pass(hbtXi))){                  // track failed - delete it and skip the push_back
	  delete hbtXi;
	  continue;
	}
      }
      hbtEvent->XiCollection()->push_back(hbtXi);
    } // end of loop over strangeness groups xi's
    printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i(%i) Xis pushed into collection \n",
	   hbtEvent->XiCollection()->size(),
	   muDstMaker->GetNXi());
  }


  /* *************************************/
  /* now read the kinks from the StEvent */
  /* *************************************/
  // Now do the Kink Stuff - mal 25May2001
  //StKinkVertex* starKink;
  StHbtKink* hbtKink;
  if( mReadKinks ) {
    for (unsigned int icount=0; icount<(unsigned int)rEvent->kinkVertices().size(); icount++)
      {
	StKinkVertex* starKink = rEvent->kinkVertices()[icount];
	hbtKink = new StHbtKink(*starKink, vp);
	// before pushing onto the StHbtKinkCollection, make sure this kink passes any front-loaded cuts...
	if (mKinkCut){
	  if (!(mKinkCut->Pass(hbtKink))){                  // kink failed - the "continue" will skip the push_back
	    delete hbtKink;
	    continue;
	  }
	}
	hbtEvent->KinkCollection()->push_back(hbtKink);
      }
    printf(" StStandardHbtEventReader::ReturnHbtEvent() - %8i(%i) Kinks pushed into collection \n",
	   hbtEvent->KinkCollection()->size(),
	   rEvent->kinkVertices().size());
  }

  // Can't get Reaction Plane until whole HbtEvent is filled
  if ( mFlowMaker && hbtEvent ) {
//     mFlowMaker->FillFlowEvent(hbtEvent);
//     // First get RP for whole event
//     mFlowMaker->FlowSelection()->SetSubevent(-1);
//     double reactionPlane = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
//     cout << "Reaction Plane " << reactionPlane << endl;
//     hbtEvent->SetReactionPlane(reactionPlane);
//     // Sub event RPs
//     mFlowMaker->FlowSelection()->SetSubevent(0);
//     double RP1 = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
//     mFlowMaker->FlowSelection()->SetSubevent(1);
//     double RP2 = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
//     hbtEvent->SetReactionPlaneSubEventDifference(RP1-RP2);
//     // if Flow Analysis is switched on ... make correction histogram
//     if (mFlowAnalysisMaker) mFlowAnalysisMaker->Make();
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





















