/******************************************************
 * $Id: StRichPIDMaker.cxx,v 2.1 2000/08/13 01:25:58 gans Exp $
 * 
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRichPIDMaker.cxx,v $
 * Revision 2.1  2000/08/13 01:25:58  gans
 * Added directory changing when using pidMaker->printCanvas("directory/")
 *
 * Revision 2.0  2000/08/09 16:26:19  gans
 * Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
 *
 * typo corrected.
 *
 * Revision 2.14  2000/11/14 22:32:50  lasiuk
 * order of setting flags corrected
 *
 * Revision 2.13  2000/11/07 14:30:58  lasiuk
 * d<3 adjustment and checkTrack() naming of varaiables
 * corrected
 *
 * Revision 2.12  2000/11/07 14:11:39  lasiuk
 * initCutParameters() and diagnositis print added.
 * bins for <d> and sigma_d added.
 * TPC hits for RICH tracks written out.
#include "StRichPIDMaker.h"
 * check flags on Hits instead of ADC value
 *
 * Revision 2.8  2000/10/19 06:12:52  horsley
#define rICH_WITH_PID_NTUPLE
#define myrICH_WITH_NTUPLE 1
 * *** empty log message ***
#include "StRichPIDMaker.h"
 * fixed bug in Minimization routine, included StMagF stuff (commented out)
// Pad monitor
 * Revision 2.2  2000/09/29 01:35:37  horsley

 * Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 * Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 * Revision 1.5  2000/06/16 02:37:11  horsley
 * many additions, added features to pad plane display (MIPS, rings, etc)
 * along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 * Revision 1.3  2000/05/22 15:14:44  horsley
 * modified StRichRings, StRichDrawableTRings to comply with sun compiler
// switches

#define myrICH_WITH_MC 1
 *
 * Revision 1.2  2000/05/19 19:06:10  horsley
 * many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
// StEvent, StarClassLibrary
 ******************************************************/
#include "StRichPIDMaker.h"

#include <algorithm>
#ifndef ST_NO_NAMESPACES
using std::min;
using std::max;
#endif

#include "StRichDisplayActivate.h"
#include "StRichTrackingControl.h"
#include "StRichMcSwitch.h"
#define myrICH_WITH_NTUPLE 1
#include "StEvent/StEventTypes.h"
#include "StEvent/StRichPidTraits.h"
  meanDistance  = ringCalculator->getMeanDistance(hit,meanAngle);
#ifdef myRICH_WITH_MC
  if (ringWidth==0) {return;}  
#ifdef myRICH_WITH_MC
		/// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! path length hard coded!!!!!!!!!
// g2t tables
// StRichPIDMaker
	cout << "\tWARNING! Cannot get B field from event->summary().  Use B= " << mMagField << endl;
// StRichPIDmaker
#include "StRichRingCalculator.h"
#include "StRichPIDTraits.h"
#include "StRichPIDAlgorithm.h"
#include "StRichRingDefinition.h"
// Its OK to leave StRichMCTrack.h in
#include "StRichTrack.h"
// StAssociationMaker
#ifdef myRICH_WITH_MC
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
#endif
#include "StRichTrackFilter.h"
#include "StRichMCTrack.h"
    if(!event->primaryVertex()) {
	cout << "\tERROR: No Vertex. Skipping..." << endl;
// StChain, etc...
#include "StChain.h"
#include "St_DataSet.h"
#include "StMessMgr.h"
#include "TNtuple.h"
#include "TH3.h"
  if ( fabs(extrapolatedPosition.x()) < (mGeometryDb->radiatorDimension().x() - mPadPlaneCut) &&
       fabs(extrapolatedPosition.y()) > (mPadPlaneCut) &&
// for finite
#include <math.h>
#ifdef SUN
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 2.1 2000/08/13 01:25:58 gans Exp $";

Int_t 
StRichPIDMaker::Make() { 
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 2.1 2000/08/13 01:25:58 gans Exp $";
  cout << "StRichPIDMaker::Make()" << endl;
  evtN++;
  
#ifdef RICH_WITH_PAD_MONITOR
  StRichGeometryDb* mGeometryDb = StRichGeometryDb::getDb();  
  StRichPadMonitor* padMonitor = StRichPadMonitor::getInstance(mGeometryDb);
  padMonitor->clearTracks();
#endif 


  // initialize  track list
  if (mListOfStRichTracks.size() > 0) {
    mListOfStRichTracks.clear();
    mListOfStRichTracks.resize(0);
  }

  if (mListOfStTracks.size() > 0) {
    mListOfStTracks.clear();
    mListOfStTracks.resize(0);
  }

  // grab StEvent
  cout << "about to instaniate Stevent" << endl;
  StEvent* rEvent;
  rEvent = (StEvent *) GetInputDS("StEvent");
  
  if (!rEvent) {
    cout << "No mEvent! Can not continue. " << endl;
    return kStOK; // If no event, we're done
  }
  
  mNumberOfRingHits=0;
  #ifdef myRICH_WITH_MC
  
#ifdef myRICH_WITH_MC
  #endif

  cout << "about to instaniate RichCollection" << endl;
  const StRichCollection* richCollection = rEvent->richCollection();
  const StSPtrVecRichHit* pRichHits = 0;
      const StSPtrVecRichCluster& richClusters = richCollection->getRichClusters();
  if (!richCollection) { cout << "No richCollection!" << endl;}
  
  cout << "about to get hits" << endl;
  if (richCollection) {
    cout << "collection ok" << endl;
      pRichClusters = &richClusters;
      cout << "hits ok " << endl;
    }

      /* cout << "Rich Hits " << richHits.size() << endl;
      int i=0;
      for (StSPtrVecRichHitIterator iter = richHits.begin(); iter != richHits.end(); ++iter) {	
	cerr << "I=" << i << endl;
	cout << "x = " << (*iter)->internal().x() << endl;
	cout << "y = " << 95-(*iter)->internal().y() << endl; 
	cout << "charge = " << (*iter)->charge() << endl;
	cout << "amplitude = " << (*iter)->maxAmplitude() << endl;
	i++;
	}*/
      
      const StSPtrVecRichHit& richHits = richCollection->getRichHits();
      pRichHits = &richHits;

  
  
// MC stuff
#ifdef myRICH_WITH_MC
  StAssociationMaker* assoc = 0;
  rcTpcHitMapType* theHitMap   = 0;
  rcTrackMapType*  theTrackMap = 0;
  mcV0MapType*     theMcV0Map  = 0;
  
  if(mEvent){
  // StAssociationMaker
  
  assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");
  
  cout << "now for the multi maps ..." << endl;
  // the Multimaps...
  theHitMap   = assoc->rcTpcHitMap();
  theTrackMap = assoc->rcTrackMap();
  theMcV0Map  = assoc->mcV0Map(); 

  if (!theHitMap) {
      cout << "----------WARNING----------\n"
	   << "No Hit Map found for this event!" << endl;
    return kStWarn;}

  }
#endif
  
  // get tracks intersecting RICH, make monte carlo associations 
  // First check whether the Primary Vertex is there at all.
  StThreeVectorF VertexPos(-999,-999,-999);
  if (kWriteNtuple) this->fillMcPhotonNtuple(mEvent,pRichClusters,pRichHits);
  if (rEvent->primaryVertex()) {
      VertexPos = rEvent->primaryVertex()->position();}  
  
  // -------------------> oh NO!
  // this is terrible !!!!!!!!!!!!!!!!!!!
  double magField  = 2.5;
  int nTracks = 0;
  int nPrimTracks = 0 ;
  double zVertex = -999;
  
  // grab tracks intersecting RICH
  
  //   loop over tracks in MCEvent, 
  //   use associator to get geant id of each track, along with
  //   the geant info for the track
  
  // L3 Stuff
#ifdef RICH_WITH_L3_TRACKS
  if(mUseL3Tracking){
      cout << "Getting L3 Tracks" << endl;
      StDAQReader*           mTheL3DataReader;//!
      EventReader*           mTheEventReader;//!
      L3_Reader* mTheL3Reader;//!
      St_DataSet*            mTheL3Data;//!
      mTheL3Data   = GetDataSet("StDAQReader");
      if(mTheL3Data) {
	  
	  mTheL3DataReader = (StDAQReader*)(mTheL3Data->GetObject());
	  if(mTheL3DataReader) {
	      //	    if (mTheL3DataReader->L3Present()) {
	      cout << "L3 Present" << endl;
	      mTheEventReader = mTheL3DataReader->getEventReader();
	      mTheL3Reader = getL3Reader(mTheEventReader);
	      
	      if (mTheL3Reader) {
		  GlobalTrackReader *l3gtr = mTheL3Reader->getGlobalTrackReader();
		  if (l3gtr) { 
		      nTracks = l3gtr->getNumberOfTracks();
		      globalTrack* track = l3gtr->getTrackList();
		      nPrimTracks = 0;
		      zVertex = findL3ZVertex(track,nTracks);
		      trackFilter.setZVertex(zVertex);
		      
		      for (int i=0; i< nTracks; ++i) {
			  StRichMCTrack* tempTrack = new StRichMCTrack(&(track[i]),magField); 
			  trackFilter.setTrack(tempTrack);
			  
			  if (trackFilter.fromPrimaryVertex(nPrimTracks) && trackFilter.trackAcceptable()){
			      
			      tempTrack->assignMIP(pRichHits);
			      mListOfStRichTracks.push_back(tempTrack);
			      
#ifdef RICH_WITH_PAD_MONITOR
			      padMonitor->addTrack(tempTrack);
#endif
			  }
			  else {
			      delete tempTrack;
			  }  
		      }
		  }
	      }
	  }
	  this->hitFilter(pRichHits,ringCalc);
  }

  
#endif // End L3 Stuff
  
  if(!mUseL3Tracking){
      if (rEvent->summary()){
	  magField  = rEvent->summary()->magneticField();
	  nTracks = rEvent->summary()->numberOfTracks();
	  nPrimTracks = rEvent->summary()->numberOfGoodPrimaryTracks();
	  cout << "Found Summary: MagField = " << magField << endl;
      }
      else
	  cout << "No Summary, Using magField = 2.5\n";
	  
      if (rEvent->numberOfPrimaryVertices() && rEvent->primaryVertex()) {
	  VertexPos = rEvent->primaryVertex()->position();
	  zVertex = VertexPos.z();
	  if( ! finite(zVertex))
	      zVertex=-999;
      } 

      nPrimTracks = numStPrimaryTracks(rEvent);
      
      StSPtrVecTrackNode& theTrackNodes = rEvent->trackNodes();
      
      for (size_t nodeIndex=0; nodeIndex<theTrackNodes.size(); nodeIndex++) {
	  size_t numberOfTracksInNode =  theTrackNodes[nodeIndex]->entries(global);
	  
	  for (size_t trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++)  {
	      
	      StRichMCTrack* tempTrack = new StRichMCTrack(theTrackNodes[nodeIndex]->track(global,trackIndex),magField); 
	      
	      trackFilter.setTrack(tempTrack);
	      
	      if (trackFilter.trackAcceptable()){
#ifdef myRICH_WITH_MC
		  if(mEvent){
		      unsigned int maxCommonTpcHits = 0;
		      unsigned int numberOfPartners = 0;
		      StMcTrack*   mcPartner = 0;
		      pair<rcTrackMapIter,rcTrackMapIter> trackBounds 
			  = theTrackMap->equal_range(dynamic_cast<StGlobalTrack*>(tempTrack->getStTrack()));
		      
		      for (rcTrackMapIter rcIt = trackBounds.first; rcIt != trackBounds.second; ++rcIt) {
			  numberOfPartners++;
			  if ((*rcIt).second->commonTpcHits() >  maxCommonTpcHits) {
			      mcPartner        = (*rcIt).second->partnerMcTrack();
			      maxCommonTpcHits = (*rcIt).second->commonTpcHits();
			  }
		      }
		      
		      tempTrack->setStMcTrack(mcPartner);
		      tempTrack->setCommonTpcHits(maxCommonTpcHits);
		      tempTrack->setNumberOfPartners(numberOfPartners);
		      tempTrack->setGeantPhotons(mEvent);
		  }
#endif
		  tempTrack->assignMIP(pRichHits);
		  mListOfStRichTracks.push_back(tempTrack);
		  
		  
#ifdef RICH_WITH_PAD_MONITOR
		  padMonitor->addTrack(tempTrack);
#endif
	      }               
	      
	      else {
		  delete tempTrack;
	      }
	      
	  }  // --> end of track loop  
      } // --> end of node loop  
  }
  // loop over the selected RICH tracks
  
#ifdef RICH_WITH_PAD_MONITOR
  cout << "zVertex = " << zVertex << endl;
  padMonitor->drawZVertex(zVertex,nPrimTracks,nTracks);
  
  int eventId = 0;
  int runId = 0;
  
  if(!mUseL3Tracking && rEvent){
      eventId = rEvent->id();
      runId = rEvent->runId();
      padMonitor->drawEventInfo(runId,eventId);}
  padMonitor->drawFileName(fileName);
  
#endif
  return kStOK;
#ifdef RICH_WITH_PID_NTUPLE  
  TFile * analFile = new TFile("analysis.root","NEW");
  TNtuple * PIDNtuple = 0;
  int appending = 0;
			theCurrentHit->setBit(eInAreap);
  cout << " i'm making nTuple" << endl;
  
  if( !analFile->IsOpen() ){
      cout << "in not open" << endl;
      delete analFile;
      analFile = new TFile("analysis.root","UPDATE");
      PIDNtuple = (TNtuple*)analFile->Get("PIDNtuple");
      appending = 1;

  else{
      cout << "making a new one" << endl;
      PIDNtuple =
	  new TNtuple("PIDNtuple","Pid Info","globalpx:globalpy:globalpz:localpx:localpy:localpz:localx:localy:pmipx:pmipy:amipx:amipy:b:ntpchits:q:pia0:pia1:pia2:pih0:pih1:pih2:kaa0:kaa1:kaa2:kah0:kah1:kah2:pra0:pra1:pra2:prh0:prh1:prh2:cut0:cut1:cut2:eta:zVertex"); 	
  }
#endif
void StRichPIDMaker::setFitPointsCut(int cut)    {mFitPointsCut = cut;}
  StRichDrawableTRings* currentTRing  = 0;
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) { // track
      	
	  StRichRingCalculator* ringCalc = new StRichRingCalculator(mListOfStRichTracks[trackIndex]);
	  
	  if (mListOfStRichTracks[trackIndex]->getCharge() > 0)  { 
	      for (size_t i=0;i<mListOfPositiveParticles.size();i++) {
		  mListOfParticles[i] = mListOfPositiveParticles[i];
	      }
	  }
	  
	  else {
	      for (size_t i=0;i<mListOfNegativeParticles.size();i++) {
		  mListOfParticles[i] = mListOfNegativeParticles[i];
	      }
	  }
	  const unsigned int sizeOfArray = 38;
	  
	  float tempArray[sizeOfArray] = {0};
	  
	  double noCut = 0; // In radians
	  double firstCut = (M_PI/180.0) * 30.0;
	  double secondCut = (M_PI/180.) * 60.0;
	  	  
	  int     totalHits     = 0;
	  int     firstCutHits  = 0;
	  int     secondCutHits = 0;
	  
	  double totalArea     = 0;
	  double firstCutArea  = 0;
	  double secondCutArea = 0;

	  StRichTrack * currentRichTrack = mListOfStRichTracks[trackIndex];
	  StTrack * currentStTrack = currentRichTrack->getStTrack();
	  StTrackGeometry * currentGeo = 0;
	  
	  // write to array track info
	  if ( currentStTrack
	      && (currentGeo = currentStTrack->geometry()) ) {
	  vector<StRichRingHit*> hits = track->getRingHits(particle);
	      StThreeVectorF currentStMomentum = currentGeo->momentum();
	      tempArray[0] = currentStMomentum.x();
	      tempArray[1] = currentStMomentum.y();
	      tempArray[2] = currentStMomentum.z();
	      tempArray[36] = currentStMomentum.pseudoRapidity();
	      tempArray[37] = zVertex;
	      tempArray[12] = currentGeo->helix().distance(VertexPos);
	      tempArray[13] = currentStTrack->detectorInfo()->hits(kTpcId).size();
		hits[hitIndex]->getNSigma() < 2 ) {
	  StThreeVector<double> tempVector = currentRichTrack->getMomentum();
	  tempArray[3] = tempVector.x();
	  tempArray[4] = tempVector.y();
	  tempArray[5] = tempVector.z();
#endif  
	  tempVector = currentRichTrack->getImpactPoint();
	  tempArray[6] = tempVector.x();
	  tempArray[7] = tempVector.y();

	  tempVector = currentRichTrack->getProjectedMIP();
	  tempArray[8] = tempVector.x();
	  tempArray[9] = tempVector.y();
void StRichPIDMaker::setPadPlaneCut(float cut)   {mPadPlaneCut = cut;}
	  tempVector = currentRichTrack->getAssociatedMIP();
	  tempArray[10] = tempVector.x();
	  tempArray[11] = tempVector.y();
				   int npri, const StSPtrVecRichHit* hits,
	  tempArray[14] = currentRichTrack->getCharge();

	  
	  
	  for (size_t particleIndex=0; particleIndex<mListOfParticles.size(); particleIndex++) {
	      // particle
	StSPtrVecRichHitConstIterator hitIter;
	  
	      StParticleDefinition* currentPart = mListOfParticles[particleIndex];
	      
	      if (currentRichTrack &&
		  currentPart &&
		  currentRichTrack->fastEnough(currentPart) &&
		  currentRichTrack->isGood(currentPart)) {
		  
		  // fast enough
		  ringCalc->setParticleType(currentPart);
		  
		  totalHits     = 0;
		  firstCutHits  = 0;
		  secondCutHits = 0;
		  
		  totalArea     = ringCalc->calculateArea(noCut);
		  firstCutArea  = ringCalc->calculateArea(firstCut);
		  secondCutArea = ringCalc->calculateArea(secondCut);
		  
		  if(pRichHits){
		      for (StSPtrVecRichHitConstIterator hitIndex = pRichHits->begin(); 
			   hitIndex != pRichHits->end();hitIndex++) {
			  // hits
			  
			  double ang   = 0.0;
			  double dist  = 0.0;
			  double meanD = 0.0;
			  
			  // hit filter
			  
			  StThreeVector<double> tempHit((*hitIndex)->local().x(),
							(*hitIndex)->local().y(),
							(*hitIndex)->local().z());
			  
			  if (hitFilter(tempHit,ringCalc,ang,dist,0.0,meanD)) {
			      // hit filter
			      totalHits++;
			      if(fabs(ang) > firstCut) {firstCutHits++;}
			      if(fabs(ang) > secondCut){secondCutHits++;}
			      
			      // hi-lite each hit found in ring
#ifdef RICH_WITH_PAD_MONITOR
			      StRichDrawableTTrack* currentTTrack = padMonitor->getTrack(currentRichTrack);
			      if(currentTTrack) {
				  StRichDrawableTRings * currentTRing = currentTTrack->getRing(currentPart);
				  if(currentTRing){
				      currentTRing->addHit((*hitIndex)->local().x(),(*hitIndex)->local().y());
				  }
			      }
#endif
			  }// ---> hit filter
			  
		      } //  -----> loop over hits
		      
		  } // --> check for valid pRichHits
		  
	      } //  -----> fast enough
	      // write to array pid info
	      if( (currentPart== pionplus) ||
		  (currentPart == pionminus)){
		  
		  tempArray[15] = totalArea;
		  tempArray[16] = firstCutArea;
		  tempArray[17] = secondCutArea;
		  tempArray[18] = totalHits;
		  tempArray[19] = firstCutHits;
		  tempArray[20] = secondCutHits;
	      }
	      
	      if( (currentPart == kaonplus) ||
		  (currentPart == kaonminus)){
		  
		  tempArray[21] = totalArea;
		  tempArray[22] = firstCutArea;
		  tempArray[23] = secondCutArea;
		  tempArray[24] = totalHits;
		  tempArray[25] = firstCutHits;
		  tempArray[26] = secondCutHits;
	      }
	      
	      if( (currentPart == proton) ||
		  (currentPart == antiproton)){
		  
		  tempArray[27] = totalArea;
		  tempArray[28] = firstCutArea;
		  tempArray[29] = secondCutArea;
		  tempArray[30] = totalHits;
		  tempArray[31] = firstCutHits;
		  tempArray[32] = secondCutHits;
	      }
	      
	      
	  } //  -------> loop over particles
	  
	  
	  // write to array cut definitions
	  tempArray[33] = noCut;
	  tempArray[34] = firstCut;
	  tempArray[35] = secondCut;
#ifdef RICH_WITH_PID_NTUPLE	  
	  PIDNtuple->Fill(tempArray);
#endif	 
	  delete ringCalc;
  } // ---------> loop over tracks
  
#ifdef RICH_WITH_PID_NTUPLE  
  if(appending)
      PIDNtuple->Write("PIDNtuple",TObject::kOverwrite);
  else
      PIDNtuple->Write();
void StRichPIDMaker::drawPadPlane(StEvent* rEvent, bool kCreatePsFile) {
  analFile->Close();
  delete analFile;  
#endif
  
#ifdef RICH_WITH_PAD_MONITOR
  ////////////////////////////////////////////////////////
  //       lets draw some rings !           
  
  cerr << "Drawing Rings\n";
  
  padMonitor->drawRings();
  padMonitor->update();
  if(mUsePrintCanvas){
      padMonitor->printCanvas(mUsePrintCanvasDir,fileName,evtN);
  }
  if(mUseResidNTup){
      padMonitor->doResiduals(zVertex,mPrimaryTracksVEta,runId,eventId);
  }
#endif
  if (rEvent->primaryVertex()) {
  // lets make sure all new's meet up with delete
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
    delete mListOfStRichTracks[trackIndex];
  for (size_t trackIndex=0;trackIndex<mListOfStRichTracks.size();trackIndex++) {
  mListOfStRichTracks.clear();
    mPadMonitor->addTrack(mListOfStRichTracks[trackIndex]);
  return kStOK;
  mPadMonitor->update();  
  if (kCreatePsFile) mPadMonitor->printCanvas("/star/rcf/scratch/horsley/",fileName,rEvent->id());    
#endif 
StRichPIDMaker::StRichPIDMaker(const Char_t *name) : StMaker(name) {
    drawinit = kFALSE;
    mUseL3Tracking = 0; // 0 == StEvent , 1 == L3
    mUsePrintCanvas = 0; // 0 == Print Each Event 1== Don't
    mUseResidNTup = 0;
    fileName = 0;
}

StRichPIDMaker::~StRichPIDMaker() {}

Int_t StRichPIDMaker::Init() {
      StSPtrVecRichPid thepids = pidTrait->getAllPids();
  evtN = 0;
      for (size_t pidcounter=0;pidcounter<thepids.size();pidcounter++) {
  for(int i = 0;i<10;i++)
      mPrimaryTracksVEta[i] = 0;
  /*
  file = new TFile("analysis.root","RECREATE");
  
  mPidNtuple =
      new TNtuple("mPidNtuple","Pid Info","globalpx:globalpy:globalpz:localpx:localpy:localpz:localx:localy:pmipx:pmipy:amipx:amipy:b:ntpchits:reserved:pia0:pia1:pia2:pih0:pih1:pih2:kaa0:kaa1:kaa2:kah0:kah1:kah2:pra0:pra1:pra2:prh0:prh1:prh2:cut0:cut1:cut2");
  
  */
 // define negatively charged particles 
  pionminus   = StPionMinus::instance();
  kaonminus   = StKaonMinus::instance();
  antiproton  = StAntiProton::instance();

  mListOfNegativeParticles.resize(3);
  mListOfNegativeParticles[0] = pionminus;
  mListOfNegativeParticles[1] = kaonminus;
  mListOfNegativeParticles[2] = antiproton;  
  
  // define positively charged particles 
  pionplus   = StPionPlus::instance();
  kaonplus   = StKaonPlus::instance();
  proton     = StProton::instance();  
  
  mListOfPositiveParticles.resize(3);
  mListOfPositiveParticles[0] = pionplus;
  mListOfPositiveParticles[1] = kaonplus;
  mListOfPositiveParticles[2] = proton; 
  
  mListOfParticles.resize(mListOfPositiveParticles.size());
  return StMaker::Init();
}
	if (thepids[pidcounter]->getRingType()==pion)  {
	  } 
void StRichPIDMaker::Clear(Option_t *opt) {
  StMaker::Clear();
}
  double adcsum,x,y,p,maxAmp,theta,quartz,rad,ang;
	  track = mListOfStRichTracks[trackIndex];
Int_t StRichPIDMaker::Finish() {
    //file->Write();
    cout << "in Finish();";
    return kStOK;
}
	  p = track->getMomentum().mag();
	  theta = track->getTheta()/degree;
vector<StRichTrack* >& StRichPIDMaker::getListOfStRichTracks() {
  return mListOfStRichTracks;
}
	    clusterFromTrack=10;
      pid->setMipResidual(residual); 
vector<StTrack* >& StRichPIDMaker::getListOfStTracks() {
  return mListOfStTracks;
      track->getPidTrait()->addPid(pid);
    }       

Int_t StRichPIDMaker::hitFilter(StThreeVector<double>& hit, 
				StRichRingCalculator* ringCalculator,
				double& ang, double& dist, double cut, double& meanD) {

  // calculate distance from inner,mean, outer rings
  ringCalculator->clear();  
  double meanAngle = 0;
  innerDistance = ringCalculator->getInnerDistance(hit,innerAngle);
  outerDistance = ringCalculator->getOuterDistance(hit,outerAngle);
  double meanDistance  = ringCalculator->getMeanDistance(hit,meanAngle);
	      tpchits = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
  StThreeVector<double> innerPoint = ringCalculator->getInnerRingPoint();
  StThreeVector<double> outerPoint = ringCalculator->getOuterRingPoint();
  StThreeVector<double> meanPoint  = ringCalculator->getMeanRingPoint();
	    }
  ringWidth = (innerPoint-outerPoint).mag();
  dist  = innerDistance/ringWidth;

  meanD = (meanDistance/ringWidth);
  ang = meanAngle;
  
  // make cuts
  if (innerDistance/ringWidth < 1.0 && outerDistance/ringWidth < 1.0) {
    if (fabs(ang) > cut ) {  return 1;}

  
  return 0;
	float array[16];
	array[1] = adcsum;
void StRichPIDMaker::setFileName(char * name){
    fileName = name;}
	array[2] = clusterFromTrack;
void StRichPIDMaker::useL3Tracking(){
    mUseL3Tracking = 1;}
	array[3] = x;
void StRichPIDMaker::usePrintCanvas(const char * directory){
    mUsePrintCanvas = 1;
    sprintf(mUsePrintCanvasDir,"%s",directory);}
	
void StRichPIDMaker::useResidNTup(){
    mUseResidNTup = 1;}

#ifdef RICH_WITH_L3_TRACKS
double StRichPIDMaker::findL3ZVertex(globalTrack * trackVec,int nTracks){

    TH1D temp("temp","vertex",500,-100,100);
    for(int i = 0;i<nTracks;i++){
	double currentVertex;
	currentVertex = trackVec[i].z0-trackVec[i].tanl*trackVec[i].r0*cos(trackVec[i].psi-trackVec[i].phi0);
	temp.Fill(currentVertex);
	  mListOfStRichTracks[trackIndex]->getPidTrait()->getPid(pion)) {

    
    return temp.GetBinCenter(temp.GetMaximumBin());
    
      }

      vector<StMcRichHit*> tempHits = richMcTrack->getGeantPhotons();
long StRichPIDMaker::numStPrimaryTracks(StEvent * rEvent){
  
    if(!rEvent){
	cout << "no rEvent" << endl;
	return -1;
    }

    for(unsigned int i =0; i < 10;i++) // clear array
	mPrimaryTracksVEta[i] = 0;

    StPrimaryVertex * primVert = rEvent->primaryVertex();

    if(primVert){
	for(unsigned int i = 0; i < primVert->numberOfDaughters();i++)
	    {
		StTrack * currentTrack = primVert->daughter(i);
		StTrackGeometry * currentGeo = currentTrack->geometry();
		if( currentTrack->flag()>0 &&
		    currentTrack->fitTraits().numberOfFitPoints() >= 10 &&
		    currentGeo->charge() < 0 &&
		    currentGeo->helix().distance(primVert->position()) < 3){
		    
		    float eta = currentGeo->momentum().pseudoRapidity();
		    
		    if(eta<-1)
			eta = -1.1;
		    else if(eta>1)
			eta = 1.1;
		    
		    mPrimaryTracksVEta[static_cast<long>(floor(4+ eta*4))]++;
		}
	    if (testThisResidual.perp() < smallestResidual && (*hitIter)->charge() > adcCut) {

	richMcTrack->getStTrack()->node()->track(global)) {
    int primCounter = 0;
    // This will return total count of tracks between -1 and 1
  StThreeVectorF 
    for(unsigned int i = 3;i < 7;i++)
	primCounter+=mPrimaryTracksVEta[i];
    
    return primCounter;
			 sin(rotationTheta)*trackLocalMomentum.z(),
  // photon case  --> get point on pad plane
				  mcTrack->richHits()[0]->position().z());
    
    (*mCoordinateTransformation)(testGlobal,testLocalPoint);
    
    hit.setY(testLocalPoint.position().y());
    hit.setZ(testLocalPoint.position().z());
  }

  // charged particle case    ---> get point on anode wire
  else {
    for (size_t i=0;i<mcTrack->richHits().size();i++) {

       StGlobalCoordinate testGlobal(mcTrack->richHits()[i]->position().x(),
				     mcTrack->richHits()[i]->position().y(),
				     mcTrack->richHits()[i]->position().z());
      
      StRichLocalCoordinate testLocalPoint(0,0,0);
      (*mCoordinateTransformation)(testGlobal,testLocalPoint);

      tempHit.setX(testLocalPoint.position().x());
      tempHit.setY(testLocalPoint.position().y());
      tempHit.setZ(testLocalPoint.position().z());
  
      // geant mip at anode wire position      
      if ( fabs(tempHit.z()) > 0.00 && fabs(tempHit.z()) < 2.0*anodeDistanceToPadPlane ) {
	if ( (tempHit-inputHit).perp() < (hit-inputHit).perp() ) {hit = tempHit;}
      } 
    }  
  }

  return hit;
}

#endif

#ifdef RICH_WITH_L3_TRACKS
double StRichPIDMaker::findL3ZVertex(globalTrack * trackVec,int nTracks){

    TH1D temp("temp","vertex",500,-100,100);
    for(int i = 0;i<nTracks;i++){
	double currentVertex;
	currentVertex = trackVec[i].z0-trackVec[i].tanl*trackVec[i].r0*cos(trackVec[i].psi-trackVec[i].phi0);
	temp.Fill(currentVertex);
    }

    return temp.GetBinCenter(temp.GetMaximumBin());    
}
#endif




ClassImp(StRichPIDMaker)
  
    








