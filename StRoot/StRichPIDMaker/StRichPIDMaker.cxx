/******************************************************
 * $Id: StRichPIDMaker.cxx,v 1.5 2000/06/16 02:37:11 horsley Exp $
 * 
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRichPIDMaker.cxx,v $
 * Revision 1.5  2000/06/16 02:37:11  horsley
 * many additions, added features to pad plane display (MIPS, rings, etc)
 * along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *
 * modified StRichRings, StRichTDrawableRings to comply with sun compiler
 * d<3 adjustment and checkTrack() naming of varaiables
 * corrected
 *
 * Revision 2.12  2000/11/07 14:11:39  lasiuk
 * initCutParameters() and diagnositis print added.
 * bins for <d> and sigma_d added.
 * TPC hits for RICH tracks written out.
#include "StRichPIDMaker.h"
#include "StRichTDrawableTrack.h"
 * Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 * modified StRichRings, StRichDrawableTRings to comply with sun compiler
// switches
#define myRICH_WITH_PADMONITOR 1
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
#include "StRrsMaker/StRichPadMonitor.h"
#include "StRichMcSwitch.h"
#define myrICH_WITH_NTUPLE 1
#include "StRrsMaker/StRichPadMonitor.h"
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
#include "TNtuple.h"
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 1.5 2000/06/16 02:37:11 horsley Exp $";

Int_t 
StRichPIDMaker::Make() { 
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 1.5 2000/06/16 02:37:11 horsley Exp $";
  cout << "StRichPIDMaker::Make()" << endl;
  evtN++;
  
#ifdef myRICH_WITH_PADMONITOR
  StRichGeometryDb* mGeometryDb = StRichGeometryDb::getDb();  
  StRichPadMonitor* padMonitor = StRichPadMonitor::getInstance(mGeometryDb);
  padMonitor->drawEventNum(evtN);
  padMonitor->drawFileName(fileName);
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
  StThreeVectorD VertexPos(0,0,0);
  if (rEvent->primaryVertex()) {
    VertexPos = rEvent->primaryVertex()->position();}  
 
  // -------------------> oh NO!
  // this is terrible !!!!!!!!!!!!!!!!!!!
  double magField  = 5.0;
  if (rEvent->summary())
      magField  = rEvent->summary()->magneticField();
  else
      cout << "No Summery, Using magField = 5.0\n";
  
  // grab tracks intersecting RICH

  //   loop over tracks in MCEvent, 
  //   use associator to get geant id of each track, along with
  //   the geant info for the track
  
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
	  
	  
#ifdef myRICH_WITH_PADMONITOR
	  padMonitor->addTrack(tempTrack);
#endif
      }               
      
      else {
	  delete tempTrack;
      }
      
    }  // --> end of track loop  
  } // --> end of node loop  
  
  
  // loop over the selected RICH tracks
 

  return kStOK;
  StRichDrawableTRings* currentTRing  = 0;
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) { // track
    
      StRichRingCalculator* ringCalc = new StRichRingCalculator(mListOfStRichTracks[trackIndex]);
  
      if (mListOfStRichTracks[trackIndex]->getCharge() > 0)  { 
	  for (size_t i=0;i<mListOfPositiveParticles.size();i++) {
	      mListOfParticles[i] = mListOfPositiveParticles[i];
		hits[hitIndex]->getNSigma() < 2 ) {
      }
      
      else {
	  for (size_t i=0;i<mListOfNegativeParticles.size();i++) {
	  mListOfParticles[i] = mListOfNegativeParticles[i];
	}
      }
      
      const unsigned int sizeOfArray = 36;
  
      float tempArray[sizeOfArray];

      double noCut = 0; // In radians
      double firstCut = (M_PI/180.0) * 30.0;
      double secondCut = (M_PI/180.) * 60.0;
      double reserved = 0;

      int     totalHits     = 0;
      int     firstCutHits  = 0;
      int     secondCutHits = 0;

      double totalArea     = 0;
      double firstCutArea  = 0;
      double secondCutArea = 0;
	      
      // let initialize pid elements to zero
      for (unsigned int loopIndex=0;loopIndex<sizeOfArray;loopIndex++) {
	  tempArray[loopIndex] = 0;
      }
  
      
      // write to array track info
      if (mListOfStRichTracks[trackIndex]->getStTrack()
	  && mListOfStRichTracks[trackIndex]->getStTrack()->geometry() ) {
	  tempArray[0] = mListOfStRichTracks[trackIndex]->getStTrack()->geometry()->momentum().x();
	  tempArray[1] = mListOfStRichTracks[trackIndex]->getStTrack()->geometry()->momentum().y();
	  tempArray[2] = mListOfStRichTracks[trackIndex]->getStTrack()->geometry()->momentum().z();

	   tempArray[12] = mListOfStRichTracks[trackIndex]->getStTrack()->geometry()->helix().distance(VertexPos);
	   tempArray[13] = mListOfStRichTracks[trackIndex]->getStTrack()->detectorInfo()->hits(kTpcId).size();
      }
      tempArray[3] = mListOfStRichTracks[trackIndex]->getMomentum().x();
      tempArray[4] = mListOfStRichTracks[trackIndex]->getMomentum().y();
      tempArray[5] = mListOfStRichTracks[trackIndex]->getMomentum().z();
	StSPtrVecRichHitConstIterator hitIter;
      tempArray[6] = mListOfStRichTracks[trackIndex]->getImpactPoint().x();
      tempArray[7] = mListOfStRichTracks[trackIndex]->getImpactPoint().y();
	  
      tempArray[8] = mListOfStRichTracks[trackIndex]->getProjectedMIP().x();
      tempArray[9] = mListOfStRichTracks[trackIndex]->getProjectedMIP().y();

      tempArray[10] = mListOfStRichTracks[trackIndex]->getAssociatedMIP().x();
      tempArray[11] = mListOfStRichTracks[trackIndex]->getAssociatedMIP().y();

      tempArray[14] = reserved;
  
      
      for (size_t particleIndex=0; particleIndex<mListOfParticles.size(); particleIndex++) {
	  // particle
		  
	  if (mListOfStRichTracks[trackIndex] &&
	      mListOfParticles[particleIndex] &&
	      mListOfStRichTracks[trackIndex]->fastEnough(mListOfParticles[particleIndex])) {
	      // fast enough
	
	      ringCalc->setParticleType(mListOfParticles[particleIndex]);
	
	      totalHits     = 0;
	      firstCutHits  = 0;
	      secondCutHits = 0;
	      
	      totalArea     = ringCalc->calculateArea(noCut);
	      firstCutArea  = ringCalc->calculateArea(firstCut);
	      secondCutArea = ringCalc->calculateArea(secondCut);

	      if(pRichHits){
		  for (StSPtrVecRichHitIterator hitIndex = pRichHits->begin(); 
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
#ifdef myRICH_WITH_PADMONITOR
		      if(padMonitor->getTrack(mListOfStRichTracks[trackIndex])) {
			  
			  if(padMonitor->
			     getTrack(mListOfStRichTracks[trackIndex])->
			     getRing(mListOfParticles[particleIndex])) {
			      
			      padMonitor->
				  getTrack(mListOfStRichTracks[trackIndex])->
				  getRing(mListOfParticles[particleIndex])->
				  addHit((*hitIndex)->local().x(),(*hitIndex)->local().y());
			  }
		      }
#endif
		      
		  } // ---> hit filter
		  
	      } //  -----> loop over hits
	      } // --> check for valid pRichHits  		  
	  } //  -----> fast enough

	  // write to array pid info
	  if( (mListOfParticles[particleIndex] == pionplus) ||
	      (mListOfParticles[particleIndex] == pionminus)){

	      tempArray[15] = totalArea;
	      tempArray[16] = firstCutArea;
	      tempArray[17] = secondCutArea;
	      tempArray[18] = totalHits;
	      tempArray[19] = firstCutHits;
	      tempArray[20] = secondCutHits;
	  }
	  
	  if( (mListOfParticles[particleIndex] == kaonplus) ||
	      (mListOfParticles[particleIndex] == kaonminus)){
	      
	      tempArray[21] = totalArea;
	      tempArray[22] = firstCutArea;
	      tempArray[23] = secondCutArea;
	      tempArray[24] = totalHits;
	      tempArray[25] = firstCutHits;
	      tempArray[26] = secondCutHits;
	  }
	  
	  if( (mListOfParticles[particleIndex] == proton) ||
	      (mListOfParticles[particleIndex] == antiproton)){
	      
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
      
      mPidNtuple->Fill(tempArray);
      delete ringCalc;
  } // ---------> loop over tracks
  
void StRichPIDMaker::drawPadPlane(StEvent* rEvent, bool kCreatePsFile) {
  
  
#ifdef myRICH_WITH_PADMONITOR
  ////////////////////////////////////////////////////////
  //       lets draw some rings !           

  cerr << "Drawing Rings\n";
  
  padMonitor->drawRings();
  padMonitor->update();
  

  
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
    fileName = 0;
}

StRichPIDMaker::~StRichPIDMaker() {}

Int_t StRichPIDMaker::Init() {
      StSPtrVecRichPid thepids = pidTrait->getAllPids();
  evtN = 0;
      for (size_t pidcounter=0;pidcounter<thepids.size();pidcounter++) {
  file = new TFile("analysis.root","RECREATE");
  
  mPidNtuple =
      new TNtuple("mPidNtuple","Pid Info","globalpx:globalpy:globalpz:localpx:localpy:localpz:localx:localy:pmipx:pmipy:amipx:amipy:b:ntpchits:reserved:pia0:pia1:pia2:pih0:pih1:pih2:kaa0:kaa1:kaa2:kah0:kah1:kah2:pra0:pra1:pra2:prh0:prh1:prh2:cut0:cut1:cut2");
  

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
    file->Write();
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
  
    








