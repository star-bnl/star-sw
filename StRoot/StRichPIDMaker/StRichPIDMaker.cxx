/******************************************************
 * $Id: StRichPIDMaker.cxx,v 1.2 2000/05/19 19:06:10 horsley Exp $
 * 
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRichPIDMaker.cxx,v $
 * Revision 1.2  2000/05/19 19:06:10  horsley
 * many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 * corrected
 *
 * Revision 2.12  2000/11/07 14:11:39  lasiuk
 * initCutParameters() and diagnositis print added.
 * bins for <d> and sigma_d added.
 * TPC hits for RICH tracks written out.
#include "StRichPIDMaker.h"
 * modified StRichRings, StRichDrawableTRings to comply with sun compiler
// switches
#define myrICH_WITH_PADMONITOR 1
 *
 * Revision 1.2  2000/05/19 19:06:10  horsley
 * many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
#include "StEventTypes.h"

#include "StTpcDedxPidAlgorithm.h"
#include "StThreeVectorF.hh"
#include <algorithm>
#include "StPhysicalHelixD.hh"
#ifndef ST_NO_NAMESPACES
#include "StMemoryInfo.hh"
#include "StRichDisplayActivate.h"
#include "StRichTrackingControl.h"
#include "StRrsMaker/StRichPadMonitor.h"
#include "StRichMcSwitch.h"
#define myrICH_WITH_NTUPLE 1
#include "StRrsMaker/StRichPadMonitor.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StRichPidTraits.h"
// g2t tables
// StRichPIDMaker
	cout << "\tWARNING! Cannot get B field from event->summary().  Use B= " << mMagField << endl;
// StRichPIDmaker
#include "StRichRingCalculator.h"
#include "StRichPIDTraits.h"
#include "StRichPIDAlgorithm.h"
#include "StRichTrack.h"
#include "StRichTrackFilter.h"
#include "StRichMCTrack.h"
    if(!event->primaryVertex()) {
	cout << "\tERROR: No Vertex. Skipping..." << endl;
// StChain, etc...
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 1.2 2000/05/19 19:06:10 horsley Exp $";
#include "St_DataSet.h"
#include "TNtuple.h"
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 1.2 2000/05/19 19:06:10 horsley Exp $";

Int_t 
StRichPIDMaker::Make() { 
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 1.2 2000/05/19 19:06:10 horsley Exp $";
  evtN++;
  StMemoryInfo* memory = StMemoryInfo::instance();
 
  // StEvent
  StEvent* rEvent = 0;
      cout << "no StEvent!" << endl;
  rEvent = (StEvent *) GetInputDS("StEvent");
  
  if (!rEvent)  {
      return kStWarn;
  }
    

#ifdef myRICH_WITH_PADMONITOR
  StRichGeometryDb* mGeometryDb = StRichGeometryDb::getDb();  
  StRichPadMonitor* padMonitor = StRichPadMonitor::getInstance(mGeometryDb);
  padMonitor->getParticleNames();   
  padMonitor->clearRingList();
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


  //  get objects associated with this event
    cout << "StRichPIDMaker::St_objectSet ptr is zero!" << endl; 
  St_ObjectSet *rchEvent = 0;
  rchEvent = (St_ObjectSet*)GetDataSet("richHits");
  if(!rchEvent) { 
  

  if(!mRichHits) { 
    cout << "StRichPIDMaker::hit collection ptr is zero!" << endl; 
  mRichHits = 0;
  mRichHits = (StRichSimpleHitCollection*)(rchEvent->GetObject());
  if(!mRichHits) {
    return kStWarn;
  cout << "starting analysis ..." << endl;
  }
  
  // get tracks intersecting RICH, make monte carlo associations 
  // First check whether the Primary Vertex is there at all.
    cout << "Position of Primary Vertex from StEvent:" << endl;
    cout << VertexPos << endl;
  StThreeVectorD VertexPos(0,0,0);
  if (rEvent->primaryVertex()) {
    VertexPos = rEvent->primaryVertex()->position();
  else {
    cout << "----------- WARNING ------------" << endl;
    cout << "No primary vertex from StEvent!!" << endl;
    cout << "Assume it is at (0,0,0)         " << endl;
  }

    

  
     
  StEvent& ev = *rEvent;
  double magField  = ev.summary()->magneticField();

  int occ = 0;
  if (ev.richPixelCollection()) {
    occ = ev.richPixelCollection()->size();
  } 

  
  // grab tracks intersecting RICH
    
  StSPtrVecTrackNode& theTrackNodes = ev.trackNodes();
  for (size_t nodeIndex=0; nodeIndex<theTrackNodes.size(); nodeIndex++) {
    size_t numberOfTracksInNode =  theTrackNodes[nodeIndex]->entries(global);
    for (size_t trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++)  {
      StRichTrack* tempTrack = new StRichTrack(theTrackNodes[nodeIndex]->track(global,trackIndex),magField); 
      trackFilter.setTrack(tempTrack);
      
      if (trackFilter.trackAcceptable())  { 
	mListOfStRichTracks.push_back(tempTrack);
      }               
	  
      else {
	delete tempTrack;
      }
      
    }  // --> end of track loop  
  } // --> end of node loop  
  cout << "looping over tracks" << endl;
  cout << "_________________________________" << endl << endl;  
  
  // loop over the selected RICH tracks
      cout << "track number::  " << trackIndex << endl;
      cout << "_____________________________________" << endl << endl;
      cout << "N tpc points = " << mListOfStRichTracks[trackIndex]->getStTrack()->fitTraits().numberOfFitPoints();
      cout << "impact parameter b = " << mListOfStRichTracks[trackIndex]->getStTrack()->geometry()->helix().distance(VertexPos);

  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) 
    { // track
    
      StRichRingCalculator ringCalc(mListOfStRichTracks[trackIndex]);

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
      
      // loop over cut angles
	cout << "        angle cut:: " << cut/degree << endl;
	cout << "         ____________________________________" << endl;
	
	double cut  = M_PI*(((double) jj)/180.0);      
	
	
	  float part_area[3] = {0,0,0};
	  float part_hits[3] = {0,0,0};
	      cout << "         particle = " << mListOfParticles[particleIndex]->name() << endl;
	  for (size_t particleIndex=0; particleIndex<mListOfParticles.size(); particleIndex++) 
	    { // particle
	      
	      if (mListOfStRichTracks[trackIndex]->fastEnough(mListOfParticles[particleIndex])) 
		{ // fast enough
		
		  ringCalc.setParticleType(mListOfParticles[particleIndex]);
		  int     hits = 0;
		  double chiSqd = 0.0;
		  double totalArea = ringCalc.calculateArea(cut);
		 
		  // loop over hits
		  for (size_t hitIndex=0; hitIndex<mRichHits->mTheHits.size(); hitIndex++) 
		    { // hits
		      
		      double ang   = 0.0;
		      double dist  = 0.0;
		      double meanD = 0.0;
		      
		      // hit filter
		      if (hitFilter(mRichHits->mTheHits[hitIndex]->local(),ringCalc,ang,dist,cut,meanD)) 
			{ // hit filter
			  
			  // hi-lite each hit found in ring
#ifdef myRICH_WITH_PADMONITOR
			  padMonitor->redrawHit(mRichHits->mTheHits[hitIndex],mListOfParticles[particleIndex]);
#endif
			  
			  hits++;
			  chiSqd += meanD;
			  //double b = mListOfStRichTracks[trackIndex]->getStTrack()->geometry()->helix().distance(VertexPos);
			 
			} // ---> hit filter
		    } //  -----> loop over hits
		  
		  part_area[particleIndex] = totalArea;
		  part_hits[particleIndex] = hits;
		  part_chi[particleIndex]  = chiSqd;
		  
		} //  -----> fast enough
	      
	    } //  -------> loop over particles
	  
	  	
	} //  -------> loop over angle cuts 
     
      
    } // ---------> loop over tracks
  
void StRichPIDMaker::drawPadPlane(StEvent* rEvent, bool kCreatePsFile) {
  
  
#ifdef myRICH_WITH_PADMONITOR
  ////////////////////////////////////////////////////////
  //       lets draw some rings !           
  
  padMonitor->update();
  
  
  for (unsigned int trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
    
    StRichTrack* tt = mListOfStRichTracks[trackIndex];
    StRichMCTrack* mcT = (StRichMCTrack*) tt;
    StRichRings bestRings(mListOfStRichTracks[trackIndex], 
			  mcT->getStMcTrack()->particleDefinition());  
    padMonitor->drawRings(bestRings,1);
    padMonitor->update();}
#endif
  if (rEvent->primaryVertex()) {

  // lets make sure all new's meet up with delete
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
    delete mListOfStRichTracks[trackIndex];
  for (size_t trackIndex=0;trackIndex<mListOfStRichTracks.size();trackIndex++) {
  mListOfStRichTracks.clear();
    mPadMonitor->addTrack(mListOfStRichTracks[trackIndex]);

  int enoughEventsToWriteFile = 10;
  memory->snapshot();
  memory->print();
  if (evtN%enoughEventsToWriteFile == 0) file->Write();
  return kStOK;
  mPadMonitor->update();  
  if (kCreatePsFile) mPadMonitor->printCanvas("/star/rcf/scratch/horsley/",fileName,rEvent->id());    
#endif 
StRichPIDMaker::StRichPIDMaker(const Char_t *name) : StMaker(name) {
  drawinit = kFALSE;
}

StRichPIDMaker::~StRichPIDMaker() {}

Int_t StRichPIDMaker::Init() {
      StSPtrVecRichPid thepids = pidTrait->getAllPids();
  evtN = 0;
      for (size_t pidcounter=0;pidcounter<thepids.size();pidcounter++) {
  file = new TFile("testFile.root","RECREATE");


 // define negatively charged particles 
  StPionMinus*  pionminus   = StPionMinus::instance();
  StKaonMinus*  kaonminus   = StKaonMinus::instance();
  StAntiProton* antiproton  = StAntiProton::instance();

  mListOfNegativeParticles.resize(3);
  mListOfNegativeParticles[0] = pionminus;
  mListOfNegativeParticles[1] = kaonminus;
  mListOfNegativeParticles[2] = antiproton;  
  
  // define positively charged particles 
  StPionPlus*  pionplus   = StPionPlus::instance();
  StKaonPlus*  kaonplus   = StKaonPlus::instance();
  StProton*    proton     = StProton::instance();  
  
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
				StRichRingCalculator& ringCalculator,
				double& ang, double& dist, double cut, double& meanD) {

  // calculate distance from inner,mean, outer rings
  ringCalculator.clear();  
  double meanAngle = 0;
  innerDistance = ringCalculator.getInnerDistance(hit,innerAngle);
  outerDistance = ringCalculator.getOuterDistance(hit,outerAngle);
  double meanDistance  = ringCalculator.getMeanDistance(hit,meanAngle);
  
  StThreeVector<double> innerPoint = ringCalculator.getInnerRingPoint();
  double ringWidth = (innerPoint-outerPoint).mag();
  StThreeVector<double> meanPoint  = ringCalculator.getMeanRingPoint();
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
  
    








