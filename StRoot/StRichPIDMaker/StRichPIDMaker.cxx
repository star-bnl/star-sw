/******************************************************
 * $Id: StRichPIDMaker.cxx,v 2.3 2000/09/29 17:55:51 horsley Exp $
 * 
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRichPIDMaker.cxx,v $
 * Revision 2.3  2000/09/29 17:55:51  horsley
 * fixed bug in Minimization routine, included StMagF stuff (commented out)
 * changed StRichRingPoint  HUGE_VALUE   ---> MAXFLOAT for default value
 *
 * Revision 2.15  2000/11/21 16:24:22  horsley
 * Major overhaul of StRichArea, introduced monte carlo integration cross check,
 * all possible areas, angles calculated together. StRichRingCalculator,
 * StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
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
 * (file) ptr checked before writing ntuple.
 * check flags on Hits instead of ADC value
 *
 * Revision 2.9  2000/10/19 15:41:57  horsley
 * added set format option to TFile, file->SetFormat(1);
#define myrICH_WITH_NTUPLE 1
 * Revision 2.8  2000/10/19 06:12:52  horsley
 *
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

// switches
#include "StRichDisplayActivate.h"
#include "StRichTrackingControl.h"
#include "StRichMcSwitch.h"
#define myrICH_WITH_NTUPLE 1
#include "StEvent/StTpcDedxPidAlgorithm.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StRichPidTraits.h"
#include "StEvent/StRichPid.h"
#include "StRchMaker/StRichSimpleHitCollection.h"
  meanDistance  = ringCalculator->getMeanDistance(hit,meanAngle);
  if (ringWidth==0) {return;}  
#ifdef myRICH_WITH_MC
// StMCEvent
#include "StMcEvent/StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"
	    StTrack* track =
// StAssociationMaker
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
		/// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! path length hard coded!!!!!!!!!
// g2t tables
#include "tables/St_g2t_track_Table.h"
#endif
	cout << "\tWARNING! Cannot get B field from event->summary().  Use B= " << mMagField << endl;
}
// StRichPIDmaker
#include "StRichRingCalculator.h"
#include "StRichRingDefinition.h"
#include "StRichTrack.h"
#include "StRichTrackFilter.h"
#include "StRichMCTrack.h"
    if(!event->primaryVertex()) {
	cout << "\tERROR: No Vertex. Skipping..." << endl;
// StChain, etc...
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"

// root
#include "TH1.h"
#include "TNtuple.h"
#include "TH3.h"
  if ( fabs(extrapolatedPosition.x()) < (mGeometryDb->radiatorDimension().x() - mPadPlaneCut) &&
       fabs(extrapolatedPosition.y()) > (mPadPlaneCut) &&
// for finite
#include <math.h>
#ifdef SUN
#include <ieeefp.h>
#endif
       fabs(impactPoint.y()) > (mRadiatorCut) &&
// magnetic field map
//#include "StarCallf77.h"
//#define gufld  F77_NAME(gufld,GUFLD)
//extern "C" {void gufld(Float_t *, Float_t *);}
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 2.3 2000/09/29 17:55:51 horsley Exp $";

	track->geometry() && 
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 2.3 2000/09/29 17:55:51 horsley Exp $";
	(fabs(track->geometry()->momentum().pseudoRapidity()) < mEtaCut) &&
Int_t StRichPIDMaker::Make() { 
  
  // grab StEvent 
  StEvent* rEvent;
  rEvent = (StEvent *) GetInputDS("StEvent");  
  if (!rEvent) {
    cout << "StRichPIDMaker:Make() ---> No StEvent! Return kStWarn." << endl;
    return kStWarn; 
  }

  
  printThisEvent = false;
  mNumberOfRingHits=0;
  
#ifdef myRICH_WITH_MC
  // StMcEvent
  StMcEvent* mEvent = 0;
  mEvent = ((StMcEventMaker*) GetMaker("StMcEvent"))->currentMcEvent();
  if (!mEvent) {
    cout << "StRichPIDMaker:Make() ---> No StMcEvent! Return kStWarn." << endl;

  evtN = rEvent->id();
  if (!this->checkEvent(rEvent)) {return kStWarn;}
	 (track->getPathLength()>0 && track->getPathLength()<mPathCut) &&
  // initialize 

  // First check whether the Primary Vertex is there at all.
  StThreeVectorF VertexPos;
  if (rEvent->primaryVertex()) {VertexPos = rEvent->primaryVertex()->position();}  
  else {return kStWarn;}
  nprimaries    = 0;

  //
  // get hits, clusters, pixels from StEvent
  //
  const StRichCollection*     richCollection = rEvent->richCollection();
  const StSPtrVecRichHit*     pRichHits      = 0;
  const StSPtrVecRichCluster* pRichClusters  = 0;
  const StSPtrVecRichPixel*   pRichPixels    = 0;

  if (!richCollection) { return kStWarn;}
  int myRichHits   = 0;
  int myRichPixels = 0;
  int adcsum       = 0;

  if (richCollection) {  
    if (richCollection->pixelsPresent()) {
      myRichPixels = richCollection->getRichPixels().size();
      const StSPtrVecRichPixel& richPixels = richCollection->getRichPixels();
      pRichPixels = &richPixels;
#ifdef  myRICH_WITH_NTUPLE 
      StSPtrVecRichPixelConstIterator i;
      for (i=pRichPixels->begin(); i != pRichPixels->end(); ++i) {
	adcsum += (*i)->adc();
      }
#endif
    }
    
    if (richCollection->clustersPresent()) {
      const StSPtrVecRichCluster& richClusters = richCollection->getRichClusters();
      pRichClusters = &richClusters;
    }

    if (richCollection->hitsPresent()) {
      const StSPtrVecRichHit& richHits = richCollection->getRichHits();
      pRichHits = &richHits;
      myRichHits = richHits.size();

  // get tracks
  //
  nrichtracks = mListOfStRichTracks.size();  
  

#ifdef myRICH_WITH_MC
  if (!makeTrackAssociations(mEvent,pRichHits)) {
    cout << "can not make track associations. Return kStWarn! " << endl;
    return kStWarn;
  }
#endif
  
  // get tracks intersecting RICH, make monte carlo associations 
  nprimaries  = this->fillTrackList(rEvent,pRichHits);
  
 
  // 
  // This is the main PID loop
  // 
  StRichTrack*          richTrack = 0;
  StParticleDefinition* particle  = 0;
  StRichRingCalculator* ringCalc  = 0;
  
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {     
    StRichTrack* richTrack = mListOfStRichTracks[trackIndex];
    for (size_t particleIndex=0; particleIndex<mListOfParticles.size(); particleIndex++) {
      particle = mListOfParticles[particleIndex];
      if (richTrack->fastEnough(particle) && richTrack->isGood(particle)) { 
	ringCalc = new StRichRingCalculator(richTrack,particle);
	this->hitFilter(pRichHits,ringCalc);
	this->fillPIDTraits(ringCalc);
	delete ringCalc;  
	ringCalc = 0;
      }
    }
    //
    // fill the StTrack's StRichPidTrait with RICH PID info
    //
    StTrack* track = richTrack->getStTrack();

    if (track && richTrack->getPidTrait()) {track->addPidTraits(richTrack->getPidTrait()); }
    richTrack = 0;
  }  
  // 
  //
  // draw rings on padplane, fill software monitor 
  //
  this->drawPadPlane(rEvent,printThisEvent);
  //
  // fill ntuples
  //
#ifdef  myRICH_WITH_NTUPLE 
  if (kWriteNtuple) this->fillEvtNtuple(rEvent, nprimaries, nnegprimaries, pRichHits,pRichPixels);
  if (kWriteNtuple) this->fillPIDNtuple();
  if (kWriteNtuple) this->fillHitNtuple(pRichHits,pRichClusters);
  if (kWriteNtuple) this->fillOverLapHist(pRichHits);
    
#ifdef myRICH_WITH_MC
  if (kWriteNtuple) this->fillMcTrackNtuple(pRichClusters);
  if (kWriteNtuple) this->fillMcPixelNtuple(pRichPixels);
  if (kWriteNtuple) this->fillMcPhotonNtuple(mEvent,pRichClusters,pRichHits);
  if (mRefit==false) {  
    // test the correction to tracks trajectory
    richTrack = 0;
    particle  = 0;
    ringCalc  = 0;
    for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {     
      richTrack = mListOfStRichTracks[trackIndex];
  ringCalc  = 0;
      cout << "Track p = " << richTrack->getMomentum() << endl;
      
      
      richTrack->clearHits();
      for (size_t particleIndex=0; particleIndex<mListOfParticles.size(); particleIndex++) {
	particle = mListOfParticles[particleIndex];
	if (richTrack->fastEnough(particle) && richTrack->isGood(particle)) { 
	  ringCalc = new StRichRingCalculator(richTrack,particle);
	  this->hitFilter(pRichHits,ringCalc);
    }
    if (kWriteNtuple) this->fillCorrectedNtuple();
  }
#endif  


  if (kWriteNtuple) this->fillCorrectedNtuple();
#endif    
  
  return kStOK;
      if (track->getAssociatedMIP()) {
			theCurrentHit->setBit(eInAreap);

		    
	    }  
// 		     << theCurrentHit->isSet(eInMultipleCAnglep)
      
      pid->setTotalHits(totalHits);
      pid->setTruncatedHits(constantHits);
      richMonitor->setNumberOfTracksCrossing(nrichtracks);

      //
      // assign the pid to the StRichTrack
      //
       track->getPidTrait()->addPid(pid);
				          int npri, const StSPtrVecRichHit* hits,
				           const StSPtrVecRichPixel* pixels ) {
//     os << meanD[4][1] << "\t" << sigmaD[4][1] << endl;
//     os << meanD[5][1] << "\t" << sigmaD[5][1] << endl;
//     os << "----------------------------------------------" << endl;
//     os << "----------------------------------------------\n" << endl;
  StThreeVectorF VertexPos(-999,-999,-999);
  if (event->primaryVertex()) {VertexPos = event->primaryVertex()->position();}  

// Event Level
void StRichPIDMaker::setVertexWindow(float window) {
    cout << "StRichPIDMaker::setVertexWindow() " << window << endl;
    mVertexWindow = window;
}
  array[4] = VertexPos.x();
  array[5] = VertexPos.y();
  array[6] = VertexPos.z();

// Track Level
void StRichPIDMaker::setLastHitCut(float cut)    {mLastHitCut = cut;}
void StRichPIDMaker::setDcaCut(float cut)        {mDcaCut = cut;}
void StRichPIDMaker::setPtCut(float cut)         {mPtCut = cut;}
void StRichPIDMaker::setEtaCut(float cut)        {mEtaCut = cut;}
void StRichPIDMaker::setFitPointsCut(int cut)    {mFitPointsCut = cut;}
void StRichPIDMaker::setPathLengthCut(float cut) {mPathCut = cut;}
void StRichPIDMaker::hiLiteFoundHits(StRichTrack* track) {
  
#ifdef RICH_WITH_PAD_MONITOR
	
  StRichDrawableTRings* currentTRing  = 0;
  StParticleDefinition* particle      = 0;
  StRichDrawableTTrack* currentTTrack = mPadMonitor->getTrack(track);
  
  if(currentTTrack) {

    for (size_t particleIndex=0;particleIndex<mListOfParticles.size();particleIndex++) {
      particle = mListOfParticles[particleIndex];

      if (particle) {
	currentTRing = currentTTrack->getRing(particle);

	if (currentTRing) {
	  vector<StRichRingHit*> hits = track->getRingHits(particle);

		hits[hitIndex]->getDist()>0 && hits[hitIndex]->getDist()<1) {

	    if( hits[hitIndex] && hits[hitIndex]->getHit() && 
		hits[hitIndex]->getNSigma() < 2 ) {
	      currentTRing->addHit(hits[hitIndex]->getHit());
	    }
	  }
	} 
      }
    }
  }
#endif  
}

void StRichPIDMaker::setPadPlaneCut(float cut)   {mPadPlaneCut = cut;}
void StRichPIDMaker::setRadiatorCut(float cut)   {mRadiatorCut = cut;}
float StRichPIDMaker::getHitSigma(float hitDist) {
  float sigma = 0.4;
  float mean = 0.45;
  return fabs(hitDist-mean)/sigma;
}
  for (int i=0;i<mListOfStRichTracks.size();i++) {

void StRichPIDMaker::DefineSaveDirectory(char* directory) {
  mySaveDirectory=directory;
}


void StRichPIDMaker::setFileName(char * name){
    fileName = name;
}


void StRichPIDMaker::setWavelengthRange(double shortest , double longest) {
  mShortWave = shortest;
  mLongWave  = longest;
  mMaterialDb = StRichMaterialsDb::getDb();
  mMaterialDb->setWavelengthRange(mShortWave,mLongWave);
}


void StRichPIDMaker::fillEvtNtuple(StEvent* event, int pri, 
				   int npri, const StSPtrVecRichHit* hits,
	
	for (StSPtrVecRichHitConstIterator hitIter = richHits->begin(); hitIter != richHits->end(); hitIter++) {
    //if (event->primaryVertex()) {VertexPos = event->primaryVertex()->position();}  

  float array[9];  
	resid<0.3*centimeter &&  track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId)>40 &&
	track->getLastHit().perp()>160.0*centimeter) {
  array[1] = mNumberOf1GeV;
  array[2] = pri;
  array[3] = npri;
  array[4] = mVertexPos.x();
  array[5] = mVertexPos.y();
  array[6] = mVertexPos.z();
  array[7] = 0;
	    for (int i=0;i<pihits.size() && pionTag==0;i++) {
	      if ((*hitIter)==pihits[i]->getHit() && pihits[i]->getDist()>0 && pihits[i]->getDist()<1) pionTag=1;



	    for (int i=0;i<kahits.size() && kaonTag==0;i++) {
	      if ((*hitIter)==kahits[i]->getHit() && kahits[i]->getDist()>0 && kahits[i]->getDist()<1) kaonTag=1;
  
  for (size_t i=0; i<mListOfStRichTracks.size(); i++) {
    
    track = mListOfStRichTracks[i];
    float p     = track->getMomentum().mag();
    float theta = track->getTheta()/degree;
    float resid = 999;
    float kaonTag=0;
    float pionTag=0;
    
    if (track->getAssociatedMIP()) resid = (track->getAssociatedMIP()->local()- track->getProjectedMIP()).perp();
    
    if (track->getStTrack()  && track->fastEnough(kaon) && track->isGood(kaon) && track->isGood(pion) &&
	resid<0.3*centimeter) {
      
      StRichRingCalculator* pionCalculator = new StRichRingCalculator(track,pion);
      StRichRingCalculator* kaonCalculator = new StRichRingCalculator(track,kaon);
      
      // calculate distance from inner,mean, outer rings
      if (richHits) {
	
	float adcCut = 300;

	StSPtrVecRichHitConstIterator hitIter;
	for (hitIter = richHits->begin(); hitIter != richHits->end(); hitIter++) {
	  

void StRichPIDMaker::drawPadPlane(StEvent* rEvent, bool kCreatePsFile) {

#ifdef RICH_WITH_PAD_MONITOR 
  
  StThreeVectorF VertexPos(-999,-999,-999);
  if (rEvent->primaryVertex()) {
    VertexPos = rEvent->primaryVertex()->position();
  } 

  mPadMonitor = StRichPadMonitor::getInstance(mGeometryDb);
  mPadMonitor->clearTracks();
  mPadMonitor->drawZVertex(VertexPos.z(),nprimaries,nrichtracks);
  mPadMonitor->drawEventInfo(rEvent->runId(),rEvent->id());
  mPadMonitor->drawFileName(fileName);

  for (size_t trackIndex=0;trackIndex<mListOfStRichTracks.size();trackIndex++) {
    mPadMonitor->addTrack(mListOfStRichTracks[trackIndex]);
    hiLiteFoundHits(mListOfStRichTracks[trackIndex]);
  }
  if (kCreatePsFile) mPadMonitor->printCanvas("DefaultName",fileName,rEvent->id());    
  mPadMonitor->drawRings();
  mPadMonitor->update();  
  if (kCreatePsFile) mPadMonitor->printCanvas("/star/rcf/scratch/horsley/",fileName,rEvent->id());    
#endif 
}


void StRichPIDMaker::fillHitNtuple(const StSPtrVecRichHit* hits, const StSPtrVecRichCluster* clusters) {
	    pionCalculator->clear();  
	    kaonCalculator->clear();  
	    
	    StThreeVectorF hit = (*hitIter)->local();

	    pionTag=0;
	    kaonTag=0;

	    vector<StRichRingHit*> pihits = track->getRingHits(pion);
	    for (size_t i=0; i<pihits.size() && pionTag==0; i++) {
	      if ((*hitIter)==pihits[i]->getHit() &&
		  pihits[i]->getDist()>0          &&
		  pihits[i]->getDist()<1)
		  pionTag=1;
    if (mListOfStRichTracks[i]->getPidTrait() && mListOfStRichTracks[i]->getPidTrait()->getPid(pion)) {
      constAngle = mListOfStRichTracks[i]->getPidTrait()->getPid(pion)->getTruncatedAzimuth();
      goodHits =  mListOfStRichTracks[i]->getPidTrait()->getPid(pion)->getTruncatedHits();
		  kahits[i]->getDist()<1)
		  kaonTag=1;
	    }
	    
	    innerDistance = kaonCalculator->getInnerDistance(hit,innerAngle);
	    outerDistance = pionCalculator->getOuterDistance(hit,outerAngle);
      for (int j=0;j<hits.size();j++) {
	
	for (int k=j+1;k<hits.size();k++) {
	    out = pionCalculator->getOuterRingPoint();
	    
	     ( hits[j]->getDist() > 0  && hits[j]->getDist() < 1) &&
	      (hits[k]->getDist() > 0  && hits[k]->getDist() < 1) ) {		 
	  if (fabs(psi1)>constAngle && fabs(psi2)>constAngle &&
	     (hits[j]->getNSigma() < 2) && (hits[k]->getNSigma() < 2) ) {		 

	    if (psi1<0) psi1 = psi1 + 2.0*M_PI;
	    if (psi2<0) psi2 = psi2 + 2.0*M_PI;
	    
	    if ( fabs(psi1-psi2) < fabs(psi1save-psi2save) ) {
	      psi1save=psi1;
	      psi2save=psi2;
	      x1 = hits[j]->getHit()->local().x();
	      y1 = hits[j]->getHit()->local().y();
	      x2 = hits[k]->getHit()->local().x();
	      y2 = hits[k]->getHit()->local().y();
	    }
    
    psi1save = -999;
    psi2save = 999;
        
      
	
	if (psi1save != -999 && psi2save != 999) { 
    StRichPidTraits* pidTrait=0;
	      closeHitNtup->Fill(mListOfStRichTracks[i]->getMomentum().mag(),
				 mListOfStRichTracks[i]->getTheta()/degree,
				 goodHits,constAngle/degree,
				 psi1save/degree,psi2save/degree,
				 x1,y1,x2,y2);
	}
      StSPtrVecRichPid thepids = pidTrait->getAllPids();
      for (size_t pidcounter=0;pidcounter<thepids.size();pidcounter++) {
	if (thepids[pidcounter]->getRingType()==pion)  {
  


	  pionPid   = thepids[pidcounter]; 
	  constAngle = pionPid->getTruncatedAzimuth();
  double adcsum,x,y,p,resid,maxAmp,theta,quartz,rad,ang;
	}
      }
    }
    
     

    vector<StRichRingHit*> hits = mListOfStRichTracks[i]->getRingHits(pion);
    
    if (goodHits>1) {
      
      for (size_t j=0; j<hits.size(); j++) {
	for (size_t k=j+1; k<hits.size(); k++) {
	  
	  psi1=hits[j]->getAngle();
	  psi2=hits[k]->getAngle();
	  
	  if (fabs(psi1)>constAngle      &&
	      fabs(psi2)>constAngle      &&
	      (hits[j]->getNSigma() < 2) &&
	      (hits[k]->getNSigma() < 2) ) {		 
	    
	      if (psi1<0) psi1 = psi1 + 2.0*M_PI;
	      if (psi2<0) psi2 = psi2 + 2.0*M_PI;
	      
	      if ( fabs(psi1-psi2) < fabs(psi1save-psi2save) ) {
	    if (track->getStTrack())       { tpchits = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);}
	    if (track->getAssociatedMIP()) { resid = (track->getAssociatedMIP()->local() - track->getProjectedMIP());}
	      }
	  } 
	}
	  for (size_t particleIndex=0;particleIndex<mListOfParticles.size() && found==0; particleIndex++) {
	    vector<StRichRingHit*> hits = track->getRingHits(mListOfParticles[particleIndex]);	    
			     mListOfStRichTracks[i]->getTheta()/degree,
	      if ((*hitIter) == hits[i]->getHit() && hits[i]->getDist()>0 
		  && hits[i]->getDist()<1 && fabs(hits[i]->getAngle())>90.0*degree) {
			     psi1save/degree,psi2save/degree,
			     x1,y1,x2,y2);
      }
    }
  }
  
  int counter=0;
  int clusternumber,npads,clusterFromTrack,found,tpchits;
  double adcsum,x,y,p,maxAmp,theta,quartz,rad,ang;
  if (clusters) {
    if (hits) {
      StSPtrVecRichHitConstIterator hitIter;
      for (hitIter=hits->begin();
	   hitIter != hits->end();
	   hitIter++) {
	
	counter++;
	quartz=0;
	rad=0;
	clusternumber  = (*hitIter)->clusterNumber();
	npads          = (*clusters)[clusternumber]->numberOfPads();
	adcsum         = (*clusters)[clusternumber]->amplitudeSum();
	maxAmp         = (*hitIter)->maxAmplitude();
	x = (*hitIter)->local().x();
	y = (*hitIter)->local().y();
	clusterFromTrack = 0;
	found = 0;
	StThreeVectorF resid(-999,-999,-999);
	tpchits=0;
	StRichTrack* track = 0;
	for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
	  track = mListOfStRichTracks[trackIndex];
	  p = track->getMomentum().mag();
	  theta = track->getTheta()/degree;
	  if (track->getAssociatedMIP()==(*hitIter)) {
	    clusterFromTrack=10;

void StRichPIDMaker::fillPIDTraits(StRichRingCalculator* ringCalc) {

  if (!ringCalc || !ringCalc->getRing(eInnerRing)) return;
  StRichTrack*          track = ringCalc->getRing(eInnerRing)->getTrack();
  StParticleDefinition* part  = ringCalc->getRing(eInnerRing)->getParticleType();  
  if (!track || !part) return;

  if (!track->getPidTrait()) return; 

  double saturatedArea  = 0;
  UShort_t totalHits    = 0;
  UShort_t constantHits = 0;
 
  if (part==pion)        saturatedArea = pionSaturatedArea;
  else if (part==kaon)   saturatedArea = kaonSaturatedArea;
  else if (part==proton) saturatedArea = protonSaturatedArea;
  else {cout << "StRichPIDMaker::fillPidTraits -> passed bad particle. abort." << endl; abort();}


    if (track->fastEnough(part) && track->isGood(part)) {
      StRichPid* pid = new StRichPid();
      pid->setRingType(part);
      double totalArea=0;
      pid->setTruncatedArea(ringCalc->calculateConstantArea(saturatedArea,doGapCorrection,totalArea));
      pid->setTotalArea(totalArea);
      pid->setTotalAzimuth(ringCalc->getTotalAngle());
      pid->setTruncatedAzimuth(ringCalc->getConstantAreaAngle());
      totalHits    = 0;
      constantHits = 0;
      vector<StRichRingHit*> hits = track->getRingHits(part);
      //cout << "filling pid traits for " << part->name() << endl;
	if (hits[i]->getDist()>0 && hits[i]->getDist()<1) {

      for (size_t i=0;i<hits.size();i++) {   
	if (hits[i]->getNSigma() < 2) {
	  totalHits++;
      cout << "tot hits = " << totalHits << "const hits = " << constantHits << endl << endl << endl;
      if (part==kaon && constantHits>2 && fabs(mVertexPosition.z())<30) {
	//printThisEvent=true;
      }

      StThreeVectorD residual(-999,-999,-999);
      if (track->getAssociatedMIP()) {
	residual.setX(track->getProjectedMIP().x()-track->getAssociatedMIP()->local().x());
	residual.setY(track->getProjectedMIP().y()-track->getAssociatedMIP()->local().y());
	residual.setZ(track->getProjectedMIP().z()-track->getAssociatedMIP()->local().z());
      }
      
      pid->setTotalHits(totalHits);
      pid->setTruncatedHits(constantHits);
      pid->setMipResidual(residual); 
      track->getPidTrait()->addPid(pid);
    }       

}


	    if (track->getStTrack()) { 
	      tpchits = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
	    }
	    if (track->getAssociatedMIP()) { 
	      resid = (track->getAssociatedMIP()->local() - track->getProjectedMIP());
	    }
	  }

	 
	  for (size_t particleIndex=0;
	       particleIndex<mListOfParticles.size() && found==0; 
	       particleIndex++) {
	    vector<StRichRingHit*> hits 
	      = track->getRingHits(mListOfParticles[particleIndex]);	    
	    for (size_t i=0;i<hits.size() && found==0; i++) {
	      if ((*hitIter) == hits[i]->getHit()  && hits[i]->getNSigma()<2 ) {
		quartz = hits[i]->getMeanPathInQuartz();
		rad = hits[i]->getMeanPathInRadiator();
		ang = hits[i]->getAngle()/degree;
		found=1;
	      }
	    }
	  }
	}

	float array[16];
	array[0] = npads;
	array[1] = adcsum;
	array[2] = clusterFromTrack;
	array[3] = x;
	array[4] = y;
	array[5] = found;
	array[6] = p;
	array[7] = resid.perp();
	array[8] = tpchits;
	array[9] = maxAmp;
	array[10] = resid.x();
	array[11] = resid.y();
	array[12] = theta;
	array[13] = quartz;
	array[14] = rad;
	array[15] = ang;
	hitNtup->Fill(array);
	
      }
    }
  }
} 


#ifdef myRICH_WITH_MC
void StRichPIDMaker::fillMcPixelNtuple(const StSPtrVecRichPixel* pixels) {
  
  if (!pixels) return;
  
  // loop over hits
  StRichMCPixel* monteCarloRichPixel = 0;
  for (StSPtrVecRichPixelConstIterator iter = pixels->begin();iter != pixels->end(); ++iter) {
    
    monteCarloRichPixel = dynamic_cast<StRichMCPixel*>(*iter);   
    if (monteCarloRichPixel) {
    Long_t  gid[4]   = {0,0,0,0};
    Float_t gq[4]    = {0,0,0,0};
    Long_t  gproc[4] = {0,0,0,0};
    
    unsigned int n = monteCarloRichPixel->contributions();
    unsigned int limit = (n>4)?(4):(n); 
    for (int i=0;i<limit;i++) {
      gid[i]   = monteCarloRichPixel->getMCInfo()[i]->gid();
      gq[i]    = monteCarloRichPixel->getMCInfo()[i]->charge();
      gproc[i] = monteCarloRichPixel->getMCInfo()[i]->process();
    }
    
    geantPixelNtuple->Fill(monteCarloRichPixel->adc(),monteCarloRichPixel->contributions(),
			   gid[0],gid[1],gid[2],gid[3],gq[0],gq[1],gq[2],gq[3],
			   gproc[0],gproc[1],gproc[2],gproc[3]);
    } 
  }
}
#endif



#ifdef myRICH_WITH_MC
void StRichPIDMaker::fillGeantHitNtuple() {
  
  StRichMCTrack* richMcTrack=0;
  
  float constAngle=-999;
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size();trackIndex++) {
    richMcTrack = dynamic_cast<StRichMCTrack*>(mListOfStRichTracks[trackIndex]);    
    if (richMcTrack) {
      if (mListOfStRichTracks[trackIndex]->getPidTrait() && 
	  mListOfStRichTracks[trackIndex]->getPidTrait()->getPid(pion)) {
	constAngle = mListOfStRichTracks[trackIndex]->getPidTrait()->getPid(pion)->getTruncatedAzimuth();
      }

      vector<StMcRichHit*> tempHits = richMcTrack->getGeantPhotons();
      float wave1,psi1,z1;
      float wave2,psi2,z2;
      float wave1save,wave2save;
      float psi1save,psi2save;
      float z1save,z2save;
      float x1,y1,x2,y2;

      for (int i=0;i<tempHits.size();i++) {
	for (int j=i+1;j<tempHits.size();j++) {

	  getGeantPhotonInfo(richMcTrack,tempHits[i]->parentTrack(),wave1,psi1,z1);
	  getGeantPhotonInfo(richMcTrack,tempHits[j]->parentTrack(),wave2,psi2,z2);

	  if (fabs(psi1)>constAngle && fabs(psi2)>constAngle)  {
	
	    if (psi1<0) psi1 = psi1 + 2.0*M_PI;
	    if (psi2<0) psi2 = psi2 + 2.0*M_PI;
	    if ( fabs(psi1-psi2) < fabs(psi1save-psi2save)) {
	      psi1save=psi1;
	      psi2save=psi2;
	      wave1save=wave1;
	      wave2save=wave2;
	      z1save = z1;
	      z2save = z2;
	      x1 = tempHits[i]->position().x();
	      y1 = tempHits[i]->position().y();
	      x2 = tempHits[j]->position().x();
	      y2 = tempHits[j]->position().y();
	    }  
	  }
	}
      }
      
      float array[13];
      array[0] = richMcTrack->getMomentum().mag();
      array[1] = richMcTrack->getTheta()/degree;
      array[2] = wave1save/nanometer;
      array[3] = wave2save/nanometer;
      array[4] = psi1save/degree;
      array[5] = psi2save/degree;
	TPC_RingCalc->calculateConstantArea(pionSaturatedArea,true,totArea);}
      array[7] = z2save;
      array[8] = x1;
      array[9] = y1;
      array[10] = x2;
	GEANT_RingCalc->calculateConstantArea(pionSaturatedArea,true,totArea);}
    double totArea = 0;
      array[12] = constAngle/degree;
      geantCloseHitNtuple->Fill(array);      
    }
  }  
}
#endif

#ifdef myRICH_WITH_MC
      if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
	TPC_RingCalc->calculateConstantArea(mPionSaturatedArea,true,totArea);}
					   const StSPtrVecRichHit* richHits) {
  
  if (!mcevent || !richHits || !clusters) return;
      if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
	GEANT_RingCalc->calculateConstantArea(mPionSaturatedArea,true,totArea);}
    if(!dsGeant || !dsGeant->GetList()) { return;}
  }

  St_DataSetIter geantDstI(dsGeant);
  St_g2t_track   *g2t_track = (St_g2t_track *) geantDstI("g2t_track"); 
  
  StRichMCTrack*        richMcTrack   = 0;
  StParticleDefinition* geantParticle = 0;
  
  StRichMCHit* monteCarloRichHit      = 0;
  StMcTrack*   theHitsStMcTrack       = 0;
  StMcTrack*   theHitsParentStMcTrack = 0;

  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size();trackIndex++) {
    
    richMcTrack    = dynamic_cast<StRichMCTrack*>(mListOfStRichTracks[trackIndex]);    
    geantParticle  = richMcTrack->getStMcTrack()->particleDefinition();
 
    if (richMcTrack && geantParticle) {
      
      richMcTrack->useTPCInfo();
      StRichRingCalculator* TPC_RingCalc = new StRichRingCalculator(richMcTrack,geantParticle);
      TPC_RingCalc->calculateArea();
      
      //if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
      //TPC_RingCalc->calculateConstantArea(mPionSaturatedArea,true,totArea);}
      
      richMcTrack->useGeantInfo();
      StRichRingCalculator* GEANT_RingCalc = new StRichRingCalculator(richMcTrack,geantParticle);	
      GEANT_RingCalc->calculateArea();

      //if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
      //GEANT_RingCalc->calculateConstantArea(mPionSaturatedArea,true,totArea);}

      richMcTrack->useTPCInfo();  
      
      float defaultValue = -999.0;
      float mcWave,mcPsi,mcZ;
      int   signalPhoton;

      // loop over hits
      StSPtrVecRichHitConstIterator iter;
      for (iter = richHits->begin();iter != richHits->end(); ++iter) {
	
	monteCarloRichHit = dynamic_cast<StRichMCHit*>(*iter); 
	theHitsStMcTrack  = getStMcTrack(monteCarloRichHit, mcevent, g2t_track);
	
	if (monteCarloRichHit && theHitsStMcTrack) {
	
	  if (theHitsStMcTrack->geantId() == 50)  {theHitsParentStMcTrack = theHitsStMcTrack->parent();}
	  else {theHitsParentStMcTrack = theHitsStMcTrack;}
	  
	  // determine if photon's parent == StRichTrack
	  signalPhoton = 0;
	  if (monteCarloRichHit->getMCInfo().process()==ePhoton  && 
	      theHitsParentStMcTrack == richMcTrack->getStMcTrack() 
	      && theHitsStMcTrack->geantId()==50) {
	    signalPhoton=1;    
	  }
	  
	  mcWave = defaultValue; mcPsi = defaultValue; mcZ = defaultValue;
	  getGeantPhotonInfo(richMcTrack,theHitsStMcTrack,mcWave,mcPsi,mcZ);
	  StThreeVectorF geantRichHit = getTheGeantHitOnPadPlane(theHitsStMcTrack,monteCarloRichHit->local()); 
	  
	  Float_t photonArray[52];
	  
	  photonArray[0] = richMcTrack->getStTrack()->geometry()->charge();
	  photonArray[1] = richMcTrack->getMomentum().x();
	  photonArray[2] = richMcTrack->getMomentum().y();
	  photonArray[3] = richMcTrack->getMomentum().z();
	  photonArray[4] = richMcTrack->getImpactPoint().x();
	  
	  photonArray[5] = richMcTrack->getImpactPoint().y();    
	  photonArray[6] = richMcTrack->getGeantImpactPointAtRadiator().x();
	  photonArray[7] = richMcTrack->getGeantImpactPointAtRadiator().y();
	  photonArray[8] = richMcTrack->getProjectedMIP().x();
	  photonArray[9] = richMcTrack->getProjectedMIP().y();
	  
	  StRichMCHit* tempHit = dynamic_cast<StRichMCHit*>(richMcTrack->getAssociatedMIP());
	    hitFilter(TPC_RingCalc, monteCarloRichHit->local(),photonArray[43],photonArray[44]);
	    hitFilter(TPC_RingCalc,geantRichHit,photonArray[45],photonArray[46]);
	    photonArray[10] = tempHit->getMCInfo().id();   
	    photonArray[11] = tempHit->getMCInfo().process();    
	    photonArray[12] = tempHit->charge();    
	    photonArray[13] = tempHit->local().x();
	    photonArray[14] = tempHit->local().y();
	    photonArray[15] = (*clusters)[tempHit->clusterNumber()]->numberOfPads();
	  }
	  
	  tempHit = richMcTrack->getGeantRecoMIP();
	  if (tempHit) { 
	    photonArray[16] = tempHit->getMCInfo().id();
	    photonArray[17] = tempHit->getMCInfo().process();
	    photonArray[18] = tempHit->charge();
	    photonArray[19] = tempHit->local().x();
	    photonArray[20] = tempHit->local().y();
	    hitFilter(GEANT_RingCalc,monteCarloRichHit->local(),photonArray[48],photonArray[49]);
	    hitFilter(GEANT_RingCalc,geantRichHit,photonArray[50],photonArray[51]);
	  tempHit=0;
	  
	  photonArray[22] = richMcTrack->getGeantMIP().x();
	  photonArray[23] = richMcTrack->getGeantMIP().y();
	  photonArray[24] = richMcTrack->getGeantMomentumAtRadiator().x();
	  photonArray[25] = richMcTrack->getGeantMomentumAtRadiator().y();
	  photonArray[26] = richMcTrack->getGeantMomentumAtRadiator().z();
	  
	  photonArray[27] = richMcTrack->getTheta()/degree;
	  photonArray[28] = richMcTrack->getGeantThetaAtRadiator()/degree;
	  photonArray[29] = richMcTrack->getPhi()/degree;
	  photonArray[30] = richMcTrack->getGeantPhiAtRadiator()/degree;
	  photonArray[31] = richMcTrack->getStMcTrack()->geantId();
          
	  photonArray[32] = signalPhoton;
	  photonArray[33] = mcWave;
	  photonArray[34] = mcPsi/degree;
	  photonArray[35] = mcZ;
	  photonArray[36] = monteCarloRichHit->getMCInfo().process();
	  
	  photonArray[37] = monteCarloRichHit->local().x();
	  photonArray[38] = monteCarloRichHit->local().y();
	  photonArray[39] = geantRichHit.x();
	  photonArray[40] = geantRichHit.y();
	  photonArray[41] = geantParticle->mass();
	  
	  bool kWriteTheNtuple = false;

	  richMcTrack->useTPCInfo();
	  if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
	    photonArray[42] = TPC_RingCalc->getConstantAreaAngle();
	    this->hitFilter(TPC_RingCalc, monteCarloRichHit->local(),photonArray[43],photonArray[44]);
	    this->hitFilter(TPC_RingCalc,geantRichHit,photonArray[45],photonArray[46]);
	    if (fabs(photonArray[44])<5 ||   fabs(photonArray[46])<5) kWriteTheNtuple=true;
	  }    
	  else {
	    photonArray[42]=defaultValue;
	    photonArray[43]=defaultValue;
	    photonArray[44]=defaultValue;
	    photonArray[45]=defaultValue;
	    photonArray[46]=defaultValue;
	  }
	  
	  
	
	  richMcTrack->useGeantInfo();
	  if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {   
	    photonArray[47] = GEANT_RingCalc->getConstantAreaAngle();
	    this->hitFilter(GEANT_RingCalc,monteCarloRichHit->local(),photonArray[48],photonArray[49]);
	    this->hitFilter(GEANT_RingCalc,geantRichHit,photonArray[50],photonArray[51]);
	    if (fabs(photonArray[49])<5 ||   fabs(photonArray[51])<5) kWriteTheNtuple=true;
	  }
	  else {
	    photonArray[47]=defaultValue;
	    photonArray[48]=defaultValue;
	    photonArray[49]=defaultValue;
	    photonArray[50]=defaultValue;
	    photonArray[51]=defaultValue;
	  }
	  
	  richMcTrack->useTPCInfo();
	  if (kWriteTheNtuple) geantPhotonNtuple->Fill(photonArray);
	}
      }
      delete TPC_RingCalc;
      delete GEANT_RingCalc;
      TPC_RingCalc   = 0;
      GEANT_RingCalc = 0;
    }
  }
}
#endif


#ifdef myRICH_WITH_MC
void StRichPIDMaker::fillMcTrackNtuple(const StSPtrVecRichCluster* clusters) {

  for (size_t trackIndex=0;trackIndex<mListOfStRichTracks.size();trackIndex++) {    
    StRichMCTrack* track = dynamic_cast<StRichMCTrack*>(mListOfStRichTracks[trackIndex]);
    if (!track) {
      cout << "trying to send a StTrack to the monte carlo ntuple! " << endl;
      abort();
    }    
    
    StRichPidTraits* pidTrait = track->getPidTrait();
    if (!pidTrait) {
      
      StRichPid* pionPid   = pidTrait->getPid(pion);
      StRichPid* kaonPid   = pidTrait->getPid(kaon);
      StRichPid* protonPid = pidTrait->getPid(proton);

      double PionTotalArea         = 0;
      double PionConstantArea      = 0;
      double PionTotalAreaAngle    = 0;
      double PionConstantAreaAngle = 0;
      int    PionTotalHits         = 0;
      int    PionConstantHits      = 0;
      double PionFactor            = 0;
      
      if (pionPid) {
	PionTotalArea         = pionPid->getTotalArea();
    trackArray[counter++] = evtN;                
	PionTotalAreaAngle    = pionPid->getTotalAzimuth();
	PionConstantAreaAngle = pionPid->getTruncatedAzimuth();
    trackArray[counter++] = vz;
    trackArray[counter++] = nprimaries;
    trackArray[counter++] = nnegprimaries;
    trackArray[counter++] = mVertexPosition.z();
    trackArray[counter++] = nrichtracks;  
      double KaonTotalArea         = 0;
      double KaonConstantArea      = 0;
      double KaonTotalAreaAngle    = 0;
      double KaonConstantAreaAngle = 0;
      int    KaonTotalHits         = 0;
      int    KaonConstantHits      = 0;
      double KaonFactor            = 0;
      
      if (kaonPid) {
	KaonTotalArea         = kaonPid->getTotalArea();
	KaonConstantArea      = kaonPid->getTruncatedArea();
	KaonTotalAreaAngle    = kaonPid->getTotalAzimuth();
	KaonConstantAreaAngle = kaonPid->getTruncatedAzimuth();
	KaonTotalHits         = kaonPid->getTotalHits();
	KaonConstantHits      = kaonPid->getTruncatedHits();
      }
      
      
      double ProtonTotalArea         = 0;
      double ProtonConstantArea      = 0;
      double ProtonTotalAreaAngle    = 0;
      double ProtonConstantAreaAngle = 0;
      double ProtonFactor            = 0;
      int ProtonTotalHits            = 0;
      int ProtonConstantHits         = 0;

      if (protonPid) {
	ProtonTotalArea         = protonPid->getTotalArea();
	ProtonConstantArea      = protonPid->getTruncatedArea();
	ProtonTotalAreaAngle    = protonPid->getTotalAzimuth();
	ProtonConstantAreaAngle = protonPid->getTruncatedAzimuth();
	ProtonTotalHits         = protonPid->getTotalHits();
	ProtonConstantHits      = protonPid->getTruncatedHits();
      }

      
      

    StThreeVectorF  globalp(track->getStTrack()->geometry()->momentum());
    int   counter=0;
    float defaultValue = -999;
    int const entries=88;
    float trackArray[entries];
    /////////

    trackArray[counter++] = mEventN;                
    trackArray[counter++] = mNumberOfPrimaries;
    trackArray[counter++] = mNegativePrimaries;
    trackArray[counter++] = mVertexPos.z();
    trackArray[counter++] = mRichTracks;  

    trackArray[counter++] = globalp.x();          
    trackArray[counter++] = globalp.y();
    trackArray[counter++] = globalp.z();
    trackArray[counter++] = track->getMomentum().x();
    trackArray[counter++] = track->getMomentum().y();   // ---> 10 

    trackArray[counter++] = track->getMomentum().z();   
    trackArray[counter++] = globalp.pseudoRapidity();  
    trackArray[counter++] = track->getStTrack()->geometry()->charge();

    StRichMCHit* tempHit = dynamic_cast<StRichMCHit*>(track->getAssociatedMIP());
    if (tempHit) {    
      trackArray[counter++] = tempHit->getMCInfo().id();   
      trackArray[counter++] = tempHit->getMCInfo().process();    
      trackArray[counter++] = tempHit->charge();    
      trackArray[counter++] = tempHit->local().x();
      trackArray[counter++] = tempHit->local().y();
      trackArray[counter++] = (*clusters)[tempHit->clusterNumber()]->numberOfPads();
    }
    else {
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
    }
    trackArray[counter++] = track->getProjectedMIP().x(); 
  

    trackArray[counter++] = track->getProjectedMIP().y();
  
    tempHit = track->getGeantRecoMIP();
    if (tempHit) { 
      trackArray[counter++] = tempHit->getMCInfo().id();
      trackArray[counter++] = tempHit->getMCInfo().process();
      trackArray[counter++] = tempHit->charge();
      trackArray[counter++] = tempHit->local().x();
      trackArray[counter++] = tempHit->local().y();
      trackArray[counter++] = (*clusters)[tempHit->clusterNumber()]->numberOfPads();
    }
    else {
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue; 
    }
  

    trackArray[counter++] = track->getImpactPoint().x(); // ----> 10 + 20 == 30  
    trackArray[counter++] = track->getImpactPoint().y();
    trackArray[counter++] = track->getUnCorrectedImpactPoint().x();  
    trackArray[counter++] = track->getUnCorrectedImpactPoint().y();
    trackArray[counter++] = track->getUnCorrectedMomentum().x();        

    trackArray[counter++] = track->getUnCorrectedMomentum().y();

    trackArray[counter++] = track->getUnCorrectedMomentum().z();
    trackArray[counter++] = track->getFirstRow();
    trackArray[counter++] = track->getLastRow();  
    trackArray[counter++] = track->getLastHit().x();
    trackArray[counter++] = track->getLastHit().y();      

    trackArray[counter++] = track->getLastHit().z(); 
    trackArray[counter++] = track->getLastHitDCA();   
    trackArray[counter++] = track->getPathLength();
    trackArray[counter++] = track->getMaxChain();
    trackArray[counter++] = track->getMaxGap();      

    trackArray[counter++] = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
    trackArray[counter++] = track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId);
    trackArray[counter++] = mMaterialDb->innerWavelength()/nanometer;
    trackArray[counter++] = mMaterialDb->outerWavelength()/nanometer;
    trackArray[counter++] = track->getGeantMomentumAtRadiator().x();     

    trackArray[counter++] = track->getGeantMomentumAtRadiator().y(); // 30 + 20 = 50
    trackArray[counter++] = track->getGeantMomentumAtRadiator().z();  
    trackArray[counter++] = track->getGeantImpactPointAtRadiator().x();
    trackArray[counter++] = track->getGeantImpactPointAtRadiator().y();  
    trackArray[counter++] = track->getGeantMIP().x();   

    trackArray[counter++] = track->getGeantMIP().y(); 
    if (track->getStMcTrack() && track->getStMcTrack()->stopVertex()) {
      trackArray[counter++] = track->getStMcTrack()->stopVertex()->position().x();
      trackArray[counter++] = track->getStMcTrack()->stopVertex()->position().y();
      trackArray[counter++] = track->getStMcTrack()->stopVertex()->position().z();
    }
    else {
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
    }
    trackArray[counter++] = track->getGeantPhotons().size();
    trackArray[counter++] = track->getRecoGeantPhotons().size(); 

    trackArray[counter++] = track->getNumberOfGeantHitsInRadiator(); // 50 + 10 = 60
    trackArray[counter++] = track->getCommonTpcHits(); 
    if (track->getStMcTrack()) {
      trackArray[counter++] = track->getStMcTrack()->momentum().x();
      trackArray[counter++] = track->getStMcTrack()->momentum().y();

      trackArray[counter++] = track->getStMcTrack()->momentum().z();
    }
    else {
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;    
    }
 
     
    if (track->getStMcTrack()) { trackArray[counter++] = track->getStMcTrack()->geantId();}
    else { trackArray[counter++] = defaultValue;}
  
    if (track->getStMcTrack() && track->getStMcTrack()->stopVertex()) {
      trackArray[counter++] = track->getStMcTrack()->stopVertex()->geantProcess();}
    else { trackArray[counter++] = defaultValue;}

    trackArray[counter++] = track->getNumberOfPartners();
    trackArray[counter++] = PionFactor;
    trackArray[counter++] = PionTotalArea;  

    trackArray[counter++] = PionConstantArea;  // 60 + 10 = 70 
    trackArray[counter++] = PionTotalAreaAngle;
    trackArray[counter++] = PionConstantAreaAngle;
    trackArray[counter++] = PionTotalHits;  
    trackArray[counter++] = PionConstantHits; 

    trackArray[counter++] = KaonFactor;
    trackArray[counter++] = KaonTotalArea;  
      StRichPid* pionPid = pidTrait->getPid(pion);
      StRichPid* kaonPid = pidTrait->getPid(kaon);
      StRichPid* protonPid = pidTrait->getPid(proton);
    
    if (counter != entries) {cout << "StRichPIDMaker::fillMcPidNtuple  wrong counter. abort." << endl; abort();}
    geantTrackNtuple->Fill(trackArray);
    }
  } 
}
#endif

void 
StRichPIDMaker::fillPIDNtuple() {

  util = new TpcHitVecUtilities();
  
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size();trackIndex++) {

	      track->getStTrack()->geometry() &&
    StRichTrack* track = mListOfStRichTracks[trackIndex];
	      fabs(track->getStTrack()->geometry()->momentum().pseudoRapidity()) < 0.5 &&
	      track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId)>40 &&
	      track->getLastHit().perp()>160.0*centimeter && 
	      track->getAssociatedMIP() && 
	      fabs(track->getAssociatedMIP()->local().x()) > 2.0*centimeter &&
	      fabs(track->getAssociatedMIP()->local().y()) > 2.0*centimeter) {

	    StThreeVectorF residual(track->getProjectedMIP() - track->getAssociatedMIP()->local());
      StRichPid* protonPid = 0;

      //
      // need to get the last entry in the list
      //
      StSPtrVecRichPid thepids = pidTrait->getAllPids();
      for (size_t pidcounter=0;pidcounter<thepids.size();pidcounter++) {
	if (thepids[pidcounter]->getRingType()==pion)   pionPid   = thepids[pidcounter]; 
	if (thepids[pidcounter]->getRingType()==kaon)   kaonPid   = thepids[pidcounter]; 
	if (thepids[pidcounter]->getRingType()==proton) protonPid = thepids[pidcounter]; 

	    pionResid->Fill(p,resid,dist);
	    pionResid_x->Fill(p,xresid,dist);
	    pionResid_y->Fill(p,yresid,dist);
	    
	    if (p>1) {
	      pionTheta->Fill(tan_theta,resid,dist);
	      pionTheta_x->Fill(tan_thetax,xresid,dist);
	      pionTheta_y->Fill(tan_thetay,yresid,dist);
	      
	      if (resid<0.3) {
		pionPsi->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
	      }
	    }  
	    if (fabs(mVertexPos.z())>30*centimeter  &&
		fabs(mVertexPos.z())<60*centimeter) {
		//cout << "30  60:: vertex = " << mVertexPosition.z() << endl;
		pionResidb->Fill(p,resid,dist);
		pionResid_xb->Fill(p,xresid,dist);
		pionResid_yb->Fill(p,yresid,dist);
	      
	      if (p>1) {
		pionThetab->Fill(tan_theta,resid,dist);
		pionTheta_xb->Fill(tan_thetax,xresid,dist);
		pionTheta_yb->Fill(tan_thetay,yresid,dist);
		
		if (resid<0.3) {
		  pionPsib->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
		}
	      } 
	    }
	
	vector<StRichRingHit*> pionHits = track->getRingHits(pion);
	for (size_t i=0;i<pionHits.size();i++) {
	  if (track->getStTrack() && fabs(pionHits[i]->getAngle())>PionConstantAreaAngle) {
	    
	    StThreeVectorF residual(-999,-999,-999);
	    if (track->getAssociatedMIP()) {
	      residual = (track->getProjectedMIP() - track->getAssociatedMIP()->local());
	    }
	    
	    double p = track->getStTrack()->geometry()->momentum().mag();
	    double resid =  residual.perp();
	    double xresid = residual.x();
	    double yresid = residual.y();
	    double dist   = pionHits[i]->getDist();
	    
	    double tan_theta = track->getMomentum().perp()/track->getMomentum().z();
	    double tan_thetax = track->getMomentum().x()/track->getMomentum().z();
	    double tan_thetay = track->getMomentum().y()/track->getMomentum().z();
	
	    
	    pionResid->Fill(p,resid,dist);
	    pionResid_x->Fill(p,xresid,dist);
	    pionResid_y->Fill(p,yresid,dist);
	    
	    if (p>1) {
	      pionTheta->Fill(tan_theta,resid,dist);
	      pionTheta_x->Fill(tan_thetax,xresid,dist);
	      pionTheta_y->Fill(tan_thetay,yresid,dist);
	      
	      if (resid<0.3) {
		pionPsi->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
      trackArray[counter++] = evtN;
	    } 
	    
	    
      trackArray[counter++] = nprimaries;
      trackArray[counter++] = nnegprimaries;
      trackArray[counter++] = vz;
      
      
      trackArray[counter++] = mVertexPosition.z();
      trackArray[counter++] = nrichtracks;  
      double KaonTotalAreaAngle    = 0;
      double KaonConstantAreaAngle = 0;
      int    KaonTotalHits    = 0;
      int    KaonConstantHits = 0;
      double KaonFactor=0;
      
      if (kaonPid) {
	KaonTotalArea         = kaonPid->getTotalArea();
	KaonConstantArea      = kaonPid->getTruncatedArea();
	KaonTotalAreaAngle    = kaonPid->getTotalAzimuth();
	KaonConstantAreaAngle = kaonPid->getTruncatedAzimuth();
	KaonTotalHits         = kaonPid->getTotalHits();
	KaonConstantHits      = kaonPid->getTruncatedHits();
      }
      
      
      double ProtonTotalArea      = 0;
      double ProtonConstantArea   = 0;
      double ProtonTotalAreaAngle = 0;
      double ProtonConstantAreaAngle = 0;
      double ProtonFactor = 0;
      int ProtonTotalHits = 0;
      int ProtonConstantHits = 0;

      if (protonPid) {
	ProtonTotalArea         = protonPid->getTotalArea();
	ProtonConstantArea      = protonPid->getTruncatedArea();
	ProtonTotalAreaAngle    = protonPid->getTotalAzimuth();
	ProtonConstantAreaAngle = protonPid->getTruncatedAzimuth();
	ProtonTotalHits         = protonPid->getTotalHits();
	ProtonConstantHits      = protonPid->getTruncatedHits();
      }

      
      
      const Int_t entries=67;
      float trackArray[entries];
  
      StThreeVectorF  globalp(track->getStTrack()->geometry()->momentum());
      int counter=0;
      float defaultValue = -999;

      trackArray[counter++] = mEventN;
      trackArray[counter++] = mNumberOfPrimaries;
      trackArray[counter++] = mNegativePrimaries;
      trackArray[counter++] = posZ;
      trackArray[counter++] = negZ;

      trackArray[counter++] = mVertexPos.z();
      trackArray[counter++] = mRichTracks;  
      trackArray[counter++] = globalp.mag();  
      trackArray[counter++] = globalp.perp();
      trackArray[counter++] = globalp.x();

      trackArray[counter++] = globalp.y();
      trackArray[counter++] = globalp.z();
      trackArray[counter++] = track->getMomentum().x();
      trackArray[counter++] = track->getMomentum().y();
      trackArray[counter++] = track->getMomentum().z();
  
      trackArray[counter++] = globalp.pseudoRapidity();
      if (track->getStTrack() && track->getStTrack()->geometry()) {
	trackArray[counter++] = track->getStTrack()->geometry()->charge();
      }
      else {
	trackArray[counter++] = defaultValue;
      }

      if (track->getAssociatedMIP()) {
	trackArray[counter++] = track->getAssociatedMIP()->charge();    
	trackArray[counter++] = track->getAssociatedMIP()->local().x();
	trackArray[counter++] = track->getAssociatedMIP()->local().y();
      }
      else {
	trackArray[counter++] = defaultValue;
	trackArray[counter++] = defaultValue;
	trackArray[counter++] = defaultValue;  // ---------------> 20 
      }
  
      trackArray[counter++] = track->getProjectedMIP().x();
      trackArray[counter++] = track->getProjectedMIP().y();
      trackArray[counter++] = track->getImpactPoint().x();  
      trackArray[counter++] = track->getImpactPoint().y();
      trackArray[counter++] = track->getUnCorrectedImpactPoint().x();
  
      trackArray[counter++] = track->getUnCorrectedImpactPoint().y();
      trackArray[counter++] = track->getUnCorrectedTheta()/degree;  
      trackArray[counter++] = track->getUnCorrectedPhi()/degree;
      trackArray[counter++] = track->getProjectedCTBPoint().x();
      trackArray[counter++] = track->getProjectedCTBPoint().y();

      trackArray[counter++] = track->getProjectedCTBPoint().z();


      trackArray[counter++] = track->getFirstRow();
      trackArray[counter++] = track->getLastRow();  
      trackArray[counter++] = track->getLastHit().x();
      trackArray[counter++] = track->getLastHit().y();

      trackArray[counter++] = track->getLastHit().z();
      trackArray[counter++] = track->getLastHitDCA();
      trackArray[counter++] = track->getPathLength();
      trackArray[counter++] = track->getMaxChain();
      trackArray[counter++] = track->getMaxGap();   // --------> 20 + 20 = 40

      trackArray[counter++] = track->getTheta()/degree;
      trackArray[counter++] = track->getPhi()/degree;
      trackArray[counter++] = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
      trackArray[counter++] = track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId);
      trackArray[counter++] = PionFactor;
void 
StRichPIDMaker::fillCorrectedNtuple() {
  
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size();trackIndex++) {

    StRichTrack* track = mListOfStRichTracks[trackIndex];
    StRichPidTraits* pidTrait = track->getPidTrait();

    int pioncHits = 0;
    int pionctHits = 0;
    
    int kaoncHits = 0;
    int kaonctHits = 0;
    
    int protoncHits = 0;
    int protonctHits = 0;
 

      

    if (pidTrait) {
    
      StRichPid* pionPid = pidTrait->getPid(pion);
      StRichPid* kaonPid = pidTrait->getPid(kaon);
      StRichPid* protonPid = pidTrait->getPid(proton);

      
      double PionTotalArea         = 0;
      double PionConstantArea      = 0;
      double PionTotalAreaAngle    = 0;
      double PionConstantAreaAngle = 0;
      int    PionTotalHits         = 0;
      int    PionConstantHits      = 0;
      double PionFactor            = 0;
      
      if (pionPid) {
	PionTotalArea         = pionPid->getTotalArea();
	PionConstantArea      = pionPid->getTruncatedArea();
	PionTotalAreaAngle    = pionPid->getTotalAzimuth();
	PionConstantAreaAngle = pionPid->getTruncatedAzimuth();
	PionTotalHits         = pionPid->getTotalHits();
	PionConstantHits      = pionPid->getTruncatedHits();
      }

      
      if (track->fastEnough(pion) && track->isGood(pion)) {
	
	PionFactor = track->getExpectedNPhots(pion);
	
	vector<StRichRingHit*> pionHits = track->getRingHits(pion);
	for (size_t i=0;i<pionHits.size();i++) {
	  if (track->getStTrack() && 
	      track->getStTrack()->geometry() &&
	      fabs(pionHits[i]->getAngle())>PionConstantAreaAngle &&
	    pionCorrectedResid->Fill(p,resid,dist);
	    pionCorrectedResid_x->Fill(p,xresid,dist);
	    pionCorrectedResid_y->Fill(p,yresid,dist);
	    
	    if (p>1) {
	      pionCorrectedTheta->Fill(tan_theta,resid,dist);
	      pionCorrectedTheta_x->Fill(tan_thetax,xresid,dist);
	      pionCorrectedTheta_y->Fill(tan_thetay,yresid,dist);
	      
	      if (resid<0.3) {
		pionCorrectedPsi->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
	      }
	    }  

	    if (fabs(mVertexPos.z()) > 30*centimeter &&
		fabs(mVertexPos.z())<60*centimeter) {
	      //cout << "corrected 30  60:: vertex = " << mVertexPosition.z() << endl;
	      pionCorrectedResidb->Fill(p,resid,dist);
	      pionCorrectedResid_xb->Fill(p,xresid,dist);
	      pionCorrectedResid_yb->Fill(p,yresid,dist);  
	      if (p>1) {
		pionCorrectedThetab->Fill(tan_theta,resid,dist);
		pionCorrectedTheta_xb->Fill(tan_thetax,xresid,dist);
		pionCorrectedTheta_yb->Fill(tan_thetay,yresid,dist);
		
		if (resid<0.3) {
		  pionCorrectedPsib->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
		}
	      }
	    }
	    
	    
	  }
	}
      }
      
      
      double KaonTotalArea      = 0;
      double KaonConstantArea   = 0;
      double KaonTotalAreaAngle    = 0;
      double KaonConstantAreaAngle = 0;
      int    KaonTotalHits    = 0;
      int    KaonConstantHits = 0;
      double KaonFactor=0;
      
      if (kaonPid) {
	KaonTotalArea         = kaonPid->getTotalArea();
	KaonConstantArea      = kaonPid->getTruncatedArea();
	KaonTotalAreaAngle    = kaonPid->getTotalAzimuth();
	KaonConstantAreaAngle = kaonPid->getTruncatedAzimuth();
	KaonTotalHits         = kaonPid->getTotalHits();
	KaonConstantHits      = kaonPid->getTruncatedHits();
      }
      
      
      double ProtonTotalArea      = 0;
      double ProtonConstantArea   = 0;
      double ProtonTotalAreaAngle = 0;
      double ProtonConstantAreaAngle = 0;
      double ProtonFactor = 0;
      int ProtonTotalHits = 0;
      int ProtonConstantHits = 0;

      if (protonPid) {
	ProtonTotalArea         = protonPid->getTotalArea();
	ProtonConstantArea      = protonPid->getTruncatedArea();
	ProtonTotalAreaAngle    = protonPid->getTotalAzimuth();
	ProtonConstantAreaAngle = protonPid->getTruncatedAzimuth();
	ProtonTotalHits         = protonPid->getTotalHits();
	ProtonConstantHits      = protonPid->getTruncatedHits();
      }




    vector<StRichRingHit*> temppionHits = track->getRingHits(pion);
    for (size_t i=0;i<temppionHits.size();i++) {
      if (temppionHits[i]->getDist()>0 && temppionHits[i]->getDist() < 1) {
	pioncHits++;
	if (fabs(temppionHits[i]->getAngle())>PionConstantAreaAngle) {pionctHits++;} 
      }
    }

    vector<StRichRingHit*> tempkaonHits = track->getRingHits(kaon);
    for (size_t i=0;i<tempkaonHits.size();i++) {
      if (tempkaonHits[i]->getDist()>0 && tempkaonHits[i]->getDist() < 1) {
	kaoncHits++;
	if (fabs(tempkaonHits[i]->getAngle())>KaonConstantAreaAngle) {kaonctHits++;} 
      }
      trackArray[counter++] = vz;

    vector<StRichRingHit*> tempproHits = track->getRingHits(proton);
      trackArray[counter++] = mVertexPosition.z();
      if (tempproHits[i]->getDist()>0 && tempproHits[i]->getDist() < 1) {
	protoncHits++;
	if (fabs(tempproHits[i]->getAngle())>ProtonConstantAreaAngle) {protonctHits++;} 
      }
    }

      
      
      const Int_t entries=57;
      float trackArray[entries];
  
      StThreeVectorF  globalp(track->getStTrack()->geometry()->momentum());
      int counter=0;
      float defaultValue = -999;

      trackArray[counter++] = mVertexPos.z();

      trackArray[counter++] = globalp.x();
      trackArray[counter++] = globalp.y();
      trackArray[counter++] = globalp.z();

      trackArray[counter++] = track->getMomentum().x();
      trackArray[counter++] = track->getMomentum().y();
      trackArray[counter++] = track->getMomentum().z();

      trackArray[counter++] = track->getUnCorrectedMomentum().x();
      trackArray[counter++] = track->getUnCorrectedMomentum().y();
      trackArray[counter++] = track->getUnCorrectedMomentum().z(); // 10
  
      trackArray[counter++] = globalp.pseudoRapidity();
      if (track->getStTrack() && track->getStTrack()->geometry()) {trackArray[counter++] = track->getStTrack()->geometry()->charge();}
      else {trackArray[counter++] = defaultValue;}

      if (track->getAssociatedMIP()) {
	trackArray[counter++] = track->getAssociatedMIP()->charge();    
	trackArray[counter++] = track->getAssociatedMIP()->local().x();
	trackArray[counter++] = track->getAssociatedMIP()->local().y();
      }
      else {
	trackArray[counter++] = defaultValue;
	trackArray[counter++] = defaultValue;
	trackArray[counter++] = defaultValue; 
      }
  
      trackArray[counter++] = track->getProjectedMIP().x();
      trackArray[counter++] = track->getProjectedMIP().y();
      
      trackArray[counter++] = track->getImpactPoint().x();  
      trackArray[counter++] = track->getImpactPoint().y();

      trackArray[counter++] = track->getUnCorrectedImpactPoint().x();  // 20
      trackArray[counter++] = track->getUnCorrectedImpactPoint().y();

      trackArray[counter++] = track->getUnCorrectedProjectedMIP().x();
      trackArray[counter++] = track->getUnCorrectedProjectedMIP().y();

      trackArray[counter++] = track->getLastHit().x();
      trackArray[counter++] = track->getLastHit().y();
      trackArray[counter++] = track->getLastHit().z();

      trackArray[counter++] = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
      trackArray[counter++] = track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId);

      trackArray[counter++] = PionFactor; 

      trackArray[counter++] = PionTotalArea; // 30
      trackArray[counter++] = PionConstantArea;  
      trackArray[counter++] = PionTotalAreaAngle;
      trackArray[counter++] = PionConstantAreaAngle;
      trackArray[counter++] = PionTotalHits;  

      trackArray[counter++] = PionConstantHits;
      trackArray[counter++] = KaonFactor;
      trackArray[counter++] = KaonTotalArea;  
      trackArray[counter++] = KaonConstantArea;
      trackArray[counter++] = KaonTotalAreaAngle; 

      trackArray[counter++] = KaonConstantAreaAngle; // 40  
      trackArray[counter++] = KaonTotalHits;
      trackArray[counter++] = KaonConstantHits;
      trackArray[counter++] = ProtonFactor; 
      trackArray[counter++] = ProtonTotalArea;  

      trackArray[counter++] = ProtonConstantArea;
      trackArray[counter++] = ProtonTotalAreaAngle;  
      trackArray[counter++] = ProtonConstantAreaAngle;
      trackArray[counter++] = ProtonTotalHits;
      trackArray[counter++] = ProtonConstantHits; 

      trackArray[counter++] = pioncHits; // 50
      trackArray[counter++] = pionctHits;
      trackArray[counter++] = protonctHits;
StRichPIDMaker::StRichPIDMaker(const Char_t *name, bool writeNtuple) : StMaker(name) {
  mPathCut = 10000; // cm
  mPadPlaneCut = 2.0; // cm
  mRadiatorCut = 2.0; // cm
}
      trackArray[counter++] = mMaterialDb->outerWavelength()/nanometer; // 57 
	cout<< "StRichPIDMaker:: fillPIDNtuple. counter = " << counter << "   --> abort." << endl; abort();} 
void StRichPIDMaker::setTrackRefit(bool refit) {mRefit = refit;}
    }
    distup = new TNtuple("dist","b","xi:yi:xo:yo:si:ld:d:oldd:oldsig:phx:phy:x:y:px:py:pz:theta:evt:numb:resx:resy:res:ring:cos:d2siline:p:q:vtx");
StRichPIDMaker::~StRichPIDMaker() {}
      trackArray[counter++] = PionConstantAreaAngle;

Int_t StRichPIDMaker::fillTrackList(StEvent* tempEvent, const StSPtrVecRichHit* richHits) {
  
  mNumberOf1GeV=0;
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
    delete mListOfStRichTracks[trackIndex];
    mListOfStRichTracks[trackIndex] = 0;
  }
  mListOfStRichTracks.clear();
  mListOfStRichTracks.resize(0);
  double magField    = 2.49117;
  if (tempEvent->summary()) {
    magField  = tempEvent->summary()->magneticField();
    cout << "!!!!!!!!!!!!!!!!!!!!!!!!     mag field = " << magField << endl;
  } 
  else {
    cout << "hard coded (!!!!!!!!!!)  mag field = " << magField << endl;
  } 
 
  // not yet in use, will implement soon
  //  Float_t x[3] = {0,0,0};
  //  Float_t b[3] = {0,0,0};
  //  gufld(x,b);
  //  cout << "field map at 0,0,0 = " 
  //  << b[0] << "   " << b[1] << "   " << b[2] << endl;
  // cout << "field map at 0,0,0 = " 
  //<< b[0] << "   " << b[1] << "   " << b[2] << endl;
  if (!tempEvent->primaryVertex()) {return 0;}  
  StThreeVectorF VertexPos = tempEvent->primaryVertex()->position();  

  size_t nptracks = tempEvent->primaryVertex()->numberOfDaughters();
  

  Int_t numberOfPrimaries=0;
    if (track && track->flag()>=0 && track->geometry() && 
	track->geometry()->helix().distance(VertexPos)<3.0*centimeter &&
	track->fitTraits().numberOfFitPoints(kTpcId) >= 10 &&
	fabs(track->geometry()->momentum().pseudoRapidity()) < 0.5) {
    StTrack* track = tempEvent->primaryVertex()->daughter(trackIndex);

      StRichMCTrack* tempTrack = new StRichMCTrack(track,magField);
      
#ifdef myRICH_WITH_MC    
      StRichTrack* tempTrack   = new StRichTrack(track,magField);
      cout << "StRichPIDMaker::fillTrackList()  -->  creating StMcTrack!" << endl;
#else 
      /// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! path length hard coded!!!!!!!!!
      if ( fabs(tempTrack->getProjectedMIP().x()) < mGeometryDb->radiatorDimension().x() &&
	   fabs(tempTrack->getProjectedMIP().y()) < mGeometryDb->radiatorDimension().y() &&
	   tempTrack->getPathLength()>0 && tempTrack->getPathLength()<10000)  {
	
	  cout << "checkTrack passed a track below the ptcut!! " << endl;
	  abort();
	tempTrack->assignMIP(richHits);
	mListOfStRichTracks.push_back(tempTrack);
	if (tempTrack->getMomentum().mag()>1) {mNumberOf1GeV++;}
	if (tempTrack->getMomentum().mag()>1) {mNumberOf1GeV++;}	
	tempTrack->addPidTrait(new StRichPidTraits());
	mListOfStRichTracks.push_back(tempTrack);  
      }
      
      else {
	delete tempTrack;
	tempTrack=0;
      }

      numberOfPrimaries++;
      if (tempEvent->primaryVertex()->daughter(trackIndex)->geometry()->charge() < 0) {
	nnegprimaries++;
      }
  }
      trackArray[counter++] = KaonTotalArea;  
      trackArray[counter++] = KaonConstantArea;
  return numberOfPrimaries;
}




void StRichPIDMaker::tagMips(StEvent* tempEvent, const StSPtrVecRichHit* hits) {
  // -------------------> oh NO!
  // this is terrible !!!!!!!!!!!!!!!!!!!
  double magField    = 2.49117;
  if (tempEvent->summary()) {
    magField  = tempEvent->summary()->magneticField();
    cout << "!!!!!!!!!!!!!!!!!!!!!!!!     mag field = " << magField << endl;
  } 
  else {
    cout << "hard coded (!!!!!!!!!!)  mag field = " << magField << endl;
  } 

  
  cout << "StRichPIDMaker::tagMips()  ---> Tagging Mips using global tracks ..." << endl;

  StSPtrVecTrackNode& theTrackNodes = tempEvent->trackNodes();
  for (size_t nodeIndex=0;nodeIndex<theTrackNodes.size();nodeIndex++) {
    for (size_t trackIndex=0;trackIndex<theTrackNodes[nodeIndex]->entries(global);trackIndex++) {
	StRichTrack* tempTrack = new StRichTrack(track,magField);
      if (track  && track->flag()>=0 && track->fitTraits().numberOfFitPoints(kTpcId) >= 10 ) {
	if ( fabs(tempTrack->getProjectedMIP().x()) < mGeometryDb->radiatorDimension().x() &&
	     fabs(tempTrack->getProjectedMIP().y()) < mGeometryDb->radiatorDimension().y() &&
	     tempTrack->getPathLength()>0 && tempTrack->getPathLength()<10000)  {
	StRichTrack* tempTrack = new StRichTrack(track,mMagField);
	/// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! path length hard coded!!!!!!!!!
	if ( checkTrack(tempTrack) )  {
	  StThreeVectorF trackPredictor = tempTrack->getProjectedMIP();
	  float adcCut = 100;
	  StRichHit* mAssociatedMIP = 0;
	  float smallestResidual = 3.0*centimeter;
	  float adcCut = 300;
	  for (StSPtrVecRichHitIterator hitIter = hits->begin(); hitIter != hits->end(); ++hitIter) {
	    testThisResidual = ((*hitIter)->local() - trackPredictor);      
	    if (testThisResidual.perp() < smallestResidual && (*hitIter)->charge() > adcCut) {
	      smallestResidual = testThisResidual.perp();   
	      mAssociatedMIP   = *hitIter;
	    }
	  }
	  // mAssociatedMIP->setMipFlag();
	  mAssociatedMIP = 0;
	}
	delete tempTrack;
      }
    }
  }
}

void StRichPIDMaker::setAdcCut(int cut) {mAdcCut = cut;}

  id=0;


Int_t StRichPIDMaker::Init() {
  mRefit = false;

  pion    = StPionMinus::instance();
  kaon    = StKaonMinus::instance();
  proton  = StAntiProton::instance();

  doGapCorrection      = true;
  pionSaturatedArea    = 100.0;
  kaonSaturatedArea    = 100.0;
  protonSaturatedArea  = 100.0;

  mListOfParticles.clear();
  mListOfParticles.resize(3);
  mListOfParticles[0]   = pion;
  mListOfParticles[1]   = kaon;
  mListOfParticles[2]  = proton;

  mMaterialDb = StRichMaterialsDb::getDb();
  mGeometryDb = StRichGeometryDb::getDb(); 
  
  mMaterialDb->setWavelengthRange(mShortWave,mLongWave);
  if (mDefaultShortWave != mShortWave && mDefaultLongWave != mLongWave) {


#ifdef  myRICH_WITH_NTUPLE
#ifdef  myRICH_WITH_NTUPLE 
  file = new TFile(finalname,"RECREATE");  

  file = new TFile(finalname,"RECREATE");
  trackCorrectedNtuple = new TNtuple("trackCorrectedNtuple","","vz:globalpx:globalpy:globalpz:localpx:localpy:localpz:olocalpx:olocalpy:olocalpz:eta:q:amipq:amipx:amipy:pmipx:pmipy:radx:rady:oradx:orady:opmipx:omipy:lasthitx:lasthity:lasthitz:tpchits:tpcfits:pionf:piontotarea:pionconstarea:piontotang:pionconstang:piontothits:pionconsthits:kaonf:kaontotarea:kaonconstarea:kaontotang:kaonconstang:kaontothits:kaonconsthits:prof:prototarea:proconstarea:prototang:proconstang:protothits:proconsthits:pionchits:pioncthits:kaonchits:kaoncthits:prochits:procthits:wave1:wave2");
#endif

  trackCorrectedNtuple = new TNtuple("trackCorrectedNtuple","","vz:globalpx:globalpy:globalpz:localpx:localpy:localpz:olocalpx:olocalpy:olocalpz:eta:q:amipq:amipx:amipy:pmipx:pmipy:radx:rady:oradx:orady:opmipx:opmipy:lasthitx:lasthity:lasthitz:tpchits:tpcfits:pionf:piontotarea:pionconstarea:piontotang:pionconstang:piontothits:pionconsthits:kaonf:kaontotarea:kaonconstarea:kaontotang:kaonconstang:kaontothits:kaonconsthits:prof:prototarea:proconstarea:prototang:proconstang:protothits:proconsthits:pionchits:pioncthits:kaonchits:kaoncthits:prochits:procthits:wave1:wave2");

  trackNtuple = new TNtuple("trackNtuple","trackwise tuple",
			    "evtn:nprimaries:nnegprimaries:posz:negz:vz:nrichtracks:globalp:globalpt:globalpx:globalpy:globalpz:localpx:localpy:localpz:eta:q:amipq:amipx:amipy:pmipx:pmipy:radx:rady:oradx:orady:otheta:ophi:ctbx:ctby:ctbz:firstrow:lastrow:lasthitx:lasthity:lasthitz:lasthitdca:pathlength:maxchain:maxgap:theta:phi:tpchits:tpcfitpoints:pionfactor:piontotalarea:pionconstarea:piontotalangle:pionconstangle:piontotalhits:pionconsthits:kaonfactor:kaontotalarea:kaonconstarea:kaontotalangle:kaonconstangle:kaontotalhits:kaonconsthits:protonfactor:protontotalarea:protonconstarea:protontotalangle:protonconstangle:protontotalhits:protonconsthits:innerwave:outerwave");

#ifdef myRICH_WITH_MC
  geantTrackNtuple = new TNtuple("geantTrackNtuple","geant trackwise tuple",
				 "evtn:nprimaries:nnegprimaries:vz:nrichtracks:globalpx:globalpy:globalpz:localpx:localpy:localpz:eta:q:amipid:amipproc:amipq:amipx:amipy:amipnpads:pmipx:pmipy:gmipid:gmipproc:gmipq:gmipx:gmipy:gmipnpads:radx:rady:oradx:orady:olocalpx:olocalpy:olocalpz:firstrow:lastrow:lasthitx:lasthity:lasthitz:lasthitdca:pathlength:maxchain:maxgap:tpchits:tpcfitpoints:innerwave:outerwave:glocalpx:glocalpy:glocalpz:gradx:grady:geantmipx:geantmipy:gstopvertx:gstopverty:gstopvertz:gphots:grecophots:gradhits:gtpccommonhits:gglobalpx:gglobalpy:gglobalpz:gid:gstopproc:gnpartners:pionfactor:piontotalarea:pionconstarea:piontotalangle:pionconstangle:piontotalhits:pionconsthits:kaonfactor:kaontotalarea:kaonconstarea:kaontotalangle:kaonconstangle:kaontotalhits:kaonconsthits:protonfactor:protontotalarea:protonconstarea:protontotalangle:protonconstangle:protontotalhits:protonconsthits");
  

  geantPhotonNtuple = new TNtuple("geantPhotonNtuple","geant photon wise tnuple","q:localpx:localpy:localpz:radx:rady:gradx:grady:pmipx:pmipy:amipid:amipproc:amipq:amipx:amipy:amipnpads:gamipid:gamipproc:gamipq:gamipx:gamipy:gamipnpads:gmipx:gmipy:glocalpx:glocalpy:glocalpz:theta:gtheta:phi:gphi:gid:signal:gwave:gpsi:gz:gproc:x:y:gx:gy:gmass:constangle:trdist:trang:tgdist:tgang:gconstangle:grdist:grang:ggdist:ggang");
    geantCloseHitNtuple = new TNtuple("geantHitNtuple","pixels","p:theta:w1:w2:psi1:psi2:z1:z2:x1:y1:x2:y2:constAngle");
  geantPixelNtuple = new TNtuple("geantPixelNtuple","pixels","adc:n:gid0:gid1:gid2:gid3:q0:q1:q2:q3:proc0:proc1:proc2:proc3");
    pionTheta_x = new TH3F("pionTheta_x","hit distribution",100,-1,1,40,-2,2,100,-5,5);


  pionResid   = new TH3F("pionResid","hit distribution",16,0,4,20,0,2,100,-5,5);
  pionResid_x = new TH3F("pionResid_x","hit distribution",16,0,4,40,-2,2,100,-5,5);
  pionResid_y = new TH3F("pionResid_y","hit distribution",16,0,4,40,-2,2,100,-5,5);

  pionTheta   = new TH3F("pionTheta","hit distribution",100,-1,1,20,0,2,100,-5,5);
  pionTheta_x = new TH3F("pionTheta_x","hit distribution",100,-1,1,40,-2,2,100,-5,5);
  pionTheta_y = new TH3F("pionTheta_y","hit distribution",100,-1,1,40,-2,2,100,-5,5);
  
  pionPsib     = new TH3F("pionPsib","hit dist",100,-1,1,180,-180,180,100,-5,5);

    
  pionCorrectedResid   = new TH3F("pionCorrectedResid","hit distribution",16,0,4,20,0,2,100,-5,5);
  pionCorrectedResid_x = new TH3F("pionCorrectedResid_x","hit distribution",16,0,4,40,-2,2,100,-5,5);
  pionCorrectedResid_y = new TH3F("pionCorrectedResid_y","hit distribution",16,0,4,40,-2,2,100,-5,5);

  pionCorrectedTheta   = new TH3F("pionCorrectedTheta","hit distribution",100,-1,1,20,0,2,100,-5,5);
  pionCorrectedTheta_x = new TH3F("pionCorrectedTheta_x","hit distribution",100,-1,1,40,-2,2,100,-5,5);
  pionCorrectedTheta_y = new TH3F("pionCorrectedTheta_y","hit distribution",100,-1,1,40,-2,2,100,-5,5);


  hitNtup = new TNtuple("hitNtup","hitwise ","npads:adcsum:trackmip:x:y:ring:p:resid:tpchits:maxamp:residx:residy:theta:quratz:rad:angle");
  pionRadius  = new TH3F("pionRadius","hit dist",16,0,4,180,0,18,100,-5,5);
  
  hitNtup = new TNtuple("hitNtup","hitwise ","npads:adcsum:trackmip:x:y:ring:p:resid:tpchits:maxamp:residx:residy:theta:quartz:rad:angle");
  
  closeHitNtup = new TNtuple("closeHitNtup","hitwise ","p:theta:n:constAngle:psi1:psi2:x1:y1:x2:y2");

  overLap = new TNtuple("overLap","hit distribution","p:theta:dist:pionhit:kaonhit:angle");

  evtNtup = new TNtuple("evtNtup","evtwise","rtracks:rtracks1gev:pri:npri:vx:vy:vz:hits:pixels");

  psiFixNtuple = new TNtuple("psiFixNtuple","","dist:sigma:hitangle:constangle:area:charge:px:py:pz:mass:x:y");

  return StMaker::Init();
}


void StRichPIDMaker::Clear(Option_t *opt) {
  StMaker::Clear();
    pionTheta_y = new TH3F("pionTheta_y","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    

    pionPsi     = new TH3F("pionPsi","hit dist",100,-1,1,180,-180,180,100,-5,5);

    //
    //
    pionResidb   = new TH3F("pionResidb","hit distribution",16,0,4,20,0,2,100,-5,5);
    pionResid_xb = new TH3F("pionResid_xb","hit distribution",16,0,4,40,-2,2,100,-5,5);
    pionResid_yb = new TH3F("pionResid_yb","hit distribution",16,0,4,40,-2,2,100,-5,5);
    
    pionThetab   = new TH3F("pionThetab","hit distribution",100,-1,1,20,0,2,100,-5,5);
    pionTheta_xb = new TH3F("pionTheta_xb","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    pionTheta_yb = new TH3F("pionTheta_yb","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    
    pionPsib     = new TH3F("pionPsib","hit dist",100,-1,1,180,-180,180,100,-5,5);

    pionCorrectedResid   = new TH3F("pionCorrectedResid","hit distribution",16,0,4,20,0,2,100,-5,5);
    pionCorrectedResid_x = new TH3F("pionCorrectedResid_x","hit distribution",16,0,4,40,-2,2,100,-5,5);
    pionCorrectedResid_y = new TH3F("pionCorrectedResid_y","hit distribution",16,0,4,40,-2,2,100,-5,5);
    
    pionCorrectedTheta   = new TH3F("pionCorrectedTheta","hit distribution",100,-1,1,20,0,2,100,-5,5);
    pionCorrectedTheta_x = new TH3F("pionCorrectedTheta_x","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    pionCorrectedTheta_y = new TH3F("pionCorrectedTheta_y","hit distribution",100,-1,1,40,-2,2,100,-5,5);

    pionCorrectedPsi     = new TH3F("pionCorrectedPsi","hit dist",100,-1,1,180,-180,180,100,-5,5);

    pionCorrectedResidb   = new TH3F("pionCorrectedResidb","hit distribution",16,0,4,20,0,2,100,-5,5);
    pionCorrectedResid_xb = new TH3F("pionCorrectedResid_xb","hit distribution",16,0,4,40,-2,2,100,-5,5);
    pionCorrectedResid_yb = new TH3F("pionCorrectedResid_yb","hit distribution",16,0,4,40,-2,2,100,-5,5);

    pionCorrectedThetab   = new TH3F("pionCorrectedThetab","hit distribution",100,-1,1,20,0,2,100,-5,5);
	richMcTrack->getStTrack()->node() && richMcTrack->getStTrack()->node()->track(global)) {

      StGlobalTrack* globaltrack = dynamic_cast<StGlobalTrack*>(richMcTrack->getStTrack()->node()->track(global));
    //
    //
    pionRadius  = new TH3F("pionRadius","hit dist",16,0,4,180,0,18,100,-5,5);
  
    hitNtup = new TNtuple("hitNtup","hitwise ","npads:adcsum:trackmip:x:y:ring:p:resid:tpchits:maxamp:residx:residy:theta:quartz:rad:angle");
  
    closeHitNtup = new TNtuple("closeHitNtup","hitwise ","p:theta:n:constAngle:psi1:psi2:x1:y1:x2:y2");
    
    overLap = new TNtuple("overLap","hit distribution","p:theta:dist:pionhit:kaonhit:angle");
    
    evtNtup = new TNtuple("evtNtup","evtwise","rtracks:rtracks1gev:pri:npri:vx:vy:vz:hits:pixels");
    
    psiFixNtuple = new TNtuple("psiFixNtuple","","dist:sigma:hitangle:constangle:area:charge:px:py:pz:mass:x:y");
#endif
}

#ifdef myRICH_WITH_MC
bool StRichPIDMaker::makeTrackAssociations(StMcEvent* temp_mEvent, const StSPtrVecRichHit* hits ) {

  if (!temp_mEvent || !hits) {
    cout << "No StMcEvent/rich hits!" << endl;
    return false;}
  
  // StAssociationMaker
  StAssociationMaker* assoc = 0;
  assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");
  if (!assoc) {
    cout << "No association maker!" << endl;
    return false;} 
  
  rcTrackMapType*  theTrackMap = 0;
  theTrackMap = assoc->rcTrackMap();
  if (!theTrackMap) {
    cout << "no track map!" << endl;
    return false;} 

  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) { // track    

    unsigned int maxCommonTpcHits = 0;
    unsigned int numberOfPartners = 0;

    StMcTrack*      mcPartner   = 0;
    StRichMCTrack*  richMcTrack = dynamic_cast<StRichMCTrack*>(mListOfStRichTracks[trackIndex]);
    cout << "rich mc track = " << richMcTrack << endl;
    if (richMcTrack && richMcTrack->getStTrack() && 
	richMcTrack->getStTrack()->node() &&
	richMcTrack->getStTrack()->node()->track(global)) {
      
      StGlobalTrack* globaltrack =
	  dynamic_cast<StGlobalTrack*>(richMcTrack->getStTrack()->node()->track(global));
      cout << "global track pointer= " << globaltrack << endl;

      if (globaltrack) { 
	pair<rcTrackMapIter,rcTrackMapIter> trackBounds = theTrackMap->equal_range(globaltrack);
	cout << "about to loop over track bounds " << endl;
	
	for (rcTrackMapIter rcIt = trackBounds.first; rcIt != trackBounds.second; ++rcIt) {
	  numberOfPartners++;  
	  if ((*rcIt).second->commonTpcHits() >  maxCommonTpcHits) {
	    mcPartner        = (*rcIt).second->partnerMcTrack();
	    maxCommonTpcHits = (*rcIt).second->commonTpcHits();
	  }
	}
	
	
	// need to get the g2t info  
	St_DataSet* dsGeant = GetDataSet("geant");
	if(!dsGeant || !dsGeant->GetList()) {
	  gMessMgr->Warning() << "Could not find dataset geant" << endm;
	  dsGeant = GetDataSet("event/geant/Event");
	  if(!dsGeant || !dsGeant->GetList()) {  // Try direct output from Geant
	    gMessMgr->Warning() << "Could not find dataset event/geant/Event" << endm;
	    return false;
	  }
	}
       

  cout << "writing file to tape" << endl;
  
#ifdef  myRICH_WITH_NTUPLE 
  file = 0;
#endif

  return kStOK;
}

  // calculate distance from inner,mean, outer rings

void StRichPIDMaker::hitFilter(const StSPtrVecRichHit* richHits, StRichRingCalculator* ringCalculator) {
    float adcCut = 300;
  if (richHits) {
    for (StSPtrVecRichHitConstIterator hitIter = richHits->begin(); hitIter != richHits->end(); hitIter++) {
     
      if ((*hitIter)->charge()<adcCut) {
	

	StThreeVectorF hit = (*hitIter)->local();

	ringCalculator->clear();  
	StThreeVectorF hit = tempHit->local();
	
	innerDistance = ringCalculator->getInnerDistance(hit,innerAngle);
	outerDistance = ringCalculator->getOuterDistance(hit,outerAngle);
	meanDistance  = ringCalculator->getMeanDistance(hit,meanAngle);
	if (ringWidth==0) {return;}
				    ringCalculator->getRing(eInnerRing)->getParticleType());
	float dist  = ((outerDistance/ringWidth > 1 && (innerDistance/ringWidth < outerDistance/ringWidth )) ? 
		       (-1.0):(1.0))*innerDistance/ringWidth;
	if (dist>0 && dist<1) mNumberOfRingHits++;
	// make cuts
	if (fabs(dist)<5) {
	  ringCalculator->getRing(eInnerRing)->getTrack()->addHit((*hitIter),dist,meanAngle,radPath,quartzPath,
			 ringCalculator->getRing(eInnerRing)->getParticleType());
	}
	  }
	}
	
      }
    }
  }
  
  return;
}


void StRichPIDMaker::hitFilter(StRichRingCalculator* ringCalculator,
			          StThreeVectorF& hit, float& angle, float& dist) {
  
  // calculate distance from inner,mean, outer rings

  ringCalculator->clear();  
  angle = -999;
  dist  = -999;
  innerDistance = ringCalculator->getInnerDistance(hit,innerAngle);
  outerDistance = ringCalculator->getOuterDistance(hit,outerAngle);
  meanDistance  = ringCalculator->getMeanDistance(hit,meanAngle);
  ringWidth     = ringCalculator->getRingWidth();
  if (ringWidth==0) {return;}  
  angle = meanAngle;
  dist  = ((outerDistance/ringWidth > 1 && (innerDistance/ringWidth < outerDistance/ringWidth )) ? 
	   (-1.0):(1.0))*innerDistance/ringWidth;
  return;
  float mean = 0.45;
  return fabs(hitDist-mean)/sigma;
}


void StRichPIDMaker::DefineSaveDirectory(char* directory) {
  mySaveDirectory=directory;
}


void StRichPIDMaker::setFileName(char * name){
    fileName = name;
void StRichPIDMaker::setWavelengthRange(double shortest, double longest) {


  mLongWave  = longest;
  mMaterialDb = StRichMaterialsDb::getDb();
  mMaterialDb->setWavelengthRange(mShortWave,mLongWave);
}

	// Now the Iterator is set up, and this allows us to access the tables
	// This is done like so:
	// TableClass *instanceOfTableClassPointer = 
	// cast to TableClassPointer instanceOfDataSetIter("actual name of table in data set");
	St_DataSetIter geantDstI(dsGeant);
	St_g2t_track   *g2t_track = (St_g2t_track   *) geantDstI("g2t_track");

	richMcTrack->setStMcTrack(mcPartner);
	richMcTrack->setGeantPhotons(temp_mEvent);	
	richMcTrack->setCommonTpcHits(maxCommonTpcHits);
	richMcTrack->setNumberOfPartners(numberOfPartners);
	richMcTrack->setGeantRecoMIP(hits, temp_mEvent, g2t_track);
	richMcTrack->setRecoGeantPhotons(hits,temp_mEvent,g2t_track);	
      }
    }
  }
  return true;
}
#endif


#ifdef  myRICH_WITH_MC
StMcTrack* StRichPIDMaker::getStMcTrack(StRichMCHit* hit, StMcEvent* mcevt, St_g2t_track* geantTracks) {
    
  StMcTrack* mcTrack=0;
  if (!hit || !mcevt || !geantTracks) return mcTrack;
  g2t_track_st *trackList = geantTracks->GetTable();

  unsigned int hitIndex = 0;
  if (hit->getMCInfo().process()==ePhoton)       hitIndex = hit->getMCInfo().id();
  else if (hit->getMCInfo().process()==eCharged) hitIndex = hit->getMCInfo().trackp();
  else { return 0;}
  
  float  start=0;
  float  end = mcevt->tracks().size()-1;
  int    index = hitIndex;
  int    counter=0;
  bool   searching=true;
  while (searching) {
    if (index >= mcevt->tracks().size()-1) return 0;
    if (trackList[hitIndex].id == mcevt->tracks()[index]->key()) {return mcevt->tracks()[index];}
    if (trackList[hitIndex].id > mcevt->tracks()[index]->key()) {start=index;}
    else {end=index;}
    index=(end-start)/2 + start;
    counter++;
    if (counter>mcevt->tracks().size()-1) searching=false; 
  }
  return mcTrack;  
}
#endif

#ifdef  myRICH_WITH_MC
void StRichPIDMaker::getGeantPhotonInfo(StRichMCTrack* richTrack, StMcTrack* photon, 
				     float& wave, float& gpsi, float& z) { 

  wave = -999;
  gpsi = -999;
  z = -999;
  if (!richTrack || !photon || photon->geantId()!=50) { return;}
   
  StRichLocalCoordinate localStartVert(-999,-999,-999);
  if (photon->startVertex()) {
    StGlobalCoordinate globalStartVert(photon->startVertex()->position().x(),
				       photon->startVertex()->position().y(),
				       photon->startVertex()->position().z());
    (*mCoordinateTransformation)(globalStartVert,localStartVert);
  }
  z = localStartVert.position().z();


  // get photons momentum at emission pt
 StThreeVector<double> globalMomentum(photon->momentum().x(),
				      photon->momentum().y(),
				      photon->momentum().z());
  
  // get geant photons azimuthal angle of emission
  // need to normalize vector (mag of photon vector ~ 1.0e-9)
  if (globalMomentum.mag()==0) {return;}
  globalMomentum.setMag(1.0);
  StThreeVector<double> localMomentum;
  mMomentumTransformation->localMomentum(globalMomentum,localMomentum);

  StThreeVectorF trackLocalMomentum = richTrack->getGeantMomentumAtRadiator();  
  if (trackLocalMomentum.mag() == 0) {return;}
  trackLocalMomentum.setMag(1.0);

  StThreeVectorF vect(0.0,0.0,1.0); 
  double rotationTheta = acos(vect.dot(trackLocalMomentum));
  double rotationPhi   = trackLocalMomentum.phi();
  
  // do rotation
  StThreeVectorF 
    rotatedTrackMomentum(cos(rotationTheta)*cos(rotationPhi)*trackLocalMomentum.x() +
			 cos(rotationTheta)*sin(rotationPhi)*trackLocalMomentum.y() -
			 sin(rotationTheta)*trackLocalMomentum.z(),
			 
			 -sin(rotationPhi)*trackLocalMomentum.x() +
			 cos(rotationPhi)*trackLocalMomentum.y(),
			 
			 cos(rotationPhi)*sin(rotationTheta)*trackLocalMomentum.x() +
			 sin(rotationPhi)*sin(rotationTheta)*trackLocalMomentum.y() +
			 cos(rotationTheta)*trackLocalMomentum.z());
  
  StThreeVectorF 
    photonRotatedMomentum(cos(rotationTheta)*cos(rotationPhi)*localMomentum.x() +
			  cos(rotationTheta)*sin(rotationPhi)*localMomentum.y() -
			  sin(rotationTheta)*localMomentum.z(),
			  
			  -sin(rotationPhi)*localMomentum.x() +
			  cos(rotationPhi)*localMomentum.y(),
			  
			  cos(rotationPhi)*sin(rotationTheta)*localMomentum.x() +
			  sin(rotationPhi)*sin(rotationTheta)*localMomentum.y() +
			  cos(rotationTheta)*localMomentum.z());
  
  gpsi = atan2( rotatedTrackMomentum.x() - photonRotatedMomentum.x(),
		rotatedTrackMomentum.y() - photonRotatedMomentum.y());
  
  double constantPhaseDifference = M_PI/2.0;
  gpsi = gpsi - constantPhaseDifference;
  if (gpsi < -M_PI) {gpsi = gpsi + 2.0*M_PI;}
  
  // get photons wavelength (nm)
  wave = ( ( (h_Planck/eV)*c_light)/(photon->energy()/eV) )/nanometer; 
}
#endif


#ifdef  myRICH_WITH_MC
StThreeVectorF StRichPIDMaker::getTheGeantHitOnPadPlane(StMcTrack* mcTrack, StThreeVectorF& inputHit) { 
  
  
  double anodeDistanceToPadPlane = mGeometryDb->anodeToPadSpacing();
  float  defaultValue = -999.9;
  StThreeVectorF hit(defaultValue,defaultValue,defaultValue);
  StThreeVectorF tempHit(defaultValue,defaultValue,defaultValue);
  if (!mcTrack) return hit;

  // photon case  --> get point on pad plane
  if (mcTrack->geantId()==50) {

    StGlobalCoordinate testGlobal(mcTrack->richHits()[0]->position().x(),
				  mcTrack->richHits()[0]->position().y(),
				  mcTrack->richHits()[0]->position().z());
    
    StRichLocalCoordinate testLocalPoint(0,0,0);
    (*mCoordinateTransformation)(testGlobal,testLocalPoint);
    
    hit.setX(testLocalPoint.position().x());
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
  
    








