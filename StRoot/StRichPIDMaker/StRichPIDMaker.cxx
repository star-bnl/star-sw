/******************************************************
 * $Id: StRichPIDMaker.cxx,v 1.1 2000/04/03 19:36:08 horsley Exp $
 * 
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRichPIDMaker.cxx,v $
 * Revision 1.1  2000/04/03 19:36:08  horsley
 * initial revision
 *
 *
 *
 * Revision 2.12  2000/11/07 14:11:39  lasiuk
 * initCutParameters() and diagnositis print added.
 * bins for <d> and sigma_d added.
#define myrICH_WITH_PADMONITOR 1
 *
 * Revision 1.2  2000/05/19 19:06:10  horsley
#include "StPhysicalHelixD.hh"
#include "StEvent/StRichPidTraits.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichGeometryDb.h"
// g2t tables
// StRichPIDMaker
	cout << "\tWARNING! Cannot get B field from event->summary().  Use B= " << mMagField << endl;
#include "SystemOfUnits.h"
	cout << "\tERROR: No Vertex. Skipping..." << endl;
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 1.1 2000/04/03 19:36:08 horsley Exp $";
// StChain, etc...
#include "St_DataSet.h"
#include "TNtuple.h"
static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 1.1 2000/04/03 19:36:08 horsley Exp $";
  
  StDetectorId RICHDetectorID = kRichId;

  padMonitor->getParticleNames();   
  // grab StEvent
  StEvent* mEvent;
  mEvent = (StEvent *) GetInputDS("StEvent");
  padMonitor->clearRingList();
  if (!mEvent) {
    cout << "No mEvent! Can not continue. " << endl;
    return kStOK; // If no event, we're done
  if (mListOfStRichTracks.size() > 0) {
 
  StEvent& ev = *mEvent;
  double magField = ev.summary()->magneticField();
  cout << endl << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;
  cout << "StEvent:: magnetic field " << magField << "   tesla " << endl;
    mListOfStRichTracks.clear();
    mListOfStTracks.clear();
  if (mListOfStRichTracks.size() > 0) mListOfStRichTracks.clear();
  if (mListOfStTracks.size() > 0)     mListOfStTracks.clear();
    mListOfStTracks.resize(0);
  // grab tracks intersecting RICH
  StSPtrVecTrackNode& theTrackNodes = ev.trackNodes();
  double lowerMomentumLimit = 0.5;

  for (unsigned int nodeIndex=0; nodeIndex<theTrackNodes.size(); nodeIndex++) {
    
    long numberOfTracksInNode =  theTrackNodes[nodeIndex]->entries(global);
    
    for (int trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++)  {
  if(!rchEvent) { 
      StRichTrackFilter trackFilter(theTrackNodes[nodeIndex]->track(global,trackIndex),magField);
      bool onRadiator = trackFilter.onRadiator();
      bool fastEnough = trackFilter.momentumIsAbove(lowerMomentumLimit);
      bool thetaOK = trackFilter.incidentAngleCheck();
      if (onRadiator && fastEnough && thetaOK) {
	mListOfStRichTracks.push_back(new  
	      StRichTrack(theTrackNodes[nodeIndex]->track(global,trackIndex),magField));
       
      }
    }  
  mRichHits = (StRichSimpleHitCollection*)(rchEvent->GetObject());
  if(!mRichHits) {
    
  if (rEvent->primaryVertex()) {
  // grab RICH pixels
   StRichPixelCollection* richPixels = ev.richPixelCollection();
   vector< StThreeVector<double> > richHits;
   StThreeVector<double> hit(0.0,0.0,0.0);
   richHits.push_back(hit);
   // ------------------> cluster finder goes here?

   
  // loop over the selected RICH tracks
   for (unsigned int trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
     
     if (trackIndex>0) break;
  if (ev.richPixelCollection()) {
     StRichRingCalculator ringCalc(mListOfStRichTracks[trackIndex]);

     for (int particleIndex=0; particleIndex<mListOfParticles.size(); particleIndex++) {
    for (size_t trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++)  {
       int    hits         = 0;
       double totalArea    = 0.0;
       double padPlaneArea = 0.0;

       if (mListOfStRichTracks[trackIndex]->fastEnough(mListOfParticles[particleIndex])) {
	 
	 ringCalc.setParticleType(mListOfParticles[particleIndex]);
	 ringCalc.calculateArea(0.0);

	 totalArea    = ringCalc.getTotalArea();
	 padPlaneArea = ringCalc.getPadPlaneArea();
	 
	 
	 for (unsigned int hitIndex=0; hitIndex<richHits.size(); hitIndex++) {	
	   if (hitFilter(richHits[hitIndex],ringCalc)) hits++;        
	 } //  -----> loop over hits
       }
       

       //StRichPIDTraits pidTrait(RICHDetectorID,mListOfParticles[particleIndex],hits,padPlaneArea,totalArea);
       //mListOfStRichTracks[trackIndex]->getTrackPointer()->addPidTraits(&pidTrait);

     } //  -------> loop over particle types 

   } // ---------> loop over tracks
    padMonitor->drawRings(bestRings,1);
    padMonitor->update();}
  for (size_t trackIndex=0;trackIndex<mListOfStRichTracks.size();trackIndex++) {
  memory->snapshot();
  memory->print();
  if (evtN%enoughEventsToWriteFile == 0) file->Write();
  
  mPadMonitor->update();  
  if (kCreatePsFile) mPadMonitor->printCanvas("/star/rcf/scratch/horsley/",fileName,rEvent->id());    
#endif 
StRichPIDMaker::StRichPIDMaker(const Char_t *name) : StMaker(name) {
  drawinit = kFALSE;
}

StRichPIDMaker::~StRichPIDMaker() {}
  mLongestPathLength = 10e10;
  mMinimumNumberOfTrackPoints = 10;
  evtN = 0;
  mRichGeometryDb = StRichGeometryDb::getDb();
  
  mRichNormalVector.setX(mRichGeometryDb->normalVectorToPadPlane().x());
  mRichNormalVector.setY(mRichGeometryDb->normalVectorToPadPlane().y());
  mRichNormalVector.setZ(mRichGeometryDb->normalVectorToPadPlane().z());
  
  StRichCoordinateTransform*  coordinateTransformation =
    StRichCoordinateTransform::getTransform(mRichGeometryDb);
  
  StThreeVector<double> localRadPoint(0.0,
				      0.0, 
				      mRichGeometryDb->proximityGap() 
                                    + mRichGeometryDb->quartzDimension().z()
                                    + mRichGeometryDb->radiatorDimension().z());
  
  StRichLocalCoordinate mRichLocalPointOnRadiator(localRadPoint);
      for (size_t pidcounter=0;pidcounter<thepids.size();pidcounter++) {
  StGlobalCoordinate mGlobalPointOnRadiator;
  StPionMinus*  pionminus   = StPionMinus::instance();
  (*coordinateTransformation)(mRichLocalPointOnRadiator,mGlobalPointOnRadiator);
  StPionPlus*  pionplus   = StPionPlus::instance();
  mRichGlobalRadiatorPoint.setX(mGlobalPointOnRadiator.position().x());
  mRichGlobalRadiatorPoint.setY(mGlobalPointOnRadiator.position().y());
  mRichGlobalRadiatorPoint.setZ(mGlobalPointOnRadiator.position().z());


 // define particles (charge does not matter) 
  StPionMinus* pion     = StPionMinus::instance();
  StKaonMinus* kaon     = StKaonMinus::instance();
  StElectron*  electron = StElectron::instance();
  StProton*    proton   = StProton::instance();
  mListOfPositiveParticles[0] = pionplus;
  // mListOfParticles.push_back(electron);
  mListOfParticles.push_back(pion);
  // mListOfParticles.push_back(kaon);
  //mListOfParticles.push_back(proton);  

  mListOfPositiveParticles[2] = proton; 
  
  mListOfParticles.resize(mListOfPositiveParticles.size());
  return StMaker::Init();
}
	if (thepids[pidcounter]->getRingType()==pion)  {
	  } 
void StRichPIDMaker::Clear(Option_t *opt) {
  StMaker::Clear();
}
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
	    }

Int_t StRichPIDMaker::hitFilter(StThreeVector<double>& hit, 
				StRichRingCalculator& ringCalculator) {
  if (innerDistance/ringWidth < 1.0 && outerDistance/ringWidth < 1.0) {
  	innerDistance = ringCalculator.getInnerDistance(hit,innerAngle);
	outerDistance = ringCalculator.getOuterDistance(hit,outerAngle);
	ringWidth     = ringCalculator.getRingWidth();

	double normalDist = innerDistance/ringWidth;
	double reconstructedAngle = innerAngle*(1.0 - normalDist) 
	                                  + outerAngle*normalDist;
	if (reconstructedAngle > -10000.0) return 1;
	return 0;

  
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
  
    








