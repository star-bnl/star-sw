/***************************************************************************
 *
 * $Id: StHbtReactionPlaneAnalysis.cxx,v 1.3 2002/03/27 19:00:44 rcwells Exp $
 *
 * Author: Randall Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtReactionPlaneAnalysis.cxx,v $
 * Revision 1.3  2002/03/27 19:00:44  rcwells
 * Corrected a test on the event plane angle
 *
 * Revision 1.2  2001/07/20 20:03:53  rcwells
 * Added pT weighting and moved event angle cal. to event cut
 *
 * Revision 1.1  2001/07/13 20:03:14  rcwells
 * Adding reaction plane analysis
 *
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtReactionPlaneAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVector.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVectorHideAway.hh"
#include "PhysicalConstants.h"

#ifdef __ROOT__ 
ClassImp(StHbtReactionPlaneAnalysis)
#endif

extern void FillHbtParticleCollection(StHbtParticleCut*         partCut,
				     StHbtEvent*               hbtEvent,
				     StHbtParticleCollection*  partCollection);


//____________________________
StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(int PtWgt, unsigned int bins, double min, double max){
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mReactionPlaneBins = bins;
  mReactionPlane[0] = min;
  mReactionPlane[1] = max;
  mUnderFlow = 0; 
  mOverFlow = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mReactionPlaneBins,mReactionPlane[0],mReactionPlane[1]);
  mPtWgt = PtWgt;
};
//____________________________

StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis& a) : StHbtAnalysis() {
  //StHbtReactionPlaneAnalysis();
  mPtWgt = a.mPtWgt;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mReactionPlaneBins = a.mReactionPlaneBins; 
  mReactionPlane[0] = a.mReactionPlane[0]; 
  mReactionPlane[1] = a.mReactionPlane[1];
  mUnderFlow = 0; 
  mOverFlow = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mReactionPlaneBins,mReactionPlane[0],mReactionPlane[1]);

  // find the right event cut
  mEventCut = a.mEventCut->Clone();
  // find the right first particle cut
  mFirstParticleCut = a.mFirstParticleCut->Clone();
  // find the right second particle cut
  if (a.mFirstParticleCut==a.mSecondParticleCut) 
    SetSecondParticleCut(mFirstParticleCut); // identical particle hbt
  else
  mSecondParticleCut = a.mSecondParticleCut->Clone();

  mPairCut = a.mPairCut->Clone();
  
  if ( mEventCut ) {
      SetEventCut(mEventCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis& a) - event cut set " << endl;
  }
  if ( mFirstParticleCut ) {
      SetFirstParticleCut(mFirstParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis& a) - first particle cut set " << endl;
  }
  if ( mSecondParticleCut ) {
      SetSecondParticleCut(mSecondParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis& a) - second particle cut set " << endl;
  }  if ( mPairCut ) {
      SetPairCut(mPairCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis& a) - pair cut set " << endl;
  }

  StHbtCorrFctnIterator iter;
  for (iter=a.mCorrFctnCollection->begin(); iter!=a.mCorrFctnCollection->end();iter++){
    cout << " StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis& a) - looking for correlation functions " << endl;
    StHbtCorrFctn* fctn = (*iter)->Clone();
    if (fctn) AddCorrFctn(fctn);
    else cout << " StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis& a) - correlation function not found " << endl;
  }

  mNumEventsToMix = a.mNumEventsToMix;

  cout << " StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(const StHbtReactionPlaneAnalysis& a) - analysis copied " << endl;

}
//____________________________
StHbtReactionPlaneAnalysis::~StHbtReactionPlaneAnalysis(){
  // now delete every PicoEvent in the EventMixingBuffer and then the Buffer itself
  delete mPicoEventCollectionVectorHideAway;
}

//____________________________
StHbtString StHbtReactionPlaneAnalysis::Report()
{
  cout << "StHbtReactionPlaneAnalysis - constructing Report..."<<endl;
  char Ctemp[200];
  StHbtString temp = "-----------\nHbt StHbtReactionPlaneAnalysis Report:\n";
  sprintf(Ctemp,"Events are mixed in %d bins in the range %E cm to %E cm.\n",mReactionPlaneBins,mReactionPlane[0],mReactionPlane[1]);
  temp += Ctemp;
  sprintf(Ctemp,"Events underflowing: %d\n",mUnderFlow);
  temp += Ctemp;
  sprintf(Ctemp,"Events overflowing: %d\n",mOverFlow);
  temp += Ctemp;
  sprintf(Ctemp,"Now adding StHbtAnalysis(base) Report\n");
  temp += Ctemp;
  temp += StHbtAnalysis::Report();
  StHbtString returnThis=temp;
  return returnThis;
}
//_________________________
void StHbtReactionPlaneAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {
  cout << " StHbtReactionPlaneAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) " << endl;
  // get right mixing buffer
  if (mPtWgt) mReactionPlaneAngle = hbtEvent->ReactionPlane(1);
  else mReactionPlaneAngle = hbtEvent->ReactionPlane(0);
  cout << "Reaction Plane " << mReactionPlaneAngle << endl; 
  if (mReactionPlaneAngle<-990) { // Test for a "good" event plane angle
    cout << "No event plane!" << endl;
    return;
  }
  if ( mReactionPlaneAngle<0.0 ) mReactionPlaneAngle+=2*pi;
  mMixingBuffer = mPicoEventCollectionVectorHideAway->PicoEventCollection(mReactionPlaneAngle); 
  if (!mMixingBuffer) {
    if ( mReactionPlaneAngle < mReactionPlane[0] ) mUnderFlow++;
    if ( mReactionPlaneAngle > mReactionPlane[1] ) mOverFlow++;
    return;
  }
  // call ProcessEvent() from StHbtAnalysis-bas
  StHbtAnalysis::ProcessEvent(hbtEvent);
}
double StHbtReactionPlaneAnalysis::ReactionPlane() {
  return mReactionPlaneAngle;
}
