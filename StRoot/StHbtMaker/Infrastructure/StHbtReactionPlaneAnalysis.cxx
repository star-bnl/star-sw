/***************************************************************************
 *
 * $Id: StHbtReactionPlaneAnalysis.cxx,v 1.6 2005/06/28 23:12:53 chajecki Exp $
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
 * Revision 1.6  2005/06/28 23:12:53  chajecki
 * UncorrectedNumberOfNegativePrimaries() -> UncorrectedNumberOfPrimaries()
 *
 * For data taken in Y2 and later the centrality definition bases
 * on UncorrectedNumberOfPrimaries() while for Y1(AuAu@130)
 * it based on UncorrectedNumberOfNegativePrimaries().
 * But in many places of HBT code the number of negative primaries
 * was used as a multiplicity for all productions.
 * This has been fixed.
 *
 * Revision 1.5  2004/04/12 14:05:46  magestro
 * Added Vz dimension to event-mixing
 *
 * Revision 1.4  2002/05/28 14:04:07  rcwells
 * Added multiplicity binning to StHbtReactionPlaneAnalysis
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
StHbtReactionPlaneAnalysis::StHbtReactionPlaneAnalysis(int PtWgt, unsigned int binsRP, double minRP, double maxRP,
						       unsigned int binsMult, double minMult, double maxMult, 
                   unsigned int binsVert, double minVert, double maxVert){
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mReactionPlaneBins = binsRP;
  mReactionPlane[0] = minRP;
  mReactionPlane[1] = maxRP;
  mUnderFlow = 0; 
  mOverFlow = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mReactionPlaneBins,mReactionPlane[0],mReactionPlane[1],binsMult,minMult,maxMult
                  ,binsVert,minVert,maxVert);
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
  // get right mixing buffer
  mVertexZ = hbtEvent->PrimVertPos().z();
  if (mPtWgt) mReactionPlaneAngle = hbtEvent->ReactionPlane(1);
  else mReactionPlaneAngle = hbtEvent->ReactionPlane(0);
  // cout << "Reaction Plane " << mReactionPlaneAngle << endl; 
  if (mReactionPlaneAngle<-990) {
    cout << "No event plane!" << endl;
    return;
  }
  if ( mReactionPlaneAngle<0.0 ) mReactionPlaneAngle+=2*pi;
  int multiplicity = hbtEvent->UncorrectedNumberOfPrimaries();
  mMixingBuffer = mPicoEventCollectionVectorHideAway->PicoEventCollection(mReactionPlaneAngle,(double)multiplicity, mVertexZ); 
  if (!mMixingBuffer) {
    if ( mReactionPlaneAngle < mReactionPlane[0] ) mUnderFlow++;
    if ( mReactionPlaneAngle > mReactionPlane[1] ) mOverFlow++;
    return;
  }
  // call ProcessEvent() from StHbtAnalysis-bas
  StHbtAnalysis::ProcessEvent(hbtEvent);
}
