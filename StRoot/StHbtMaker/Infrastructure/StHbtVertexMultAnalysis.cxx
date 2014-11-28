/***************************************************************************
 *
 * $Id: StHbtVertexMultAnalysis.cxx,v 1.2 2005/06/28 23:12:24 chajecki Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtVertexMultAnalysis.cxx,v $
 * Revision 1.2  2005/06/28 23:12:24  chajecki
 * UncorrectedNumberOfNegativePrimaries() -> UncorrectedNumberOfPrimaries()
 *
 * For data taken in Y2 and later the centrality definition bases
 * on UncorrectedNumberOfPrimaries() while for Y1(AuAu@130)
 * it based on UncorrectedNumberOfNegativePrimaries().
 * But in many places of HBT code the number of negative primaries
 * was used as a multiplicity for all productions.
 * This has been fixed.
 *
 * Revision 1.1  2001/11/11 18:34:14  laue
 * StHbtPicoEventCollectionVectorHideAway: updated for 3d grid
 * StHbtVertexMultAnalysis: new
 *
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtVertexMultAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVector.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVectorHideAway.hh"


#ifdef __ROOT__ 
ClassImp(StHbtVertexMultAnalysis)
#endif

extern void FillHbtParticleCollection(StHbtParticleCut*         partCut,
				     StHbtEvent*               hbtEvent,
				     StHbtParticleCollection*  partCollection);


//____________________________
StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(unsigned int binsVertex, double minVertex, double maxVertex,
						 unsigned int binsMult, double minMult, double maxMult) 
  : mVertexZBins(binsVertex), mMultBins(binsMult) {
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mVertexZ[0] = minVertex;
  mVertexZ[1] = maxVertex;
  mUnderFlowVertexZ = 0; 
  mOverFlowVertexZ = 0; 
  mMult[0] = minMult;
  mMult[1] = maxMult;
  mUnderFlowMult = 0; 
  mOverFlowMult = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mVertexZBins,mVertexZ[0],mVertexZ[1],
										  mMultBins,mMult[0],mMult[1]);
};
//____________________________

StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis& a) : StHbtAnalysis() {
  //StHbtVertexMultAnalysis();
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mVertexZBins = a.mVertexZBins; 
  mVertexZ[0] = a.mVertexZ[0]; 
  mVertexZ[1] = a.mVertexZ[1];
  mUnderFlowVertexZ = 0; 
  mOverFlowVertexZ = 0; 
  mMultBins = a.mMultBins; 
  mMult[0] = a.mMult[0]; 
  mMult[1] = a.mMult[1];
  mUnderFlowMult = 0; 
  mOverFlowMult = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mVertexZBins,mVertexZ[0],mVertexZ[1],
										  mMultBins,mMult[0],mMult[1]);

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
      cout << " StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis& a) - event cut set " << endl;
  }
  if ( mFirstParticleCut ) {
      SetFirstParticleCut(mFirstParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis& a) - first particle cut set " << endl;
  }
  if ( mSecondParticleCut ) {
      SetSecondParticleCut(mSecondParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis& a) - second particle cut set " << endl;
  }  if ( mPairCut ) {
      SetPairCut(mPairCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis& a) - pair cut set " << endl;
  }

  StHbtCorrFctnIterator iter;
  for (iter=a.mCorrFctnCollection->begin(); iter!=a.mCorrFctnCollection->end();iter++){
    cout << " StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis& a) - looking for correlation functions " << endl;
    StHbtCorrFctn* fctn = (*iter)->Clone();
    if (fctn) AddCorrFctn(fctn);
    else cout << " StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis& a) - correlation function not found " << endl;
  }

  mNumEventsToMix = a.mNumEventsToMix;

  cout << " StHbtVertexMultAnalysis::StHbtVertexMultAnalysis(const StHbtVertexMultAnalysis& a) - analysis copied " << endl;

}
//____________________________
StHbtVertexMultAnalysis::~StHbtVertexMultAnalysis(){
  // now delete every PicoEvent in the EventMixingBuffer and then the Buffer itself
  delete mPicoEventCollectionVectorHideAway;
}

//____________________________
StHbtString StHbtVertexMultAnalysis::Report()
{
  cout << "StHbtVertexMultAnalysis - constructing Report..."<<endl;
  char Ctemp[200];
  StHbtString temp = "-----------\nHbt StHbtVertexMultAnalysis Report:\n";
  sprintf(Ctemp,"Events are mixed in %d VertexZ bins in the range %E cm to %E cm.\n",mVertexZBins,mVertexZ[0],mVertexZ[1]);
  temp += Ctemp;
  sprintf(Ctemp,"Events underflowing: %d\n",mUnderFlowVertexZ);
  temp += Ctemp;
  sprintf(Ctemp,"Events overflowing: %d\n",mOverFlowVertexZ);
  temp += Ctemp;
  sprintf(Ctemp,"Events are mixed in %d Mult bins in the range %E cm to %E cm.\n",mMultBins,mMult[0],mMult[1]);
  temp += Ctemp;
  sprintf(Ctemp,"Events underflowing: %d\n",mUnderFlowMult);
  temp += Ctemp;
  sprintf(Ctemp,"Events overflowing: %d\n",mOverFlowMult);
  temp += Ctemp;
  sprintf(Ctemp,"Now adding StHbtAnalysis(base) Report\n");
  temp += Ctemp;
  temp += StHbtAnalysis::Report();
  StHbtString returnThis=temp;
  return returnThis;
}
//_________________________
void StHbtVertexMultAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {
  cout << " StHbtVertexMultAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) " << endl;
  // get right mixing buffer
  double vertexZ = hbtEvent->PrimVertPos().z();
  double mult = hbtEvent->UncorrectedNumberOfPrimaries();
  mMixingBuffer = mPicoEventCollectionVectorHideAway->PicoEventCollection(vertexZ,mult); 
  if (!mMixingBuffer) {
    if ( vertexZ < mVertexZ[0] ) mUnderFlowVertexZ++;
    if ( vertexZ > mVertexZ[1] ) mOverFlowVertexZ++;
    if ( mult < mMult[0] ) mUnderFlowMult++;
    if ( mult > mMult[1] ) mOverFlowMult++;
    return;
  }
  // call ProcessEvent() from StHbtAnalysis-base
  StHbtAnalysis::ProcessEvent(hbtEvent);
}
