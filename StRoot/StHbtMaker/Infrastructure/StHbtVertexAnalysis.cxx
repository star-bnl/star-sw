/***************************************************************************
 *
 * $Id: StHbtVertexAnalysis.cxx,v 1.5 2001/05/25 23:24:00 lisa Exp $
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
 * $Log: StHbtVertexAnalysis.cxx,v $
 * Revision 1.5  2001/05/25 23:24:00  lisa
 * Added in StHbtKink stuff
 *
 * Revision 1.4  2000/08/31 22:31:32  laue
 * StHbtAnalysis: output changed (a little bit less)
 * StHbtEvent: new version, members for reference mult added
 * StHbtIOBinary: new IO for new StHbtEvent version
 * StHbtTypes: TTree typedef to StHbtTTree added
 * StHbtVertexAnalysis: overflow and underflow added
 *
 * Revision 1.1  2000/07/16 21:44:11  laue
 * Collection and analysis for vertex dependent event mixing
 *
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtVertexAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVector.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVectorHideAway.hh"


#ifdef __ROOT__ 
ClassImp(StHbtVertexAnalysis)
#endif

extern void FillHbtParticleCollection(StHbtParticleCut*         partCut,
				     StHbtEvent*               hbtEvent,
				     StHbtParticleCollection*  partCollection);


//____________________________
StHbtVertexAnalysis::StHbtVertexAnalysis(unsigned int bins, double min, double max){
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mVertexBins = bins;
  mVertexZ[0] = min;
  mVertexZ[1] = max;
  mUnderFlow = 0; 
  mOverFlow = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mVertexBins,mVertexZ[0],mVertexZ[1]);
};
//____________________________

StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) : StHbtAnalysis() {
  //StHbtVertexAnalysis();
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mVertexBins = a.mVertexBins; 
  mVertexZ[0] = a.mVertexZ[0]; 
  mVertexZ[1] = a.mVertexZ[1];
  mUnderFlow = 0; 
  mOverFlow = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mVertexBins,mVertexZ[0],mVertexZ[1]);

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
      cout << " StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) - event cut set " << endl;
  }
  if ( mFirstParticleCut ) {
      SetFirstParticleCut(mFirstParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) - first particle cut set " << endl;
  }
  if ( mSecondParticleCut ) {
      SetSecondParticleCut(mSecondParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) - second particle cut set " << endl;
  }  if ( mPairCut ) {
      SetPairCut(mPairCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) - pair cut set " << endl;
  }

  StHbtCorrFctnIterator iter;
  for (iter=a.mCorrFctnCollection->begin(); iter!=a.mCorrFctnCollection->end();iter++){
    cout << " StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) - looking for correlation functions " << endl;
    StHbtCorrFctn* fctn = (*iter)->Clone();
    if (fctn) AddCorrFctn(fctn);
    else cout << " StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) - correlation function not found " << endl;
  }

  mNumEventsToMix = a.mNumEventsToMix;

  cout << " StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) - analysis copied " << endl;

}
//____________________________
StHbtVertexAnalysis::~StHbtVertexAnalysis(){
  // now delete every PicoEvent in the EventMixingBuffer and then the Buffer itself
  delete mPicoEventCollectionVectorHideAway;
}

//____________________________
StHbtString StHbtVertexAnalysis::Report()
{
  cout << "StHbtVertexAnalysis - constructing Report..."<<endl;
  char Ctemp[200];
  StHbtString temp = "-----------\nHbt StHbtVertexAnalysis Report:\n";
  sprintf(Ctemp,"Events are mixed in %d bins in the range %E cm to %E cm.\n",mVertexBins,mVertexZ[0],mVertexZ[1]);
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
void StHbtVertexAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {
  cout << " StHbtVertexAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) " << endl;
  // get right mixing buffer
  double vertexZ = hbtEvent->PrimVertPos().z();
  mMixingBuffer = mPicoEventCollectionVectorHideAway->PicoEventCollection(vertexZ); 
  if (!mMixingBuffer) {
    if ( vertexZ < mVertexZ[0] ) mUnderFlow++;
    if ( vertexZ > mVertexZ[1] ) mOverFlow++;
    return;
  }
  // call ProcessEvent() from StHbtAnalysis-base
  StHbtAnalysis::ProcessEvent(hbtEvent);
}
