/***************************************************************************
 *
 * $Id: StHbtAnalysis.cxx,v 1.3 1999/10/04 15:38:53 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtAnalysis.cxx,v $
 * Revision 1.3  1999/10/04 15:38:53  lisa
 * include Franks new accessor methods StHbtAnalysis::CorrFctn and StHbtManager::Analysis as well as McEvent example macro
 *
 * Revision 1.2  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"

ClassImp(StHbtAnalysis)

//____________________________
StHbtAnalysis::StHbtAnalysis(){
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mMixingBuffer = new StHbtPicoEventCollection;
}

//____________________________
StHbtAnalysis::~StHbtAnalysis(){
  //delete mControlSwitch     ;
  delete mEventCut          ;
  delete mFirstParticleCut  ;
  delete mSecondParticleCut ;
  delete mPairCut           ;
  // now delete every CorrFunction in the Collection, and then the Collection itself
  StHbtCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    delete *iter;
  }
  delete mCorrFctnCollection;
  // now delete every PicoEvent in the EventMixingBuffer and then the Buffer itself
  StHbtPicoEventIterator piter;
  for (piter=mMixingBuffer->begin();piter!=mMixingBuffer->end();piter++){
    delete *piter;
  }
  delete mMixingBuffer;
}
//______________________
StHbtCorrFctn* StHbtAnalysis::CorrFctn(int n){  // return pointer to n-th correlation function
  if ( n<0 || n > mCorrFctnCollection->size() )
    return NULL;
  StHbtCorrFctnIterator iter=mCorrFctnCollection->begin();
  for (int i=0; i<n ;i++){
    iter++;
  }
  return *iter;
}
//____________________________
StHbtString StHbtAnalysis::Report()
{
  cout << "StHbtAnalysis - constructing Report..."<<endl;
  string temp = "-----------\nHbt Analysis Report:\n";
  temp += "\nEvent Cuts:\n";
  temp += mEventCut->Report();
  temp += "\nParticle Cuts - First Particle:\n";
  temp += mFirstParticleCut->Report();
  temp += "\nParticle Cuts - Second Particle:\n";
  temp += mSecondParticleCut->Report();
  temp += "\nPair Cuts:\n";
  temp += mPairCut->Report();
  temp += "\nCorrelation Functions:\n";
  StHbtCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    temp += (*iter)->Report();
    temp += "\n";
  }
  temp += "-------------\n";
  StHbtString returnThis=temp;
  return returnThis;
}
//_________________________
void StHbtAnalysis::Finish(){
  StHbtCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->Finish();
  }
}
