/***************************************************************************
 *
 * $Id: StHbtLikeSignAnalysis.cxx,v 1.4 2003/09/02 17:58:32 perev Exp $
 *
 * Author: Frank Laue, Ohio State, Laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtLikeSignAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVector.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVectorHideAway.hh"
#include "Stiostream.h"

#ifdef __ROOT__ 
ClassImp(StHbtLikeSignAnalysis)
#endif

// this little function used to apply ParticleCuts (TrackCuts or V0Cuts) and fill ParticleCollections of picoEvent
//  it is called from StHbtAnalysis::ProcessEvent()


extern void FillHbtParticleCollection(StHbtParticleCut*         partCut,
				     StHbtEvent*               hbtEvent,
				     StHbtParticleCollection*  partCollection);

 
//____________________________
StHbtLikeSignAnalysis::StHbtLikeSignAnalysis(unsigned int bins, double min, double max) : StHbtAnalysis() {
  mVertexBins = bins;
  mVertexZ[0] = min;
  mVertexZ[1] = max;
  mUnderFlow = 0; 
  mOverFlow = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mVertexBins,mVertexZ[0],mVertexZ[1]);
    /* no-op */
}
//____________________________
StHbtLikeSignAnalysis::StHbtLikeSignAnalysis(const StHbtLikeSignAnalysis& a) : StHbtAnalysis(a) {
  mVertexBins = a.mVertexBins; 
  mVertexZ[0] = a.mVertexZ[0]; 
  mVertexZ[1] = a.mVertexZ[1];
  mUnderFlow = 0; 
  mOverFlow = 0; 
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mVertexBins,mVertexZ[0],mVertexZ[1]);
 }
//____________________________ 
StHbtLikeSignAnalysis::~StHbtLikeSignAnalysis(){
  delete mPicoEventCollectionVectorHideAway; mPicoEventCollectionVectorHideAway=0;
}
//____________________________
StHbtString StHbtLikeSignAnalysis::Report()
{  
  char Ctemp[200];
  cout << "StHbtLikeSignAnalysis - constructing Report..."<<endl;
  StHbtString temp = "-----------\nHbt Analysis Report:\n";
  sprintf(Ctemp,"Events are mixed in %d bins in the range %E cm to %E cm.\n",mVertexBins,mVertexZ[0],mVertexZ[1]);
  temp += Ctemp;
  sprintf(Ctemp,"Events underflowing: %d\n",mUnderFlow);
  temp += Ctemp;
  sprintf(Ctemp,"Events overflowing: %d\n",mOverFlow);
  temp += Ctemp;
  sprintf(Ctemp,"Now adding StHbtAnalysis(base) Report\n");
  temp += Ctemp; 
  temp += "Adding StHbtAnalysis(base) Report now:\n";
  temp += StHbtAnalysis::Report();
  temp += "-------------\n";
  StHbtString returnThis=temp;
  return returnThis;
}
//_________________________
void StHbtLikeSignAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {
  // get right mixing buffer
  double vertexZ = hbtEvent->PrimVertPos().z();
  mMixingBuffer = mPicoEventCollectionVectorHideAway->PicoEventCollection(vertexZ); 
  if (!mMixingBuffer) {
    if ( vertexZ < mVertexZ[0] ) mUnderFlow++;
    if ( vertexZ > mVertexZ[1] ) mOverFlow++;
    return;
  }

  // startup for EbyE 
  EventBegin(hbtEvent);  
  // event cut and event cut monitor
  bool tmpPassEvent = mEventCut->Pass(hbtEvent);
  mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
  if (tmpPassEvent) {
      mNeventsProcessed++;
      cout << "StHbtLikeSignAnalysis::ProcessEvent() - " << hbtEvent->TrackCollection()->size();
      cout << " #track=" << hbtEvent->TrackCollection()->size();
      // OK, analysis likes the event-- build a pico event from it, using tracks the analysis likes...
      StHbtPicoEvent* picoEvent = new StHbtPicoEvent;       // this is what we will make pairs from and put in Mixing Buffer
      FillHbtParticleCollection(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstParticleCollection());
      if ( !(AnalyzeIdenticalParticles()) )
	FillHbtParticleCollection(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondParticleCollection());
      cout <<"   #particles in First, Second Collections: " <<
	picoEvent->FirstParticleCollection()->size() << " " <<
	picoEvent->SecondParticleCollection()->size() << endl;
      
      if (picoEvent->SecondParticleCollection()->size()*picoEvent->FirstParticleCollection()->size()==0) {
	delete picoEvent;
	cout << "StHbtLikeSignAnalysis - picoEvent deleted due to empty collection " <<endl; 
	return;
      }
      // OK, pico event is built
      // make real pairs...
      
      // Fabrice points out that we do not need to keep creating/deleting pairs all the time
      // We only ever need ONE pair, and we can just keep changing internal pointers
      // this should help speed things up
      StHbtPair* ThePair = new StHbtPair;
      
      StHbtParticleIterator PartIter1;
      StHbtParticleIterator PartIter2;
      StHbtCorrFctnIterator CorrFctnIter;
      StHbtParticleIterator StartOuterLoop = picoEvent->FirstParticleCollection()->begin();  // always
      StHbtParticleIterator EndOuterLoop   = picoEvent->FirstParticleCollection()->end();    // will be one less if identical
      StHbtParticleIterator StartInnerLoop;
      StHbtParticleIterator EndInnerLoop;
      if (AnalyzeIdenticalParticles()) {             // only use First collection
	EndOuterLoop--;                                               // outer loop goes to next-to-last particle in First collection
	EndInnerLoop = picoEvent->FirstParticleCollection()->end() ;  // inner loop goes to last particle in First collection
      }
      else {                                                          // nonidentical - loop over First and Second collections
	StartInnerLoop = picoEvent->SecondParticleCollection()->begin(); // inner loop starts at first particle in Second collection
	EndInnerLoop   = picoEvent->SecondParticleCollection()->end() ;  // inner loop goes to last particle in Second collection
      }
      // real pairs
      for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
	if (AnalyzeIdenticalParticles()){
	  StartInnerLoop = PartIter1;
	  StartInnerLoop++;
	}
	ThePair->SetTrack1(*PartIter1);
	for (PartIter2 = StartInnerLoop; PartIter2!=EndInnerLoop;PartIter2++){
	  ThePair->SetTrack2(*PartIter2);
	  // The following lines have to be uncommented if you want pairCutMonitors
	  // they are not in for speed reasons
	  // bool tmpPassPair = mPairCut->Pass(ThePair);
          // mPairCut->FillCutMonitor(ThePair, tmpPassPair);
	  // if ( tmpPassPair ) {
	  if (mPairCut->Pass(ThePair)){
	    for (CorrFctnIter=mCorrFctnCollection->begin();
		 CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	      StHbtLikeSignCorrFctn* CorrFctn = dynamic_cast<StHbtLikeSignCorrFctn*>(*CorrFctnIter);
	      if (CorrFctn) CorrFctn->AddRealPair(ThePair);
	    }
	  }  // if passed pair cut
	}    // loop over second particle
      }      // loop over first particle
#ifdef STHBTDEBUG
      cout << "StHbtLikeSignAnalysis::ProcessEvent() - reals done" << endl;
#endif

      StHbtParticleIterator nextIter;
      StHbtParticleIterator prevIter;

      // like sign first partilce collection pairs
      prevIter = EndOuterLoop;
      prevIter--;
      for (PartIter1=StartOuterLoop;PartIter1!=prevIter;PartIter1++){
	ThePair->SetTrack1(*PartIter1);
	nextIter = PartIter1;
	nextIter++;
	for (PartIter2 = nextIter; PartIter2!=EndOuterLoop;PartIter2++){
	  ThePair->SetTrack2(*PartIter2);
	  // The following lines have to be uncommented if you want pairCutMonitors
	  // they are not in for speed reasons
	  // bool tmpPassPair = mPairCut->Pass(ThePair);
          // mPairCut->FillCutMonitor(ThePair, tmpPassPair);
	  // if ( tmpPassPair ) {
	  if (mPairCut->Pass(ThePair)){
	    for (CorrFctnIter=mCorrFctnCollection->begin();
		 CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	      StHbtLikeSignCorrFctn* CorrFctn = dynamic_cast<StHbtLikeSignCorrFctn*>(*CorrFctnIter);
	      if (CorrFctn) CorrFctn->AddLikeSignPositivePair(ThePair);
	    }
	  }  // if passed pair cut
	}    // loop over second particle
      }      // loop over first particle
#ifdef STHBTDEBUG
      cout << "StHbtLikeSignAnalysis::ProcessEvent() - like sign first collection done" << endl;
#endif
      // like sign second partilce collection pairs
      prevIter = EndInnerLoop;
      prevIter--;
      for (PartIter1=StartInnerLoop;PartIter1!=prevIter;PartIter1++){
	ThePair->SetTrack1(*PartIter1);
	nextIter = PartIter1;
	nextIter++;
	for (PartIter2 = nextIter; PartIter2!=EndInnerLoop;PartIter2++){
	  ThePair->SetTrack2(*PartIter2);
	  // The following lines have to be uncommented if you want pairCutMonitors
	  // they are not in for speed reasons
	  // bool tmpPassPair = mPairCut->Pass(ThePair);
          // mPairCut->FillCutMonitor(ThePair, tmpPassPair);
	  // if ( tmpPassPair ) {
	  if (mPairCut->Pass(ThePair)){
	    for (CorrFctnIter=mCorrFctnCollection->begin();
		 CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	      StHbtLikeSignCorrFctn* CorrFctn = dynamic_cast<StHbtLikeSignCorrFctn*>(*CorrFctnIter);
	      if (CorrFctn) CorrFctn->AddLikeSignNegativePair(ThePair);
	    }
	  }  // if passed pair cut
	}    // loop over second particle
      }      // loop over first particle
#ifdef STHBTDEBUG
      cout << "StHbtLikeSignAnalysis::ProcessEvent() - like sign second collection done" << endl;
#endif
      
      if (MixingBufferFull()){
#ifdef STHBTDEBUG
	cout << "Mixing Buffer is full - lets rock and roll" << endl;
#endif
      }
      else {
	cout << "Mixing Buffer not full -gotta wait " << MixingBuffer()->size() << endl;
      }
      if (MixingBufferFull()){
	StartOuterLoop = picoEvent->FirstParticleCollection()->begin();
	EndOuterLoop   = picoEvent->FirstParticleCollection()->end();
	StHbtPicoEvent* storedEvent;
	StHbtPicoEventIterator picoEventIter;
	for (picoEventIter=MixingBuffer()->begin();picoEventIter!=MixingBuffer()->end();picoEventIter++){
	  storedEvent = *picoEventIter;
	  if (AnalyzeIdenticalParticles()){
	    StartInnerLoop = storedEvent->FirstParticleCollection()->begin();
	    EndInnerLoop = storedEvent->FirstParticleCollection()->end();
	  }
	  else{
	    StartInnerLoop = storedEvent->SecondParticleCollection()->begin();
	    EndInnerLoop = storedEvent->SecondParticleCollection()->end();
	  }
	  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
	    ThePair->SetTrack1(*PartIter1);
	    for (PartIter2=StartInnerLoop;PartIter2!=EndInnerLoop;PartIter2++){
	      ThePair->SetTrack2(*PartIter2);
	      // testing...	      cout << "ThePair defined... going to pair cut... ";
	      if (mPairCut->Pass(ThePair)){
		// testing...		cout << " ThePair passed PairCut... ";
		for (CorrFctnIter=mCorrFctnCollection->begin();
		     CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
		  StHbtLikeSignCorrFctn* CorrFctn = dynamic_cast<StHbtLikeSignCorrFctn*>(*CorrFctnIter);
		  if (CorrFctn) { 
		    CorrFctn->AddMixedPair(ThePair);
		    //cout << " ThePair has been added to MixedPair method " << endl;
		  }
		}
	      }  // if passed pair cut
	    }    // loop over second particle
	  }      // loop over first particle
	}        // loop over pico-events stored in Mixing buffer
	// Now get rid of oldest stored pico-event in buffer.
	// This means (1) delete the event from memory, (2) "pop" the pointer to it from the MixingBuffer
	delete MixingBuffer()->back();
	MixingBuffer()->pop_back();
      }  // if mixing buffer is full
      delete ThePair;
      MixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
    }   // if currentEvent is accepted by currentAnalysis
    EventEnd(hbtEvent);  // cleanup for EbyE 
    //    cout << "StHbtLikeSignAnalysis::ProcessEvent() - return to caller ... " << endl;
}



