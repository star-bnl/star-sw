/***************************************************************************
 *
 * $Id: StHbtVertexAnalysis.cxx,v 1.3 2000/08/11 16:35:41 rcwells Exp $
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
 * Revision 1.3  2000/08/11 16:35:41  rcwells
 * Added number of events processed to each HBT analysis
 *
 * Revision 1.2  2000/07/16 22:23:17  laue
 * I forgot that we moved memberfunctions out of StHbtBaseAnalysis.
 * So my previous check-ins didn't compile with the library.
 * Now they do.
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
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVector.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollectionVectorHideAway.hh"


#ifdef __ROOT__ 
ClassImp(StHbtVertexAnalysis)
#endif

extern void FillHbtParticleCollection(StHbtParticleCut*         partCut,
				     StHbtEvent*               hbtEvent,
				     StHbtParticleCollection*  partCollection);


// this little function used to apply ParticleCuts (TrackCuts or V0Cuts) and fill ParticleCollections of picoEvent
//  it is called from StHbtVertexAnalysis::ProcessEvent()
    

// void FillHbtParticleCollection(StHbtParticleCut*         partCut,
// 			       StHbtEvent*               hbtEvent,
// 			       StHbtParticleCollection*  partCollection)
// {
//   switch (partCut->Type()) {
//   case hbtTrack:       // cut is cutting on Tracks
//     {
//       StHbtTrackCut* pCut = (StHbtTrackCut*) partCut;
//       StHbtTrack* pParticle;
//       StHbtTrackIterator pIter;
//       StHbtTrackIterator startLoop = hbtEvent->TrackCollection()->begin();
//       StHbtTrackIterator endLoop   = hbtEvent->TrackCollection()->end();
//       for (pIter=startLoop;pIter!=endLoop;pIter++){
// 	pParticle = *pIter;
// 	bool tmpPassParticle = pCut->Pass(pParticle);
// 	pCut->FillCutMonitor(pParticle, tmpPassParticle);
// 	if (tmpPassParticle){
// 	  StHbtParticle* particle = new StHbtParticle(pParticle,pCut->Mass());
// 	  partCollection->push_back(particle);
// 	}
//       }
//       break;
//     }
//   case hbtV0:          // cut is cutting on V0s
//     {
//       StHbtV0Cut* pCut = (StHbtV0Cut*) partCut;
//       StHbtV0* pParticle;
//       StHbtV0Iterator pIter;
//       StHbtV0Iterator startLoop = hbtEvent->V0Collection()->begin();
//       StHbtV0Iterator endLoop   = hbtEvent->V0Collection()->end();
//       // this following "for" loop is identical to the one above, but because of scoping, I can's see how to avoid repitition...
//       for (pIter=startLoop;pIter!=endLoop;pIter++){
// 	pParticle = *pIter; 
// 	bool tmpPassV0 = pCut->Pass(pParticle);
// 	pCut->FillCutMonitor(pParticle,tmpPassV0);
// 	if (tmpPassV0){
// 	  StHbtParticle* particle = new StHbtParticle(pParticle,partCut->Mass());
// 	  partCollection->push_back(particle);
// 	}
//       }
//       break;
//     }
//   default:
//     cout << "FillHbtParticleCollection function (in StHbtVertexAnalysis.cxx) - undefined Particle Cut type!!! \n";
//   }
// }


//____________________________
StHbtVertexAnalysis::StHbtVertexAnalysis(unsigned int bins, double min, double max){
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mNeventsProcessed = 0;
  mVertexBins = bins;
  mVertexZ[0] = min;
  mVertexZ[1] = max;
  if (mMixingBuffer) delete mMixingBuffer;
  mPicoEventCollectionVectorHideAway = new StHbtPicoEventCollectionVectorHideAway(mVertexBins,mVertexZ[0],mVertexZ[1]);
};
//____________________________

StHbtVertexAnalysis::StHbtVertexAnalysis(const StHbtVertexAnalysis& a) : StHbtBaseAnalysis() {
  //StHbtVertexAnalysis();
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mNeventsProcessed = 0;
  mVertexBins = a.mVertexBins; 
  mVertexZ[0] = a.mVertexZ[0]; 
  mVertexZ[1] = a.mVertexZ[1]; 
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
  delete mPicoEventCollectionVectorHideAway;
}
//______________________
StHbtCorrFctn* StHbtVertexAnalysis::CorrFctn(int n){  // return pointer to n-th correlation function
  if ( n<0 || n > (int)mCorrFctnCollection->size() )
    return NULL;
  StHbtCorrFctnIterator iter=mCorrFctnCollection->begin();
  for (int i=0; i<n ;i++){
    iter++;
  }
  return *iter;
}
//____________________________
StHbtString StHbtVertexAnalysis::Report()
{
  cout << "StHbtVertexAnalysis - constructing Report..."<<endl;
  char Ctemp[100];
  string temp = "-----------\nHbt Analysis Report:\n";
  sprintf(Ctemp,"Events are mixed in %d bins in the range %E cm to %E cm.\n",mVertexBins,mVertexZ[0],mVertexZ[1]);
  temp +=Ctemp;
  temp += mEventCut->Report();
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
  if ( mCorrFctnCollection->size()==0 ) {
    cout << "StHbtVertexAnalysis-Warning : no correlations functions in this analysis " << endl;
  }
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    temp += (*iter)->Report();
    temp += "\n";
  }
  temp += "-------------\n";
  StHbtString returnThis=temp;
  return returnThis;
}
//_________________________
void StHbtVertexAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {
  // Add event to processed events
  AddEventProcessed();
  // startup for EbyE 
  EventBegin(hbtEvent);  
  // get the right mixing buffer
  double vertexZ = hbtEvent->PrimVertPos().z();
  mMixingBuffer = mPicoEventCollectionVectorHideAway->PicoEventCollection(vertexZ);
  if (!mMixingBuffer) return;
  // event cut and event cut monitor
  bool tmpPassEvent = mEventCut->Pass(hbtEvent);
  mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
  if (tmpPassEvent) {
    cout << "StHbtVertexAnalysis::ProcessEvent() - Event: #tracks= " <<
      hbtEvent->TrackCollection()->size();
    // OK, analysis likes the event-- build a pico event from it, using tracks the analysis likes...
    StHbtPicoEvent* picoEvent = new StHbtPicoEvent;       // this is what we will make pairs from and put in Mixing Buffer
    FillHbtParticleCollection(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstParticleCollection());
    if ( !(AnalyzeIdenticalParticles()) )
      FillHbtParticleCollection(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondParticleCollection());
    cout <<" PicoEvent: #particle in first,second collections= " <<
      picoEvent->FirstParticleCollection()->size() << "," <<
      picoEvent->SecondParticleCollection()->size() << endl;
      
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
	    StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	    CorrFctn->AddRealPair(ThePair);
	  }
	}  // if passed pair cut
      }    // loop over second particle
    }      // loop over first particle
      
    // ok, now make mixed pairs, if the Mixing buffer is full
      
    cout << "StHbtVertexAnalysis::ProcessEvent() - reals done, ";
    if (MixingBufferFull()){
      cout << "Mixing Buffer is full - lets rock and roll" << endl;
    }
    else {
      cout << "Mixing Buffer not full -gotta wait " << MixingBuffer()->size() << endl;
    }
    if (MixingBufferFull()){
      StHbtPicoEvent* storedEvent;
      StHbtPicoEventIterator picoEventIter;
      for (picoEventIter=MixingBuffer()->begin();picoEventIter!=MixingBuffer()->end();picoEventIter++){
	storedEvent = *picoEventIter;
	if (AnalyzeIdenticalParticles()){
	  StartOuterLoop = picoEvent->FirstParticleCollection()->begin();
	  EndOuterLoop   = picoEvent->FirstParticleCollection()->end();
	  StartInnerLoop = storedEvent->FirstParticleCollection()->begin();
	  EndInnerLoop = storedEvent->FirstParticleCollection()->end();
	  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
	    ThePair->SetTrack1(*PartIter1);
	    for (PartIter2=StartInnerLoop;PartIter2!=EndInnerLoop;PartIter2++){
	      ThePair->SetTrack2(*PartIter2);
	      // testing...	      cout << "ThePair defined... going to pair cut... ";
	      if (mPairCut->Pass(ThePair)){
		// testing...		cout << " ThePair passed PairCut... ";
		for (CorrFctnIter=mCorrFctnCollection->begin();
		     CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
		  StHbtCorrFctn* CorrFctn = *CorrFctnIter;
		  CorrFctn->AddMixedPair(ThePair);
		  // testing...cout << " ThePair has been added to MixedPair method " << endl;
		} // loop over correlation function
	      } // if passed pair cut
	    } // loop over second particle
	  } // loop over first particle
	} /* if identical particles */ else { // non identical particles
	  // mix current first collection with second collection from the mixing buffer
	  StartOuterLoop = picoEvent->FirstParticleCollection()->begin();
	  EndOuterLoop   = picoEvent->FirstParticleCollection()->end();
 	  StartInnerLoop = storedEvent->SecondParticleCollection()->begin();
	  EndInnerLoop = storedEvent->SecondParticleCollection()->end();
	  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
	    ThePair->SetTrack1(*PartIter1);
	    for (PartIter2=StartInnerLoop;PartIter2!=EndInnerLoop;PartIter2++){
	      ThePair->SetTrack2(*PartIter2);
	      // testing...	      cout << "ThePair defined... going to pair cut... ";
	      if (mPairCut->Pass(ThePair)){
		// testing...		cout << " ThePair passed PairCut... ";
		for (CorrFctnIter=mCorrFctnCollection->begin();
		     CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
		  StHbtCorrFctn* CorrFctn = *CorrFctnIter;
		  CorrFctn->AddMixedPair(ThePair);
		  // testing...cout << " ThePair has been added to MixedPair method " << endl;
		} // loop over correlation function
	      } // if passed pair cut
	    } // loop over second particle
	  } // loop over first particle
	  // mix current second collection with first collection from the mixing buffer
	  StartOuterLoop = picoEvent->SecondParticleCollection()->begin();
	  EndOuterLoop   = picoEvent->SecondParticleCollection()->end();
 	  StartInnerLoop = storedEvent->FirstParticleCollection()->begin();
	  EndInnerLoop = storedEvent->FirstParticleCollection()->end();
	  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
	    ThePair->SetTrack1(*PartIter1);
	    for (PartIter2=StartInnerLoop;PartIter2!=EndInnerLoop;PartIter2++){
	      ThePair->SetTrack2(*PartIter2);
	      // testing...	      cout << "ThePair defined... going to pair cut... ";
	      if (mPairCut->Pass(ThePair)){
		// testing...		cout << " ThePair passed PairCut... ";
		for (CorrFctnIter=mCorrFctnCollection->begin();
		     CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
		  StHbtCorrFctn* CorrFctn = *CorrFctnIter;
		  CorrFctn->AddMixedPair(ThePair);
		  // testing...cout << " ThePair has been added to MixedPair method " << endl;
		} // loop over correlation function
	      } // if passed pair cut
	    } // loop over second particle
	  } // loop over first particle
	} // else non identical particles
      }        // loop over pico-events stored in Mixing buffer
      // Now get rid of oldest stored pico-event in buffer.
      // This means (1) delete the event from memory, (2) "pop" the pointer to it from the MixingBuffer
      picoEventIter = MixingBuffer()->end();
      picoEventIter--;   // bug fixed malisa 27jul99 - end() is one BEYOND the end! (besides crashing on linux, this was a memory leak)
      delete *picoEventIter;
      MixingBuffer()->pop_back();
    }  // if mixing buffer is full
    delete ThePair;
    MixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
  }   // if currentEvent is accepted by currentAnalysis
  EventEnd(hbtEvent);  // cleanup for EbyE 
  //cout << "StHbtVertexAnalysis::ProcessEvent() - return to caller ... " << endl;
}
//_________________________
void StHbtVertexAnalysis::EventBegin(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  mPairCut->EventBegin(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventBegin(ev);
  }
}
//_________________________
void StHbtVertexAnalysis::EventEnd(const StHbtEvent* ev){
  mFirstParticleCut->EventEnd(ev);
  mSecondParticleCut->EventEnd(ev);
  mPairCut->EventEnd(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventEnd(ev);
  }
}
//_________________________
void StHbtVertexAnalysis::Finish(){
  StHbtCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->Finish();
  }
}
//_________________________
void StHbtVertexAnalysis::AddEventProcessed() {
  mNeventsProcessed++;
}
