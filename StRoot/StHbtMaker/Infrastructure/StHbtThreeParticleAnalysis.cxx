/***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the derived Class for three particle Analysis objects.  
 *      Each of the simultaneous Analyses should have one of derived 
 *      Analysis classes runningthese instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtThreeParticleAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"

#ifdef __ROOT__ 
ClassImp(StHbtThreeParticleAnalysis)
#endif

//  this little function used to apply ParticleCuts (TrackCuts or V0Cuts) and fill 
//  ParticleCollections of picoEvent.  It is declared below, and defined in StHbtAnalysis.cxx
//  It is called from StHbtThreeParticleAnalysis::ProcessEvent()
void FillHbtParticleCollection(StHbtParticleCut*         partCut,
					    StHbtEvent*               hbtEvent,
					    StHbtParticleCollection*  partCollection);


//____________________________
StHbtThreeParticleAnalysis::StHbtThreeParticleAnalysis(){
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mThirdParticleCut  = 0;
  mTripletCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtThreeParticleCorrFctnCollection;
  mMixingBuffer = new StHbtPicoEventCollection;
}

//____________________________
StHbtThreeParticleAnalysis::~StHbtThreeParticleAnalysis(){
  //delete mControlSwitch     ;
  delete mEventCut          ;
  delete mFirstParticleCut  ;
  delete mSecondParticleCut ;
  delete mThirdParticleCut ;
  delete mTripletCut           ;
  // now delete every CorrFunction in the Collection, and then the Collection itself
  StHbtThreeParticleCorrFctnIterator iter;
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
StHbtThreeParticleCorrFctn* StHbtThreeParticleAnalysis::CorrFctn(int n){  // return pointer to n-th correlation function
  if ( n<0 || n > (int)mCorrFctnCollection->size() )
    return NULL;
  StHbtThreeParticleCorrFctnIterator iter=mCorrFctnCollection->begin();
  for (int i=0; i<n ;i++){
    iter++;
  }
  return *iter;
}
//____________________________
StHbtString StHbtThreeParticleAnalysis::Report()
{
  cout << "StHbtThreeParticleAnalysis - constructing Report..."<<endl;
  string temp = "-----------\nHbt Analysis Report:\n";
  temp += "\nEvent Cuts:\n";
  temp += mEventCut->Report();
  temp += "\nParticle Cuts - First Particle:\n";
  temp += mFirstParticleCut->Report();
  temp += "\nParticle Cuts - Second Particle:\n";
  temp += mSecondParticleCut->Report();
  temp += "\nParticle Cuts - Third Particle:\n";
  temp += mThirdParticleCut->Report();
  temp += "\nTriplet Cuts:\n";
  temp += mTripletCut->Report();
  temp += "\nCorrelation Functions:\n";
  StHbtThreeParticleCorrFctnIterator iter;
  if ( mCorrFctnCollection->size()==0 ) {
    cout << "StHbtThreeParticleAnalysis-Warning : no correlations functions in this analysis " << endl;
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
void StHbtThreeParticleAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {
  // startup for EbyE 
  EventBegin(hbtEvent);  
  // event cut and event cut monitor
  bool tmpPassEvent = mEventCut->Pass(hbtEvent);
  mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
  if (tmpPassEvent) {
    cout << "StHbtThreeParticleAnalysis::ProcessEvent() - Event has passed cut - build picoEvent from " <<
      hbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
    // OK, analysis likes the event-- build a pico event from it, using tracks the analysis likes...
    StHbtPicoEvent* picoEvent = new StHbtPicoEvent;       // this is what we will make Triplets from and put in Mixing Buffer
    FillHbtParticleCollection(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstParticleCollection());
    if ( !(AnalyzeIdenticalParticles()) ) {
      FillHbtParticleCollection(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondParticleCollection());
      FillHbtParticleCollection(mThirdParticleCut,(StHbtEvent*)hbtEvent,picoEvent->ThirdParticleCollection());}
    cout <<"StHbtThreeParticleAnalysis::ProcessEvent - #particles in First, Second, Third Collections: " <<
      picoEvent->FirstParticleCollection()->size() << " " <<
      picoEvent->SecondParticleCollection()->size() << " " <<
      picoEvent->ThirdParticleCollection()->size() << endl;
    
    // OK, pico event is built
    // make real Triplets...
    
    // Fabrice points out that we do not need to keep creating/deleting Triplets all the time
    // We only ever need ONE Triplet, and we can just keep changing internal pointers
    // this should help speed things up
    StHbtTriplet* TheTriplet = new StHbtTriplet;
    
    StHbtParticleIterator PartIter1;
    StHbtParticleIterator PartIter2;
    StHbtParticleIterator PartIter3;
    StHbtThreeParticleCorrFctnIterator CorrFctnIter;
    StHbtParticleIterator StartOuterLoop = picoEvent->FirstParticleCollection()->begin();  // always
    StHbtParticleIterator EndOuterLoop   = picoEvent->FirstParticleCollection()->end();    // will be two less if identical
    StHbtParticleIterator StartMiddleLoop;
    StHbtParticleIterator EndMiddleLoop;
    StHbtParticleIterator StartInnerLoop;
    StHbtParticleIterator EndInnerLoop;
    if (AnalyzeIdenticalParticles()) {             // only use First collection
      EndOuterLoop--;
      EndOuterLoop--;                                               // outer loop goes to next-to-next-to-last particle in First collection
      EndMiddleLoop = picoEvent->FirstParticleCollection()->end() ;  // middle loop goes to next-to-last particle in First collection
      EndMiddleLoop--;
      EndInnerLoop = picoEvent->FirstParticleCollection()->end() ;  // inner loop goes to last particle in First collection
    }
    else {                                                          // nonidentical - loop over First, Second and Third collections
      StartMiddleLoop = picoEvent->SecondParticleCollection()->begin(); // middle loop starts at first particle in Second collection
      EndMiddleLoop   = picoEvent->SecondParticleCollection()->end() ;  // middle loop goes to last particle in Second collection
      StartInnerLoop = picoEvent->ThirdParticleCollection()->begin(); // inner loop starts at first particle in Third collection
      EndInnerLoop   = picoEvent->ThirdParticleCollection()->end() ;  // inner loop goes to last particle in Third collection
    }
    for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
      if (AnalyzeIdenticalParticles()){
	StartMiddleLoop = PartIter1;
	StartMiddleLoop++;
	StartInnerLoop = PartIter1;
	StartInnerLoop++;
	StartInnerLoop++;
      }
      TheTriplet->SetTrack1(*PartIter1);
      for (PartIter2 = StartMiddleLoop; PartIter2!=EndMiddleLoop;PartIter2++){
	TheTriplet->SetTrack2(*PartIter2);
	for (PartIter3 = StartInnerLoop; PartIter3!=EndInnerLoop;PartIter3++){
	  TheTriplet->SetTrack3(*PartIter3);
	  // The following lines have to be uncommented if you want TripletCutMonitors
	  // they are not in for speed reasons
	  // bool tmpPassTriplet = mTripletCut->Pass(TheTriplet);
	  // mTripletCut->FillCutMonitor(TheTriplet, tmpPassTriplet);
	  // if ( tmpPassTriplet ) {
	  if (mTripletCut->Pass(TheTriplet)){
	    for (CorrFctnIter=mCorrFctnCollection->begin();
		 CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	      StHbtThreeParticleCorrFctn* CorrFctn = *CorrFctnIter;
	      CorrFctn->AddRealTriplet(TheTriplet);
	    }
	  }  // if passed Triplet cut
	}  // loop over third particle
      }    // loop over second particle
    }      // loop over first particle
    
    // ok, now make mixed Triplets, if the Mixing buffer is full
    
    cout << "StHbtThreeParticleAnalysis::ProcessEvent() - reals done" << endl;
    if (MixingBufferFull()){
      cout << "Mixing Buffer is full - lets rock and roll" << endl;
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
	  StartMiddleLoop = storedEvent->FirstParticleCollection()->begin();
	  EndMiddleLoop = storedEvent->FirstParticleCollection()->end();
	  StartInnerLoop = storedEvent->FirstParticleCollection()->begin();
	  EndInnerLoop = storedEvent->FirstParticleCollection()->end();
	}
	else{
	  StartMiddleLoop = storedEvent->SecondParticleCollection()->begin();
	  EndMiddleLoop = storedEvent->SecondParticleCollection()->end();
	  StartInnerLoop = storedEvent->ThirdParticleCollection()->begin();
	  EndInnerLoop = storedEvent->ThirdParticleCollection()->end();
	}
	for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
	  TheTriplet->SetTrack1(*PartIter1);
	  for (PartIter2=StartMiddleLoop;PartIter2!=EndMiddleLoop;PartIter2++){
	    TheTriplet->SetTrack2(*PartIter2);
	    for (PartIter3=StartInnerLoop;PartIter3!=EndInnerLoop;PartIter3++){
	      TheTriplet->SetTrack3(*PartIter3);
	      // testing...	      cout << "TheTriplet defined... going to Triplet cut... ";
	      if (mTripletCut->Pass(TheTriplet)){
		// testing...		cout << " TheTriplet passed TripletCut... ";
		for (CorrFctnIter=mCorrFctnCollection->begin();
		     CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
		  StHbtThreeParticleCorrFctn* CorrFctn = *CorrFctnIter;
		  CorrFctn->AddMixedTriplet(TheTriplet);
		  // testing...cout << " TheTriplet has been added to MixedTriplet method " << endl;
		}
	      }  // if passed Triplet cut
	    }  // loop over third particle
	  }    // loop over second particle
	}      // loop over first particle
      }        // loop over pico-events stored in Mixing buffer
      // Now get rid of oldest stored pico-event in buffer.
      // This means (1) delete the event from memory, (2) "pop" the pointer to it from the MixingBuffer
      picoEventIter = MixingBuffer()->end();
      picoEventIter--;   // bug fixed malisa 27jul99 - end() is one BEYOND the end! (besides crashing on linux, this was a memory leak)
      delete *picoEventIter;
      MixingBuffer()->pop_back();
    }  // if mixing buffer is full
    delete TheTriplet;
    MixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
  }    // if currentEvent is accepted by currentAnalysis
  EventEnd(hbtEvent);  // cleanup for EbyE 
  cout << "StHbtThreeParticleAnalysis::ProcessEvent() - return to caller ... " << endl;
}

//_________________________
void StHbtThreeParticleAnalysis::EventBegin(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  mThirdParticleCut->EventBegin(ev);
  for (StHbtThreeParticleCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventBegin(ev);
  }
}
//_________________________
void StHbtThreeParticleAnalysis::EventEnd(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  mThirdParticleCut->EventBegin(ev);
  for (StHbtThreeParticleCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventEnd(ev);
  }
}
//_________________________
void StHbtThreeParticleAnalysis::Finish(){
  StHbtThreeParticleCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->Finish();
  }
}


