/***************************************************************************
 *
 * $Id: StHbtManager.cxx,v 1.10 1999/09/17 22:38:02 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The Manager is the top-level object that coordinates activities
 *   and performs event, particle, and pair loops, and checks the
 *   various Cuts of the Analyses in its AnalysisCollection
 *
 ***************************************************************************
 *
 * $Log: StHbtManager.cxx,v $
 * Revision 1.10  1999/09/17 22:38:02  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.9  1999/09/08 04:15:52  lisa
 * persistent microDST implementation tweaked to please fickle solaris details
 *
 * Revision 1.8  1999/09/05 02:58:11  lisa
 * add ASCII microDST reader/writer AND franksParticle cuts
 *
 * Revision 1.7  1999/09/04 04:41:01  lisa
 * StHbtEvent IO   --and--  StHbtEventWriter (microDST) method added to framework
 *
 * Revision 1.6  1999/09/03 22:39:15  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.5  1999/07/27 10:47:04  lisa
 * now works in dev on linux and solaris - mistake in deleting picoEvents fixed
 *
 * Revision 1.4  1999/07/26 16:21:26  lisa
 * always convert string to char when output - needed on solaris
 *
 * Revision 1.3  1999/07/22 18:49:10  lisa
 * Implement idea of Fabrice to not create and delete StHbtPair all the time
 *
 * Revision 1.2  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtManager.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.hh"
#include "StHbtMaker/Base/StHbtV0Cut.hh"
#include <cstdio>

ClassImp(StHbtManager)


// this little function used to apply ParticleCuts (TrackCuts or V0Cuts) and fill ParticleCollections of picoEvent
//  it is called from StHbtManager::ProcessEvent()
void FillHbtParticleCollection(StHbtParticleCut*         partCut,
			       StHbtEvent*               hbtEvent,
			       StHbtParticleCollection*  partCollection)
{
  switch (partCut->Type()) {
  case hbtTrack:       // cut is cutting on Tracks
    {
      StHbtTrackCut* pCut = (StHbtTrackCut*) partCut;
      StHbtTrack* pParticle;
      StHbtTrackIterator pIter;
      StHbtTrackIterator startLoop = hbtEvent->TrackCollection()->begin();
      StHbtTrackIterator endLoop   = hbtEvent->TrackCollection()->end();
      for (pIter=startLoop;pIter!=endLoop;pIter++){
	pParticle = *pIter;    
	if (pCut->Pass(pParticle)){
	  StHbtParticle* particle = new StHbtParticle(pParticle,pCut->Mass());
	  partCollection->push_back(particle);
	}
      }
      break;
    }
  case hbtV0:          // cut is cutting on V0s
    {
      StHbtV0Cut* pCut = (StHbtV0Cut*) partCut;
      StHbtV0* pParticle;
      StHbtV0Iterator pIter;
      StHbtV0Iterator startLoop = hbtEvent->V0Collection()->begin();
      StHbtV0Iterator endLoop   = hbtEvent->V0Collection()->end();
      // this following "for" loop is identical to the one above, but because of scoping, I can's see how to avoid repitition...
      for (pIter=startLoop;pIter!=endLoop;pIter++){
	pParticle = *pIter;    
	if (pCut->Pass(pParticle)){
	  StHbtParticle* particle = new StHbtParticle(pParticle,partCut->Mass());
	  partCollection->push_back(particle);
	}
      }
      break;
    }
  default:
    cout << "FillHbtParticleCollection function (in StHbtManager.cxx) - undefined Particle Cut type!!! \n";
  }
}



//____________________________
StHbtManager::StHbtManager(){
  mAnalysisCollection = new StHbtAnalysisCollection;
  mEventReader = 0;
  mEventWriter = 0;
}
//____________________________
StHbtManager::~StHbtManager(){
  delete mEventReader;
  // now delete each Analysis in the Collection, and then the Collection itself
  StHbtAnalysisIterator AnalysisIter;
  for (AnalysisIter=mAnalysisCollection->begin();AnalysisIter!=mAnalysisCollection->end();AnalysisIter++){
    delete *AnalysisIter;
    *AnalysisIter = 0;
  }
  delete mAnalysisCollection;
}
//____________________________
int StHbtManager::Init(){
  StHbtString temp = " ";
  if (mEventReader) {
    if (mEventReader->Init("r")){
      cout << " StHbtManager::Init() - Reader initialization failed " << endl;
      return (1);
    }
    temp = mEventReader->Report();
  }
  if (mEventWriter) {
    if (mEventWriter->Init("w",temp)){
      cout << " StHbtManager::Init() - Writer initialization failed " << endl;
      return (1);
    }
  }
  return (0);
}
//____________________________
void StHbtManager::Finish(){
  StHbtAnalysisIterator AnalysisIter;
  StHbtAnalysis* currentAnalysis;
  for (AnalysisIter=mAnalysisCollection->begin();AnalysisIter!=mAnalysisCollection->end();AnalysisIter++){
    currentAnalysis = *AnalysisIter;
    currentAnalysis->Finish();
  }
  if (mEventReader) mEventReader->Finish();
  if (mEventWriter) mEventWriter->Finish();
}
//____________________________
StHbtString StHbtManager::Report(){
  string stemp;
  //  cout << "StHbtManager Starting report " << endl;
  //cout << mAnalysisCollection->size() << endl;
  char ctemp[100];
  stemp = mEventReader->Report();
  sprintf(ctemp,"\nStHbtManager Reporting %u Analyses\n",mAnalysisCollection->size());
  stemp += ctemp;
  StHbtAnalysisIterator AnalysisIter;
  StHbtAnalysis* currentAnalysis;
  //cout << stemp.c_str();
  for (AnalysisIter=mAnalysisCollection->begin();AnalysisIter!=mAnalysisCollection->end();AnalysisIter++){
    //cout << "StHbtManager - asking for Analysis Report" << endl;
    currentAnalysis = *AnalysisIter;
    stemp+=currentAnalysis->Report();
  }
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
int StHbtManager::ProcessEvent(){

  cout << "StHbtManager::ProcessEvent" << endl;
  // NOTE - this ReturnHbtEvent makes a *new* StHbtEvent - delete it when done!
  StHbtEvent* currentHbtEvent = mEventReader->ReturnHbtEvent();

  cout << "Event reader has returned control to manager" << endl;

  // if no HbtEvent is returned, then we abort processing.
  // the question is now: do we try again next time (i.e. there may be an HbtEvent next time)
  // or are we at EOF or something?  If Reader says Status=0, then that means try again later.
  // so, we just return the Reader's Status.
  if (!currentHbtEvent){
    cout << "StHbtManager::ProcessEvent() - Reader::ReturnHbtEvent() has returned null pointer\n";
    return mEventReader->Status();
  }

  // shall we write a microDST? - added 3sep99
  if (mEventWriter) mEventWriter->WriteHbtEvent(currentHbtEvent);

  StHbtAnalysisIterator AnalysisIter;
  StHbtAnalysis* currentAnalysis;
  for (AnalysisIter=mAnalysisCollection->begin();AnalysisIter!=mAnalysisCollection->end();AnalysisIter++){
    currentAnalysis = *AnalysisIter;
    if (currentAnalysis->EventCut()->Pass(currentHbtEvent)){
      cout << "StHbtManager::ProcessEvent() - Event has passed cut - build picoEvent from " <<
	currentHbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
      // OK, analysis likes the event-- build a pico event from it, using tracks the analysis likes...
      StHbtPicoEvent* picoEvent = new StHbtPicoEvent;       // this is what we will make pairs from and put in Mixing Buffer
      FillHbtParticleCollection(currentAnalysis->FirstParticleCut(),currentHbtEvent,picoEvent->FirstParticleCollection());
      if ( !(currentAnalysis->AnalyzeIdenticalParticles()) )
	FillHbtParticleCollection(currentAnalysis->SecondParticleCut(),currentHbtEvent,picoEvent->SecondParticleCollection());
      cout <<"StHbtManager::ProcessEvent - #particles in First, Second Collections: " <<
	picoEvent->FirstParticleCollection()->size() << " " <<
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
      if (currentAnalysis->AnalyzeIdenticalParticles()) {             // only use First collection
	EndOuterLoop--;                                               // outer loop goes to next-to-last particle in First collection
	EndInnerLoop = picoEvent->FirstParticleCollection()->end() ;  // inner loop goes to last particle in First collection
      }
      else {                                                          // nonidentical - loop over First and Second collections
	StartInnerLoop = picoEvent->SecondParticleCollection()->begin(); // inner loop starts at first particle in Second collection
	EndInnerLoop   = picoEvent->SecondParticleCollection()->end() ;  // inner loop goes to last particle in Second collection
      }
      for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
	if (currentAnalysis->AnalyzeIdenticalParticles()){
	  StartInnerLoop = PartIter1;
	  StartInnerLoop++;
	}
	ThePair->SetTrack1(*PartIter1);
	for (PartIter2 = StartInnerLoop; PartIter2!=EndInnerLoop;PartIter2++){
	  ThePair->SetTrack2(*PartIter2);
	  if (currentAnalysis->PairCut()->Pass(ThePair)){
	    for (CorrFctnIter=currentAnalysis->CorrFctnCollection()->begin();
		 CorrFctnIter!=currentAnalysis->CorrFctnCollection()->end();CorrFctnIter++){
	      StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	      CorrFctn->AddRealPair(ThePair);
	    }
	  }  // if passed pair cut
	}    // loop over second particle
      }      // loop over first particle
      
      // ok, now make mixed pairs, if the Mixing buffer is full

      cout << "StHbtManager::ProcessEvent() - reals done" << endl;
      if (currentAnalysis->MixingBufferFull()){
	cout << "Mixing Buffer is full - lets rock and roll" << endl;
      }
      else {
	cout << "Mixing Buffer not full -gotta wait " << currentAnalysis->MixingBuffer()->size() << endl;
      }
      if (currentAnalysis->MixingBufferFull()){
	StartOuterLoop = picoEvent->FirstParticleCollection()->begin();
	EndOuterLoop   = picoEvent->FirstParticleCollection()->end();
	StHbtPicoEvent* storedEvent;
	StHbtPicoEventIterator picoEventIter;
	for (picoEventIter=currentAnalysis->MixingBuffer()->begin();picoEventIter!=currentAnalysis->MixingBuffer()->end();picoEventIter++){
	  storedEvent = *picoEventIter;
	  if (currentAnalysis->AnalyzeIdenticalParticles()){
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
	      if (currentAnalysis->PairCut()->Pass(ThePair)){
		// testing...		cout << " ThePair passed PairCut... ";
		for (CorrFctnIter=currentAnalysis->CorrFctnCollection()->begin();
		     CorrFctnIter!=currentAnalysis->CorrFctnCollection()->end();CorrFctnIter++){
		  StHbtCorrFctn* CorrFctn = *CorrFctnIter;
		  CorrFctn->AddMixedPair(ThePair);
		  // testing...cout << " ThePair has been added to MixedPair method " << endl;
		}
	      }  // if passed pair cut
	    }    // loop over second particle
	  }      // loop over first particle
	}        // loop over pico-events stored in Mixing buffer
	// Now get rid of oldest stored pico-event in buffer.
	// This means (1) delete the event from memory, (2) "pop" the pointer to it from the MixingBuffer
	picoEventIter = currentAnalysis->MixingBuffer()->end();
	picoEventIter--;   // bug fixed malisa 27jul99 - end() is one BEYOND the end! (besides crashing on linux, this was a memory leak)
	delete *picoEventIter;
	currentAnalysis->MixingBuffer()->pop_back();
      }  // if mixing buffer is full
      delete ThePair;
      currentAnalysis->MixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
    }   // if currentEvent is accepted by currentAnalysis
  }     // loop over Analyses
  delete currentHbtEvent;
  return 0;    // 0 = "good return"
}       // ProcessEvent
