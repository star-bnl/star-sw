/***************************************************************************
 *
 * $Id: StHbtManager.cxx,v 1.3 1999/07/22 18:49:10 lisa Exp $
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
#include <cstdio>

ClassImp(StHbtManager)

//____________________________
StHbtManager::StHbtManager(){
  mAnalysisCollection = new StHbtAnalysisCollection;
  mEventReader = 0;
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
void StHbtManager::Init(){
  /* noop */
}
//____________________________
void StHbtManager::Finish(){
  StHbtAnalysisIterator AnalysisIter;
  StHbtAnalysis* currentAnalysis;
  for (AnalysisIter=mAnalysisCollection->begin();AnalysisIter!=mAnalysisCollection->end();AnalysisIter++){
    currentAnalysis = *AnalysisIter;
    currentAnalysis->Finish();
  }
}
//____________________________
StHbtString StHbtManager::Report(){
  string stemp;
  cout << "StHbtManager Starting report " << endl;
  cout << mAnalysisCollection->size() << endl;
  char ctemp[100];
  sprintf(ctemp,"\nStHbtManager Reporting %u Analyses\n",mAnalysisCollection->size());
  stemp = ctemp;
  StHbtAnalysisIterator AnalysisIter;
  StHbtAnalysis* currentAnalysis;
  cout << stemp;
  for (AnalysisIter=mAnalysisCollection->begin();AnalysisIter!=mAnalysisCollection->end();AnalysisIter++){
    cout << "StHbtManager - asking for Analysis Report" << endl;
    currentAnalysis = *AnalysisIter;
    stemp+=currentAnalysis->Report();
  }
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void StHbtManager::ProcessEvent(){

  cout << "StHbtManager::ProcessEvent" << endl;
  // NOTE - this ReturnHbtEvent makes a *new* StHbtEvent - delete it when done!
  StHbtEvent* currentHbtEvent = mEventReader->ReturnHbtEvent();

  cout << "Event reader has returned control to manager" << endl;

  StHbtAnalysisIterator AnalysisIter;
  StHbtAnalysis* currentAnalysis;
  for (AnalysisIter=mAnalysisCollection->begin();AnalysisIter!=mAnalysisCollection->end();AnalysisIter++){
    currentAnalysis = *AnalysisIter;
    if (currentAnalysis->EventCut()->Pass(currentHbtEvent)){
      cout << "StHbtManager::ProcessEvent() - Event has passed cut - build picoEvent from " <<
	currentHbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
      // OK, analysis likes the event-- build a pico event from it, using tracks the analysis likes...
      StHbtPicoEvent* picoEvent = new StHbtPicoEvent;       // this is what we will make pairs from and put in Mixing Buffer
      StHbtTrackIterator TrkIter;
      StHbtTrack* currentTrk;
      if (currentAnalysis->AnalyzeIdenticalParticles()){
	for (TrkIter=currentHbtEvent->TrackCollection()->begin();TrkIter!=currentHbtEvent->TrackCollection()->end();TrkIter++){
	  currentTrk = *TrkIter;
	  if (currentAnalysis->FirstParticleCut()->Pass(currentTrk)){
	    StHbtParticle* particle = new StHbtParticle(currentTrk,currentAnalysis->FirstParticleCut()->Mass());
	    picoEvent->FirstParticleCollection()->push_back(particle);
	  }
	}
      }
      else{      // non-identical particles...
	for (TrkIter=currentHbtEvent->TrackCollection()->begin();TrkIter!=currentHbtEvent->TrackCollection()->end();TrkIter++){
	  currentTrk = *TrkIter;
	  if (currentAnalysis->FirstParticleCut()->Pass(currentTrk)){
	    StHbtParticle* particle = new StHbtParticle(currentTrk,currentAnalysis->FirstParticleCut()->Mass());
	    picoEvent->FirstParticleCollection()->push_back(particle);
	  }
	  if (currentAnalysis->SecondParticleCut()->Pass(currentTrk)){
	    StHbtParticle* particle = new StHbtParticle(currentTrk,currentAnalysis->SecondParticleCut()->Mass());
	    picoEvent->SecondParticleCollection()->push_back(particle);
	  }
	}
      }
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
	  //	  OBSOLETE StHbtPair* pair = new StHbtPair(*PartIter1,*PartIter2);
	  if (currentAnalysis->PairCut()->Pass(ThePair)){
	    for (CorrFctnIter=currentAnalysis->CorrFctnCollection()->begin();
		 CorrFctnIter!=currentAnalysis->CorrFctnCollection()->end();CorrFctnIter++){
	      StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	      CorrFctn->AddRealPair(ThePair);
	    }
	  }  // if passed pair cut
	  // OBSOLETE	  delete pair;
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
	      // OBSOLETE	      StHbtPair* pair = new StHbtPair(*PartIter1,*PartIter2);
	      ThePair->SetTrack2(*PartIter2);
	      if (currentAnalysis->PairCut()->Pass(ThePair)){
		for (CorrFctnIter=currentAnalysis->CorrFctnCollection()->begin();
		     CorrFctnIter!=currentAnalysis->CorrFctnCollection()->end();CorrFctnIter++){
		  StHbtCorrFctn* CorrFctn = *CorrFctnIter;
		  CorrFctn->AddMixedPair(ThePair);
		}
	      }  // if passed pair cut
	      // OBSOLETE	      delete pair;
	    }    // loop over second particle
	  }      // loop over first particle
	}        // loop over pico-events stored in Mixing buffer
	// Now get rid of oldest stored pico-event in buffer.
	// This means (1) delete the event from memory, (2) "pop" the pointer to it from the MixingBuffer
	picoEventIter = currentAnalysis->MixingBuffer()->end();
	delete *picoEventIter;
	currentAnalysis->MixingBuffer()->pop_back();
      }  // if mixing buffer is full
      delete ThePair;
      currentAnalysis->MixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
    }   // if currentEvent is accepted by currentAnalysis
  }     // loop over Analyses
  delete currentHbtEvent;
}       // ProcessEvent
