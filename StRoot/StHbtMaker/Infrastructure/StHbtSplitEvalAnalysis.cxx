/***************************************************************************
 *
 * $Id: StHbtSplitEvalAnalysis.cxx,v 1.2 2001/11/05 14:11:19 lisa Exp $
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
 * $Log: StHbtSplitEvalAnalysis.cxx,v $
 * Revision 1.2  2001/11/05 14:11:19  lisa
 * small modifications to Splitting Analysis class and macro
 *
 * Revision 1.1  2000/08/15 22:18:47  lisa
 * Add a special HbtAnalysis class that estimates amount of splitting and add a macro to use it
 *
 * Revision 1.12  2000/07/16 22:23:17  laue
 * I forgot that we moved memberfunctions out of StHbtBaseAnalysis.
 * So my previous check-ins didn't compile with the library.
 * Now they do.
 *
 * Revision 1.11  2000/07/16 21:38:22  laue
 * StHbtCoulomb.cxx StHbtSectoredAnalysis.cxx : updated for standalone version
 * StHbtV0.cc StHbtV0.hh : some cast to prevent compiling warnings
 * StHbtParticle.cc StHbtParticle.hh : pointers mTrack,mV0 initialized to 0
 * StHbtIOBinary.cc : some printouts in #ifdef STHBTDEBUG
 * StHbtEvent.cc : B-Field set to 0.25Tesla, we have to think about a better
 *                 solution
 *
 * Revision 1.10  2000/07/06 18:45:51  laue
 * Copy constructor fixed. It couldn't handle identicle particles.
 *
 * Revision 1.9  2000/04/13 20:20:22  laue
 * Event mixing corrected. Now the first collection of the current event is
 * mixed with the second collection from the mixing buffer _AND_ vice verse
 *
 * Revision 1.8  2000/03/23 23:00:01  laue
 * StHbtSplitEvalAnalysis copy constructor now uses Clone() function of cuts
 * StHbtTypes now has StHbtTF1 for fitting purposes
 *
 * Revision 1.7  2000/03/17 17:23:05  laue
 * Roberts new three particle correlations implemented.
 *
 * Revision 1.6  2000/03/16 02:07:04  laue
 * Copy constructor added to StHbtSplitEvalAnalysis (only known cuts, corrfctn).
 *
 * StHbtBinaryReader can now derive filename from StIOMaker and read a list
 * of files.
 *
 * StHbtManager now holds a collection of StHbtEventWriters (multiple writes
 * possible now)
 *
 * Revision 1.5  2000/02/13 17:17:12  laue
 * Calls to the EventBegin() and EventEnd() functions implemented
 * The actual analysis is moved from StHbtManager to StHbtSplitEvalAnalysis
 *
 * Revision 1.4  2000/01/25 17:35:16  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.3  1999/10/04 15:38:53  lisa
 * include Franks new accessor methods StHbtSplitEvalAnalysis::CorrFctn and StHbtManager::Analysis as well as McEvent example macro
 *
 * Revision 1.2  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtSplitEvalAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"

#ifdef __ROOT__ 
ClassImp(StHbtSplitEvalAnalysis)
#endif

StHbtEventCut*    copyTheCut(StHbtEventCut*);
StHbtParticleCut* copyTheCut(StHbtParticleCut*);
StHbtPairCut*     copyTheCut(StHbtPairCut*);
StHbtCorrFctn*    copyTheCorrFctn(StHbtCorrFctn*);

// this little function used to apply ParticleCuts (TrackCuts or V0Cuts) and fill ParticleCollections of picoEvent
//  it is called from StHbtSplitEvalAnalysis::ProcessEvent()
 void FillHbtParticleCollection(StHbtParticleCut*         partCut,
 			       StHbtEvent*               hbtEvent,
				StHbtParticleCollection*  partCollection);
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
//     cout << "FillHbtParticleCollection function (in StHbtSplitEvalAnalysis.cxx) - undefined Particle Cut type!!! \n";
//   }
// }
//____________________________
StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(){
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mMixingBuffer = new StHbtPicoEventCollection;

  mQinvCut = 0.05;

  int npTbins = 20;
  float pTbinmax = 2.0;
  mRealSplits = new StHbt1DHisto("realSplit","realSplit",npTbins,0.0,pTbinmax);
  mRealAll = new StHbt1DHisto("realAll","realAll",npTbins,0.0,pTbinmax);
  mMixedSplits = new StHbt1DHisto("mixedSplit","mixedSplit",npTbins,0.0,pTbinmax);
  mMixedAll = new StHbt1DHisto("mixedAll","mixedAll",npTbins,0.0,pTbinmax);

  mSplitFractionUpperLimit = new StHbt1DHisto("SplitFracUpperLimit","SplitFracUpperLImit",npTbins,0.0,pTbinmax);
  mSplitFractionLowerLimit = new StHbt1DHisto("SplitFracLowerLimit","SplitFracLowerLImit",npTbins,0.0,pTbinmax);

}
//____________________________

StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis& a) : StHbtBaseAnalysis() {
  //StHbtSplitEvalAnalysis();
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mMixingBuffer = new StHbtPicoEventCollection;

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
      cout << " StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis& a) - event cut set " << endl;
  }
  if ( mFirstParticleCut ) {
      SetFirstParticleCut(mFirstParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis& a) - first particle cut set " << endl;
  }
  if ( mSecondParticleCut ) {
      SetSecondParticleCut(mSecondParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis& a) - second particle cut set " << endl;
  }  if ( mPairCut ) {
      SetPairCut(mPairCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis& a) - pair cut set " << endl;
  }

  StHbtCorrFctnIterator iter;
  for (iter=a.mCorrFctnCollection->begin(); iter!=a.mCorrFctnCollection->end();iter++){
    cout << " StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis& a) - looking for correlation functions " << endl;
    StHbtCorrFctn* fctn = (*iter)->Clone();
    if (fctn) AddCorrFctn(fctn);
    else cout << " StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis& a) - correlation function not found " << endl;
  }

  mNumEventsToMix = a.mNumEventsToMix;

  cout << " StHbtSplitEvalAnalysis::StHbtSplitEvalAnalysis(const StHbtSplitEvalAnalysis& a) - analysis copied " << endl;

}
//____________________________
StHbtSplitEvalAnalysis::~StHbtSplitEvalAnalysis(){
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
StHbtCorrFctn* StHbtSplitEvalAnalysis::CorrFctn(int n){  // return pointer to n-th correlation function
  if ( n<0 || n > (int)mCorrFctnCollection->size() )
    return NULL;
  StHbtCorrFctnIterator iter=mCorrFctnCollection->begin();
  for (int i=0; i<n ;i++){
    iter++;
  }
  return *iter;
}
//____________________________
StHbtString StHbtSplitEvalAnalysis::Report()
{
  cout << "StHbtSplitEvalAnalysis - constructing Report..."<<endl;
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
  if ( mCorrFctnCollection->size()==0 ) {
    cout << "StHbtSplitEvalAnalysis-Warning : no correlations functions in this analysis " << endl;
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
void StHbtSplitEvalAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {
  // startup for EbyE 
  EventBegin(hbtEvent);  
  // event cut and event cut monitor
  bool tmpPassEvent = mEventCut->Pass(hbtEvent);
  mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
  if (tmpPassEvent) {
    cout << "StHbtSplitEvalAnalysis::ProcessEvent() - Event has passed cut - build picoEvent from " <<
      hbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
    // OK, analysis likes the event-- build a pico event from it, using tracks the analysis likes...
    StHbtPicoEvent* picoEvent = new StHbtPicoEvent;       // this is what we will make pairs from and put in Mixing Buffer
    FillHbtParticleCollection(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstParticleCollection());
    if ( !(AnalyzeIdenticalParticles()) )
      FillHbtParticleCollection(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondParticleCollection());
    cout <<"StHbtSplitEvalAnalysis::ProcessEvent - #particles in First, Second Collections: " <<
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
    if (AnalyzeIdenticalParticles()) {             // only use First collection
      EndOuterLoop--;                                               // outer loop goes to next-to-last particle in First collection
      EndInnerLoop = picoEvent->FirstParticleCollection()->end() ;  // inner loop goes to last particle in First collection
    }
    else {                                                          // nonidentical - loop over First and Second collections
      StartInnerLoop = picoEvent->SecondParticleCollection()->begin(); // inner loop starts at first particle in Second collection
      EndInnerLoop   = picoEvent->SecondParticleCollection()->end() ;  // inner loop goes to last particle in Second collection
    }


    bool isSplit;
    double trackPT;

    /* NOTE: the reason this "special analysis" requires a StHbtAnalysis class is that
       we make plots of TRACKS not PAIRS (so that it cannot be done with a StHbtCorrFctn class).
       Also note that in what we do below, if there is deemed to be a tracksplit, then one
       of the daughters fills the "split" histogram, and the other the "all" histogram.
       This is the correct thing to do, so we don't double-count in the "all" histogram.
    */


    for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
      isSplit=0;
      if (AnalyzeIdenticalParticles()){
	StartInnerLoop = PartIter1;
	StartInnerLoop++;
      }
      ThePair->SetTrack1(*PartIter1);
      trackPT = ThePair->track1()->FourMomentum().vect().perp();
      for (PartIter2 = StartInnerLoop; PartIter2!=EndInnerLoop;PartIter2++){
	ThePair->SetTrack2(*PartIter2);
	if (fabs(ThePair->qInv()) < mQinvCut){
	  //	if (fabs(ThePair->qInv()) < (0.05*trackPT)){
	  if (!(mPairCut->Pass(ThePair))){
	    isSplit=1;
	  }  // if failed pair cut
	}
      }    // loop over second particle
      if (isSplit){
	mRealSplits->Fill(trackPT);
      }
      else{
	mRealAll->Fill(trackPT);
      }
    }      // loop over first particle
      
    // ok, now make mixed pairs, if the Mixing buffer is full
      
    cout << "StHbtSplitEvalAnalysis::ProcessEvent() - reals done" << endl;
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
	    isSplit=0;
	    ThePair->SetTrack1(*PartIter1);
	    trackPT = ThePair->track1()->FourMomentum().vect().perp();
	    for (PartIter2=StartInnerLoop;PartIter2!=EndInnerLoop;PartIter2++){
	      ThePair->SetTrack2(*PartIter2);
	      // testing...	      cout << "ThePair defined... going to pair cut... ";
	      if (fabs(ThePair->qInv()) < mQinvCut){
		//		if (fabs(ThePair->qInv()) < (0.05*trackPT)){
		if (!(mPairCut->Pass(ThePair))){
		  isSplit=1;
		}  // if failed pair cut
	      }
	    } // loop over second particle
	    if (isSplit){
	      mMixedSplits->Fill(trackPT);
	    }
	    else{
	      mMixedAll->Fill(trackPT);
	    }
	  } // loop over first particle
	} /* if identical particles */ 
	else { // non identical particles
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
      delete MixingBuffer()->back();
      MixingBuffer()->pop_back();
    }  // if mixing buffer is full
    delete ThePair;
    MixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
  }   // if currentEvent is accepted by currentAnalysis
  EventEnd(hbtEvent);  // cleanup for EbyE 
  cout << "StHbtSplitEvalAnalysis::ProcessEvent() - return to caller ... " << endl;
}
//_________________________
void StHbtSplitEvalAnalysis::EventBegin(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  mPairCut->EventBegin(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventBegin(ev);
  }
}
//_________________________
void StHbtSplitEvalAnalysis::EventEnd(const StHbtEvent* ev){
  mFirstParticleCut->EventEnd(ev);
  mSecondParticleCut->EventEnd(ev);
  mPairCut->EventEnd(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventEnd(ev);
  }
}
//_________________________
void StHbtSplitEvalAnalysis::Finish(){
  StHbtCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->Finish();
  }
  //  now divide histos etc to make splitting estimates...
  mSplitFractionUpperLimit->Divide(mRealSplits,mRealAll);

  //  mSplitFractionLowerLimit->Subtract(mRealSplits,mMixedSplits);
  mSplitFractionLowerLimit->Add(mRealSplits,mMixedSplits,1.0,-1.0);
  mSplitFractionLowerLimit->Divide(mRealAll);
}

