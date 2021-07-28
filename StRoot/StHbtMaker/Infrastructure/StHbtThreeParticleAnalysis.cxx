/***************************************************************************
 *
 * $Id: StHbtThreeParticleAnalysis.cxx,v 1.8 2003/09/02 17:58:32 perev Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *  
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *      This is the derived class for three-particle analysis objects.  
 *      Each of the simultaneous analyses should have one of derived 
 *      analysis classes running these instantiated.  They link
 *      into the aanager in an analysis collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtThreeParticleAnalysis.cxx,v $
 * Revision 1.8  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.7  2002/08/22 13:14:25  willson
 * Removed some warnings
 *
 * Revision 1.6  2001/06/03 20:56:40  willson
 * Sectoring, Cos(phi) calculation added
 *
 * Revision 1.5  2000/08/11 16:35:41  rcwells
 * Added number of events processed to each HBT analysis
 *
 * Revision 1.4  2000/07/25 03:26:52  willson
 * Error with small event collections fixed.
 *
 * Revision 1.3  2000/05/11 21:18:56  willson
 * Removed StHbtThreeParticleCorrFctn's...put methods in StHbtCorrFctn
 * Some methods in derived analysis classes moved to base analysis class
 *
 * Revision 1.2  2000/04/12 01:54:20  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtThreeParticleAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "TFile.h"

#ifdef __ROOT__ 
ClassImp(StHbtThreeParticleAnalysis)
#endif

// Factorial Function
int Factorial(int arg) {
  int fact=1;
  for (int i=arg; i>0; i--) fact*=i;
  return fact;
}

//____________________________
int StHbtThreeParticleAnalysis::Index(int binx, int biny, int binz)
{
  if (binx==-1 || binx==mNumBinsX || biny==-1 || biny==mNumBinsY || binz==-1 || binz==mNumBinsZ)
    return -1;
  return binx + biny*mNumBinsX + binz*mNumBinsX*mNumBinsY;
}



//This function sorts the event into a number of sectors if mIsSectoring==1;

//____________________________
void StHbtThreeParticleAnalysis::SortHbtParticleCollection(StHbtParticleCut*         partCut,
							       StHbtEvent*               hbtEvent,
							       StHbtParticleCollection** SectoredPicoEvent)
{
  int i,j,k;
  switch (partCut->Type()) {
  case hbtTrack:       // cut is cutting on Tracks
    {
      StHbtTrackCut* pCut = (StHbtTrackCut*) partCut;
      StHbtTrack* pParticle;
      StHbtTrackIterator pIter;
      StHbtTrackIterator startLoop = hbtEvent->TrackCollection()->begin();
      StHbtTrackIterator endLoop   = hbtEvent->TrackCollection()->end();
      float pxave=0.0, pyave=0.0, pzave=0.0, pxhigh=0.0, pxlow=0.0, pyhigh=0.0, pylow=0.0, pzhigh=0.0, pzlow=0.0;
      int num=0;

      for (pIter=startLoop;pIter!=endLoop;pIter++){
	pParticle = *pIter;
	bool tmpPassParticle = pCut->Pass(pParticle);
	pCut->FillCutMonitor(pParticle, tmpPassParticle);
	if (tmpPassParticle){
	  StHbtParticle* particle = new StHbtParticle(pParticle,pCut->Mass());
	  i = (int) floor((particle->FourMomentum().px()-mPXmin)/mDeltaP);
	  j = (int) floor((particle->FourMomentum().py()-mPYmin)/mDeltaP);
	  k = (int) floor((particle->FourMomentum().pz()-mPZmin)/mDeltaP);
	  if (particle->FourMomentum().px() > pxhigh) pxhigh = particle->FourMomentum().px();
	  if (particle->FourMomentum().px() < pxlow) pxlow = particle->FourMomentum().px();
	  if (particle->FourMomentum().py() > pyhigh) pyhigh = particle->FourMomentum().py();
	  if (particle->FourMomentum().py() < pylow) pylow = particle->FourMomentum().py();
	  if (particle->FourMomentum().pz() > pzhigh) pzhigh = particle->FourMomentum().pz();
	  if (particle->FourMomentum().pz() < pzlow) pzlow = particle->FourMomentum().pz();
	  if (i<mNumBinsX && j<mNumBinsY && k<mNumBinsZ && i>=0 && j>=0 && k>=0) {
	    SectoredPicoEvent[Index(i,j,k)]->push_back(particle);
	    pxave+=particle->FourMomentum().px();
	    pyave+=particle->FourMomentum().py();
	    pzave+=particle->FourMomentum().pz();
	    num++;
	  } 
	}
      }
      pxave /= num;
      pyave /= num;
      pzave /= num;

      cout << "AVE px=" << pxave << "  AVE py="<<pyave<<"  AVE pz="<<pzave<< endl;
      cout << "HIGH px=" << pxhigh << "     LOW px="<<pxlow<< "     HIGH py=" << pyhigh << "     LOW py="<<pylow<<"     HIGH pz=" << pzhigh << "     LOW pz="<<pzlow<<endl;

      break;      
    }
    
  case hbtV0:          // cut is cutting on V0s
    {
      StHbtV0Cut* pCut = (StHbtV0Cut*) partCut;
      StHbtV0* pParticle;
      StHbtV0Iterator pIter;
      StHbtV0Iterator startLoop = hbtEvent->V0Collection()->begin();
      StHbtV0Iterator endLoop   = hbtEvent->V0Collection()->end();
      // this following "for" loop is identical to the one above, but because of scoping, I can's see how to avoid repetition...
      
      for (pIter=startLoop;pIter!=endLoop;pIter++){
	pParticle = *pIter;
	bool tmpPassV0 = pCut->Pass(pParticle);
	pCut->FillCutMonitor(pParticle, tmpPassV0);
	if (tmpPassV0){
	  StHbtParticle* particle = new StHbtParticle(pParticle,pCut->Mass());
	  i = (int) floor((particle->FourMomentum().px()-mPXmin)/mDeltaP);
	  j = (int) floor((particle->FourMomentum().py()-mPYmin)/mDeltaP);
	  k = (int) floor((particle->FourMomentum().pz()-mPZmin)/mDeltaP);
	  SectoredPicoEvent[Index(i,j,k)]->push_back(particle);
	}
      }
      break;
    }
  default:
    cout << "FillHbtParticleCollection function (in StHbtThreeParticleAnalysis.cxx) - undefined Particle Cut type!!! \n";
  } 
}

//  this little function used to apply ParticleCuts (TrackCuts or V0Cuts) and fill 
//  ParticleCollections of picoEvent.  It is declared below, and defined in StHbtAnalysis.cxx
//  It is called from StHbtThreeParticleAnalysis::ProcessEvent() if mIsSectoring==0.
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
  mNeventsProcessed = 0;
  mCalcCosPhi = false;
  mPXmax = 5.0;
  mPXmin = -5.0;
  mPYmax = 5.0;
  mPYmin = -5.0;
  mPZmax = 5.0;
  mPZmin = -5.0;
  mDeltaP = 1.0;
  mNumBinsX = 1;
  mNumBinsY = 1;
  mNumBinsZ = 1;  
  mNormFactor = 1.0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mMixingBuffer = new StHbtPicoEventCollection;
  mSectoredMixingBuffer = new StHbtSectoredPicoEventCollection;
  mIsSectoring=0;
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
  // now delete every sectored PicoEvent in the SectoredEventMixingBuffer and then the Buffer itself
  StHbtSectoredPicoEventIterator spiter;
  for (spiter=mSectoredMixingBuffer->begin();spiter!=mSectoredMixingBuffer->end();spiter++){
    delete *spiter;
  }
  delete mSectoredMixingBuffer;
  
  mNeventsProcessed = 0;

  if (mCalcCosPhi) delete mCosPhiE;
}
//______________________
StHbtCorrFctn* StHbtThreeParticleAnalysis::CorrFctn(int n){  // return pointer to n-th correlation function
  if ( n<0 || n > (int)mCorrFctnCollection->size() )
    return NULL;
  StHbtCorrFctnIterator iter=mCorrFctnCollection->begin();
  for (int i=0; i<n ;i++){
    iter++;
  }
  return *iter;
}
//____________________________
StHbtString StHbtThreeParticleAnalysis::Report()
{
  char Sect[100];
  sprintf(Sect, "Sector bounds:  %4.2f<px<%4.2f %4.2f<py<%4.2f %4.2f<pz<%4.2f, DeltaP=%4.2f", mPXmin, mPXmax, mPYmin, mPYmax, mPZmin, mPZmax, mDeltaP);
  if (mCalcCosPhi) 
    cout << "CosPhiAnalysis - constructing Report..."<<endl;
  else
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
  StHbtCorrFctnIterator iter;
  if ( mCorrFctnCollection->size()==0 ) {
    cout << "StHbtThreeParticleAnalysis-Warning : no correlations functions in this analysis " << endl;
  }
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    temp += (*iter)->Report();
    temp += "\n";
  }

  if (mIsSectoring) {
    temp += "\nSectoring Information:\n";
    temp += Sect;
  }
  temp += "-------------\n";
  StHbtString returnThis=temp;
  return returnThis;
}
//_________________________
void StHbtThreeParticleAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {
  
  // Add event to processed events
  AddEventProcessed();

  if (!mIsSectoring) {

    if (!mCalcCosPhi) {
      int NumTriplets=0, NumParticles, NumMixedParticles1, NumMixedParticles2; 
      double TotalRealTriplets, TotalMixedTriplets=0.0, Rate;
      time_t TotalTime, StartTime;    
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
	
	NumParticles = picoEvent->FirstParticleCollection()->size();
	TotalRealTriplets = (NumParticles)*(NumParticles-1.0)*(NumParticles-2.0)/6.0;
	
	// OK, pico event is built
	// make real Triplets...
	
	// Fabrice points out that we do not need to keep creating/deleting Triplets all the time
	// We only ever need ONE Triplet, and we can just keep changing internal pointers
	// this should help speed things up
	StHbtTriplet* TheTriplet = new StHbtTriplet;
	
	StHbtParticleIterator PartIter1;
	StHbtParticleIterator PartIter2;
	StHbtParticleIterator PartIter3;
	StHbtCorrFctnIterator CorrFctnIter;
	StHbtParticleIterator StartOuterLoop = picoEvent->FirstParticleCollection()->begin();  // always
	StHbtParticleIterator EndOuterLoop   = picoEvent->FirstParticleCollection()->end();    // will be two less if identical
	StHbtParticleIterator StartMiddleLoop;
	StHbtParticleIterator EndMiddleLoop;
	StHbtParticleIterator StartInnerLoop;
	StHbtParticleIterator EndInnerLoop;
	
	if (NumParticles>2) {
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
	  
	  StartTime = time(NULL);
	  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
	    if (AnalyzeIdenticalParticles()){
	    StartMiddleLoop = PartIter1;
	    StartMiddleLoop++;
	    }
	    TheTriplet->SetTrack1(*PartIter1);
	    for (PartIter2 = StartMiddleLoop; PartIter2!=EndMiddleLoop;PartIter2++){
	      if (AnalyzeIdenticalParticles()){
		StartInnerLoop = PartIter2;
		StartInnerLoop++;
	      }
	      TheTriplet->SetTrack2(*PartIter2);
	      for (PartIter3 = StartInnerLoop; PartIter3!=EndInnerLoop;PartIter3++){
		TheTriplet->SetTrack3(*PartIter3);
		// The following lines have to be uncommented if you want TripletCutMonitors
		// they are not in for speed reasons
		// bool tmpPassTriplet = mTripletCut->Pass(TheTriplet);
		// mTripletCut->FillCutMonitor(TheTriplet, tmpPassTriplet);
		// if ( tmpPassTriplet ) {
		if (mTripletCut->Pass(TheTriplet)){
		  NumTriplets++;
		  for (CorrFctnIter=mCorrFctnCollection->begin();
		       CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
		    StHbtCorrFctn* CorrFctn = *CorrFctnIter;
		    CorrFctn->AddRealTriplet(TheTriplet);
		  }
		}  // if passed Triplet cut
	      }  // loop over third particle
	    }    // loop over second particle
	  }      // loop over first particle
	}        // Make sure we have at least 3 particles in collection
	
	// ok, now make mixed Triplets, if the Mixing buffer is full
	
	TotalTime = time(NULL) - StartTime;
	Rate = (double)NumTriplets/(double)TotalTime;
	
	cout << "StHbtThreeParticleAnalysis::ProcessEvent() - reals done, " << NumTriplets << " triplets entered out of " << TotalRealTriplets << "." << endl;
	cout << "Total time: " << TotalTime << "seconds.  This is " << Rate << " triplets per second." << endl; 
	if (MixingBufferFull()){
	  cout << "Mixing Buffer is full - lets rock and roll" << endl;
	}
	else {
	  cout << "Mixing Buffer not full -gotta wait " << MixingBuffer()->size() << endl;
	}
	NumTriplets=0;
	if (MixingBufferFull()){
	  StartOuterLoop = picoEvent->FirstParticleCollection()->begin();
	  EndOuterLoop   = picoEvent->FirstParticleCollection()->end();
	  StHbtPicoEvent* storedEvent1;
	  StHbtPicoEventIterator picoEventIter1;
	  StHbtPicoEventIterator BeginPicoEvent1;
	  StHbtPicoEventIterator EndPicoEvent1;
	  StHbtPicoEvent* storedEvent2;
	  StHbtPicoEventIterator picoEventIter2;
	  StHbtPicoEventIterator BeginPicoEvent2;
	  StHbtPicoEventIterator EndPicoEvent2;
	  
	  
	  BeginPicoEvent1 = MixingBuffer()->begin();
	  EndPicoEvent1   = MixingBuffer()->end();
	  EndPicoEvent1--;
	  EndPicoEvent2   = MixingBuffer()->end();
	  
	  StartTime = time(NULL);
	  for (picoEventIter1=BeginPicoEvent1;picoEventIter1!=EndPicoEvent1;picoEventIter1++){
	    cout << "Outer Event Done" << endl;
	    BeginPicoEvent2 = picoEventIter1;
	    BeginPicoEvent2++;
	    for (picoEventIter2=BeginPicoEvent2;picoEventIter2!=EndPicoEvent2;picoEventIter2++){
	      storedEvent1 = *picoEventIter1;
	      storedEvent2 = *picoEventIter2;
	      if (AnalyzeIdenticalParticles()){
		StartMiddleLoop = storedEvent1->FirstParticleCollection()->begin();
		EndMiddleLoop = storedEvent1->FirstParticleCollection()->end();
		StartInnerLoop = storedEvent2->FirstParticleCollection()->begin();
		EndInnerLoop = storedEvent2->FirstParticleCollection()->end();
	      }
	      else{
		// This is WRONG...Will have to be fixed later.
		StartMiddleLoop = storedEvent1->SecondParticleCollection()->begin();
		EndMiddleLoop = storedEvent1->SecondParticleCollection()->end();
		StartInnerLoop = storedEvent2->ThirdParticleCollection()->begin();
		EndInnerLoop = storedEvent2->ThirdParticleCollection()->end();
	      }
	      
	      NumMixedParticles1 = storedEvent1->FirstParticleCollection()->size();
	      NumMixedParticles2 = storedEvent2->FirstParticleCollection()->size();
	      TotalMixedTriplets += NumParticles*NumMixedParticles1*NumMixedParticles2;
	      
	      if (NumMixedParticles1>0 && NumMixedParticles2>0) {
		for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
		  TheTriplet->SetTrack1(*PartIter1);
		  for (PartIter2=StartMiddleLoop;PartIter2!=EndMiddleLoop;PartIter2++){
		    TheTriplet->SetTrack2(*PartIter2);
		    for (PartIter3=StartInnerLoop;PartIter3!=EndInnerLoop;PartIter3++){
		      TheTriplet->SetTrack3(*PartIter3);
		      if (mTripletCut->Pass(TheTriplet)){
			// testing...		cout << " TheTriplet passed TripletCut... ";
			NumTriplets++;
			for (CorrFctnIter=mCorrFctnCollection->begin();
			     CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
			  StHbtCorrFctn* CorrFctn = *CorrFctnIter;
			  CorrFctn->AddMixedTriplet(TheTriplet);
			  // testing...cout << " TheTriplet has been added to MixedTriplet method " << endl;
			}
		      }  // if passed Triplet cut
		    }  // loop over third particle
		  }    // loop over second particle
		}      // loop over first particle
	      }        // make sure there are particles in the collections
	    }        // loop over subset of pico-events stored in Mixing buffer
	  }          // loop over set of pico-events stored in Mixing buffer
	  
	  TotalTime = time(NULL) - StartTime;
	  Rate = NumTriplets/(double)TotalTime;
	  // Now get rid of oldest stored pico-event in buffer.
	  // This means (1) delete the event from memory, (2) "pop" the pointer to it from the MixingBuffer
	  picoEventIter1 = MixingBuffer()->end();
	  picoEventIter1--;   // bug fixed malisa 27jul99 - end() is one BEYOND the end! (besides crashing on linux, this was a memory leak)
	  delete *picoEventIter1;
	  MixingBuffer()->pop_back();
	}  // if mixing buffer is full
	delete TheTriplet;
	MixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
      }    // if currentEvent is accepted by currentAnalysis
      EventEnd(hbtEvent);  // cleanup for EbyE 
      cout << "StHbtThreeParticleAnalysis::ProcessEvent() - return to caller ...  " << NumTriplets << " triplets out of " << TotalMixedTriplets << " in mixing buffer." << endl;
      cout << "Total time: " << TotalTime << "seconds.  This is " << Rate << " triplets per second." << endl; 
    }  //  normal correlation function calculation
    
    if (mCalcCosPhi) {
      double C2Q12,C2Q23,C2Q31,C3,arg1,arg2,arg3,arg4,cosphi,cosphiError,termt;
      int bin1,bin2,bin3,bin4;
      // event cut and event cut monitor
      bool tmpPassEvent = mEventCut->Pass(hbtEvent);
      mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
      if (tmpPassEvent) {
	cout << "StHbtThreeParticleAnalysis::ProcessCosPhi() - Event has passed cut - build picoEvent from " <<
	  hbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
	// OK, analysis likes the event-- build a pico event from it, using tracks the analysis likes...
	StHbtPicoEvent* picoEvent = new StHbtPicoEvent;       // this is what we will make Triplets from
	FillHbtParticleCollection(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstParticleCollection());
	if ( !(AnalyzeIdenticalParticles()) ) {
	  FillHbtParticleCollection(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondParticleCollection());
	  FillHbtParticleCollection(mThirdParticleCut,(StHbtEvent*)hbtEvent,picoEvent->ThirdParticleCollection());}
	cout <<"StHbtThreeParticleAnalysis::ProcessCosPhi - #particles in First, Second, Third Collections: " <<
	  picoEvent->FirstParticleCollection()->size() << " " <<
	  picoEvent->SecondParticleCollection()->size() << " " <<
	  picoEvent->ThirdParticleCollection()->size() << endl;
	
	// OK, pico event is built
	// make real Triplets...
	
	// Fabrice points out that we do not need to keep creating/deleting Triplets all the time
	// We only ever need ONE Triplet, and we can just keep changing internal pointers
	// this should help speed things up
	StHbtTriplet* TheTriplet = new StHbtTriplet;
	
	if (picoEvent->FirstParticleCollection()->size()>2) {
	  
	  StHbtParticleIterator PartIter1;
	  StHbtParticleIterator PartIter2;
	  StHbtParticleIterator PartIter3;
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
	    }
	    TheTriplet->SetTrack1(*PartIter1);
	    for (PartIter2 = StartMiddleLoop; PartIter2!=EndMiddleLoop;PartIter2++){
	      if (AnalyzeIdenticalParticles()){
		StartInnerLoop = PartIter2;
		StartInnerLoop++;
	      }
	      TheTriplet->SetTrack2(*PartIter2);
	      for (PartIter3 = StartInnerLoop; PartIter3!=EndInnerLoop;PartIter3++){
		TheTriplet->SetTrack3(*PartIter3);
		if (mTripletCut->Pass(TheTriplet)){
		  bin1 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv12());
		  bin2 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv23());
		  bin3 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv31());
		  bin4 = mQ3CF->GetXaxis()->FindBin(TheTriplet->qInv());
		  
		  C2Q12 = mQ2CF->GetBinContent(bin1);
		  C2Q23 = mQ2CF->GetBinContent(bin2);
		  C2Q31 = mQ2CF->GetBinContent(bin3);
		  C3    = mNormFactor*(mQ3CF->GetBinContent(bin4));
		  
		  termt  = ::sqrt(fabs((C2Q12-1.0)*(C2Q23-1.0)*(C2Q31-1.0)));
		  if (termt) {
		    cosphi = ((C3-1.0) - (C2Q12-1.0) - (C2Q23-1.0) - (C2Q31-1.0))/(2.0*termt);
		    // cout << "CosPhi = " << cosphi << " Q12 = " << TheTriplet->qInv12() << " Q23 = " << TheTriplet->qInv23() << " Q31 = " << TheTriplet->qInv31() << " Q3 = " << TheTriplet->qInv() << " termt = " << termt << endl;
		    arg1 = mQ3CF->GetBinError(bin4);
		    arg2 = mQ2CF->GetBinError(bin1)*(1.0 + (C2Q23-1.0)*(C2Q31-1.0)*cosphi/termt);
		    arg3 = mQ2CF->GetBinError(bin2)*(1.0 + (C2Q31-1.0)*(C2Q12-1.0)*cosphi/termt);
		    arg4 = mQ2CF->GetBinError(bin3)*(1.0 + (C2Q12-1.0)*(C2Q23-1.0)*cosphi/termt);
		    cosphiError = (1.0/(2.0*termt))*::sqrt(arg1*arg1+arg2*arg2+arg3*arg3+arg4*arg4);
		  }
		  else {
		    cosphi = 0.0;
		    cosphiError = 0.0;
		  }
		  
		  //	cout << "Q12:" << TheTriplet->qInv12() << "  Q23:" << TheTriplet->qInv23() << "  Q31:" << TheTriplet->qInv31() << "  bin12:" << bin1 << "  bin23:" << bin2 << "  bin31:" << bin3 << "  Q3:" << TheTriplet->qInv() << "   
		  mCosPhi->AddBinContent(bin4, cosphi);
		  mCosPhiE->AddBinContent(bin4, cosphiError);
		  mCosPhiN->AddBinContent(bin4);
		}// if passed Triplet cut
	      }  // loop over third particle
	    }    // loop over second particle
	  }      // loop over first particle
	  
	  cout << "StHbtThreeParticleAnalysis::ProcessCosPhi() - done" << endl;
	  
	} // if there are at least 3 particles in our collection	
	
	delete picoEvent;
	delete TheTriplet;
      }    // if currentEvent is accepted by currentAnalysis
      cout << "StHbtThreeParticleAnalysis::ProcessCosPhi() - return to caller ... " << endl;
    }  //  Calculate CosPhi
    
  }  // Non-Sectored Analysis
  

  if (mIsSectoring) {

    if (!mCalcCosPhi) {
      int NumTriplets=0, NumParticles, i, j, k, i2, j2, k2, i3, j3, k3;
      int size1=0, size2=0, size3=0;
      double TotalRealTriplets, Rate;
      time_t TotalTime, StartTime; 
      
      // startup for EbyE 
      EventBegin(hbtEvent);  
      // event cut and event cut monitor
      bool tmpPassEvent = mEventCut->Pass(hbtEvent);
      mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
      if (tmpPassEvent) {
	cout << "StHbtThreeParticleAnalysis::ProcessEvent() - Event has passed cut - build sectoredpicoEvent from " <<
	  hbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
	// OK, analysis likes the event-- build a sectored pico event from it, using tracks the analysis likes...
	
	// this is what we will make Triplets from and put in Mixing Buffer
	StHbtSectoredPicoEvent* picoEvent = new StHbtSectoredPicoEvent(mNumBinsX, mNumBinsY, mNumBinsZ);     
	
	SortHbtParticleCollection(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstSectoredCollection());
	if ( !(AnalyzeIdenticalParticles()) ) {
	  SortHbtParticleCollection(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondSectoredCollection());
	  SortHbtParticleCollection(mThirdParticleCut,(StHbtEvent*)hbtEvent,picoEvent->ThirdSectoredCollection());
	}
	
	for (k=0; k<mNumBinsZ; k++) 
	  for (j=0; j<mNumBinsY; j++) 
	    for (i=0; i<mNumBinsX; i++) {
	      size1 += picoEvent->FirstSectoredCollection()[Index(i,j,k)]->size();
	      //  cout << "There are " << picoEvent->FirstSectoredCollection()[Index(i,j,k)]->size() << " particles in C["<<i<<"]["<<j<<"]["<<k<<"]"<<endl;
	      if ( !(AnalyzeIdenticalParticles()) ) {
		size2 += picoEvent->SecondSectoredCollection()[Index(i,j,k)]->size();
		size3 += picoEvent->ThirdSectoredCollection()[Index(i,j,k)]->size();
	      }
	    }
	
	cout <<"StHbtThreeParticleAnalysis::ProcessEvent (Sect) - #particles in First, Second, Third Collections: " <<
	  size1 << " " << size2 << " " << size3 << endl;
	
	NumParticles = size1;
	TotalRealTriplets = (NumParticles)*(NumParticles-1.0)*(NumParticles-2.0)/6.0;
	
	StartTime = time(NULL);
	
	if (AnalyzeIdenticalParticles()) {
	  // Real Triplets--Identical Particles
	  for (k=0; k<mNumBinsZ; k++) 
	    for (j=0; j<mNumBinsY; j++)
	      for (i=0; i<mNumBinsX; i++) {
		
		// 71 Terms!  This was computed earlier -- Avoids any repetition
		
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j-1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j-1,k+1), Index(i-1,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j-1,k+1), Index(i,j-1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j-1,k+1), Index(i,j,k+1));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j-1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i-1,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i+1,j-1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i+1,j,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i+1,j,k+1));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j-1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j-1,k+1), Index(i,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j-1,k+1), Index(i+1,j,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j-1,k+1), Index(i+1,j,k+1));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i-1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i-1,j+1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i,j+1,k+1));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i-1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i-1,j+1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i,j+1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i+1,j,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i+1,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i+1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i+1,j+1,k+1));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i,j+1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i+1,j,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i+1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i+1,j+1,k+1));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j+1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j+1,k+1), Index(i-1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j+1,k+1), Index(i,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j+1,k+1), Index(i,j+1,k+1));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i-1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i+1,j,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i+1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i+1,j+1,k+1));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j+1,k+1));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j+1,k+1), Index(i,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j+1,k+1), Index(i+1,j,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j+1,k+1), Index(i+1,j+1,k));
		
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i-1,j+1,k), Index(i,j+1,k));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k), Index(i+1,j,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i,j+1,k), Index(i+1,j+1,k));
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j+1,k));
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j+1,k), Index(i+1,j,k));
		
		//-------------//
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k), Index(i+1,j,k));
		
		//Mix sector with itself
		NumTriplets+=CreateRealTriplets(picoEvent, Index(i,j,k));	    
	      }
	  cout << "done" << endl;
	}
	else {
	  // Real Triplets--Non-Identical Particles
	  for (i=0; i<mNumBinsX; i++) 
	    for (j=0; j<mNumBinsY; j++)
	      for (k=0; k<mNumBinsZ; k++)
		for (i2=i-1; i2<i+1; i2++)
		  for (j2=j-1; j2<j+1; j2++)
		    for (k2=k-1; k2<k+1; k2++)
		      if (i2<mNumBinsX && j2<mNumBinsY && k2<mNumBinsZ && i2!=-1 && j2!=-1 && k2!=-1)
			for (i3=i-1; i3<i+1; i3++)
			  for (j3=j-1; j3<j+1; j3++)
			    for (k3=k-1; k3<k+1; k3++)
			      if (i3<mNumBinsX && j3<mNumBinsY && k3<mNumBinsZ && i3!=-1 && j3!=-1 && k3!=-1)
				CreateRealTriplets(picoEvent, Index(i,j,k), Index(i2,j2,k2), Index(i3,j3,k3));
	}
	
	TotalTime = time(NULL) - StartTime;
	Rate = (double)NumTriplets/(double)TotalTime;
	
	cout << "StHbtThreeParticleAnalysis::ProcessEvent() (Sect) - reals done, " << NumTriplets << " triplets entered out of " << (int)TotalRealTriplets << "." << endl;
	cout << "Total time: " << TotalTime << "seconds.  This is " << Rate << " triplets per second." << endl; 
	
	// ok, now make mixed Triplets, if the Mixing buffer is full
	
	if (SectoredMixingBufferFull()){
	  cout << "Mixing Buffer is full - lets rock and roll" << endl;
	}
	else {
	  cout << "Mixing Buffer not full -gotta wait " << SectoredMixingBuffer()->size() << endl;
	}
	NumTriplets=0;
	if (SectoredMixingBufferFull()){
	  StHbtSectoredPicoEvent* storedEvent1;
	  StHbtSectoredPicoEventIterator picoEventIter1;
	  StHbtSectoredPicoEventIterator BeginPicoEvent1;
	  StHbtSectoredPicoEventIterator EndPicoEvent1;
	  StHbtSectoredPicoEvent* storedEvent2;
	  StHbtSectoredPicoEventIterator picoEventIter2;
	  StHbtSectoredPicoEventIterator BeginPicoEvent2;
	  StHbtSectoredPicoEventIterator EndPicoEvent2;
	  
	  BeginPicoEvent1 = SectoredMixingBuffer()->begin();
	  EndPicoEvent1   = SectoredMixingBuffer()->end();
	  EndPicoEvent1--;
	  EndPicoEvent2   = SectoredMixingBuffer()->end();
	  
	  StartTime = time(NULL);
	  for (picoEventIter1=BeginPicoEvent1;picoEventIter1!=EndPicoEvent1;picoEventIter1++){
	    BeginPicoEvent2 = picoEventIter1;
	    BeginPicoEvent2++;
	    for (picoEventIter2=BeginPicoEvent2;picoEventIter2!=EndPicoEvent2;picoEventIter2++){
	      storedEvent1 = *picoEventIter1;
	      storedEvent2 = *picoEventIter2;
	      if (AnalyzeIdenticalParticles()){
		// Mixed Triplets--Identical Particles
		for (i=0; i<mNumBinsX; i++) 
		  for (j=0; j<mNumBinsY; j++)
		    for (k=0; k<mNumBinsZ; k++)
		      for (i2=i-1; i2<=i+1; i2++)
			for (j2=j-1; j2<=j+1; j2++)
			  for (k2=k-1; k2<=k+1; k2++)
			    if (i2<mNumBinsX && j2<mNumBinsY && k2<mNumBinsZ && i2!=-1 && j2!=-1 && k2!=-1)
			      for (i3=i-1; i3<=i+1; i3++)
				for (j3=j-1; j3<=j+1; j3++)
				  for (k3=k-1; k3<=k+1; k3++)
				    if (i3<mNumBinsX && j3<mNumBinsY && k3<mNumBinsZ && i3!=-1 && j3!=-1 && k3!=-1 && i3!=i2-2 && i3!=i2+2 && j3!=j2-2 && j3!=j2+2 && k3!=k2-2 && k3!=k2+2)
				      NumTriplets+=CreateMixedTriplets(picoEvent->FirstSectoredCollection()[Index(i,j,k)], storedEvent1->FirstSectoredCollection()[Index(i2,j2,k2)], storedEvent2->FirstSectoredCollection()[Index(i3,j3,k3)]);
	      }
	      else{
		// This is WRONG...Will have to be fixed later.
		// Mixed Triplets--Identical Particles
		for (i=0; i<mNumBinsX; i++) 
		  for (j=0; j<mNumBinsY; j++)
		    for (k=0; k<mNumBinsZ; k++)
		      for (i2=i-1; i2<i+1; i2++)
			for (j2=j-1; j2<j+1; j2++)
			  for (k2=k-1; k2<k+1; k2++)
			    if (i2<mNumBinsX && j2<mNumBinsY && k2<mNumBinsZ && i2!=-1 && j2!=-1 && k2!=-1)
			      for (i3=i-1; i3<i+1; i3++)
				for (j3=j-1; j3<j+1; j3++)
				  for (k3=k-1; k3<k+1; k3++)
				    if (i3<mNumBinsX && j3<mNumBinsY && k3<mNumBinsZ && i3!=-1 && j3!=-1 && k3!=-1)
				      CreateMixedTriplets(picoEvent->FirstSectoredCollection()[Index(i,j,k)], storedEvent1->SecondSectoredCollection()[Index(i2,j2,k2)], storedEvent2->ThirdSectoredCollection()[Index(i3,j3,k3)]);	     
	      }
	    }
	  }  
	  
	  TotalTime = time(NULL) - StartTime;
	  Rate = NumTriplets/(double)TotalTime;
	  
	  // Now get rid of oldest stored pico-event in buffer.
	  // This means (1) delete the event from memory, (2) "pop" the pointer to it from the MixingBuffer
	  picoEventIter1 = SectoredMixingBuffer()->end();
	  picoEventIter1--;   // bug fixed malisa 27jul99 - end() is one BEYOND the end! (besides crashing on linux, this was a memory leak)
	  delete *picoEventIter1;
	  
	  SectoredMixingBuffer()->pop_back();
	}  // if mixing buffer is full
	SectoredMixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
      }    // if currentEvent is accepted by currentAnalysis
      EventEnd(hbtEvent);  // cleanup for EbyE 
      cout << "StHbtThreeParticleAnalysis::ProcessEvent() (Sect) - return to caller ...  " << NumTriplets << " triplets out of about " << size1*size1*size1 << " in mixing buffer." << endl;
      cout << "Total time: " << TotalTime << "seconds.  This is " << Rate << " triplets per second." << endl; 
      
    }  // normal correlation function calculation
    
    if (mCalcCosPhi) {
      int NumTriplets=0, NumParticles, i, j, k, i2, j2, k2, i3, j3, k3;
      int size1=0, size2=0, size3=0;
      double TotalRealTriplets, Rate;
      time_t TotalTime, StartTime; 
      
      // event cut and event cut monitor
      bool tmpPassEvent = mEventCut->Pass(hbtEvent);
      mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
      if (tmpPassEvent) {
	cout << "StHbtThreeParticleAnalysis::ProcessEvent() - Event has passed cut - build sectoredpicoEvent from " <<
	  hbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
	// OK, analysis likes the event-- build a sectored pico event from it, using tracks the analysis likes...
	
	// this is what we will make Triplets from and put in Mixing Buffer
	StHbtSectoredPicoEvent* picoEvent = new StHbtSectoredPicoEvent(mNumBinsX, mNumBinsY, mNumBinsZ);     
	
	SortHbtParticleCollection(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstSectoredCollection());
	if ( !(AnalyzeIdenticalParticles()) ) {
	  SortHbtParticleCollection(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondSectoredCollection());
	  SortHbtParticleCollection(mThirdParticleCut,(StHbtEvent*)hbtEvent,picoEvent->ThirdSectoredCollection());
	}
	
	for (k=0; k<mNumBinsZ; k++) 
	  for (j=0; j<mNumBinsY; j++) 
	    for (i=0; i<mNumBinsX; i++) {
	      size1 += picoEvent->FirstSectoredCollection()[Index(i,j,k)]->size();
	      //  cout << "There are " << picoEvent->FirstSectoredCollection()[Index(i,j,k)]->size() << " particles in C["<<i<<"]["<<j<<"]["<<k<<"]"<<endl;
	      if ( !(AnalyzeIdenticalParticles()) ) {
		size2 += picoEvent->SecondSectoredCollection()[Index(i,j,k)]->size();
		size3 += picoEvent->ThirdSectoredCollection()[Index(i,j,k)]->size();
	      }
	    }
	
	cout <<"StHbtThreeParticleAnalysis::ProcessEvent (Sect) - #particles in First, Second, Third Collections: " <<
	  size1 << " " << size2 << " " << size3 << endl;
	
	NumParticles = size1;
	TotalRealTriplets = (NumParticles)*(NumParticles-1.0)*(NumParticles-2.0)/6.0;
	
	StartTime = time(NULL);
	
	if (AnalyzeIdenticalParticles()) {
	  // Real Triplets--Identical Particles
	  for (k=0; k<mNumBinsZ; k++) 
	    for (j=0; j<mNumBinsY; j++)
	      for (i=0; i<mNumBinsX; i++) {
		
		// 71 Terms!  This was computed earlier -- Avoids any repetition
		
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j-1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j-1,k+1), Index(i-1,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j-1,k+1), Index(i,j-1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j-1,k+1), Index(i,j,k+1));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j-1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i-1,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i+1,j-1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i+1,j,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j-1,k+1), Index(i+1,j,k+1));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j-1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j-1,k+1), Index(i,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j-1,k+1), Index(i+1,j,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j-1,k+1), Index(i+1,j,k+1));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i-1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i-1,j+1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j,k+1), Index(i,j+1,k+1));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i-1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i-1,j+1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i,j+1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i+1,j,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i+1,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i+1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j,k+1), Index(i+1,j+1,k+1));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i,j+1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i+1,j,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i+1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j,k+1), Index(i+1,j+1,k+1));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j+1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j+1,k+1), Index(i-1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j+1,k+1), Index(i,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j+1,k+1), Index(i,j+1,k+1));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i-1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i+1,j,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i+1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k+1), Index(i+1,j+1,k+1));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j+1,k+1));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j+1,k+1), Index(i,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j+1,k+1), Index(i+1,j,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j+1,k+1), Index(i+1,j+1,k));
		
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i-1,j+1,k), Index(i,j+1,k));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k), Index(i+1,j,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i,j+1,k), Index(i+1,j+1,k));
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j+1,k));
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j+1,k), Index(i+1,j,k));
		
		//-------------//
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k), Index(i+1,j,k));
		
		//Mix sector with itself
		NumTriplets+=CalculateCosPhi(picoEvent, Index(i,j,k));	    
	      }
	  cout << "done" << endl;
	}
	else {
	  // Real Triplets--Non-Identical Particles
	  for (i=0; i<mNumBinsX; i++) 
	    for (j=0; j<mNumBinsY; j++)
	      for (k=0; k<mNumBinsZ; k++)
		for (i2=i-1; i2<i+1; i2++)
		  for (j2=j-1; j2<j+1; j2++)
		    for (k2=k-1; k2<k+1; k2++)
		      if (i2<mNumBinsX && j2<mNumBinsY && k2<mNumBinsZ && i2!=-1 && j2!=-1 && k2!=-1)
			for (i3=i-1; i3<i+1; i3++)
			  for (j3=j-1; j3<j+1; j3++)
			    for (k3=k-1; k3<k+1; k3++)
			      if (i3<mNumBinsX && j3<mNumBinsY && k3<mNumBinsZ && i3!=-1 && j3!=-1 && k3!=-1)
				CalculateCosPhi(picoEvent, Index(i,j,k), Index(i2,j2,k2), Index(i3,j3,k3));
	}
	
	TotalTime = time(NULL) - StartTime;
	Rate = (double)NumTriplets/(double)TotalTime;
	
	cout << "StHbtThreeParticleAnalysis::ProcessCosPhi() (Sect) - done, " << NumTriplets << " triplets entered out of " << (int)TotalRealTriplets << "." << endl;
	cout << "Total time: " << TotalTime << "seconds.  This is " << Rate << " triplets per second." << endl; 
	
	delete picoEvent;
	
      }  // if currentEvent is accepted by currentAnalysis
      cout << "StHbtThreeParticleAnalysis::ProcessCosPhi() (Sect) - return to caller ... " << endl;
    } // Calculate CosPhi
    
  } // Sectoring Analysis

} // ProcessEvent
 
//_________________________
int StHbtThreeParticleAnalysis::CreateRealTriplets(StHbtSectoredPicoEvent *picoEvent, int Index1) {

  int NumTriplets=0;

  StHbtParticleCollection *partColl1 = picoEvent->FirstSectoredCollection()[Index1];

  if (partColl1->size()<3) return 0;
 
  StHbtTriplet* TheTriplet = new StHbtTriplet;
  
  StHbtParticleIterator PartIter1;
  StHbtParticleIterator PartIter2;
  StHbtParticleIterator PartIter3;
  StHbtCorrFctnIterator CorrFctnIter;
  StHbtParticleIterator StartOuterLoop = partColl1->begin();  // always
  StHbtParticleIterator EndOuterLoop   = partColl1->end();    // will be two less
  StHbtParticleIterator StartMiddleLoop;
  StHbtParticleIterator EndMiddleLoop;
  StHbtParticleIterator StartInnerLoop;
  StHbtParticleIterator EndInnerLoop;
	
  EndOuterLoop--;
  EndOuterLoop--;                     // outer loop goes to next-to-next-to-last particle in collection
  EndMiddleLoop = partColl1->end() ;  // middle loop goes to next-to-last particle in collection
  EndMiddleLoop--;
  EndInnerLoop = partColl1->end() ;   // inner loop goes to last particle in collection

  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
    StartMiddleLoop = PartIter1;
    StartMiddleLoop++;
    TheTriplet->SetTrack1(*PartIter1);
    for (PartIter2 = StartMiddleLoop; PartIter2!=EndMiddleLoop;PartIter2++){
      StartInnerLoop = PartIter2;
      StartInnerLoop++;
      TheTriplet->SetTrack2(*PartIter2);
      for (PartIter3 = StartInnerLoop; PartIter3!=EndInnerLoop;PartIter3++){
	TheTriplet->SetTrack3(*PartIter3);
	// The following lines have to be uncommented if you want TripletCutMonitors
	// they are not in for speed reasons
	// bool tmpPassTriplet = mTripletCut->Pass(TheTriplet);
	// mTripletCut->FillCutMonitor(TheTriplet, tmpPassTriplet);
	// if ( tmpPassTriplet ) {
	if (mTripletCut->Pass(TheTriplet)){
	  NumTriplets++;
	  for (CorrFctnIter=mCorrFctnCollection->begin();
	       CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	    StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	    CorrFctn->AddRealTriplet(TheTriplet);
	  }
	}  // if passed Triplet cut
      }  // loop over third particle
    }    // loop over second particle
  }      // loop over first particle   

  delete TheTriplet;

  return NumTriplets;
 
}



//_________________________
int StHbtThreeParticleAnalysis::CreateRealTriplets(StHbtSectoredPicoEvent *picoEvent, int Index1, int Index2) {

  int NumTriplets=0;

  if (Index1==-1 || Index2==-1) return 0;

  StHbtParticleCollection *partColl1 = picoEvent->FirstSectoredCollection()[Index1];
  StHbtParticleCollection *partColl2 = picoEvent->FirstSectoredCollection()[Index2];

  StHbtTriplet* TheTriplet = new StHbtTriplet;
	
  // First, take two particles from the first collection and one particle from the second.

  if (partColl1->size()>=2 && partColl2->size()>=1) {
    
    StHbtParticleIterator PartIter1;
    StHbtParticleIterator PartIter2;
    StHbtParticleIterator PartIter3;
    StHbtCorrFctnIterator CorrFctnIter;
    StHbtParticleIterator StartOuterLoop = partColl1->begin();  // always
    StHbtParticleIterator EndOuterLoop   = partColl1->end();    // will be one less
    StHbtParticleIterator StartMiddleLoop;                      // depends on start value of outer loop
    StHbtParticleIterator EndMiddleLoop  = partColl1->end();
    StHbtParticleIterator StartInnerLoop = partColl2->begin();  
    StHbtParticleIterator EndInnerLoop   = partColl2->end();
    
    EndOuterLoop--;  // outer loop goes to next-to-last particle in First collection
    
    for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
      StartMiddleLoop = PartIter1;
      StartMiddleLoop++;
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
	    NumTriplets++;
	    for (CorrFctnIter=mCorrFctnCollection->begin();
		 CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	      StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	      CorrFctn->AddRealTriplet(TheTriplet);
	    }
	  }  // if passed Triplet cut
	}  // loop over third particle
      }    // loop over second particle
    }      // loop over first particle
    
  }


 // Then, take two particles from the second collection and one particle from the first.

  if (partColl1->size()>=1 && partColl2->size()>=2) {
    
    StHbtParticleIterator PartIter1;
    StHbtParticleIterator PartIter2;
    StHbtParticleIterator PartIter3;
    StHbtCorrFctnIterator CorrFctnIter;
    StHbtParticleIterator StartOuterLoop = partColl2->begin();  // always
    StHbtParticleIterator EndOuterLoop   = partColl2->end();    // will be one less
    StHbtParticleIterator StartMiddleLoop;                      // depends on start value of outer loop
    StHbtParticleIterator EndMiddleLoop  = partColl2->end();
    StHbtParticleIterator StartInnerLoop = partColl1->begin();  
    StHbtParticleIterator EndInnerLoop   = partColl1->end();
    
    EndOuterLoop--;  // outer loop goes to next-to-last particle in First collection
    
    for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
      StartMiddleLoop = PartIter1;
      StartMiddleLoop++;
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
	    NumTriplets++;
	    for (CorrFctnIter=mCorrFctnCollection->begin();
		 CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	      StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	      CorrFctn->AddRealTriplet(TheTriplet);
	    }
	  }  // if passed Triplet cut
	}  // loop over third particle
      }    // loop over second particle
    }      // loop over first particle
    
  }

  delete TheTriplet;

  return NumTriplets;
    
}
 

//_________________________
int StHbtThreeParticleAnalysis::CreateRealTriplets(StHbtSectoredPicoEvent *picoEvent, int Index1, int Index2, int Index3) {

  int NumTriplets=0;

  if (Index1==-1 || Index2==-1 || Index3==-1) return 0;

  StHbtParticleCollection *partColl1 = picoEvent->FirstSectoredCollection()[Index1];
  StHbtParticleCollection *partColl2 = picoEvent->FirstSectoredCollection()[Index2];
  StHbtParticleCollection *partColl3 = picoEvent->FirstSectoredCollection()[Index3];

  if (!partColl1->size() || !partColl2->size() || !partColl2->size()) return 0;

  StHbtTriplet* TheTriplet = new StHbtTriplet;
 
  StHbtParticleIterator PartIter1;
  StHbtParticleIterator PartIter2;
  StHbtParticleIterator PartIter3;
  StHbtCorrFctnIterator CorrFctnIter;
  StHbtParticleIterator StartOuterLoop = partColl1->begin();
  StHbtParticleIterator EndOuterLoop   = partColl1->end();    
  StHbtParticleIterator StartMiddleLoop = partColl2->begin();
  StHbtParticleIterator EndMiddleLoop = partColl2->end();
  StHbtParticleIterator StartInnerLoop = partColl3->begin();  
  StHbtParticleIterator EndInnerLoop = partColl3->end();
  
  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
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
	  NumTriplets++;
	  for (CorrFctnIter=mCorrFctnCollection->begin();
	       CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	    StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	    CorrFctn->AddRealTriplet(TheTriplet);
	  }
	}  // if passed Triplet cut
      }  // loop over third particle
    }    // loop over second particle
  }      // loop over first particle

  delete TheTriplet;
  
  return NumTriplets;

}


//_________________________
int StHbtThreeParticleAnalysis::CalculateCosPhi(StHbtSectoredPicoEvent *picoEvent, int Index1) {

  double C2Q12,C2Q23,C2Q31,C3,arg1,arg2,arg3,arg4,cosphi,cosphiError,termt;
  int bin1,bin2,bin3,bin4,NumTriplets=0;

  StHbtParticleCollection *partColl1 = picoEvent->FirstSectoredCollection()[Index1];

  if (partColl1->size()<3) return 0;
 
  StHbtTriplet* TheTriplet = new StHbtTriplet;
  
  StHbtParticleIterator PartIter1;
  StHbtParticleIterator PartIter2;
  StHbtParticleIterator PartIter3;
  StHbtParticleIterator StartOuterLoop = partColl1->begin();  // always
  StHbtParticleIterator EndOuterLoop   = partColl1->end();    // will be two less
  StHbtParticleIterator StartMiddleLoop;
  StHbtParticleIterator EndMiddleLoop;
  StHbtParticleIterator StartInnerLoop;
  StHbtParticleIterator EndInnerLoop;
	
  EndOuterLoop--;
  EndOuterLoop--;                     // outer loop goes to next-to-next-to-last particle in collection
  EndMiddleLoop = partColl1->end() ;  // middle loop goes to next-to-last particle in collection
  EndMiddleLoop--;
  EndInnerLoop = partColl1->end() ;   // inner loop goes to last particle in collection

  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
    StartMiddleLoop = PartIter1;
    StartMiddleLoop++;
    TheTriplet->SetTrack1(*PartIter1);
    for (PartIter2 = StartMiddleLoop; PartIter2!=EndMiddleLoop;PartIter2++){
      StartInnerLoop = PartIter2;
      StartInnerLoop++;
      TheTriplet->SetTrack2(*PartIter2);
      for (PartIter3 = StartInnerLoop; PartIter3!=EndInnerLoop;PartIter3++){
	TheTriplet->SetTrack3(*PartIter3);
	// The following lines have to be uncommented if you want TripletCutMonitors
	// they are not in for speed reasons
	// bool tmpPassTriplet = mTripletCut->Pass(TheTriplet);
	// mTripletCut->FillCutMonitor(TheTriplet, tmpPassTriplet);
	// if ( tmpPassTriplet ) {
	if (mTripletCut->Pass(TheTriplet)){
	  NumTriplets++;
	  bin1 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv12());
	  bin2 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv23());
	  bin3 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv31());
	  bin4 = mQ3CF->GetXaxis()->FindBin(TheTriplet->qInv());
	  
	  C2Q12 = mQ2CF->GetBinContent(bin1);
	  C2Q23 = mQ2CF->GetBinContent(bin2);
	  C2Q31 = mQ2CF->GetBinContent(bin3);
	  C3    = mNormFactor*(mQ3CF->GetBinContent(bin4));
	  
	  termt  = ::sqrt(fabs((C2Q12-1.0)*(C2Q23-1.0)*(C2Q31-1.0)));
	  if (termt) {
	    cosphi = ((C3-1.0) - (C2Q12-1.0) - (C2Q23-1.0) - (C2Q31-1.0))/(2.0*termt);
	    arg1 = mQ3CF->GetBinError(bin4);
	    arg2 = mQ2CF->GetBinError(bin1)*(1.0 + (C2Q23-1.0)*(C2Q31-1.0)*cosphi/termt);
	    arg3 = mQ2CF->GetBinError(bin2)*(1.0 + (C2Q31-1.0)*(C2Q12-1.0)*cosphi/termt);
	    arg4 = mQ2CF->GetBinError(bin3)*(1.0 + (C2Q12-1.0)*(C2Q23-1.0)*cosphi/termt);
	    cosphiError = (1.0/(2.0*termt))*::sqrt(arg1*arg1+arg2*arg2+arg3*arg3+arg4*arg4);
	  }
	  else {
	    cosphi = 0.0;
	    cosphiError = 0.0;
	  }
	  
	  mCosPhi->AddBinContent(bin4, cosphi);
	  mCosPhiE->AddBinContent(bin4, cosphiError);
	  mCosPhiN->AddBinContent(bin4);

	}// if passed Triplet cut
      }  // loop over third particle
    }    // loop over second particle
  }      // loop over first particle   

  delete TheTriplet;

  return NumTriplets;
 
}



//_________________________
int StHbtThreeParticleAnalysis::CalculateCosPhi(StHbtSectoredPicoEvent *picoEvent, int Index1, int Index2) {

  double C2Q12,C2Q23,C2Q31,C3,arg1,arg2,arg3,arg4,cosphi,cosphiError,termt;
  int bin1,bin2,bin3,bin4,NumTriplets=0;

  if (Index1==-1 || Index2==-1) return 0;

  StHbtParticleCollection *partColl1 = picoEvent->FirstSectoredCollection()[Index1];
  StHbtParticleCollection *partColl2 = picoEvent->FirstSectoredCollection()[Index2];

  StHbtTriplet* TheTriplet = new StHbtTriplet;
	
  // First, take two particles from the first collection and one particle from the second.

  if (partColl1->size()>=2 && partColl2->size()>=1) {
    
    StHbtParticleIterator PartIter1;
    StHbtParticleIterator PartIter2;
    StHbtParticleIterator PartIter3;
    StHbtParticleIterator StartOuterLoop = partColl1->begin();  // always
    StHbtParticleIterator EndOuterLoop   = partColl1->end();    // will be one less
    StHbtParticleIterator StartMiddleLoop;                      // depends on start value of outer loop
    StHbtParticleIterator EndMiddleLoop  = partColl1->end();
    StHbtParticleIterator StartInnerLoop = partColl2->begin();  
    StHbtParticleIterator EndInnerLoop   = partColl2->end();
    
    EndOuterLoop--;  // outer loop goes to next-to-last particle in First collection
    
    for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
      StartMiddleLoop = PartIter1;
      StartMiddleLoop++;
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
	    NumTriplets++;
	    bin1 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv12());
	    bin2 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv23());
	    bin3 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv31());
	    bin4 = mQ3CF->GetXaxis()->FindBin(TheTriplet->qInv());
	    
	    C2Q12 = mQ2CF->GetBinContent(bin1);
	    C2Q23 = mQ2CF->GetBinContent(bin2);
	    C2Q31 = mQ2CF->GetBinContent(bin3);
	    C3    = mNormFactor*(mQ3CF->GetBinContent(bin4));
	    
	    termt  = ::sqrt(fabs((C2Q12-1.0)*(C2Q23-1.0)*(C2Q31-1.0)));
	    if (termt) {
	      cosphi = ((C3-1.0) - (C2Q12-1.0) - (C2Q23-1.0) - (C2Q31-1.0))/(2.0*termt);
	      arg1 = mQ3CF->GetBinError(bin4);
	      arg2 = mQ2CF->GetBinError(bin1)*(1.0 + (C2Q23-1.0)*(C2Q31-1.0)*cosphi/termt);
	      arg3 = mQ2CF->GetBinError(bin2)*(1.0 + (C2Q31-1.0)*(C2Q12-1.0)*cosphi/termt);
	      arg4 = mQ2CF->GetBinError(bin3)*(1.0 + (C2Q12-1.0)*(C2Q23-1.0)*cosphi/termt);
	      cosphiError = (1.0/(2.0*termt))*::sqrt(arg1*arg1+arg2*arg2+arg3*arg3+arg4*arg4);
	    }
	    else {
	      cosphi = 0.0;
	      cosphiError = 0.0;
	    }
	    
	    mCosPhi->AddBinContent(bin4, cosphi);
	    mCosPhiE->AddBinContent(bin4, cosphiError);
	    mCosPhiN->AddBinContent(bin4);

	  }// if passed Triplet cut
	}  // loop over third particle
      }    // loop over second particle
    }      // loop over first particle
    
  }


 // Then, take two particles from the second collection and one particle from the first.

  if (partColl1->size()>=1 && partColl2->size()>=2) {
    
    StHbtParticleIterator PartIter1;
    StHbtParticleIterator PartIter2;
    StHbtParticleIterator PartIter3;
    StHbtParticleIterator StartOuterLoop = partColl2->begin();  // always
    StHbtParticleIterator EndOuterLoop   = partColl2->end();    // will be one less
    StHbtParticleIterator StartMiddleLoop;                      // depends on start value of outer loop
    StHbtParticleIterator EndMiddleLoop  = partColl2->end();
    StHbtParticleIterator StartInnerLoop = partColl1->begin();  
    StHbtParticleIterator EndInnerLoop   = partColl1->end();
    
    EndOuterLoop--;  // outer loop goes to next-to-last particle in First collection
    
    for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
      StartMiddleLoop = PartIter1;
      StartMiddleLoop++;
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
	    NumTriplets++;
	    bin1 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv12());
	    bin2 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv23());
	    bin3 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv31());
	    bin4 = mQ3CF->GetXaxis()->FindBin(TheTriplet->qInv());
	    
	    C2Q12 = mQ2CF->GetBinContent(bin1);
	    C2Q23 = mQ2CF->GetBinContent(bin2);
	    C2Q31 = mQ2CF->GetBinContent(bin3);
	    C3    = mNormFactor*(mQ3CF->GetBinContent(bin4));
	    
	    termt  = ::sqrt(fabs((C2Q12-1.0)*(C2Q23-1.0)*(C2Q31-1.0)));
	    if (termt) {
	      cosphi = ((C3-1.0) - (C2Q12-1.0) - (C2Q23-1.0) - (C2Q31-1.0))/(2.0*termt);
	      arg1 = mQ3CF->GetBinError(bin4);
	      arg2 = mQ2CF->GetBinError(bin1)*(1.0 + (C2Q23-1.0)*(C2Q31-1.0)*cosphi/termt);
	      arg3 = mQ2CF->GetBinError(bin2)*(1.0 + (C2Q31-1.0)*(C2Q12-1.0)*cosphi/termt);
	      arg4 = mQ2CF->GetBinError(bin3)*(1.0 + (C2Q12-1.0)*(C2Q23-1.0)*cosphi/termt);
	      cosphiError = (1.0/(2.0*termt))*::sqrt(arg1*arg1+arg2*arg2+arg3*arg3+arg4*arg4);
	    }
	    else {
	      cosphi = 0.0;
	      cosphiError = 0.0;
	    }
	    
	    mCosPhi->AddBinContent(bin4, cosphi);
	    mCosPhiE->AddBinContent(bin4, cosphiError);
	    mCosPhiN->AddBinContent(bin4);
	    
	  }  // if passed Triplet cut
	}  // loop over third particle
      }    // loop over second particle
    }      // loop over first particle
    
  }

  delete TheTriplet;

  return NumTriplets;
    
}
 

//_________________________
int StHbtThreeParticleAnalysis::CalculateCosPhi(StHbtSectoredPicoEvent *picoEvent, int Index1, int Index2, int Index3) {

  double C2Q12,C2Q23,C2Q31,C3,arg1,arg2,arg3,arg4,cosphi,cosphiError,termt;
  int bin1,bin2,bin3,bin4,NumTriplets=0;

  if (Index1==-1 || Index2==-1 || Index3==-1) return 0;

  StHbtParticleCollection *partColl1 = picoEvent->FirstSectoredCollection()[Index1];
  StHbtParticleCollection *partColl2 = picoEvent->FirstSectoredCollection()[Index2];
  StHbtParticleCollection *partColl3 = picoEvent->FirstSectoredCollection()[Index3];

  if (!partColl1->size() || !partColl2->size() || !partColl2->size()) return 0;

  StHbtTriplet* TheTriplet = new StHbtTriplet;
 
  StHbtParticleIterator PartIter1;
  StHbtParticleIterator PartIter2;
  StHbtParticleIterator PartIter3;
  StHbtParticleIterator StartOuterLoop = partColl1->begin();
  StHbtParticleIterator EndOuterLoop   = partColl1->end();    
  StHbtParticleIterator StartMiddleLoop = partColl2->begin();
  StHbtParticleIterator EndMiddleLoop = partColl2->end();
  StHbtParticleIterator StartInnerLoop = partColl3->begin();  
  StHbtParticleIterator EndInnerLoop = partColl3->end();
  
  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
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
	  NumTriplets++;
	  bin1 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv12());
	  bin2 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv23());
	  bin3 = mQ2CF->GetXaxis()->FindBin(TheTriplet->qInv31());
	  bin4 = mQ3CF->GetXaxis()->FindBin(TheTriplet->qInv());
	  
	  C2Q12 = mQ2CF->GetBinContent(bin1);
	  C2Q23 = mQ2CF->GetBinContent(bin2);
	  C2Q31 = mQ2CF->GetBinContent(bin3);
	  C3    = mNormFactor*(mQ3CF->GetBinContent(bin4));
	  
	  termt  = ::sqrt(fabs((C2Q12-1.0)*(C2Q23-1.0)*(C2Q31-1.0)));
	  if (termt) {
	    cosphi = ((C3-1.0) - (C2Q12-1.0) - (C2Q23-1.0) - (C2Q31-1.0))/(2.0*termt);
	    arg1 = mQ3CF->GetBinError(bin4);
	    arg2 = mQ2CF->GetBinError(bin1)*(1.0 + (C2Q23-1.0)*(C2Q31-1.0)*cosphi/termt);
	    arg3 = mQ2CF->GetBinError(bin2)*(1.0 + (C2Q31-1.0)*(C2Q12-1.0)*cosphi/termt);
	    arg4 = mQ2CF->GetBinError(bin3)*(1.0 + (C2Q12-1.0)*(C2Q23-1.0)*cosphi/termt);
	    cosphiError = (1.0/(2.0*termt))*::sqrt(arg1*arg1+arg2*arg2+arg3*arg3+arg4*arg4);
	  }
	  else {
	    cosphi = 0.0;
	    cosphiError = 0.0;
	  }
	  
	  mCosPhi->AddBinContent(bin4, cosphi);
	  mCosPhiE->AddBinContent(bin4, cosphiError);
	  mCosPhiN->AddBinContent(bin4);
	  
	}  // if passed Triplet cut
      }  // loop over third particle
    }    // loop over second particle
  }      // loop over first particle
  
  delete TheTriplet;
  
  return NumTriplets;

}

//_________________________
int StHbtThreeParticleAnalysis::CreateMixedTriplets(StHbtParticleCollection *partColl1, StHbtParticleCollection *partColl2, StHbtParticleCollection *partColl3) {

  int NumTriplets=0;
  
  if (!partColl1->size() || !partColl1->size() || !partColl1->size()) return 0;

  StHbtTriplet* TheTriplet = new StHbtTriplet;

  StHbtParticleIterator PartIter1;
  StHbtParticleIterator PartIter2;
  StHbtParticleIterator PartIter3;
  StHbtCorrFctnIterator CorrFctnIter;
  StHbtParticleIterator StartOuterLoop = partColl1->begin();
  StHbtParticleIterator EndOuterLoop   = partColl1->end();    
  StHbtParticleIterator StartMiddleLoop = partColl2->begin();
  StHbtParticleIterator EndMiddleLoop = partColl2->end();
  StHbtParticleIterator StartInnerLoop = partColl3->begin();  
  StHbtParticleIterator EndInnerLoop = partColl3->end();
  
  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
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
	  NumTriplets++;
	  for (CorrFctnIter=mCorrFctnCollection->begin();
	       CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	    StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	    CorrFctn->AddMixedTriplet(TheTriplet);
	  }
	}  // if passed Triplet cut
      }  // loop over third particle
    }    // loop over second particle
  }      // loop over first particle

  delete TheTriplet;

  return NumTriplets;

}
 
//_________________________
void StHbtThreeParticleAnalysis::EventBegin(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  mThirdParticleCut->EventBegin(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventBegin(ev);
  }
}
//_________________________
void StHbtThreeParticleAnalysis::EventEnd(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  mThirdParticleCut->EventBegin(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventEnd(ev);
  }
}
//_________________________
void StHbtThreeParticleAnalysis::Finish(){
  if (mCalcCosPhi) {

    mCosPhiE->Divide(mCosPhiN);
    mCosPhi->Divide(mCosPhiN);
    for (int Iter1=1; Iter1<=mCosPhiN->GetNbinsX();Iter1++) 
      mCosPhi->SetBinError(Iter1, mCosPhiE->GetBinContent(Iter1));

    cout << "Writing to file: CosPhi" << endl;
    TFile cosfile(mSaveFile, "NEW");
    mCosPhi->Write("cosphi");
    mCosPhiN->Write("cosphiN");
    cosfile.Close();
    cout << "Done writing: CosPhi" << endl;
  }
  StHbtCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->Finish();
  }
}

//_________________________
void StHbtThreeParticleAnalysis::AddEventProcessed() {
  mNeventsProcessed++;
}

