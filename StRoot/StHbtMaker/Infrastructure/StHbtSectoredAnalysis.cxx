/***************************************************************************
 *
 * $Id: StHbtSectoredAnalysis.cxx,v 1.3 2000/08/11 16:35:41 rcwells Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *  
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *      This is a derived class for two-particle analysis objects.  
 *      In this analysis, momentum space is sectored into a number of
 *      cubes, which are then mixed with nearest neighbors.  Several
 *      initializations are needed to set up the sectoring, see 
 *      StHbtSectoredExample.C in the /doc directory for use.
 *
 ***************************************************************************
 *
 * $Log: StHbtSectoredAnalysis.cxx,v $
 * Revision 1.3  2000/08/11 16:35:41  rcwells
 * Added number of events processed to each HBT analysis
 *
 * Revision 1.2  2000/07/16 21:38:23  laue
 * StHbtCoulomb.cxx StHbtSectoredAnalysis.cxx : updated for standalone version
 * StHbtV0.cc StHbtV0.hh : some cast to prevent compiling warnings
 * StHbtParticle.cc StHbtParticle.hh : pointers mTrack,mV0 initialized to 0
 * StHbtIOBinary.cc : some printouts in #ifdef STHBTDEBUG
 * StHbtEvent.cc : B-Field set to 0.25Tesla, we have to think about a better
 *                 solution
 *
 * Revision 1.1  2000/04/12 01:46:18  willson
 * Initial Installation
 *
 * 
 ***************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtSectoredAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"

#ifdef __ROOT__ 
ClassImp(StHbtSectoredAnalysis)
#endif

//This function gives the correct linear transformation from a 3-D coordinate to a 1-D array index. 

//____________________________
int StHbtSectoredAnalysis::Index(int binx, int biny, int binz)
{
  return binx + biny*mNumBinsX + binz*mNumBinsX*mNumBinsY;
}



//This function sorts the event into a number of sectors.  The overflow is the last array element.

//____________________________
void StHbtSectoredAnalysis::SortHbtParticleCollection(StHbtParticleCut*         partCut,
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

      for (pIter=startLoop;pIter!=endLoop;pIter++){
	pParticle = *pIter;
	bool tmpPassParticle = pCut->Pass(pParticle);
	pCut->FillCutMonitor(pParticle, tmpPassParticle);
	if (tmpPassParticle){
	  StHbtParticle* particle = new StHbtParticle(pParticle,pCut->Mass());
	  if (particle->FourMomentum().px()<mPXmin || particle->FourMomentum().px()>mPXmax ||
	      particle->FourMomentum().py()<mPYmin || particle->FourMomentum().py()>mPYmax ||
	      particle->FourMomentum().pz()<mPZmin || particle->FourMomentum().pz()>mPZmax)
	    SectoredPicoEvent[mNumBinsX*mNumBinsY*mNumBinsZ]->push_back(particle);
	  else {
	    i = (int) floor((particle->FourMomentum().px()-mPXmin)/mDeltaP);
	    j = (int) floor((particle->FourMomentum().py()-mPYmin)/mDeltaP);
	    k = (int) floor((particle->FourMomentum().pz()-mPZmin)/mDeltaP);
	    SectoredPicoEvent[Index(i,j,k)]->push_back(particle);
	  }
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
      // this following "for" loop is identical to the one above, but because of scoping, I can's see how to avoid repetition...
      
      for (pIter=startLoop;pIter!=endLoop;pIter++){
	pParticle = *pIter;
	bool tmpPassV0 = pCut->Pass(pParticle);
	pCut->FillCutMonitor(pParticle, tmpPassV0);
	if (tmpPassV0){
	  StHbtParticle* particle = new StHbtParticle(pParticle,pCut->Mass());
	  if (particle->FourMomentum().px()<mPXmin || particle->FourMomentum().px()>mPXmax ||
	      particle->FourMomentum().py()<mPYmin || particle->FourMomentum().py()>mPYmax ||
	      particle->FourMomentum().pz()<mPZmin || particle->FourMomentum().pz()>mPZmax)
	    SectoredPicoEvent[mNumBinsX*mNumBinsY*mNumBinsZ]->push_back(particle);
	  else {
	    i = (int) floor((particle->FourMomentum().px()-mPXmin)/mDeltaP);
	    j = (int) floor((particle->FourMomentum().py()-mPYmin)/mDeltaP);
	    k = (int) floor((particle->FourMomentum().pz()-mPZmin)/mDeltaP);
	    SectoredPicoEvent[Index(i,j,k)]->push_back(particle);
	  }
	}
      }
      break;
    }
  default:
    cout << "FillHbtParticleCollection function (in StHbtSectoredAnalysis.cxx) - undefined Particle Cut type!!! \n";
  } 
}


//____________________________
StHbtSectoredAnalysis::StHbtSectoredAnalysis(){
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mNeventsProcessed = 0;
  // Defaults for sectoring
  mPXmax = 5.0;
  mPXmin = -5.0;
  mPYmax = 5.0;
  mPYmin = -5.0;
  mPZmax = 5.0;
  mPZmin = -5.0;
  mDeltaP = 10.0;
  mNumBinsX = 1;
  mNumBinsY = 1;
  mNumBinsZ = 1;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mSectoredMixingBuffer = new StHbtSectoredPicoEventCollection;
}

//____________________________
StHbtSectoredAnalysis::StHbtSectoredAnalysis(const StHbtSectoredAnalysis& a) : StHbtBaseAnalysis() {
  //  mControlSwitch     = 0;
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mNeventsProcessed = 0;
  mPXmax = 5.0;
  mPXmin = -5.0;
  mPYmax = 5.0;
  mPYmin = -5.0;
  mPZmax = 5.0;
  mPZmin = -5.0;
  mDeltaP = 10.0;
  mNumBinsX = 1;
  mNumBinsY = 1;
  mNumBinsZ = 1;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mSectoredMixingBuffer = new StHbtSectoredPicoEventCollection;
  
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
      cout << " StHbtAnalysis::StHbtAnalysis(const StHbtAnalysis& a) - event cut set " << endl;
  }
  if ( mFirstParticleCut ) {
      SetFirstParticleCut(mFirstParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtAnalysis::StHbtAnalysis(const StHbtAnalysis& a) - first particle cut set " << endl;
  }
  if ( mSecondParticleCut ) {
      SetSecondParticleCut(mSecondParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtAnalysis::StHbtAnalysis(const StHbtAnalysis& a) - second particle cut set " << endl;
  }  if ( mPairCut ) {
      SetPairCut(mPairCut); // this will set the myAnalysis pointer inside the cut
      cout << " StHbtAnalysis::StHbtAnalysis(const StHbtAnalysis& a) - pair cut set " << endl;
  }

  StHbtCorrFctnIterator iter;
  for (iter=a.mCorrFctnCollection->begin(); iter!=a.mCorrFctnCollection->end();iter++){
    cout << " StHbtAnalysis::StHbtAnalysis(const StHbtAnalysis& a) - looking for correlation functions " << endl;
    StHbtCorrFctn* fctn = (*iter)->Clone();
    if (fctn) AddCorrFctn(fctn);
    else cout << " StHbtAnalysis::StHbtAnalysis(const StHbtAnalysis& a) - correlation function not found " << endl;
  }

  mNumEventsToMix = a.mNumEventsToMix;

  cout << " StHbtSectoredAnalysis::StHbtSectoredAnalysis(const StHbtSectoredAnalysis& a) - analysis copied " << endl;
  
}

//____________________________
StHbtSectoredAnalysis::~StHbtSectoredAnalysis(){
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
  StHbtSectoredPicoEventIterator piter;
  for (piter=mSectoredMixingBuffer->begin();piter!=mSectoredMixingBuffer->end();piter++){
    delete *piter;
  }
  delete mSectoredMixingBuffer;
  
}
//______________________
StHbtCorrFctn* StHbtSectoredAnalysis::CorrFctn(int n){  // return pointer to n-th correlation function
  if ( n<0 || n > (int)mCorrFctnCollection->size() )
    return NULL;
  StHbtCorrFctnIterator iter=mCorrFctnCollection->begin();
  for (int i=0; i<n ;i++){
    iter++;
  }
  return *iter;
}
//____________________________
StHbtString StHbtSectoredAnalysis::Report()
{
  char Sect[100];
  sprintf(Sect, "Sector bounds:  %4.2f<px<%4.2f %4.2f<py<%4.2f %4.2f<pz<%4.2f, DeltaP=%4.2f", mPXmin, mPXmax, mPYmin, mPYmax, mPZmin, mPZmax, mDeltaP);
  cout << "StHbtSectoredAnalysis - constructing Report..."<<endl;
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
    cout << "StHbtSectoredAnalysis-Warning : no correlations functions in this analysis " << endl;
  }
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    temp += (*iter)->Report();
    temp += "\n";
  }
  temp += "\nSectoring Information:\n";
  temp += Sect;
  temp += "\n-------------\n";
  StHbtString returnThis=temp;
  return returnThis;
}
//_________________________
void StHbtSectoredAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {

  // Add event to processed events
  AddEventProcessed();

  int i, j, k, i2, j2, k2;   // used in loops over sectors
  int size1=0, size2=0;      // To get the total size of all sectors
  bool DidOverflow;          // To only count the overflow bin once per sector
   
  // startup for EbyE 
  EventBegin(hbtEvent);  
  // event cut and event cut monitor
  bool tmpPassEvent = mEventCut->Pass(hbtEvent);
  mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
  if (tmpPassEvent) {
    cout << "StHbtSectoredAnalysis::ProcessEvent() - Event has passed cut - build sectoredpicoEvent from " <<
      hbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
    // OK, analysis likes the event-- build a sectored pico event from it, using tracks the analysis likes...
    
    // this is what we will make Pairs from and put in Mixing Buffer
    StHbtSectoredPicoEvent* picoEvent = new StHbtSectoredPicoEvent(mNumBinsX, mNumBinsY, mNumBinsZ);     
    
    SortHbtParticleCollection(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstSectoredCollection());
    if ( !(AnalyzeIdenticalParticles()) ) 
      SortHbtParticleCollection(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondSectoredCollection());
    
    //Obtain combined size of all sectors
    
    for (i=0; i<mNumBinsX; i++) 
      for (j=0; j<mNumBinsY; j++) 
	for (k=0; k<mNumBinsZ; k++) {
	  size1 += picoEvent->FirstSectoredCollection()[Index(i,j,k)]->size();
	  if ( !(AnalyzeIdenticalParticles()) ) 
	    size2 += picoEvent->SecondSectoredCollection()[Index(i,j,k)]->size();
	}
    
    cout <<"StHbtSectoredAnalysis::ProcessEvent - #particles in First, Second Collections: " <<
      size1 << " " << size2 << endl;
    
    if (AnalyzeIdenticalParticles()) {
      // Real Pairs--Identical Particles
      for (i=0; i<mNumBinsX; i++) 
	for (j=0; j<mNumBinsY; j++)
	  for (k=0; k<mNumBinsZ; k++) {
	    DidOverflow=false;
	    
	    // A sector sweep of a single sectoredpicoevent - 9 terms with i+1, 3 term with i, j+1, 
	    // and 1 term with i, j, k+1.  Plus another term for mixing a sector with itself.

	    //The 9 terms
	      for (j2=j-1; j2<=j+1; j2++)
		for (k2=k-1; k2<=k+1; k2++) {
		  if (j2<mNumBinsY && k2<mNumBinsZ && j2!=-1 && k2!=-1 && i+1<mNumBinsX) {
		    CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], picoEvent->FirstSectoredCollection()[Index(i+1,j2,k2)]);
		  }
		  else 
		    if (!DidOverflow) {
		      CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
		      DidOverflow=true;
		    }
		}

	      //The 3 terms
	      for (k2=k-1; k2<=k+1; k2++) {
		if (k2<mNumBinsZ && k2!=-1 && j+1<mNumBinsY) {
		  CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], picoEvent->FirstSectoredCollection()[Index(i,j+1,k2)]);
		}
		else 
		  if (!DidOverflow) {
		    CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
		    DidOverflow=true;
		  }
	      }

	      //The 1 term
	      if (k+1<mNumBinsZ) {
		CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], picoEvent->FirstSectoredCollection()[Index(i,j,k+1)]);
	      }
	      else 
		if (!DidOverflow) {
		  CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
		  DidOverflow=true;
		}
	            
	      //Mix sector with itself
	      CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)]);
	  }
      //Mix overflow bin with itself
      CreateRealPairs(picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
	  
    }  
    
    else {
      // Real Pairs--Non-Identical Particles
      for (i=0; i<mNumBinsX; i++) 
	for (j=0; j<mNumBinsY; j++)
	  for (k=0; k<mNumBinsZ; k++) {
	    DidOverflow=false;
	    for (i2=i-1; i2<=i+1; i2++)
	      for (j2=j-1; j2<=j+1; j2++)
		for (k2=k-1; k2<=k+1; k2++)
		  if (i2<mNumBinsX && j2<mNumBinsY && k2<mNumBinsZ && i2!=-1 && j2!=-1 && k2!=-1)
		    CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], picoEvent->SecondSectoredCollection()[Index(i2,j2,k2)]);
		  else 
		    if (!DidOverflow) {
		      CreateRealPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], picoEvent->SecondSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
		      DidOverflow=true;
		    }
	  }
    }
    //Need to mix with the overflow bin of FirstSectoredCollection:
    for (i=0; i<mNumBinsX; i++) 
      for (j=0; j<mNumBinsY; j++)
	for (k=0; k<mNumBinsZ; k++)
	  if (i==0 || j==0 || k==0 || i==mNumBinsX-1 || j==mNumBinsY-1 || k==mNumBinsZ-1)
	    CreateRealPairs(picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ], picoEvent->SecondSectoredCollection()[Index(i,j,k)]);

    //Need to mix both overflow bins
    CreateRealPairs(picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ], picoEvent->SecondSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
    
    
    cout << "StHbtSectoredAnalysis::ProcessEvent() - reals done, " << endl; 
    
    // ok, now make mixed Pairs, if the Mixing buffer is full
    
    if (SectoredMixingBufferFull()){
      cout << "Mixing Buffer is full - lets rock and roll" << endl;
    }
    else {
      cout << "Mixing Buffer not full -gotta wait " << SectoredMixingBuffer()->size() << endl;
    }
    if (SectoredMixingBufferFull()){
      StHbtSectoredPicoEvent* storedEvent;
      StHbtSectoredPicoEventIterator picoEventIter;
      for (picoEventIter=SectoredMixingBuffer()->begin();picoEventIter!=SectoredMixingBuffer()->end();picoEventIter++){
	storedEvent = *picoEventIter;
	if (AnalyzeIdenticalParticles()){
	  // Mixed Pairs--Identical Particles
	  for (i=0; i<mNumBinsX; i++) 
	    for (j=0; j<mNumBinsY; j++)
	      for (k=0; k<mNumBinsZ; k++) {
		DidOverflow=false;
		for (i2=i-1; i2<=i+1; i2++)
		  for (j2=j-1; j2<=j+1; j2++)
		    for (k2=k-1; k2<=k+1; k2++)
		      if (i2<mNumBinsX && j2<mNumBinsY && k2<mNumBinsZ && i2!=-1 && j2!=-1 && k2!=-1) {
			CreateMixedPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], storedEvent->FirstSectoredCollection()[Index(i2,j2,k2)]);
		      }
		      else 
			if (!DidOverflow) {
			  CreateMixedPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], storedEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
			  DidOverflow=true;
			}
	      }
	  
	  //Need to mix with the overflow bin of FirstSectoredCollection:
	  for (i=0; i<mNumBinsX; i++) 
	    for (j=0; j<mNumBinsY; j++)
	      for (k=0; k<mNumBinsZ; k++)
		if (i==0 || j==0 || k==0 || i==mNumBinsX-1 || j==mNumBinsY-1 || k==mNumBinsZ-1){
		  CreateMixedPairs(picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ], storedEvent->FirstSectoredCollection()[Index(i,j,k)]);
		}

	  //Need to mix the overflow bin of picoEvent with the overflow bin of storedEvent
	  CreateMixedPairs(picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ], storedEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
	  
	}
	  
	else{
	  // Mixed Pairs--Non-Identical Particles
	  for (i=0; i<mNumBinsX; i++) 
	    for (j=0; j<mNumBinsY; j++)
	      for (k=0; k<mNumBinsZ; k++) {
		DidOverflow=false;
		for (i2=i-1; i2<=i+1; i2++)
		  for (j2=j-1; j2<=j+1; j2++)
		    for (k2=k-1; k2<=k+1; k2++)
		      if (i2<mNumBinsX && j2<mNumBinsY && k2<mNumBinsZ && i2!=-1 && j2!=-1 && k2!=-1)
			CreateMixedPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], storedEvent->SecondSectoredCollection()[Index(i2,j2,k2)]);	    
		      else 
			if (!DidOverflow) {
			  CreateMixedPairs(picoEvent->FirstSectoredCollection()[Index(i,j,k)], storedEvent->SecondSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
			  DidOverflow=true;
			}
	      }
	  //Need to mix with the overflow bin of FirstSectoredCollection:
	  for (i=0; i<mNumBinsX; i++) 
	    for (j=0; j<mNumBinsY; j++)
	      for (k=0; k<mNumBinsZ; k++)
		if (i==0 || j==0 || k==0 || i==mNumBinsX-1 || j==mNumBinsY-1 || k==mNumBinsZ-1)
		  CreateMixedPairs(picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ], storedEvent->SecondSectoredCollection()[Index(i,j,k)]);
	  
	  //Need to mix the overflow bin of picoEvent with the overflow bin of storedEvent
	  CreateMixedPairs(picoEvent->FirstSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ], storedEvent->SecondSectoredCollection()[mNumBinsX*mNumBinsY*mNumBinsZ]);
	 
	}
      }
      
      // Now get rid of oldest stored pico-event in buffer.
      // This means (1) delete the event from memory, (2) "pop" the pointer to it from the MixingBuffer
      picoEventIter = SectoredMixingBuffer()->end();
      picoEventIter--;   // bug fixed malisa 27jul99 - end() is one BEYOND the end! (besides crashing on linux, this was a memory leak)
      delete *picoEventIter;
      
      SectoredMixingBuffer()->pop_back();
    }  // if mixing buffer is full
    SectoredMixingBuffer()->push_front(picoEvent);  // store the current pico-event in buffer
  }    // if currentEvent is accepted by currentAnalysis
  EventEnd(hbtEvent);  // cleanup for EbyE 
  cout << "StHbtSectoredAnalysis::ProcessEvent() - return to caller ...  " << endl; 
    
}

      
//_________________________
void StHbtSectoredAnalysis::CreateRealPairs(StHbtParticleCollection *partColl1) {

  if (partColl1->size()<2) return;

  StHbtPair* ThePair = new StHbtPair;
  
  StHbtParticleIterator PartIter1;
  StHbtParticleIterator PartIter2;
  StHbtCorrFctnIterator CorrFctnIter;
  StHbtParticleIterator StartOuterLoop = partColl1->begin();  // always
  StHbtParticleIterator EndOuterLoop   = partColl1->end();    // will be one less
  StHbtParticleIterator StartInnerLoop;
  StHbtParticleIterator EndInnerLoop;
	
  EndOuterLoop--;                     // outer loop goes to next-to-last particle in collection
  EndInnerLoop = partColl1->end() ; 

  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
    StartInnerLoop = PartIter1;
    StartInnerLoop++;
    ThePair->SetTrack1(*PartIter1);
    for (PartIter2 = StartInnerLoop; PartIter2!=EndInnerLoop;PartIter2++){
      ThePair->SetTrack2(*PartIter2);
      // The following lines have to be uncommented if you want PairCutMonitors
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
      }  // if passed Pair cut
    }    // loop over second particle
  }      // loop over first particle   
  
  delete ThePair;
 
}

//_________________________
void StHbtSectoredAnalysis::CreateRealPairs(StHbtParticleCollection *partColl1, StHbtParticleCollection *partColl2) {

  if (partColl1->size()<1 || partColl2->size()<1) return;

  StHbtPair* ThePair = new StHbtPair;
	
  // We are taking one particle from each of the two collections

  StHbtParticleIterator PartIter1;
  StHbtParticleIterator PartIter2;
  StHbtCorrFctnIterator CorrFctnIter;
  StHbtParticleIterator StartOuterLoop = partColl1->begin(); 
  StHbtParticleIterator EndOuterLoop   = partColl1->end();    
  StHbtParticleIterator StartInnerLoop = partColl2->begin();  
  StHbtParticleIterator EndInnerLoop   = partColl2->end();
  
  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
    ThePair->SetTrack1(*PartIter1);
    for (PartIter2 = StartInnerLoop; PartIter2!=EndInnerLoop;PartIter2++){
      ThePair->SetTrack2(*PartIter2);
      // The following lines have to be uncommented if you want PairCutMonitors
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
      }  // if passed Pair cut
    }    // loop over second particle
  }      // loop over first particle
  
  delete ThePair;

}

//_________________________
void StHbtSectoredAnalysis::CreateMixedPairs(StHbtParticleCollection *partColl1, StHbtParticleCollection *partColl2) {

  if (partColl1->size()<1 || partColl2->size()<1) return;

  StHbtPair* ThePair = new StHbtPair;
	
  // We are taking one particle from each of the two collections

  StHbtParticleIterator PartIter1;
  StHbtParticleIterator PartIter2;
  StHbtCorrFctnIterator CorrFctnIter;
  StHbtParticleIterator StartOuterLoop = partColl1->begin(); 
  StHbtParticleIterator EndOuterLoop   = partColl1->end();    
  StHbtParticleIterator StartInnerLoop = partColl2->begin();  
  StHbtParticleIterator EndInnerLoop   = partColl2->end();
  
  for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
    ThePair->SetTrack1(*PartIter1);
    for (PartIter2 = StartInnerLoop; PartIter2!=EndInnerLoop;PartIter2++){
      ThePair->SetTrack2(*PartIter2);
      // The following lines have to be uncommented if you want PairCutMonitors
      // they are not in for speed reasons
      // bool tmpPassPair = mPairCut->Pass(ThePair);
      // mPairCut->FillCutMonitor(ThePair, tmpPassPair);
      // if ( tmpPassPair ) {
      if (mPairCut->Pass(ThePair)){
	for (CorrFctnIter=mCorrFctnCollection->begin();
	     CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	  StHbtCorrFctn* CorrFctn = *CorrFctnIter;
	  CorrFctn->AddMixedPair(ThePair);
	}
      }  // if passed Pair cut
    }    // loop over second particle
  }      // loop over first particle
  
  delete ThePair;

}
 
//_________________________
void StHbtSectoredAnalysis::EventBegin(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  mPairCut->EventBegin(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventBegin(ev);
  }
}
//_________________________
void StHbtSectoredAnalysis::EventEnd(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  mPairCut->EventEnd(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventEnd(ev);
  }
}
//_________________________
void StHbtSectoredAnalysis::Finish(){
  StHbtCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->Finish();
  }
}
//_________________________
void StHbtSectoredAnalysis::AddEventProcessed() {
  mNeventsProcessed++;
}


