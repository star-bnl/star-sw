#include "StHbtMaker/Infrastructure/StParityAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"

#ifdef __ROOT__ 
ClassImp(StParityAnalysis)
#endif

StHbtEventCut*    copyTheCut(StHbtEventCut*);
StHbtParticleCut* copyTheCut(StHbtParticleCut*);
StHbtPairCut*     copyTheCut(StHbtPairCut*);
StHbtCorrFctn*    copyTheCorrFctn(StHbtCorrFctn*);

// this little function used to apply ParticleCuts (TrackCuts or V0Cuts) and fill ParticleCollections of picoEvent
//  it is called from StParityAnalysis::ProcessEvent()
void FillHbtParticleCollection2(StHbtParticleCut*         partCut,
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
	bool tmpPassParticle = pCut->Pass(pParticle);
	pCut->FillCutMonitor(pParticle, tmpPassParticle);
	if (tmpPassParticle){
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
	bool tmpPassV0 = pCut->Pass(pParticle);
	pCut->FillCutMonitor(pParticle,tmpPassV0);
	if (tmpPassV0){
	  StHbtParticle* particle = new StHbtParticle(pParticle,partCut->Mass());
	  partCollection->push_back(particle);
	}
      }
      break;
    }
  default:
    cout << "FillHbtParticleCollection2 function (in StParityAnalysis.cxx) - undefined Particle Cut type!!! \n";
  }
}
//____________________________
StParityAnalysis::StParityAnalysis(){
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mNeventsProcessed = 0;
}
//____________________________

StParityAnalysis::StParityAnalysis(const StParityAnalysis& a) : StHbtBaseAnalysis() {
  //StParityAnalysis();
  mEventCut          = 0;
  mFirstParticleCut  = 0;
  mSecondParticleCut = 0;
  mPairCut           = 0;
  mCorrFctnCollection= 0;
  mCorrFctnCollection = new StHbtCorrFctnCollection;
  mNeventsProcessed = 0;

  // find the right event cut
  mEventCut = a.mEventCut->Clone();
  // find the right first particle cut
  mFirstParticleCut = a.mFirstParticleCut->Clone();
  // find the right second particle cut
  if (a.mFirstParticleCut==a.mSecondParticleCut!=0) 
    SetSecondParticleCut(mSecondParticleCut); // identical particle hbt
  else
  mSecondParticleCut = a.mSecondParticleCut->Clone();

  mPairCut = a.mPairCut->Clone();
  
  if ( mEventCut ) {
      SetEventCut(mEventCut); // this will set the myAnalysis pointer inside the cut
      cout << " StParityAnalysis::StParityAnalysis(const StParityAnalysis& a) - event cut set " << endl;
  }
  if ( mFirstParticleCut ) {
      SetFirstParticleCut(mFirstParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StParityAnalysis::StParityAnalysis(const StParityAnalysis& a) - first particle cut set " << endl;
  }
  if ( mSecondParticleCut ) {
      SetSecondParticleCut(mSecondParticleCut); // this will set the myAnalysis pointer inside the cut
      cout << " StParityAnalysis::StParityAnalysis(const StParityAnalysis& a) - second particle cut set " << endl;
  }  if ( mPairCut ) {
      SetPairCut(mPairCut); // this will set the myAnalysis pointer inside the cut
      cout << " StParityAnalysis::StParityAnalysis(const StParityAnalysis& a) - pair cut set " << endl;
  }

  StHbtCorrFctnIterator iter;
  for (iter=a.mCorrFctnCollection->begin(); iter!=a.mCorrFctnCollection->end();iter++){
    cout << " StParityAnalysis::StParityAnalysis(const StParityAnalysis& a) - looking for correlation functions " << endl;
    StHbtCorrFctn* fctn = (*iter)->Clone();
    if (fctn) AddCorrFctn(fctn);
    else cout << " StParityAnalysis::StParityAnalysis(const StParityAnalysis& a) - correlation function not found " << endl;
  }

  mNumEventsToMix = a.mNumEventsToMix;

  cout << " StParityAnalysis::StParityAnalysis(const StParityAnalysis& a) - analysis copied " << endl;
}

//____________________________
StParityAnalysis::~StParityAnalysis(){
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
}
//______________________
StHbtCorrFctn* StParityAnalysis::CorrFctn(int n){  // return pointer to n-th correlation function
  if ( n<0 || n > (int)mCorrFctnCollection->size() )
    return NULL;
  StHbtCorrFctnIterator iter=mCorrFctnCollection->begin();
  for (int i=0; i<n ;i++){
    iter++;
  }
  return *iter;
}
//____________________________
StHbtString StParityAnalysis::Report()
{
  cout << "StParityAnalysis - constructing Report..."<<endl;
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
    cout << "StParityAnalysis-Warning : no correlations functions in this analysis " << endl;
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
void StParityAnalysis::ProcessEvent(const StHbtEvent* hbtEvent) {

  // Add event to processed events
  AddEventProcessed();

  ParityBuff PlusSame;
  ParityBuff MinusSame;
  ParityBuff PlusMixed;
  ParityBuff MinusMixed;
  #define BUFFERSIZ 50 
  static ParityBuff PlusBuffer[BUFFERSIZ];
  static ParityBuff MinusBuffer[BUFFERSIZ];
  static long nEvent = 0;
  StHbtParticle  *pParticle;

  EventBegin(hbtEvent);  
  // event cut and event cut monitor
  bool tmpPassEvent = mEventCut->Pass(hbtEvent);
  mEventCut->FillCutMonitor(hbtEvent, tmpPassEvent);
  if (tmpPassEvent) {
    cout << "StParityAnalysis::ProcessEvent() - Event has passed cut - build picoEvent from " <<
      hbtEvent->TrackCollection()->size() << " tracks in TrackCollection" << endl;
    // OK, analysis likes the event-- build a pico event from it, using tracks the analysis likes...
    StHbtPicoEvent* picoEvent = new StHbtPicoEvent;       // this is what we will make pairs from and put in Mixing Buffer
    FillHbtParticleCollection2(mFirstParticleCut,(StHbtEvent*)hbtEvent,picoEvent->FirstParticleCollection());
     FillHbtParticleCollection2(mSecondParticleCut,(StHbtEvent*)hbtEvent,picoEvent->SecondParticleCollection());
    cout <<"StParityAnalysis::ProcessEvent - #particles in First, Second Collections: " <<
      picoEvent->FirstParticleCollection()->size() << " " <<
      picoEvent->SecondParticleCollection()->size() << endl;
      
    // OK, pico event is built
    // Now copy it into the Parity Mixing Buffer...
      
    StHbtParticleIterator PartIter1;
    StHbtParticleIterator PartIter2;
    StHbtParticleIterator StartOuterLoop = picoEvent->FirstParticleCollection()->begin(); 
    StHbtParticleIterator EndOuterLoop   = picoEvent->FirstParticleCollection()->end();    
    StHbtParticleIterator StartInnerLoop;
    StHbtParticleIterator EndInnerLoop;
    StartInnerLoop = picoEvent->SecondParticleCollection()->begin(); // inner loop starts at first particle in Second collection
    EndInnerLoop   = picoEvent->SecondParticleCollection()->end() ;  // inner loop goes to last particle in Second collection

    // Begin Parity  ************************************************************

    int evtMod = (nEvent%BUFFERSIZ);
    nEvent++;
    int bufferIndex = evtMod;          // if we haven't filled the buffer yet, we continue filling it
    if (nEvent  >= BUFFERSIZ ) {                    // if it is full, we just keep filling and destroying the
      bufferIndex = (BUFFERSIZ - 1);           // final event in the buffer  (after mixing it, of course)
    }

    //COPY JUST 4-VECTORS FROM PICO-EVENT INTO MIXING BUFFER

      PlusBuffer[bufferIndex].clear();

    for (PartIter1=StartOuterLoop;PartIter1!=EndOuterLoop;PartIter1++){
        pParticle = *PartIter1;
        PlusBuffer[bufferIndex].push_back(pParticle->FourMomentum() ) ;
    }
    int newPlusSize =  PlusBuffer[bufferIndex].size();
    if (newPlusSize == 0){cout << " 0 tracks copied to buffer for particle 1!!!"<< endl ;}

      MinusBuffer[bufferIndex].clear();

     for (PartIter2 = StartInnerLoop; PartIter2!=EndInnerLoop;PartIter2++){
        pParticle = *PartIter2;
        MinusBuffer[bufferIndex].push_back(pParticle->FourMomentum() ) ;
    }
    int newMinusSize =  MinusBuffer[bufferIndex].size();
    if (newMinusSize == 0){cout << " 0 tracks copied to buffer for particle 2!!!"<< endl ;}

    // DONE COPYING PICO-EVENT

    // CALCULATE the RXN PLANE here-in future we may can just use Flow Maker (?) 

    StHbtThreeVector RxnPlaneVec;
       {for (int iii = 0;iii<newPlusSize;iii++){
	 if (PlusBuffer[bufferIndex][iii].z() > 0 ) RxnPlaneVec += PlusBuffer[bufferIndex][iii].vect(); 
	 if (PlusBuffer[bufferIndex][iii].z() < 0 ) RxnPlaneVec -= PlusBuffer[bufferIndex][iii].vect(); 
       }}
       {for (int iii = 0;iii<newMinusSize;iii++){
	 if (MinusBuffer[bufferIndex][iii].z() > 0 ) RxnPlaneVec += MinusBuffer[bufferIndex][iii].vect(); 
	 if (MinusBuffer[bufferIndex][iii].z() < 0 ) RxnPlaneVec -= MinusBuffer[bufferIndex][iii].vect(); 
       }}
       RxnPlaneVec.setZ(0);
       double phi_rxn_plane = acos(RxnPlaneVec.x()/RxnPlaneVec.mag());
       if (RxnPlaneVec.y() < 0) phi_rxn_plane = -phi_rxn_plane;

       // now rotate rxn plane to be at x-axis 
       //this could be combined with some other loop but for now we'll leave it here for simplicity
       StHbtThreeVector tempVct; // unfortunately for now need to use this to satisfy 
                                 // compiler (to avoid rotating const vector)
       {for (int iii = 0;iii<newPlusSize;iii++){
	 //	 PlusBuffer[bufferIndex][iii].vect().rotateZ(-phi_rxn_plane);
	  tempVct = PlusBuffer[bufferIndex][iii].vect();
	 tempVct.rotateZ(-phi_rxn_plane);
	 (PlusBuffer[bufferIndex][iii]).setX(tempVct.x()); 
	 (PlusBuffer[bufferIndex][iii]).setY(tempVct.y()); 

       }}
       {for (int iii = 0;iii<newMinusSize;iii++){
	 // MinusBuffer[bufferIndex][iii].vect().rotateZ(-phi_rxn_plane);
		 tempVct = MinusBuffer[bufferIndex][iii].vect();
		 tempVct.rotateZ(-phi_rxn_plane);
		 (MinusBuffer[bufferIndex][iii]).setX(tempVct.x()); 
		 (MinusBuffer[bufferIndex][iii]).setY(tempVct.y()); 

       }}
    // DONE w/ RXN PLANE calculation

    // now RANDOMIZE the BUFFERS (should in the future rotate the rxn plane here as well)

    {for (int iii = 0;iii<2;iii++){                             // do 2 shuffles
       for (int iii = 0;iii<newPlusSize;iii++){
	int switchto = (rand() % newPlusSize);
	StHbtLorentzVector tempVec = PlusBuffer[bufferIndex][iii];
	PlusBuffer[bufferIndex][iii] = PlusBuffer[bufferIndex][switchto];
	PlusBuffer[bufferIndex][switchto] = tempVec;
       }
      }}
    
    {for (int iii = 0;iii<2;iii++){                             // do 2 shuffles
       for (int iii = 0;iii<newMinusSize;iii++){
	int switchto = (rand() % newMinusSize);
	StHbtLorentzVector  tempVec = MinusBuffer[bufferIndex][iii];
	MinusBuffer[bufferIndex][iii] = MinusBuffer[bufferIndex][switchto];
	MinusBuffer[bufferIndex][switchto] = tempVec;
       }
      }}
    // END OF RANDOMIZING TRACK ORDER

    // COPY  latest EVENT to PlusSame and MinusSame vectors
     PlusSame.clear();
     MinusSame.clear();
      {for (int jjj = 0; jjj < newPlusSize; jjj++){
	PlusSame.push_back(PlusBuffer[bufferIndex][jjj]);
      }}     
      {for (int jjj = 0; jjj < newMinusSize; jjj++){
	MinusSame.push_back(MinusBuffer[bufferIndex][jjj]);
      }}     
    // END of COPYing latest event

    // NOW BUILD A MIXED EVENT in the PlusMixed and MinusMixed vectors;

      if (nEvent  >= BUFFERSIZ ) {  // to ensure multiplicity dist is same for SAME and MIXED, don't go
	                                               // any further unless buffer is full
      PlusMixed.clear();
      MinusMixed.clear();

      if (nEvent  >= BUFFERSIZ ) {
	StHbtLorentzVector vTemp;
	int mixedenum;
	int mixedtnum;
	                                                                                                                                   
	// take the newest buffered event and mix it with other events
                   {for (int jjj = 0; jjj < newPlusSize; jjj++){
	    mixedenum = (rand() % BUFFERSIZ);
	    mixedtnum = (rand() % PlusBuffer[mixedenum].size());
	    vTemp = PlusBuffer[mixedenum][mixedtnum];
	    PlusBuffer[mixedenum][mixedtnum] = PlusBuffer[bufferIndex][jjj];
	    PlusBuffer[bufferIndex][jjj] = vTemp;
                    }}		     
                   {for (int jjj = 0; jjj < newMinusSize; jjj++){
	    mixedenum = (rand() % BUFFERSIZ);
	    mixedtnum = (rand() % MinusBuffer[mixedenum].size());
	    vTemp = MinusBuffer[mixedenum][mixedtnum];
	    MinusBuffer[mixedenum][mixedtnum] = MinusBuffer[bufferIndex][jjj];
	    MinusBuffer[bufferIndex][jjj] = vTemp;
                    }}		     
		   // now that the newest buffer is randomized, copy it to mixed event

                   {for (int jjj = 0; jjj < newPlusSize; jjj++){
                     PlusMixed.push_back(PlusBuffer[bufferIndex][jjj]);
                   }}
                   {for (int jjj = 0; jjj < newMinusSize; jjj++){
                     MinusMixed.push_back(MinusBuffer[bufferIndex][jjj]);
                   }}
      }
      // END OF BUILDING MIXED EVENT

      // NOW 'PAIR' CUTS
      
      mPairCut->ParityPairCuts(&PlusSame, &MinusSame);
      if (nEvent  >= BUFFERSIZ ) {
         mPairCut->ParityPairCuts(&PlusMixed, &MinusMixed);
      }
      
      // now force the final multiplicities to be the same by removing tracks from SAME or MIXED event
      //( whichever has more)...

      if (nEvent  >= BUFFERSIZ ) {
	int eraseMe;
	while (PlusMixed.size() > PlusSame.size()){
	     eraseMe = (rand() % PlusMixed.size());
	     PlusMixed.erase(PlusMixed.begin()+eraseMe);
	}
	while (MinusMixed.size() > MinusSame.size() ){
	     eraseMe = (rand() % MinusMixed.size());
	    MinusMixed.erase(MinusMixed.begin()+eraseMe);
	}
	while (PlusSame.size() > PlusMixed.size()){
	     eraseMe = (rand() % PlusSame.size());
	     PlusSame.erase(PlusSame.begin()+eraseMe);
	}
	while (MinusSame.size() > MinusMixed.size() ){
	     eraseMe = (rand() % MinusSame.size());
	    MinusSame.erase(MinusSame.begin()+eraseMe);
	}
      }
     cout  << " sizes:plussame minussame plusmixed minusmixed" << PlusSame.size()<< "," 
	                                                                                                              <<MinusSame.size()<< ","   
	                                                                                                              <<PlusMixed.size()<< ","   
	                                                                                                              <<MinusMixed.size()<< endl   ;

      // NOW WE HAVE the FINAL 'SAME' and 'MIXED' EVENTS, let's DO PARITY CALCULATIONS  
      // loop through the correlation functions

       StHbtCorrFctnIterator CorrFctnIter;
       for (CorrFctnIter=mCorrFctnCollection->begin();CorrFctnIter!=mCorrFctnCollection->end();CorrFctnIter++){
	 StHbtCorrFctn* CorrFctn = *CorrFctnIter;
                   CorrFctn->ParityCompute(&PlusSame, &MinusSame,  SAME );
              if (nEvent  >= BUFFERSIZ ) {
                  CorrFctn->ParityCompute(&PlusMixed, &MinusMixed,  MIXED );
             }

       }

      }
       // END of PARITY CALCULATIONS

    delete picoEvent ;    

  }   // if currentEvent is accepted by currentAnalysis
  EventEnd(hbtEvent);  // cleanup for EbyE 
  cout << "StParityAnalysis::ProcessEvent() - return to caller ... " << endl;
}
//_________________________
void StParityAnalysis::EventBegin(const StHbtEvent* ev){
  mFirstParticleCut->EventBegin(ev);
  mSecondParticleCut->EventBegin(ev);
  //  mPairCut->EventBegin(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventBegin(ev);
  }
}
//_________________________
void StParityAnalysis::EventEnd(const StHbtEvent* ev){
  mFirstParticleCut->EventEnd(ev);
  mSecondParticleCut->EventEnd(ev);
  //  mPairCut->EventEnd(ev);
  for (StHbtCorrFctnIterator iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->EventEnd(ev);
  }
}
//_________________________
void StParityAnalysis::Finish(){
  StHbtCorrFctnIterator iter;
  for (iter=mCorrFctnCollection->begin(); iter!=mCorrFctnCollection->end();iter++){
    (*iter)->Finish();
  }
}
//_________________________
void StParityAnalysis::AddEventProcessed() {
  mNeventsProcessed++;
}
