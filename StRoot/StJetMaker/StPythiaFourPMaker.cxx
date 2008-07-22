//std
#include <iostream>
using namespace std;

#include "StPythiaFourPMaker.h"

//SCL
#include "StarClassLibrary/SystemOfUnits.h"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"
#include "StThreeVectorD.hh"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

//StEvent
#include "StDetectorId.h"

//McEvent
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"

//Table stuff
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"

//Get Pythia record 
#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"

//StJetMaker
#include "emulator/StMuTrackFourVec.h"
#include "StPythia/StPythiaMuTrackFourVec.h"

#include <TLorentzVector.h>

ClassImp(StPythiaFourPMaker)
    
StPythiaFourPMaker::StPythiaFourPMaker(const char* name, StMCAsymMaker* sim,  StMcEventMaker* mc) 
: StFourPMaker(name), mSimuMaker(sim), mMcEventMaker(mc)
{ }

void StPythiaFourPMaker::Clear(Option_t* opt)
{
  for (Pythia4Vec::iterator it=mVec.begin(); it!=mVec.end(); ++it) {
    delete (*it);
    (*it) = 0;
  }
  mVec.clear();
  tracks.clear();
  return;
}

Int_t StPythiaFourPMaker::Make()
{
  //SubProcess ID from StMcEventMaker
  StMcEvent* mcEvent = (StMcEvent*) GetDataSet("StMcEvent");
  //StMcEvent* mcEvent = dynamic_cast<StMcEvent*>( mMcEventMaker->currentMcEvent() );
  if(!mcEvent)  {
    cout <<"No McEvent!!!"<<endl;
    return kStErr;
  }
  
  //access the ptyhia particle table.  Should access via StMcEvent, but that's not yet updated for Maxim's newest additions
  const St_particle* particleTabPtr = mSimuMaker->particleTable();
  const particle_st* particleTable = particleTabPtr->GetTable();

  for (int i=0; i<particleTabPtr->GetNRows();++i) {
    int status = particleTable[i].isthep;   //status
		
    if (status==1) { //ok, showering is finished, but these particles could decay further (e.g., pi0 -> gamma gamma)
			
      //particleTable[i].idhep;// particle id
      //particleTable[i].phep[4];//
      StThreeVectorF momentum(particleTable[i].phep[0],//px
			      particleTable[i].phep[1],//py
			      particleTable[i].phep[2]//pz
			      );
			
      double energy = particleTable[i].phep[3]; //E
      // StLorentzVectorF p4( energy, momentum);
      TLorentzVector p4(momentum.x(), momentum.y(), momentum.z(), energy);
			
      // if (fabs(p4.pseudoRapidity())<5.0) {
      if (fabs(p4.Eta()) < 5.0) {
				
	StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, i, kUnknownId);
				
	//void Init(StMuTrack* track, StLorentzVectorF P, Int_t i, StDetectorId detId);
	// pmu->Init(0, p4, i, kUnknownId);
				
	mVec.push_back(pmu);
	tracks.push_back(pmu);
				
      }
    }
  }
	
  return kStOK;
}







