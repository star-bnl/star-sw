#include "TChain.h"

#include "StGammaPythiaEvent.h"
#include "StGammaPythiaEventMaker.h"
#include "StGammaEventMaker.h"
#include "StGammaEvent.h"
#include "StGammaScheduleMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

ClassImp(StGammaEventMaker);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaEventMaker::StGammaEventMaker(const char *name): StMaker(name)
{}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaEventMaker::~StGammaEventMaker()
{}

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
Int_t StGammaEventMaker::Init()
{

    // Instantaite a new StGammaEvent
    mGammaEvent = new StGammaEvent();
    
    // Retrieve StGammaPythiaEventMaker from the chain
    mPythia = 0;
    mPythiaMaker = dynamic_cast<StGammaPythiaEventMaker*>(GetMakerInheritsFrom("StGammaPythiaEventMaker"));
    if(mPythiaMaker) 
    {
        mPythia = new StGammaPythiaEvent;
        mPythiaMaker->SetPythia(mPythia);
        mGammaEvent->SetPythia(mPythia);
    }

    // Retrieve StMuDstMaker from the chain
    muDstMaker  = dynamic_cast<StMuDstMaker*>(GetMakerInheritsFrom("StMuDstMaker"));
    assert(muDstMaker);
    
    return StMaker::Init();
}

//////////////////////////////////////////////////
//                 Maker Clear                  //
//////////////////////////////////////////////////
void StGammaEventMaker::Clear(Option_t *opts)
{
    mGammaEvent->Clear(opts);
    StMaker::Clear(opts);
}


//////////////////////////////////////////////////
//                  Maker Make                  //
//////////////////////////////////////////////////
Int_t StGammaEventMaker::Make()
{

    // Retrieve MuDst
    if(!GetDataSet("MuDst")) 
    {
        LOG_WARN << "No MuDst" << endm;
        return kStFatal;
    }

    // Retrieve the primary vertex, or set
    // vertex to zero if none is found
    StMuPrimaryVertex *pv = StMuDst::primaryVertex();
    if(pv)
    {
        mGammaEvent->SetVertex(TVector3(pv->position().xyz()));
        mGammaEvent->SetVertexRank(pv->ranking());
        mGammaEvent->mFlags |= TPC_VERTEX;
    }
    else
    {
        mGammaEvent->SetVertex(TVector3(0.,0.,0.));
        mGammaEvent->SetVertexRank(-100);
        mGammaEvent->mFlags |= !(TPC_VERTEX);
    }

    mGammaEvent->SetRunNumber( StMuDst::event()->runNumber() );
    mGammaEvent->SetEventNumber( StMuDst::event()->eventNumber() );
    mGammaEvent->SetMudstFileName( muDstMaker->chain()->GetFile()->GetName() );
    mGammaEvent->SetMagneticField( StMuDst::event()->magneticField() );
    mGammaEvent->SetTriggerIds( StMuDst::event()->triggerIdCollection().nominal().triggerIds() );
    mGammaEvent->SetBunchCrossing48bit( StMuDst::event()->l0Trigger().bunchCrossingId() );
    mGammaEvent->SetBunchCrossing7bit( StMuDst::event()->l0Trigger().bunchCrossingId7bit( mGammaEvent->runNumber() ) );

    // Store simulated triggers
    vector<unsigned int> simuTriggers;

    StTriggerSimuMaker *triggerSimu = dynamic_cast<StTriggerSimuMaker*>(GetMakerInheritsFrom("StTriggerSimuMaker"));
    if(triggerSimu)
    {

        for(unsigned int i = 0; i < mRequestedTriggers.size(); ++i)
        {

            if(triggerSimu->isTrigger(mRequestedTriggers.at(i)))
            {
                simuTriggers.push_back(mRequestedTriggers.at(i));
                cout << "StGammaEventMaker::Make() - " << mRequestedTriggers.at(i) << " fired!" << endl;
            }
            else
            {
                cout << "StGammaEventMaker::Make() - " << mRequestedTriggers.at(i) << " did not fire!" << endl;
            }

        }

    }

    mGammaEvent->SetSimuTriggerIds(simuTriggers); 

    // Store spin information
    StSpinDbMaker* spinDb = dynamic_cast<StSpinDbMaker*>(GetMakerInheritsFrom("StSpinDbMaker"));
    if(spinDb)
    {

        mGammaEvent->SetValidDb(spinDb->isValid());
        mGammaEvent->SetSpin4( spinDb->spin4usingBX48( mGammaEvent->bunchCrossing48bit() ) );
        mGammaEvent->SetBunchCrossingStar( spinDb->BXstarUsingBX48( mGammaEvent->bunchCrossing48bit() ) );        

        if(spinDb->isPolDirLong())    mGammaEvent->SetPolarizationType(StGammaEvent::kLongLong);
        else if(spinDb->isPolDirTrans()) mGammaEvent->SetPolarizationType(StGammaEvent::kTransTrans);

    }

    
    // Store timestamp index in place of run number in simulation
    // when dynamic timestamps have been specified in the GammaMaker
    StGammaScheduleMaker *scheduler = dynamic_cast<StGammaScheduleMaker*>(GetMakerInheritsFrom("StGammaScheduleMaker"));
    if(scheduler) 
    {
        if(scheduler->nStamps()) mGammaEvent->SetRunNumber(scheduler->index());
    }
    
    return kStOK;
  
}
