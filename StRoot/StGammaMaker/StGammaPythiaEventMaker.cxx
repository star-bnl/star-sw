//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 19 July 2007
//
// Algorithms and implementations by Michael Betancourt (MIT)
//

#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "StMcEvent/StMcEventTypes.hh"
#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"
#include "StGammaPythiaEvent.h"
#include "StGammaPythiaEventMaker.h"
#include "TMath.h"

ClassImp(StGammaPythiaEventMaker);

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
int StGammaPythiaEventMaker::Make()
{

    // Check pointer to Pythia event
    if(!mPythia)
    {
        LOG_WARN << "No StGammaPythiaEvent" << endm;
        return kStWarn;
    }

    // Get StMcEvent
    StMcEvent* mcEvent = (StMcEvent*)GetDataSet("StMcEvent");
    if(!mcEvent)
    {
        LOG_WARN << "No StMcEvent" << endm;
        return kStWarn;
    }
    
    // Get base StPythiaEvent pointer from StMCAsymMaker
    StMCAsymMaker* asym = dynamic_cast<StMCAsymMaker*>(GetMakerInheritsFrom("StMCAsymMaker"));
    if(!asym) 
    {
        LOG_WARN << "No StMCAsymMaker" << endm;
        return kStWarn;
    }

    new (mPythia) StGammaPythiaEvent(asym->pythiaEvent());

    //////////////////////////////////////////////////
    //  Store stable hadrons from the PYTHIA record //
    //////////////////////////////////////////////////    

    // Fetch the PYTHIA event record
    TDataSet *Event = GetDataSet("geant");
    if(!Event)
    {
        LOG_WARN << "No geant data set!" << endm;
        return kStWarn;
    }
    
    TDataSetIter geantDstI(Event);
    St_particle *particleTabPtr = (St_particle*)geantDstI("particle");
    particle_st *pTable = particleTabPtr->GetTable();

    // Store stable hadrons
    for(int i = 0; i < particleTabPtr->GetNRows(); ++i)
    {
        
        if(pTable[i].isthep != 1) continue;

        if(TMath::Abs(pTable[i].idhep) < 100) continue;
        
        mPythia->hadron().push_back(TLorentzVector(pTable[i].phep[0], pTable[i].phep[1], pTable[i].phep[2], pTable[i].phep[3]));

        if(pTable[i].idhep == 111)
        {
            mPythia->neutralPion().push_back(TLorentzVector(pTable[i].phep[0], pTable[i].phep[1], pTable[i].phep[2], pTable[i].phep[3]));
        }

    }
    
    //////////////////////////////////////////////////
    //     Store prompt photons, distinguishing     //
    //      between converted and nonconverted      //
    ////////////////////////////////////////////////// 

    // Check for prompt photon subprocess ID
    St_g2t_event* Pg2t_event = (St_g2t_event*)geantDstI("g2t_event");
    g2t_event_st* g2t_event = Pg2t_event->GetTable();
    long pid = g2t_event->subprocess_id;

    bool photonEvent = (pid==14) || (pid==18) || (pid==29) || (pid==114) || (pid==115);
    if(!photonEvent) return kStOk;

    // Collect photons from primary vertex, looping over daughters
    StMcVertex *primary=mcEvent->primaryVertex();
        
    for(UInt_t id = 0; id < primary->numberOfDaughters(); ++id)
    {
        
        StMcTrack *track = primary->daughter(id);
        if ( !track ) continue;
        const StLorentzVectorF& trackV=track->fourMomentum();
            
        // Require that track is a photon
        bool promptFlag = (track->geantId() == 1);
    
        // Require that track is not a daughter of the initial partons
        promptFlag &= track->parent()->eventGenLabel() >= 5;
        
        // Require that track is a daughter of a photon (how PYTHIA handles the evoluton of the
        // final state prompt photon to a stable photon)
        promptFlag &= track->parent()->pdgId() == 22;
            
        // Check for conversion and store photons
        if(promptFlag)
        {
            
            if(track->stopVertex())
            {
                mPythia->conversion().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
            }
            else
            {
                mPythia->prompt().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
            }
            
        }

    }
        
    return kStOk;
    
}
