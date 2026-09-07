#ifndef RHICFEVENTACTION_H
#define RHICFEVENTACTION_H 1

#include <iostream>
#include <vector>

#include "G4RunManager.hh"
#include "G4EventManager.hh"
#include "G4Event.hh"
#include "G4UserEventAction.hh"
#include "G4SDManager.hh"

#include "G4ios.hh"
#include "G4UnitsTable.hh"

#include "RHICfPrimaryGeneratorAction.hh"

#include "TROOT.h"
#include "TTree.h"

#include "RHICfGSOplateSD.hh"
#include "RHICfGSObarSD.hh"
#include "RHICfFCSD.hh"
#include "RHICfZDCSD.hh"
#include "RHICfSMDSD.hh"

#include "StRHICfSimDst.h"
#include "StRHICfSimEvent.h"
#include "StRHICfSimTrack.h"
#include "StRHICfSimRHICfHit.h"
#include "StRHICfSimZDC.h"

#include "RHICfSimUtil.hh"
#include "RHICfSimOptions.hh"

class G4Event;

class RHICfEventAction: public G4UserEventAction
{
    public:
        RHICfEventAction();
        virtual ~RHICfEventAction();

        virtual void BeginOfEventAction(const G4Event*);
        virtual void EndOfEventAction(const G4Event*);

        void SetOutputSimDstTree(TTree* tree){fOutputTree = tree;}
        void SetSimDst(StRHICfSimDst* simDst){fSimDst = simDst;}

        void SetRHICfMatchTrkID(int tower, int simTrkID, int g4TrkID);
        void SetZDCMatchTrkID(int simTrkID, int g4TrkID);

    private:
        bool IsShowerTrigger();
        bool IsType1Pi0Trigger();
        bool IsHighEMTrigger();

        void EventPrint();

        G4HCofThisEvent* fHitCollThisEvent;
        G4RunManager* fRunManager;
        G4SDManager* fSDManager;

        RHICfPrimaryGeneratorAction* fGenAction;

        // Hit Collection
        FCHitsCollection* fRHICfFCHitColl;
        GSOplateHitsCollection* fRHICfGSOPlateHitColl;
        GSObarHitsCollection* fRHICfGSOBarHitColl;
        ZDCHitsCollection* fZDCPMTHitColl;
        SMDHitsCollection* fZDCSMDHitColl;

        // SimUtil
        RHICfSimUtil* fSimUtil;
        RHICfSimOptions* fSimOpt;

        // SimDst
        TTree* fOutputTree;
        StRHICfSimDst* fSimDst;
        StRHICfSimEvent* fSimEvent;
        StRHICfSimTrack* fSimTrack;
        StRHICfSimRHICfHit* fSimRHICfHit;
        StRHICfSimZDC* fSimZDC;

};

#endif
