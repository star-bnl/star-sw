#ifndef RHICFPRIMARYGENERATORACTION_H
#define RHICFPRIMARYGENERATORACTION_H 1

#include "globals.hh"
#include "G4VUserPrimaryGeneratorAction.hh"

#include <vector>

#include "TTree.h"

#include "G4Event.hh"
#include "G4ParticleGun.hh"
#include "G4RunManager.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "G4SystemOfUnits.hh"
#include "globals.hh"

#include "RHICfRunAction.hh"

#include "RHICfSimUtil.hh"
#include "StRHICfSimDst.h"
#include "StRHICfSimEvent.h"
#include "StRHICfSimTrack.h"

class G4VPrimaryGenerator;

class RHICfPrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction 
{
    public:
        RHICfPrimaryGeneratorAction();
        ~RHICfPrimaryGeneratorAction();

        virtual void GeneratePrimaries(G4Event* anEvent);

        void SetInputSimDstTree(TTree* tree){fInputTree = tree;}
        void SetSimDst(StRHICfSimDst* simDst){fSimDst = simDst;}
        
        int GetGenerateTrkNum();
        int GetGenerateSimTrkID(int trackID);

    private:
        void ReadSimDstParticle(G4Event* anEvent);
        void CreateSingleGen(G4Event* anEvent);

        RHICfSimUtil* fSimUtil;
        RHICfSimOptions* fSimOpt;

        G4ParticleGun* fParticleGun;

        G4RunManager* fRunManager;
        RHICfRunAction* fRunAction;
        G4ParticleDefinition* fParticle;
        G4ParticleTable* fParTable;

        TTree* fInputTree;
        StRHICfSimDst* fSimDst;
        StRHICfSimEvent* fSimEvent;
        StRHICfSimTrack* fSimTrack;

        std::vector<int> fGenSimTrkID;

};

#endif
