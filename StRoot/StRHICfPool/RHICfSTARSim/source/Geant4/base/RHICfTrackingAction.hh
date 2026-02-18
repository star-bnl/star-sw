#ifndef RHICFTRACKINGACTION_H
#define RHICFTRACKINGACTION_H 1

#include "G4UserTrackingAction.hh"
#include "G4TrackingManager.hh"
#include "G4Track.hh"

#include "G4RunManager.hh"
#include "G4SDManager.hh"
#include "RHICfTruthCounterSD.hh"
#include "RHICfZDCTruthCounterSD.hh"

#include "RHICfPrimaryGeneratorAction.hh"
#include "StRHICfSimDst.h"
#include "StRHICfSimTrack.h"
#include "StRHICfSimRHICfHit.h"
#include "StRHICfSimZDC.h"

class RHICfTrackingAction: public G4UserTrackingAction
{
    public:
        RHICfTrackingAction();
        ~RHICfTrackingAction();

        void PreUserTrackingAction(const G4Track*);
        void PostUserTrackingAction(const G4Track*);

        void SetSimDst(StRHICfSimDst* simDst){fSimDst = simDst;}

    private:
        G4RunManager* fRunManager;
        RHICfPrimaryGeneratorAction* fGenAction;
        StRHICfSimDst* fSimDst;
        StRHICfSimTrack* fSimTrack;
        StRHICfSimRHICfHit* fSimRHICfHit;
        StRHICfSimZDC* fSimZDC;

        G4SDManager* fSDManager;
        RHICfTruthCounterSD* fRHICfTruthCounterSD;
        RHICfZDCTruthCounterSD* fZDCTruthCounterSD;
};

#endif
