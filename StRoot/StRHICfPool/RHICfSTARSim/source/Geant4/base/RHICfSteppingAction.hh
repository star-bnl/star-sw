#ifndef RHICFSTEPPINGACTION_H
#define RHICFSTEPPINGACTION_H 1

#include "G4UserSteppingAction.hh"

#include "G4Step.hh"
#include "G4Track.hh"
#include "RHICfTrackInfo.hh"

class RHICfSteppingAction: public G4UserSteppingAction
{
    public:
        RHICfSteppingAction();
        ~RHICfSteppingAction();

        void UserSteppingAction(const G4Step* step);

    private:
        G4Track* fTrack;
        RHICfTrackInfo* fTrackInfo;
};

#endif
