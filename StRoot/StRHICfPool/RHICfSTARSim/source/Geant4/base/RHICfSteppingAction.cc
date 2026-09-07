#include "RHICfSteppingAction.hh"

RHICfSteppingAction::RHICfSteppingAction()
{
}

RHICfSteppingAction::~RHICfSteppingAction()
{
}

void RHICfSteppingAction::UserSteppingAction(const G4Step* step)
{
    // Set the Shower particles origin!
    auto secondaries = step -> GetSecondaryInCurrentStep();
    if(secondaries->size() != 0){
        fTrack = step -> GetTrack();
        fTrackInfo = (RHICfTrackInfo*)(fTrack->GetUserInformation());
        int primaryID = fTrackInfo -> GetPrimaryID();

        for (auto* secondTrack : *secondaries) {
            RHICfTrackInfo* secondTrackInfo = new RHICfTrackInfo(primaryID);
            const_cast<G4Track*>(secondTrack)->SetUserInformation(secondTrackInfo);
        }
    }
}
