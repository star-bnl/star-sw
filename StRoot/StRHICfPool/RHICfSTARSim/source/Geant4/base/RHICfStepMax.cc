#include "G4VPhysicalVolume.hh"

#include "RHICfStepMax.hh"
//#include "RHICfStepMaxMessenger.hh"

RHICfStepMax::RHICfStepMax(const G4String& processName):
  G4VDiscreteProcess(processName), fMaxChargedStep(DBL_MAX)//, fMessenger(0)
{
  //  fMessenger = new RHICfStepMaxMessenger(this);
}

RHICfStepMax::~RHICfStepMax() 
{ 
  //  delete fMessenger; 
}

G4bool RHICfStepMax::IsApplicable(const G4ParticleDefinition& particle)
{
  return (particle.GetPDGCharge() != 0.);
}

void RHICfStepMax::SetStepMax(G4double step) 
{
  fMaxChargedStep = step;
}


G4double 
RHICfStepMax::PostStepGetPhysicalInteractionLength(const G4Track&, G4double, G4ForceCondition* condition)
{
  // condition is set to "Not Forced"
  *condition = NotForced;
  fProposedStep = fMaxChargedStep;

  return fProposedStep;
}

G4VParticleChange* RHICfStepMax::PostStepDoIt(const G4Track& aTrack, const G4Step&)
{
  aParticleChange.Initialize(aTrack);
  return &aParticleChange;
}

G4double RHICfStepMax::GetMeanFreePath(const G4Track&, G4double, G4ForceCondition*)
{
  return 0.;
}
