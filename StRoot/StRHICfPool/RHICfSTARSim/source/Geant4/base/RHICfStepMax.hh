#ifndef RHICFSTEPMAX_H
#define RHICFSTEPMAX_H 1

#include "globals.hh"

#include "G4Step.hh"
#include "G4VDiscreteProcess.hh"
#include "G4ParticleDefinition.hh"

class RHICfStepMax : public G4VDiscreteProcess
{
  public:

    RHICfStepMax(const G4String& processName = "UserStepMax");
    RHICfStepMax(RHICfStepMax &);

    virtual ~RHICfStepMax();

    virtual G4bool IsApplicable(const G4ParticleDefinition&);

    void SetStepMax(G4double);

    G4double GetStepMax() {return fMaxChargedStep;};

    virtual G4double PostStepGetPhysicalInteractionLength(const G4Track& track,
                                                  G4double previousStepSize,
                                                  G4ForceCondition* condition);

    virtual G4VParticleChange* PostStepDoIt(const G4Track&, const G4Step&);

  protected:

    G4double GetMeanFreePath(const G4Track&, G4double, G4ForceCondition*);

  private:

    // hide assignment operator as private
    RHICfStepMax & operator=(const RHICfStepMax &right);
    RHICfStepMax(const RHICfStepMax&);

  private:

  G4double fProposedStep;
    G4double fMaxChargedStep;

};

#endif
