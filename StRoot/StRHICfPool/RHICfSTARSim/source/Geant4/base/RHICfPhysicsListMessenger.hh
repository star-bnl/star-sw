#ifndef RHICFPHYSICSLISTMESSENGER_H
#define RHICFPHYSICSLISTMESSENGER_H 1

#include "globals.hh"
#include "G4UImessenger.hh"

#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"

#include "G4DecayTable.hh"
#include "G4VDecayChannel.hh"

class RHICfPhysicsList;

class G4UIdirectory;
class G4UIcmdWithABool;
class G4UIcmdWithAString;
class G4UIcmdWithAnInteger;
class G4UIcmdWithoutParameter;
class G4UIcmdWithADoubleAndUnit;

/// Provide control of the physics list and cut parameters

class RHICfPhysicsListMessenger : public G4UImessenger
{
  public:

    RHICfPhysicsListMessenger(RHICfPhysicsList* );
    virtual ~RHICfPhysicsListMessenger();

    virtual void SetNewValue(G4UIcommand*, G4String);

  private:

    RHICfPhysicsList* fPhysicsList;

    G4UIdirectory* fDirectory;
    G4UIdirectory* fDecayDirectory;

    G4UIcmdWithABool* fSetAbsorptionCmd;

    G4UIcmdWithAnInteger* fVerboseCmd;
    G4UIcmdWithAnInteger* fCerenkovCmd;

    G4UIcmdWithADoubleAndUnit* fGammaCutCmd;
    G4UIcmdWithADoubleAndUnit* fElectCutCmd;
    G4UIcmdWithADoubleAndUnit* fPosCutCmd;
    G4UIcmdWithADoubleAndUnit* fCutCmd;
    G4UIcmdWithADoubleAndUnit* fAllCutCmd;
    G4UIcmdWithoutParameter* fListHadCmd;
    G4UIcmdWithADoubleAndUnit* fStepMaxCmd;

    G4UIcmdWithAString*        fRemovePhysicsCmd;
    G4UIcmdWithoutParameter*   fClearPhysicsCmd;

    G4UIcmdWithAString*   fListCmd;

    G4UIcmdWithoutParameter* fPienuCmd;
    G4UIcmdWithoutParameter* fPimunuCmd;

};

#endif
