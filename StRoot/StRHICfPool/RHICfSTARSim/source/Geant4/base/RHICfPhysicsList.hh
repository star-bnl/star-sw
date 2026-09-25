#ifndef RHICFPHYSICSLIST_H
#define RHICFPHYSICSLIST_H 1

#include "globals.hh"
#include "G4VModularPhysicsList.hh"

class G4VPhysicsConstructor;
class RHICfPhysicsListMessenger;

class RHICfStepMax;
class RHICfOpticalPhysics;

class RHICfPhysicsList: public G4VModularPhysicsList
{
  public:
    RHICfPhysicsList(G4String);
    virtual ~RHICfPhysicsList();

    void SetCuts();
    void SetCutForGamma(G4double);
    void SetCutForElectron(G4double);
    void SetCutForPositron(G4double);
    void SetCutForProton(G4double);

    void SetStepMax(G4double);
    RHICfStepMax* GetStepMaxProcess();
    void AddStepMax();

    /// Remove specific physics from physics list.
    void RemoveFromPhysicsList(const G4String&);

    /// Make sure that the physics list is empty.
    void ClearPhysics();

    virtual void ConstructParticle();
    virtual void ConstructProcess();

    // Turn on or off the absorption process
    void SetAbsorption(G4bool);

    void SetNbOfPhotonsCerenkov(G4int);

    void SetVerbose(G4int);

private:

    G4double fCutForGamma;
    G4double fCutForElectron;
    G4double fCutForPositron;
    G4double fCutForProton;

    RHICfStepMax* fStepMaxProcess;

    RHICfOpticalPhysics* fOpticalPhysics;

    RHICfPhysicsListMessenger* fMessenger;

    G4bool fAbsorptionOn;
    
    G4VMPLData::G4PhysConstVectorData* fPhysicsVector;

};

#endif
