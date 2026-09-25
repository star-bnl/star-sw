#ifndef RHICFOpticalPhysics_h
#define RHICFOpticalPhysics_h 1

#include "globals.hh"

#include "G4OpWLS.hh"
#include "G4Cerenkov.hh"
#include "G4Scintillation.hh"

#include "G4OpMieHG.hh"
#include "G4OpRayleigh.hh"
#include "G4OpAbsorption.hh"
#include "G4OpBoundaryProcess.hh"

#include "G4VPhysicsConstructor.hh"

class RHICfOpticalPhysics : public G4VPhysicsConstructor
{
  public:

    RHICfOpticalPhysics(G4bool toggle=true);
    virtual ~RHICfOpticalPhysics();

    virtual void ConstructParticle();
    virtual void ConstructProcess();

    G4OpWLS* GetRHICfProcess() {return fRHICfProcess;}
    G4Cerenkov* GetCerenkovProcess() {return fCerenkovProcess;}
    G4Scintillation* GetScintillationProcess() {return fScintProcess;}
    G4OpAbsorption* GetAbsorptionProcess() {return fAbsorptionProcess;}
    G4OpRayleigh* GetRayleighScatteringProcess() {return fRayleighScattering;}
    G4OpMieHG* GetMieHGScatteringProcess() {return fMieHGScatteringProcess;}
    G4OpBoundaryProcess* GetBoundaryProcess() { return fBoundaryProcess;}

    void SetNbOfPhotonsCerenkov(G4int);

private:

    G4OpWLS*             fRHICfProcess;
    G4Cerenkov*          fCerenkovProcess;
    G4Scintillation*     fScintProcess;
    G4OpAbsorption*      fAbsorptionProcess;
    G4OpRayleigh*        fRayleighScattering;
    G4OpMieHG*           fMieHGScatteringProcess;
    G4OpBoundaryProcess* fBoundaryProcess;
 
    G4bool fAbsorptionOn;

};
#endif
