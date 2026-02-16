#include "G4LossTableManager.hh"
#include "G4EmSaturation.hh"

#include "RHICfOpticalPhysics.hh"

RHICfOpticalPhysics::RHICfOpticalPhysics(G4bool toggle)
    : G4VPhysicsConstructor("Optical")
{
  fRHICfProcess              = NULL;
  fScintProcess              = NULL;
  fCerenkovProcess           = NULL;
  fBoundaryProcess           = NULL;
  fAbsorptionProcess         = NULL;
  fRayleighScattering        = NULL;
  fMieHGScatteringProcess    = NULL;

  fAbsorptionOn              = toggle;
}

RHICfOpticalPhysics::~RHICfOpticalPhysics() { }

#include "G4OpticalPhoton.hh"

void RHICfOpticalPhysics::ConstructParticle()
{
  G4OpticalPhoton::OpticalPhotonDefinition();
}

#include "G4ProcessManager.hh"

void RHICfOpticalPhysics::ConstructProcess()
{
    G4cout << "RHICfOpticalPhysics:: Add Optical Physics Processes"
           << G4endl;

  fRHICfProcess = new G4OpWLS();

  fScintProcess = new G4Scintillation();
  fScintProcess->SetScintillationYieldFactor(1.);
  fScintProcess->SetTrackSecondariesFirst(true);

  fCerenkovProcess = new G4Cerenkov();
  //  fCerenkovProcess->SetMaxNumPhotonsPerStep(300);
  fCerenkovProcess->SetMaxNumPhotonsPerStep(20);
  fCerenkovProcess->SetTrackSecondariesFirst(true);

  fAbsorptionProcess      = new G4OpAbsorption();
  fRayleighScattering     = new G4OpRayleigh();
  fMieHGScatteringProcess = new G4OpMieHG();
  fBoundaryProcess        = new G4OpBoundaryProcess();

  G4ProcessManager* pManager =
    G4OpticalPhoton::OpticalPhoton()->GetProcessManager();

  if (!pManager) {
     std::ostringstream o;
     o << "Optical Photon without a Process Manager";
     G4Exception("RHICfOpticalPhysics::ConstructProcess()","",
                  FatalException,o.str().c_str());
  }

  if (fAbsorptionOn) pManager->AddDiscreteProcess(fAbsorptionProcess);

  pManager->AddDiscreteProcess(fRayleighScattering);
  pManager->AddDiscreteProcess(fMieHGScatteringProcess);

  pManager->AddDiscreteProcess(fBoundaryProcess);

  fRHICfProcess->UseTimeProfile("delta");
  //fRHICfProcess->UseTimeProfile("exponential");

  pManager->AddDiscreteProcess(fRHICfProcess);

  fScintProcess->SetScintillationYieldFactor(1.);
  fScintProcess->SetScintillationExcitationRatio(0.0);
  fScintProcess->SetTrackSecondariesFirst(true);

  // Use Birks Correction in the Scintillation process

  G4EmSaturation* emSaturation = G4LossTableManager::Instance()->EmSaturation();
  fScintProcess->AddSaturation(emSaturation);

  auto theParticleIterator = GetParticleIterator();
  theParticleIterator->reset();
  while ( (*theParticleIterator)() ){

    G4ParticleDefinition* particle = theParticleIterator->value();
    G4String particleName = particle->GetParticleName();

    pManager = particle->GetProcessManager();
    if (!pManager) {
       std::ostringstream o;
       o << "Particle " << particleName << "without a Process Manager";
       G4Exception("RHICfOpticalPhysics::ConstructProcess()","",
                    FatalException,o.str().c_str());
    }

    if(fCerenkovProcess->IsApplicable(*particle)){
      pManager->AddProcess(fCerenkovProcess);
      pManager->SetProcessOrdering(fCerenkovProcess,idxPostStep);
    }
    if(fScintProcess->IsApplicable(*particle)){
      pManager->AddProcess(fScintProcess);
      pManager->SetProcessOrderingToLast(fScintProcess,idxAtRest);
      pManager->SetProcessOrderingToLast(fScintProcess,idxPostStep);
    }

  }
}

void RHICfOpticalPhysics::SetNbOfPhotonsCerenkov(G4int maxNumber)
{
  fCerenkovProcess->SetMaxNumPhotonsPerStep(maxNumber);
}
