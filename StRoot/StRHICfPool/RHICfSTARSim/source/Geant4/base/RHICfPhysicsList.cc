#include "RHICfPhysicsList.hh"
#include "RHICfPhysicsListMessenger.hh"

#include "RHICfExtraPhysics.hh"
#include "RHICfOpticalPhysics.hh"

#include "G4LossTableManager.hh"

#include "G4ProcessManager.hh"
#include "G4ParticleTypes.hh"
#include "G4ParticleTable.hh"

//#include "G4PhysListFactory.hh"
#include "FTFP_BERT.hh"
#include "QGSP_BERT_HP.hh"
#include "QGSP_BERT.hh"

#include "G4Gamma.hh"
#include "G4Electron.hh"
#include "G4Positron.hh"

#include "RHICfStepMax.hh"

#include "G4ProcessTable.hh"

#include "G4PionDecayMakeSpin.hh"
#include "G4DecayWithSpin.hh"

#include "G4DecayTable.hh"
#include "G4MuonDecayChannelWithSpin.hh"
#include "G4MuonRadiativeDecayChannelWithSpin.hh"

#include "G4RadioactiveDecayPhysics.hh"

#include "G4SystemOfUnits.hh"

RHICfPhysicsList::RHICfPhysicsList(G4String physName): G4VModularPhysicsList()
{
    G4LossTableManager::Instance();

    defaultCutValue  = 1.*mm;
    fCutForGamma     = defaultCutValue;
    fCutForElectron  = defaultCutValue;
    fCutForPositron  = defaultCutValue;
    fCutForProton    = defaultCutValue;

//    G4PhysListFactory factory;
    G4VModularPhysicsList* phys = NULL;
    if (physName == "QGSP_BERT") {
       phys = new QGSP_BERT;
    } else {
       phys = new FTFP_BERT;
    }
//    if (factory.IsReferencePhysList(physName)) {
//       phys = factory.GetReferencePhysList(physName);
//       if(!phys)G4Exception("RHICfPhysicsList::RHICfPhysicsList","InvalidSetup",
//                            FatalException,"PhysicsList does not exist");
       fMessenger = new RHICfPhysicsListMessenger(this);
//    }

    for (G4int i = 0; ; ++i) {
       G4VPhysicsConstructor* elem =
                  const_cast<G4VPhysicsConstructor*> (phys->GetPhysics(i));
       if (elem == NULL) break;
       G4cout << "RegisterPhysics: " << elem->GetPhysicsName() << G4endl;
       RegisterPhysics(elem);
    }

    fAbsorptionOn = true;
    
    //This looks complex, but it is not:
    //Get from base-class the pointer of the phsyicsVector
    //to be used. Remember: G4VModularPhysicsList is now a split class.
    //Why G4VModularPhysicsList::RegisterPhysics method is not used instead?
    //If possible we can remove this...
    fPhysicsVector =
                GetSubInstanceManager().offset[GetInstanceID()].physicsVector;
    
    fPhysicsVector->push_back(new RHICfExtraPhysics());
    fPhysicsVector->push_back(fOpticalPhysics =
                                        new RHICfOpticalPhysics(fAbsorptionOn));

    fPhysicsVector->push_back(new G4RadioactiveDecayPhysics());

    fStepMaxProcess = new RHICfStepMax();
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

RHICfPhysicsList::~RHICfPhysicsList()
{
    delete fMessenger;

    delete fStepMaxProcess;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void RHICfPhysicsList::ClearPhysics()
{
    for (G4PhysConstVector::iterator p  = fPhysicsVector->begin();
                                     p != fPhysicsVector->end(); ++p) {
        delete (*p);
    }
    fPhysicsVector->clear();
}

void RHICfPhysicsList::ConstructParticle()
{
    G4VModularPhysicsList::ConstructParticle();

    G4DecayTable* MuonPlusDecayTable = new G4DecayTable();
    MuonPlusDecayTable -> Insert(new
                           G4MuonDecayChannelWithSpin("mu+",0.986));
    MuonPlusDecayTable -> Insert(new
                           G4MuonRadiativeDecayChannelWithSpin("mu+",0.014));
    G4MuonPlus::MuonPlusDefinition() -> SetDecayTable(MuonPlusDecayTable);

    G4DecayTable* MuonMinusDecayTable = new G4DecayTable();
    MuonMinusDecayTable -> Insert(new
                            G4MuonDecayChannelWithSpin("mu-",0.986));
    MuonMinusDecayTable -> Insert(new
                            G4MuonRadiativeDecayChannelWithSpin("mu-",0.014));
    G4MuonMinus::MuonMinusDefinition() -> SetDecayTable(MuonMinusDecayTable);

}

void RHICfPhysicsList::ConstructProcess()
{
    G4VModularPhysicsList::ConstructProcess();

    SetVerbose(0);

    G4DecayWithSpin* decayWithSpin = new G4DecayWithSpin();

    G4ProcessTable* processTable = G4ProcessTable::GetProcessTable();

    G4VProcess* decay;
    decay = processTable->FindProcess("Decay",G4MuonPlus::MuonPlus());

    G4ProcessManager* pManager;
    pManager = G4MuonPlus::MuonPlus()->GetProcessManager();

    if (pManager) {
      if (decay) pManager->RemoveProcess(decay);
      pManager->AddProcess(decayWithSpin);
      // set ordering for PostStepDoIt and AtRestDoIt
      pManager ->SetProcessOrdering(decayWithSpin, idxPostStep);
      pManager ->SetProcessOrdering(decayWithSpin, idxAtRest);
    }

    decay = processTable->FindProcess("Decay",G4MuonMinus::MuonMinus());

    pManager = G4MuonMinus::MuonMinus()->GetProcessManager();

    if (pManager) {
      if (decay) pManager->RemoveProcess(decay);
      pManager->AddProcess(decayWithSpin);
      // set ordering for PostStepDoIt and AtRestDoIt
      pManager ->SetProcessOrdering(decayWithSpin, idxPostStep);
      pManager ->SetProcessOrdering(decayWithSpin, idxAtRest);
    }

    G4PionDecayMakeSpin* poldecay = new G4PionDecayMakeSpin();

    decay = processTable->FindProcess("Decay",G4PionPlus::PionPlus());

    pManager = G4PionPlus::PionPlus()->GetProcessManager();

    if (pManager) {
      if (decay) pManager->RemoveProcess(decay);
      pManager->AddProcess(poldecay);
      // set ordering for PostStepDoIt and AtRestDoIt
      pManager ->SetProcessOrdering(poldecay, idxPostStep);
      pManager ->SetProcessOrdering(poldecay, idxAtRest);
    }

    decay = processTable->FindProcess("Decay",G4PionMinus::PionMinus());

    pManager = G4PionMinus::PionMinus()->GetProcessManager();

    if (pManager) {
      if (decay) pManager->RemoveProcess(decay);
      pManager->AddProcess(poldecay);
      // set ordering for PostStepDoIt and AtRestDoIt
      pManager ->SetProcessOrdering(poldecay, idxPostStep);
      pManager ->SetProcessOrdering(poldecay, idxAtRest);
    }

    AddStepMax();

}

void RHICfPhysicsList::RemoveFromPhysicsList(const G4String& name)
{
    G4bool success = false;
    for (G4PhysConstVector::iterator p  = fPhysicsVector->begin();
                                     p != fPhysicsVector->end(); ++p) {
        G4VPhysicsConstructor* e = (*p);
        if (e->GetPhysicsName() == name) {
           fPhysicsVector->erase(p);
           success = true;
           break;
        }
    }
    if (!success) {
       G4ExceptionDescription message;
       message << "PhysicsList::RemoveFromEMPhysicsList "<< name << "not found";
       G4Exception("example RHICfPhysicsList::RemoveFromPhysicsList()",
       "ExamRHICfPhysicsList01",FatalException,message);
    }
}

void RHICfPhysicsList::SetAbsorption(G4bool toggle)
{
       fAbsorptionOn = toggle;
       RemoveFromPhysicsList("Optical");
       fPhysicsVector->
                    push_back(fOpticalPhysics = new RHICfOpticalPhysics(toggle));
       fOpticalPhysics->ConstructProcess();
}

void RHICfPhysicsList::SetCuts()
{
    if (verboseLevel >0) {
        G4cout << "RHICfPhysicsList::SetCuts:";
        G4cout << "CutLength : " << G4BestUnit(defaultCutValue,"Length")
               << G4endl;
    }

    // set cut values for gamma at first and for e- second and next for e+,
    // because some processes for e+/e- need cut values for gamma
    SetCutValue(fCutForGamma, "gamma");
    SetCutValue(fCutForElectron, "e-");
    SetCutValue(fCutForPositron, "e+");
    SetCutValue(fCutForProton, "proton");

    if (verboseLevel>0) DumpCutValuesTable();
}

void RHICfPhysicsList::SetCutForGamma(G4double cut)
{
    fCutForGamma = cut;
    SetParticleCuts(fCutForGamma, G4Gamma::Gamma());
}

void RHICfPhysicsList::SetCutForElectron(G4double cut)
{
    fCutForElectron = cut;
    SetParticleCuts(fCutForElectron, G4Electron::Electron());
}

void RHICfPhysicsList::SetCutForPositron(G4double cut)
{
    fCutForPositron = cut;
    SetParticleCuts(fCutForPositron, G4Positron::Positron());
}

void RHICfPhysicsList::SetCutForProton(G4double cut)
{
  fCutForProton = cut;
  SetParticleCuts(fCutForProton, G4Proton::Proton());
}

void RHICfPhysicsList::SetStepMax(G4double step)
{
  fStepMaxProcess->SetStepMax(step);
}

RHICfStepMax* RHICfPhysicsList::GetStepMaxProcess()
{
  return fStepMaxProcess;
}

void RHICfPhysicsList::AddStepMax()
{
  // Step limitation seen as a process

  auto theParticleIterator = GetParticleIterator();
  theParticleIterator->reset();
  while ((*theParticleIterator)()){
      G4ParticleDefinition* particle = theParticleIterator->value();
      G4ProcessManager* pmanager = particle->GetProcessManager();

      if (fStepMaxProcess->IsApplicable(*particle) && !particle->IsShortLived())
      {
         if (pmanager) pmanager ->AddDiscreteProcess(fStepMaxProcess);
      }
  }
}

void RHICfPhysicsList::SetNbOfPhotonsCerenkov(G4int maxNumber)
{
   fOpticalPhysics->SetNbOfPhotonsCerenkov(maxNumber);
}

void RHICfPhysicsList::SetVerbose(G4int verbose)
{
   fOpticalPhysics->GetCerenkovProcess()->SetVerboseLevel(verbose);
   fOpticalPhysics->GetScintillationProcess()->SetVerboseLevel(verbose);
   fOpticalPhysics->GetAbsorptionProcess()->SetVerboseLevel(verbose);
   fOpticalPhysics->GetRayleighScatteringProcess()->SetVerboseLevel(verbose);
   fOpticalPhysics->GetMieHGScatteringProcess()->SetVerboseLevel(verbose);
   fOpticalPhysics->GetBoundaryProcess()->SetVerboseLevel(verbose);
}
