#include "G4UImanager.hh"

#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithABool.hh"
#include "G4UIcmdWithoutParameter.hh"

#include "RHICfPhysicsListMessenger.hh"
#include "RHICfPhysicsList.hh"

RHICfPhysicsListMessenger::RHICfPhysicsListMessenger(RHICfPhysicsList* pPhys):
  G4UImessenger(),fPhysicsList(pPhys),
  fGammaCutCmd(0), fElectCutCmd(0), fPosCutCmd(0), fCutCmd(0), fAllCutCmd(0),
  fListHadCmd(0) 
{   
  fListCmd = new G4UIcmdWithAString("/lhcf/phys/addPhysics",this);
  fListCmd->SetGuidance("Add modula physics list.");
  fListCmd->SetParameterName("PList",false);
  fListCmd->AvailableForStates(G4State_PreInit);

  fGammaCutCmd = new G4UIcmdWithADoubleAndUnit("/lhcf/phys/CutGamma",this);  
  fGammaCutCmd->SetGuidance("Set gamma cut.");
  fGammaCutCmd->SetParameterName("Gcut",false);
  fGammaCutCmd->SetUnitCategory("Length");
  fGammaCutCmd->SetRange("Gcut>=0.0");
  fGammaCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fElectCutCmd = new G4UIcmdWithADoubleAndUnit("/lhcf/phys/CutEl",this);  
  fElectCutCmd->SetGuidance("Set electron cut.");
  fElectCutCmd->SetParameterName("Ecut",false);
  fElectCutCmd->SetUnitCategory("Length");
  fElectCutCmd->SetRange("Ecut>=0.0");
  fElectCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);
  
  fPosCutCmd = new G4UIcmdWithADoubleAndUnit("/lhcf/phys/CutPos",this);
  fPosCutCmd->SetGuidance("Set positron cut.");
  fPosCutCmd->SetParameterName("Pcut",false);
  fPosCutCmd->SetUnitCategory("Length");
  fPosCutCmd->SetRange("Pcut>=0.0");
  fPosCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fCutCmd = new G4UIcmdWithADoubleAndUnit("/lhcf/phys/CutProt",this);
  fCutCmd->SetGuidance("Set proton cut.");
  fCutCmd->SetParameterName("ProtCut",false);
  fCutCmd->SetUnitCategory("Length");
  fCutCmd->SetRange("ProtCut>=0.0");
  fCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fAllCutCmd = new G4UIcmdWithADoubleAndUnit("/lhcf/phys/CutsAll",this);
  fAllCutCmd->SetGuidance("Set cut for all.");
  fAllCutCmd->SetParameterName("cut",false);
  fAllCutCmd->SetUnitCategory("Length");
  fAllCutCmd->SetRange("cut>=0.0");
  fAllCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fListHadCmd = new G4UIcmdWithoutParameter("/lhcf/phys/list",this);
  fListHadCmd->SetGuidance("Available Physics Lists");
  fListHadCmd->AvailableForStates(G4State_PreInit,G4State_Idle);
}


RHICfPhysicsListMessenger::~RHICfPhysicsListMessenger()
{
  delete fListCmd;
  delete fGammaCutCmd;
  delete fElectCutCmd;
  delete fPosCutCmd;
  delete fCutCmd;
  delete fAllCutCmd;
  delete fListHadCmd;
}


void RHICfPhysicsListMessenger::SetNewValue(G4UIcommand* command, G4String newValue)
{
  //  if( command == fListCmd )
  //   { fPhysicsList->AddPhysicsList(newValue);}

  G4UImanager* UI = G4UImanager::GetUIpointer();
  if( command == fGammaCutCmd ) {
    if(fPhysicsList) {
      fPhysicsList->SetCutForGamma(fGammaCutCmd->GetNewDoubleValue(newValue));
    } else {
      UI->ApplyCommand("/run/setCutForAGivenParticle gamma " + newValue);
    }

  } else if( command == fElectCutCmd ) {
    if(fPhysicsList) {
      fPhysicsList->SetCutForElectron(
        fElectCutCmd->GetNewDoubleValue(newValue));
    } else {
      UI->ApplyCommand("/run/setCutForAGivenParticle e- " + newValue);
    }

  } else if( command == fPosCutCmd ) {
    if(fPhysicsList) {
      fPhysicsList->SetCutForPositron(fPosCutCmd->GetNewDoubleValue(newValue));
    } else {
      UI->ApplyCommand("/run/setCutForAGivenParticle e+ " + newValue);
    }

  } else if( command == fCutCmd ) {
    if(fPhysicsList) {
      fPhysicsList->SetCutForProton(fCutCmd->GetNewDoubleValue(newValue));
    } else {
      UI->ApplyCommand("/run/setCutForAGivenParticle proton " + newValue);
    }

  } else if( command == fAllCutCmd ) {

    if(fPhysicsList) {
      G4double cut = fAllCutCmd->GetNewDoubleValue(newValue);
      fPhysicsList->SetCutForGamma(cut);
      fPhysicsList->SetCutForElectron(cut);
      fPhysicsList->SetCutForPositron(cut);
      fPhysicsList->SetCutForProton(cut);
    } else {
      UI->ApplyCommand("/run/setCut " + newValue);
    }

  }
  /*
 else if( command == fListHadCmd ) {
    if(fPhysicsList) {
      fPhysicsList->List();
    } else { 
      G4cout << "### RHICfPhysicsListMessenger WARNING: "
             << " /testhadr/ListPhysics UI command is not available "
             << "for reference Physics List" << G4endl;
    }
  }
  */
}
