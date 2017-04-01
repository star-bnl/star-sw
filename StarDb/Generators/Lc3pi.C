#include "Riostream.h"
#include "TMath.h"
#include "TDatabasePDG.h"
TDataSet *CreateTable() {
  Double_t pTlow  =   1;
  Double_t pThigh =   1;
  Double_t Ylow   =  -1; 
  Double_t Yhigh  =   1;
  Double_t Philow =   0;
  Double_t Phihigh= 2*TMath::Pi();
  Double_t Zlow   =  -10; 
  Double_t Zhigh  =   10; 
  Int_t    Npart  =    1;
  if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
    if (gClassTable->GetID("St_geant_Maker") < 0) {
      cout << "You have to use root4star with St_geant_Maker already loaded" << endl; 
      return 0;
    }
    if (! St_geant_Maker::instance()) return 0;
  } else {
    if (! StVMCMaker::instance()) return 0;
    if (! StarVMCApplication::Instance()) return 0;
    if (! TVirtualMC::GetMC()->IsA()->InheritsFrom("TGeant3TGeo")) return 0;
    TGeant3TGeo *g3 = (TGeant3TGeo *) TVirtualMC::GetMC();
    g3->SetProcess("DCAY", 1);
#if 0
    Gcflag_t* cflag = g3->Gcflag();
    cflag->idebug = 1;
    cflag->idemax = 10000;
    cflag->iswit[0] = 2;
    cflag->iswit[1] = 2;
    cflag->iswit[2] = 2; 
#endif
    // Reset branching ratio's for Lc+ -> Lambda 2pi+pi-
    const Char_t *nameP ="Lambda_c+";
    TParticlePDG *p = TDatabasePDG::Instance()->GetParticle(nameP);
    if (! p) return;
    Int_t pdg = p->PdgCode();
    if (pdg < 0) return 0;
    Int_t iD  = g3->IdFromPDG(pdg);
    const Char_t *modes[3][3] = {
      {"Sigma*+","pi+","pi-"},
      {"Sigma*-","pi+","pi+"},
      {"Lambda0","rho0","pi+"}
    };
    Float_t branches[3] = {28.57, 28.57, 42.86};
    Int_t mode[6] = {0};
    Float_t bratio[6] = {0};
    for (Int_t i = 0; i < 3; i++) {
      bratio[i] = branches[i];
      mode[i] = 0;
      cout << "Force decay of " << nameP << " => ";
      for (Int_t j = 0; j < 3; j++) {
	cout << modes[i][j];
	if (j < 3) cout << " + ";
	p = TDatabasePDG::Instance()->GetParticle(modes[i][j]);
	if (! p) { cout << "p for " << modes[i][j] << " is not found" << endl; continue;}
	pdg = p->PdgCode();
	//	if (pdg < 0) { cout << "pdg = " << pdg << " for " << modes[i][j] << " is not found"; continue;}
	Int_t Id = g3->IdFromPDG(pdg);
	//	cout << " pdg " << pdg << " => Id " << Id;
	mode[i] = 100*mode[i] + Id;
      }
      cout << endl;
      //      cout << "; mode = " << mode[i] << " branching = " << bratio[i] << endl;
    }
    g3->Gsdk(iD, bratio, mode);
    // Reset branching Sigma*+- -> Lambda0 pi+-
    //    p = TDatabasePDG::Instance()->GetParticle(modes[2][0]);
    StarMCSimplePrimaryGenerator *gener = (StarMCSimplePrimaryGenerator *) StarVMCApplication::Instance()->GetPrimaryGenerator();
    if ( gener && ! gener->IsA()->InheritsFrom( "StarMCSimplePrimaryGenerator" ) ) {
      delete gener; gener = 0;
    }
    if (! gener) gener =  new 
      StarMCSimplePrimaryGenerator( Npart, iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, "G");
    else
      gener->SetGenerator( Npart, iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, "G");
    StarVMCApplication::Instance()->SetPrimaryGenerator(gener);
    cout << "Set StarMCSimplePrimaryGenerator" << endl;
  }
  TDataSet *tableSet = new TDataSet("20muons");
  return (TDataSet *)tableSet;
}
