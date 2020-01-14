#include "Riostream.h"
#include "TMath.h"
#include "TDatabasePDG.h"
TDataSet *CreateTable() {
  Double_t pTlow  =    0;
  Double_t pThigh =   10;
  Double_t Ylow   =  -1.5; 
  Double_t Yhigh  =   1.5;
  Double_t Philow =   0;
  Double_t Phihigh= 2*TMath::Pi();
  Double_t zCut   =   70;
  Double_t Zlow   =  -zCut; 
  Double_t Zhigh  =   zCut; 
  Int_t    Npart  =   -1;
  const Char_t *nameP = "phi";
  const Char_t *opt = "5PerCentGmTsq";
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
    TGeant3TGeo *g3 = (TGeant3TGeo *)TVirtualMC::GetMC();
    TParticlePDG *p = TDatabasePDG::Instance()->GetParticle(nameP);
    if (! p) return;
    Int_t pdg = p->PdgCode();
    if (!pdg) return 0;
    Int_t iD  = g3->IdFromPDG(pdg);
    StarVMCApplication::Instance()->ForceDecay(nameP, "K+", "K-", 0, 100);
    StarMCSimplePrimaryGenerator *gener = (StarMCSimplePrimaryGenerator *) StarVMCApplication::Instance()->GetPrimaryGenerator();
    if ( gener && ! gener->IsA()->InheritsFrom( "StarMCSimplePrimaryGenerator" ) ) {
      delete gener; gener = 0;
    }
    if (! gener) gener =  new 
      StarMCSimplePrimaryGenerator( Npart, iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, opt);
    else
      gener->SetGenerator( Npart, iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, opt);
    StarVMCApplication::Instance()->SetPrimaryGenerator(gener);
    cout << "Set StarMCSimplePrimaryGenerator" << endl;
    StVMCMaker *geant = (StVMCMaker *)chain2->Maker("geant");
    if (! geant) return 0;
    geant->SetZminmax(Zlow, Zhigh);
  }
  TString fOpt(nameP); fOpt += opt; fOpt += "Z"; fOpt += zCut; fOpt += "cm";
  TDataSet *tableSet = new TDataSet(fOpt.Data());
  return (TDataSet *)tableSet;
}
