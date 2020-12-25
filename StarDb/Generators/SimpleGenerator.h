#ifndef __ROOTCINT__
#include "Riostream.h"
#include "TMath.h"
#include "TDatabasePDG.h"
#include "TSystem.h"
#include "TString.h"
#endif
TDataSet *CreateTable(const Char_t *theFile) {
  TString NameP(gSystem->BaseName(theFile));
  NameP.ReplaceAll("XiM_bar"   ,"Xi+");
  NameP.ReplaceAll("XiM"       ,"Xi-");
  NameP.ReplaceAll("Omega_bar" ,"Omega+");
  NameP.ReplaceAll("Omega"     ,"Omega-");
  NameP.ReplaceAll("M.C","-.C");
  NameP.ReplaceAll("MFixed","-Fixed");
  NameP.ReplaceAll("M_bar.C","-bar.C");
  NameP.ReplaceAll("M_barFixed","-barFixed");
  NameP.ReplaceAll("P.C","+.C");
  NameP.ReplaceAll("PFixed","+Fixed");
  NameP.ReplaceAll("P_bar.C","+_bar.C");
  NameP.ReplaceAll("P_barFixed","+_barFixed");
  NameP.ReplaceAll(".C","");
  Bool_t isFixed = kFALSE;
  Double_t pTlow  =    0;
  Double_t pThigh =   20;
  Double_t Ylow   =  -2; 
  Double_t Yhigh  =   2;
  Double_t Philow =   0;
  Double_t Phihigh= 2*TMath::Pi();
  Double_t Zlow   =  -70; 
  Double_t Zhigh  =   70; 
  Int_t    Npart  =   20;
  TString  opt("GmTsq");
  if (NameP.Contains("Fixed")) {
    isFixed = kTRUE;
    NameP.ReplaceAll("Fixed","");
    Ylow   =  -2.4; 
    Yhigh  =   0.1;
    Zlow   =  200; 
    Zhigh  =  200; 
    opt = "GmTsqy"; // rapidity
    if (NameP.Contains("Flat")) {
      opt ="G";
      pThigh =   5;
      NameP.ReplaceAll("Flat","");
    }
  }
  const Char_t *nameP = NameP.Data();
  cout << "nameP = " << nameP << "\tfile = " << theFile << endl;
  gEnv->SetValue("FixedSigmaX", 0.0250);
  gEnv->SetValue("FixedSigmaY", 0.0250);
  gEnv->SetValue("FixedSigmaZ", 0.0250);
  cout << "PV errors: "
       <<   "FixedSigmaX = " << gEnv->GetValue("FixedSigmaX", 0.0250) 
       << "\tFixedSigmaY = " << gEnv->GetValue("FixedSigmaY", 0.0250) 
       << "\tFixedSigmaZ = " << gEnv->GetValue("FixedSigmaZ", 0.0250) << endl;
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
    TObjArray *obj = NameP.Tokenize(":");
    Int_t nParsed = obj->GetEntries();
    Int_t iDD = 0;
    for (Int_t k = 0; k < nParsed; k++) {
      if (! obj->At(k)) continue;
      TString name(((TObjString *) obj->At(k))->GetName());
      cout << k << "\t" << name.Data() << endl;
      TGeant3TGeo *g3 = (TGeant3TGeo *)TVirtualMC::GetMC();
      TParticlePDG *p = TDatabasePDG::Instance()->GetParticle(name.Data());
      if (! p) {
	cout << "TDatabasePDG::GetParticle(" << name.Data() << ") is not found" << endl; 
	return 0;
      }
      Int_t pdg = p->PdgCode();
      if (!pdg) {
	return 0;
      }
      Int_t iD  = g3->IdFromPDG(pdg);
      if (! iD) {
	cout << "TGeant3TGeo::IdFromPDG(" << pdg << ") is not found" << endl; 
	return 0;
      }
      if (iDD > 0) iDD *= 1000;
      iDD += iD;
      StarVMCApplication::Instance()->ForceDecay(name);
      cout << "iD = " << iD << "\tiDD = " << iDD << endl;
      // StarVMCApplication::Instance()->ForceDecay(name, "K+", "K-", "", 100);
    }
    StarMCSimplePrimaryGenerator::SetTemperature(0.155); // 155 MeV for fixed target
    StarMCSimplePrimaryGenerator *gener = (StarMCSimplePrimaryGenerator *) StarVMCApplication::Instance()->GetPrimaryGenerator();
    if (! gener) {
      cout << "Create StarMCSimplePrimaryGenerator(" <<  Npart << "," <<  iDD << "," 
	   << pTlow << "," << pThigh<< "," <<Ylow<< "," << Yhigh<< "," 
	   << Philow<< "," << Phihigh<< "," << Zlow<< "," << Zhigh<< "," << opt.Data() << ")"  << endl;
      gener =  new 
	StarMCSimplePrimaryGenerator( Npart, iDD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, opt);
    } else {
      cout << "Set StarMCSimplePrimaryGenerator(" <<  Npart << "," <<  iDD << "," 
	   << pTlow << "," << pThigh<< "," <<Ylow<< "," << Yhigh<< "," 
	   << Philow<< "," << Phihigh<< "," << Zlow<< "," << Zhigh<< "," << opt.Data() << ")" << endl;
      gener->SetGenerator( Npart, iDD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, opt);
    }
    StarVMCApplication::Instance()->SetPrimaryGenerator(gener);
    cout << "Set StarMCSimplePrimaryGenerator" << endl;
  }
  TDataSet *tableSet = new TDataSet(nameP);
  return (TDataSet *)tableSet;
}
