#include "Riostream.h"
#include "TMath.h"
TDataSet *CreateTable() {
  Int_t    iD     =   6; 
  Double_t pTlow  =   1;
  Double_t pThigh =   1;
  Double_t Ylow   =  -1; 
  Double_t Yhigh  =   1;
  Double_t Philow =   0;
  Double_t Phihigh= 2*TMath::Pi();
  Double_t Zlow   =  -10; 
  Double_t Zhigh  =   10; 
  Int_t    Npart  =  20;
#ifndef __CLING__
  if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
    if (gClassTable->GetID("St_geant_Maker") < 0) {
      cout << "You have to use root4star with St_geant_Maker already loaded" << endl; 
      return 0;
    }
    if (! St_geant_Maker::instance()) return 0;
    // gkine #particles partid ptrange yrange phirange vertexrange
    TString kine(Form("gkine %i %i %f %f %f %f %f %f %f %f",Npart,iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh));
    cout << "Set kine : " << kine.Data() << endl;
    St_geant_Maker::instance()->Do(kine.Data());
    //    St_geant_Maker::instance()->Do("gspread   0.015 0.015 42.00");
    TDataSet *tableSet = new TDataSet("20muons");
    return (TDataSet *)tableSet;
  }
#endif
  if (! StVMCMaker::instance()) return 0;
  if (! StarVMCApplication::Instance()) return 0;
  StarMCSimplePrimaryGenerator *gener = (StarMCSimplePrimaryGenerator *) StarVMCApplication::Instance()->GetPrimaryGenerator();
  if (! gener) {
    cout << "Create StarMCSimplePrimaryGenerator(" <<  Npart << "," <<  iD << "," 
	 << pTlow << "," << pThigh<< "," <<Ylow<< "," << Yhigh<< "," 
	 << Philow<< "," << Phihigh<< "," << Zlow<< "," << Zhigh<< "," << "G)" << endl;
    gener =  new 
    StarMCSimplePrimaryGenerator( Npart, iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, "G");
  } else {
    cout << "Set StarMCSimplePrimaryGenerator(" <<  Npart << "," <<  iD << "," 
	 << pTlow << "," << pThigh<< "," <<Ylow<< "," << Yhigh<< "," 
	 << Philow<< "," << Phihigh<< "," << Zlow<< "," << Zhigh<< "," << "G)" << endl;
    gener->SetGenerator( Npart, iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, "G");
  }
  gener->SetSigmasOrigin(.015, 0.015, 42.00);
  StarVMCApplication::Instance()->SetPrimaryGenerator(gener);
  cout << "Set StarMCSimplePrimaryGenerator" << endl;
  TDataSet *tableSet = new TDataSet("20muons");
  return (TDataSet *)tableSet;
}
