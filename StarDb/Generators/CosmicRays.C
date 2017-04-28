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
  if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
    cout << "You cannot run CosmicsRays.C with root4star => abort" << endl;
    return 0;
  } else {
    if (! StVMCMaker::instance()) {
      cout << "You have to StVMCMaker => abort" << endl;
      return 0;
    }
    if (! StarVMCApplication::Instance()) {
      cout << "You have to load StarVMCApplication => abort" << endl;
      return 0;
    }
    StarCosmicRaysGenerator *gener = (StarCosmicRaysGenerator *) StarVMCApplication::Instance()->GetPrimaryGenerator();
    if ( gener && ! gener->IsA()->InheritsFrom( "StarCosmicRaysGenerator" ) ) {
      delete gener; gener = 0;
    }
    if (! gener) gener = new StarCosmicRaysGenerator();
    StarVMCApplication::Instance()->SetPrimaryGenerator(gener);
    cout << "Set StarCosmicRaysGenerator " << endl;
  }
  TDataSet *tableSet = new TDataSet("StarCosmicRaysGenerators");
  return (TDataSet *)tableSet;
 }
