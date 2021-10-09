#include "Riostream.h"
#include "TMath.h"
#include "TDatabasePDG.h"
TDataSet *CreateTable() {
  gEnv->SetValue("FixedSigmaX", 0.0250);
  gEnv->SetValue("FixedSigmaY", 0.0250);
  gEnv->SetValue("FixedSigmaZ", 0.0250);
  StarMCTTreePrimaryGenerator *gener = (StarMCTTreePrimaryGenerator *) StarVMCApplication::Instance()->GetPrimaryGenerator();
  if ( gener && ! gener->IsA()->InheritsFrom( "StarMCTTreePrimaryGenerator" ) ) {
    delete gener; gener = 0;
  }
  if (! gener) gener =  new 		 StarMCTTreePrimaryGenerator();
  else         gener->SetGenerator();
  gener->SetSpread(0.1,0.1,11.);
  StarVMCApplication::Instance()->SetPrimaryGenerator(gener);
  cout << "Set StarMCTTreePrimaryGenerator" << endl;
  TDataSet *tableSet = new TDataSet("StarMCTTreePrimaryGenerator");
  return (TDataSet *)tableSet;
}
