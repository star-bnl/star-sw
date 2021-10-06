#include "Riostream.h"
#include "TMath.h"
#include "TDatabasePDG.h"
TDataSet *CreateTable() {
  gEnv->SetValue("FixedSigmaX", 0.0250);
  gEnv->SetValue("FixedSigmaY", 0.0250);
  gEnv->SetValue("FixedSigmaZ", 0.0250);
  StarMCPythia6PrimaryGenerator *gener = (StarMCPythia6PrimaryGenerator *) StarVMCApplication::Instance()->GetPrimaryGenerator();
  if ( gener && ! gener->IsA()->InheritsFrom( "StarMCPythia6PrimaryGenerator" ) ) {
    delete gener; gener = 0;
  }
  if (! gener) gener =  new 		 StarMCPythia6PrimaryGenerator();
  else         gener->SetGenerator();
  StarVMCApplication::Instance()->SetPrimaryGenerator(gener);
  cout << "Set StarMCPythia6PrimaryGenerator" << endl;
  TDataSet *tableSet = new TDataSet("StarMCPythia6PrimaryGenerator");
  return (TDataSet *)tableSet;
}
