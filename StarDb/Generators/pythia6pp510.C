TDataSet *CreateTable() {
  gROOT->LoadMacro("pythia6.C");
  if (pythia6("pp:510")) return 0;
  TDataSet *tableSet = new TDataSet("pythia6:pp:510");
  return (TDataSet *)tableSet;
 }
