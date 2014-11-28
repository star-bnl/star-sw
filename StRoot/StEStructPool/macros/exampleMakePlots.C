void exampleMakePlots(const char* rootfile){
 
  /* run as:

root.exe exampleMakePlots.C'("/star/data05/scratch/porter/estruct/ytdetadphi02_pc/data/all/all.root")'

  */


 gSystem->Load("StEStructPoolSupport.so");
 gROOT->LoadMacro("makePlots.C");
 gROOT->LoadMacro("setupPalette.C");

 TFile* tf=new TFile(rootfile);
 StEStructSupport ehelp(tf,1);
 TH2F** dedp=(TH2F**)ehelp.buildChargeTypeRFunctions("DEtaDPhi");


TCanvas* c1=new TCanvas("c1");
setupPalette();
gPad->Divide(2,2);
gStyle->SetOptTitle(0);
makePlots(dedp,0);

};
