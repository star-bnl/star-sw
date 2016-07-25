//void bFitAll(const Char_t *name3D = "zbgx");
TFile *file = 0;
void runbFunc(Int_t i=0, Int_t j=0) {
  //  file = TFile::Open("hist223.root");
  //  file = TFile::Open("/star/data05/calib/fisyak/Histograms/hist230.root");
  //  file = TFile::Open("/star/data05/calib/fisyak/dEdx/Hist263P02gh1/2257004_0010.root");
  //  file = TFile::Open("Hist295P02gh1.root");
  TDirectory *dir = gDirectory;
  gSystem->Load("StBichsel");
  //  gInterpreter->ProcessLine(".L Bichsel.cxx+");
  gInterpreter->ProcessLine(".L bFitMip.C");
  //  gInterpreter->ProcessLine(".T");
  dir->cd();
  //  bFit(hyp,i,j);
  bFitMip(i,j);
}
