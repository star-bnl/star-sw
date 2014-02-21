/*
  Example macro for plotting the amount of material in front of the TPC.
 */
void mkBudgetSet( 
    const Char_t *filename,   // ROOT file containing geometry
    const Char_t *top="CAVE", // TOP level to probe
    const Double_t rmin=0.00, // minimum radius
    const Double_t rmax=46.6, // maximum radius (46.6 is min radius of TPC inner field cage)
    const Double_t zmin=-5000.,
    const Double_t zmax=+5000.
)
{
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("libEve.so");
  gSystem->Load("StarAgmlViewer.so");
  
  gStyle->SetHistMinimumZero(); // braindead root default

  TFile *file = TFile::Open(filename,"update");
  TGeoManager::Import(filename);

  StarAgmlChecker checker( gGeoManager );
  TObjectSet *set = (TObjectSet *)
    checker.MaterialPlot( top, 50, -2.50, 2.50, 360.0, -180.0, 180.0, rmin, rmax, zmin, zmax  );
  set->Write();
  
  // Now generate a plot
  TH2F *hist   = (TH2F *)set->GetObject();
  TH1F *budget = (TH1F *)hist->ProjectionX();

  TString name = filename;
  name.ReplaceAll("-agml.root","");  name.ReplaceAll("-agstar.root","");
  
  TString title = Form("Material in front of TPC [%s];#eta;depth [1/#chi_{0}]",name.Data());
  budget->SetTitle(title);
  budget->SetFillColor(4);
  budget->SetFillStyle(1);
  TCanvas *canvas = new TCanvas("canvas","material budget",500,500);
  budget->Scale( 1.0 / 360.0 );
  budget->Draw();
  budget->GetXaxis()->SetRangeUser(-1.0,+1.0);

  canvas->Print(name + "-budget.png");


  file->Close();
  delete file;

}
