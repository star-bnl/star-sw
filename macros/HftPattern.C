/* 
   Plot HFT hit pattern for global track using Plot.root from TbyTPlots.C
   root.exe *Plots.root HftPattern.C
 */
void HftPattern() {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("Plots")) continue;
    TH2D *h2 = (TH2D*)  f->Get("HftPattern");
    if (! h2) continue;
    FitFiles[NF] = f; NF++;
  }
  TH1D **proj = new TH1D*[NF];
  gStyle->SetOptStat(0);
  TString same("");
  TLegend *l = new TLegend(0.6,0.6,0.9,0.9);
  for (Int_t i = 0; i < NF; i++) {
    f = FitFiles[i];
    if (! f) continue;
    TH2D *h2 = (TH2D*)  f->Get("HftPattern");
    if (! h2) continue;
    proj[i] = h2->ProjectionX(Form("proj%i",i),5,6); // new Global (+,-)
    proj[i]->SetNormFactor(1);
    proj[i]->SetMarkerColor(i+1);
    proj[i]->SetLineColor(i+1);
    proj[i]->Draw(same); same = "same";
    TString fName(gSystem->BaseName(f->GetName()));
    TString Title;
    if      (fName.Contains("HFT2")) Title = "Hit Weight =   2222";
    else if (fName.Contains("HFT3")) Title = "Hit Weight = 112211";
    else if (fName.Contains("HFT4")) Title = "Hit Weight =  12201";
    if (Title != "") l->AddEntry(proj[i],Title);
  }
  l->Draw();
}
