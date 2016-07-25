static TFile *Sti = 0;
static TFile *StiCA = 0;
static TFile *Stv = 0;
static TFile *StvCA = 0;
/*
.x  logCpu.C("cpu:tpcHits",0,0,2e5,2e2)
.x  logCpu.C("cpu:tracks",0,0,5e3,2e2)
.x  logCpu.C("cpu:good_tracks",0,0,3e3,2e2)
*/
TCanvas *c1 = 0;
//________________________________________________________________________________
void logCpu(const Char_t *draw="cpu:tpcHits", Double_t x1 = 0, Double_t y1 = 0, Double_t x2 = 0, Double_t y2 = 0, Int_t logY = 0, Int_t prof = 0) {
  if (! Sti)   Sti   = TFile::Open("log_Sti.root");
  if (! StiCA) StiCA = TFile::Open("log_StiCA.root");
  if (! Stv)   Stv   = TFile::Open("log_Stv.root");
  if (! StvCA)   StvCA   = TFile::Open("log_StvCA.root");
  const Int_t NF = 4;
  TFile *files[4] = {Stv, StvCA, Sti, StiCA};
  const Char_t *fNames[4] = {"Stv","StvCA","Sti","StiCA"};
  gStyle->SetOptStat(0);
  c1 = new TCanvas(draw,draw);
  if (logY) c1->SetLogy(1);
  TH1F *frame = 0;
  TString same("");
  if (x1 < x2 && y1 < y2) {
    TString X, Y;
    TString D(draw);
    TObjArray *obj = D.Tokenize(":");
    Int_t nParsed = obj->GetEntries();
    if (nParsed == 2) {
      Y = ((TObjString *) obj->At(0))->GetName();
      X = ((TObjString *) obj->At(1))->GetName();
      frame = c1->DrawFrame(x1,y1,x2,y2);
      frame->SetTitle(Form("%s (seconds) versus no. of %s", Y.Data(), X.Data()));
      frame->SetXTitle(X);
      frame->SetYTitle(Y);
      same = "same";
    }
  }
  TLegend *leg = new TLegend(0.2,0.7,0.4,0.9);
  for (Int_t i = 0; i < NF; i++) {
    if (! files[i]) continue;
    files[i]->cd();
    TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
    FitP->SetMarkerStyle(1);
    FitP->SetMarkerColor(i+1);
    FitP->SetLineColor(i+1);
    TString Draw(draw);
    Draw += Form(" >> h%s",fNames[i]);
    cout << "Draw\t" << Draw.Data() << endl;
    if (! prof)  {
      FitP->Draw(Draw.Data(),"",same); 
      same = "same";
      TH2 *h = (TH2 *) gDirectory->Get(Form("h%s",fNames[i]));
      if (h) leg->AddEntry(h,fNames[i]);
    } else {
      FitP->Draw(Draw.Data(),"","goff");
      TH2 *h = (TH2 *) gDirectory->Get(Form("h%s",fNames[i]));
      if (! h) continue;
      TH1 *h1 = h->ProfileX();
      h1->SetMarkerStyle(20);
      h1->Draw(same);
      same = "same";
      if (h1) leg->AddEntry(h1,fNames[i]);
    }
  }
  leg->Draw();
  TString pngName("");
  c1->Modified();
  c1->Update(); pngName = c1->GetName();
  if (prof) pngName += "Prof";
  pngName += ".png"; 
  pngName.ReplaceAll(":","_");
  c1->SaveAs(pngName);
}
