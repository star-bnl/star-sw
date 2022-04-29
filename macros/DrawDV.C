/*
  root.exe 2022*.root DrawDV.C
*/
void DrawDV(Double_t x1 = 846e6, Double_t y1 = 5.38, Double_t x2 =862e6, Double_t y2 = 5.61) {
  TCanvas *c1 = new TCanvas("c1","c1");
  TH1* frame = c1->DrawFrame(x1,y1,x2,y2);
  frame->GetXaxis()->SetTimeDisplay(1);
  frame->GetXaxis()->SetTimeFormat("%m/%d/%y%F1995-01-01 00:00:00");
  frame->SetTitle("Drift Velocity versus date");
  c1->Modified();
  c1->Update();
  TLegend *l = new TLegend(0.7,0.3,0.8,0.4);
  TSeqCollection *files = gROOT->GetListOfFiles();
  TIter  next(files, kFALSE);
  TFile *f = 0;  
  Int_t color = 0;
  while ((f = (TFile *) next())) {
    TString name(gSystem->BaseName(f->GetName()));
    name.ReplaceAll(".root","");
    name.Prepend("L");
    color++;
    f->cd();
    TNtuple *RunNT = (TNtuple*) f->Get("RunNT");
    if (! RunNT) continue;
    RunNT->SetMarkerColor(color);
    RunNT->SetMarkerStyle(20);
    RunNT->Draw(Form("dvAll:utime-788936400>>%s",name.Data()),"ok==1","same");
    TH1 *hist = (TH1 *) gDirectory->Get(name);
    l->AddEntry(hist,name);
  }
  l->Draw();
}
