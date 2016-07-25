void DrawT0() {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  TCanvas *c1 = (TCanvas*)gROOT->GetListOfCanvases()->FindObject("c1");
  if (!c1) c1 = new TCanvas("c1","c1");
  c1->Divide(1,2);
  c1->cd(1);
  TH1F *frameI = c1->DrawFrame(0.5,-0.2,24.5,0.2,"Inner T0 versus sector");
  TLegend *lI = new TLegend(0.2,0.2,0.6,0.6);
  lI->Draw();
  gStyle->SetOptStat(0);
  c1->cd(2);
  TH1F *frameO = c1->DrawFrame(0.5,-0.2,24.5,0.2,"Outer T0 versus sector");
  TLegend *lO = new TLegend(0.2,0.2,0.6,0.6);
  lO->Draw();
  TIter next(files);
  TFile *f = 0;
  Int_t color = 1;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString Name(f->GetName());
    Name.ReplaceAll(".root","");
    Name.ReplaceAll("GainT0.","");
    TNtuple *FitP = (TNtuple *) f->Get("FitP");
    if (! FitP) continue;
    FitP->SetMarkerColor(color); color++;
    c1->cd(1);
    FitP->Draw("t:s>>Si(24,0.5,24.5)","r<=13","profsame");
    TH1 *h = (TH1 *) f->Get("Si");
    if (h) lI->AddEntry(h,Name.Data());
    c1->cd(2);
    FitP->Draw("t:s>>So(24,0.5,24.5)","r>13","profsame");
    TH1 *h = (TH1 *) f->Get("So");
    if (h) lO->AddEntry(h,Name.Data());
  }
}
