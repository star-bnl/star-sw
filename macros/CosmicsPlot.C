void CosmicsPlot() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  Int_t NF = 0;
  TLegend *l = new TLegend(0.2,0.6,0.4,0.8);
  TCanvas *c1 = new TCanvas("c1","c1");
  c1->SetLogy(1);
  TString same;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    FitFiles[NF++] = f;
    TString F(f->GetName());
    F.ReplaceAll(".root","");
    TH1 *sigma = (TH1 *) f->Get("DpT_2");
    if (! sigma) {
      TH2F *DpT = (TH2F *) f->Get("DpT");
      if (! DpT) {
	TTree *CosmicT = (TTree *) f->Get("CosmicT");
	if (CosmicT) {
	  TCanvas *c = new TCanvas(F,F);
	  c->SetLogz(1);
	  c->cd();
	  CosmicT->SetMarkerColor(NF);
	  CosmicT->SetLineColor(NF);
	  CosmicT->Draw("(K.mPti+L.mPti)*TMath::Sqrt(2.)/TMath::Abs(K.mPti-L.mPti):-TMath::Log10(TMath::Abs(K.mPti-L.mPti)/2.)>>DpT(60,-1,2,400,-0.2,0.2)","chi2<200","colz");
	  DpT = (TH2F *) f->Get("DpT");
	}
      }
      if (DpT) {
	DpT->FitSlicesY();
	TH1 *mu = (TH1 *) f->Get("DpT_1");
	mu->SetMarkerStyle(20);
	mu->SetLineColor(NF);
	mu->SetMarkerColor(NF);
	mu->Draw("same");
	sigma = (TH1 *) f->Get("DpT_2");
	sigma->SetLineColor(NF);
	sigma->SetMarkerColor(NF);
	sigma->SetMarkerStyle(21); sigma->Draw("same");
	c->Update();
      }
    }
    if (! sigma) continue;
    l->AddEntry(sigma,F);
    c1->cd();
    sigma->SetStats(0);
    sigma->SetTitle("dp_{T}/p_{T} versus log_{10}p_{T}");
    sigma->SetXTitle("log_{10}p_{T}[GeV/c]");
    if (NF == 1) {sigma->Draw(same); l->Draw(); same = "same";}
    else         {sigma->Draw(same);}
    c1->Update();
  }
}
