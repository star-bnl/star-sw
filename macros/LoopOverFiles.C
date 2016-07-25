TH1D *Diff(TH1D *first=0, TH1D *second=0) {
  if (! first && ! second) return 0;
  TH1D *diff = new TH1D(*first);
  diff->SetName(Form("Diff%s",first->GetName()));
  diff->Add(second,-1.);
  return diff;
}
//________________________________________________________________________________
void LoopOverFiles(Int_t iSigma = 0, Int_t nFiles = 5, const Char_t *hname = "DLKpiNPL", const Char_t *fname = "Kpi_D0BCut") {
  TSeqCollection  *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t iplot = 0;
  TCanvas *c = new TCanvas(Form("Sigma%i",iSigma),Form("Sigma%i",iSigma));
  c->DrawFrame(0,0,0.2,250);
  TLegend *leg = new TLegend(0.65,0.6,0.9,0.9,"");
  TIter next(files);
  TFile *f = 0;
  while ((f = (TFile *) next())) {
    f->cd();
    TString FN(gDirectory->GetName());
    cout << "File : " << FN << endl;
    if (! FN.Contains(fname)) continue;
    TString P(hname); P += "p";
    if (iSigma > 0) P += iSigma;
    TString N(hname); N += "n";
    if (iSigma > 0) N += iSigma;
    TH1D* p = (TH1D *) gDirectory->Get(P);
    if (! p) {
      cout<< "Histogram " << P << " is not found" << endl;
      continue;
    }
    TH1D* n = (TH1D *) gDirectory->Get(N);
    if (! n) {
      cout<< "Histogram " << N << " is not found" << endl;
      continue;
    }
    TH1D *d = Diff(p,n); cout << d->GetName() << "\tNo.Entries = " << d->GetEntries() << endl;
    d->SetLineColor(iplot+1);
    d->SetMarkerStyle(20);
    d->SetMarkerColor(iplot+1);
#if 0
    if (iplot) d->Draw("same");
    else      {d->SetStats(0), d->Draw();}
#else
    d->Draw("same");
    d->Draw("samee");
#endif
    FN.ReplaceAll(".root","");
    leg->AddEntry(d,FN);
    iplot++;
    if (iplot == 4) iplot++;
  }
  leg->Draw();
}
