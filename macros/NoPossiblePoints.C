void NoPossiblePoints() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  TCanvas *cN = new TCanvas("No.of Possible Points","No.of Possible Points");
  TCanvas *cR = new TCanvas("Ratio of No.of Possible to Fit Points","Ratio of No.of Possible to Fit Points");
  TLegend *lN = new TLegend(0.2,0.6,0.5,0.9);
  TLegend *lR = new TLegend(0.2,0.6,0.5,0.9);
  Int_t color = 0;
  TString same("eh");
  TString name;
  //  gStyle->SetOptStat(0);
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    name = f->GetName();
    name.ReplaceAll(".root",""); cout << "File " << name.Data() << " same " << same.Data() << endl;
    color++;
    TTree *MuDst = (TTree *) f->Get("MuDst");
    if (! MuDst) continue;
    MuDst->SetMarkerStyle(20);
    MuDst->SetMarkerColor(color);
    MuDst->SetLineColor(color);
    cN->cd();
    MuDst->Draw(Form("GlobalTracks.mNHitsPoss>> No%s(51,-0.5,50.5)",name.Data()),"",same);
    TH1 *h = (TH1 *) f->Get(Form("No%s",name.Data()));
    if (! h) continue;
    lN->AddEntry(h,name.Data());
    if (same == "eh") lN->Draw();
    h->SetNormFactor(1.);
    cR->cd();
    MuDst->Draw(Form("GlobalTracks.mNHits/GlobalTracks.mNHitsPoss>> R%s(51,-0.01,1.01)",name.Data()),"",same);
    h = (TH1 *) f->Get(Form("R%s",name.Data()));
    if (! h) continue;
    h->SetNormFactor(1.);
    lR->AddEntry(h,name.Data());
    if (same == "eh") {
      lR->Draw();
      same = "sameseh";
    }
  }
}
