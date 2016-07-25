void marks () {
   TFile *f = TFile::Open("hsimple.root"); //from tutorials
   TTree *ntuple = (TTree*)f->Get("ntuple");
   Int_t n = ntuple->Draw("py:px","pz/5","a");
   TMarker *m;
   for (Int_t i=0;i<n;i++) {
      m = new TMarker(ntuple->GetV1()[i],ntuple->GetV2()[i],24);
      m->SetMarkerSize(ntuple->GetW()[i]);
      m->Draw();
   }
}
