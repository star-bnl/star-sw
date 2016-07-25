void AvScale() {
  TList *list = gROOT->GetListOfFiles();
  if (! list) return;
  TIter next(list);
  TFile *f = 0;
  while (f = (TFile *) next()) {
    TH1D *IU = (TH1D*) f->Get("hdEUI");
    TH1D *I =  (TH1D*) f->Get("hdEI");
    TH1D *OU = (TH1D*) f->Get("hdEUO");
    TH1D *O =  (TH1D*) f->Get("hdEO");
    Double_t ScaleI, ScaleO;
    ScaleI = ScaleO = -9999;
    if (IU && I) ScaleI = TMath::Exp(I->GetMean() - IU->GetMean());
    if (OU && O) ScaleO = TMath::Exp(O->GetMean() - OU->GetMean());
    cout << f->GetName() << "\tI\t" << ScaleI << "\tO\t" << ScaleO << endl;
  }
}
