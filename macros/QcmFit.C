void QcmFit() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (F.Contains("Qcm")) {
      cout << "Found file " << F.Data() << endl;
      break;
    }
  }
  if (! f) return 0;
  f->cd();
  TTree *FitP = (TTree *) f->Get("FitP");
  if (! FitP) return 0;
  for (Int_t row = 1; row <= 45; row++) {
    FitP->Draw(Form("mu:TMath::Log(y)>> Qcm%i",row),Form("(i&&j&&i==%i)/dmu**2",row),"profg");
    TProfile *h = (TProfile *) f->Get(Form("Qcm%i",row));
    if (! h) continue;
    Int_t status = h->Fit("pol3","eq");
    if (status) continue;
    TF1 *pol3 = (TF1 *) h->GetListOfFunctions()->FindObject("pol3");
    if (! pol3) continue;
    cout << "\t" << pol3->GetParameter(0) << "," << pol3->GetParameter(1) << "," <<  pol3->GetParameter(2) << "," <<  pol3->GetParameter(3) 
<< ",// row :" << row << endl; 
  }
}
