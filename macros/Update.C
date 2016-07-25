//________________________________________________________________________________
void Update(const Int_t nx =101,const Int_t ny=11) {
  //  TFile *newf = new TFile(tFName,"update");
  TH2   *mean = gDirectory->Get("mean");   if(! mean)  return; mean->Reset();
  TH2   *sigma = gDirectory->Get("sigma"); if(! sigma) return; sigma->Reset();
  TH2   *chisq = gDirectory->Get("chisq"); if(! chisq) return; chisq->Reset();
  for (Int_t i=1; i<=nx; i++) {
    for (Int_t j=1; j<=ny; j++) {
      TH1 *proj = (TH1 *) gDirectory->Get(Form("bin_%i_%i",i,j));
      if (! proj) continue;
      TF1 *func = proj->GetFunction("func");
      if (! func) continue;
      Double_t p0 = func->GetParameter(0), dp0 = func->GetParError(0);
      Double_t p1 = func->GetParameter(1), dp1 = func->GetParError(1);
      if (p1 < 0) p1 = - p1;
      if (p1 > 0.1) continue;
      if (p0 < -0.5 || p0 > 0.5) continue;
      Double_t ch2  = func->GetChisquare();
      mean->SetBinContent(i,j,p0);
      mean->SetBinError(i,j,dp0);
      sigma->SetBinContent(i,j,p1);
      sigma->SetBinError(i,j,dp1);
      chisq->SetBinContent(i,j,ch2);
    }
  }  
#if 0
  mean->Write();
  sigma->Write();
  chisq->Write();
#endif
}
