void DrawQcm() {
  TNtuple* FitP = (TNtuple *) gDirectory->Get("FitP");
  Double_t QcmMax = 1000;
  Int_t N = 100;
  Double_t xbins[100];
  Double_t Qcm = 0;
  xbins[0] = Qcm;
  Double_t dQcm = 2; 
  Int_t nn = 0;
  for (Int_t bin = 1; bin < N && Qcm < QcmMax; bin++) {
    if (Qcm > 100) dQcm =  10;
    if (Qcm > 150) dQcm =  20;
    if (Qcm > 200) dQcm =  30;
    if (Qcm > 400) dQcm = 100;
    Qcm += dQcm;
    xbins[bin] = Qcm;
    nn++;
  }
  TProfile *pInner = (TProfile *) gDirectory->Get("pInner");
  if (pInner) delete pInner;
  TProfile *pInner = new TProfile("pInner","Relative gas gain for Inner and Outer sectors versus accumulated charge on anode wires from Run X data",nn,xbins," ");
  pInner->SetXTitle("Accumulated Charge since 01/01/2009 per wire length (#muC/cm)"); 
  pInner->SetYTitle("Relative Gas Gain (%)");
  pInner->SetMarkerStyle(20);
  pInner->SetMarkerColor(2);
  FitP->Draw("100*mu:y>>pInner","(i&&j&&i<=13&&mu>-0.02)/(dmu**2)","profw");
  pInner->Draw();
  TProfile *pOuter = new TProfile("pOuter","Aging for Outer sectors",100,0,200," ");
  pOuter->SetMarkerStyle(20);
  pOuter->SetMarkerColor(3);
  FitP->Draw("100*mu:y>>pOuter","(i&&j&&i>13)/(dmu**2)","profwsame");
  TLegend *leg = new TLegend(0.6,0.6,0.85,0.85);
  leg->AddEntry(pInner,"Inner Sector");
  leg->AddEntry(pOuter,"Outer Sector");
  leg->Draw();
}
