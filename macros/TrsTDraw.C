void TrsTDraw(Int_t opt=1) {
  TTree *TrsT = (TTree *) _file0->Get("TrsT");
  TrsT->SetMarkerStyle(20);
  TrsT->SetMarkerColor(opt%10);
  if (opt == 1) 
    TrsT->Draw("fAnalogSignals.Signal/fSumAnalogs.:fAnalogSignals.pad-(pad+dXpad)>>OuterPadsA",
	       "row>=14&&fAnalogSignals.tb<0 && fSumAnalogs<1e4 && fSumAnalogs>100","prof");
  if (opt == 2)
    TrsT->Draw("fAnalogSignals.Signal/fSumAnalogs:fAnalogSignals.pad-(pad+dXpad)>>InnerPadsA",
	       "row<14&&fAnalogSignals.tb<0 && fSumAnalogs<1e4&& fSumAnalogs>100","prof");
  if (opt == 3) 
    TrsT->Draw("fDigitalSignals.Signal/fSumDigitals:fDigitalSignals.pad-(pad+dXpad)>>OuterPadsD(100,-5,5)",
	       "row>=14&&fDigitalSignals.tb<0 && fSumDigitals>100","prof");
  if (opt == 4)
    TrsT->Draw("fDigitalSignals.Signal/fSumDigitals:fDigitalSignals.pad-(pad+dXpad)>>InnerPadsD(100,-5,5)",
	       "row<14&&fDigitalSignals.tb<0 && fSumDigitals>100","prof");
  if (opt == 5) 
    TrsT->Draw("fAnalogSignals.Signal/fSumAnalogs:fAnalogSignals.tb-(timeBin+dT)>>OuterTimeA",
"row>=14&&fAnalogSignals.pad<0 && fSumAnalogs<1e4 && fSumAnalogs>100","prof");
  if (opt == 6) 
    TrsT->Draw("fAnalogSignals.Signal/fSumAnalogs:fAnalogSignals.tb-(timeBin+dT)>>InnerTimeA",
"row<14&&fAnalogSignals.pad<0 && fSumAnalogs<1e4 && fSumAnalogs>100","prof");
  if (opt == 7) 
    TrsT->Draw("fDigitalSignals.Signal/fSumDigitals:fDigitalSignals.tb-(timeBin+dT) - 4.77954e+00>>OuterTimeD(100,-5,5)",
	       "row>=14&&fDigitalSignals.pad<0 && fSumDigitals>100","prof");
  if (opt == 8) 
    TrsT->Draw("fDigitalSignals.Signal/fSumDigitals:fDigitalSignals.tb-(timeBin+dT) - 3.03434e+00>>InnerTimeD(100,-5,5)",
	       "row<14&&fDigitalSignals.pad<0 && fSumDigitals>100","prof");
  if (opt == 13) // fcf
    TrsT->Draw("fDigitalSignals.Signal/fSumDigitals:fDigitalSignals.pad-(pad+dXpad)>>OuterPadsY(100,-5,5)",
	       "row>=14&&fDigitalSignals.tb<0 && fSumDigitals>100 && abs(fDigitalSignals.pad-(pad-dXpad))<5",
	       "prof");
  if (opt == 14)
    TrsT->Draw("fDigitalSignals.Signal/fSumDigitals:fDigitalSignals.pad-(pad+dXpad)>>InnerPadsY(100,-5,5)",
	       "row<14&&fDigitalSignals.tb<0 && fSumDigitals>100&& abs(fDigitalSignals.pad-(pad-dXpad))<5",
	       "prof");
  if (opt == 17) 
    TrsT->Draw("fDigitalSignals.Signal/fSumDigitals:fDigitalSignals.tb-(timeBin+dT)+3.08770e-01>>OuterTimeY(100,-5,5)",
	       "row>=14&&fDigitalSignals.pad<0 && fSumDigitals>100","prof");
  if (opt == 18) 
    TrsT->Draw("fDigitalSignals.Signal/fSumDigitals:fDigitalSignals.tb-(timeBin+dT)+3.28817e-01>>InnerTimeY(100,-5,5)",
	       "row<14&&fDigitalSignals.pad<0 && fSumDigitals>100","prof");
}
void dEDraw() {
  TString name("dEdx_");
  name += gDirectory->GetName();
  name.ReplaceAll(".root","");
  TCanvas *c1 = new TCanvas(name.Data(),name.Data());
  c1->SetGrid();
  c1->SetLogy();
  TTree *TrsT = (TTree *) gDirectory->Get("TrsT");
  TrsT->SetLineColor(1);
  TrsT->Draw("1e6*abs(fdE)/fdS >>dEGeant","abs(fdE)/fdS<50e-6");
  TrsT->SetLineColor(2);
  TrsT->Draw("1e6*fdED/fdSD >>dED","fdED/fdSD<50e-6");
  TH1D *dEGeant = (TH1D *) gDirectory->Get("dEGeant");
  TH1D *dED = (TH1D *) gDirectory->Get("dED");
  if (dEGeant && dED) {
    dEGeant->Draw();
    dED->Draw("same");
    TPaveStats *st = (TPaveStats*) 
      dED->GetListOfFunctions()->FindObject("stats");
    if (st) {
      //      st->SetX1NDC(newx1); //new x start position
      //      st->SetX2NDC(newx2); //new x end position
      st->Draw();
    }
  }
}
//________________________________________________________________________________
void Norm(Char_t *opt = "M", Int_t color=1) {
  TIter next(gDirectory->GetListOfKeys() );
  TKey *key = 0;
  while ((key = (TKey*) next())) {
    TObject *obj = key->ReadObj();
    if ( obj->IsA()->InheritsFrom( "TProfile" ) ) {
      cout << "Found histogram " << obj->GetName() << endl;
      TProfile *hist = (TProfile *) obj;
      TProfile *newhist = new TProfile(*hist);
      TString Name(opt);
      Name += newhist->GetName();
      newhist->SetName(Name.Data());
      Double_t scale = 1./newhist->GetMaximum();
      newhist->Scale(scale);
      if (color) newhist->SetMarkerColor(color);
    }
  }
}
