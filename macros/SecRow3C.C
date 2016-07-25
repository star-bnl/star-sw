void SecRow3C() {
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  const Char_t *names[3] = {"WE","W","E"};
  const Char_t*cuts[3] = {"","i<=12&&","i>12&&"};
  TProfile *profs[3];
  TString opt("profg");
  TF1 *pol1 = new TF1("Pol1","[0]+[1]*x",0.5,45.5);
  pol1->SetParameters(0,0);
  ofstream out;
  out.open("SecRow3C.data", ios::app);
  cout << gDirectory->GetName() << endl;
  out << "/* \t" << gDirectory->GetName() << " */" << endl;
  FitP->Draw("mu >> I","i&&j&&j<=13&&abs(mu)<10");
  TH1 *I = (TH1 *) gDirectory->Get("I");
  Double_t meanI = I->GetMean();
  Double_t rmsI  = I->GetRMS();
  FitP->Draw("mu >> O","i&&j&&j>13&&abs(mu)<10");
  TH1 *O = (TH1 *) gDirectory->Get("O");
  Double_t meanO = O->GetMean();
  Double_t rmsO  = O->GetRMS();
  for (Int_t i = 0; i < 3; i++) {
    FitP->SetMarkerColor(i+1);
    TString title("mu:j");
    TString plot = title; plot += Form(" >> %s",names[i]);
    TString cut(Form("(%si&&j&&(j<=13&&abs(mu-%f)<%f || j>13&&abs(mu-%f)<%f))/(dmu**2)",cuts[i],
		     meanI,5*rmsI,meanO,5*rmsO));
    title += " | ";
    title += cut;
    cout << title << endl;
    profs[i] = new TProfile(names[i],title,45,0.5,45.5);
    profs[i]->SetMarkerStyle(20);
    profs[i]->SetMarkerColor(i+1);
    FitP->Draw(plot,cut,opt);
    if (! i) opt += "sames";
    if (! profs[i]) continue;
    profs[i]->Fit(pol1,"er","",0,13.5); 
    cout << "/* " << profs[i]->GetName() << " */\t" << pol1->GetParameter(0) << ",\t" << pol1->GetParameter(1) << endl;
    out << "/* "  << profs[i]->GetName() << " */\t" << pol1->GetParameter(0) << ",\t" << pol1->GetParameter(1);
    profs[i]->Fit(pol1,"er+","",13.5,45.5); 
    cout << ",\t" << pol1->GetParameter(0) << ",\t" << pol1->GetParameter(1) << "," << endl;
    out << ",\t" << pol1->GetParameter(0) << ",\t" << pol1->GetParameter(1) << "," << endl;
  }
  opt = "";
  TLegend *leg = new TLegend(0.4,0.75,0.9,0.9);
  for (Int_t i = 0; i < 3; i++) {
    if (! profs[i]) continue;
    profs[i]->SetStats(0);
    profs[i]->Draw(opt);
    TString T(names[i]);
    pol1 = (TF1 *) profs[i]->GetListOfFunctions()->FindObject("Pol1");
    if (pol1) {
      T += Form("%6.3f +/- %6.3f + (%6.3f +/- %6.3f) * row",
		pol1->GetParameter(0), pol1->GetParError(0),
		pol1->GetParameter(1), pol1->GetParError(1));
    }
    leg->AddEntry(profs[i],T);
    opt = "sames";
  }
  leg->Draw();
  out.close();
}
