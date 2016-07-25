void dudvPlots() {
  TTree *T = (TTree *) gDirectory->Get("T");
  TString Name(gSystem->BaseName(gDirectory->GetName()));
  Name.ReplaceAll(".root","");
  TF1 *g = new TF1("F","gaus(0)+pol0(3)",-.4,.4);
  g->SetParameters(640.,0,0.05,410);
  g->SetParName(0,"A");
  g->SetParName(1,"#mu");
  g->SetParName(2,"#sigma");
  g->SetParName(3,"B");
  TCanvas *c1 = new TCanvas(Name,"du");
  c1->Divide(2,4);
  for (Int_t barrel = 1; barrel <= 4; barrel++) {
    for (Int_t j = 0; j < 2; j++) {
      TString name(Form("B%i",barrel));
      if (j == 1) name += "W";
      TString Plot("u-uP>>");
      Plot += name;
      TString Cut(Form("barrel==%i&&abs(u-uP)<0.5",barrel));
      if (j == 1) Cut += "&&abs(v-vP)<0.5";
      c1->cd(j+1 + 2*(barrel-1));
      T->Draw(Plot,Cut);
      TH1* h1 = (TH1 *) gDirectory->Get(name); 
      h1->Fit(g,"ri");
    }
  }
}
