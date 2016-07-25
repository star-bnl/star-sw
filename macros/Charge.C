TFile *f = 0;
Int_t Nevent = 0;
void Charge(const Char_t *HistName = "Space2ChargeT") {
  TCanvas *c1 = new TCanvas(HistName,HistName);
  c1->SetGrid();
  c1->Divide(1,2);
  TString cname(HistName);
  cname += "R";  
  TCanvas *c2 = new TCanvas(HistName,HistName);
  c2->Divide(1,2);
//   Char_t *run[2] = {"productionCentral_ReversedFullField",
// 		    "ProductionMinBias_ReversedFullField"};
  Char_t *run[2] = {"pulser",
		    "Hist309P02gi2"};
  for (Int_t opt = 0; opt < 2; opt++) {
    f = TFile::Open(Form("%s.root",run[opt]));
    if (! f) continue;
    TH1 *outputGasTemperature = (TH1 *) f->Get("outputGasTemperature");
    if (!outputGasTemperature) continue; 
    Nevent = outputGasTemperature->GetEntries();
    cout << "File: " << f->GetName() << "\t" << Nevent << "events" << endl;
    const TH2D *hist =  (TH2D *) f->Get(HistName);
    if (! hist) continue;
    //    Double_t norm = hist->GetEntries()/Nevent;
    //    hist->SetNormFactor(norm);
    hist->SetStats(0);
    c1->cd(opt+1);
    gPad->SetLogz();
    TH2D *hist2 = new TH2D(*hist);
    TH2D *hist3 = new TH2D(*hist);
    hist2->Reset();
    Int_t nx = hist2->GetNbinsX(); 
    Int_t ny = hist2->GetNbinsY();
    TAxis *ar = hist2->GetXaxis();
    TAxis *az = hist2->GetYaxis();
    for (int i = 1; i <= nx; i++) {
      Double_t r1 = ar->GetBinLowEdge(i);
      Double_t r2 = ar->GetBinUpEdge(i);
      Double_t s = TMath::Pi()*(r2*r2 - r1*r1);
      for (int j = 1; j <= ny; j++) {
	Double_t z1 = az->GetBinLowEdge(j);
	Double_t z2 = az->GetBinUpEdge(j);
	Double_t v = 1.e-6*s*(z2 - z1)*Nevent;
	hist2->SetBinContent(i,j,v);
      }
    }
    hist3->Divide(hist2);
    TString title(run[opt]);
    title.ReplaceAll("production","");
    title.ReplaceAll("Production","");
    title.ReplaceAll("_"," ");
    title += " ";
    title += HistName;
    title += " (keV/(cm**3)/Event)";
    TString Name(HistName);
    Name += "N";
    hist3->SetName(Name.Data());
    hist3->SetTitle(title.Data());
    hist3->SetXTitle("R(cm)          ");
    hist3->SetYTitle("Z(cm)          ");
    hist3->Draw("colz");
    TH1D* proj = hist3->ProjectionX();
    TString NewName(proj->GetName());
    NewName += "2";
    TH1* proj2 = proj->Rebin(2,NewName.Data());
    title.ReplaceAll("(keV/(cm**3)/Event)","");
    proj2->SetTitle(title.Data());
    proj2->SetStats(0);
    proj2->SetXTitle("R(cm)       ");
    proj2->SetYTitle("dE(keV/(cm**2)/Event)");
    c2->cd(opt+1);
    proj2->Draw();
  }
}
