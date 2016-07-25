TFile *rootf = 0;
Int_t Nevent = 0;
TH1D *pxC[2];
void SpaceCharge(Int_t NFile = 0) {
  const Int_t NF = 5;
  Char_t *run[NF] = {"pulser",
		     "Hist309P02gi2","Hist309P02gi1",
		     "Hist309AuAu200Min","Hist309dAu200Min"
  };
  TFile *padG = new TFile("padG3.root");
  TH1D  *rG   = (TH1D*) padG->Get("rGeom");
  TF1 *powfit = new TF1("powfit","[0]*pow(x,[1])",65,200);
  powfit->SetParameters(0.5,-2.);
  //  for (Int_t nf = 0; nf < NF; nf++) {
  //    if (nf != NFile) continue;
  Int_t nf = NFile;
  rootf = TFile::Open(Form("%s.root",run[nf]));
  if (! rootf) continue;
  if (nf < 3) {
    TH1 *outputGasTemperature = (TH1 *) rootf->Get("outputGasTemperature");
    if (!outputGasTemperature) continue; 
    Nevent = outputGasTemperature->GetEntries();
    cout << "File: " << nf << "\t" << rootf->GetName() << "\t" << Nevent << "events" << endl;
  }
  else {
    if (nf == 3) Nevent = 4372; // Hist309AuAu200Min
    if (nf == 4) Nevent = 146682; // Hist309dAu200Min
  }
  TString name("C");
  name += run[nf];
  name.ReplaceAll("Hist","");
  TCanvas *c1 = new TCanvas(name,name,2);
  c1->SetGrid();
  //                      total           used in tracks
  const Int_t NH = 2;
  Char_t *HistName[NH] = {"Space2ChargeT","Space2ChargeU"};
  for (int h = 0; h<NH; h++) {
    cout << HistName[h] << endl;
    const TH2D *hist =  (TH2D *) rootf->Get(HistName[h]);
    const Int_t ny = hist->GetNbinsY();
    TAxis *az = hist->GetYaxis();
    if (! hist) continue;
    TH1D *px = hist->ProjectionX();
    TAxis *ar = hist->GetXaxis();
    pxC[h] = new TH1D(*px);
    pxC[h]->SetName(Form("C%s",px->GetName()));
    pxC[h]->Reset();
    const Int_t nx = px->GetNbinsX(); 
    for (int i = 1; i <= nx; i++) {
      Double_t r1 = ar->GetBinLowEdge(i);
      Double_t r2 = ar->GetBinUpEdge(i);
      Double_t s = TMath::Pi()*(r2*r2 - r1*r1);
      Double_t e = rG->GetBinContent(i);
      Double_t v = 1.e6*px->GetBinContent(i)/s/Nevent;
      //	if (r1 < 120.) v *= 2.12104336450742892e+00;
#if 0
      cout << "i\t" << i << "\tr1\t" << r1 
	   << "\tr2\t" << r2
	   << "\ts\t" << s << "\tv\t" << v << "\te\t" << e << endl;
#endif
      if (e > 0.1) {
	pxC[h]->SetBinContent(i,v/e);
// 	Double_t err = 1.e6*px->GetBinError(i)/s/Nevent/e;
// 	pxC[h]->SetBinError(i,err);
      }
      //	else {
      // 	  pxC[h]->SetBinContent(i,0);
      // 	  //	  pxC[h]->SetBinError(i,0);
      // 	}
    }
    //      pxC[h]->Rebin(2);
    pxC[h]->SetLineColor(h+1);
    pxC[h]->SetXTitle("R(cm)                ");
    pxC[h]->SetYTitle("1/(#piR) dE/dR(keV/(cm**2/Event)");
    powfit->SetLineColor(h+1);
    powfit->SetRange(130,190);
    pxC[h]->Fit(powfit,"r");//pxC[h]->Draw();
    powfit->SetRange(60,200);
    powfit->Draw("same");
  }
#if 1
  TLegend *leg = new TLegend(0.4,0.7,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  pxC[0]->SetStats(0);
  name = run[nf];
  name.ReplaceAll("Hist","");
  pxC[0]->SetTitle(name.Data());
  pxC[0]->Draw();
  leg->AddEntry(pxC[0],Form("Total N=%6.2f",pxC[0]->GetFunction("powfit")->GetParameter(1)),"L");
  pxC[1]->Draw("same");
  leg->AddEntry(pxC[1],Form("Used in fit N=%6.2f  ",pxC[1]->GetFunction("powfit")->GetParameter(1)),"L");
  leg->Draw();
#endif
}
