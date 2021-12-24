/* AdcMCL - AdcRCL  versus |Z| and AdcRCL and time bucket length
  
  root.exe *ADC3U.root AdcTpcTDraw()
*/
void AdcTpcTDraw() {
  // from TpcTAdc
  // ADC block
  enum {kTPC = 4, kVar = 6, kOpt = 2, ktmBins = 13};
  const Char_t *tpcName[4] = {"I","O","IC","OC"};
  const Char_t *WE[4] = {"W","E","West","East"};
  const Char_t *IO[4] = {"I","O","Inner","Outer"};
  TH3F *io3D[2] = {0};
  TCanvas *c1 = new TCanvas("c1","Z");
  TCanvas *c2 = new TCanvas("c2","AdcLR");
  TObjArray *arr = new TObjArray(4);
  TString samex, samey;
  Int_t colorx = 0, colory = 0;
  for (Int_t io = 0; io < 2; io++) {
    for (Int_t we = 0; we < 2; we++) {
      for (Int_t tb = 0; tb < ktmBins; tb++) {
	TString Name(Form("%s%s_%i",WE[we],IO[io],tb+1));
	TH3F *h3 = (TH3F *) gDirectory->Get(Name);
	if (h3) {
	  if (! io3D[io]) {
	    io3D[io] = new TH3F(*h3);
	    io3D[io]->SetName(IO[io]);
	  } else {
	    io3D[io]->Add(h3);
	  }
	  TH2D *h2zx = (TH2D *) h3->Project3D("zx");
	  h2zx->FitSlicesY(0, 0, -1, 0, "QNR", arr);
	  TH1D *mu = (TH1D *) (*arr)[1];
	  if (mu) {
	    c1->cd(); colorx++; mu->SetMarkerColor(colorx);
	    if (colorx == 1) mu->Draw();
	    else             mu->Draw("same");
	    c1->Update();
	  }
	  TH2D *h2zy = (TH2D *) h3->Project3D("zy");
	  h2zy->FitSlicesY(0, 0, -1, 0, "QNR", arr);
	  TH1D *mu = (TH1D *) (*arr)[1];
	  if (mu) {
	    c2->cd(); colory++; mu->SetMarkerColor(colory);
	    if (colorx == 1) mu->Draw();
	    else             mu->Draw("same");
	    c2->Update();
	  }
	}
      }
    }
  }
}
