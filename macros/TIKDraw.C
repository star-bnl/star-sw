const Int_t BL[4] = {8, 12, 16, 20}; // ladders in barrel
const Int_t BW[4] = {4,  6,  7, 16}; // wafers in barrel
void TIKDraw(Int_t barrel = 1) {
  Int_t nx = BL[barrel-1];
  Int_t ny = BW[barrel-1];
  TCanvas *c1 = new TCanvas(Form("TimeBin%i",barrel),Form("Barrel %i",barrel) ,10,10,600,800);
  cout << "nx/ny\t" << nx << "/" << ny << endl;
  c1->Divide(2*ny,nx);
  for (Int_t ladder = 1; ladder <= nx; ladder++) {
    for (Int_t wafer = 1; wafer <= ny; wafer++) {
      for (Int_t hybrid = 1; hybrid <= 2; hybrid++) {
	Int_t ij = hybrid + 2*(wafer-1) + 2*ny*(ladder-1);
	c1->cd(ij);
	THStack *hs = new THStack(Form("stack%i_%i_%i",ladder,wafer,hybrid),Form("B%i L%02i W%02i H%i",barrel,ladder,wafer,hybrid));
	for (Int_t anode =1; anode <=3; anode++) {
	  TString Name("timeB");
	  Name += Form("L%02iB%iW%02iH%iA%i", ladder, barrel, wafer, hybrid, anode);
	  TH1F *hist = (TH1F *) gDirectory->Get(Name);
	  if (! hist) continue;
	  hist->SetLineColor(anode);
	  hs->Add(hist);
	}	   
	hs->Draw("nostack");
      }
    }
  } 
}
