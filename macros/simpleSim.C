void simpleSim(Int_t n=1000, Float_t coupling=0.1)
{
  // define histograms
  TH1F* histIn  = new TH1F("histIn",  0, 100, 0, 1000);
  TH1F* histOut = new TH1F("histOut", 0, 100, 0, 1000);
  // define signal containers
  Float_t signal[5];
  Float_t signalCoupled[5];
  // define mean variables for testing that the mean is the same
  Float_t meanIn  = 0;
  Float_t meanOut = 0;
  for(Int_t i = 0; i < n; i++) {
    for(Int_t j = 0; j < 5; j++) {
      // generate signals
      signal[j] = gRandom->Landau(200, 30);
    }
    for(Int_t j = 0; j < 5; j++) {
      // fill signals in histograms
      histIn->Fill(signal[j]);
      meanIn += signal[j]/n/5.0;
    }
    for(Int_t j = 0; j < 5; j++) {
      // low is in principle j-1 and high is j+1, but they are cyclic 
      // so for j=0 low is not -1 but 4 and for j=4 high is not 5, but 0
      Int_t low = (j+4)%5;
      Int_t high = (j+1)%5;
      //      cout << low << ", " << high << endl;
      // derive coupled signals
      signalCoupled[j] = coupling*(signal[low]+signal[high])
	+(1.0-2.0*coupling)*signal[j];
    }
    for(Int_t j = 0; j < 5; j++) {
      // fill coupled signals in histograms
      histOut->Fill(signalCoupled[j]);
      meanOut += signalCoupled[j]/n/5.0;
    }
  }
  cout << meanIn << " vs " << meanOut << endl;
  cout << histIn->GetMean() << " vs " << histOut->GetMean() << endl;
  // draw histograms
  histOut->SetLineColor(4);
  histOut->Draw();
  histIn->Draw("SAME");
}
