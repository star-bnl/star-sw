void TestTFractionFitter() {
  // An example application of this fit is given below. For a TH1* histogram
  // ("data") fitted as the sum of three Monte Carlo sources ("mc"):
  //
  TH1F *data = new TH1F("data","data histogram",100,-2,2);
  TH1F *MCs[4];
  MCs[0]  = new TH1F("mc0","1-st MC histogram",100,-2,2);
  MCs[1]  = new TH1F("mc1","2-nd MC histogram",100,-2,2);
  MCs[2]  = new TH1F("mc2","3-rd MC histogram",100,-2,2);
  MCs[3]  = new TH1F("mc3","4-th MC histogram",100,-2,2);
  const Int_t NF = 4;
  //                      e       pi     K     P
  Double_t fracR[4] = {0.05, 0.75, 0.08, 0.12};
  Double_t meanR[4] = {-0.1,  0.0,  0.2,  0.4};
  Double_t sigma = 0.08;
  TH1 *frac = new TH1D("frac","e       pi     K     P fraction",NF,0.5,0.5+NF);
  for (Int_t ix = 1; ix <= NF; ix++) {
    frac->SetBinContent(ix,fracR[ix-1]);
  }
  for (Int_t iev = 1; iev <= 10000; iev++) {
    Int_t f = TMath::Nint(frac->GetRandom());
    Double_t x = gRandom->Gaus(meanR[f-1],sigma);
    data->Fill(x);
    for (Int_t j = 0; j < 100; j++) {
      x = gRandom->Gaus(meanR[f-1],sigma);
      MCs[f-1]->Fill(x,0.01);
    }
  }
  /* 2500 ev
     FCN=-20268.3 FROM MIGRAD    STATUS=CONVERGED     197 CALLS         198 TOTAL
     EDM=5.94133e-07    STRATEGY= 1      ERROR MATRIX ACCURATE 
     EXT PARAMETER                                   STEP         FIRST   
     NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
     1  frac0        4.71992e-02   1.53288e-02   6.29055e-03  -2.33759e-04
     2  frac1        7.42802e-01   2.92369e-02   2.50789e-03   4.76954e-03
     3  frac2        8.16039e-02   1.19976e-02   1.13224e-03   4.20246e-02
     4  frac3        1.28407e-01   1.10724e-02   1.06217e-03   6.33705e-02

     10000 ev
     FCN=-108417 FROM MIGRAD    STATUS=CONVERGED     170 CALLS         171 TOTAL
     EDM=1.5847e-06    STRATEGY= 1      ERROR MATRIX ACCURATE 
     EXT PARAMETER                                   STEP         FIRST   
     NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
     1  frac0        4.74948e-02   8.29831e-03   7.66508e-03  -1.67853e-02
     2  frac1        7.50604e-01   1.51261e-02   2.91889e-03   4.79574e-03
     3  frac2        7.74034e-02   6.27847e-03   1.34961e-03   1.37012e-01
     4  frac3        1.24504e-01   5.54679e-03   1.21921e-03   1.69269e-01
  */
  TObjArray *mc = new TObjArray(NF);        // MC histograms are put in this array
  for (Int_t i = 0; i < NF; i++) mc->Add(MCs[i]);
  TFractionFitter* fit = new TFractionFitter(data, mc); // initialise
  fit->Constrain(1,0.0,1.0);               // constrain fraction 1 to be between 0 and 1
  //  fit->SetRangeX(1,15);                    // use only the first 15 bins in the fit
  Int_t status = fit->Fit();               // perform the fit
  cout << "fit status: " << status << endl;
  if (status == 0) {                       // check on fit status
    TH1F* result = (TH1F*) fit->GetPlot();
    data->Draw("Ep");
    result->Draw("same");
  }
}
// Applying constraints
// ====================
// Fit parameters can be constrained through
//     fit->Constrain(parameter #, lower bound, upper bound);
// Setting lower bound = upper bound = 0 removes the constraint (a la Minuit);
// however, a function
//     fit->Unconstrain(parameter #)
// is also provided to simplify this.
//
// Setting parameter values
// ========================
// The function
//     TVirtualFitter* vFit = fit->GetFitter();
// is provided for direct access to the TVirtualFitter object. This allows to
// set and fix parameter values, and set step sizes directly.
//
// Restricting the fit range
// =========================
// The fit range can be restricted through
//     fit->SetRangeX(first bin #, last bin #);
// and freed using
//     fit->ReleaseRangeX();
// For 2D histograms the Y range can be similarly restricted using
//     fit->SetRangeY(first bin #, last bin #);
//     fit->ReleaseRangeY();
// and for 3D histograms also
//     fit->SetRangeZ(first bin #, last bin #);
//     fit->ReleaseRangeZ();
//
// Weights histograms
// ==================
// Weights histograms (for a motivation see the above publication) can be specified
// for the individual MC sources through
//     fit->SetWeight(parameter #, pointer to weights histogram);
// and unset by specifying a null pointer.
//
// Obtaining fit results
// =====================
// The fit is carried out through
//     Int_t status = fit->Fit();
// where  status  is the code returned from the "MINIMIZE" command. For fits
// that converged, parameter values and errors can be obtained through
//     fit->GetResult(parameter #, value, error);
// and the histogram corresponding to the total Monte Carlo prediction (which
// is not the same as a simple weighted sum of the input Monte Carlo distributions)
// can be obtained by
//     TH1* result = fit->GetPlot();
//
// Using different histograms
// ==========================
// It is possible to change the histogram being fitted through
//     fit->SetData(TH1* data);
// and to change the template histogram for a given parameter number through
//     fit->SetMC(parameter #, TH1* MC);
// This can speed up code in case of multiple data or template histograms;
// however, it should be done with care as any settings are taken over from
// the previous fit. In addition, neither the dimensionality nor the numbers of
// bins of the histograms should change (in that case it is better to instantiate
// a new TFractionFitter object).
//
