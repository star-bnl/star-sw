struct gasGain_t {
  Float_t time;
  Float_t gain;
  Float_t pressure;
};
gasGain_t Gain;
void pressure() {
  TFile *Timehist213 = TFile::Open("Histograms/Timehist213.root");
  TFile *f = new TFile("./gasForYuri/tpcGas.root");
  TH1D *mu = (TH1D*) Timehist213->Get("mu");
  TProfile *prof = new TProfile("prof","",
				mu->GetNbinsX(),
				mu->GetXaxis()->GetXmin(),
				mu->GetXaxis()->GetXmax());
  TNtuple  *gas = (TNtuple  *) f->Get("gas");
  gas->Draw("log(press):itime>>prof","press>0");
  TH1D *nu = new TH1D(*mu); nu->SetName("Nu");
  nu->Divide(prof);
  TNtuple *gain = new TNtuple("gain","gain pressure corelation",
		      "time:gain:pressure");
  Int_t nx = mu->GetNbinsX();
  for (Int_t i = 1; i <= nx; i++) {
    Gain.time = i;
    Gain.gain = mu->GetBinContent(i);
    Gain.pressure = prof->GetBinContent(i);
    gain->Fill(&Gain.time);
  }
  /*
  
  TFile *tpcGas = new TFile("./gasForYuri/tpcGas.root");
  TNtuple  *gas = (TNtuple  *) tpcGas->Get("gas");
  gas->SetMarkerStyle(20);
  gas->SetMarkerSize(0.3);
  gas->SetMarkerColor(2);
  gas->Draw(" 2.35801e+01 -3.42114e+00*log(press):(itime-978325200)/24/3600","press>950")
  TFile *Timehist213 = TFile::Open("Histograms/Timehist213.root");
  htem->SetTitle("Gain as 2.35801e+01 -3.42114e+00*log(pressure)");
  htemp->SetXTitle("days");
  TNtuple  *FitP = (TNtuple  *) Timehist213->Get("FitP");
  FitP->SetMarkerStyle(20);
  FitP->SetMarkerSize(0.3);
  FitP->SetMarkerColor(4);
  FitP->Draw("mu:(x-978325200)/24/3600","","same");
  
//    gain->Draw("gain:pressure","gain<0&&gain>-.2&&pressure>0","prof")
//    htemp->Fit("pol1")
   */

}
