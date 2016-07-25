void presDraw(){
  TFile *tpcGas = new TFile("./gasForYuri/tpcGas.root");
  TNtuple  *gas = (TNtuple  *) tpcGas->Get("gas");
  gas->SetMarkerStyle(20);
  gas->SetMarkerSize(0.3);
  gas->SetMarkerColor(2);
  gas->Draw(" 2.35801e+01 -3.42114e+00*log(press):(itime-978325200)/24/3600","press>950");
  htemp->SetTitle("Gain as 2.35801e+01 -3.42114e+00*log(pressure)");
  htemp->SetXTitle("days");
  TFile *Timehist213 = TFile::Open("Histograms/Timehist213.root");
  TH1 *htemp = gROOT->Get("htemp");
  TNtuple  *FitP = (TNtuple  *) Timehist213->Get("FitP");
  FitP->SetMarkerStyle(20);
  FitP->SetMarkerSize(0.4);
  FitP->SetMarkerColor(4);
  FitP->Draw("mu:(x-978325200)/24/3600","","same");
}
