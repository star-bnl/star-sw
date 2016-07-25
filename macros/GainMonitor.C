static const Char_t *FNames[] = {
  "GainMonitorHist243P02gh1",
  "GainMonitorbbcTrigger_ReversedFullField_Hist243P02gh1",
  "GainMonitorFPDEMCproduction_FullField_Hist243P02gh1",
  "GainMonitorfpdTrigger_ReversedFullField_Hist243P02gh1",
  "GainMonitorMinBiasVertex_ReversedFullField_Hist243P02gh1",
  "GainMonitorppMinBias_FullField_Hist243P02gh1",
  "GainMonitorppMinBias_ReversedFullField_Hist243P02gh1",
  "GainMonitorproductionCentral1200_ReversedFullField_Hist243P02gh1",
  "GainMonitorproductionCentral600_FullField_Hist243P02gh1",
  "GainMonitorproductionCentral_FullField_Hist243P02gh1",
  "GainMonitorproductionCentral_ReversedFullField_Hist243P02gh1",
  "GainMonitorProductionMinBias_FullField_Hist243P02gh1",
  "GainMonitorProductionMinBias_ReversedFullField_Hist243P02gh1",
  0};
TCanvas *c1;
void GainMonitor(){
TProfile *temps[20];
  c1 = new TCanvas();
  c1->DrawFrame(75.,0.75,115.,1.0);
  TLegend *leg = new TLegend(0.71,0.11,1.00,0.89,"");//TLegend(0.79,0.91,0.89,0.89,"");
  Char_t *name = 0;
  for (int i=0; (name = FNames[i]); i++) {
    cout << "i:" << i << " Name: " << name;
    TString rfname(name);
    rfname += ".root";
    cout << "Root File : " << rfname;
    TFile *f = new TFile(rfname.Data());
    if (! f) goto ENDL;
    TString title(name);
    title.ReplaceAll("GainMonitor","");
    title.ReplaceAll("_Hist243P02gh1","");
    title.ReplaceAll("Hist243P02gh1","");
    if (title == "") title = "All";
    cout << " Title: " << title;
    int marker = 20;
    int color  = i+1;
    if (color > 7) {marker++; color -= 6;}
    TNtuple  *FitP = (TNtuple  *) gDirectory->Get("FitP");
    if (! FitP) goto ENDL;
    FitP->SetMarkerStyle(marker); FitP->SetMarkerColor(color);  
    FitP->Draw("exp(mu):exp(x) >> htemp","","profsame");
    temps[i] = (TProfile *)  gDirectory->Get("htemp");
    if (! temps[i]) goto ENDL;
    leg->AddEntry(temps[i],title.Data(),"p");
  ENDL: 
    //    delete f;
    cout << endl;
  }
  leg->Draw();
}
