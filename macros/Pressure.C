static const Char_t *FNames[] = {
  "PressureHist243P02gh1",
  "PressurebbcTrigger_ReversedFullField_Hist243P02gh1",
  "PressureFPDEMCproduction_FullField_Hist243P02gh1",
  "PressurefpdTrigger_ReversedFullField_Hist243P02gh1",
  "PressureMinBiasVertex_ReversedFullField_Hist243P02gh1",
  "PressureppMinBias_FullField_Hist243P02gh1",
  "PressureppMinBias_ReversedFullField_Hist243P02gh1",
  "PressureproductionCentral1200_ReversedFullField_Hist243P02gh1",
  "PressureproductionCentral600_FullField_Hist243P02gh1",
  "PressureproductionCentral_FullField_Hist243P02gh1",
  "PressureproductionCentral_ReversedFullField_Hist243P02gh1",
  "PressureProductionMinBias_FullField_Hist243P02gh1",
  "PressureProductionMinBias_ReversedFullField_Hist243P02gh1",
  0};
TCanvas *c1;
void Pressure(){
TProfile *temps[20];
  c1 = new TCanvas();
  TH1* frame = c1->DrawFrame(6.9.,-0.25,6.96,0);
  frame->SetXTitle("log(Pressure [mbar])");
  frame->SetYTitle("z = <log(dEdx/Pion)>");
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
    title.ReplaceAll("Pressure","");
    title.ReplaceAll("_Hist243P02gh1","");
    title.ReplaceAll("Hist243P02gh1","");
    if (title == "") title = "All";
    //    cout << " Title: " << title;
    int marker = 20;
    int color  = i+1;
    if (color > 7) {marker++; color -= 6;}
    TNtuple  *FitP = (TNtuple  *) gDirectory->Get("FitP");
    if (! FitP) goto ENDL;
    FitP->SetMarkerStyle(marker); FitP->SetMarkerColor(color);  
    FitP->Draw("mu:x >> htemp","","profsame");
    temps[i] = (TProfile *)  gDirectory->Get("htemp");
    if (! temps[i]) goto ENDL;
    leg->AddEntry(temps[i],title.Data(),"p");
  ENDL: 
    //    delete f;
    cout << endl;
  }
  leg->Draw();
}
