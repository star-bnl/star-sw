/* 
   root.exe Load.C hold/15040049.ClnoW.root AddHeader.C 
*/
void AddHeader() {
  StEvtHddr *hddr = (StEvtHddr *) gDirectory->Get("EvtHddr");
  if (! hddr) {cout << "Cannot find header" << endl; return;}
  hddr->Print("");
  TString InName(gSystem->BaseName(gDirectory->GetName()));
  Int_t index = InName.Index(".ClnoW.root");
  TString OutName(InName);
  if (index > 0)  OutName = TString(InName,index);
  OutName.ReplaceAll(".root","");
  OutName += ".ClnoW.Fit.g3.LangauIFreQ.All.root";
  TFile *fOut = new TFile(OutName,"update");
  if (! fOut) {cout << "Cannot open file " << OutName.Data() << endl; return;}
  hddr->Write();
}
