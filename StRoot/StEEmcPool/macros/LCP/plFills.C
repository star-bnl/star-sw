TFile *outH, *inpH;
const int mxR=3;


addFills(TString fill="F2201", TString wrkDir="./") {
  wrkDir="/star/data04/sim/balewski/LcpRun2/setEta10/";

  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);

  TString fname=wrkDir+"/r"+fill+".hist.root";
  
  inpH=new TFile(fname);
  inpH->ls();
  //  (inpH->Get("r1All"))->Draw();
  (inpH->Get("r1Pt1"))->Draw();

}
