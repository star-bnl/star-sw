struct BPoint_t {
  Float_t i, West, dWest, East, dEast; 
};
const Char_t *vNames = "i:West:dWest:East:dEast";
BPoint_t BPoint;
void MakeNTupleFromAscii4(Char_t *FileName="Drift.data") {
  TString fName(FileName);
  fName.ReplaceAll(".data",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","Drift",vNames);
  FILE *fp = fopen(FileName,"r");
  if (! fp ) return;
  char line[121];
  Int_t i = 0;
  fgets(&line[0],120,fp);
  BPoint.i = 0;
  while (fgets(&line[0],120,fp)) {
    BPoint.i++;
    sscanf(&line[0],"%f%f%f%f",&BPoint.West, &BPoint.dWest, &BPoint.East, &BPoint.dEast);
    FitP->Fill(&BPoint.i);
  }
  fclose(fp);
  f->Write();
}
//________________________________________________________________________________
void Draw() {
  TNtuple  *FitP = (TNtuple  *) gDirectory->Get("FitP");
  if (! FitP ) return;
  FitP->Draw("West:East>>WE","dWest>0 && dEast >0","colz");
  TH2* WE = (TH2*) gDirectory->Get("WE");
  if (! WE) return;
  FitP->Draw("West:East>>WEp","dWest>0 && dEast >0","prof");
  TProfile* WEp = (TProfile*) gDirectory->Get("WEp");
  if (! WEp) return;
  WEp->SetMarkerStyle(20);
  WEp->Fit("pol1");
  WE->Draw("colz");
  WEp->Draw("sames");
}
