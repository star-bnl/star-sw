struct BPoint_t {
  Float_t i, dE, e2, e1, f;
};
BPoint_t BPoint;
void MakeNTupleFromAscii3(Char_t *FileName="CH4.data") {
  TString fName(FileName);
  fName.ReplaceAll(".data",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","Bichsel's table for CH4","i:dE:e2:e1:f");
  FILE *fp = fopen(FileName,"r");
  if (! fp ) return;
  char line[121];
  Int_t i = 0;
  fgets(&line[0],120,fp);
  while (fgets(&line[0],120,fp)) {
    sscanf(&line[0],"%f%f%f%f%f",&BPoint.i,&BPoint.dE,&BPoint.e2,&BPoint.e1,&BPoint.f);
    BPoint.f *= 100;
    FitP->Fill(&BPoint.i);
    i++;
    if (i%1000 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
