struct BPoint_t {
  Float_t i, dE, log10dEdx, f; 
};
BPoint_t BPoint;
void MakeNTupleFromAscii2(Char_t *FileName="p10gr63.data") {
  TString fName(FileName);
  fName.ReplaceAll(".data",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","Bichsel a b","i:dE:log10dEdx:f");
  FILE *fp = fopen(FileName,"r");
  if (! fp ) return;
  char line[121];
  Int_t i = 0;
  fgets(&line[0],120,fp);
  while (fgets(&line[0],120,fp)) {
    sscanf(&line[0],"%f%f%f%f",&BPoint.i,&BPoint.dE,&BPoint.log10dEdx,&BPoint.f);
    FitP->Fill(&BPoint.i);
    i++;
    if (i%1000 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
