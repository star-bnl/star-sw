struct BPoint_t {
  Float_t x, y;
};
BPoint_t BPoint;
void MakeNTupleFromAsciiXY(Char_t *FileName="puls_profile.data"){// Ar.data") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) return;
  TString fName(FileName);
  fName.ReplaceAll(".data",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","XY","x:y");//:D_l:D_p:D_u:w:a:b:c");
  char line[121];
  Int_t i = 0;
  //  fgets(&line[0],120,fp);
  while (fgets(&line[0],120,fp)) {
    sscanf(&line[0],"%f%f",&BPoint.x,&BPoint.y);
    //    sscanf(&line[0],"%f%f%f%f%f%f%f a %f%f%f",&BPoint.i,&BPoint.bg,&BPoint.pc,&BPoint.D_l,&BPoint.D_p,&BPoint.D_u,&BPoint.w,&BPoint.c,&BPoint.a,&BPoint.b);
    FitP->Fill(&BPoint.x);
    i++;
    if (i%10 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
