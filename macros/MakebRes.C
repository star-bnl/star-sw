struct XY_t {
  Float_t i, phi;
};
XY_t XY;
void MakebRes(Char_t *FileName="bResl.data") {
  f = new TFile("bResl.root","RECREATE");
  XYP = new TNtuple("XYP","Bichsel a b","i:phi");
  FILE *fp = fopen(FileName,"r");
  char line[121];
  Int_t i = 0;
  fgets(&line[0],120,fp);
  while (fgets(&line[0],120,fp)) {
    sscanf(&line[0],"%f%f",&XY.i,&XY.phi);
    XYP->Fill(&XY.i);
    i++;
    if (i%100 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
