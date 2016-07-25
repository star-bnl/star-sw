struct BPoint_t {
  Float_t sector, time, peak;//
};
BPoint_t BPoint;
void TonkoLandauPeak(Char_t *FileName="TonkoLandauPeak.dat") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  //  fName.ReplaceAll(".data",".root");
  fName.ReplaceAll(".dat",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","Pulser","sector:time:peak");//:D_l:D_p:D_u:w:a:b:c");
  Int_t sector, time;
  Float_t peak;
  Char_t line[121];
  while (fgets(&line[0],120,fp)) {
    if (line[0] == '#') continue;
    Int_t n = sscanf(&line[0],"%d %d %f",&sector, &time, &peak);
    BPoint.sector = sector;
    BPoint.time = time;
    BPoint.peak = peak;
    FitP->Fill(&BPoint.sector);
  }
  fclose(fp);
  f->Write();
}
