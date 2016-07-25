/*
  GetDriftVelocities.pl | tee WestEast
 */
struct BPoint_t {
  Float_t all, dall, west, dwest, east, deast;//
};
BPoint_t BPoint;
void MakeNTupleFromAsciiWEDrift(Char_t *FileName="WestEast") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open " << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  //  fName.ReplaceAll(".data",".root");
  fName += ".root";
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","Pulser","all:dall:west:dwest:east:deast");//:D_l:D_p:D_u:w:a:b:c");
  char line[121];
  Float_t all,dall,west, east, dwest, deast;
  Int_t i;
  while (fgets(&line[0],120,fp)) {
    Int_t n = sscanf(&line[0],"All = %f +/- %f; West = %f +/- %f; DVEast = %f +/- %f;",&all,&dall,&west,&dwest,&east,&deast);
    printf("%s",line);
    printf("All = %f +/- %f; West = %f +/- %f; East = %f +/- %f \n",all,dall,west,dwest,east,deast);
    BPoint.all = all;
    BPoint.dall = dall;
    BPoint.west = west;
    BPoint.dwest = dwest;
    BPoint.east = east;
    BPoint.deast = deast;
    FitP->Fill(&BPoint.all);
    //    if (i > 10) break;
    //    if (i%10 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
