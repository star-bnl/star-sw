struct BPoint_t {
  Float_t day, freq;//
};
BPoint_t BPoint;
void MakeNTupleFromAscii(Char_t *FileName="/star/u/deph/wrkdir/080130/clockTimes.hold") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString fName(gSystem->BaseName(FileName));
  //  fName.ReplaceAll(".data",".root");
  fName.ReplaceAll(".hold",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","Pulser","day:freq");//:D_l:D_p:D_u:w:a:b:c");
  char line[121];
  Int_t i = 0;
  Int_t mon, d, y, h, m;
  Float_t s, freq;
  TDatime dtime(20080101,0);
  UInt_t u0 = dtime.Convert();
  TUnixTime tut;
  while (fgets(&line[0],120,fp)) {
    if (line[0] == '#') continue;
    Int_t n = sscanf(&line[0],"%2d/%2d/%4d%2d:%2d:%f%f",&mon,&d,&y,&h,&m,&s,&freq);
    //    printf("%i %s",n,line);
    //    printf("%2d/%2d/%4d %2d:%2d:%f %f\n",mon,d,y,h,m,s,freq);
    dtime.Set(y,mon,d,h,m,s);
    tut.SetLTime(dtime);
    Int_t idate, itime;
    tut.GetGTime(idate,itime);
    TDatime dt(idate,itime);
    UInt_t u1 = dt.Convert();
    BPoint.day = (u1 - u0)/24./3600.;
    BPoint.freq = freq;
    FitP->Fill(&BPoint.day);
    i++;
    //    if (i > 10) break;
    //    if (i%10 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
