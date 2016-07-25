//select beginTime, unix_timestamp(beginTime),CH4_M3 as "OutputCH4", CH4_M4 as "InputCH4",T6 as "Temperature"
struct BPoint_t {
  Float_t utime, OutputCH4,  InputCH4, Temperature, Pressure;
};
BPoint_t BPoint;
void MakeNTuple4TpcGas(Char_t *FileName="tpcGas2Onl.data") {
  TString fName(FileName);
  fName.ReplaceAll(".data",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","TpcGas","utime:OutputCH4:InputCH4:Temperature:Pressure");
  FILE *fp = fopen(FileName,"r");
  if (! fp ) return;
  cout << FileName << " has been opened." << endl;
  char line[121];
  char date[11];
  char time[9];
  Int_t i = 0;
  Float_t sum_curr_1 = -9999;
  fgets(&line[0],120,fp);
  while (fgets(&line[0],120,fp)) {
    //2005-01-29 08:14:45     1106986485      10.53999996     10.52000046     297.29998779
    //    cout << line << endl;
    UInt_t u;
    sscanf(&line[0],"%10s %8s  %i %f %f %f %f",&date[0],&time[0],&u,&BPoint.OutputCH4,&BPoint.InputCH4,&BPoint.Temperature, &BPoint.Pressure);
    //    cout << date << "/" << time << endl;
    BPoint.utime = u;
    //    printf("%f %f %f %f\n",BPoint.utime,BPoint.OutputCH4,BPoint.InputCH4,BPoint.Temperature, Pressure);
    //    break;
    FitP->Fill(&BPoint.utime);
    i++;
    if (i%1000 == 1) {
      cout << "i:" << i << "\t" << line;
      //      TUnixTime t(BPoint.utime);
      TDatime t;
      UInt_t u = (UInt_t) BPoint.utime;
      t.Set(u);
      t.Print();
      printf("%f %f %f %f\n",BPoint.utime,BPoint.OutputCH4,BPoint.InputCH4,BPoint.Temperature, BPoint.Pressure);
    }
  }
  fclose(fp);
  f->Write();
}
