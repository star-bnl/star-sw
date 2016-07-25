struct P_t {
  Float_t sector;
  Float_t channel;
  Float_t Voltage;
};
P_t P;
void MakeNTupleFromAsciiV(Char_t *Dir="~/WWW/star/Tpc") {
  TFile *f = new TFile("Voltage.root","RECREATE");
  TNtuple *PP = new TNtuple("PP","Voltage","sector:channel:Voltage");
  TFileSet *fileset = new TFileSet(Dir);
  TDataSetIter iter(fileset);
  TDataSet *set = 0;
  while ((set = iter())) {
    TString Title(set->GetTitle());
    if (Title != "file") continue;
    TString file(set->GetName());
    //    cout << file << end;
    if (!file.BeginsWith("chanel") || !file.EndsWith(".txt")) continue;
    FILE *fp = fopen(file.Data(),"r");
    char line[121];
    Int_t i = 0;
    fgets(&line[0],120,fp);
    Int_t date, time;
    TDatime t1(2000,1,1,0,0,0); UInt_t i1 = t1.Convert();
    while (fgets(&line[0],120,fp)) {
      sscanf(&line[0],"%i%i%f",&date,&time,&P.correction);
    TDatime t2(date,time); UInt_t i2 = t2.Convert();
    P.time = (i2 - i1)/(24.*60.*60.);
    PP->Fill(&P.time);
    i++;
    if (i%10 == 1) {cout << "i:" << i << "\t" << line; 
    cout << date << "\t" << time << "\ttime = \t" << P.time << "\tcorrection = \t" << P.correction << endl;}
  }
  fclose(fp);
  f->Write();
}
