void checksvtRDOs() {
  if (gClassTable->GetID("St_svtRDOstripped")  < 0) gSystem->Load("libStDb_Tables.so");
  const Char_t *path = "/afs/rhic.bnl.gov/star/packages/.DEV2/StarDb/Calibrations/svt";
  TFileSet dir(path);
  TDataSetIter next(&dir,999);
  TDataSet *ds = 0;
  TDataSet *set = 0;
  St_svtRDOstripped *table = 0;
  TString cmd;
  while ((ds = next())) {
    //    cout << ds->GetName() << "\t" << ds->GetTitle() << endl;
    if (strncmp("file",ds->GetTitle(),4))	continue;  
    //    if (!ds->InheritsFrom(TTable::Class()))     continue;
    //    cout << "it is the file" << endl;
    cmd = ".L "; cmd += path; cmd += "/"; cmd += ds->GetName();
    gInterpreter->ProcessLine(cmd);
    cmd.ReplaceAll(".L",".U");
    set =  (TDataSet *) gInterpreter->Calc("CreateTable()");
    gInterpreter->ProcessLine(cmd);
    if (! set->InheritsFrom(St_svtRDOstripped::Class()))     continue;
    //    cout << "it is the St_svtRDOstripped" << endl;
    table = (St_svtRDOstripped *) set;
    svtRDOstripped_st *row = table->GetTable();
    Int_t N = table->GetNRows();
    Int_t iok = -1;
    for (Int_t i = 0; i < N; i++, row++) {
      if (! row->lvFault && TMath::Abs(row->hvVolt+1500) < 5) {
	iok = 0;
	break;
      }
    }
    Int_t date, time;
    sscanf(ds->GetName(),"svtRDOstripped.%d.%d.C",&date,&time);
    TDatime t(date, time);
    cout << t.AsSQLString() << "\t" << ds->GetName() << "\t";
    if (iok ) cout << " dead";
    else      cout << " alive";
    cout << endl;
    delete table;
  }
}
