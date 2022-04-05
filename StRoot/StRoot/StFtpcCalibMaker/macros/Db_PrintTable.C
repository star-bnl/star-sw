class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    // Baseline shared libraries
    gSystem->Load("libTable");
    gSystem->Load("St_base"); 
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    // DB-specific libs
    // may only need libStDb_Tables.so
    // but may need St_Tables.so... so I'm using this one
    //  gSystem->Load("libStDb_Tables.so");
    gSystem->Load("St_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so"); 
    gSystem->Load("St_db_Maker.so");
  }
 dbMk = new St_db_Maker("db","MySQL:StarDb");//,"$STAR/StarDb","$PWD/StarDb");
 dbMk->SetDebug(1);
  //dbMk->SetFlavor("ofl");
 dbMk->Init();
}
//________________________________________________________________________________
void Db_PrintTable(const Char_t *tabNam  = "Calibrations/ftpc/ftpcCoordTrans", 
	Int_t date = 20040227, 
	Int_t time = 1){
fprintf(stderr," 0. cout = %x \n", (void*)cout);
  if (dbMk == 0) Load();
  dbMk->SetDateTime(date,time); 
  fprintf(stderr," 1. cout = %x \n",(void*) cout);
#if 0
  //  to browse many databases, use this approach
  const char* dbs[] = {"Geometry","Calibrations","RunLog","Conditions",0}; 
  for(int i=0;dbs[i]!=0;i++)dbMk->GetDataBase(dbs[i]); 
#else
  // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
  table = (TTable *) set->Find(gSystem->BaseName(tabNam));
  if (table) {
    TDatime t[2];
    dbMk->GetValidity(table,t);
    cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
    Int_t Nrows = table->GetNRows();
    if (Nrows > 10) Nrows = 10;
    table->Print(0,Nrows);
#if 0
    TString name(gSystem->BaseName(tabNam));
    name += Form(".%06i.%06i.root",t[0].GetDate(),t[0].GetTime());
    TFile *f = new TFile(name.Data(),"RECREATE");
    table->Write();
    delete f;
#endif
  }
  else cout << "Table:" << tabNam << " has not been found" << endl;
#endif
  fprintf(stderr," 2. cout = %x \n",(void*) cout); 
 //  TBrowser* b2 = new TBrowser("TestDbMaker",dbMk);
  
}


