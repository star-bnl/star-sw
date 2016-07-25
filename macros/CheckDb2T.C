class St_db_Maker;
St_db_Maker *dbMk[2] = {0,0};
//________________________________________________________________________________
void CheckDb2T(TString DataBase = "Calibrations/svt", Char_t *date0="y2004c", Char_t *date1="y2005c") {
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
  TTable *table[2] = {0,0};
  Int_t nrows = 0;
  Int_t NRows[2] = {0, 0};
  StChain *chain = new StChain("DbTest");
  St_db_Maker *dbMk[2] = {0,0};
  Int_t idb = 0;

//   for (idb = 0; idb < 2; idb++) { cout << "idb = " << idb << endl;
//     dbMk[idb] = new St_db_Maker(Form("db%i",idb),"MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
//     dbMk[idb]->SetFlavor("ofl+sim");
//     if (idb == 0) dbMk[idb]->SetDateTime(date1); 
//     else          dbMk[idb]->SetDateTime(date2); 
//     cout << "idb = " << idb << " dbMk[idb] " << dbMk[idb] << endl;
//   }
  St_db_Maker *dbMk0 = new St_db_Maker("db0","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk0->SetFlavor("ofl+sim");
  dbMk0->SetDateTime(date0); 
  dbMk[0] = dbMk0;
  St_db_Maker *dbMk1 = new St_db_Maker("db1","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk1->SetFlavor("ofl+sim");
  dbMk1->SetDateTime(date1); 
  dbMk[1] = dbMk1;
  chain->Init();
  if (! (dbMk[0] && dbMk[1])) return;
  TDataSet *set[2] = {0,0};
  TDataSet *set0 = dbMk0->GetDataBase(DataBase.Data()); 
  TDataSet *set1 = dbMk1->GetDataBase(DataBase.Data());
  set[0] = set0;
  set[1] = set1;
  if (set0) cout << "Found " << set0->GetName() << endl; else return;
  if (set1) cout << "Found " << set1->GetName() << endl; else return;
//   for (idb = 0; idb < 2; idb++) {
//     if (! dbMk[idb]) continue;
//     set[idb] = dbMk[idb]->GetDataBase(DataBase);
//     if (! set[idb]) {cout << "Can't find set " << dName << " in " << Names[idb]  << endl; continue;}
//   }
  if (! (set[0] && set[1])) return;
  TFile *f = new TFile(Form("%s/%s.root",DataBase.Data(),date0),"recreate");
  set0->Write();
  delete f;
  TFile *f = new TFile(Form("%s/%s.root",DataBase.Data(),date1),"recreate");
  set1->Write();
  delete f;
  TDatime t[2];
  TDataSetIter next(set[0],0);
  TDataSet *ds = 0;

  while ((ds = next())) {
    if (! ds->HasData()) continue;
    table[0] = (TTable *) ds; cout << "Found " << table[0]->GetName() << " with path " << table[0]->Path() << endl;
    table[1] = (TTable *) set[1]->Find(table[0]->Path());
    if (! table[idb]) {cout << "Can't find " << table[0]->Path() << " in 2'nd set" << endl; continue;}
    TDatime t[2];
    for (int idb = 0; idb < 2; idb++) {
      dbMk[idb]->GetValidity(table[idb],t);
      cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	   << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
      NRows[idb] = table[idb]->GetNRows();
      cout << "Get from " << idb << " NRows = " << NRows[idb] << endl;
    }
    nrows = TMath::Min(NRows[0],NRows[1]);
    if (NRows[0] != NRows[1]) {
      cout << "No. of rows is different " << NRows[0] << "\t" << NRows[1] << endl; 
      continue;
    }
    if (table[0]->GetRowSize() != table[1]->GetRowSize()) {
      cout << "RowSizes are different " << table[0]->GetRowSize() << " != " << table[1]->GetRowSize() << endl;
      continue;
    }
    Char_t *c0 = table[0]->GetArray();
    Char_t *c1 = table[1]->GetArray();
    int n = nrows*table[0]->GetRowSize();
    if (memcmp(c0,c1,n)) {
      cout << "Content is different" << endl;
      cout << "Db table ===========================" << endl;
      table[0]->Print(0,nrows);
      cout << "Cint table ===========================" << endl;
      table[1]->Print(0,nrows);
    }
    else                  {
      cout << "Table are the same" << endl;
    }
  }
}







