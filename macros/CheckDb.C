/*  Check Cint Db with Respect to MySQL 
  root.exe lDb.C 'CheckDb.C("Calibrations/tpc/TpcSecRowB",20140101,6)'
*/

class St_db_Maker;
St_db_Maker *dbMk[2] = {0,0};
class TTable;
TTable *table[2] = {0,0};
Int_t nrows = 0;
Int_t NRows[2] = {0, 0};
#if 0
class StChain;
StChain *chain = 0;
#endif
const Char_t *Names[2] = {"db","dbMySQL"};
//________________________________________________________________________________
void loadDb() {
  cout << "Load ==========" << endl;
  dbMk[0] = (St_db_Maker*) chain->Maker("db"); //("dbCint","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  cout << "Get " << dbMk[0]->GetName() << endl;
  dbMk[0]->SetDebug(1);
  dbMk[0]->SetFlavor("ofl+sim");
  dbMk[1] = new St_db_Maker("dbMySQL","MySQL:StarDb","","");
  cout << "create " << dbMk[1]->GetName() << endl;
  dbMk[1]->SetDebug(1);
  dbMk[1]->SetFlavor("ofl+sim");
  chain->Init();
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
//void CheckDb(TString Table = "Calibrations/tpc/TpcSecRowB", Int_t date = 20100527, Int_t time=20036) {
void CheckDb(TString Table = "Calibrations/tpc/TpcSecRowB", Int_t date = 20100424, Int_t time=40036) {
  cout << "dbMk[1] = " << dbMk[1] << endl;
  if (dbMk[1] == 0) loadDb();
  TString bName(gSystem->BaseName(Table.Data())); cout << bName << endl;
  TString dName(gSystem->DirName(Table.Data())); cout << dName << endl;
  Int_t indx = bName.Index(".");
  if (indx >= 0) {
    TString TName(bName.Data(),indx);  cout << "Table name " << TName << endl;
    TString Time(bName.Data()+indx+1); 
    Time.ReplaceAll("C","");
    Time.ReplaceAll("root","");//  cout << "Time " << Time << endl;
    sscanf(Time.Data(),"%d.%d",&date,&time);
    TDatime Date(date,time); cout << " Date " << Date.GetDate() << "\tTime " << Date.GetTime() << endl;
    bName = TString(bName,indx);   
    TString dName(gSystem->DirName(Table));
  }
  TString DB("");
  Int_t indx = dName.Index("StarDb/");
  if (indx >= 0) {
    DB = dName.Data()+indx+7;
    dName = DB;
  }
  cout << "Database: " << dName << " Table: " << bName << endl;
  TDataSet *set[2] = {0,0};
  for (int idb = 0; idb < 2; idb++) {
    if (! dbMk[idb]) continue;
    dbMk[idb]->SetDateTime(date,time); 
    set[idb] = dbMk[idb]->GetDataBase(dName.Data());
    if (! set[idb]) {cout << "Can't find set " << dName << " in " << Names[idb]  << endl; continue;}
    TDatime t[2];
    table[idb] = (TTable *) set[idb]->Find(bName.Data());
    if (! table[idb]) {cout << "Can't find " << bName << " in " << Names[idb]  << endl; return;}
    TDatime t[2];
    dbMk[idb]->GetValidity(table[idb],t);
    cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
    NRows[idb] = table[idb]->GetNRows();
    cout << "Get from " << Names[idb] << " NRows = " << NRows[idb] << endl;
  }
  nrows = TMath::Min(NRows[0],NRows[1]);
  if (NRows[0] != NRows[1]) {
    cout << "No. of rows is different " << NRows[0] << "\t" << NRows[1] << endl; 
    if (! (NRows[0] == 50 ||  NRows[1] ==50)) return;
  }
  if (TString(table[0]->GetName()) != TString(table[1]->GetName())) {
    cout << "Names are different " << table[0]->GetName() << " != " << table[1]->GetName() << endl;
    return;
  }
  if (TString(table[0]->GetTitle()) != TString(table[1]->GetTitle())) {
    cout << "Titles are different " << table[0]->GetTitle() << " != " << table[1]->GetTitle() << endl;
    return;
  }
  if (table[0]->GetRowSize() != table[1]->GetRowSize()) {
    cout << "RowSizes are different " << table[0]->GetRowSize() << " != " << table[1]->GetRowSize() << endl;
    return;
  }
  Char_t *c0 = table[0]->GetArray();
  Char_t *c1 = table[1]->GetArray();
  int n = nrows*table[0]->GetRowSize();
  Int_t NN = nrows;
  if (NN > 10) NN = 10;
  if (memcmp(c0,c1,n)) {
    cout << "Content is different" << endl;
    cout << "Db table ===========================" << endl;
    table[0]->Print(0,NN);
    cout << "MySQL table ===========================" << endl;
    table[1]->Print(0,NN);
  } else                  {
    cout << "Table are the same" << endl;
  }
}







