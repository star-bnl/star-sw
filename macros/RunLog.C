class TSQLServer;
TSQLServer *db = 0;
void RunLog(TDatime time, const Char_t * selection = NULL) {
  if (!db || ! gClassTable->GetID("TMySQLServer") < 0) {
    gSystem->Load("libmysqlclient.so");
  }
  db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3501/RunLog","", "");
  if (!db) return;
 

  /*
[onlsun1] ~ > mysql -h onlsun1 --port=3501 -C RunLog
mysql> explain runDescriptor;
+--------------+------------------+------+-----+---------------------+----------------+
| Field        | Type             | Null | Key | Default             | Extra          |
+--------------+------------------+------+-----+---------------------+----------------+
| dataID       | int(11)          |      | MUL | NULL                | auto_increment |
| entryTime    | timestamp(14)    | YES  | MUL | NULL                |                |
| nodeID       | int(11)          |      |     | 0                   |                |
| elementID    | smallint(6)      |      |     | 0                   |                |
| beginTime    | datetime         |      |     | 1969-12-31 19:00:00 |                |
| flavor       | varchar(8)       |      |     | ofl                 |                |
| numRows      | smallint(6)      |      |     | 1                   |                |
| schemaID     | int(11)          |      |     | 1                   |                |
| deactive     | int(10) unsigned |      |     | 0                   |                |
| runNumber    | int(10) unsigned | YES  | MUL | NULL                |                |
| startRunTime | int(10) unsigned | YES  |     | NULL                |                |
| endRunTime   | int(10) unsigned | YES  |     | NULL                |                |
| runTypeID    | int(11)          | YES  |     | NULL                |                |
| glbSetupName | varchar(64)      | YES  |     | NULL                |                |
| daqSetupName | varchar(64)      | YES  |     | NULL                |                |
| trgSetupName | varchar(64)      | YES  |     | NULL                |                |
| l3SetupName  | varchar(64)      | YES  |     | NULL                |                |
| scSetupName  | varchar(64)      | YES  |     | NULL                |                |
   */
  TSQLResult *res;
  TSQLRow *row;
  //  TStringLong query= "select * from daqTagFiles where entryTime>=20000820000000 and eventType=3";
  //  TStringLong query= "select max(runNumber) as runNumber from runDescriptor where "; 
  TStringLong query= "select min(runNumber) as runNumber, startRunTime from runDescriptor where "; 
  UInt_t i1 = time.Convert();
  //  TDatime time; time.Set(i1);
  UInt_t i2 = i1 + 3600;
  query += Form("startRunTime>=%i",i1);
  //  query += " and ";
  //  query += Form("startRunTime<=%i",i2);
  if (selection!=NULL)
    query+=selection;
  //  query+="%'";

  cout << query.Data() << endl;
  res = db->Query(query.Data());
  if (!res) return;
  Int_t nRows = res->GetRowCount();

  cout<<"Selected "<<nRows<<" files from RunLog/runDescriptor"<<endl;
  if  (nRows==0) goto ENDM;
  cout << "\t" << time.AsString() << "\t|RunNumber(s)";
  while ((row = res->Next())) {
    cout << "\t|" << row->GetField(0);
  }
 ENDM:
  cout << endl;
  db->Close();
}
//________________________________________________________________________________
void RunLog( UInt_t Secs=1004515200, const Char_t * selection = NULL)
{
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// MySQL.C                                                      //
//                                                                      //
// shows how to use the STAR MDC3 tag database                          //
// Input: selection                                                     //
// used to select all datasets with names matching the selection string //
//                                                                      //
// what it does:                                                        //
// 1. selects tagDB files from the fileCatalog database in mysql        //
// 2. creates ROOT TChain from all selected tagDB files                 //
// 3. loops over all events in the chain                                //
//                                                                      //
// owner: Alexandre V. Vaniachine <AVVaniachine@lbl.gov>                //
//////////////////////////////////////////////////////////////////////////
  TDatime time; time.Set(Secs);
  RunLog(time,selection);
}
