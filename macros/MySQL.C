void MySQL(const Char_t * selection = NULL)
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

  if (gClassTable->GetID("TSQLServer") <= 0) {
    gSystem->Load("libmysqlclient.so");
    gSystem->Load("libMySQL");
  }
  TSQLServer *db = TSQLServer::
    Connect("mysql://onlsun1.star.bnl.gov:3316/RunLog_daq","","");
    //       Connect("mysql://duvall.star.bnl.gov/fcMDC3", "", "");
  if (!db) return;
  TSQLResult *res;
  TSQLRow *row;
  TStringLong query= "select * from daqTagFiles where entryTime>=20000820000000 and eventType=3";
  if (selection!=NULL)
    query+=selection;
  //  query+="%'";

  res = db->Query(query.Data());
  Int_t nRows = res->GetRowCount();

  cout<<"Selected "<<nRows<<" files from the tagDB"<<endl;
  if  (nRows==0) return;
    cout << "\t|dataId"         
	 << "\t|entryTime"      
	 << "\t|run      "            
	 << "\t|Type"      //EventType"      
	 << "\t|Seq"   //fileSequence"   
	 << "\t|file                           "           
	 << "\t|begin"     //beginEvent"     
	 << "\t|end"       //endEvent"       
	 << "\t|numberOfEvents" 
	 << endl;
  while ((row = res->Next())) {
    cout << "\t|" << row->GetField(0) 
	 << "\t|" << row->GetField(1) 
	 << "\t|" << row->GetField(2)
	 << "\t|" << row->GetField(3)
	 << "\t|" << row->GetField(4)
	 << "\t|" << row->GetField(5)
	 << "\t|" << row->GetField(6)
	 << "\t|" << row->GetField(7);
    cout << "\t|" << row->GetField(8) 
	 << endl;
  }
}
