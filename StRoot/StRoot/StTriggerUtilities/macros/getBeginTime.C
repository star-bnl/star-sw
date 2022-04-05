void getBeginTime(int runNumber = 15047028)
{
  const char* database = "mysql://db04.star.bnl.gov:3413/RunLog?timeout=60";
  const char* user = "zchang";
  const char* pass = "";
  TMySQLServer* mysql = TMySQLServer::Connect(database,user,pass);
  gSystem->mkdir("beginTimes");
  ofstream of(Form("beginTimes_t/%0.8d.beginTimes.txt", runNumber));
  if (!mysql) {
    cerr << "Connection to " << database << " failed" << endl;
    return;
  }

  TString query;
  TMySQLResult* result;
  
  query = Form("select distinct beginTime from `RunLog`.`runDescriptor` where runNumber = %d",runNumber);

  result = (TMySQLResult*)mysql->Query(query);
  if (result) {
    TMySQLRow* row;
    while (row = (TMySQLRow*)result->Next()) {
      TString str(Form("%s", row->GetField(0)));
      cout<<str.Data()<<endl;
      TString str_new = str;
      cout<<str_new.Data()<<endl;
      of<<str_new.Data()<<endl;
      delete row;
    }
    result->Close();
  }
  of.close();
  mysql->Close();  
}
