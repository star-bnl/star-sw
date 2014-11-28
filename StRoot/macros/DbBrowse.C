void DbBrowse(char* database="Calibrations/tpc"){

  // macro to browse a particular database.
  // database is in the form => "type/domain"
  // For examples
  //  database=>"Calibrations/tpc"
  //  database=>"Geometry/ftpc"
  //   ....


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

  // create makers connecting to Star databases 

  char *db2 = "StarDb";
  if (gSystem->AccessPathName(db2) !=0) {
    printf("File %s does not exist\n",db2);
    db2 = "";
  }
  
  
  St_db_Maker *dbMk = new St_db_Maker("dbName","MySQL:StarDb","$STAR/StarDb",db2);

  dbMk->Init();

  // Make reaquests for data
  // choose timestamp 

  //  dbMk->SetDateTime(20011201,10000);
  dbMk->SetDateTime(20011117,103638);


  /*  to browse many databases, use this approach

  const char* dbs[] = {"Geometry","Calibrations","RunLog","Conditions",0};
  for(int i=0;dbs[i]!=0;i++)dbMk->GetDataBase(dbs[i]);

  */

  // to browse 1 database, use this one

  dbMk->GetDataBase(database);
  TBrowser* b2 = new TBrowser("TestDbMaker",dbMk);

}







