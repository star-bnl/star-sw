void DbBrowse(char* aliasTag){

  /*

Input should be one of these alias tags which the St_db_Maker
translates into the timestamp  of dates+time

static const char *aliases[]={
"sd97",   "sd98",   "year_1a","year_1b","year_1c",
"es99",   "er99",   "dc99"   ,"year_1d","year_1e",
"year_1h","year_2a", "year_2b", 0};   

static const int   dates[]=  {
19970101, 19980101, 19990101, 19990501, 19991001,
19990615, 19990616, 19991206, 19991101, 19991201,
20000614, 20010610, 20010501, 0};
static const int   times[]=  {
       0,        0,        0,        0,        0,
       0,   120000,    80000,        0,        0,
  175430,        0,        0,        0};

*/

  // Baseline shared libraries
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

  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");

  dbMk->Init();

  // Make reaquests for data
  // choose timestamp 

  //  dbMk->SetDateTime(20001001,10000);

  dbMk->SetDateTime(aliasTag);

  const char* dbs[] = {"Geometry","Calibrations","RunLog","Conditions",0};
  for(int i=0;dbs[i]!=0;i++)dbMk->GetDataBase(dbs[i]);
  TBrowser* b2 = new TBrowser("TestDbMaker",dbMk);

}







