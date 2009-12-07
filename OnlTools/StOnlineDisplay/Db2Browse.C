  float rowindx=2;
  float x= 0;
  float y= 0;
  float pad=1;
  int p = 11;

void Db2Browse(char* database="Geometry/tpc"){


  // Baseline shared libraries
//   gSystem->Load("libTable");
  gSystem->Load("StarRoot");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
 
  gSystem->Load("St_Tables.so");
  gSystem->Load("St_db_Maker.so");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
 
  // create makers connecting to Star databases 

  char *db2 = "StarDb";
  if (gSystem->AccessPathName(db2) !=0) {
    printf("File %s does not exist\n",db2);
    db2 = "";
  }
  
  // Create the steering chain
  // StChain *chain - new StChain();
//  St_db_Maker *dbMk = new St_db_Maker("dbName","$STAR/StarDb","MySQL:StarDb");
  St_db_Maker *dbMk = new St_db_Maker("dbName","MySQL:StarDb","$STAR/StarDb");
  dbMk->Shunt(0);
//  St_db_Maker *dbMk    = new St_db_Maker("dbName","db");
  StTpcDbMaker *tpcDbMk  = new StTpcDbMaker();
  // tpcDbMk->Shunt(0);
  dbMk->SetAttr("dbSnapshot","dvSnapShot.root");
  dbMk->Init(); // read Db
  // Make requests for data
  // choose timestamp 

   dbMk->SetDateTime(20090312, 94451);
   //  dbMk->InitRun(8352025); //8343021);
   // dbMk->InitRun(85684689); //8343021);8352025
   dbMk->Make();
   tpcDbMk->Make();

   printf(" InitRun -------\n");
  /*  to browse many databases, use this approach
  const char* dbs[] = {"Geometry","Calibrations","RunLog","Conditions",0};
  for(int i=0;dbs[i]!=0;i++)dbMk->GetDataBase(dbs[i]);

  */
   ds = dbMk->GetInputDB("Calibrations/emc/map");
   ds->ls(0);
   ds  = dbMk->GetInputDB("RunLog/onl/starClockOnl");
   assert(ds);
   ds->ls(0);
   ds  = dbMk->GetInputDB("Geometry/tpc");   
   assert(ds);
   ds->ls(0);

  // to browse 1 database, use this one
 // row = 0;
// tpc_row_to_y_(&rowindx,&y);
// printf(" %d %f \n",rowindx,y);

//  p =  tpc_x_to_pad_(&rowindx,&x,&pad);
//  printf(" %d %f %f %f\n", p,rowindx,x,pad);
  // to write down the snapshot file one has to call "Finish"
  dbMk->Finish();
        
 // TBrowser* b2 = new TBrowser("TestDbMaker",dbMk);

}







