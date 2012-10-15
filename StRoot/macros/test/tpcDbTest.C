void tpcDbTest() {
  //  gSystem->Load("libmysqlclient");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StarMagField");
  gSystem->Load("StEvent");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  
  //  Create the makers to be called by the current chain
  StChain *chain =  new StChain();
  St_db_Maker *dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
  dbMk->SetDebug();
  tpcDbMk->SetDebug();
  //  dbMk->SetDateTime(20090701,10000);
  //  dbMk->SetDateTime(20090328,164000);
  //  dbMk->SetDateTime(20100107,132403);
  dbMk->SetDateTime(20090328,164000);
  //  dbMk->SetDateTime("y2010");
  chain->SetDebug();
  chain->Init();
  new StarMagField;
  chain->Make();
  StTpcCoordinateTransform tran(gStTpcDb);
  Int_t row = 1;
  for (Int_t sector = 5; sector <= 24; sector += 12) {
    StTpcPadCoordinate padP(sector,row,1,0);  cout << "padP\t" << padP << endl;
    StTpcLocalCoordinate locP;
    StTpcLocalSectorCoordinate locS;
    tran(padP,locP,kFALSE,kFALSE);            cout << "locP\t" << locP << endl;
    tran(locP,locS);                          cout << "locS\t" << locS << endl;
    tran(locP,padP,kFALSE,kFALSE);            cout << "padP\t"  << padP  << endl;
    StGlobalCoordinate globP;
    tran(padP,globP,kFALSE,kFALSE);            cout << "globP\t" << globP << endl;
    tran(globP,padP,sector,row,kFALSE,kFALSE); cout << "padP\t"  << padP  << endl;
    StGlobalDirection dirG(0,0,5);            cout << "dirG\t" << dirG << endl;
    StTpcLocalSectorDirection dirL;
    tran(dirG,dirL,sector,row);               cout << "dirL\t" << dirL << endl;
    tran(dirL,dirG);                          cout << "dirG\t" << dirG << endl;
  }
  /*
    StTpcCoordinateTransform tran(gStTpcDb);
    Int_t row = 1;
    Int_t sector = 3;
    StTpcPadCoordinate padP(sector,row,1,0);  cout << "padP\t" << padP << endl;
    StTpcLocalCoordinate locP;
    tran(padP,locP,kFALSE,kFALSE);            cout << "locP\t" << locP << endl;
    tran(locP,padP,kFALSE,kFALSE);            cout << "padP\t"  << padP  << endl;
   */
}

