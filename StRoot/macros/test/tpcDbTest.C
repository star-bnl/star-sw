{
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTpcDb");

  //  Create the makers to be called by the current chain
  const char *mainDB =  "MySQL:StarDb";
  const char *userDB = "StarDb";
  StChain *chain =  new StChain();
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
  dbMk->SetDebug();
  tpcDbMk->SetDebug();
  dbMk->SetDateTime(19970101,10000);
  chain->SetDebug();
  chain->Init();
  chain->Make();
}

