{
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTpcDb");

  //  Create the makers to be called by the current chain
  const char *mainDB =  "/afs/rhic/star/users/fine/public/StDb";
  const char *userDB = ""; // "MyDb/params";
  StChain *chain =  new StChain();
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB,userDB);
  StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
  dbMk->SetDebug();
  tpcDbMk->SetDebug();
  chain->SetDebug();
  chain->Init();
  chain->Make();
  new TBrowser ("TPC",tpcDbMk->FindByName(".const"));
}

