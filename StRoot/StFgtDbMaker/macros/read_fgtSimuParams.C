read_fgtSimuParams(){
    // base libraries
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");

    // db-related libraries
    gSystem->Load("St_Tables");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");

    St_db_Maker *dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
    dbMk->SetDebug();
    dbMk->SetDateTime(20120301,0); // event or run start time, set to your liking
    dbMk->SetFlavor("ofl");

    dbMk->Init();
    dbMk->Make();

    TDataSet *DB = 0;
    DB = dbMk->GetDataBase("Calibrations/fgt/fgtSimuParams");
    if (!DB) {
        std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
    }

    St_fgtSimuParams *dataset = 0;
    dataset = (St_fgtSimuParams*) DB->Find("fgtSimuParams");
    Int_t rows = dataset->GetNRows();
    if (rows > 1) {
        std::cout << "BAD INFO: found INDEXED table with " << rows << " rows, abort" << std::endl;
	return;
    }

    if (dataset) {
        fgtSimuParams_st *table = dataset->GetTable();
	int irow=0;
	printf(" print fgtSimuParams: comment=%s=\n", table[irow].comment);
        for (Int_t i = 0; i < 15; i++) {
    	    // sample output of few params
            std::cout << i << "th param : " << table[irow].param[i] << std::endl;
        }
    } else {
      std::cout << "ERROR: dataset does not contain requested table" << std::endl;
      return;
    }   

}
