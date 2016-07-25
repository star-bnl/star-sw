class St_db_Maker;
St_db_Maker *dbMk = 0;
void read_trgTimeOffsetB()
{
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

    dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
    dbMk->SetDebug();
    dbMk->SetDateTime(20090301,0); // event or run start time, set to your liking
    dbMk->SetFlavor("ofl");

    dbMk->Init();
    dbMk->Make();

    TDataSet *DB = 0;
    DB = dbMk->GetDataBase("Conditions/trg/trgTimeOffsetB");
    if (!DB) {
        std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
    }

    St_trgTimeOffset *dataset = 0;
    dataset = (St_trgTimeOffset*) DB->Find("trgTimeOffsetB");
    Int_t rows = dataset->GetNRows();
    if (rows > 1) {
        std::cout << "INFO: found INDEXED table with " << rows << " rows" << std::endl;
    }

    if (dataset) {
	TDatime val[2];
        dbMk->GetValidity((TTable*)dataset,val);
        std::cout << "Dataset validity range: [ " << val[0].GetDate() << "." << val[0].GetTime() << " - " 
	<< val[1].GetDate() << "." << val[1].GetTime() << " ] "
        << std::endl;

        trgTimeOffset_st *table = dataset->GetTable();
        for (Int_t i = 0; i < rows; i++) {
    	    // sample output of first member variable
            std::cout << i << "th row : " << table[i]->offset << std::endl;
        }
    } else {
        std::cout << "ERROR: dataset does not contain requested table" << std::endl;
    }

}
