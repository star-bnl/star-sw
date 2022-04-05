{
#include <iomanip>
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
    dbMk->SetDateTime(20111001,0); // event or run start time, set to your liking
    dbMk->SetFlavor("ofl");

    dbMk->Init();
    dbMk->Make();

    TDataSet *DB = 0;
    DB = dbMk->GetDataBase("Calibrations/fgt/fgtElosCutoff");
    if (!DB) {
        std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
		return;
    }

    St_fgtElosCutoff *dataset = 0;
    dataset = (St_fgtElosCutoff*) DB->Find("fgtElosCutoff");
    Int_t rows = dataset->GetNRows();
    if (rows > 1) {
        std::cout << "INFO: found INDEXED table with " << rows << " rows" << std::endl;
    }

    if (dataset) {
        fgtElosCutoff_st *table = dataset->GetTable();
		std::cout << setprecision(10);
		for (int i = 0; i < 10000; i++) {
            std::cout << i <<"th channel: " << table[0]->cutoff[i] << std::endl;
    if(i>15) break;    }
		std::cout << "COMMENT: " << table[0]->comment << std::endl;
    } else {
        std::cout << "ERROR: dataset does not contain requested table" << std::endl;
    }

}
