#include "TROOT.h"
#include "TDataSet.h"
#include "TDatime.h"
#include "TString.h"
#include "TSystem.h"

#include <iostream>
#include <fstream>

void fPostConstant_db(
		const char* opt   = "",
		const char* year  = "17sim")
{
    // storeTime is beginning time for validity range in case of WRITING DB
    TString option(opt), yr(year), storeTime;
    int date, time; // time for READING DB
    
    std::cout <<"year = " <<year <<std::endl;
    if (yr.Contains("17sim"))
	{
	    storeTime = "2016-12-10 00:00:00";
	    date = 20161210;
	    time = 0;
	}
    else if (yr.Contains("17ofl"))
	{
	    storeTime = "2016-12-20 00:00:00";
	    date = 20161220;
	    time = 0;
	}
    else { std::cout << "Please specify valid year tag\n"; exit; }
    std::cout << "Opt ="         << opt << "\n";
    std::cout << "write = "      << option.Contains("writedb") << "\n";
    std::cout << "storetime = "  << storeTime << "\n";
    std::cout << "date, time = " << date <<" "<< time << "\n";
    
    gROOT->Macro("./loadlib.C");
    
    //-------------------------------------------
    
    fpostConstant_st in;
    
    //-------------------------------------------
    
    if (option.Contains("writedb"))
	{
	    gSystem->Setenv("DB_ACCESS_MODE", "write");
	    StDbManager*    mgr     = StDbManager::Instance();
	    StDbConfigNode* node    = mgr->initConfig("Geometry_fps");
	    StDbTable*      dbtable = node->addDbTable("fpostConstant");
	    mgr->setStoreTime(storeTime.Data());

		in.nQuad	 = 2;
		in.nLayer	 = 6;
		in.maxSlat	 = 43;
		in.maxQTaddr     = 8;
		in.maxQTch	 = 32;

	    dbtable->SetTable((char*)&in, 1);
	    if (yr.Contains("sim")) dbtable->setFlavor("sim");
	    mgr->storeDbTable(dbtable);
	    std::cout << "INFO: table saved to database" << std::endl;
	}

	//-------------------------------------------

    if (option.Contains("readdb"))
	{
	    std::cout << "INFO: Reading database" << std::endl;
	    gSystem->Unsetenv("DB_ACCESS_MODE");
	    //gSystem->Unsetenv("DB_SERVER_LOCAL_CONFIG");
	    
	    St_db_Maker* dbMk = new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
	    dbMk->SetDebug();
	    dbMk->SetDateTime(date, time); // event or run start time, set to your liking
	    if      (yr.Contains("ofl")) { dbMk->SetFlavor("ofl"); }
	    else if (yr.Contains("sim")) { dbMk->SetFlavor("sim"); }
	    dbMk->Init();
	    dbMk->Make();
	    
	    TDataSet *DB = 0;
	    DB = dbMk->GetDataBase("Geometry/fps/fpostConstant");
	    if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
	    
	    St_fpostConstant *dataset = 0;
	    dataset = (St_fpostConstant*) DB->Find("fpostConstant");
	    if (!dataset) { std::cout << "ERROR: dataset does not contain requested table" << std::endl; return; }
	    Int_t rows = dataset->GetNRows();
	    if (rows > 1) std::cout << "INFO: found INDEXED table with " << rows << " rows" << std::endl;
	    
	    TDatime val[2];
	    dbMk->GetValidity((TTable*)dataset, val);
	    std::cout << "Dataset validity range: [ " << val[0].GetDate() << "." << val[0].GetTime() << " - "
		      << val[1].GetDate() << "." << val[1].GetTime() << " ] " << std::endl;
	    
	    fpostConstant_st *table = (fpostConstant_st*)dataset->GetTable();
	    for (Int_t i = 0; i < rows; i++)
		{
			std::cout << Form("Row=%d nQuad=%1d nLayer=%1d maxSlat=%2d maxQTaddr=%1d maxQTch=%2d\n",
					i, table[i].nQuad, table[i].nLayer, table[i].maxSlat, table[i].maxQTaddr, table[i].maxQTch);
		}
	}

    return;
}//Main
