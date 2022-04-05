#include "TROOT.h"
#include "TDataSet.h"
#include "TDatime.h"
#include "TString.h"
#include "TSystem.h"

#include <iostream>
#include <fstream>

void fPostChannelGeometry_db(
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
    
    const Int_t MAX_DB_INDEX = 12;
    fpostChannelGeometry_st in[MAX_DB_INDEX];
    
    //-------------------------------------------
    
    if (option.Contains("writedb"))
	{
	    gSystem->Setenv("DB_ACCESS_MODE", "write");
	    StDbManager*    mgr     = StDbManager::Instance();
	    StDbConfigNode* node    = mgr->initConfig("Geometry_fps");
	    StDbTable*      dbtable = node->addDbTable("fpostChannelGeometry");
	    mgr->setStoreTime(storeTime.Data());

		in[ 0].quad = 1; in[ 0].layer = 1; in[ 0].nslat =  9; //SL1
		in[ 1].quad = 1; in[ 1].layer = 2; in[ 1].nslat = 14; //SL2
		in[ 2].quad = 1; in[ 2].layer = 3; in[ 2].nslat =  0; //SL3 (NOT EXIST)
		in[ 3].quad = 1; in[ 3].layer = 4; in[ 3].nslat = 25; //SL4 (top + bottom)
		in[ 4].quad = 1; in[ 4].layer = 5; in[ 4].nslat = 43; //SL5 (top + bottom)
		in[ 5].quad = 1; in[ 5].layer = 6; in[ 5].nslat = 34; //SL6 (top + bottom)
		in[ 6].quad = 2; in[ 6].layer = 1; in[ 6].nslat =  0; //NL1 (NOT EXIST)
		in[ 7].quad = 2; in[ 7].layer = 2; in[ 7].nslat =  0; //NL2 (NOT EXIST)
		in[ 8].quad = 2; in[ 8].layer = 3; in[ 8].nslat = 14; //NL3
		in[ 9].quad = 2; in[ 9].layer = 4; in[ 9].nslat = 25; //NL4 (top + bottom)
		in[10].quad = 2; in[10].layer = 5; in[10].nslat = 43; //NL5 (top + bottom)
		in[11].quad = 2; in[11].layer = 6; in[11].nslat = 34; //NL6 (top + bottom)

	    dbtable->SetTable((char*)&in, MAX_DB_INDEX);
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
	    DB = dbMk->GetDataBase("Geometry/fps/fpostChannelGeometry");
	    if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
	    
	    St_fpostChannelGeometry* dataset = 0;
	    dataset = (St_fpostChannelGeometry*) DB->Find("fpostChannelGeometry");
	    if (!dataset) { std::cout << "ERROR: dataset does not contain requested table" << std::endl; return; }
	    Int_t rows = dataset->GetNRows();
	    if (rows > 1) std::cout << "INFO: found INDEXED table with " << rows << " rows" << std::endl;
	    
	    TDatime val[2];
	    dbMk->GetValidity((TTable*)dataset, val);
	    std::cout << "Dataset validity range: [ " << val[0].GetDate() << "." << val[0].GetTime() << " - "
		      << val[1].GetDate() << "." << val[1].GetTime() << " ] " << std::endl;
	    
	    fpostChannelGeometry_st* table = (fpostChannelGeometry_st*)dataset->GetTable();
	    for (Int_t i = 0; i < rows; i++)
		{
		    std::cout << Form("Row=%3d quad=%1d layer=%1d slat=%2d\n",
				      i, table[i].quad, table[i].layer, table[i].nslat);
		}
	}

    return;
}//Main
