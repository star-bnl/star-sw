{
#include <iomanip>
#include "fgtElosCutoff.h"
	gSystem->Setenv("DB_ACCESS_MODE", "write");
    // Load all required libraries
    gROOT->Macro("LoadLogger.C");
    gSystem->Load("St_base.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");

    // Initialize db manager
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fgt");
    StDbTable* dbtable = node->addDbTable("fgtElosCutoff");
    TString storeTime = "2011-09-22 12:00:00"; // beginTime timestamp in MySQL format: "YYYY-MM-DD HH:mm:ss"
    mgr->setStoreTime(storeTime.Data());

    // Create your c-struct
    fgtElosCutoff_st table;
    
	for (int i = 0; i < 10000; i++) {
 	   // zero out data, just in case..
    	table.cutoff[i] = 0;
	}
    std::string comment = "test comment";                                                                                                                               
    size_t length;                                                                                                                                                      
    length = comment.copy(table.comment, comment.size());                                                                                                               
    table.comment[length] = '\0';

	std::ifstream in("BichselELossProbHighBG.dat");
	if (!in.is_open()) { cout << "Can't find eloss file!\n"; exit(0); }
	Double_t cl1, cl2, cl3, cl4, cl5, cl6, cl7;

	in.precision(10);
	std::cout << setprecision(10);

	for (int i = 0; i < 10000; i++) {
		in >> cl1 >> cl2 >> cl3 >> cl4 >> cl5 >> cl6 >> cl7;
		// std::cout << i << ") " << cl1 << ", " << cl2 << ", " << cl3 << ", " << cl4 << ", " << cl5 << ", " << cl6 << ", " << cl7 << "\n";

		table.cutoff[i] = cl7;
	}

	in.close();

    // Store data to the StDbTable
    dbtable->SetTable((char*)&table, 1);

    // Store table to database
    mgr->storeDbTable(dbtable);

}
