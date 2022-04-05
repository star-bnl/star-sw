// 
// Script to re-upload database data from timestamp X to timestamp Y, also
// known as "annual db initialization procedure"
//
// Input parameters: 
// - database name; typically Calibrations_{subsystem_name} or Geometry_{subsystem_name}. Ex: Calibrations_tpc
// - table name; please check http://www.star.bnl.gov/Browser/STAR/ for the list of your tables
// - flavor name; could be "ofl", "simu" and something else, specific to your subsystem
// - request timestamp; "FROM" timestamp
// - store timestamp; "TO" timestamp
//
// contact : "Dmitry Arkhipkin" <arkhipkin@bnl.gov>
// BNL office: (631) 344 4922 
// skype: dmitry.arkhipkin
// 

void table_reupload(const char* fDbName = "Geometry_fms", const char* fTableName = "fmsChannelGeometry", const char* fFlavorName = "ofl", 
				const char* fRequestTimestamp = "2014-02-22 00:00:00", 
				const char* fStoreTimestamp = "2014-12-20 00:00:00" ) {

	// real-life example : 
	// fDbName = "Calibrations_tpc";
	// fTableName = "tpcGas";
	// fFlavorName = "ofl"; // "ofl", "simu", other..
	// fRequestTimestamp = "2010-05-05 00:00:00";
    // fStoreTimestamp = "2011-05-05 00:00:00";

	if (!fDbName || !fTableName || !fFlavorName || !fRequestTimestamp || ! fStoreTimestamp) {
		std::cerr << "ERROR: Missing initialization data, please check input parameters!\n";
		return;
	}

	gSystem->Setenv("DB_ACCESS_MODE", "write");

    // Load all required libraries
    gROOT->Macro("LoadLogger.C");
    gSystem->Load("St_base.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");


    // Initialize db manager
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig(fDbName);
    StDbTable* dbtable = node->addDbTable(fTableName);
	dbtable->setFlavor(fFlavorName);

	// read data for specific timestamp
	mgr->setRequestTime(fRequestTimestamp);
	mgr->fetchDbTable(dbtable); 
	
	// output results 
	std::cout << "READ CHECK: " << dbtable->printCstructName() << " has data: " << (dbtable->hasData() ? "yes" : "no") << " (" << dbtable->GetNRows() << " rows)" << std::endl;

	if (!dbtable->hasData()) {
		std::cout << "ERROR: This table has no data to reupload. Please try some other timestamp!";
		return;
	}

	char confirm[255];
	std::string test_cnf;
	std::cout << "ATTENTION: please confirm that you want to reupload " << fDbName << " / " << fTableName << ", " << fRequestTimestamp << " data with " << fStoreTimestamp << " timestamp.\n Type YES to proceed: ";
	std::cin.getline(confirm,256);
	test_cnf = confirm;
	if (test_cnf != "YES") {
		std::cout << "since you've typed \"" << test_cnf << "\" and not \"YES\", data won't be reuploaded." << std::endl;
		return;
	}

	// store data back with new timestamp
	if (dbtable->hasData()) {
		mgr->setStoreTime(fStoreTimestamp);
		if (mgr->storeDbTable(dbtable)) {
			std::cout << "SUCCESS: Data reupload complete for " << fDbName << " / " << fTableName << " [ flavor : " << fFlavorName << " ]"
			<< "\n" << "Data copied FROM " << fRequestTimestamp << " TO " << fStoreTimestamp << std::endl << std::endl; 
		} else {
			std::cerr << "ERROR: Something went wrong. Please send error message text to DB Admin!" << std::endl;
		}
	}
}
