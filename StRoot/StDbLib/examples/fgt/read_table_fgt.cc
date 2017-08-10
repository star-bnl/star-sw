#include <ctime>
#include <iostream>

#include "fgtGain.h"

#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"

int main () {

	const char* fDbName = "Calibrations_fgt";	// Database bame
	const char* fTableName = "fgtGain";		// c-structure name that is same as table in database
	const char* fFlavorName = "ofl";			// flavor name, like 'ofl', 'sim'  
	time_t funixTime = time(0); // requested timestamp
	int fDebug = 0; // debug mode..

	if ( funixTime==0 ) {
		std::cout << " No timestamp specified " << std::endl;
		return 0;
	}

	if ( fDbName == 0 ){
		std::cout << "No database name specified" << std::endl;
		return 0;
	}

	if ( fTableName == 0 ){
		std::cout << "No table name specified" << std::endl;
		return 0;
	}

	StDbManager* mgr = StDbManager::Instance();               // Get the singleton manager
	if ( fDebug == 1 ){ 
		mgr->setVerbose(fDebug);                             // Set verbose mode for debuging for fDebug=1
	}

	StDbConfigNode* configNode = mgr->initConfig(fDbName);  // Connect to the db & get an empty container

	StDbTable* dbtable = configNode->addDbTable(fTableName);

	if ( dbtable == 0 ){                                // If table asigned by fTableName does not exist in Dababase
		std::cout << " No Table : " << fTableName << std::endl;   // program is stoped and exit from this function.
		return 0;
	}

	if ( fFlavorName != 0 ){
		dbtable->setFlavor(fFlavorName);
		std::cout << "Flavor is set as " << fFlavorName << " by StDbTable::setFlavor." << std::endl;
	} else {
		std::cout << "Flavor is NOT assigned. Default value is set as 'ofl'. " << std::endl;
		dbtable->setFlavor("ofl");
	}

    mgr->setRequestTime(funixTime);
 
	mgr->fetchDbTable(dbtable); // Fetch the data from Database 

	fgtGain_st* gain = (fgtGain_st*)dbtable->GetTable();	
	std::cout << gain->Gain[0] << std::endl;

  return 1;
}
