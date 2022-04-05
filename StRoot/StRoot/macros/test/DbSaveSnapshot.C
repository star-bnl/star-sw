#include <iomanip>

void DbSaveSnapshot(){

  // Baseline shared libraries
  gSystem->Load("libTable");
  gSystem->Load("St_Tables");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StStarLogger.so");
  gSystem->Load("StUtilities.so");

  // DB-specific libs
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_db_Maker");


  // create makers connecting to databases RunParams & Geometry

  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
  dbMk->SetMaxEntryTime(20100401,0); // DBV timestamp, must be set before Init() call
  dbMk->SetFlavor("ofl+sim");
  dbMk->Init();
  dbMk->SetDateTime(20090320,45000); 	// SDT timestamp / event timestamp

  dbMk->SaveSnapshotPlus("Geometry");     // All three databases required
  dbMk->SaveSnapshotPlus("Calibrations"); // to get a consistent snapshot.
  dbMk->SaveSnapshotPlus("Conditions");   // RunLog databases are not covered yet..

  dbMk->SaveSnapshotPlus("RunLog/onl");                                                                                                              
  dbMk->SaveSnapshotPlus("RunLog/MagFactor"); 

  // Update for SIM tables :

  gSystem->Exec("rm ./StarDb/Geometry/tpc/tpcGlobalPosition*");                                                                                      
  gSystem->Exec("rm ./StarDb/RunLog/onl/starClockOnl*"); 

  dbMk->SetFlavor("sim");
  dbMk->SaveSnapshotPlus("Geometry/tpc/tpcGlobalPosition");
  dbMk->SaveSnapshotPlus("RunLog/onl/starClockOnl");


	std::string rm;
	std::stringstream ostr;
	ostr.str("");

	for (int i = 1; i <= 24; i++) {
		ostr.str("");
		ostr << "Geometry/tpc/Sector_";
		ostr << std::setw(2) << std::setfill('0') << i << "/tpcSectorPosition";
		std::cout << ostr.str() << std::endl;
		rm = "rm ./StarDb/";
		rm.append(ostr.str());
		rm.append("*");
		std::cout << rm << std::endl;
		gSystem->Exec(rm.c_str());

		dbMk->SaveSnapshotPlus(ostr.str().c_str());
		ostr.str("");
		ostr << "Calibrations/tpc/Sector_";
		ostr << std::setw(2) << std::setfill('0') << i << "/tpcISTimeOffsets";
		std::cout << ostr.str() << std::endl;
		rm = "rm ./StarDb/";
		rm.append(ostr.str());
		rm.append("*");
		std::cout << rm << std::endl;
		gSystem->Exec(rm.c_str());


		dbMk->SaveSnapshotPlus(ostr.str().c_str());
		ostr.str("");
		ostr << "Calibrations/tpc/Sector_";
		ostr << std::setw(2) << std::setfill('0') << i << "/tpcOSTimeOffsets";
		std::cout << ostr.str() << std::endl;
		rm = "rm ./StarDb/";
		rm.append(ostr.str());
		rm.append("*");
		std::cout << rm << std::endl;
		gSystem->Exec(rm.c_str());

		dbMk->SaveSnapshotPlus(ostr.str().c_str());
	}

}








