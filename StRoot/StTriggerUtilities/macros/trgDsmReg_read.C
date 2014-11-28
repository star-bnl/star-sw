{

  // generic libraries, nothing special here
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StBFChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");

  // db-related libraries
  gSystem->Load("St_Tables");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");


// this will be in chain
St_db_Maker *dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
dbMk->SetDebug();
dbMk->SetDateTime(20090425,55558); // run start time
dbMk->SetFlavor("ofl"); // for offline calibrations/mapping
// dbMk->SetFlavor("simu"); // for simulations
dbMk->Init();
int runNumber = 10115007;
dbMk->InitRun(runNumber); // run number
dbMk->Make();

// this is usually done inside ::Make method 
TDataSet *DB = 0;
// "dbMk->" will NOT be needed, if done inside your Maker. Simply use DB = GetInputDb("RunLog/onl/triggerID")
DB = dbMk->GetInputDB("RunLog/onl/trgDsmReg");
if (!DB) { std::cout << "ERROR: no db maker constructed?" << std::endl; }

//
// c-struct headers, produced from .idl files, reside here :
//   less $STAR_LIB/../include/triggerID.h
// and here:
//   less $STAR_LIB/../include/tables/St_triggerID_Table.h 
//

// fetch ROOT descriptor of db table
St_trgDsmReg *desc = 0;
desc = (St_trgDsmReg*) DB->Find("trgDsmReg");
// fetch data and place it to appropriate structure
if (desc) {
	trgDsmReg_st *table = desc->GetTable();
	Int_t numrows = desc->GetNRows();	
	std::cout << "total rows for trgDsmReg, run " << runNumber << " found: " << numrows << std::endl;
	// display data
	for (int i = 0; i < numrows; i++) {
		std::cout << table[i].runNumber << ", "
		<< (unsigned)table[i].dcObject << ", " 
		<< (unsigned)table[i].dcIndex << ", " 
		<< table[i].dcRegister << ", " 
		<< table[i].dcLabel << ", " 
		<< table[i].dcValue << ", " 
		<< table[i].dcDefaultvalue << "\n"; 
	}
} else {
	std::cout << "WARNING: No data in trgDsmReg table (wrong timestamp?). Nothing to return, then.\n";
}

} // end of macro
