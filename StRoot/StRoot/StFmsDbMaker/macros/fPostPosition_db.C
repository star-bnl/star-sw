#include "TROOT.h"
#include "TDataSet.h"
#include "TDatime.h"
#include "TString.h"
#include "TSystem.h"

#include <iostream>
#include <fstream>

#if 0
typedef struct
{
	unsigned short slatid;	/* 0-240: slat Id */
	float length;			/* length ( Depends on S1,S2,etc. cm) */
	float width;			/* width (5 cm) */
	float thickness;		/* thickness (1 cm) */
	float angle_xy;			/* ? angle in the xy plan measured wrt the positive x-axis (45 South, 135 North)*/
	float xoffset;			/* xoffset from beam line to center of detector (cm) */
	float yoffset;			/* yoffset from beam line to center of detector (cm) */
	float zoffset;			/* z position from IR (cm) */
} fpostPosition_st;
#endif

void fPostPosition_db(
		const char* opt   = "",
		const char* year  = "17sim",
		const char* input = "fPostPosition.txt")
{
	// storeTime is beginning time for validity range in case of WRITING DB
	TString option(opt), yr(year), storeTime;
	int date, time; // time for READING DB

	std::cout <<"year = " <<year <<std::endl;
	if (yr.Contains("17sim"))
	{
		storeTime = "2016-12-10 00:00:01";
		date = 20161210;
		time = 1;
	}
	else if (yr.Contains("17ofl"))
	{
		storeTime = "2016-12-20 00:00:01";
		date = 20161220;
		time = 1;
	}
	else { std::cout << "Please specify valid year tag\n"; exit; }
	std::cout << "Opt ="         << opt << "\n";
	std::cout << "write = "      << option.Contains("writedb") << "\n";
	std::cout << "storetime = "  << storeTime << "\n";
	std::cout << "date, time = " << date <<" "<< time << "\n";

	gROOT->Macro("./loadlib.C");

	//-------------------------------------------

	const Int_t MAX_DB_INDEX = 241;
	fpostPosition_st in[MAX_DB_INDEX];

	FILE *FP = fopen(input, "r");
	if (!FP) { printf("Could not open %s\n", input); exit; }
	printf("\nReading %s\n", input);

	char line[1000];
	int n = 0;
	while (fgets(line, 1000, FP) != NULL)
	{
		sscanf(line,"%d	%f %f %f %f	%f %f %f ",
				&in[n].slatid,
				&in[n].length, &in[n].width, &in[n].thickness, &in[n].angle_xy,
				&in[n].xoffset,	&in[n].yoffset,	&in[n].zoffset);
		printf("Id=%3d D=%9.4f %9.4f %9.4f %9.4f X=%9.4f %9.4f %9.4f\n",
				in[n].slatid,
				in[n].length, in[n].width, in[n].thickness, in[n].angle_xy,
				in[n].xoffset, in[n].yoffset, in[n].zoffset);
		n++;
	}
	printf("Found %d entries\n", n);

	//-------------------------------------------

	#if 1
	if (option.Contains("writedb"))
	{
		gSystem->Setenv("DB_ACCESS_MODE", "write");
		StDbManager*    mgr     = StDbManager::Instance();
		StDbConfigNode* node    = mgr->initConfig("Geometry_fps");
		StDbTable*      dbtable = node->addDbTable("fpostPosition");
		mgr->setStoreTime(storeTime.Data());
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
		DB = dbMk->GetDataBase("Geometry/fps/fpostPosition");
		if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;

		St_fpostPosition* dataset = 0;
		dataset = (St_fpostPosition*) DB->Find("fpostPosition");
		if (!dataset) { std::cout << "ERROR: dataset does not contain requested table" << std::endl; return; }
		Int_t rows = dataset->GetNRows();
		if (rows > 1) std::cout << "INFO: found INDEXED table with " << rows << " rows" << std::endl;

		TDatime val[2];
		dbMk->GetValidity((TTable*)dataset, val);
		std::cout << "Dataset validity range: [ " << val[0].GetDate() << "." << val[0].GetTime() << " - "
			<< val[1].GetDate() << "." << val[1].GetTime() << " ] " << std::endl;    

		fpostPosition_st *table = dataset->GetTable();
		for (Int_t i = 0; i < rows; i++)
		{
			std::cout << Form("Row=%3d slatid=%3d dim=%9.4f %9.4f %9.4f %9.4f offset=%9.4f %9.4f %9.4f\n",
					i, table[i].slatid,
					table[i].length, table[i].width, table[i].thickness, table[i].angle_xy,
					table[i].xoffset, table[i].yoffset, table[i].zoffset);
		}
	}
	#endif

	return;
}//Main
