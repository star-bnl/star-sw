//
//  StBTofSimResParams.h
//
//
//  Created by jdb on 03/08/18.
//
//

#ifndef StBTofSimResParams_h
#define StBTofSimResParams_h

#include <iostream>
#include <fstream>
#include <vector>
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_tofSimResParams_Table.h"


// using std::string;
// class tofSimResParams_st;

class StBTofSimResParams : public StMaker {
public:

		StBTofSimResParams() {}
		~StBTofSimResParams() {}
	
	/**
	* Calculates the average resolution across all 38 tubes (discounts inactive tubes)
	* then returns a single vertex resolution (in ps) for use in embedding w/ vpdStart
	*/
	double timeres_tof(uint itray, uint imodule, uint icell) {
		double result = 8.5e-11;
		if ( itray > 120 || imodule > 32 || icell > 6 )
			return result;
		
		return params[ itray ][ imodule * 6 + icell ];

	}

	//! Loads Vpd Sim Params from database

	void loadParams(const int date = 20160913, const int time = 175725, const char* Default_time = "2016-09-13 17:57:25")
	{
	
		St_db_Maker *dbMk = dynamic_cast<St_db_Maker*>( GetChain()->GetMakerInheritsFrom("St_db_Maker") );

		TDataSet *DB = GetDataBase("Calibrations/tof/tofSimResParams");
		
		if (!DB) {
			LOG_WARN << "No data set found, creating new St_db_Maker... with date/time" << date << "/" << time << endm;
			dbMk = new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
			dbMk->SetDebug();
			dbMk->SetDateTime(date,time); //! event or run start time, set to your liking
			dbMk->SetFlavor("ofl");
			dbMk->Init();
			dbMk->Make();
		}

		if (!DB) {
			LOG_WARN << "Failed to connect to Database!" << endm;
			return;
		}

		St_tofSimResParams *dataset = 0;
		dataset = (St_tofSimResParams*) DB->Find("tofSimResParams");
	
		if (dataset) {
			TDatime val[3];
			dbMk->GetValidity((TTable*)dataset,val);
			tofSimResParams_st* table = static_cast<tofSimResParams_st*>(dataset->GetTable());

			for ( int i = 0; i < 120; i++ ){ //  nTrays
				for ( int j = 0; j < 192; j++ ){
					size_t index = i * 120 + j;
					params[i][j] =  table[0].resolution[index];
					LOG_DEBUG << "tray:" << i << ", mod cell:" << j << " = " << table[0].resolution[index]  << " == " << params[i][j] << endm;
				}
			}
			LOG_INFO << "Loaded tofSimResParams" << endm;
			return;
		}
		else {
			LOG_WARN << "ERROR: dataset does not contain requested table" << endm;
			return;
		}
	} // loadParams

protected:
	double params[120][192];
};

#endif /* Config_h */
