//
//  StBTofSimResParams.h
//
//
//  Modified by Zaochen on 07/21/2021.
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

		double average_timeres_tof(){return mAverageTimeResTof;}

		/**
		 * Calculates the average resolution across all 38 tubes (discounts inactive tubes)
		 * then returns a single vertex resolution (in ps) for use in embedding w/ vpdStart
		 */
		double timeres_tof(uint itray, uint imodule, uint icell) 
		{
			double result = 8.5e-11;
			if ( itray > 120 || imodule > 32 || icell > 6 )
				return result;

			return params[ itray-1 ][ (imodule-1) * 6 + (icell-1) ];
		}

		//! Loads BTOF Sim Params from database
		//void loadParams(const int date = 20160913, const int time = 175725, const char* Default_time = "2016-09-13 17:57:25")
		void loadParams(const int runNumber = 20076002)
		{
			
			const int yearNumber = runNumber/1000000 - 1 + 2000;
			const int date       = (yearNumber-1)*1000*10 + 1001; // prior year + October 1st(Date)

			St_db_Maker *dbMk = new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
			dbMk->SetDebug();
			dbMk->SetDateTime(date,1);
			dbMk->SetFlavor("ofl");
			dbMk->Init();
			dbMk->Make();

			TDataSet *DB = 0;
			DB = dbMk->GetDataBase("Calibrations/tof/tofSimResParams");

			if(!DB)
			{
				LOG_WARN << "ERROR: could not read requested table from DB" << endm;
				return;
			}

			St_tofSimResParams *dataset = 0;
			dataset = (St_tofSimResParams*) DB->Find("tofSimResParams");

			if(dataset) 
			{
				TDatime val[2];
				dbMk->GetValidity((TTable*)dataset,val);
				
				tofSimResParams_st* tofTable = static_cast<tofSimResParams_st*>(dataset->GetTable());
				
				const int mNTray         = 120;
				const int mNCellsPerTray = 192;

				mAverageTimeResTof = 0;
				for ( int i = 0; i < mNTray; i++ )
				{
					for ( int j = 0; j < mNCellsPerTray; j++ )
					{
						size_t index = i * mNCellsPerTray + j;
						params[i][j] = tofTable[0].resolution[index];

						mAverageTimeResTof += params[i][j];

						LOG_DEBUG << "tray:" << i << ", mod cell:" << j << " = " << tofTable[0].resolution[index]  << " == " << params[i][j] << endm;
					}
				}

				mAverageTimeResTof=mAverageTimeResTof/(mNTray*mNCellsPerTray);

				LOG_INFO << "Loaded tofSimResParams. Average = " << mAverageTimeResTof << endm;
				return;
			}
			else
			{
				LOG_WARN << "ERROR: dataset does not contain requested table" << endm;
				return;
			}

		} // loadParams

	protected:
		double params[120][192];
		double mAverageTimeResTof;
};

#endif /* Config_h */
