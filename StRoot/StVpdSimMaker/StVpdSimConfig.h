//
//  StVpdSimConfig.h
//
//
//  Created by Nickolas Luttrell on 6/21/16.
//
//

#ifndef StVpdSimConfig_h
#define StVpdSimConfig_h

#include <iostream>
#include <fstream>
#include <vector>
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_vpdSimParams_Table.h"
#include "phys_constants.h"
#include <TRandom3.h>

using std::string;
class vpdSimParams_st;

class StVpdSimConfig : public StMaker {
public:

		StVpdSimConfig() {}
		~StVpdSimConfig() {}

    //! structure containing tube parameters
	struct SingleTubeParams{
        float singleTubeRes;    //!< Resolution of a particular Vpd tube in ps
        int tubeId;             //!< Tube Id (number) [0,37] with west Vpd [0,18] and east Vpd [19,37]
        int tubeStatusFlag;     //!< Status flag for whether tube was active (1) or inactive (0)
        int tubeTriggerFlag;    //!< Status flag for whether tube was triggered on (1) or not (0)
	};
    
    /**
    * Calculates the average resolution across all 38 tubes (discounts inactive tubes)
    * then returns a single vertex resolution (in ps) for use in embedding w/ vpdStart
    */
    double getVpdResolution(int nWest, int nEast) {
        loadVpdSimParams();
        double randNum = 0;
        double vpdRes = 0;
        int counter = 0;
        double tubeTimeWest = 0;
        double tubeTimeEast = 0;
        TRandom3 randEngine(0);
        
        for (int i=0; i<MAX_ARRAY_INDEX; i++) {
            if (mSimParams[i].tubeStatusFlag) {
                vpdRes += mSimParams[i].singleTubeRes;
                counter += 1;
            }
        }
        
        if (counter != 0) {
            vpdRes = vpdRes/counter;    //! Take an average
        }
        else {
            LOG_WARN << "No resolutions found! Exit!" << endm;
            return 0.;
        }
        
        for (int j=0;j<nWest;j++) {    //! Loop through West tubes
            randNum = randEngine.Gaus(0, vpdRes);
            tubeTimeWest += randNum;
        }
        
        for (int j=0;j<nEast;j++) {   //! Loop through East tubes
            randNum = randEngine.Gaus(0, vpdRes);
            tubeTimeEast += randNum;
        }
        
        tubeTimeWest = tubeTimeWest/nWest; //! Avg on west in ps
        tubeTimeEast = tubeTimeEast/nEast; //! Avg on east in ps
        
        LOG_INFO << "The resolution returned is: " << (tubeTimeEast - tubeTimeWest)/2 << endm;
        return (tubeTimeEast - tubeTimeWest)/2;
    }

    //! Loads Vpd Sim Params from database

	void loadVpdSimParams(const int date = 20160913, const int time = 175725, const char* Default_time = "2016-09-13 17:57:25")
	{
    
        St_db_Maker *dbMk = 0;

        TDataSet *DB = GetDataBase("Calibrations/tof/vpdSimParams");
        if (!DB) {
            LOG_INFO << "No data set found, creating new St_db_Maker..." << endm;
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

		St_vpdSimParams *dataset = 0;
		dataset = (St_vpdSimParams*) DB->Find("vpdSimParams");
    
        if (dataset) {
			TDatime val[3];
			dbMk->GetValidity((TTable*)dataset,val);
			vpdSimParams_st* table = static_cast<vpdSimParams_st*>(dataset->GetTable());

            //! Extract the parameter values from db into map
			SingleTubeParams params;
			for (int i = 0; i < MAX_ARRAY_INDEX; i++) {
				params.tubeId = table->tubeID[i];
				params.singleTubeRes = table->tubeRes[i];
				params.tubeStatusFlag = table->tubeStatusFlag[i];
				params.tubeTriggerFlag = table->tubeTriggerFlag[i];
				mSimParams[table->tubeID[i]] = params;
			}

			return;
		}
		else {
			LOG_WARN << "ERROR: dataset does not contain requested table" << endm;
			return;
		}
	}

	/** Reads VPD Sim Params from a file for DEBUG purposes
	 * TODO: add some safety for badly formed files
	 */
	void loadVpdSimParams(string params_filename ) {

		int MAX_DB_INDEX = 38;

		int vpdTubeRes;
		int vpdTubeId;
		int vpdTubeStatusFlag;
		int vpdTubeTriggerFlag;

		SingleTubeParams params;

		std::ifstream inData;
		inData.open( params_filename.c_str() );

		for (int i = 0; i < MAX_DB_INDEX; i++) {
			inData >> vpdTubeId >> vpdTubeRes >> vpdTubeStatusFlag >> vpdTubeTriggerFlag;
			params.tubeId = vpdTubeId;
			params.singleTubeRes = vpdTubeRes;
			params.tubeStatusFlag = vpdTubeStatusFlag;
			params.tubeTriggerFlag = vpdTubeTriggerFlag;
			mSimParams[params.tubeId] = params;
		}

		inData.close();
		return;
	}

	std::map<int, SingleTubeParams> getParams(){
		return mSimParams;
	}
    
    float getThreshold()  const { return mThreshold; }
    float getVpdDistance()    const { return VPDDISTANCE; }
    float getTDiffCut()   const { return TDIFFCUT; }
    float getMcClock()    const { return kMcClock; }

protected:
    
	//! stores a map of the single tube params indexed on tubeId
	std::map<int, SingleTubeParams> mSimParams;
	int mThreshold = 1;         //!< Threshold value for a tube to recognize it as a true hit.

    const int MAX_ARRAY_INDEX = 38;
	const float VPDDISTANCE = 570;       //!< Distance (in cm) of each Vpd from the zero point
	const float TDIFFCUT = 0.8;       //!< Cut value for eliminating times with a significant deviation from avg.
    const float kMcClock = 570*1.e9/C_C_LIGHT;    //!< Standard clock for pure simulation in ns. This is present to match the clock of BTof hits.
};

#endif /* Config_h */
