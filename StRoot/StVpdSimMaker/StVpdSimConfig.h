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
#include "StDetectorDbMaker/St_vpdSimParamsC.h"
#include "phys_constants.h"
#include <TRandom3.h>

using std::string;
class vpdSimParams_st;

class StVpdSimConfig {
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

	void loadVpdSimParams()	{
	  SingleTubeParams params;
	  for (int i = 0; i < MAX_ARRAY_INDEX; i++) {
	    params.tubeId = St_vpdSimParamsC::instance()->tubeID()[i];
	    params.singleTubeRes = St_vpdSimParamsC::instance()->tubeRes()[i];
	    params.tubeStatusFlag = St_vpdSimParamsC::instance()->tubeStatusFlag()[i];
	    params.tubeTriggerFlag = St_vpdSimParamsC::instance()->tubeTriggerFlag()[i];
	    mSimParams[St_vpdSimParamsC::instance()->tubeID()[i]] = params;
	  }
	  return;
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
