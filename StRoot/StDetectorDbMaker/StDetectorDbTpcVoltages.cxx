#include "StMessMgr.h"
#include "StDetectorDbTpcVoltages.h"
#include "tables/St_tpcHighVoltages_Table.h"

/*! 
 This class is used to get TPC voltages durring the run. The data contained within is NOT the voltages over time, but rather the set points used. These have only changed a few times per year. 
*/

/// Initialize Instance
StDetectorDbTpcVoltages* StDetectorDbTpcVoltages::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbTpcVoltages* StDetectorDbTpcVoltages::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbTpcVoltages();
    }

    // get new address of struck eack instance
    // data somehow gets garbles between events
    if(sInstance->mTable){ 
      sInstance->mTpcVoltages = (tpcHighVoltages_st*)(sInstance->mTable->GetArray());
	}
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbTpcVoltages::update(StMaker* maker){
    
    if(maker){
	// RDO in RunLog_onl
	
	TDataSet* dataSet = maker->GetDataBase("Calibrations/tpc");
	
	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("tpcHighVoltages"));
	    
	    if(mTable){
	      mTpcVoltages = (tpcHighVoltages_st*)(mTable->GetArray());
	    }
	    
	}
	
    }
};

/// Default constructor
StDetectorDbTpcVoltages::StDetectorDbTpcVoltages(){
    ~(*gMessMgr) << "StDetectorDbTpcVoltages::StDetectorDbTpcVoltages" << endm;
    mTpcVoltages = 0;
    mTable = 0;
};
	
/// Default destructor
StDetectorDbTpcVoltages::~StDetectorDbTpcVoltages(){
  delete sInstance;
  sInstance = 0;
};

/// Gets cathode voltages
double StDetectorDbTpcVoltages::getCathodeVoltage(){
  double value = 0;
  
  if(mTpcVoltages)
    value = mTpcVoltages->cathode;

  return value;
};

/// Gets gating grid voltage
double StDetectorDbTpcVoltages::getGGVoltage(){
  double value = 0;
  
  if(mTpcVoltages)
    value = mTpcVoltages->gatedGridRef;

  return value;
};


/// outputs information to ostream
ostream& operator<<(ostream& os, StDetectorDbTpcVoltages& v){

  os << "TPC High Voltages: Cathode = " << v.getCathodeVoltage()
     << " Gating Grid = " << v.getGGVoltage() << endl; 

  return os;
}

