#include "StDetectorDbTpcVoltages.h"
#include "tables/St_tpcHighVoltages_Table.h"


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
    cout << "StDetectorDbTpcVoltages::StDetectorDbTpcVoltages" << endl;
    mTpcVoltages = 0;
    mTable = 0;
};
	
/// Default destructor, does nothing
StDetectorDbTpcVoltages::~StDetectorDbTpcVoltages(){};


double StDetectorDbTpcVoltages::getCathodeVoltage(){
  double value = 0;
  
  if(mTpcVoltages)
    value = mTpcVoltages->cathode;

  return value;
};

double StDetectorDbTpcVoltages::getGGVoltage(){
  double value = 0;
  
  if(mTpcVoltages)
    value = mTpcVoltages->gatedGridRef;

  return value;
};



ostream& operator<<(ostream& os, StDetectorDbTpcVoltages& v){

  os << "TPC High Voltages: Cathode = " << v.getCathodeVoltage()
     << " Gating Grid = " << v.getGGVoltage() << endl; 

  return os;
}
