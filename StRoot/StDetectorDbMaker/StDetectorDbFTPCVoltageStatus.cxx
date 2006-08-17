#include "StMessMgr.h"
#include "StDetectorDbFTPCVoltageStatus.h"
#include "tables/St_ftpcVoltageStatus_Table.h"

/* !
  The class holds the FTPC voltage status database values needed in the chain. It is a singleton which requires manual updating, usually taken care of in StDetectorDbMaker.cxx::Make(). If no data exists all values return 0. To use:
  #include "StDetectorDbMaker/StDetectorDbFTPCVoltageStatus.h"
  StDetectorFTPCVoltageStatus * gas = StDetectorDbFTPCVoltageStatus::instance();
  ~(*gMessMgr) << *voltage << endm;

  gas->getStatusFTPCEast();
  or any other access methods.

  Do not use the update function unless you know what you are doing. It should not cause a problem, it will just access the database more often than needed.
*/

/// Initilizes singleton pointer
StDetectorDbFTPCVoltageStatus* StDetectorDbFTPCVoltageStatus::sInstance = 0;


/// returns singleton pointer if it exits, makes new one if needed
/// this is the default way to invoke an instance
StDetectorDbFTPCVoltageStatus* StDetectorDbFTPCVoltageStatus::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbFTPCVoltageStatus();
    }

    return sInstance;
};


/// Updates data from database. Only use if know what you are doing
void StDetectorDbFTPCVoltageStatus::update(StMaker* maker){

    if(maker){
	TDataSet* dataSet = maker->GetDataBase("Calibrations/ftpc");
	if(dataSet){
	    TTable* voltageTable  = 
                   dynamic_cast<TTable*>(dataSet->Find("ftpcVoltageStatus"));
	    if(voltageTable) 
		mVoltageStatus = (ftpcVoltageStatus_st*)(voltageTable->GetArray());
	}
	
    }
};

/// Default constructor
StDetectorDbFTPCVoltageStatus::StDetectorDbFTPCVoltageStatus(){
    ~(*gMessMgr) << "StDetectorDbFTPCVoltageStatus::StDetectorDbFTPCVoltageStatus" << endm;
    mVoltageStatus = 0;
};

/// Default destructor 
StDetectorDbFTPCVoltageStatus::~StDetectorDbFTPCVoltageStatus(){
  delete sInstance;
  sInstance = 0;
};


/// Status East (1 is good, 0 is bad)
unsigned int StDetectorDbFTPCVoltageStatus::getStatusFTPCEast(){
    unsigned int value = 999;
    if(mVoltageStatus)
	value = mVoltageStatus->statusEast;
    return value;
};
/// Status East (1 is good, 0 is bad)
unsigned int StDetectorDbFTPCVoltageStatus::getStatusFTPCWest(){
    unsigned int value = 999;
    if(mVoltageStatus)
	value = mVoltageStatus->statusWest;
    return value;
};

ostream& operator<<(ostream& os, StDetectorDbFTPCVoltageStatus& v){

    os << "East Status = " << v.getStatusFTPCEast() << endl; 
    os << "West Status  = " << v.getStatusFTPCWest() << endl;
    
    
    return os;
    
};
