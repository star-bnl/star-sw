#include "StMessMgr.h"
#include "StDetectorDbFTPCVoltage.h"
#include "tables/St_ftpcVoltage_Table.h"

/* !
  The class holds the FTPC voltage database values needed in the chain. It is a singleton which requires manual updating, usually taken care of in StDetectorDbMaker.cxx::Make(). If no data exists all values return 0. To use:
  #include "StDetectorDbMaker/StDetectorDbFTPCVoltage.h"
  StDetectorFTPCVoltage * gas = StDetectorDbFTPCVoltage::instance();
  ~(*gMessMgr) << *voltage << endm;

  gas->getCathodeVEast();
  or any other access methods.

  Do not use the update function unless you know what you are doing. It should not cause a problem, it will just access the database more often than needed.
*/

/// Initilizes singleton pointer
StDetectorDbFTPCVoltage* StDetectorDbFTPCVoltage::sInstance = 0;


/// returns singleton pointer if it exits, makes new one if needed
/// this is the default way to invoke an instance
StDetectorDbFTPCVoltage* StDetectorDbFTPCVoltage::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbFTPCVoltage();
    }

    return sInstance;
};


/// Updates data from database. Only use if know what you are doing
void StDetectorDbFTPCVoltage::update(StMaker* maker){

    if(maker){
	TDataSet* dataSet = maker->GetDataBase("Calibrations/ftpc");
	if(dataSet){
	    TTable* voltageTable  = 
                   dynamic_cast<TTable*>(dataSet->Find("ftpcVoltage"));
	    if(voltageTable) 
		mVoltage = (ftpcVoltage_st*)(voltageTable->GetArray());
	}
	
    }
};

/// Default constructor
StDetectorDbFTPCVoltage::StDetectorDbFTPCVoltage(){
    ~(*gMessMgr) << "StDetectorDbFTPCVoltage::StDetectorDbFTPCVoltage" << endm;
    mVoltage = 0;
};

/// Default destructor 
StDetectorDbFTPCVoltage::~StDetectorDbFTPCVoltage(){
  delete sInstance;
  sInstance = 0;
};


/// Cathode voltage East (ElementID = 2)
double StDetectorDbFTPCVoltage::getCathodeVEast(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->cathodeVEast;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV1East(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV1East;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV2East(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV2East;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV3East(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV3East;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV4East(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV4East;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV5East(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV5East;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV6East(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV6East;
    return value;
};

/// Cathode voltage West (ElementID=1)
double StDetectorDbFTPCVoltage::getCathodeVWest(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->cathodeVWest;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV1West(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV1West;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV2West(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV2West;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV3West(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV3West;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV4West(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV4West;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV5West(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV5West;
    return value;
};

double StDetectorDbFTPCVoltage::getAnodeV6West(){
    double value = 0;
    if(mVoltage)
	value = mVoltage->anodeV6West;
    return value;
};

ostream& operator<<(ostream& os, StDetectorDbFTPCVoltage& v){

    os << "West CathodeV = " << v.getCathodeVWest() << endl; 
    os << "West AnodeV1  = " << v.getAnodeV1West() << " "
       << "West AnodeV2  = " << v.getAnodeV2West() << endl;
    os << "West AnodeV3  = " << v.getAnodeV3West() << " " 
       << "West AnodeV4  = " << v.getAnodeV4West() << endl;
    os << "West AnodeV5  = " << v.getAnodeV5West() << " " 
       << "West AnodeV6  = " << v.getAnodeV6West() << endl;
    os << "East Voltage = "  << v.getCathodeVEast() << endl;
    os << "East AnodeV1  = " << v.getAnodeV1East() << " " 
       << "East AnodeV2  = " << v.getAnodeV2East() << endl;
    os << "East AnodeV3  = " << v.getAnodeV3East() << " " 
       << "East AnodeV4  = " << v.getAnodeV4East() << endl;
    os << "East AnodeV5  = " << v.getAnodeV5East() << " " 
       << "East AnodeV6  = " << v.getAnodeV6East() << endl;
    
    return os;
    
};
