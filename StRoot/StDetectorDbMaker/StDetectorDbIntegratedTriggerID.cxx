#include "StMessMgr.h"
#include "StDetectorDbIntegratedTriggerID.h"
#include "tables/St_triggerInfo_Table.h"
#include "tables/St_defaultTrgLvl_Table.h"
#include "TUnixTime.h"

/*!
   This class provides an interface to triggerInfo from the database.

*/

/// Initialize Instance
StDetectorDbIntegratedTriggerID* StDetectorDbIntegratedTriggerID::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbIntegratedTriggerID* StDetectorDbIntegratedTriggerID::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbIntegratedTriggerID();
    }
    // Need to reinitilize array for some unkown reason
    // the array memeory changed althoug the TTable stays the same
    if(sInstance->mIDTable)
	sInstance->mTriggerInfo = (triggerInfo_st*)(sInstance->mIDTable->GetArray());
    if(sInstance->mDefTrgLvlTable)
	sInstance->mDefaultTriggerLevel = (defaultTrgLvl_st*)(sInstance->mDefTrgLvlTable->GetArray());
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbIntegratedTriggerID::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
	
	// TTable of triggerID	
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    mIDTable = dynamic_cast<TTable*>(dataSet->Find("triggerInfo"));
	    
	    if(mIDTable){
		mIDNumRows = mIDTable->GetNRows();
		mTriggerInfo = (triggerInfo_st*)(mIDTable->GetArray());
	    }
	}

	dataSet = 0;
	dataSet = maker->GetDataBase("Calibrations/trg");
	
	if(dataSet){
	    // TTable of default trigger levels
	    mDefTrgLvlTable = dynamic_cast<TTable*>(dataSet->Find("defaultTrgLvl"));
	    if(mDefTrgLvlTable){
		mDefaultTriggerLevel = (defaultTrgLvl_st*)(mDefTrgLvlTable->GetArray());
	    }
	}
	
	
    }
};

/// Default constructor
StDetectorDbIntegratedTriggerID::StDetectorDbIntegratedTriggerID(){
    ~(*gMessMgr) << "StDetectorDbIntegratedTriggerID::StDetectorDbIntegratedTriggerID" << endm;
    mTriggerInfo = 0;
    mIDNumRows = 0;
    mIDTable = 0;

    mDefaultTriggerLevel = 0;
    mDefTrgLvlTable = 0;
    
};

/// triggerID members
/// Returns Number of Entries in database
unsigned int StDetectorDbIntegratedTriggerID::getIDNumRows(){
    return mIDNumRows;
};

/// Returns Run Number
int StDetectorDbIntegratedTriggerID::getIDRunNumber(){
    int value = 0;
    if(mTriggerInfo)
	value = mTriggerInfo[0].runNumber;
    return value;
};

/// Default destructor
StDetectorDbIntegratedTriggerID::~StDetectorDbIntegratedTriggerID(){
  delete sInstance;
  sInstance = 0;
};

/// Returns idxTrg
int StDetectorDbIntegratedTriggerID::getIdxTrg(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].idxTrg;
    return value;
};

/// Returns daqTrgId
int StDetectorDbIntegratedTriggerID::getDaqTrgId(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].daqTrgId;
    return value;
};

/// Returns offlineTrgId
int StDetectorDbIntegratedTriggerID::getOfflineTrgId(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].offlineTrgId;
    return value;
};

/// Returns trgNameVersion
int StDetectorDbIntegratedTriggerID::getTrgNameVersion(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].trgNameVersion;
    return value;
};

/// Returns trgVersion
int StDetectorDbIntegratedTriggerID::getTrgVersion(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].trgVersion;
    return value;
};

/// Returns threashVersion
int StDetectorDbIntegratedTriggerID::getThreashVersion(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].threashVersion;
    return value;
};

/// Returns psVersion
int StDetectorDbIntegratedTriggerID::getPsVersion(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].psVersion;
    return value;
};

/// Returns psL0 
int StDetectorDbIntegratedTriggerID::getPsL0(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].psL0;
    return value;
};

/// Returns name 
char* StDetectorDbIntegratedTriggerID::getName(unsigned int entry){
    char* value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].name;
    return value;
};

/// Returns detectorLiveOnBits 
unsigned int StDetectorDbIntegratedTriggerID::getDetectorLiveOnBits(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].detectorLiveOnBits;
    return value;
};

/// Returns detectorLiveOffBits 
unsigned int StDetectorDbIntegratedTriggerID::getDetectorLiveOffBits(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].detectorLiveOffBits;
    return value;
};

/// Returns detectorRequest 
unsigned int StDetectorDbIntegratedTriggerID::getDetectorRequest(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].detectorRequest;
    return value;
};

/// Returns idxLevel
int StDetectorDbIntegratedTriggerID::getIdxLevel(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].idxLevel;
    return value;
};

/// Returns algorithmId
int StDetectorDbIntegratedTriggerID::getAlgorithmId(unsigned int entry){
    int value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].algorithmId;
    return value;
};


/// Returns ps 
float StDetectorDbIntegratedTriggerID::getPs(unsigned int entry){
    float value = 0;
    if(mTriggerInfo && entry < this->getIDNumRows() )
	value = mTriggerInfo[entry].ps;
    return value;
};

unsigned int StDetectorDbIntegratedTriggerID::getDefaultTriggerLevel(){
    unsigned int value = 999;
    if(mDefaultTriggerLevel)
	value = mDefaultTriggerLevel->level;
    return value;
};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbIntegratedTriggerID& v){
    os << endl << "RunNumber =  " << v.getIDRunNumber() << endl;
    
    for(unsigned int i = 0; i < v.getIDNumRows(); i++){
	os << "idx_trg: " << v.getIdxTrg(i) 
	   << "  daqTrg: " << v.getDaqTrgId(i)  
	   << "  offlineTrgId: " << v.getOfflineTrgId(i)  
	   << "  trgNameVersion: " << v.getTrgNameVersion(i) << endl
	   << "trgVersion: " << v.getTrgVersion(i)  
	   << "  threashVersion: " << v.getThreashVersion(i)  
	   << "  psVersion: " << v.getPsVersion(i) << endl;
	os << "psL0: " << v.getPsL0(i)  
	   << "  name: " << v.getName(i)   
	   << "  detectorLiveOnBits: " << v.getDetectorLiveOnBits(i) << endl 
	   << "detectorLiveOffBits: " << v.getDetectorLiveOffBits(i) 
	   << "  detectorRequest: " << v.getDetectorRequest(i)  << endl; 
	os << "idxLevel: " << v.getIdxLevel(i)  
	   << "  id (algorithm) : " << v.getAlgorithmId(i)  
	   << "  prescale : " << v.getPs(i) << endl <<endl; 
    }

    os << "Default Trigger Level: " << v.getDefaultTriggerLevel() <<endl <<endl;
    
    return os;
};
