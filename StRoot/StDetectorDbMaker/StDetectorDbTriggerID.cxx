#include <iostream.h>
#include "StDetectorDbTriggerID.h"
#include "tables/St_triggerID_Table.h"
#include "TUnixTime.h"

/*!
   This class provides an interface to triggerID's from the database.

*/

/// Initialize Instance
StDetectorDbTriggerID* StDetectorDbTriggerID::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbTriggerID* StDetectorDbTriggerID::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbTriggerID();
    }
    // Need to reinitilize array for some unkown reason
    // the array memeory changed althoug the TTable stays the same
    if(sInstance->mTable)
	sInstance->mTriggerID = (triggerID_st*)(sInstance->mTable->GetArray());
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbTriggerID::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
	// RDO in RunLog_onl
	
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("triggerID"));
	    
	    if(mTable){
		mNumRows = mTable->GetNRows();
		mTriggerID = (triggerID_st*)(mTable->GetArray());
	    }
	}
    }
};

/// Default constructor
StDetectorDbTriggerID::StDetectorDbTriggerID(){
    cout << "StDetectorDbTriggerID::StDetectorDbTriggerID" << endl;
    mTriggerID = 0;
    mNumRows = 0;
    mTable = 0;
};

/// Returns Number of Entries in database
unsigned int StDetectorDbTriggerID::getNumRows(){
    return mNumRows;
};

/// Returns Run Number
unsigned int StDetectorDbTriggerID::getRunNumber(){
    unsigned int value = 0;
    if(mTriggerID)
	value = mTriggerID[0].runNumber;
    return value;
};

/// Default destructor
StDetectorDbTriggerID::~StDetectorDbTriggerID(){
  delete sInstance;
  sInstance = 0;
};

/// Returns idxTrg
unsigned int StDetectorDbTriggerID::getIdxTrg(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getNumRows() )
	value = mTriggerID[entry].idxTrg;
    return value;
};

/// Returns daqTrgId
unsigned int StDetectorDbTriggerID::getDaqTrgId(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getNumRows() )
	value = mTriggerID[entry].daqTrgId;
    return value;
};

/// Returns offlineTrgId
unsigned int StDetectorDbTriggerID::getOfflineTrgId(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getNumRows() )
	value = mTriggerID[entry].offlineTrgId;
    return value;
};

/// Returns trgNameVersion
unsigned int StDetectorDbTriggerID::getTrgNameVersion(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getNumRows() )
	value = mTriggerID[entry].trgNameVersion;
    return value;
};

/// Returns trgVersion
unsigned int StDetectorDbTriggerID::getTrgVersion(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getNumRows() )
	value = mTriggerID[entry].trgVersion;
    return value;
};

/// Returns threashVersion
unsigned int StDetectorDbTriggerID::getThreashVersion(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getNumRows() )
	value = mTriggerID[entry].threashVersion;
    return value;
};

/// Returns psVersion
unsigned int StDetectorDbTriggerID::getPsVersion(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getNumRows() )
	value = mTriggerID[entry].psVersion;
    return value;
};


/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbTriggerID& v){
    os << "Run: " << v.getRunNumber() << endl;

	for(unsigned int i = 0; i < v.getNumRows(); i++){
	    os << "idx_trg: " << v.getIdxTrg(i) 
               << "  daqTrg: " << v.getDaqTrgId(i)  
               << "  offlineTrgId: " << v.getOfflineTrgId(i)  
               << "  trgNameVersion: " << v.getTrgNameVersion(i) << endl
               << "  trgVersion: " << v.getTrgVersion(i)  
               << "  threashVersion: " << v.getThreashVersion(i)  
               << "  psVersion: " << v.getPsVersion(i) << endl;
	}
    return os;
};
