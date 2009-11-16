// $Id: StDetectorDbTriggerID.cxx,v 1.13 2007/05/11 05:30:29 dunlop Exp $
//
// $Log: StDetectorDbTriggerID.cxx,v $
// Revision 1.13  2007/05/11 05:30:29  dunlop
// Add in the additionalTriggerID table
//
// Revision 1.12  2006/11/29 20:53:37  fisyak
// Fix for icc
//
// Revision 1.11  2006/05/04 17:44:34  dunlop
// moved $LOG
//
// Revision 1.10  2006/05/04 17:39:57  dunlop
// Doxygenized and cvs commented
//

#include "StMessMgr.h"
#include "StDetectorDbTriggerID.h"
#include "tables/St_triggerID_Table.h"
#include "tables/St_additionalTriggerID_Table.h"
#include "tables/St_trigPrescales_Table.h"
#include "tables/St_L0TriggerInfo_Table.h"
#include "tables/St_defaultTrgLvl_Table.h"
#include "tables/St_trigL3Expanded_Table.h"
#include "tables/St_dsmPrescales_Table.h"

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
    if(sInstance->mIDTable)
	sInstance->mTriggerID = (triggerID_st*)(sInstance->mIDTable->GetArray());
    if(sInstance->mAdditionalTriggerIDTable)
	sInstance->mAdditionalTriggerID = (additionalTriggerID_st*)(sInstance->mAdditionalTriggerIDTable->GetArray());
    if(sInstance->mSTable)
	sInstance->mTrigPrescales = (trigPrescales_st*)(sInstance->mSTable->GetArray());
    if(sInstance->mL0Table)
	sInstance->mL0TriggerInfo = (L0TriggerInfo_st*)(sInstance->mL0Table->GetArray());
    if(sInstance->mDefTrgLvlTable)
	sInstance->mDefaultTriggerLevel = (defaultTrgLvl_st*)(sInstance->mDefTrgLvlTable->GetArray());
    if(sInstance->mTrigL3ExpandedTable)
	sInstance->mTrigL3Expanded = (trigL3Expanded_st*)(sInstance->mTrigL3ExpandedTable->GetArray());
    if(sInstance->mDsmPrescalesTable)
	sInstance->mDsmPrescales = (dsmPrescales_st*)(sInstance->mDsmPrescalesTable->GetArray());
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbTriggerID::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
	
	// TTable of triggerID	
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    mIDTable = dynamic_cast<TTable*>(dataSet->Find("triggerID"));
	    
	    if(mIDTable){
		mIDNumRows = mIDTable->GetNRows();
		mTriggerID = (triggerID_st*)(mIDTable->GetArray());
	    }
	    // TTable of additionalTriggerID
	    mAdditionalTriggerIDTable = dynamic_cast<TTable*>(dataSet->Find("additionalTriggerID"));
	    
	    if(mAdditionalTriggerIDTable){
		mAdditionalTriggerIDNumRows = mAdditionalTriggerIDTable->GetNRows();
		mAdditionalTriggerID = (additionalTriggerID_st*)(mAdditionalTriggerIDTable->GetArray());
	    }

	    // TTable of trigPrescales	
	    mSTable = dynamic_cast<TTable*>(dataSet->Find("trigPrescales"));
	    
	    if(mSTable){
		mSNumRows = mSTable->GetNRows();
		mTrigPrescales = (trigPrescales_st*)(mSTable->GetArray());
            }

	    // TTable of L0TriggerInfo	
	    mL0Table = dynamic_cast<TTable*>(dataSet->Find("L0TriggerInfo"));
	    
	    if(mL0Table){
		mL0NumRows = mL0Table->GetNRows();
		mL0TriggerInfo = (L0TriggerInfo_st*)(mL0Table->GetArray());
            }

	    // TTable of trigL3Expanded	
	    mTrigL3ExpandedTable = dynamic_cast<TTable*>(dataSet->Find("trigL3Expanded"));
	    
	    if(mTrigL3ExpandedTable){
		mTrigL3ExpandedNumRows = mTrigL3ExpandedTable->GetNRows();
		mTrigL3Expanded = (trigL3Expanded_st*)(mTrigL3ExpandedTable->GetArray());
            }

	    // TTable of dsmPrescales	
	    mDsmPrescalesTable = dynamic_cast<TTable*>(dataSet->Find("dsmPrescales"));
	    
	    if(mDsmPrescalesTable){
		mDsmPrescalesNumRows = mDsmPrescalesTable->GetNRows();
		mDsmPrescales = (dsmPrescales_st*)(mDsmPrescalesTable->GetArray());
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
StDetectorDbTriggerID::StDetectorDbTriggerID(){
    ~(*gMessMgr) << "StDetectorDbTriggerID::StDetectorDbTriggerID" << endm;
    mTriggerID = 0;
    mIDNumRows = 0;
    mIDTable = 0;

    mAdditionalTriggerID = 0;
    mAdditionalTriggerIDNumRows = 0;
    mAdditionalTriggerIDTable = 0;

    mTrigPrescales = 0;
    mSNumRows = 0;
    mSTable = 0;

    mL0TriggerInfo = 0;
    mL0NumRows = 0;
    mL0Table = 0;

    mDefaultTriggerLevel = 0;
    mDefTrgLvlTable = 0;

    mTrigL3Expanded = 0;
    mTrigL3ExpandedTable = 0;
    mTrigL3ExpandedNumRows = 0;

    mDsmPrescales = 0;
    mDsmPrescalesTable = 0;
    mDsmPrescalesNumRows = 0;
    
};
/// Default destructor
StDetectorDbTriggerID::~StDetectorDbTriggerID(){
  delete sInstance;
  sInstance = 0;
};

/// triggerID members
/// Returns Number of Entries in database
unsigned int StDetectorDbTriggerID::getIDNumRows(){
    return mIDNumRows;
};

/// Returns Run Number
unsigned int StDetectorDbTriggerID::getIDRunNumber(){
    unsigned int value = 0;
    if(mTriggerID)
	value = mTriggerID[0].runNumber;
    return value;
};


/// Returns idxTrg
unsigned int StDetectorDbTriggerID::getIdxTrg(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getIDNumRows() )
	value = mTriggerID[entry].idxTrg;
    return value;
};

/// Returns daqTrgId
unsigned int StDetectorDbTriggerID::getDaqTrgId(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getIDNumRows() )
	value = mTriggerID[entry].daqTrgId;
    return value;
};

/// Returns offlineTrgId
unsigned int StDetectorDbTriggerID::getOfflineTrgId(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getIDNumRows() )
	value = mTriggerID[entry].offlineTrgId;
    return value;
};

/// Returns trgNameVersion
unsigned int StDetectorDbTriggerID::getTrgNameVersion(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getIDNumRows() )
	value = mTriggerID[entry].trgNameVersion;
    return value;
};

/// Returns trgVersion
unsigned int StDetectorDbTriggerID::getTrgVersion(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getIDNumRows() )
	value = mTriggerID[entry].trgVersion;
    return value;
};

/// Returns threashVersion
unsigned int StDetectorDbTriggerID::getThreashVersion(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getIDNumRows() )
	value = mTriggerID[entry].threashVersion;
    return value;
};

/// Returns psVersion
unsigned int StDetectorDbTriggerID::getPsVersion(unsigned int entry){
    unsigned int value = 0;
    if(mTriggerID && entry < this->getIDNumRows() )
	value = mTriggerID[entry].psVersion;
    return value;
};


/// additionalTriggerID members
/// Returns Number of Entries in database
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDNumRows(){
    return mAdditionalTriggerIDNumRows;
};

/// Returns Run Number
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDRunNumber(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows())
	value = mAdditionalTriggerID[entry].runNumber;
    return value;
};

/// Returns Event Number
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDEventNumber(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows())
	value = mAdditionalTriggerID[entry].eventNumber;
    return value;
};



/// Returns idxTrg
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDIdxTrg(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows() )
	value = mAdditionalTriggerID[entry].idxTrg;
    return value;
};

/// Returns daqTrgId
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDDaqTrgId(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows() )
	value = mAdditionalTriggerID[entry].daqTrgId;
    return value;
};

/// Returns offlineTrgId
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDOfflineTrgId(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows() )
	value = mAdditionalTriggerID[entry].offlineTrgId;
    return value;
};

/// Returns trgNameVersion
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDTrgNameVersion(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows() )
	value = mAdditionalTriggerID[entry].trgNameVersion;
    return value;
};

/// Returns trgVersion
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDTrgVersion(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows() )
	value = mAdditionalTriggerID[entry].trgVersion;
    return value;
};

/// Returns threashVersion
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDThreashVersion(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows() )
	value = mAdditionalTriggerID[entry].threashVersion;
    return value;
};

/// Returns psVersion
unsigned int StDetectorDbTriggerID::getAdditionalTriggerIDPsVersion(unsigned int entry){
    unsigned int value = 0;
    if(mAdditionalTriggerID && entry < this->getAdditionalTriggerIDNumRows() )
	value = mAdditionalTriggerID[entry].psVersion;
    return value;
};


/// trigPrescales members
/// Returns Number of Entries in database
unsigned int StDetectorDbTriggerID::getSNumRows(){
    return mSNumRows;
};

/// Returns Run Number
int StDetectorDbTriggerID::getSRunNumber(){
    int value = 0;
    if(mTrigPrescales)
	value = mTrigPrescales[0].runNumber;
    return value;
};

/// Returns idxTrigger
int StDetectorDbTriggerID::getIdxTrigger(unsigned int entry){
    int value = 0;
    if(mTrigPrescales && entry < this->getSNumRows() )
	value = mTrigPrescales[entry].idxTrigger;
    return value;
};

/// Returns idxLevel
int StDetectorDbTriggerID::getIdxLevel(unsigned int entry){
    int value = 0;
    if(mTrigPrescales && entry < this->getSNumRows() )
	value = mTrigPrescales[entry].idxLevel;
    return value;
};

/// Returns id (algorithms)
int StDetectorDbTriggerID::getId(unsigned int entry){
    int value = 0;
    if(mTrigPrescales && entry < this->getSNumRows() )
	value = mTrigPrescales[entry].id;
    return value;
};


/// Returns ps 
float StDetectorDbTriggerID::getPs(unsigned int entry){
    float value = 0;
    if(mTrigPrescales && entry < this->getSNumRows() )
	value = mTrigPrescales[entry].ps;
    return value;
};

/// L0TriggerInfo members
/// Returns Number of Entries in database
unsigned int StDetectorDbTriggerID::getL0NumRows(){
    return mL0NumRows;
};

/// Returns L0 Run Number
int StDetectorDbTriggerID::getL0RunNumber(){
    int value = 0;
    if(mL0TriggerInfo)
	value = mL0TriggerInfo[0].runNumber;
    return value;
};

/// Returns daqTriggerId 
int StDetectorDbTriggerID::getL0DaqTrgId(unsigned int entry){
    int value = 0;
    if(mL0TriggerInfo && entry < this->getL0NumRows() )
	value = mL0TriggerInfo[entry].daqTriggerId;
    return value;
};

/// Returns offlineTriggerId 
int StDetectorDbTriggerID::getL0OfflineTrgId(unsigned int entry){
    int value = 0;
    if(mL0TriggerInfo && entry < this->getL0NumRows() )
	value = mL0TriggerInfo[entry].offlineTriggerId;
    return value;
};

/// Returns psL0 
int StDetectorDbTriggerID::getPsL0(unsigned int entry){
    int value = 0;
    if(mL0TriggerInfo && entry < this->getL0NumRows() )
	value = mL0TriggerInfo[entry].psL0;
    return value;
};

/// Returns name 
char* StDetectorDbTriggerID::getName(unsigned int entry){
    char* value = 0;
    if(mL0TriggerInfo && entry < this->getL0NumRows() )
	value = mL0TriggerInfo[entry].name;
    return value;
};

/// Returns detectorLiveOnBits 
unsigned int StDetectorDbTriggerID::getDetectorLiveOnBits(unsigned int entry){
    unsigned int value = 0;
    if(mL0TriggerInfo && entry < this->getL0NumRows() )
	value = mL0TriggerInfo[entry].detectorLiveOnBits;
    return value;
};

/// Returns detectorLiveOffBits 
unsigned int StDetectorDbTriggerID::getDetectorLiveOffBits(unsigned int entry){
    unsigned int value = 0;
    if(mL0TriggerInfo && entry < this->getL0NumRows() )
	value = mL0TriggerInfo[entry].detectorLiveOffBits;
    return value;
};

/// Returns detectorRequest 
unsigned int StDetectorDbTriggerID::getDetectorRequest(unsigned int entry){
    unsigned int value = 0;
    if(mL0TriggerInfo && entry < this->getL0NumRows() )
	value = mL0TriggerInfo[entry].detectorRequest;
    return value;
};

unsigned int StDetectorDbTriggerID::getDefaultTriggerLevel(){
    unsigned int value = 999;
    if(mDefaultTriggerLevel)
	value = mDefaultTriggerLevel->level;
    return value;
};

/// trigL3Expanded members
/// Returns Number of Entries in database
unsigned int StDetectorDbTriggerID::getTrigL3ExpandedNumRows(){
    return mTrigL3ExpandedNumRows;
};

/// Returns Run Number
int StDetectorDbTriggerID::getTrigL3ExpandedRunNumber(){
    int value = 0;
    if(mTrigL3Expanded)
	value = mTrigL3Expanded[0].runNumber;
    return value;
};

/// Returns l2TriggerResultType
char* StDetectorDbTriggerID::getTrigL3ExpandedL2TriggerResultType(unsigned int entry){
    char* value = 0;
    if(mTrigL3Expanded && entry < this->getTrigL3ExpandedNumRows() )
	value = mTrigL3Expanded[entry].l2TriggerResultType;
    return value;
};

/// Returns name 
char* StDetectorDbTriggerID::getTrigL3ExpandedName(unsigned int entry){
    char* value = 0;
    if(mTrigL3Expanded && entry < this->getTrigL3ExpandedNumRows() )
      value = (char *) mTrigL3Expanded[entry].name;
    return value;
};

/// Returns l3TrgId
int StDetectorDbTriggerID::getTrigL3ExpandedL3TrgId(unsigned int entry){
    int value = 0;
    if(mTrigL3Expanded && entry < this->getTrigL3ExpandedNumRows() )
	value = mTrigL3Expanded[entry].l3TrgId;
    return value;
};

/// Returns l3ExpandedTrgId 
int StDetectorDbTriggerID::getTrigL3ExpandedL3ExpandedTrgId(unsigned int entry){
    int value = 0;
    if(mTrigL3Expanded && entry < this->getTrigL3ExpandedNumRows() )
	value = mTrigL3Expanded[entry].l3ExpandedTrgId;
    return value;
};

/// Returns l2Algo 
int StDetectorDbTriggerID::getTrigL3ExpandedL2Algo(unsigned int entry){
    int value = 0;
    if(mTrigL3Expanded && entry < this->getTrigL3ExpandedNumRows() )
	value = mTrigL3Expanded[entry].l2Algo;
    return value;
};

/// Returns l2Ps 
float StDetectorDbTriggerID::getTrigL3ExpandedL2Ps(unsigned int entry){
    float value = 0;
    if(mTrigL3Expanded && entry < this->getTrigL3ExpandedNumRows() )
	value = mTrigL3Expanded[entry].l2Ps;
    return value;
};

/// dsmPrescale members
/// Returns Number of Entries in database
unsigned int StDetectorDbTriggerID::getDsmPrescalesNumRows(){
    return mDsmPrescalesNumRows;
};

/// Returns Run Number
int StDetectorDbTriggerID::getDsmPrescalesRunNumber(){
    int value = 0;
    if(mDsmPrescales)
	value = mDsmPrescales[0].runNumber;
    return value;
};

/// Returns trgId 
int StDetectorDbTriggerID::getDsmPrescalesTrgId(unsigned int entry){
    int value = 0;
    if(mDsmPrescales && entry < this->getDsmPrescalesNumRows() )
	value = mDsmPrescales[entry].trgId;
    return value;
};

/// Returns dsmPrescale 
int StDetectorDbTriggerID::getDsmPrescalesDsmPrescale(unsigned int entry){
    int value = 0;
    if(mDsmPrescales && entry < this->getDsmPrescalesNumRows() )
	value = mDsmPrescales[entry].dsmPrescale;
    return value;
};

map<int,float> StDetectorDbTriggerID::getTotalPrescales() 
{
    map<int,float> value;
    // First walk forward through the multiple levels of prescales
    for (unsigned int irow=0;irow<this->getDsmPrescalesNumRows(); ++irow) {
	int trgId = this->getDsmPrescalesTrgId(irow);
	value[trgId] = float(this->getDsmPrescalesDsmPrescale(irow));
    }

    for (unsigned int irow=0; irow<this->getL0NumRows(); ++irow) {
	int trgId = this->getL0OfflineTrgId(irow);
	map<int,float>::iterator p=value.find(trgId);
	if (p != value.end()) {
	    (*p).second *= float(getPsL0(irow));
	}
	else {
	    value[trgId] = float(getPsL0(irow));
	}
    }
    // For completeness: this one is always unity as far as I can tell
    for (unsigned int irow=0; irow<this->getSNumRows(); ++irow) {
	unsigned int idxTrigger = this->getIdxTrigger(irow);
	int trgId = 0;
	for (unsigned int jrow=0; jrow<this->getIDNumRows(); ++jrow) {
	    if (idxTrigger == this->getIdxTrg(jrow)) {
		trgId = this->getOfflineTrgId(jrow);
		break;
	    }
	}
	map<int,float>::iterator p=value.find(trgId);
	
	if (p != value.end()) {
	    (*p).second *= float(getPs(irow));
	}
	else {
	    value[trgId] = float(getPs(irow));
	}
    }
    
    // Now deal with L3Expanded
    for (unsigned int irow=0; irow<this->getTrigL3ExpandedNumRows(); ++irow) {
	int oldtid = this->getTrigL3ExpandedL3TrgId(irow);
	int newtid = this->getTrigL3ExpandedL3ExpandedTrgId(irow);
	float l2ps = this->getTrigL3ExpandedL2Ps(irow);
	
	map<int,float>::iterator p = value.find(oldtid);
	if (p!= value.end()) {
	    value[newtid] = ((*p).second)*l2ps;
	}
	else {
	    value[newtid] = l2ps;
	}
	
    }
    return value;
}

    

float StDetectorDbTriggerID::getTotalPrescaleByTrgId(int trgId) 
{
    map<int,float> theMap = this->getTotalPrescales();
    map<int,float>::const_iterator p = theMap.find(trgId);
    if (p != theMap.end()) {
	return (*p).second;
    }
    else {
	return 0;
    }
}




/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbTriggerID& v){
    os << endl << "Run shown in triggerID: " << v.getIDRunNumber() << endl;
    
    for(unsigned int i = 0; i < v.getIDNumRows(); i++){
	os << "idx_trg: " << v.getIdxTrg(i) 
	   << "  daqTrg: " << v.getDaqTrgId(i)  
	   << "  offlineTrgId: " << v.getOfflineTrgId(i)  
	   << "  trgNameVersion: " << v.getTrgNameVersion(i) << endl
	   << "  trgVersion: " << v.getTrgVersion(i)  
	   << "  threashVersion: " << v.getThreashVersion(i)  
	   << "  psVersion: " << v.getPsVersion(i) << endl;
    }
    os << endl << "Run shown in trigPreScales: " << v.getSRunNumber() << endl;
    
    for(unsigned int i = 0; i < v.getSNumRows(); i++){
	os << "idxTrigger: " << v.getIdxTrigger(i) 
	   << "  idxLevel: " << v.getIdxLevel(i)  
	   << "  id (algorithm) : " << v.getId(i)  
	   << "  prescale : " << v.getPs(i) << endl; 
    }
    
    os << endl<<  "Run shown in L0TriggerInfo: " << v.getL0RunNumber() << endl;

    for(unsigned int i = 0; i < v.getL0NumRows(); i++){
	os << "daqTriggerId: " << v.getL0DaqTrgId(i) 
	   << " OfflineTriggerId: " << v.getL0OfflineTrgId(i)   
	   << " psL0: " << v.getPsL0(i)  
	   << " name : " << v.getName(i) << endl 
	   << "detectorLiveOnBits: " << v.getDetectorLiveOnBits(i)  
	   << " detectorLiveOffBits: " << v.getDetectorLiveOffBits(i) 
	   << " detectorRequest: " << v.getDetectorRequest(i)  << endl; 
    }

    os << "Default Trigger Level: " << v.getDefaultTriggerLevel() << endl;

    os << endl << "Run shown in trigL3Expanded: " << v.getTrigL3ExpandedRunNumber() << endl;
    for (unsigned int i=0; i< v.getTrigL3ExpandedNumRows(); ++i) {
	os << "L2TriggerResultType: "<< v.getTrigL3ExpandedL2TriggerResultType(i) 
	   << " name: " << v.getTrigL3ExpandedName(i) 
	   << " L3TrgId: " << v.getTrigL3ExpandedL3TrgId(i) 
	   << " L3ExpandedTrgId: " << v.getTrigL3ExpandedL3ExpandedTrgId(i) 
	   << " L2Algo: " << v.getTrigL3ExpandedL2Algo(i) 
           << " L2Ps: " << v.getTrigL3ExpandedL2Ps(i) << endl;
    }
    
    os << endl << "Run shown in dsmPrescales: " << v.getDsmPrescalesRunNumber() << endl;
    for (unsigned int i=0; i<v.getDsmPrescalesNumRows(); ++i) {
	os << "trgId: " << v.getDsmPrescalesTrgId(i) 
	   << " dsmPrescale: " << v.getDsmPrescalesDsmPrescale(i) << endl;
    }

    os << endl << "Run shown in additionalTriggerID: " << v.getAdditionalTriggerIDRunNumber() << endl;
    for(unsigned int i = 0; i < v.getAdditionalTriggerIDNumRows(); i++){
	os << "eventNumber: " << v.getAdditionalTriggerIDEventNumber(i)
	    << "idx_trg: " << v.getAdditionalTriggerIDIdxTrg(i) 
	   << "  daqTrg: " << v.getAdditionalTriggerIDDaqTrgId(i)  
	   << "  offlineTrgId: " << v.getAdditionalTriggerIDOfflineTrgId(i)  
	   << "  trgNameVersion: " << v.getAdditionalTriggerIDTrgNameVersion(i) << endl
	   << "  trgVersion: " << v.getAdditionalTriggerIDTrgVersion(i)  
	   << "  threashVersion: " << v.getAdditionalTriggerIDThreashVersion(i)  
	   << "  psVersion: " << v.getAdditionalTriggerIDPsVersion(i) << endl;
    }
    
    return os;
};
