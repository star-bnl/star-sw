#include "StMessMgr.h"
#include "StDetectorDbTpcOmegaTau.h"
#include "tables/St_tpcOmegaTau_Table.h"
#include "StMaker.h"

/*!

 This class provides an interface to get the TPC OmegaTau tensor components

*/

/// Initialize Instance
StDetectorDbTpcOmegaTau* StDetectorDbTpcOmegaTau::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbTpcOmegaTau* StDetectorDbTpcOmegaTau::instance()
{
    
    if(!sInstance){
	sInstance = new StDetectorDbTpcOmegaTau();
    }
    // Need to reinitialize array for some unknown reason
    // the array memory changed although the TTable stays the same
    if(sInstance->mTable)
	sInstance->mOmegaTau = (tpcOmegaTau_st*)(sInstance->mTable->GetArray());
    
    return sInstance;
}

/// Updates data in instance from database
void StDetectorDbTpcOmegaTau::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
		
	TDataSet* dataSet = maker->GetDataBase("Calibrations/tpc");

	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("tpcOmegaTau"));

	    if(mTable){
		mOmegaTau = (tpcOmegaTau_st*)(mTable->GetArray());
	    }
	}
    }
}

/// Default constructor
StDetectorDbTpcOmegaTau::StDetectorDbTpcOmegaTau(){
    ~(*gMessMgr) << "StDetectorDbTpcOmegaTau::StDetectorDbTpcOmegaTau" << endm;
    mOmegaTau = 0;
    mTable = 0;
}

/// Default destructor
StDetectorDbTpcOmegaTau::~StDetectorDbTpcOmegaTau(){
  //delete sInstance; //unnecessary, deleting it if here!
  sInstance = 0;
}

/// Returns OmegaTau tensor V2 component
float StDetectorDbTpcOmegaTau::getOmegaTauTensorV2(){
  if(mOmegaTau) return mOmegaTau->tensorV2;
  return 0;
}

/// Returns OmegaTau tensor V1 component
float StDetectorDbTpcOmegaTau::getOmegaTauTensorV1(){
  if(mOmegaTau) return mOmegaTau->tensorV1;
  return 0;
}

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbTpcOmegaTau& v){

    os << "TPC OmegaTau Tensor Components" << endl;
    os << "Tensor V1: " << v.getOmegaTauTensorV1() << endl;
    os << "Tensor V2: " << v.getOmegaTauTensorV2() << endl;
    
    return os;
}


