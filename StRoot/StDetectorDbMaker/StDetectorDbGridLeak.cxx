#include "StMessMgr.h"
#include "StDetectorDbGridLeak.h"
#include "tables/St_tpcGridLeak_Table.h"
#include "StMaker.h"

/*!

 This class provides an interface to get the GridLeak correction.

*/

/// Initialize Instance
StDetectorDbGridLeak* StDetectorDbGridLeak::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbGridLeak* StDetectorDbGridLeak::instance()
{
    
    if(!sInstance){
	sInstance = new StDetectorDbGridLeak();
    }
    // Need to reinitialize array for some unknown reason
    // the array memory changed although the TTable stays the same
    if(sInstance->mTable)
	sInstance->mGridLeak = (tpcGridLeak_st*)(sInstance->mTable->GetArray());
    
    return sInstance;
}

/// Updates data in instance from database
void StDetectorDbGridLeak::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
		
	TDataSet* dataSet = maker->GetDataBase("Calibrations/tpc");

	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("tpcGridLeak"));
	    
	    if(mTable){
		mGridLeak = (tpcGridLeak_st*)(mTable->GetArray());
	    }
	}
    }
}

/// Default constructor
StDetectorDbGridLeak::StDetectorDbGridLeak(){
    ~(*gMessMgr) << "StDetectorDbGridLeak::StDetectorDbGridLeak" << endm;
    mGridLeak = 0;
    mTable = 0;
}

/// Default destructor
StDetectorDbGridLeak::~StDetectorDbGridLeak(){
  //delete sInstance; //unnecessary, deleting it if here!
  sInstance = 0;
}

/// Returns GridLeak strength (relative to SpaceCharge) for given position
double StDetectorDbGridLeak::getGridLeakStrength(StGLpos pos){
  if(mGridLeak){
    switch (pos) {
      case (kGLinner) : return mGridLeak->InnerGLStrength;
      case (kGLmiddl) : return mGridLeak->MiddlGLStrength;
      case (kGLouter) : return mGridLeak->OuterGLStrength;
    }
  }
  return 0;
}

/// Returns GridLeak radius for given position
double StDetectorDbGridLeak::getGridLeakRadius(StGLpos pos){
  if(mGridLeak){
    switch (pos) {
      case (kGLinner) : return mGridLeak->InnerGLRadius;
      case (kGLmiddl) : return mGridLeak->MiddlGLRadius;
      case (kGLouter) : return mGridLeak->OuterGLRadius;
    }
  }
  return 0;
}

/// Returns GridLeak width for given position
double StDetectorDbGridLeak::getGridLeakWidth(StGLpos pos){
  if(mGridLeak){
    switch (pos) {
      case (kGLinner) : return mGridLeak->InnerGLWidth;
      case (kGLmiddl) : return mGridLeak->MiddlGLWidth;
      case (kGLouter) : return mGridLeak->OuterGLWidth;
    }
  }
  return 0;
}


/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbGridLeak& v){

    os << "GridLeak Corrections" << endl;
    os << "Inner  Strength: " << v.getGridLeakStrength(kGLinner) << endl;
    os << "Middle Strength: " << v.getGridLeakStrength(kGLmiddl) << endl;
    os << "Outer  Strength: " << v.getGridLeakStrength(kGLouter) << endl;
    os << "Inner  Radius:   " << v.getGridLeakRadius(kGLinner)   << endl;
    os << "Middle Radius:   " << v.getGridLeakRadius(kGLmiddl)   << endl;
    os << "Outer  Radius:   " << v.getGridLeakRadius(kGLouter)   << endl;
    os << "Inner  Width:    " << v.getGridLeakWidth(kGLinner)    << endl;
    os << "Middle Width:    " << v.getGridLeakWidth(kGLmiddl)    << endl;
    os << "Outer  Width:    " << v.getGridLeakWidth(kGLouter)    << endl;
    
    return os;
}


