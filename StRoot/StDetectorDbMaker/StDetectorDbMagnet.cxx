#include "StMessMgr.h"
#include "StDetectorDbMagnet.h"
#include "tables/St_starMagOnl_Table.h"
#include <math.h>
/*!
   This class provites and interface to the magnet status. It can provide the nominal scale factor as well as the current.

*/

/// Initialize Instance
StDetectorDbMagnet* StDetectorDbMagnet::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbMagnet* StDetectorDbMagnet::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbMagnet();
    }
    // Need to reinitilize array for some unkown reason
    // the array memeory changed althoug the TTable stays the same
    if(sInstance->mTable)
	sInstance->mStarMagOnl = (starMagOnl_st*)(sInstance->mTable->GetArray());
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbMagnet::update(StMaker* maker){
    
    if(maker){
	// RDO in RunLog_onl
	
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("starMagOnl"));
	    
	    if(mTable){
		mNumRows = mTable->GetNRows();
		mStarMagOnl = (starMagOnl_st*)(mTable->GetArray());
	    }
	}
    }
};

/// Default constructor
StDetectorDbMagnet::StDetectorDbMagnet(){
    ~(*gMessMgr) << "StDetectorDbMagnet::StDetectorDbMagnet" << endm;
    mStarMagOnl = 0;
    mNumRows = 0;
    mTable = 0;
};


/// Convert Current into scale factor
double StDetectorDbMagnet::currentToScaleFactor(double current){

    double value = -9999;

    if(current < -4450 && current > -4550)
	value = -1.0;
    else if(current < -2200 && current > -2300)
	value = -0.5;
    else if(current > -50 && current < 50)
	value = 0.0;
    else if(current > 2200 && current < 2300)
	value = 0.5;
    else if(current > 4450 && current < 4550)
	value = 1.0;

    return value;
};

/// Returns Number of Entries in database
unsigned int StDetectorDbMagnet::getNumRows(){
    return mNumRows;
};


/// Returns Magnetic Current Entry in database
/// 0 returns first entry, 1 returns second..etc
/// returns -9999 if out of bounds
double StDetectorDbMagnet::getMagnetCurrentEntry(unsigned int entry){
    double value = -9999;
    
    if(mStarMagOnl && entry < mNumRows )
	value = mStarMagOnl[entry].current;
    
    return value;
};

/// Returns time Entry in database
/// 0 returns first entry, 1 returns second..etc
/// returns 0 if out of bounds
unsigned int StDetectorDbMagnet::getTimeEntry(unsigned int entry){
    unsigned int value = 0;
    
    if(mStarMagOnl && entry < mNumRows )
	value = mStarMagOnl[entry].time;
    
    return value;
};

double StDetectorDbMagnet::getMagnetCurrent(unsigned int time){

    double tempCurrent = -9999;
    
    // If only one row, value is stable
    // Or if want at begning of run
        
    if(mNumRows == 1 || time == 0){
	tempCurrent = this->getMagnetCurrentEntry(0);
    }
    else{
	for(unsigned int i = 0; i < this->getNumRows() - 1 ; i++){
	    if( time >= getTimeEntry(i) && time <= getTimeEntry(i+1) ){
		if( fabs(this->getMagnetCurrentEntry(i) - this->getMagnetCurrentEntry(i+1)) < 50 )
		    tempCurrent = this->getMagnetCurrentEntry(i);
	    }
	}
    }
    
    return tempCurrent;
};

/// Returns the Magnetic Field at current time
/// If no argument Magnetic Field at start of run
StMagnetPolarity StDetectorDbMagnet::getMagneticField(unsigned int time){
    StMagnetPolarity value = eUnknownMField;
    
    double scaleFactor = this->getScaleFactor(time);

    if(scaleFactor == 1.0)
	value = eFullMFieldPolA;
    if(scaleFactor == 0.5)
	value = eHalfMFieldPolA;
    if(scaleFactor == 0.0)
	value = eZeroMField;
    if(scaleFactor == -0.5)
	value = eHalfMFieldPolB;
    if(scaleFactor == -1.0)
	value = eFullMFieldPolB;

    return value;
};

/// Returns the scale factor at current time
/// -9999 is the error value
double StDetectorDbMagnet::getScaleFactor(unsigned int time){
    double tempCurrent = this->getMagnetCurrent(time);
    return this->currentToScaleFactor(tempCurrent);
};

/// Returns Run Number
unsigned int StDetectorDbMagnet::getRunNumber(){
    unsigned int value = 0;
    if(mStarMagOnl)
	value = mStarMagOnl[0].runNumber;
    return value;
};

/// Default destructor
StDetectorDbMagnet::~StDetectorDbMagnet(){
  delete sInstance;
  sInstance = 0;
};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbMagnet& v){

    os << "Run: " << v.getRunNumber() << endl;
    os << "Current: " << v.getMagnetCurrent() << endl;

    double scaleFactor = v.getScaleFactor();
    
    os << "Scale Factor: " << scaleFactor << endl; 
    
    if(scaleFactor == 1.0)
	os << "Full Field - Polarity A (Positive)";
    else if(scaleFactor == 0.5)
	os << "Half Field - Polarity A (Positive)";
    else if(scaleFactor == 0.0)
	os << "Zero Field";
    else if(scaleFactor == -0.5)
	os << "Half Field - Polarity B (Negative)";
    else if(scaleFactor == -1.0)
	os << "Full Field - Polarity B (Negative)";
    else
	os << "Unknown Field";

    if( v.getNumRows() != 1 ){
	os << "Magetic Field Changes Drastically Durring Run" << endl;
	for(unsigned int i = 0; i < v.getNumRows(); i++){
	    os << "Time: " << v.getTimeEntry(i) << "  Current: " << v.getMagnetCurrentEntry(i) << endl;
	}
    }
    return os;
};
