#include "StMessMgr.h"
#include "StDetectorDbClock.h"
#include "tables/St_starClockOnl_Table.h"
#include "TUnixTime.h"

/*!
   This class provides an interface to get RHIC clock rate. It will also determine if the clock changes drastically durring a run.

*/

/// Initialize Instance
StDetectorDbClock* StDetectorDbClock::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbClock* StDetectorDbClock::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbClock();
    }
    // Need to reinitilize array for some unkown reason
    // the array memeory changed althoug the TTable stays the same
    if(sInstance->mTable)
	sInstance->mStarClockOnl = (starClockOnl_st*)(sInstance->mTable->GetArray());
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbClock::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
	// RDO in RunLog_onl
	
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("starClockOnl"));
	    
	    if(mTable){
		mNumRows = mTable->GetNRows();
		mStarClockOnl = (starClockOnl_st*)(mTable->GetArray());
	    }
	}
    }
};

/// Default constructor
StDetectorDbClock::StDetectorDbClock(){
    ~(*gMessMgr) << "StDetectorDbClock::StDetectorDbClock" << endm;
    mStarClockOnl = 0;
    mNumRows = 0;
    mTable = 0;
    mMaker = 0;
};

/// Returns Number of Entries in database
unsigned int StDetectorDbClock::getNumRows(){
    return mNumRows;
};

/// Returns Run Number
unsigned int StDetectorDbClock::getRunNumber(){
    unsigned int value = 0;
    if(mStarClockOnl)
	value = mStarClockOnl[0].runNumber;
    return value;
};

/// Default destructor
StDetectorDbClock::~StDetectorDbClock(){
  delete sInstance;
  sInstance = 0;
};

/// Returns Frequency at the begining of the run
double StDetectorDbClock::getFrequencyEntry(unsigned int entry){
    double value = 0;
    if(mStarClockOnl && entry < this->getNumRows() )
	value = mStarClockOnl[entry].frequency;
    return value;
};

/// Gets the nth time entry in the database
unsigned int StDetectorDbClock::getTimeEntry(unsigned int entry){
    unsigned int value = 0;
    if(mStarClockOnl && entry < this->getNumRows() )
	value = mStarClockOnl[entry].time;
    return value;
};

/// gets the nth frequency entry in the database
double StDetectorDbClock::getFrequency(unsigned int time){
    double tempFrequency = -9999 ;
    
    if(mNumRows == 1 || time == 0){
	tempFrequency = this->getFrequencyEntry(0);
    }
    else{
	for(unsigned int i = 0; i < this->getNumRows() - 1 ; i++){
	    if( time >= getTimeEntry(i) && time <= getTimeEntry(i+1) - 10 ){
		tempFrequency = this->getFrequencyEntry(i);
	    }
	}
    }
    
    return tempFrequency;
    
};

/// Gets the current frequency (figures out correct event time automatically)
double StDetectorDbClock::getCurrentFrequency(){

    double value = -9999;
    
    if(mMaker){
	TDatime rootTime = mMaker->GetDateTime(); // Gets time from maker 
	TUnixTime unixTime;                       // Handles conversion to UnixTime
	unixTime.SetGTime(rootTime.GetDate(),rootTime.GetTime());
	unsigned int gmtTime = unixTime.GetUTime();
	value = this->getFrequency(gmtTime);
    }
    return value;
};


/// returns 1 if current frequency is the same +/- 25,000 Hz as the first frequency
/// 0 if not
bool StDetectorDbClock::getStatus(unsigned int time){

    double currentFrequency = this->getFrequency(time);
    double firstFrequency = this->getFrequencyEntry(0);

    return (currentFrequency > firstFrequency - 25000 && currentFrequency < firstFrequency + 2500);
        
};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbClock& v){
    os << "Run: " << v.getRunNumber() << endl;
    os << "Starting RHIC Clock Frequency: " << v.getFrequency() << endl;

    if( v.getNumRows() > 1 ){
	os << "RHIC Clock Frequency Changes Drastically Durring Run" << endl;
	for(unsigned int i = 0; i < v.getNumRows(); i++){
	    os << "Time: " << v.getTimeEntry(i) << "  Frequency: " << v.getFrequencyEntry(i) << endl;
	}
    }
    return os;
};
