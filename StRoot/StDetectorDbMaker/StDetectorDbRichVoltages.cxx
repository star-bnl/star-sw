#include "StDetectorDbRichVoltages.h"
#include "StDetectorDbInterpolator.h"
#include "TDataSet.h"
#include "TTable.h"
#include "StMaker.h"

/// Needs a StMaker* passed in. This is because GetDatabase only works in a maker class. So this must be declared inside a maker.
StDetectorDbRichVoltages::StDetectorDbRichVoltages(StMaker* maker){
    cout << "StDetectorDbRichVoltages::StDetectorDbRichVoltages" << endl;

    mNumEntries = 0;
    mRunNumber = 0;
    mStatusTimes = 0;
    mStatus = 0;
    mEventTime = 0;

    if(maker){
	// For some reason, the maker GetDateTime is not in GMT!!!!
	mEventTime = maker->GetDateTime().Convert() - 14400;
		
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    TTable* table = dynamic_cast<TTable*>(dataSet->Find("richvoltages"));
	    if(table){
		mNumEntries = table->GetNRows() ;
		if(mNumEntries){
		    richvoltages_st* value = (richvoltages_st*)(table->GetArray());
		    
		    mRunNumber = value[0].runNumber;
		    mStatusTimes = new unsigned int[mNumEntries+1];
		    mStatus = new unsigned int[mNumEntries];
		    
		    for(unsigned int i = 0;i < mNumEntries;i++){
			mStatusTimes[i] = value[i].startStatusTime;
			mStatusTimes[i+1] = value[i].endStatusTime;
			mStatus[i] = value[i].status;
		    }
		}
	    }
	}
    }
};
    
    /// Delete all the arrays
StDetectorDbRichVoltages::~StDetectorDbRichVoltages(){
    
    delete mStatusTimes;
    delete mStatus;
};

/// sets the Event time
void StDetectorDbRichVoltages::setEventTime(unsigned int value){ mEventTime = value;};

/// sets the run number
void StDetectorDbRichVoltages::setRunNumber(unsigned int value){ mRunNumber = value;};

/// sets the number of entries in the class (must be done because using static arrays
void StDetectorDbRichVoltages::setNumEntries(unsigned int value){ mNumEntries = value;};

/// sets the status time. This is a list of values when the status of the detector changes.
void StDetectorDbRichVoltages::setStatusTimes(unsigned int* value){ mStatusTimes = value;};

/// sets the status that corresponds to the statusTimes. each value is valid from the
/// corresponding element in statusTimes untill the next element in statusTime where
void StDetectorDbRichVoltages::setStatus(unsigned int* value){ mStatus = value;};

/// gets the Eventtime
unsigned int StDetectorDbRichVoltages::getEventTime(){ return mEventTime;};

/// gets the runnumber of the data
unsigned int StDetectorDbRichVoltages::getRunNumber(){ return mRunNumber;};

/// gets the number of different status corresponding to an event
unsigned int StDetectorDbRichVoltages::getNumEntries(){ return mNumEntries;};

/// gets the status of the event from the passed in timestamp
/// one can get the event timestamp from StEvent::time() along with other methods
unsigned int StDetectorDbRichVoltages::getStatus(unsigned int time){

    if(time==0)
	time = this->getEventTime();
    
    StDetectorDbInterpolator<unsigned int> inter(this->getNumEntries(),mStatusTimes,mStatus);
    return inter.getLowerValue(time);
};

/// prints all the datamembers to standard out
ostream& operator<<(ostream& os, StDetectorDbRichVoltages& v){
    
    os << "Run " << v.mRunNumber << "\t Number of Rows " << v.mNumEntries << endl;

    os << "Status" << "\t" << "BeginTime" << "\t" << "EndTime" << endl;
    
    for(unsigned int i=0; i < v.getNumEntries() ; i++){
	os << v.mStatus[i] << "\t" << v.mStatusTimes[i] << "\t" << v.mStatusTimes[i+1] << endl;
    }

    return os;
    
};


