#include "StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbInterpolator.h"
#include "TDataSet.h"
#include "TTable.h"
#include "StMaker.h"
#include "TUnixTime.h"

/* Needs a StMaker* passed in. This is because GetDatabase only works in a maker class. So this must be declared inside a maker.
In database, TPC Sector 1 and 2 are packed into Sector 1 with higher 6 bits belonding to sector 2, lower order to sector 1. Sector 3 and 4 in sector 2, etc. This class pulls the sector, rdo enable out in human readable Sector (1-24) rdo 1-6
*/
StDetectorDbTpcRDOMasks::StDetectorDbTpcRDOMasks(StMaker* maker){
    cout << "StDetectorDbTpcRDOMasks::StDetectorDbTpcRDOMasks" << endl;

    mNumEntries = 0;
    mRunNumber = 0;
    mSectors = 0;
    mMasks = 0;
    
    if(maker){

	// Time conversions to avoid root automatically converting timezones
	TDatime rootTime = maker->GetDateTime();
	TUnixTime unixTime;
	unixTime.SetGTime(rootTime.GetDate(),rootTime.GetTime());
			
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    TTable* table = dynamic_cast<TTable*>(dataSet->Find("tpcRDOMasks"));
	    if(table){
		mNumEntries = table->GetNRows() ;
		if(mNumEntries == 12){
		    tpcRDOMasks_st* value = (tpcRDOMasks_st*)(table->GetArray());
		    
		    mRunNumber = value[0].runNumber;
		    mSectors = new unsigned int[mNumEntries];
		    mMasks = new unsigned int[mNumEntries];
		    
		    for(unsigned int i = 0;i < mNumEntries;i++){
			mSectors[i] = value[i].sector;
			mMasks[i] = value[i].mask;
		    }
		}
	    }
	}
    }
};
    
/// Delete all the arrays
StDetectorDbTpcRDOMasks::~StDetectorDbTpcRDOMasks(){
    
    delete mSectors;
    delete mMasks;
};

/// sets the run number
void StDetectorDbTpcRDOMasks::setRunNumber(unsigned int value){ mRunNumber = value;};

/// sets the number of entries in the class (must be done because using static arrays
void StDetectorDbTpcRDOMasks::setNumEntries(unsigned int value){ mNumEntries = value;};

/// sets the sector list array.
void StDetectorDbTpcRDOMasks::setSectors(unsigned int* value){ mSectors = value;};

/// sets the mask that corresponds to the sector.
void StDetectorDbTpcRDOMasks::setMasks(unsigned int* value){ mMasks = value;};

/// gets the run number of the data
unsigned int StDetectorDbTpcRDOMasks::getRunNumber(){ return mRunNumber;};

/// gets the number of different status corresponding to an event
unsigned int StDetectorDbTpcRDOMasks::getNumEntries(){ return mNumEntries;};

/// one can get the event timestamp from StEvent::time() along with other methods
bool StDetectorDbTpcRDOMasks::isOn(unsigned int sector,unsigned int rdo){

    if(sector < 1 || sector > 24 || rdo < 1 || rdo > 6)
	return 0;
    
    unsigned int mask = getSectorMask(sector);
    
    mask = mask >> (rdo - 1);
    mask &= 0x00000001;
    return mask;
};

unsigned int StDetectorDbTpcRDOMasks::getSectorMask(unsigned int sector){

    unsigned int mask = 0xDEAD; // DEAD in Hex (all masks should only be 12 bit anyways)
    
    if(sector < 1 || sector > 24 || mNumEntries == 0)
	return mask;

    
    mask = mMasks[ ((sector + 1) / 2) - 1]; // does the mapping from sector 1-24 to packed sectors

    if( sector % 2 == 0){ // if its even relevent bits are 6-11
	mask = mask >> 6;
    }
                        // Otherwise want lower 6 bits    
    mask &= 0x000003F; // Mask out higher order bits
    return mask;
};

/// prints all the datamembers to standard out
ostream& operator<<(ostream& os, StDetectorDbTpcRDOMasks& v){
    
    os << "Run " << v.mRunNumber << endl;

    os << "Sector" << "\t" << "Enable Mask (HEX)" << endl;
    
    for(unsigned int i=1; i <= 24 ; i++){
	// Hex and Dec Operators. Make sure to put back to dec after
	os << "Sector " << dec << i << "\t" << hex << v.getSectorMask(i) << dec << endl;
    }

    return os;
    
};


