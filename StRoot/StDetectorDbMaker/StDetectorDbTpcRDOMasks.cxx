#include "StMessMgr.h"
#include "StDetectorDbTpcRDOMasks.h"
#include "tables/St_tpcRDOMasks_Table.h"

/*! 
In database, TPC Sector 1 and 2 are packed into Sector 1 with higher 6 bits belonding to sector 2, lower order to sector 1. Sector 3 and 4 in sector 2, etc. This class pulls the sector, rdo enable out in human readable Sector (1-24) rdo 1-6

It is a singleton which requires manual updating, usually taken care of in StDetectorDbMaker.cxx::Make(). If no data exists all values return 0. To use:

  #include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
  StDetectorDbTpcRDOMasks * masks = StDetectorDbTpcRDOMasks::intstance();
  ~gMess << *masks << endm;
  ~gMess << masks->isOn(sector,rdo) << endm;
 
*/

/// Initialize Instance
StDetectorDbTpcRDOMasks* StDetectorDbTpcRDOMasks::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbTpcRDOMasks* StDetectorDbTpcRDOMasks::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbTpcRDOMasks();
    }

    // get new address of struck eack instance
    // data somehow gets garbles between events
    if(sInstance->mTable){ 
	sInstance->mNumEntries = sInstance->mTable->GetNRows() ;
	if(sInstance->mNumEntries == 12){
	    sInstance->mMaskVector = (tpcRDOMasks_st*)(sInstance->mTable->GetArray());
	}
	else{
	    sInstance->mNumEntries = 0;
	}
    }
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbTpcRDOMasks::update(StMaker* maker){
    
    if(maker){
	// RDO in RunLog_onl
	
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("tpcRDOMasks"));
	    
	    if(mTable){
		mNumEntries = mTable->GetNRows() ;
		if(mNumEntries == 12){
		    mMaskVector = (tpcRDOMasks_st*)(mTable->GetArray());
		}
		else{
		    mNumEntries = 0;
		}
	    }
	}
	
	if( dataSet == 0 || mTable == 0 || mNumEntries == 0)
	    ~gMess << "StDetectorDbTpcRDOMasks: No RDO Table For This Run";
    }
    else
	~gMess << "StDetectorDbTpcRDOMasks: No RDO Table For This Run";
    
};

/// Default constructor
StDetectorDbTpcRDOMasks::StDetectorDbTpcRDOMasks(){
    ~gMess << "StDetectorDbTpcRDOMasks::StDetectorDbTpcRDOMasks" << endm;
    mNumEntries = 0;
    mMaskVector = 0;
    mTable = 0;
};
	
/// Default destructor,
StDetectorDbTpcRDOMasks::~StDetectorDbTpcRDOMasks(){
  delete sInstance;
  sInstance = 0;
};

/// boolen that returns status of sector and rdo
/// 1 is on, 0 for off
bool StDetectorDbTpcRDOMasks::isOn(unsigned int sector,unsigned int rdo){

    if(sector < 1 || sector > 24 || rdo < 1 || rdo > 6)
	return 0;
    
    unsigned int mask = getSectorMask(sector);
    
    mask = mask >> (rdo - 1);
    mask &= 0x00000001;
    return mask;
};

/// Gets sectors mask. Returns in 2 Hex Digits for 6 RDO per mask
/// higher order bit is higher mask
unsigned int StDetectorDbTpcRDOMasks::getSectorMask(unsigned int sector){

    unsigned int mask = 0x0000; // default is to mask it out
    //unsigned int mask = 0xFFFF; // change to  ON by default ** THIS WAS A HACK
    
    if(sector < 1 || sector > 24 || mNumEntries == 0){
      ~gMess << "StDetectorDbTpcRDOMasks::getSectorMask : return default mask for " 
	   << "sector= " << sector << " mNumEntries=" << mNumEntries << endm;
      return mask;
    }
    
    mask = mMaskVector[ ((sector + 1) / 2) - 1].mask; // does the mapping from sector 1-24 to packed sectors

    if( sector % 2 == 0){ // if its even relevent bits are 6-11
	mask = mask >> 6;
    }
                        // Otherwise want lower 6 bits    
    mask &= 0x000003F; // Mask out higher order bits
    return mask;
};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbTpcRDOMasks& v){
    
    os << "Sector" << "\t" << "Enable Mask (HEX)" << endl;
    
    for(unsigned int i=1; i <= 24 ; i++){
	// Hex and Dec Operators. Make sure to put back to dec after
	os << "Sector " << dec << i << "\t" << hex << v.getSectorMask(i) << dec << endl;
    }

    return os;
    
};


