#include <Stiostream.h>
#include "StDetectorDbRichScalers.h"
#include "tables/St_trigDetSums_Table.h"
#include "tables/St_richvoltages_Table.h"
#include "tables/St_y1Mult_Table.h"

/*!
  The class holds the Rich database values needed in the chain. It is a singleton which requires manual updating, usually taken care of in StDetectorDbMaker.cxx::Make(). If no data exists all values return 0. To use:

  #include "StDetectorDbMaker/StDetectorDbRichScalers.h"
  StDetectorRichScalers * scalers = StDetectorRichScalers::intstance();
  cout << *scalers << endl;
  cout << scalers->getZDCWest() << endl;
  cout << scalers->getRichHVStatus() << endl;
  
  or any other access methods.
  
  note: the getMult() will return either the year 1 or year 2 value, depending on timestamp

  Do not use the update function unless you know what you are doing. It should not cause a problem, it will just access the database more often than needed.
*/

/// Initilizes singleton pointer
StDetectorDbRichScalers* StDetectorDbRichScalers::sInstance = 0;

/// returns singleton pointer if it exits, makes new one if needed
/// this is the default way to invoke an instance
StDetectorDbRichScalers* StDetectorDbRichScalers::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbRichScalers();
    }

    return sInstance;
};

/// Default constructor
StDetectorDbRichScalers::StDetectorDbRichScalers(){
    cout << "StDetectorDbRichScalers::StDetectorDbRichScalers" << endl;
    mScalers = 0;
    mVolts = 0;
    mY1Mults = 0;
};

/// Updates data from database. Only use if know what you are doing
void StDetectorDbRichScalers::update(StMaker* maker){
        
    if(maker){
		
	TDataSet* dataSet = maker->GetDataBase("Calibrations/rich");

	if(dataSet){
	    TTable* scalerTable = dynamic_cast<TTable*>(dataSet->Find("trigDetSums"));
	    TTable* voltTable   = dynamic_cast<TTable*>(dataSet->Find("richvoltages"));
	
	    // Get Scaler Table
	    if(scalerTable){
		mScalers = (trigDetSums_st*)(scalerTable->GetArray());
	    }
	    // Get Voltage State Table
	    if(voltTable){
		mVolts = (richvoltages_st*)(voltTable->GetArray());
	    }
	    // If niether exit, may be year 1 data, so get y1Mult table
	    if(!scalerTable && !voltTable){
		TTable* y1multTable   = dynamic_cast<TTable*>(dataSet->Find("y1Mult"));
		if(y1multTable){
		    mY1Mults = (y1Mult_st*)(y1multTable->GetArray());
		}
	    }
	}
    }
    
};

/// Default destructor
StDetectorDbRichScalers::~StDetectorDbRichScalers(){
  delete sInstance;
  sInstance = 0;
};

/// CTB West
double StDetectorDbRichScalers::getCTBWest(){
    double value = 0;
    if(mScalers)
	value = mScalers->ctbWest;
    return value;
};

/// CTB East
double StDetectorDbRichScalers::getCTBEast(){
    double value = 0;
    if(mScalers)
	value = mScalers->ctbEast;
    return value;
};

/// CTBWest + CTBEast + TOFp
double StDetectorDbRichScalers::getCTBOrTOFp(){
    double value = 0;
    if(mScalers)
	value = mScalers->ctbTOFp;
    return value;
};

/// TOFp
double StDetectorDbRichScalers::getTOFp(){
    double value = 0;
    if(mScalers)
	value = mScalers->tofp;
    return value;
};

/// ZDC West
double StDetectorDbRichScalers::getZDCWest(){
    double value = 0;
    if(mScalers)
	value = mScalers->zdcWest;
    return value;
};

/// ZDC East
double StDetectorDbRichScalers::getZDCEast(){
    double value = 0;
    if(mScalers)
	value = mScalers->zdcEast;
    return value;
};

/// ZDC East * ZDC West
double StDetectorDbRichScalers::getZDCX(){
    double value = 0;
    if(mScalers)
	value = mScalers->zdcX;
    return value;
};

/// Mult (year 1 or year 2)
double StDetectorDbRichScalers::getMult(){
    double value = 0;
    if(mScalers)
	value = mScalers->mult;
    else if(mY1Mults)
	value = mY1Mults->mult;
    
    return value;
};

/// LO
double StDetectorDbRichScalers::getL0(){
    double value = 0;
    if(mScalers)
	value = mScalers->L0;
    return value;
};

/// BBC East * BBC West
double StDetectorDbRichScalers::getBBCX(){
    double value = 0;
    if(mScalers)
	value = mScalers->bbcX;
    return value;
};

/// (BBC East * BBC West) * (CTB West + CTB East + TOFp)
double StDetectorDbRichScalers::getBBCXCTB(){
    double value = 0;
    if(mScalers)
	value = mScalers->bbcXctbTOFp;
    return value;
};

/// BBC West
double StDetectorDbRichScalers::getBBCWest(){
    double value = 0;
    if(mScalers)
	value = mScalers->bbcWest;
    return value;
};

/// BBC East
double StDetectorDbRichScalers::getBBCEast(){
    double value = 0;
    if(mScalers)
	value = mScalers->bbcEast;
    return value;
};

/// BBC Yellow Background 
double StDetectorDbRichScalers::getBBCYellowBkg(){
    double value = 0;
    if(mScalers)
	value = mScalers->bbcYellowBkg;
    return value;
};

/// BBC Blue Background 
double StDetectorDbRichScalers::getBBCBlueBkg(){
    double value = 0;
    if(mScalers)
	value = mScalers->bbcBlueBkg;
    return value;
};

/// PVPD West
double StDetectorDbRichScalers::getPVPDWest(){
    double value = 0;
    if(mScalers)
	value = mScalers->pvpdWest;
    return value;
};

/// PVPD East
double StDetectorDbRichScalers::getPVPDEast(){
    double value = 0;
    if(mScalers)
	value = mScalers->pvpdEast;
    return value;
};

/// Rich High Voltage status (1 is good, 0 is bad)
unsigned int StDetectorDbRichScalers::getRichHVStatus(){
    unsigned int value = 999;
    if(mVolts)
	value = mVolts->status;
    return value;
};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbRichScalers& v){
    
    os << "CTB East        = " << v.getCTBEast() << endl;
    os << "CTB West        = " << v.getCTBWest() << endl;
    os << "CTB Or + TOFp   = " << v.getCTBOrTOFp() << endl;
    os << "TOFp            = " << v.getTOFp() << endl;
    os << "ZDC East        = " << v.getZDCEast() << endl;
    os << "ZDC West        = " << v.getZDCWest() << endl;
    os << "ZDC And         = " << v.getZDCX() << endl;
    os << "Mult            = " << v.getMult() << endl;
    os << "L0              = " << v.getL0() << endl;
    os << "BBC And         = " << v.getBBCX() << endl;
    os << "BBC*(CTB+TOFp)  = " << v.getBBCXCTB() << endl;
    os << "BBC West        = " << v.getBBCWest() << endl;
    os << "BBC East        = " << v.getBBCEast() << endl;
    os << "BBC Yellow Bkg  = " << v.getBBCYellowBkg() << endl;
    os << "BBC Blue   Bkg  = " << v.getBBCBlueBkg() << endl;
    os << "pVPD West       = " << v.getPVPDWest() << endl;
    os << "pVPD East       = " << v.getPVPDEast() << endl;
    
    os << "Rich HV  = " << v.getRichHVStatus() << endl;
        
    return os;
};
