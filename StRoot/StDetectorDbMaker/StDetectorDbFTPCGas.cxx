#include "StDetectorDbFTPCGas.h"
#include "tables/St_ftpcGasOut_Table.h"
#include "tables/St_ftpcGasSystem_Table.h"
#include "tables/St_tpcGas_Table.h"

/* !
  The class holds the FTPC Gas system database values needed in the chain. It is a singleton which requires manual updating, usually taken care of in StDetectorDbMaker.cxx::Make(). If no data exists all values return 0. To use:
  #include "StDetectorDbMaker/StDetectorDbFTPCGas.h"
  StDetectorFTPCGas * gas = StDetectorDbFTPCGas::instance();
  cout << *gas << endl;
  gas->getWestO2ppm();

  or any other access methods.

  Do not use the update function unless you know what you are doing. It should not cause a problem, it will just access the database more often than needed.
*/

/// Initilizes singleton pointer
StDetectorDbFTPCGas* StDetectorDbFTPCGas::sInstance = 0;


/// returns singleton pointer if it exits, makes new one if needed
/// this is the default way to invoke an instance
StDetectorDbFTPCGas* StDetectorDbFTPCGas::instance()
{

    if(!sInstance){
	sInstance = new StDetectorDbFTPCGas();
    }

    return sInstance;
};


/// Updates data from database. Only use if know what you are doing
void StDetectorDbFTPCGas::update(StMaker* maker){

    if(maker){
		
	TDataSet* dataSet = maker->GetDataBase("Calibrations/ftpc");
	if(dataSet){
	    TTable* gasSystemTable = dynamic_cast<TTable*>(dataSet->Find("ftpcGasSystem"));
	    TTable* gasOutTable    = dynamic_cast<TTable*>(dataSet->Find("ftpcGasOut"));
	    
	    if(gasSystemTable)
		mGasSystem = (ftpcGasSystem_st*)(gasSystemTable->GetArray());
	
	    if(gasOutTable)
		mGasOut = (ftpcGasOut_st*)(gasOutTable->GetArray());
	}
	
	dataSet = 0;
	dataSet = maker->GetDataBase("Calibrations/tpc");
	
	if(dataSet){
	
	    TTable* tpcGasTable = dynamic_cast<TTable*>(dataSet->Find("tpcGas"));
	
	    if(tpcGasTable)
		mTpcGas = (tpcGas_st*)(tpcGasTable->GetArray());
	}
    }
};

/// Default constructor
StDetectorDbFTPCGas::StDetectorDbFTPCGas(){
    cout << "StDetectorDbFTPCGas::StDetectorDbFTPCGas" << endl;
    mGasSystem = 0;
    mGasOut = 0;
};

/// Default destructor 
StDetectorDbFTPCGas::~StDetectorDbFTPCGas(){
  delete sInstance;
  sInstance = 0;
};

/// West Oxygen ppm
double StDetectorDbFTPCGas::getWestO2ppm(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->westO2ppm;
    return value;
};

/// West Oxygen mv
double StDetectorDbFTPCGas::getWestO2mv(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->westO2mv;
    return value;
};

/// East Oxygen ppm
double StDetectorDbFTPCGas::getEastO2ppm(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->eastO2ppm;
    return value;
};


///  West Oxygen mv
double StDetectorDbFTPCGas::getEastO2mv(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->eastO2mv;
    return value;
};

/// Ext Oxygen ppm
double StDetectorDbFTPCGas::getExtO2ppm(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->extO2ppm;
    return value;
};

/// Ext Oxygen mv
double StDetectorDbFTPCGas::getExtO2mv(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->extO2mv;
    return value;
};

/// West H20 dew point
double StDetectorDbFTPCGas::getWestH2Odp(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->westH2Odp;
    return value;
};

/// East H20 dew point
double StDetectorDbFTPCGas::getEastH2Odp(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->eastH2Odp;
    return value;
};

/// Argon Flow
double StDetectorDbFTPCGas::getFlowAr(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->flowAr;
    return value;
};

/// CO2 Flow
double StDetectorDbFTPCGas::getFlowCO2(){
    double value = 0;
    if(mGasSystem)
	value = mGasSystem->flowCO2;
    return value;
};

/// Gasout East (ElementID = 1)
double StDetectorDbFTPCGas::getGasOutEast(){
    double value = 0;
    if(mGasOut)
	value = mGasOut->gasOutEast;
    return value;
};

/// Gasout West (ElementID=2)
double StDetectorDbFTPCGas::getGasOutWest(){
    double value = 0;
    if(mGasOut)
	value = mGasOut->gasOutWest;
    return value;
};

/// Barometric Presure
double StDetectorDbFTPCGas::getBarometricPressure(){
    double value = 0;
    if(mTpcGas)
	value = mTpcGas->barometricPressure;
    return value;
};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbFTPCGas& v){

    os << "West O2 PPM = " << v.getWestO2ppm() << endl;
    os << "West O2 mV  = " << v.getWestO2mv() << endl;
    os << "East O2 PPM = " << v.getEastO2ppm() << endl;
    os << "East O2 mV  = " << v.getEastO2mv() << endl;
    os << "Ext O2 PPM  = " << v.getExtO2ppm() << endl;
    os << "Ext O2 mV   = " << v.getExtO2mv() << endl;
    os << "West GasOut = " << v.getGasOutWest() << endl;
    os << "East GasOut = " << v.getGasOutEast() << endl;
    os << "BarometricPressure = " << v.getBarometricPressure() << endl;
    
    return os;
    
};
