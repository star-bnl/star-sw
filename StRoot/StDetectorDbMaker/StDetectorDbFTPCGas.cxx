#include "StDetectorDbFTPCGas.h"
#include "StDetectorDbInterpolator.h"
#include "TDataSet.h"
#include "TTable.h"
#include "StMaker.h"
#include "TUnixTime.h"

/// Needs a StMaker* passed in. This is because GetDatabase only works in a maker class. So this must be declared inside a maker.
StDetectorDbFTPCGas::StDetectorDbFTPCGas(StMaker* maker){
    cout << "StDetectorDbFTPCGas::StDetectorDbFTPCGas" << endl;
    mRunNumber = 0;
    mNumEntries = 0;
    mTimes = 0;
    mEventTime = 0;
    mWestO2ppm = 0;
    mWestO2mv = 0;
    mEastO2ppm = 0;
    mEastO2mv = 0;
    mExtO2ppm = 0;
    mExtO2mv = 0;
    mWestH2Odp = 0;
    mEastH2Odp = 0;
    mFlowAr = 0;
    mFlowCO2 = 0;

    if(maker){
	// Time conversions to avoid root automatically converting timezones
	TDatime rootTime = maker->GetDateTime();
	TUnixTime unixTime;
	unixTime.SetGTime(rootTime.GetDate(),rootTime.GetTime());
	mEventTime = unixTime.GetUTime();
	
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    TTable* table = dynamic_cast<TTable*>(dataSet->Find("ftpcGasSystem"));
	    if(table){
		mNumEntries = table->GetNRows() ;
		if(mNumEntries){
		    ftpcGasSystem_st* zfit = (ftpcGasSystem_st*)(table->GetArray());
		    
		    mRunNumber = zfit[0].runNumber;
		    mTimes = new unsigned int[mNumEntries];
		    mWestO2ppm = new double[mNumEntries];
		    mWestO2mv = new double[mNumEntries];
		    mEastO2ppm = new double[mNumEntries];
		    mEastO2mv = new double[mNumEntries];
		    mExtO2ppm = new double[mNumEntries];
		    mExtO2mv = new double[mNumEntries];
		    mWestH2Odp = new double[mNumEntries];
		    mEastH2Odp = new double[mNumEntries];
		    mFlowAr = new double[mNumEntries];
		    mFlowCO2 = new double[mNumEntries];
		    
		    
		    for(unsigned int i = 0;i < mNumEntries;i++){
			mTimes[i] = zfit[i].timeOffset;
			mWestO2ppm[i] = zfit[i].westO2ppm;
			mWestO2mv[i] = zfit[i].westO2mv;
			mEastO2ppm[i] = zfit[i].eastO2ppm;
			mEastO2mv[i] = zfit[i].eastO2mv;
			mExtO2ppm[i] = zfit[i].extO2ppm;
			mExtO2mv[i] = zfit[i].extO2mv;
			mWestH2Odp[i] = zfit[i].westH2Odp;
			mEastH2Odp[i] = zfit[i].eastH2Odp;
			mFlowAr[i] = zfit[i].flowAr;
			mFlowCO2[i] = zfit[i].flowCO2;
		    }
		}
	    }
	}
    }
};

/// Delete all the arrays
StDetectorDbFTPCGas::~StDetectorDbFTPCGas(){
    
    delete mTimes;
    delete mWestO2ppm;
    delete mWestO2mv;
    delete mEastO2ppm;
    delete mEastO2mv;
    delete mExtO2ppm;
    delete mExtO2mv;
    delete mWestH2Odp;
    delete mEastH2Odp;
    delete mFlowAr;
    delete mFlowCO2;

};

/// sets the event time
void StDetectorDbFTPCGas::setEventTime(unsigned int value){ mEventTime = value;};

/// sets the run number
void StDetectorDbFTPCGas::setRunNumber(unsigned int value){ mRunNumber = value;};

/// sets the number of entries in the class (must be done because using static arrays
void StDetectorDbFTPCGas::setNumEntries(unsigned int value){ mNumEntries = value;};

/// Fills the time arrays
void StDetectorDbFTPCGas::setTimes(unsigned int* value){ mTimes = value;};

/// Fills the West Oxygen ppm array
void StDetectorDbFTPCGas::setWestO2ppm(double* value) { mWestO2ppm = value;};

/// Fills the West Oxygen mV array
void StDetectorDbFTPCGas::setWestO2mv(double* value) { mWestO2mv = value;};

/// Fills the East Oxygen ppm array
void StDetectorDbFTPCGas::setEastO2ppm(double* value) { mEastO2ppm = value;};

/// Fills the East Oxygen mV array
void StDetectorDbFTPCGas::setEastO2mv(double* value) { mEastO2mv = value;};

/// Fills the Ext Oxygen ppm array
void StDetectorDbFTPCGas::setExtO2ppm(double* value) { mExtO2ppm = value;};

/// Fills the Ext Oxygen mv array
void StDetectorDbFTPCGas::setExtO2mv(double* value) { mExtO2mv = value;};

/// Fills the West H2O Dew Point array
void StDetectorDbFTPCGas::setWestH2Odp(double* value) { mWestH2Odp = value;};

/// Fills the East H2O Dew Point array
void StDetectorDbFTPCGas::setEastH2Odp(double* value) { mEastH2Odp = value;};

/// Fills the Argon Flow array
void StDetectorDbFTPCGas::setFlowAr(double* value) { mFlowAr = value;};

/// Fills the CO2 Flow array
void StDetectorDbFTPCGas::setFlowCO2(double* value) { mFlowCO2 = value;};

/// Gets the Run Number
unsigned int StDetectorDbFTPCGas::getRunNumber(){ return mRunNumber;};

/// Gets the Event Time
unsigned int StDetectorDbFTPCGas::getEventTime(){ return mEventTime;};

/// Gets the number of entries
unsigned int StDetectorDbFTPCGas::getNumEntries(){ return mNumEntries;};


/// Interpolates the value of a given array bassed on the timestamp
double StDetectorDbFTPCGas::interpolateValue(unsigned int time,double* array){
    if(time ==0)
	time = mEventTime;
    
    StDetectorDbInterpolator<double> inter(this->getNumEntries(),mTimes,array);
    double value = inter.interpolate(time);
    return value;
};

/// Interpolates West Oxygen ppm to time
double StDetectorDbFTPCGas::getWestO2ppm(unsigned int time){return interpolateValue(time,mWestO2ppm);};

/// Interpolates West Oxygen mv to time
double StDetectorDbFTPCGas::getWestO2mv(unsigned int time){return interpolateValue(time,mWestO2mv);};

/// Interpolates East Oxygen ppm to time
double StDetectorDbFTPCGas::getEastO2ppm(unsigned int time){return interpolateValue(time,mEastO2ppm);};

/// Interpolates West Oxygen mv to time
double StDetectorDbFTPCGas::getEastO2mv(unsigned int time){return interpolateValue(time,mEastO2mv);};

/// Interpolates Ext Oxygen ppm to time
double StDetectorDbFTPCGas::getExtO2ppm(unsigned int time){return interpolateValue(time,mExtO2ppm);};

/// Interpolates Ext Oxygen mv to time
double StDetectorDbFTPCGas::getExtO2mv(unsigned int time){return interpolateValue(time,mExtO2mv);};

/// Interpolates West H20 dew point
double StDetectorDbFTPCGas::getWestH2Odp(unsigned int time){return interpolateValue(time,mWestH2Odp);};

/// Interpolates East H20 dew point
double StDetectorDbFTPCGas::getEastH2Odp(unsigned int time){return interpolateValue(time,mEastH2Odp);};

/// Interpolates the Argon Flow
double StDetectorDbFTPCGas::getFlowAr(unsigned int time){return interpolateValue(time,mFlowAr);};

/// Interpolates the CO2 Flow
double StDetectorDbFTPCGas::getFlowCO2(unsigned int time){return interpolateValue(time,mFlowCO2);};



/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbFTPCGas& v){
    
    os << "Run " << v.mRunNumber << "\t Number of Rows " << v.mNumEntries << endl;
    os << "Time" << "\t" << "West ppm" << "\t" << "West mv" << "\t" << "East PPM" << "\t" << "East mV"
       << "\t" << "Ext ppm" << "\t" << "Ext mv" << "\t" << "wH2O dp" << "\t"<< "wH2O dp" << "\t" << "Flow Ar" << "\t" << "Flow CO2" << endl;
    
    for(unsigned int i=0; i < v.getNumEntries() ; i++){
	os << v.mTimes[i] <<"\t" << v.mWestO2ppm[i] <<"\t" << v.mWestO2mv[i] << "\t"
	   << v.mEastO2ppm[i] <<"\t" << v.mEastO2mv[i]
	   <<"\t" << v.mExtO2ppm[i] <<"\t" << v.mExtO2mv[i]
	   <<"\t" << v.mWestH2Odp[i] <<"\t" << v.mEastH2Odp[i] << "\t" << v.mFlowAr[i] << "\t"
	   << v.mFlowCO2[i] << endl;
    }

    return os;
    
};
