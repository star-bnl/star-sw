#include "StDetectorDbRichScalers.h"
#include "StDetectorDbInterpolator.h"
#include "TDataSet.h"
#include "TTable.h"
#include "StMaker.h"
#include "TUnixTime.h"

/// Needs a StMaker* passed in. This is because GetDatabase only works in a maker class. So this must be declared inside a maker.
StDetectorDbRichScalers::StDetectorDbRichScalers(StMaker* maker){
    cout << "StDetectorDbRichScalers::StDetectorDbRichScalers" << endl;
    mRunNumber = 0;
    mEventTime = 0;
    mNumEntries = 0;
    mTimes = 0;
    mCTBWest = 0;
    mCTBEast = 0;
    mCTBOR = 0;
    mTOFp = 0;
    mZDCWest = 0;
    mZDCEast = 0;
    mZDCX = 0;
    mMult = 0;
    mL0 = 0;

    if(maker){
	// Time conversions to avoid root automatically converting timezones
	TDatime rootTime = maker->GetDateTime();
	TUnixTime unixTime;
	unixTime.SetGTime(rootTime.GetDate(),rootTime.GetTime());
	mEventTime = unixTime.GetUTime();
	
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");
	
	if(dataSet){
	    TTable* table = dynamic_cast<TTable*>(dataSet->Find("trigDetSums"));
	    if(table){
		mNumEntries = table->GetNRows() ;
		if(mNumEntries){
		    trigDetSums_st* zfit = (trigDetSums_st*)(table->GetArray());
		    
		    mRunNumber = zfit[0].runNumber;
		    mTimes = new unsigned int[mNumEntries];
		    mCTBWest = new double[mNumEntries];
		    mCTBEast = new double[mNumEntries];
		    mCTBOR = new double[mNumEntries];
		    mTOFp = new double[mNumEntries];
		    mZDCWest = new double[mNumEntries];
		    mZDCEast = new double[mNumEntries];
		    mZDCX = new double[mNumEntries];
		    mMult = new double[mNumEntries];
		    mL0 = new double[mNumEntries]; 
		    
		    for(unsigned int i = 0;i < mNumEntries;i++){
			mTimes[i] = zfit[i].timeOffset;
			mCTBWest[i] = zfit[i].ctbWest;
			mCTBEast[i] = zfit[i].ctbEast;
			mCTBOR[i] = zfit[i].ctbOR;
			mTOFp[i] = zfit[i].ctbTOFp;
			mZDCWest[i] = zfit[i].zdcWest;
			mZDCEast[i] = zfit[i].zdcEast;
			mZDCX[i] = zfit[i].zdcX;
			mMult[i] = zfit[i].mult;
			mL0[i] = zfit[i].L0;
		    }
		}
	    }
	}
    }
};

/// Delete all the arrays
StDetectorDbRichScalers::~StDetectorDbRichScalers(){
    
    delete mTimes;
    delete mCTBWest;
    delete mCTBEast;
    delete mCTBOR;
    delete mTOFp;
    delete mZDCWest;
    delete mZDCEast;
    delete mZDCX;
    delete mMult;
    delete mL0;

};

/// sets the event time
void StDetectorDbRichScalers::setEventTime(unsigned int value){ mEventTime = value;};

/// sets the run number
void StDetectorDbRichScalers::setRunNumber(unsigned int value){ mRunNumber = value;};

/// sets the number of entries in the class (must be done because using static arrays
void StDetectorDbRichScalers::setNumEntries(unsigned int value){ mNumEntries = value;};

/// Fills the time arrays
void StDetectorDbRichScalers::setTimes(unsigned int* value){ mTimes = value;};

/// Fills the CTB West scaler array
void StDetectorDbRichScalers::setCTBWest(double* value){mCTBWest = value;};

/// Fills the CTB East scaler array
void StDetectorDbRichScalers::setCTBEast(double* value){mCTBEast = value;};

/// Fills the CTB Or scaler array
void StDetectorDbRichScalers::setCTBOr(double* value){mCTBOR = value;};

/// Fills the TOFp scaler array
void StDetectorDbRichScalers::setTOFp(double* value){mTOFp = value;};

/// Fills the ZDC West scaler array
void StDetectorDbRichScalers::setZDCWest(double* value){mZDCWest = value;};

/// Fills the ZDC East scaler array
void StDetectorDbRichScalers::setZDCEast(double* value){mZDCEast = value;};

/// Fills the ZDC AND scaler array
void StDetectorDbRichScalers::setZDCX(double* value){mZDCX = value;};

/// Fills the Mult scaler array
void StDetectorDbRichScalers::setMult(double* value){mMult = value;};

/// Fills the L0 scaler array
void StDetectorDbRichScalers::setL0(double* value){mL0= value;};

/// Gets the Run Number
unsigned int StDetectorDbRichScalers::getRunNumber(){ return mRunNumber;};

/// Gets the Event Time
unsigned int StDetectorDbRichScalers::getEventTime(){ return mEventTime;};

/// Gets the number of entries
unsigned int StDetectorDbRichScalers::getNumEntries(){ return mNumEntries;};

/// Interpolates the value of a given array bassed on the timestamp
double StDetectorDbRichScalers::interpolateValue(unsigned int time,double* array){
    if(time ==0)
	time = mEventTime;
    
    StDetectorDbInterpolator<double> inter(this->getNumEntries(),mTimes,array);
    double value = inter.interpolate(time);
    return value;
};
/// Interpolates the CTB West value by given timestamp
double StDetectorDbRichScalers::getCTBWest(unsigned int time){return interpolateValue(time,mCTBWest);};

/// Interpolates the CTB East value by given timestamp
double StDetectorDbRichScalers::getCTBEast(unsigned int time){return interpolateValue(time,mCTBEast);};

/// Interpolates the CTB Or value by given timestamp
double StDetectorDbRichScalers::getCTBOr(unsigned int time){return interpolateValue(time,mCTBOR);};

/// Interpolates the TOFp Or value by given timestamp
double StDetectorDbRichScalers::getTOFp(unsigned int time){return interpolateValue(time,mTOFp);};

/// Interpolates the ZDC West value by given timestamp
double StDetectorDbRichScalers::getZDCWest(unsigned int time){return interpolateValue(time,mZDCWest);};

/// Interpolates the ZDC East value by given timestamp
double StDetectorDbRichScalers::getZDCEast(unsigned int time){return interpolateValue(time,mZDCEast);};

/// Interpolates the ZDC AND value by given timestamp
double StDetectorDbRichScalers::getZDCX(unsigned int time){return interpolateValue(time,mZDCX);};

/// Interpolates the Mult value by given timestamp
double StDetectorDbRichScalers::getMult(unsigned int time){return interpolateValue(time,mMult);};

/// Interpolates the LO value by given timestamp
double StDetectorDbRichScalers::getL0(unsigned int time){return interpolateValue(time,mL0);};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbRichScalers& v){
    
    os << "Run " << v.mRunNumber << "\t Number of Rows " << v.mNumEntries << endl;
    os << "Time" << "\t" << "        CTBW" << "\t" << "CTBE" << "\t" << "CTBOR" << "\t" << "TOFp"
       << "\t" << "ZDCW" << "\t" << "ZDCE" << "\t" << "ZDCX" << "\t"<< "MULT" << "\t" << "L0" << endl;
    
    for(unsigned int i=0; i < v.getNumEntries() ; i++){
	os << v.mTimes[i] <<"\t" << v.mCTBWest[i] <<"\t" << v.mCTBEast[i] << "\t"
	   << v.mCTBOR[i] <<"\t" << v.mTOFp[i]
	   <<"\t" << v.mZDCWest[i] <<"\t" << v.mZDCEast[i]
	   <<"\t" << v.mZDCX[i] <<"\t" << v.mMult[i] << "\t" << v.mL0[i]
	   << endl;
    }

    return os;
    
};
