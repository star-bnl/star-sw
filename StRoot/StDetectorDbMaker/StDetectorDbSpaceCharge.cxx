#include "StMessMgr.h"
#include "StDetectorDbSpaceCharge.h"
#include "tables/St_spaceChargeCor_Table.h"
#include "TUnixTime.h"
#include "StDetectorDbMagnet.h"
#include "StDetectorDbRichScalers.h"

/*!

 This class provides and interface to get the space charge correction.

*/

/// Initialize Instance
StDetectorDbSpaceCharge* StDetectorDbSpaceCharge::sInstance = 0;
StDetectorDbSpaceChargeR2* StDetectorDbSpaceChargeR2::sInstanceR2 = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbSpaceCharge* StDetectorDbSpaceCharge::instance()
{
    
    if(!sInstance){
	sInstance = new StDetectorDbSpaceCharge();
    }
    // Need to reinitialize array for some unknown reason
    // the array memory changed although the TTable stays the same
    if(sInstance->mTable)
	sInstance->mSpaceCharge = (spaceChargeCor_st*)(sInstance->mTable->GetArray());
    
    return sInstance;
};
StDetectorDbSpaceCharge* StDetectorDbSpaceCharge::instanceR2() {
  return StDetectorDbSpaceChargeR2::instance();
}

/// Updates data in instance from database
void StDetectorDbSpaceCharge::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
		
	TDataSet* dataSet = maker->GetDataBase("Calibrations/rich");

	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find(tableName));
	    
	    if(mTable){
		mSpaceCharge = (spaceChargeCor_st*)(mTable->GetArray());
	    }
	}
    }
};

/// Default constructor
StDetectorDbSpaceCharge::StDetectorDbSpaceCharge(){
    ~gMess << "StDetectorDbSpaceCharge::StDetectorDbSpaceCharge" << endm;
    mSpaceCharge = 0;
    mTable = 0;
    sprintf(tableName,"spaceChargeCor");
};
/// Default destructor
StDetectorDbSpaceCharge::~StDetectorDbSpaceCharge(){
  //delete sInstance; //unnecessary, deleting it if here!
  sInstance = 0;
};

/// Returns Space Charge Correction for given field scale factor
double StDetectorDbSpaceCharge::getSpaceChargeCorrection(double scaleFactor){
    double value = 0;
    if(mSpaceCharge){

	if(scaleFactor < -.75 && scaleFactor > -1.25)
	    value = mSpaceCharge->fullFieldB;
	else if(scaleFactor < -0.25)
	    value = mSpaceCharge->halfFieldB;
	else if(scaleFactor < .25)
	    value = mSpaceCharge->zeroField;
	else if(scaleFactor < 0.75)
	    value = mSpaceCharge->halfFieldA;
	else if(scaleFactor < 1.25)
	    value = mSpaceCharge->fullFieldA;
    }
	
    return value;
};

/// Returns Space Charge Correction for current field
double StDetectorDbSpaceCharge::getSpaceChargeCorrection(){

    StDetectorDbMagnet * magnet = StDetectorDbMagnet::instance();
    return getSpaceChargeCorrection(magnet->getScaleFactor());

};

/// Returns Space Charge Correction for given
double StDetectorDbSpaceCharge::getSpaceChargeCoulombs(double scaleFactor){

    StDetectorDbRichScalers* scalers = StDetectorDbRichScalers::instance();
    double mult;
    switch ((int) this->getSpaceChargeDetector()) {
	case (0) : mult = scalers->getMult(); break;
	case (1) : mult = scalers->getBBCX(); break;
	case (2) : mult = scalers->getZDCX(); break;
	case (3) : mult = scalers->getZDCEast()+scalers->getZDCWest(); break;
	default  : mult = 0.;
    }
    double saturation = this->getSpaceChargeSatRate();
    double correction = this->getSpaceChargeCorrection(scaleFactor);
    double factor     = this->getSpaceChargeFactor();

    double intens = (mult < saturation) ? mult : saturation;
    return factor * intens * correction;
    
};

/// Returns Space Charge Correction for given
double StDetectorDbSpaceCharge::getSpaceChargeCoulombs(){
    StDetectorDbMagnet * magnet = StDetectorDbMagnet::instance();
    return getSpaceChargeCoulombs(magnet->getScaleFactor());
};

/// Returns Space Charge Saturation Rate
double StDetectorDbSpaceCharge::getSpaceChargeSatRate(){
    double value = 0;
    if(mSpaceCharge){
	value = mSpaceCharge->satRate;
    }
    return value;
    
};

/// Returns multiplicative shape scale factor to use for Space Charge
float StDetectorDbSpaceCharge::getSpaceChargeFactor(){
    float value = 0;
    if(mSpaceCharge){
	value = mSpaceCharge->factor;
    }
    return value;
    
};

/// Returns Detector to use for Space Charge measure
float StDetectorDbSpaceCharge::getSpaceChargeDetector(){
    float value = 0;
    if(mSpaceCharge){
	value = mSpaceCharge->detector;
    }
    return value;
    
};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbSpaceCharge& v){

    os << "Space Charge Corrections" << endl;
    os << "Negative Full Field: " << v.getSpaceChargeCorrection(-1.0) << endl;
    os << "Negative Half Field: " << v.getSpaceChargeCorrection(-0.5) << endl;
    os << "Zero Field:          " << v.getSpaceChargeCorrection(0.0) << endl;
    os << "Positive Half Field: " << v.getSpaceChargeCorrection(0.5) << endl;
    os << "Positive Full Field: " << v.getSpaceChargeCorrection(1.0) << endl;
    os << "Currerently Using:   " << v.getSpaceChargeCorrection() << endl;
    os << endl;
    os << "Saturation Rate:     " << v.getSpaceChargeSatRate() << endl;
    os << "Coulombs:            " << v.getSpaceChargeCoulombs() << endl;
    os << "Shape Scale Factor:  " << v.getSpaceChargeFactor() << endl;
    os << "Detector:            " << v.getSpaceChargeDetector() << endl;
    
    return os;
};


/// Returns previous instance if exits, if not makes new one
StDetectorDbSpaceCharge* StDetectorDbSpaceChargeR2::instance()
{
    
    if(!sInstanceR2){
	sInstanceR2 = new StDetectorDbSpaceChargeR2();
    }
    // Need to reinitialize array for some unknown reason
    // the array memory changed although the TTable stays the same
    if(sInstanceR2->mTable)
	sInstanceR2->mSpaceCharge = (spaceChargeCor_st*)(sInstanceR2->mTable->GetArray());
    
    return (StDetectorDbSpaceCharge*) sInstanceR2;
};

/// Default constructor
StDetectorDbSpaceChargeR2::StDetectorDbSpaceChargeR2() :
  StDetectorDbSpaceCharge() {
    ~gMess << "StDetectorDbSpaceCharge:: ...in R2 mode" << endm;
    sprintf(tableName,"spaceChargeCorR2");
}
/// Default destructor
StDetectorDbSpaceChargeR2::~StDetectorDbSpaceChargeR2(){
  //delete sInstanceR2; //unnecessary, deleting it if here!
  sInstanceR2 = 0;
};
