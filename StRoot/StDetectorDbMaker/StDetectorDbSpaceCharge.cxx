#include <iostream.h>
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

/// Returns previous instance if exits, if not makes new one
StDetectorDbSpaceCharge* StDetectorDbSpaceCharge::instance()
{
    
    if(!sInstance){
	sInstance = new StDetectorDbSpaceCharge();
    }
    // Need to reinitilize array for some unkown reason
    // the array memeory changed althoug the TTable stays the same
    if(sInstance->mTable)
	sInstance->mSpaceCharge = (spaceChargeCor_st*)(sInstance->mTable->GetArray());
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbSpaceCharge::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
		
	TDataSet* dataSet = maker->GetDataBase("Calibrations/rich");

	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("spaceChargeCor"));
	    
	    if(mTable){
		mSpaceCharge = (spaceChargeCor_st*)(mTable->GetArray());
	    }
	}
    }
};

/// Default constructor
StDetectorDbSpaceCharge::StDetectorDbSpaceCharge(){
    cout << "StDetectorDbSpaceCharge::StDetectorDbSpaceCharge" << endl;
    mSpaceCharge = 0;
    mTable = 0;
};
/// Default destructor
StDetectorDbSpaceCharge::~StDetectorDbSpaceCharge(){
  delete sInstance;
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
    double mult = scalers->getMult();
    double saturation = this->getSpaceChargeSatRate();
    double correction = this->getSpaceChargeCorrection(scaleFactor);

    double intens = (mult < saturation) ? mult : saturation;
    return intens * correction;
    
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
    
    return os;
};
