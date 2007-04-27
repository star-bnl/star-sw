/**********************************************************
 * $Id: StRichMaterialsDb.cxx,v 2.7 2007/04/27 12:03:27 hippolyt Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMaterialsDb.cxx,v $
 *  Revision 2.7  2007/04/27 12:03:27  hippolyt
 *  Star logger recommendations
 *
 *  Revision 2.6  2001/04/10 16:56:06  lasiuk
 *  Change parameters to bring into line with richgeo.g and CERN.
 *
 *  Revision 2.5  2001/01/30 16:38:43  horsley
 *  updated PID maker for next production run, included new class for TTree
 *
 *  Revision 2.4  2000/12/08 04:58:20  lasiuk
 *  allow for index of refraction for liquid less than 170nm
 *
 *  Revision 2.3  2000/11/30 23:31:32  lasiuk
 *  default setting of mInner and mOuter in c'tor
 *
 *  Revision 2.2  2000/10/02 23:06:33  horsley
 *  *** empty log message ***
 *
 *  Revision 2.1  2000/09/29 01:35:36  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
  **********************************************************/

#include "StRichMaterialsDb.h"
#include "StMessMgr.h"
#include "StGlobals.hh"
#include "SystemOfUnits.h"


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif


StRichMaterialsDb* StRichMaterialsDb::p2Db = 0;
double StRichMaterialsDb::mHc = 1239.84270217*eV*nanometer;

StRichMaterialsDb::StRichMaterialsDb() {

    this->my_fill();

}

void StRichMaterialsDb::my_fill() {

    
    mVersion = 2.0;
 
    mLongestWavelength  = 220.0*nanometer;    // --> inner ring
    mMeanWavelength     = 177.4*nanometer;    // --> mode frequency  
    mShortestWavelength = 160.0*nanometer;    // --> outer ring
 
    //
    // default...can be reset by macro
    //
    mOuterWave = mShortestWavelength;
    mInnerWave = mLongestWavelength;

    mMeanRadiatorDepth = 0.5; // normalized to unit pathlength in radiator
  
    // goes from long to short wavelength
    ////////// index of refraction  ////////////////////////////
    const int N = 26;
    double energy[N] = {5.5, 5.6, 5.7, 5.8, 5.9,
			6.0, 6.1, 6.2, 6.3, 6.4,
			6.5, 6.6, 6.7, 6.8, 6.9,
			7.0, 7.1, 7.2, 7.3, 7.4, 
			7.5, 7.6, 7.7, 7.8, 7.9,
			8.0};

    double quartzN[N] = {1.52398, 1.52738, 1.5309, 1.5346, 1.53835,
			 1.542302, 1.54641, 1.55067, 1.55513, 1.55976,
			 1.56458, 1.56962, 1.57488, 1.58037, 1.58611,
			 1.59212, 1.59842, 1.60503, 1.61197, 1.61927,
			 1.62696, 1.63506, 1.64362, 1.65269, 1.662295,
			 1.6725};

    double quartzL[N] = {105.80, 65.520, 48.580, 42.850, 35.790,
			 31.262, 28.598, 27.527, 25.007, 22.815,
			 21.040, 19.266, 17.525, 15.878, 14.177,
			 11.719, 9.282,   6.620,  4.0925, 2.601,
			 1.149,  0.667,   0.3627, 0.192,  0.1497,
			 0.10857};
    
    double freonN[N] = {1.2716, 1.27332, 1.27504, 1.27676, 1.27848, 
			1.2802, 1.28192, 1.28364, 1.28536, 1.28708,
			1.2888, 1.29052, 1.29224, 1.29396, 1.29568,
			1.2974, 1.29912, 1.30084, 1.30256, 1.30428, 
			1.306,  1.30772, 1.30944, 1.31116, 1.31288, 
			1.3146};
    
    double freonL[N] = {176.4219, 176.4219, 176.4219, 176.4219, 176.4219,
			176.4219, 176.4219, 176.4219, 176.4219, 176.4219,
			176.4219, 176.4219, 176.4219, 176.4219,  58.322,
			41.6371,   22.5158,  10.9767,   4.01237,  1.65053,
			0.783613,   0.452046, 0.312553, 0.15627,  0.0,
			0.0};

    double csiQE[N] = {0.0002,   0.0006, 0.0007, 0.005,   0.0075,
		       0.010125, 0.0243, 0.0405, 0.06885, 0.1053,
		       0.1215,   0.1435, 0.16,   0.164,   0.1681,
		       0.17,     0.1785, 0.1811, 0.1836,  0.187,
		       0.1904,   0.1921, 0.1938, 0.1955, 0.2125,
		       0.221}; 


    for(int ii=0; ii<N; ii++) {
	mEnergy.push_back(energy[ii]*eV);
	mFreonN.push_back(freonN[ii]);
	mFreonL.push_back(freonL[ii]*centimeter);
	mQuartzN.push_back(quartzN[ii]);
	mQuartzL.push_back(quartzL[ii]*centimeter);
	mCsIQE.push_back(csiQE[ii]);
    }

    mBinSize = ( (mEnergy.back() - mEnergy.front())/(mEnergy.size()-1));
    PR(mBinSize/eV);
    
    mMethaneIndexOfRefraction  = 1.000444;
    mMethaneAbsCoeff = 1000000.0 * centimeter;
    
}

StRichMaterialsDb* StRichMaterialsDb::getDb() {

    if(!p2Db)
	p2Db = new StRichMaterialsDb();

    return p2Db;
}


void StRichMaterialsDb::setWavelengthRange(double shortwave, double longwave) {
  mInnerWave = longwave;
  mOuterWave = shortwave;

  { LOG_INFO << "StRichMaterialsDb::setWavelenghtRange() ---> using wavelengths " <<  mInnerWave/nanometer  << "  nm     and " << mOuterWave/nanometer << "nm " << endm; }
}

bool StRichMaterialsDb::boundsCheck(double index) const {
  
    bool status = true;
    if ( (index>mEnergy.size()-1) || index<0) {
      { LOG_ERROR << "StRichMaterialsDb::boundsCheck() %n WARNING %n index = " << index << endm; }
      status = false;
    }
    
    return status;
}

double StRichMaterialsDb::convertToEnergy(double lambda) const {

    return (mHc/lambda);
}

double StRichMaterialsDb::whichBin(double lambda) const {

    double energy = this->convertToEnergy(lambda);
    //PR(energy/eV);
    
//     double bin = -1;
    if( (energy < mEnergy.front()) || (energy > mEnergy.back()) ) {
      { LOG_INFO << "StRichMaterialsDb::whichBin() %n WARNING : %n Energy Out of Range (" << (energy/eV) << " eV)" << endm; }
    }
    return ( (energy - mEnergy.front())/mBinSize);
}

double StRichMaterialsDb::indexOfRefractionOfC6F14At(double wavelength) const {

    double value = -999.;
    double theBinIndex;
    double fraction = modf(this->whichBin(wavelength), &theBinIndex);

    int index = static_cast<int>(theBinIndex);
//     PR(index);
//     PR(fraction);
    if(this->boundsCheck(index)) {
	value = (mFreonN[index] + (mFreonN[index+1]-mFreonN[index])*fraction);
    }
    
    return value;    
}

double StRichMaterialsDb::indexOfRefractionOfQuartzAt(double wavelength) const {

    double value = -999.;
    double theBinIndex;
    double fraction = modf(this->whichBin(wavelength), &theBinIndex);

    int index = static_cast<int>(theBinIndex);
//     PR(index);
//     PR(fraction);
    
    if(this->boundsCheck(index)) {
	value = (mQuartzN[index] + (mQuartzN[index+1]-mQuartzN[index])*fraction);
    }

    return value;
}

double StRichMaterialsDb::indexOfRefractionOfMethaneAt(double wavelength) const {
  return mMethaneIndexOfRefraction;
}
						       
double StRichMaterialsDb::absorptionCoefficientOfC6F14At(double wavelength) const {

    double value = -999.;
    double theBinIndex;
    double fraction = modf(this->whichBin(wavelength), &theBinIndex);

    int index = static_cast<int>(theBinIndex);
//     PR(index);
//     PR(fraction);
    
    if(this->boundsCheck(index)) {
	value = (mFreonL[index] + (mFreonL[index+1]-mFreonL[index])*fraction);
    }

    return value;
}

double StRichMaterialsDb::absorptionCoefficientOfQuartzAt(double wavelength) const {
    
    double value = -999.;
    double theBinIndex;
    double fraction = modf(this->whichBin(wavelength), &theBinIndex);

    int index = static_cast<int>(theBinIndex);
//     PR(index);
//     PR(fraction);
    
    if(this->boundsCheck(index)) {
	value = (mQuartzL[index] + (mQuartzL[index+1]-mQuartzL[index])*fraction);
    }

    return value;
}


double StRichMaterialsDb::quantumEfficiencyOfCsIAt(double wavelength)  const {

    double value = -999.;
    double theBinIndex;
    double fraction = modf(this->whichBin(wavelength), &theBinIndex);

    int index = static_cast<int>(theBinIndex);
//     PR(index);
//     PR(fraction);

    if(this->boundsCheck(index)) {
	value =(mCsIQE[index] + (mCsIQE[index+1]-mCsIQE[index])*fraction);
    }

    return value;
}

double StRichMaterialsDb::absorptionCoefficientOfMethaneAt(double wavelength) const {
     return mMethaneAbsCoeff;
}

void StRichMaterialsDb::print(ostream& os) const {

    os << "**************** StRichMaterialsDb::print() ****************" << endl;
    os << "Version: " << this->version() << endl;
    os << "                     Liquid         Quartz" << endl;
    os << "Energy Lambda      n      Lo       n      Lo       CsI QE" << endl;
    os << " (eV)   (nm)             (cm)            (cm)" << endl;
    os << "-----------------------------------------------------------" << endl;
    os.precision(5);
    for(double lambda=160*nanometer; lambda<220*nanometer; lambda += 5.*nanometer) {

	os << (this->convertToEnergy(lambda)/eV)                         << "\t"
	   << (lambda/nanometer)                                         << "\t"
	   << this->indexOfRefractionOfC6F14At(lambda)                   << "\t"
	   << (this->absorptionCoefficientOfC6F14At(lambda)/centimeter)  << "\t"
	   << this->indexOfRefractionOfQuartzAt(lambda)                  << "\t"
	   << (this->absorptionCoefficientOfQuartzAt(lambda)/centimeter) << "\t"
	   << this->quantumEfficiencyOfCsIAt(lambda)                     << endl;
    }
    os << "==========================================================="  << endl;

}
