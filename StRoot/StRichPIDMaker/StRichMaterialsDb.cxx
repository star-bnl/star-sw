/**********************************************************
 * $Id: StRichMaterialsDb.cxx,v 2.2 2000/10/02 23:06:33 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMaterialsDb.cxx,v $
 *  Revision 2.2  2000/10/02 23:06:33  horsley
 *  *** empty log message ***
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
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif


StRichMaterialsDb* StRichMaterialsDb::p2Db = 0;

StRichMaterialsDb::StRichMaterialsDb() {
  my_fill();
}

void StRichMaterialsDb::my_fill() {
  mVersion = 1.0;
 
  mLongestWavelength  = 220.0*nanometer;    // --> inner ring
  mMeanWavelength     = 177.4*nanometer;    // --> mode frequency  
  mShortestWavelength = 160.0*nanometer;    // --> outer ring
 
  
  double numberOfEntries = 13;
  mConversion  
    = ((mLongestWavelength/nanometer-mShortestWavelength/nanometer)/(numberOfEntries-1));

  mMeanRadiatorDepth = 0.5; // normalized to unit pathlength in radiator
  
  // goes from long to short wavelength
  ////////// index of refraction  ////////////////////////////
  int entries = static_cast<int>(numberOfEntries);
  double index_freon[13] = { 1.269946, 
				  1.271246, 
				  1.272452, 
				  1.273844,
				  1.275236,
				  1.276721, 
				  1.278206, 
				  1.279876,
				  1.281546, 
				  1.283402, 
				  1.285444,
                                  1.2869634,
                                  1.2884828};
  // the last two entries correspond to 164 and 159 nm
  // gotten from a linear extrapolation of the CERN measurements

  
  double index_quartz[13] = { 1.528309, 
				   1.533333, 
				   1.538243, 
				   1.544223,
				   1.550568, 
				   1.557770, 
				   1.565463, 
				   1.574765,
				   1.584831, 
				   1.597027, 
				   1.611858,
                                   1.620049,
                                   1.628240};

  // the last two entries correspond to 164 and 159 nm
  // gotten from a linear extrapolation of the CERN measurements


  
  mMethaneIndexOfRefraction  = 1.000444;
  
  
  /////////// absorption length  (cm) ////////////////////////////////// 
  double absco_freon[13]  = { 179.0987   * centimeter, 
				   179.0987   * centimeter,
				   179.0987   * centimeter,
				   179.0987   * centimeter,
				   179.0987   * centimeter,
				   121.9547   * centimeter, 
				   43.40067  * centimeter, 
				   15.7394   * centimeter,
				   9.417928 * centimeter, 
				   5.195241 * centimeter,  
				   1.415808 * centimeter,
                                   1.415808 * centimeter,
                                   1.415808 * centimeter};
  
  double absco_quartz[13] = { 1000000.    * centimeter,  
				   1000000.    * centimeter,  
				   1000000.    * centimeter,  
				   1000000.    * centimeter,  
				   1000000.    * centimeter,  
				   29.85  * centimeter,   
				   7.34  * centimeter,   
				   4.134 * centimeter,   
				   1.273 * centimeter,  
				   0.722 * centimeter,   
				   0.365 * centimeter,
				   0.365 * centimeter,
                                   0.365 * centimeter };
  
  mMethaneAbsCoeff = 1000000.0 * centimeter;
  
  ////////// CsI quantum efficiency  //////////////
  double effic_csi[13] = { 3.15e-4, 
				4.50e-4, 
				6.75e-3, 
				1.125e-2, 
				2.115e-2,
				3.60e-2, 
				8.46e-2,  
				.15533,   
				.20286,
				.24745,
				.27881,
                                .27881, 
                                .27881};
  
  // fill arrays
  for (int i=0;i<entries;i++) {
    
    mC6F14IndexOfRefraction[i]  = index_freon[i];
    mQuartzIndexOfRefraction[i] = index_quartz[i];
    
    mC6F14AbsCoeff[i]  = absco_freon[i];
    mQuartzAbsCoeff[i] = absco_quartz[i];
    
    mCsIQE[i] = effic_csi[i];
  }
}

StRichMaterialsDb* StRichMaterialsDb::getDb() {
  if(!p2Db) p2Db = new StRichMaterialsDb();
  return p2Db;
}

void StRichMaterialsDb::print(ostream& os) const {
  // os << "**************** StRichMaterialsDb::print() ****************" << endl;
}


double StRichMaterialsDb::meanWavelength() {
  return mMeanWavelength;
}


double StRichMaterialsDb::longestWavelength() {
  return mLongestWavelength;
}


double StRichMaterialsDb::shortestWavelength() {
  return mShortestWavelength;
}

double StRichMaterialsDb::meanRadiatorDepth() {
  return mMeanRadiatorDepth;
}


double StRichMaterialsDb::innerWavelength() {
  return mInnerWave;
}


double StRichMaterialsDb::outerWavelength() {
  return mOuterWave;
}


void StRichMaterialsDb::setWavelengthRange(double shortwave, double longwave) {
  mInnerWave = longwave;
  mOuterWave = shortwave;

  cout << "StRichMaterialsDb::setWavelenghtRange() ---> using wavelengths " 
       <<  mInnerWave/nanometer  << "  nm     and " 
       << mOuterWave/nanometer << "nm " << endl; 
}


double StRichMaterialsDb::indexOfRefractionOfC6F14At(double wavelength) {
  
  // convert wavelenght from centimeters (base unit) into nanometers
  wavelength = wavelength/nanometer;
  
  double index = (mLongestWavelength/nanometer - wavelength)/mConversion; 
  if(boundsCheck(index)) {
    double fraction = index - static_cast<int>(index);
    return (1.0-fraction)*mC6F14IndexOfRefraction[static_cast<int>(index)]
               + (fraction)*mC6F14IndexOfRefraction[static_cast<int>(index) + 1];
  }
  return 0;
}

double StRichMaterialsDb::indexOfRefractionOfQuartzAt(double wavelength) {

  // convert wavelenght from centimeters (base unit) into nanometers
  wavelength = wavelength/nanometer;
  double index = (mLongestWavelength/nanometer - wavelength)/mConversion; 
  if(boundsCheck(index)) {
    double fraction = index - static_cast<int>(index);
    return (1.0-fraction)*mQuartzIndexOfRefraction[static_cast<int>(index)]
             + (fraction)*mQuartzIndexOfRefraction[static_cast<int>(index) + 1];
  }
  return 0;
}

double StRichMaterialsDb::indexOfRefractionOfMethaneAt(double wavelength) {
  return mMethaneIndexOfRefraction;
}
						       
double StRichMaterialsDb::absorptionCoefficientOfC6F14At(double wavelength) {

  // convert wavelenght from centimeters (base unit) into nanometers
  wavelength = wavelength/nanometer;
  
   double index = (mLongestWavelength/nanometer - wavelength)/mConversion; 
   
   if(boundsCheck(index)) {
     double fraction = index - static_cast<int>(index);
     return (1.0-fraction)*mC6F14AbsCoeff[static_cast<int>(index)]
	        + (fraction)*mC6F14AbsCoeff[static_cast<int>(index) + 1];
   }
   return 0;
}

double StRichMaterialsDb::absorptionCoefficientOfQuartzAt(double wavelength) {
    
  // convert wavelenght from centimeters (base unit) into nanometers
  wavelength = wavelength/nanometer;
  
  double index = (mLongestWavelength/nanometer - wavelength)/mConversion; 
  
  if(boundsCheck(index)) {
    double fraction = index - static_cast<int>(index);
    return (1.0-fraction)*mQuartzAbsCoeff[static_cast<int>(index)]
               + (fraction)*mQuartzAbsCoeff[static_cast<int>(index) + 1];
  }
  return 0.0;
}


double StRichMaterialsDb::quantumEfficiencyOfCsIAt(double wavelength)  {
    
  // convert wavelenght from centimeters (base unit) into nanometers
  wavelength = wavelength/nanometer;
  
  double index = (mLongestWavelength/nanometer - wavelength)/mConversion; 
 
  if(boundsCheck(index)) {
    double fraction = index - static_cast<int>(index);
    return (1.0-fraction)*mCsIQE[static_cast<int>(index)]
      + (fraction)*mCsIQE[static_cast<int>(index) + 1];
  }

  return 0;
}

bool StRichMaterialsDb::boundsCheck(double index) {
  
  
  if (index>13.0 || index<0.0) {
    cerr << "index = " << index << endl;
  cerr << "!!!!!!!!!!!!!!!!!!!!!!!!!"
	 << endl << "WARNING: StRichMaterialsDb passed invalid"
	 << " wavelength = " <<  mLongestWavelength/nanometer - mConversion*index
	 << " nm.  Expected Range between 220 - 159 nm." << endl
	 << "!!!!!!!!!!!!!!!!!!!!!!!!!" << endl << endl;
    return false;}
  
  return true;
}

double StRichMaterialsDb::version() const {
  return mVersion;}

double StRichMaterialsDb::absorptionCoefficientOfMethaneAt(double wavelength)  {
     return mMethaneAbsCoeff;
}










