/**********************************************************
 * $Id: StRichMaterialsDb.cxx,v 1.1 2000/04/03 19:36:07 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMaterialsDb.cxx,v $
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/
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

  mLongestWaveLength = 220.0*nanometer;
  mShortestWaveLength = 169.0*nanometer;
  mConversion = 5.1;
  

  
  mMeanRadiatorDepth = 0.5; // normalized to unit pathlength in radiator
  double index_freon[11] = { 1.269946, 
			           1.271246, 
			           1.272452, 
			           1.273844,
			           1.275236,
			           1.276721, 
			           1.278206, 
			           1.279876,
			           1.281546, 
			           1.283402, 
			           1.285444 };

  double index_quartz[11] = { 1.528309, 
			            1.533333, 
			            1.538243, 
			            1.544223,
			            1.550568, 
			            1.557770, 
			            1.565463, 
			            1.574765,
			            1.584831, 
			            1.597027, 
			            1.611858 };
  // gotten from a linear extrapolation of the CERN measurements

  
    
    /////////// absorption length  (cm) ////////////////////////////////// 
    double absco_freon[11]  = { 179.0987   * centimeter, 
			              179.0987   * centimeter,
			              179.0987   * centimeter,
			              179.0987   * centimeter,
		                      179.0987   * centimeter,
				      121.9547   * centimeter, 
				       43.40067  * centimeter, 
				       15.7394   * centimeter,
				        9.417928 * centimeter, 
			     	        5.195241 * centimeter,  
				        1.415808 * centimeter};

    double absco_quartz[11] = { 1000000.    * centimeter,  
				      1000000.    * centimeter,  
                                      1000000.    * centimeter,  
                                      1000000.    * centimeter,  
                                      1000000.    * centimeter,  
			                   29.85  * centimeter,   
                                            7.34  * centimeter,   
                                            4.134 * centimeter,   
                                            1.273 * centimeter,  
                                            0.722 * centimeter,   
                                            0.365 * centimeter};

    mMethaneAbsCoeff = 1000000.0 * centimeter;
    
    ////////// CsI quantum efficiency  //////////////
    double effic_csi[11] = { 3.15e-4, 
			          4.50e-4, 
			          6.75e-3, 
			          1.125e-2, 
			          2.115e-2,
			          3.60e-2, 
			          8.46e-2,  
			           .15533,   
			           .20286,
			           .24745,
			           .27881 };
    
    // fill arrays
    for (int i=0;i<11;i++) {
      
      mC6F14IndexOfRefraction[i]  = index_freon[i];
      mQuartzIndexOfRefraction[i] = index_quartz[i];
      
      mC6F14AbsCoeff[i]  = absco_freon[i];
      mQuartzAbsCoeff[i] = absco_quartz[i];
      
      mCsIQE[i] = effic_csi[i];
    }
    
    mCsIQE[i] = effic_csi[i];
  }
    if(!p2Db) p2Db = new StRichMaterialsDb();
    return p2Db;
StRichMaterialsDb* StRichMaterialsDb::getDb() {
  if(!p2Db) p2Db = new StRichMaterialsDb();
  return p2Db;
}

       << mOuterWave/nanometer << "nm " << endl; 
}


double StRichMaterialsDb::indexOfRefractionOfC6F14At(double wavelength) {
  
  double index = (mLongestWaveLength/nanometer - wavelength)/mConversion; 
  
  wavelength = wavelength/nanometer;
  
  double index = (mLongestWavelength/nanometer - wavelength)/mConversion; 
  if(boundsCheck(index)) {
    double fraction = index - static_cast<int>(index);
    return (1.0-fraction)*mC6F14IndexOfRefraction[static_cast<int>(index)]
               + (fraction)*mC6F14IndexOfRefraction[static_cast<int>(index) + 1];
  }
  return 0;
      

double StRichMaterialsDb::indexOfRefractionOfQuartzAt(double wavelength) {

  double index = (mLongestWaveLength/nanometer - wavelength)/mConversion; 
  
  // convert wavelenght from centimeters (base unit) into nanometers
  wavelength = wavelength/nanometer;
  double index = (mLongestWavelength/nanometer - wavelength)/mConversion; 
               + (fraction)*mQuartzIndexOfRefraction[static_cast<int>(index) + 1];
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

   double index = (mLongestWaveLength/nanometer - wavelength)/mConversion; 
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
    
  double index = (mLongestWaveLength/nanometer - wavelength)/mConversion; 
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
    
  double index = (mLongestWaveLength/nanometer - wavelength)/mConversion; 
  wavelength = wavelength/nanometer;
  
  double index = (mLongestWavelength/nanometer - wavelength)/mConversion; 
 
  if(boundsCheck(index)) {
    double fraction = index - static_cast<int>(index);
    return (1.0-fraction)*mCsIQE[static_cast<int>(index)]
      + (fraction)*mCsIQE[static_cast<int>(index) + 1];
  }

  return 0;
}
  if (index>11.0 || index<0.0) {
    cerr << "!!!!!!!!!!!!!!!!!!!!!!!!!"
  if (index>13.0 || index<0.0) {
	 << " wavelength = " <<  mLongestWaveLength/nanometer - mConversion*index
	 << " nm.  Expected Range between 220 - 169 nm." << endl
	 << endl << "WARNING: StRichMaterialsDb passed invalid"
	 << " wavelength = " <<  mLongestWavelength/nanometer - mConversion*index
	 << " nm.  Expected Range between 220 - 159 nm." << endl
	 << "!!!!!!!!!!!!!!!!!!!!!!!!!" << endl << endl;
    return false;}
  
  return true;
}

double StRichMaterialsDb::version() const {
  return mVersion;}




