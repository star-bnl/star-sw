/**********************************************************
 * $Id: StRichMaterialsDb.h,v 2.1 2000/09/29 01:35:36 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMaterialsDb.h,v $
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

#ifndef ST_RICH_MATERIALS_H
#define ST_RICH_MATERIALS_H

#include "StRichMaterialsDbInterface.h"

class StRichMaterialsDb : public StRichMaterialsDbInterface {
public:

  static StRichMaterialsDb* getDb();
  
  // Interface
  // common to all materials
  double meanWavelength();
  double shortestWavelength();
  double longestWavelength();
  double meanRadiatorDepth();  
  double innerWavelength();
  double outerWavelength();
  void   setWavelengthRange(double, double);

  // C6F14    
  double indexOfRefractionOfC6F14At(double wavelength);
  double absorptionCoefficientOfC6F14At(double wavelength);
  
  // quartz 
  double indexOfRefractionOfQuartzAt(double wavelength);
  double absorptionCoefficientOfQuartzAt(double wavelength);
  
  // methane
  double indexOfRefractionOfMethaneAt(double wavelength);
  double absorptionCoefficientOfMethaneAt(double wavelength);
  
  double quantumEfficiencyOfCsIAt(double wavelength);

  double version() const;
  void   print(ostream& os = cout) const;
    
protected:
    StRichMaterialsDb();
    
private:
    void my_fill();             // fill with my own stuff
    
    static StRichMaterialsDb* p2Db;   // handle to only instance

    bool boundsCheck(double index);

  double mVersion;
  double mLongestWavelength;
  double mShortestWavelength;
  double mMeanWavelength;  
  double mConversion;
  double mInnerWave;
  double mOuterWave;


  double mMeanRadiatorDepth;


    ///////////    measured CERN data    /////////////
    /* index of refraction measured at 11 different wavelengths  169 -- 220 */
  //    static const int arraySize = 11;
    double mC6F14IndexOfRefraction[13]; 
    double mQuartzIndexOfRefraction[13];
    double mMethaneIndexOfRefraction; 
    
    /* photon absorption coefficient measured at 11 different wavelengths  169 -- 220 */
    double mC6F14AbsCoeff[13]; 
    double mQuartzAbsCoeff[13];
    double mMethaneAbsCoeff;

    /* CsI QE measured at 11 different wavelengths  169 -- 220 */
    double mCsIQE[13];
      
};

#endif


