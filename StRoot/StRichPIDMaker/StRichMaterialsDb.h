/**********************************************************
 * $Id: StRichMaterialsDb.h,v 1.1 2000/04/03 19:36:07 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMaterialsDb.h,v $
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/

#ifndef ST_RICH_MATERIALS_H
#define ST_RICH_MATERIALS_H

#include "StRichMaterialsDbInterface.h"

class StRichMaterialsDb : public StRichMaterialsDbInterface {
public:

  static StRichMaterialsDb* getDb();
  
  // Interface     
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
  double mLongestWaveLength;
  double mShortestWaveLength;
  double mConversion;

    ///////////    measured CERN data    /////////////
    /* index of refraction measured at 11 different wavelengths  169 -- 220 */
  //    static const int arraySize = 11;
    double mC6F14IndexOfRefraction[11]; 
    double mQuartzIndexOfRefraction[11];
    double mMethaneIndexOfRefraction; 
    
    /* photon absorption coefficient measured at 11 different wavelengths  169 -- 220 */
    double mC6F14AbsCoeff[11]; 
    double mQuartzAbsCoeff[11];
    double mMethaneAbsCoeff;

    /* CsI QE measured at 11 different wavelengths  169 -- 220 */
    double mCsIQE[11];
      
};

#endif


