/**********************************************************
 * $Id: StRichMaterialsDbInterface.h,v 1.1 2000/04/03 19:36:08 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMaterialsDbInterface.h,v $
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/

#ifndef ST_RICH_MATERIALS_INTERFACE_H
#define ST_RICH_MATERIALS_INTERFACE_H

#include <iostream.h>
#include "StThreeVector.hh"

class StRichMaterialsDbInterface {
public:
  
  virtual ~StRichMaterialsDbInterface() {}
  //StRichMaterialsInterface(const StRichMaterialsInterface&);
  //StRichMaterialsInterface&(const StRichMaterialsInterface&);
  
  virtual double version() const = 0;
  
  // index of refraction
  virtual double indexOfRefractionOfC6F14At(double wavelength) = 0;
  virtual double indexOfRefractionOfQuartzAt(double wavelength) = 0;
  virtual double indexOfRefractionOfMethaneAt(double wavelength) = 0;	
  
  // absorption coeff. for photons in a material
  virtual double absorptionCoefficientOfC6F14At(double wavelength) = 0;
  virtual double absorptionCoefficientOfQuartzAt(double wavelength) = 0;
  virtual double absorptionCoefficientOfMethaneAt(double wavelength) = 0;
  
  // QE of CsI
  virtual double quantumEfficiencyOfCsIAt(double wavelength) = 0;
  
  
  virtual void   print(ostream& os = cout) const = 0;
};
#endif
