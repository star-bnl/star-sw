/**********************************************************
 * $Id: StRichMaterialsDbInterface.h,v 2.3 2003/09/02 17:58:53 perev Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMaterialsDbInterface.h,v $
 *  Revision 2.3  2003/09/02 17:58:53  perev
 *  gcc 3.2 updates + WarnOff
 *
 *  Revision 2.2  2001/04/10 16:56:07  lasiuk
 *  Change parameters to bring into line with richgeo.g and CERN.
 *
 *  Revision 2.1  2000/09/29 01:35:36  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/

#ifndef ST_RICH_MATERIALS_INTERFACE_H
#define ST_RICH_MATERIALS_INTERFACE_H

#include <Stiostream.h>
#include "StThreeVector.hh"

class StRichMaterialsDbInterface {
public:
  
  virtual ~StRichMaterialsDbInterface() {}
  //StRichMaterialsInterface(const StRichMaterialsInterface&);
  //StRichMaterialsInterface&(const StRichMaterialsInterface&);
  
  virtual double version() const = 0;
  
  // index of refraction
  virtual double indexOfRefractionOfC6F14At(double wavelength) const = 0;
  virtual double indexOfRefractionOfQuartzAt(double wavelength) const = 0;
  virtual double indexOfRefractionOfMethaneAt(double wavelength) const = 0;	
  
  // absorption coeff. for photons in a material
  virtual double absorptionCoefficientOfC6F14At(double wavelength) const = 0;
  virtual double absorptionCoefficientOfQuartzAt(double wavelength) const = 0;
  virtual double absorptionCoefficientOfMethaneAt(double wavelength) const = 0;
  
  // QE of CsI
  virtual double quantumEfficiencyOfCsIAt(double wavelength) const = 0;
  
  
  virtual void   print(ostream& os = cout) const = 0;
};
#endif
