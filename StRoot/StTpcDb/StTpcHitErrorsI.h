/***************************************************************************
 *
 * $Id: StTpcHitErrorsI.h,v 1.2 2003/09/02 17:59:12 perev Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for Tpc Hit Errors
 *              The errors are parameterized as 
 *              error = ::sqrt(Sig2Intrinsic+Sig2Drift*driftlength/cos(angle)**2+
 *                           Sig2Tan*tan(angle)**2)
 *              The angle is the crossing angle for the padrow direction errors
 *              and the dip angle for the drift direction errors
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef __STTPCHitErrorsI__
#define __STTPCHitErrorsI__
#include <TObject.h>

class StTpcHitErrorsI : public TObject {

  //Abstract base class defining accessors
public:

  virtual float Sig2IntrinsicOuterX()    const = 0; //sigma^2, intrinsic
  virtual float Sig2DriftOuterX()        const = 0; //sigma^2, drift dependent
  virtual float Sig2TanOuterX()          const = 0; //sigma^2, angular wire

  virtual float Sig2IntrinsicOuterZ()    const = 0; //sigma^2, intrinsic
  virtual float Sig2DriftOuterZ()        const = 0; //sigma^2, drift dependent
  virtual float Sig2TanOuterZ()          const = 0; //sigma^2, angular wire

  virtual float Sig2IntrinsicInnerX()    const = 0; //sigma^2, intrinsic
  virtual float Sig2DriftInnerX()        const = 0; //sigma^2, drift dependent
  virtual float Sig2TanInnerX()          const = 0; //sigma^2, angular wire

  virtual float Sig2IntrinsicInnerZ()    const = 0; //sigma^2, intrinsic
  virtual float Sig2DriftInnerZ()        const = 0; //sigma^2, drift dependent
  virtual float Sig2TanInnerZ()          const = 0; //sigma^2, angular wire




ClassDef(StTpcHitErrorsI,0)

};
#endif















