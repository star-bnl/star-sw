/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : This ThPair is used in ThCFGaussFit. it Inherit from 
 * ThPairGauss. functionality are add to save the position of emission in 
 * the gaussian distribution before the lorentz Transformation. Then
 * the method GetRejectionProb2Size(x,y,z,t) return the rejection probability 
 * that must be applied so that the effective distribution is a gaussian with
 * size x,y,z,t
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef ST_HBT_THPAIR_GAUSSFIT_HH
#define ST_HBT_THPAIR_GAUSSFIT_HH

#include "StHbtMaker/ThCorrFctn/StHbtThPairGauss.h"


class StHbtThPairGaussFit : public StHbtThPairGauss {
  
 public:
  
  StHbtThPairGaussFit();
  virtual ~StHbtThPairGaussFit();
  virtual void Set(const StHbtPair* aPair);
  virtual double GetRejectionProb2Size(double aX, double aY, double aZ, double aT);
  virtual void setVariables(const StHbtPair*);
  
 protected:
 StHbtLorentzVector mSourceDist1;
 StHbtLorentzVector mSourceDist2;

};


#endif
