/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description: Calculate the quantum statistic only
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/


#include "StHbtMaker/Base/StHbtThPair.hh"
#include "StHbtMaker/ThCorrFctn/StHbtFsiQuantStat.h"
#include "StarClassLibrary/PhysicalConstants.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

#ifdef __ROOT__
ClassImp(StHbtFsiQuantStat)
#endif
double StHbtFsiQuantStat::GetWeight(const StHbtThPair* aThPair){
  
  if (aThPair->GetPid1()==aThPair->GetPid2()){
    int tSpin=abs(aThPair->GetPid1())%10;  // tSpin=2J+1
    if (tSpin==0) return 1;  // non identified particle
    double tSpinFactor=-::pow(-1.,tSpin)/tSpin;  //for non polarised particle
    StHbtLorentzVector q= *(aThPair->GetRealMomentum1())-
      *(aThPair->GetRealMomentum2());
    StHbtLorentzVector r=*(aThPair->GetEmPoint1())-
      *(aThPair->GetEmPoint2());
    double wei=(1.+tSpinFactor*cos((double)(q*r)*fermi/hbarc));
    return wei;
  } else {
    return 1;
  }
}

//inline double StHbtFsiQuantStat::GetWeightDen() {return mWeightDen;}

