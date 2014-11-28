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
#ifndef StHbtFsiQuanStat_hh
#define StHbtFsiQuanStat_hh

#include "StHbtMaker/Base/StHbtFsiWeight.hh"

class StHbtFsiQuantStat : public  StHbtFsiWeight {
 public:

  double GetWeight(const StHbtThPair* aThPair);
   
  //virtual StHbtString Report(); 

#ifdef __ROOT__
   ClassDef(StHbtFsiQuantStat,0)
#endif
};
  
#endif
