/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Base class for the Weight calculator
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtFsiWeight_hh
#define StHbtFsiWeight_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtThPair;


class StHbtFsiWeight {
public: 
  StHbtFsiWeight();
  virtual ~StHbtFsiWeight();
  
  // This function provide a standard input for the weight calculator
  // No matter the hidden info format
  virtual double GetWeight(const StHbtThPair* aThPair)=0;
  // Use when the 3 body calculation is switched on
  virtual double GetWeightDen();

  virtual StHbtString Report();

protected:
double mWeightDen;

#ifdef __ROOT__
  ClassDef(StHbtFsiWeight,1)
#endif
};

#endif
