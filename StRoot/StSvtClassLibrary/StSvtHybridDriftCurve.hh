/***************************************************************************
 *
 * $Id: StSvtHybridDriftCurve.hh,v 1.1 2004/07/26 00:04:43 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Drift Velocity curve class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDriftCurve.hh,v $
 * Revision 1.1  2004/07/26 00:04:43  munhoz
 * adding drift curve class
 *
 *
 **************************************************************************/

#ifndef STSVTHYBRIDDRIFTCURVE_HH
#define STSVTHYBRIDDRIFTCURVE_HH

#include "StSvtHybridObject.hh"

class StSvtHybridDriftCurve : public StSvtHybridObject
{
public:
  StSvtHybridDriftCurve();
  StSvtHybridDriftCurve(int barrel, int ladder, int wafer, int hybrid);
  ~StSvtHybridDriftCurve();

  StSvtHybridDriftCurve(const StSvtHybridDriftCurve&);
  StSvtHybridDriftCurve& operator = (const StSvtHybridDriftCurve&);

  void setParameter(int adc, int param, double value){driftCurve[adc-1][param-1]=value;}
  double getParameter(int adc, int param){return driftCurve[adc-1][param-1];}
  
private:
  double driftCurve[3][10];

  ClassDef(StSvtHybridDriftCurve,1)
};

#endif
