//////////////////////////////////////////////////////////////////////////
//
// Author: Aleksei Pavlinov, WSU 08-mar-2001
//
// StEmcMath.h => Encapsulate math routines for special EMC case.
//
//////////////////////////////////////////////////////////////////////////
#ifndef STAR_StEmcMath
#define STAR_StEmcMath

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

class StMeasuredPoint;

class StEmcMath {

private:
public:
  static Bool_t etaPhi(StMeasuredPoint* point, StMeasuredPoint* vertex, 
                       Double_t &eta, Double_t &phi);
  static Double_t pseudoRapidity(StMeasuredPoint* point, StMeasuredPoint* vertex=0);
  static Double_t eta(StMeasuredPoint* point, StMeasuredPoint* vertex=0) 
    {return pseudoRapidity(point,vertex);}
  static Double_t phi(StMeasuredPoint* point, StMeasuredPoint* vertex=0);

  ClassDef(StEmcMath, 1)   // Definition of patch
};
#endif
// $Id: StEmcMath.h,v 1.1 2001/03/09 16:59:49 pavlinov Exp $
// $Log: StEmcMath.h,v $
// Revision 1.1  2001/03/09 16:59:49  pavlinov
// Alpha Version
//
