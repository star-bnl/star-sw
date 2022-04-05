/****************************************************************
 * $Id: StMuBTofUtil.h,v 1.1 2009/02/20 17:05:59 tone421 Exp $
 *
 * Author: Xin Dong, Feb 2009
 *
 *****************************************************************
 *
 * Description:
 * Convert StBTofHitCollection in StEvent to 
 * StMuBTofHitCollection in MuDst
 *
 *****************************************************************
 *
 * $Log: StMuBTofUtil.h,v $
 * Revision 1.1  2009/02/20 17:05:59  tone421
 * *** empty log message ***
 *
 *
 ****************************************************************/
#ifndef StMuBTofUtil_h
#define StMuBTofUtil_h
#include "TObject.h"

class StMuBTofHitCollection;
class StEvent;
class StBTofCollection;

class StMuBTofUtil : public TObject
{
 protected:
 
 public:
  StMuBTofUtil();
  ~StMuBTofUtil();
  StMuBTofHitCollection* getMuBTofHit(StBTofCollection *);
  void fillMuBTofHit(StMuBTofHitCollection*, StBTofCollection*);
  
  ClassDef(StMuBTofUtil,1)
};
    
#endif
