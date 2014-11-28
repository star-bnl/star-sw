/***************************************************************************
 *
 * $Id: StFpdMaker.h,v 1.3 2014/08/06 11:43:15 jeromel Exp $ 
 *
 * Author: AKIO OGAWA
 *
 ***************************************************************************
 *
 * Description: FPD offline software
 ***************************************************************************
 *
 * $Log: StFpdMaker.h,v $
 * Revision 1.3  2014/08/06 11:43:15  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.2  2003/09/10 19:47:16  perev
 * ansi corrs
 *
 * Revision 1.1  2002/01/16 20:21:07  akio
 * first version
 *
 *
 **************************************************************************/
#ifndef STAR_StFpdMaker
#define STAR_StFpdMaker
#include "StMaker.h"

//#define FPDP_DEBUG

class TH1F;
class StEvent;
class StDAQReader;
class StFpdReaderInterface;

// class definition
class StFpdMaker : public StMaker {
public: 
  StFpdMaker(const char *name="fpd");
  virtual ~StFpdMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
  
protected:
  
private:
  StEvent* mEvent;

  ClassDef(StFpdMaker,0)   //StAF chain virtual base class for Makers
};
#endif
