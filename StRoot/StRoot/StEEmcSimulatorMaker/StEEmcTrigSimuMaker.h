// $Id: StEEmcTrigSimuMaker.h,v 1.3 2014/08/06 11:43:04 jeromel Exp $
// $Log: StEEmcTrigSimuMaker.h,v $
// Revision 1.3  2014/08/06 11:43:04  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2003/09/10 19:47:08  perev
// ansi corrs
//
// Revision 1.1  2003/01/28 23:13:00  balewski
// star
//

/* \class  StEEmcTrigSimuMaker
\author Jan Balewski, Rene Fatemi

Simulates E+B EMC trigger response, based on StEvent  hits <br>

Example how to use this maker:
www.star.bnl.gov/STAR/eemc -->How To

 */


#ifndef STAR_StEmcTrigSimuMaker
#define STAR_StEmcTrigSimuMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif



class StEEmcTrigSimuMaker : public StMaker {
 private:
  // static Char_t  m_VersionCVS = "$Id: StEEmcTrigSimuMaker.h,v 1.3 2014/08/06 11:43:04 jeromel Exp $";
  
 protected:
 public: 
  StEEmcTrigSimuMaker(const char *name="EEmcTrigSimu");
  virtual       ~StEEmcTrigSimuMaker();
  void printE();///< Endcap hits in StEvent

  virtual Int_t Init();
  virtual Int_t  Make();
   
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcTrigSimuMaker.h,v 1.3 2014/08/06 11:43:04 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(StEEmcTrigSimuMaker,0)
};

#endif

