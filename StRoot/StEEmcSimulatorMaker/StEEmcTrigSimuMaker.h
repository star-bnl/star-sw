// $Id: StEEmcTrigSimuMaker.h,v 1.1 2003/01/28 23:13:00 balewski Exp $
// $Log: StEEmcTrigSimuMaker.h,v $
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
  // static Char_t  m_VersionCVS = "$Id: StEEmcTrigSimuMaker.h,v 1.1 2003/01/28 23:13:00 balewski Exp $";
  
 protected:
 public: 
  StEEmcTrigSimuMaker(const char *name="EEmcTrigSimu");
  virtual       ~StEEmcTrigSimuMaker();
  void printE();///< Endcap hits in StEvent

  virtual Int_t Init();
  virtual Int_t  Make();
   
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcTrigSimuMaker.h,v 1.1 2003/01/28 23:13:00 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StEEmcTrigSimuMaker, 1)
};

#endif

