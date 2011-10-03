/*!\class StEEmcMatchMaker
Author Wei-Ming Zhang              KSU    4/27/2005
An example maker to read maps from StEEmcAssociationMaker for efficiency study
*/

#ifndef STAR_StEEmcMatchMaker
#define STAR_StEEmcMatchMaker
#include "StMaker.h"
                                                      
class StEEmcMatchMaker : public StMaker 
{
  private:

  protected:

  public: 

                  StEEmcMatchMaker(const char *name="eemcEff");
    virtual       ~StEEmcMatchMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
      
    virtual const char *GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEEmcMatchMaker.h,v 1.1.1.1 2005/05/31 18:54:40 wzhang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEEmcMatchMaker,0) 
};
#endif

///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcMatchMaker.h,v 1.1.1.1 2005/05/31 18:54:40 wzhang Exp $
// $Log: StEEmcMatchMaker.h,v $
// Revision 1.1.1.1  2005/05/31 18:54:40  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////
