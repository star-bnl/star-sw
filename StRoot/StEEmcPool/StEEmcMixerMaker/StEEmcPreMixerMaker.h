/*!\class StEEmcPreMixerMaker
\author Alexandre A. P. Suaide
Modified for EEMC                 Wei-Ming Zhang 3/28/2005

This Maker is a should be used as a pre embedding maker because
depending on the situation the event data is overwritten and need
to be reset. This maker just gets the event date from the first 
StEvent in the memory and set as the chain date so the database
tables pointers can be set properly.
*/

#ifndef STAR_StEEmcPreMixerMaker
#define STAR_StEEmcPreMixerMaker
#include "StMaker.h"
                                                      
class StEEmcPreMixerMaker : public StMaker 
{
  private:

  protected:

  public: 

                  StEEmcPreMixerMaker(const char *name="eemcPreEmbed");
    virtual       ~StEEmcPreMixerMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
      
    virtual const char *GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEEmcPreMixerMaker.h,v 1.1.1.1 2005/05/31 18:53:25 wzhang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEEmcPreMixerMaker,0) 
};
#endif

///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcPreMixerMaker.h,v 1.1.1.1 2005/05/31 18:53:25 wzhang Exp $
// $Log: StEEmcPreMixerMaker.h,v $
// Revision 1.1.1.1  2005/05/31 18:53:25  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////
