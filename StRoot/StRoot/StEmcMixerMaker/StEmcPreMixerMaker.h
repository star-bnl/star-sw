/*!\class StEmcPreMixerMaker
\author Alexandre A. P. Suaide

This Maker is a should be used as a pre embedding maker because
depending on the situation the event data is overwritten and need
to be reset. This maker just gets the event date from the first 
StEvent in the memory and set as the chain date so the database
tables pointers can be set properly.
*/

#ifndef STAR_StEmcPreMixerMaker
#define STAR_StEmcPreMixerMaker
#include "StMaker.h"
                                                      
class StEmcPreMixerMaker : public StMaker 
{
  private:

  protected:

  public: 

                  StEmcPreMixerMaker(const char *name="emcEmbed");
    virtual       ~StEmcPreMixerMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
      
    virtual const char *GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEmcPreMixerMaker.h,v 1.3 2014/08/06 11:43:05 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StEmcPreMixerMaker,0) 
};
#endif








