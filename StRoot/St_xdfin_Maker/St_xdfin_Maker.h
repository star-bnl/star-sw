#ifndef STAR_St_xdfin_Maker
#define STAR_St_xdfin_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_xdfin_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StMaker.h"

class St_xdfin_Maker : public StMaker {
public:
                  St_xdfin_Maker();
                  St_xdfin_Maker(const char *name,const char *title="");
   virtual       ~St_xdfin_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual void   Clear(Option_t *option);
   virtual void   PrintInfo();
   ClassDef(St_xdfin_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
