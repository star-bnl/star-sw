#ifndef STAR_St_TLA_Maker
#define STAR_St_TLA_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TLA_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_TLA_Maker : public StMaker {
 private:
               Bool_t drawinit;
 protected:
 public: 
                  St_TLA_Maker();
                  St_TLA_Maker(const char *name, const char *title);
   virtual       ~St_TLA_Maker();
   virtual void   Clear(Option_t *option="");
   virtual void   Finish();
   virtual void   Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_TLA_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
