#ifndef STAR_St_xdfin_Maker
#define STAR_St_xdfin_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_xdfin_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_xdfin_Maker : public StMaker {
private:
   Bool_t         m_Init_Done; //Flag that it should be get initialization from xdf (kFALSE)
   virtual void   Split();
public:
                  St_xdfin_Maker(const char *name="xdfin",const char *title="bfc_xdf");
   virtual       ~St_xdfin_Maker();
   virtual Int_t  Init();
   virtual void   Init_Done (Bool_t k=kFALSE){m_Init_Done = k;} // *MENU*
   virtual Int_t  Make();
   virtual void   Finish(Option_t *option);
   virtual void   PrintInfo();
   ClassDef(St_xdfin_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
