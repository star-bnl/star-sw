// $Id: St_params_Maker.h,v 1.1 1999/01/02 19:08:16 fisyak Exp $
// $Log: St_params_Maker.h,v $
// Revision 1.1  1999/01/02 19:08:16  fisyak
// Add ctf
//
// Revision 1.1  1998/10/31 00:28:32  fisyak
// Makers take care about branches
//
// Revision 1.2  1998/10/06 18:00:44  perev
// cleanup
//
// Revision 1.1  1998/08/07 19:34:54  fisyak
// Add St_params_Maker
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
#ifndef STAR_St_params_Maker
#define STAR_St_params_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_params_Maker virtual base class for Maker                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_params_Maker : public StMaker {
 private:
               Bool_t drawinit;
//  static Char_t m_VersionCVS = "$Id: St_params_Maker.h,v 1.1 1999/01/02 19:08:16 fisyak Exp $";
 protected:
 public: 
                  St_params_Maker(const char *name="params", const char *title="run/params");
   virtual       ~St_params_Maker();
   virtual void   Clear(Option_t *option="");
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_params_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif
