#ifndef STAR_StXiMaker
#define STAR_StXiMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StXiMaker virtual base class for Maker                               //
//                                                                      //
// $Id: StXiMaker.h,v 1.5 1999/07/15 13:57:55 perev Exp $
// $Log: StXiMaker.h,v $
// Revision 1.5  1999/07/15 13:57:55  perev
// cleanup
//
// Revision 1.4  1999/07/12 23:04:17  fisyak
// Remove glob2
//
// Revision 1.3  1999/07/08 19:09:53  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_exi_exipar;
class St_exi_aux;

class StXiMaker : public StMaker {

 private:
  Bool_t drawinit;
  // static Char_t m_VersionCVS = "$Id: StXiMaker.h,v 1.5 1999/07/15 13:57:55 perev Exp $";
  St_exi_exipar  *m_exipar;      //!
  St_exi_aux     *m_exiaux;      //!

 protected:

  
 public: 
  StXiMaker(const char *name="xi");
  virtual       ~StXiMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StXiMaker.h,v 1.5 1999/07/15 13:57:55 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StXiMaker, 1)   //StAF chain virtual base class for Makers
    };

#endif
