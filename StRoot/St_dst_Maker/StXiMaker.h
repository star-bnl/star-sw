#ifndef STAR_StXiMaker
#define STAR_StXiMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StXiMaker virtual base class for Maker                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_exi_exipar;
class St_exi_aux;

class StXiMaker : public StMaker {

 private:
  Bool_t drawinit;
  // static Char_t m_VersionCVS = "$Id: StXiMaker.h,v 1.1.2.1 1999/07/01 17:27:41 fisyak Exp $";
  St_exi_exipar  *m_exipar;      //!
  St_exi_aux     *m_exiaux;      //!

 protected:

  
 public: 
  StXiMaker(const char *name="xi");
  virtual       ~StXiMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  ClassDef(StXiMaker, 1)   //StAF chain virtual base class for Makers
    };

#endif
