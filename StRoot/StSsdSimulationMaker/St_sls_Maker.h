#ifndef STAR_St_sls_Maker
#define STAR_St_sls_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_sls_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_sdm_geom_par;
class St_svg_geom;
class St_sls_ctrl;

class St_sls_Maker : public StMaker {
 private:
  St_sdm_geom_par *m_geom_par;//!
  St_svg_geom     *m_geom;//!
  St_sls_ctrl     *m_ctrl;//!
 public: 
	          St_sls_Maker(const char *name="sls_strip");
   virtual       ~St_sls_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();
   ClassDef(St_sls_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif







