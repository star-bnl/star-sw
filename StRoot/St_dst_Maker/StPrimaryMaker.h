#ifndef STAR_StPrimaryMaker
#define STAR_StPrimaryMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StPrimaryMaker virtual base class for Maker                          //
//                                                                      //
// $Id: StPrimaryMaker.h,v 1.7 1999/12/10 17:38:42 genevb Exp $
// $Log: StPrimaryMaker.h,v $
// Revision 1.7  1999/12/10 17:38:42  genevb
// Added fixed vtx functionality, allow lmv and fixed vtx only one vtx entry
//
// Revision 1.6  1999/09/12 23:03:04  fisyak
// Move parameters into makers
//
// Revision 1.5  1999/07/15 13:57:53  perev
// cleanup
//
// Revision 1.4  1999/07/12 23:04:16  fisyak
// Remove glob2
//
// Revision 1.3  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_evr_privert;
class St_evr_evrpar;
class St_egr_propagate;
class St_egr_egrpar;

class St_svg_shape; 
class St_svg_config; 
class St_svg_geom ;
class St_srs_activea;
class St_srs_srspar;
class dst_vertex_st;

class StPrimaryMaker : public StMaker {
  
 private:
  Int_t            m_flag;       //
  St_evr_privert *m_evr_privert; //!  
  St_evr_evrpar  *m_evr_evrpar;  //!
  St_egr_propagate *m_tp_param;  //!
  St_egr_egrpar  *m_egr_egrpar;  //!
  St_egr_egrpar  *m_egr2_egrpar; //!
  dst_vertex_st  *m_fixedVertex; //!
 protected:
  
  
 public: 
  StPrimaryMaker(const char *name="primary");
  virtual       ~StPrimaryMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void  FixVertex(Float_t x=0, Float_t y=0, Float_t z=0);
  virtual void  UnFixVertex();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPrimaryMaker.h,v 1.7 1999/12/10 17:38:42 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StPrimaryMaker, 1)   //StAF chain virtual base class for Makers
    };
    
#endif
    
