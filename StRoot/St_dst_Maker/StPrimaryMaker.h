#ifndef STAR_StPrimaryMaker
#define STAR_StPrimaryMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StPrimaryMaker virtual base class for Maker                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_svm_ctrl;
class St_est_ctrl;
class St_evr_privert;
class St_evr_evrpar;
class St_egr_propagate;
class St_egr_egrpar;

class St_svg_shape; 
class St_svg_config; 
class St_svg_geom ;
class St_srs_activea;
class St_srs_srspar;

class StPrimaryMaker : public StMaker {

 private:
  Int_t            m_flag;       //
  St_evr_privert *m_evr_privert; //!  
  St_evr_evrpar  *m_evr_evrpar;  //!
  St_egr_propagate *m_tp_param;  //!
  St_egr_egrpar  *m_egr_egrpar;  //!
  St_egr_egrpar  *m_egr2_egrpar; //!
  
 protected:

  
 public: 
  StPrimaryMaker(const char *name="primary");
  virtual       ~StPrimaryMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  ClassDef(StPrimaryMaker, 1)   //StAF chain virtual base class for Makers
    };

#endif
