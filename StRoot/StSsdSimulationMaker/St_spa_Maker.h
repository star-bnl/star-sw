#ifndef STAR_St_spa_Maker
#define STAR_St_spa_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_spa_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_sdm_condition_par;
class St_sdm_geom_par;
class St_sdm_calib_par;
class St_sdm_calib_db;
class St_sdm_condition_db;
class St_sls_ctrl;

class St_spa_Maker : public StMaker {
 private:
  St_sdm_condition_par *m_cond_par;//!
  St_sdm_geom_par      *m_geom_par;//!
  St_sdm_calib_par     *m_cal_par;//!
  St_sdm_calib_db      *m_noise;//!
  St_sdm_condition_db  *m_condition;//!
  St_sls_ctrl          *m_ctrl;//!
 public: 
                  St_spa_Maker(const char *name="spa_strip");
   virtual       ~St_spa_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();
   ClassDef(St_spa_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif







