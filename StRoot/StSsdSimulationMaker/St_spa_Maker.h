/*!
 * \class St_spa_Maker
 * \author B.Hippolyte, Walter Pinganaud 
 * \date 2000
 *
 *  Daq simulation for the Silicon Strip Detectors
 * 
 *  This maker controls the DAQ simulation :
 *  Simulated signals on all strips (491520) are read and a 
 *  noise is randomly (read from sdm datadbase) added to each one.
 *  Pedestal substraction and DAQ cut are applied, after that a table 
 *  is created with all the strip remaining.  
 * 
 * See documentation at http://star.in2p3.fr/STAR_informatique/daq.html
 */
#ifndef STAR_St_spa_Maker
#define STAR_St_spa_Maker

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

   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: St_spa_Maker.h,v 1.4 2003/04/30 20:38:43 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_spa_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 *
 * $Log: St_spa_Maker.h,v $
 * Revision 1.4  2003/04/30 20:38:43  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.3  2002/03/25 20:06:44  suire
 * Doxygen documentation, cleaning
 *
 *
 **************************************************************************/







