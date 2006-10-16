/*!
 * \class St_spa_Maker
 * \author B.Hippolyte, W.Pinganaud 
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
class St_ssdDimensions;
class St_sdm_calib_par;
class St_sdm_calib_db;
class St_sdm_condition_db;
class St_slsCtrl;
class St_ssdStripCalib;
class StSsdBarrel;
class ssdConfiguration_st;

class St_spa_Maker : public StMaker {
 private:
  St_sdm_condition_par *m_cond_par; //!
  St_ssdDimensions     *m_geom_par; //!
  St_sdm_calib_par     *m_cal_par;  //!
  St_ssdStripCalib     *m_noise;    //!
  St_sdm_condition_db  *m_condition;//!
  St_slsCtrl           *m_ctrl;     //!
  ssdConfiguration_st  *m_config;   //!
  StSsdBarrel          *mySsd;      //!
 public: 
                  St_spa_Maker(const char *name="spa_strip");
   virtual       ~St_spa_Maker();
   virtual Int_t  Init();
   virtual Int_t  InitRun(Int_t runumber);
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();

    virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_spa_Maker.h,v 1.9 2006/10/16 16:36:08 bouchet Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_spa_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 * $Id: St_spa_Maker.h,v 1.9 2006/10/16 16:36:08 bouchet Exp $
 *
 * $Log: St_spa_Maker.h,v $
 * Revision 1.9  2006/10/16 16:36:08  bouchet
 * Unify classes : Remove StSlsStrip, StSlsPoint, StSpaStrip, StSpaNoise by the same classes used in StSsdPointMaker (StSsdStrip,StSsdPoint) ; The methods for these classes are in StSsdUtil
 *
 * Revision 1.8  2006/09/15 21:09:52  bouchet
 * read the noise and pedestal from ssdStripCalib
 *
 * Revision 1.7  2005/05/13 08:39:34  lmartin
 * CVS tags added
 *
 * Revision 1.6  2003/10/08 03:46:34  suire
 * *** empty log message ***
 *
 * Revision 1.3  2002/03/25 20:06:44  suire
 * Doxygen documentation, cleaning
 *
 *
 **************************************************************************/







