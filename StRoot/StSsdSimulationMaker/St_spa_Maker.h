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
class St_ssdStripCalib;
class St_sdm_condition_db;
class St_slsCtrl;
class slsCtrl_st;
class St_spa_Maker : public StMaker {
 private:
  St_ssdStripCalib     *m_noise;    //!
  St_sdm_condition_db  *m_condition;//!
  slsCtrl_st           *m_ctrl;//!
 public: 
  St_spa_Maker(const char *name="spa_strip");
  virtual       ~St_spa_Maker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   PrintInfo();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_spa_Maker.h,v 1.15 2014/08/06 11:43:43 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

   ClassDef(St_spa_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 * $Id: St_spa_Maker.h,v 1.15 2014/08/06 11:43:43 jeromel Exp $
 *
 * $Log: St_spa_Maker.h,v $
 * Revision 1.15  2014/08/06 11:43:43  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.14  2008/08/12 22:48:39  bouchet
 * retrieve positions and dimensions tables using Get methods
 *
 * Revision 1.13  2008/05/29 03:07:28  bouchet
 * remove inactive variables;fix a potential memory leak
 *
 * Revision 1.12  2008/04/15 21:04:43  bouchet
 * remove latest change
 *
 * Revision 1.11  2008/04/12 14:21:29  bouchet
 * Add a switch to use constant noise and pedestal
 *
 * Revision 1.10  2007/03/21 17:19:56  fisyak
 * use new StSsdBarrel
 *
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







