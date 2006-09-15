/*!
 * \class St_sls_Maker
 * \author B.Hippolyte, Walter Pinganaud
 * \date 2000
 *
 *  Realistic simulation  for the Silicon Strip Detectors
 * 
 *  This maker controls the simulated response of the SSD to 
 *  particles. Geant hits are converted into signal on strips
 *  in the detector frame, inactive areas are taking into account.  
 *  Parameters of signal diffusion and induction have been extracted
 *  from beam test measurements.
 * 
 * See documentation at http://star.in2p3.fr/STAR_informatique/simulators.html#sls
 */
#ifndef STAR_St_sls_Maker
#define STAR_St_sls_Maker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_ssdDimensions;
class St_ssdWafersPosition;
class St_slsCtrl;

class St_sls_Maker : public StMaker {
 private:
  St_ssdDimensions *m_geom_par;//!
  St_ssdWafersPosition     *m_geom;//!
  St_slsCtrl     *m_ctrl;//!
 public: 
	          St_sls_Maker(const char *name="sls_strip");
   virtual       ~St_sls_Maker();
   virtual Int_t  Init();
   virtual Int_t  InitRun(Int_t runNumber);
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();

   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: St_sls_Maker.h,v 1.8 2006/09/15 21:09:52 bouchet Exp $ built "__DATE__" "__TIME__ ; return cvs;}



   ClassDef(St_sls_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 * $Id: St_sls_Maker.h,v 1.8 2006/09/15 21:09:52 bouchet Exp $
 *
 * $Log: St_sls_Maker.h,v $
 * Revision 1.8  2006/09/15 21:09:52  bouchet
 * read the noise and pedestal from ssdStripCalib
 *
 * Revision 1.7  2005/05/13 08:39:33  lmartin
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






