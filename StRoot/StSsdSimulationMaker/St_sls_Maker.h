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
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StSsdUtil/StSsdBarrel.hh"
#include "StSsdUtil/StSsdWafer.hh"
#endif
class St_g2t_ssd_hit;
class St_g2t_svt_hit;
class St_sls_strip;
class slsCtrl_st;
class St_slsCtrl;
class St_g2t_track;
class St_ssdDimensions;
class St_ssdWafersPosition;
class StMcEvent;
class StDAQReader;
class StMcSsdHit;

class St_sls_Maker : public StMaker {
  private :

  slsCtrl_st           *m_ctrl;//!
  St_ssdDimensions     *m_dimensions;//!
  St_ssdWafersPosition *m_positions; //!
  double mBField; // z component of BField; 
  StMcSsdHit *mHit;//!
 public: 
 St_sls_Maker(const char *name="sls_strip");   
  virtual       ~St_sls_Maker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   PrintInfo();
  virtual void   Clear(const char *opt);
  Int_t          readPointFromTable(St_g2t_ssd_hit *g2t_ssd_hit);
  Int_t          readPointFromTable(St_g2t_svt_hit *g2t_svt_hit) {return readPointFromTable((St_g2t_ssd_hit *) g2t_svt_hit);}
  Int_t          removeInactiveHitInTable(St_g2t_ssd_hit *g2t_ssd_hit);
  Int_t          removeInactiveHitInTable(St_g2t_svt_hit *g2t_svt_hit) {return removeInactiveHitInTable((St_g2t_ssd_hit *) g2t_svt_hit);}
  void           chargeSharingOverStrip(slsCtrl_st  *ctrl);
  Int_t          writeStripToTable(St_sls_strip *sls_strip);
  Int_t          readPointFromTableWithEmbedding(St_g2t_ssd_hit *g2t_ssd_hit,St_g2t_track *g2t_track,Int_t N,ssdWafersPosition_st *positions);
  void           setSsdParameters(ssdDimensions_st *geom_par);  
  void           printSsdParameters();  
  Int_t          idWaferToWaferNumb(Int_t idWafer); //  idwafer = layer*1000+waf*100+ladder => waferNumb = mNWaferPerLadder*(ladder-1) + waf - 1
  Int_t          idWaferToLadderNumb(Int_t idWafer);//  idwafer => ladder-1
  Int_t          waferNumbToIdWafer(Int_t waferNumb);// waferNumb = mNWaferPerLadder*(ladder-1) + waf - 1 => idwafer
  Int_t          idWaferToWafer(Int_t idWafer) {return (idWafer-7000)/100-1;}
  Int_t          ideal2RealTranslation(StThreeVector<double> *pos, StThreeVector<double> *mtm, double charge, int wafId, int index, ssdWafersPosition_st  *positions,Int_t *IL, Int_t *IW);
  int            IsOnWafer(const StThreeVector<double>& LocalPosition);
  void           debugUnPeu(); 
  
  private :
  Int_t    mSsdLayer;
  Int_t    mNLadder;
  Int_t    mNWaferPerLadder;
  Int_t    mNStripPerSide;
  Int_t    mActiveLadders[20];
  Float_t  mDetectorLargeEdge;
  Float_t  mDetectorSmallEdge;
  Float_t  mStripPitch;
  Float_t  mTheta;
  Int_t    *counter;
  TH1F     *hRejected;
  ssdDimensions_st *mDimensions;
   protected:
  StMcEvent            *mcEvent;
  Int_t                N;
  ssdWafersPosition_st *positions;

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_sls_Maker.h,v 1.14 2014/08/06 11:43:43 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}



  ClassDef(St_sls_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 * $Id: St_sls_Maker.h,v 1.14 2014/08/06 11:43:43 jeromel Exp $
 *
 * $Log: St_sls_Maker.h,v $
 * Revision 1.14  2014/08/06 11:43:43  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.13  2008/08/12 22:48:38  bouchet
 * retrieve positions and dimensions tables using Get methods
 *
 * Revision 1.12  2008/05/29 03:07:27  bouchet
 * remove inactive variables;fix a potential memory leak
 *
 * Revision 1.11  2008/05/07 22:59:11  bouchet
 * EmbeddingMaker:initial version ; modified reading of GEANT hits
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






