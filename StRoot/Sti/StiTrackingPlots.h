/*
 * $Id: StiTrackingPlots.h,v 2.8 2003/07/22 17:16:31 pruneau Exp $
 *
 *
 * $Log: StiTrackingPlots.h,v $
 * Revision 2.8  2003/07/22 17:16:31  pruneau
 * various
 *
 * Revision 2.7  2003/05/01 20:46:52  pruneau
 * changed error parametrization
 *
 * Revision 2.6  2003/04/29 18:48:34  pruneau
 * *** empty log message ***
 *
 * Revision 2.5  2003/04/04 14:44:25  pruneau
 * Fix to the hit error calculator and the getCharge methods.
 *
 * Revision 2.4  2003/03/31 17:19:02  pruneau
 * various
 *
 * Revision 2.3  2003/03/13 21:21:29  pruneau
 * getPhase() fixed. MUST inclde -helicity()*pi/2
 *
 * Revision 2.2  2003/03/13 18:59:16  pruneau
 * various updates
 *
 * Revision 2.1  2003/03/12 16:36:28  andrewar
 * Sti tracking plots package.
 *
 */
#ifndef StiTrackingPlots_H_INCLUDED
#define StiTrackingPlots_H_INCLUDED

class TH1D;
class TH2D;
class TH3D;
class TFile;
class TProfile;
class TProfile2D;

#include "Sti/Base/HistogramGroup.h"

class StiTrackingPlots : public HistogramGroup
{
 public:
  StiTrackingPlots();
  StiTrackingPlots(const string & name, const string & description);
  ~StiTrackingPlots();

  void initialize();
  void setOutFileName(string nme){mOutFile = nme;}
  void fill(StiTrackContainer* mTrackStore);
  void addFilter(StiDefaultTrackFilter *filter){mFilter=filter;}

 private:
  StiDefaultTrackFilter *mFilter;
  string mOutFile;

  TH1D *numTracks;
  TH1D * _eta;
  TH1D * _etaPlus;
  TH1D * _etaMinus;
  TH1D * _phi;
  TH1D * _phiPlus;
  TH1D * _phiMinus;
  TH1D * _pt;
  TH1D * _ptPlus;
  TH1D * _ptMinus;
  TH1D * _dca40;
  TH1D * _dca40Plus;
  TH1D * _dca40Minus;
  TH1D * _globalDca;
  TH1D * _globalDcaPlus;
  TH1D * _globalDcaMinus;

  TH2D *_nptsVsPt;
  TH2D *_nptsVsPtPlus;
  TH2D *_nptsVsPtMinus;
  TH2D *_nptsVsEtaPtGt200;
  TH2D *_nptsVsEtaPtLt200;

  TH2D *_nptsVsEta;
  TH2D *_nptsVsEtaPlus;
  TH2D *_nptsVsEtaMinus;

  TH2D *_nptsVsPhi;
  TH2D *_nptsVsPhiPlus;
  TH2D *_nptsVsPhiMinus;

  //rad length maps
  TH2D * _radLengthVsPhi;
  TH2D * _radLengthVsEta;
  TProfile * _radLengthVsPhiProf;
  TProfile * _radLengthVsEtaProf;

  TH2D *_xLastHitVsXLastNode;
  TH2D *_xLastHitVsXLastNode1;
  TH2D *_xLastHitVsXLastNode2;

  TH1D * _chi2;
  TH2D * _chi2VsNpts;

  //track kinematics & helix parameters
  //make all plots 3D - value,Phi,Eta - then cut 
  TH1D * mPx;

  TH3D * mCurv;
  TH3D * mHeli;
  TH3D * mMomX;
  TH3D * mMomY;
  TH3D * mMomZ;
  TH3D * mPhase;

  TH2D * _yVsXPosEta;
  TH2D * _yVsXNegEta;
};


#endif
