/*
 * $Id: StiTrackingPlots.h,v 2.16 2005/01/17 03:43:23 pruneau Exp $
 *
 *
 * $Log: StiTrackingPlots.h,v $
 * Revision 2.16  2005/01/17 03:43:23  pruneau
 * change track container to vector
 *
 * Revision 2.15  2004/12/23 15:07:19  pruneau
 * added point of closest approach to vertex (pca) histos
 *
 * Revision 2.14  2004/12/11 22:21:02  pruneau
 * new histos
 *
 * Revision 2.13  2004/12/01 14:02:09  pruneau
 * svt
 *
 * Revision 2.12  2004/11/22 19:21:49  pruneau
 * revamped the plotting package
 *
 * Revision 2.11  2004/11/10 21:45:11  pruneau
 * added diagnostic plots
 *
 * Revision 2.10  2003/11/14 22:31:57  andrewar
 * Added isPrimary() cut so only Primary tracks are histogrammed
 * (not a mix of Primary and global).
 *
 * Revision 2.9  2003/07/30 19:19:24  pruneau
 * sigh
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
#include "Sti/Base/HistogramGroup.h"

class StiTrackingPlots : public HistogramGroup
{
 public:
  StiTrackingPlots();
  StiTrackingPlots(const string & name, const string & description);
  ~StiTrackingPlots();

  void initialize();
  void setOutFileName(string nme){mOutFile = nme;}
  void fill(StiTrackContainer* mTrackStore, StiHit * vertex=0);
  void addFilter(StiDefaultTrackFilter *filter){mFilter.push_back(filter);}

 private:
  vector<StiDefaultTrackFilter *> mFilter;
  string mOutFile;

#define NPLOTS 9

  TH1D * _track[NPLOTS];
  TH2D * _track2D[NPLOTS][NPLOTS];

  TH1D * _eta[NPLOTS];
  TH1D * _phi[NPLOTS];
  TH1D * _pt[NPLOTS];
  TH1D * _dca[NPLOTS];
  TH1D * _gdca[NPLOTS];
  TH2D * _nptsVsPt[NPLOTS];
  TH2D * _nptsVsEta[NPLOTS];
  TH2D * _nptsVsPhi[NPLOTS];
  TH2D * _nFitVsPt[NPLOTS];
  TH2D * _nFitVsEta[NPLOTS];
  TH2D * _nFitVsPhi[NPLOTS];
  TH2D * _nFitVsN[NPLOTS];
  TH2D * _nFitVsNSvt[NPLOTS];
  TH1D * _chi2[NPLOTS];
  TH2D * _chi2VsNpts[NPLOTS];
  TH2D * _chi2VsDca[NPLOTS];
  TH2D * _xLastHitVsXLastNode[NPLOTS];
  TH2D * radLengthZ[NPLOTS];
  TH2D * radLengthPhi[NPLOTS];
  TH2D * radLengthEta[NPLOTS];
  TH1D * _chi2Inc[NPLOTS][51];
  TH2D * _chi2IncVsDca[NPLOTS][51];
  TH1D * _yPull[NPLOTS][51];
  TH1D * _zPull[NPLOTS][51];
  TH1D * _dx[NPLOTS][51];
  TH1D * _dy[NPLOTS][51];
  TH1D * _dz[NPLOTS][51];
  TH2D * _dyVsTanCA[NPLOTS][51][12];
  TH2D * _dyVsY[NPLOTS][51][12];
  TH2D * _dyVsZ[NPLOTS][51][12];
  TH2D * _dzVsTanL[NPLOTS][51][12];
  TH2D * _dzVsY[NPLOTS][51][12];
  TH2D * _dzVsZ[NPLOTS][51][12];
  TH2D * _svtNhitVsNode[NPLOTS];
  TH2D * _svtNfitVsNode[NPLOTS];

  TH2D * _pcaxy[NPLOTS];
  TH2D * _pcazt[NPLOTS];

  //track kinematics & helix parameters
  //make all plots 3D - value,Phi,Eta - then cut 
  TH1D * mPx;
  TH3D * mCurv;
  TH3D * mHeli;
  TH3D * mMomX;
  TH3D * mMomY;
  TH3D * mMomZ;
  TH3D * mPhase;
  TH3D * mGDcavNptsvEtaA;
  //TH3D * mPDcavNptsvEtaA;
  TH3D * mGDcavNptsvPtA;
  //TH3D * mPDcavNptsvPtA;
  TH3D * mNptsvPtvEtaA;
  TH3D * mGDcavEtavPtA;
  //TH3D * mPDcavEtavPtA;
  TH3D * mGDcavNptsvEtaP;
  //TH3D * mPDcavNptsvEtaP;
  TH3D * mGDcavNptsvPtP;
  //TH3D * mPDcavNptsvPtP;
  TH3D * mNptsvPtvEtaP;
  TH3D * mGDcavEtavPtP;
  //TH3D * mPDcavEtavPtP;
  TH3D * mGDcavNptsvEtaM;
  //TH3D * mPDcavNptsvEtaM;
  TH3D * mGDcavNptsvPtM;
  //TH3D * mPDcavNptsvPtM;
  TH3D * mNptsvPtvEtaM;
  TH3D * mGDcavEtavPtM;
  //TH3D * mPDcavEtavPtM;

};


#endif
