/*
 * $Id: StiTrackingPlots.h,v 2.12 2004/11/22 19:21:49 pruneau Exp $
 *
 *
 * $Log: StiTrackingPlots.h,v $
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
  void fill(StiTrackContainer* mTrackStore);
  void addFilter(StiDefaultTrackFilter *filter){mFilter.push_back(filter);}

 private:
  vector<StiDefaultTrackFilter *> mFilter;
  string mOutFile;

  TH1D * _track[5];
  TH2D * _track2D[5][5];

  TH1D * _eta[5];
  TH1D * _phi[5];
  TH1D * _pt[5];
  TH1D * _dca[5];
  TH1D * _gdca[5];
  TH2D * _nptsVsPt[5];
  TH2D * _nptsVsEta[5];
  TH2D * _nptsVsPhi[5];
  TH2D * _nFitVsPt[5];
  TH2D * _nFitVsEta[5];
  TH2D * _nFitVsPhi[5];
  TH2D * _nFitVsN[5];
  TH2D * _nFitVsNSvt[5];
  TH1D * _chi2[5];
  TH2D * _chi2VsNpts[5];
  TH2D * _chi2VsDca[5];
  TH2D * _xLastHitVsXLastNode[5];
  TH2D * radLengthZ[5];
  TH2D * radLengthPhi[5];
  TH2D * radLengthEta[5];
  TH1D * _chi2Inc[5][51];
  TH2D * _chi2IncVsDca[5][51];
  TH1D * _yPull[5][51];
  TH1D * _zPull[5][51];
  TH1D * _dx[5][51];
  TH1D * _dy[5][51];
  TH1D * _dz[5][51];
  TH2D * _dyVsTanCA[5][51][12];
  TH2D * _dzVsTanL[5][51][12];


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
