/*
 * $Id: StiTrackingPlots.h,v 2.3 2003/03/13 21:21:29 pruneau Exp $
 *
 *
 * $Log: StiTrackingPlots.h,v $
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
  ~StiTrackingPlots();

  void setOutFileName(string nme){mOutFile = nme;}
  void writeHists();
  void writeHists(TFile *outFile);
  void fillStandardPlots(StiTrackContainer* mTrackStore);
  void addFilter(StiDefaultTrackFilter *filter){mFilter=filter;}

 private:
  StiDefaultTrackFilter *mFilter;
  string mOutFile;

  TH1D *numTracks;
  TH1D * _eta;
  TH1D * _phi;
  TH1D * _pt;
  TH1D * _dca40;

  //rad length maps
  TH2D * radLengthZ;
  TH2D * radLengthPhi;
  TH2D * radLengthEta;

  //track kinematics & helix parameters
  //make all plots 3D - value,Phi,Eta - then cut 
  TH1D * mPx;

  TH3D * mCurv;
  TH3D * mHeli;
  TH3D * mMomX;
  TH3D * mMomY;
  TH3D * mMomZ;
  TH3D * mPhase;

  TH1D * globalDca;

};


#endif
