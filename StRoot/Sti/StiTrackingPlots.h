/*
 * $Id: StiTrackingPlots.h,v 2.1 2003/03/12 16:36:28 andrewar Exp $
 *
 *
 * $Log: StiTrackingPlots.h,v $
 * Revision 2.1  2003/03/12 16:36:28  andrewar
 * Sti tracking plots package.
 *
 */


class TH1D;
class TH2D;
class TH3D;
class TFile;

class StiTrackingPlots
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

  //rad length maps
  TH2D * radLengthZ;
  TH2D * radLengthPhi;
  TH2D * radLengthEta;

  //track kinematics & helix parameters
  //make all plots 3D - value,Phi,Eta - then cut 
  TH1D * mEta;
  TH1D * mPx;

  TH3D * mCurv;
  TH3D * mHeli;
  TH3D * mMomX;
  TH3D * mMomY;
  TH3D * mMomZ;
  TH3D * mPhase;

  TH1D * globalDca;

};
