/*
  The purpose of this header is to represent the basic "pico" level information about FCS information from simulation and data. The other reason to keep all these classes separate is so that you can load this library into ROOT without any other STAR libraries.

  @[June 13, 2023](David Kapukchyan) > Copied from StFcsPicoTree.h and only left the relevant "pico" classes
  
  @[September 6, 2023](David Kapukchyan) > Changed "mLorentz" variables in StFcsPicoPoint to be just px, py, pz,and E. Also, changed "mLorentz" variables in StFcsPicoCluster to just px, py, pz, e since these weren't being set any way. NB:For both StFcsPicoCluster and StFcsPicoPoint *mEnergy* is the energy directly from the StFcsCluster and StFcsPoint object, mE is the one from the "fourmomentum()"

  @[September 18, 2023](David Kapukchyan) > Changed 'mNS' in *StFcsPicoPoint* to 'mDetId' along with where it is used. I also experimented with trying to create a function that will auto fill the data when given a *StFcsHit*, *StFcsCluster*, or *StFcsPoint* but realized that this will require the use of STAR headers and libraries and will break the simplicity of this data structure; simplicity of which is the goal of these classes. I did however leave them in since I may move these methods to a different class later. Also added a source file for this header where those functions were implemented

  @[October 10, 2023](David Kapukchyan) > Added a variable to StFcsPicoPoint that can be used to store the parent cluster index that corresponds to a TClonesArray of StFcsPicoClusters. Since cluster id is detector dependent, it will not always lead to the correct index.

  @[January 26, 2024](David Kapukchyan) > Added 'mClusterId' to *StFcsPicoHit* to keep track of cluster associated to a hit. This is not the most efficient way to do this since now you have to loop over all hits to find which ones are associated to a specific cluster and this will result in extra loops. Since this information will be used mostly in testing it is good enough for now.
  
  @[January 27, 2024](David Kapukchyan) > Added 'mXLocal' and 'mYLocal' to *StFcsPicoHit* to keep the x,y of the hit in local coordinates too since this is what is used for the point maker and I needed it for testing the shower shape.

  @[February 6, 2024](David Kapukchyan) > Fixed mChi2Ndf2Phoron to mChi2Ndf2Photon
 */

#ifndef StFcsPico_H
#define StFcsPico_H

//C/C++ Headers
#include <math.h>

//ROOT headers
#include "TObject.h"

//STAR headers
//#include "StFcsHit.h"
//#include "StFcsCluster.h"
//#include "StFcsPoint.h"

//A dummy class to hold just basic FCS hit information
class StFcsPicoHit : public TObject{
public:
  StFcsPicoHit() {}
  
  //virtual void Set(StFcsHit* hit, Double_t xpos=0, Double_t ypos=0, Double_t zpos=0);
  //virtual void Print() const;
  
  UShort_t mDetId = 0;
  UShort_t mChId = 0;
  UInt_t mAdcSum = 0;
  Float_t mEnergy = 0;
  Int_t mClusterId = -1;  //Store cluster Id this hit belongs to, each hit can only be a part of a single cluster

  //Local coordinates
  Float_t mXLocal = 0; //(Column space)
  Float_t mYLocal = 0; //(Row space)

  //STAR coordinates
  Float_t mXstar = 0;
  Float_t mYstar = 0;
  Float_t mZstar = 0;
  
  ClassDef(StFcsPicoHit,2);
};

//A dummy class to hold just basic FCS cluster information
class StFcsPicoCluster : public TObject{
public:
  StFcsPicoCluster(){}
  //virtual void Set(StFcsCluster* clus, Double_t px=0, Double_t py=0, Double_t px=0, Double_t e=0);
  //virtual void Print() const;
  
  Int_t mId = -1;  //Cluster Id
  UShort_t mDetId = 0;
  Int_t mCategory=0;
  Int_t mNTowers=0;
  Int_t mNNeighbor=0;
  Int_t mNPoints=0;
  Float_t mEnergy = 0;
  Float_t mX=0;         //This is local x (Column space)
  Float_t mY=0;         //This is local y (Row space)
  Float_t mSigmaMin=0;
  Float_t mSigmaMax=0;
  Float_t mTheta=0;
  Float_t mChi2Ndf1Photon=0;
  Float_t mChi2Ndf2Photon=0;

  //Lorentz 4 momentum of cluster
  Double_t mPx = 0;
  Double_t mPy = 0;
  Double_t mPz = 0;
  Double_t mE  = 0;

  ClassDef(StFcsPicoCluster,2);
};

//A dummy clas to hold just the basic FCS point information
class StFcsPicoPoint : public TObject{
public:
  StFcsPicoPoint(){}
  //virtual void Set(StFcsPoint* point, Double_t x=0, Double_t y=0, Double_t z=0, Double_t px=0, Double_t py=0, Double_t pz=0, Double_t e=0);
  //virtual void Print() const;

  UShort_t mDetId = 0;
  Float_t mEnergy = 0;
  Float_t mXlocal=0;
  Float_t mYlocal=0;
  Int_t mParentClusterId = -1;     //Counting does start from 0, but different detector ids can still have id=0
  Int_t mNParentClusterPhotons=-1;
  Int_t mClusterIndex = -1; //This can be used to associate a point with its corresponding cluster in a TClonesArray of clusters

  //STAR xyx
  Double_t mXstar = 0;
  Double_t mYstar = 0;
  Double_t mZstar = 0;

  //Lorentz 4 momentum of point
  Double_t mPx = 0;
  Double_t mPy = 0;
  Double_t mPz = 0;
  Double_t mE  = 0;

  ClassDef(StFcsPicoPoint,4);
};

//A dummy  class to hold just the basic G2t track information
class StPicoG2tTrack : public TObject
{
public:
  StPicoG2tTrack() {}

  long id = 0;
  long ge_pid = 0;
  double mPx = 0;
  double mPy = 0;
  double mPz = 0;
  double mE  = 0;
  double mEta = 0;
  double eta()  { if( mEta<0 ){ return asinh(mPz/pt()); }else{ return mEta; } }
  double phi()  { return atan2(mPy,mPx); }
  double pt()   { return sqrt( mPx*mPx + mPy*mPy ); }
  double ptot() { return sqrt( mPx*mPx + mPy*mPy + mPz*mPz ); }
  double theta(){ return 2.0*atan(exp(-1.0*eta())); }
  double mass() { return sqrt(mE*mE - ptot()*ptot()); }
  double mXProj = 0;
  double mYProj = 0;
  double mZProj = 0;

  ClassDef(StPicoG2tTrack,1);
};



#endif
