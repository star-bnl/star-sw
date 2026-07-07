/*
  The purpose of this StMaker is to understand the shower shape function used by StFcsPointMaker to develop improvements for it

  @[April 3, 2023](David Kapukchyan) > First instance
  
  @[April 14, 2023](David Kapukchyan) > Added a energy cut to both clusters and points
  
  @[September 18, 2023](David Kapukchyan) > Added some more histograms and started using the #LoadH1() and #LoadH2() functions in *Rtools* to create/load histograms. This way I can just copy the histograms and the #LoadHistograms() function to easily make plots in another macro. Also added the analysis histograms from 'anaShowerAna.cc' to here.

  @[October 10, 2023](David Kapukchyan) > Added the mClusterIndex variable into the stored StFcsPicoPoint. Added some more histograms from Fcs2019/FcsSim2023/anaShowerAna.cc. Also fixed is that invariant mass histograms, among a few others now use the parent track and not primary track to fill the appropriate histograms

  @[November 8, 2023](David Kapukchyan) > Fixed how I get the cluster mean position in StarXYZ. Added Taxicab and Chebeyshev distance functions. Also added histograms that will store hit energy vs. those distance functions. Also changed the range on some histograms to more accurately capture their true meaning.

  @[December 5, 2023](David Kapukchyan) > Added functions to get the column and row information from STAR xyz global coordinates. Essentially a global to local conversion but integer values for the column and row information. The idea was to see if distances in column and row space are better to use than the Euclidean one.

  @[February 1, 2024](David Kapukchyan) > Added mFile to help with reading ROOT files

  @[February 6, 2024](David Kapukchyan) > Added Chi2/NDF histograms and fixed mChi2Ndf2Phoron to mChi2Ndf2Photon

  @[February 20, 2024](David Kapukchyan) > Added histograms #mH3F_Showers and #mH2F_NhitsPerClus to understand shower shape. Added #mTestFile that will write the hit and cluster information as plain text to a file. This was needed to debug why cluster and hit information were not matching between the MuDST and #mDataTree. Fixed the bug that was causing this issue by forcing #StFcsPicoHit::mClusterId to be -1 after initializing.

  @[July 7, 2026](David Kapukchyan) > Changed #mDataTree to TTree and added TClonesArrays for hits, clusters, points, etc. to get rid of dependency on MyTools. Changed #mHistArr to mHistMan which uses the #HistManger in StSpinPool/StFwdData/. Changed loading histograms to be consistent with how #HistManager works. Added #InitRun() which is where checking if pointers were loaded should happen. Additional fixes and changes to no longer depend on MyTools. It compiles with cons now but haven't tried running it
 */

#ifndef StFcsShowerAnaMaker_H
#define StFcsShowerAnaMaker_H

//C/C++ headers
#include <algorithm>

//ROOT headers
#include "TFile.h"
#include "TLorentzVector.h"
#include "TString.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TTree.h"

//STAR headers
#include "StThreeVectorD.hh"
#include "StMaker.h"

//Custom Headers
#include "StSpinPool/StFwdData/StFcsPico.h"
#include "StSpinPool/StFwdData/HistManager.h"

class StFcsDb;
class StFcsCollection;

class StFcsShowerAnaMaker : public StMaker
{
  
 public:
  StFcsShowerAnaMaker(const char* name="StFcsShowerAnaMaker");
  ~StFcsShowerAnaMaker();
  
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t runnumber);
  virtual Int_t Make();
  virtual Int_t Finish();

  void setFileName(const char* name){ mFileName = name; }    ///< set file name that #Finish() will use to write to a file
  void setEnCut( double value )  { mEnCut = value; }

  //static void ReadHists(const char* filename); ///< Load from file all the histograms??
  //void WriteHists(const char* filename, const char* mode="RECREATE");      ///< Write all histograms to a file

  UInt_t LoadHistograms(TFile* file, HistManager* histman);

  static Double_t DistStThreeVecD(StThreeVectorD &vec1, StThreeVectorD &vec2);
  static Double_t TaxiDistStThreeVecD(StThreeVectorD &vec1, StThreeVectorD &vec2);  ///< Taxicab (Manhattan) distance of two StThreeVectors D=Sum_i^n|vec1_i-vec2_i|
  static Double_t ChebDistStThreeVecD(StThreeVectorD &vec1, StThreeVectorD &vec2);  ///< Chebyshev distance of two StThreeVectors D=max(vec1-vec2)
  StThreeVectorD starXYZtoLocal(int det, const StThreeVectorD &xyz, double showermaxz=-1);
  void starXYZtoColRow(int det, const StThreeVectorD &xyz, Int_t &col, Int_t &row, double showermaxz=-1);
  
 protected:
  StFcsDb* mFcsDb = 0;               ///< FCS db object
  StFcsCollection* mFcsColl = 0;     ///< FCS collection of hits, clusters, and points

  TString mFileName = "";        ///< name of output file name
  
  //Histograms
  TH1* mH1F_PointXLocal = 0;    ///< Histogram of point local x value but only the decimal value is stored. Local means that deltax=1 is equivalent to 1 cell, e.g. x=3.4 means 3 towers away from beam pipe plus 40% extra into that tower
  TH1* mH1F_PointYLocal = 0;    ///< Histogram of point local y value but only the decimal value is stored. Local means that deltay=1 is equivalent to 1 cell, e.g. y=2.6 means 2 towers from bottom plus 60% extra into that tower
  TH1* mH2F_PointLocalyVx = 0;  ///< 2D histogram of decimal point local y value vs. decimal point local y value, i.e. PointYLocal vs. PointXLocal
  TH1* mH1F_Chi2Ndf1Photon = 0; ///< 1D histogram of Chi^2/NDF from Point Maker fit of 1 Photon
  TH1* mH1F_Chi2Ndf2Photon = 0; ///< 1D histogram of Chi^2/NDF from Point Maker fit of 2 Photon

  TH1* mH2F_PointXProjX = 0;    //! Histogram of reconstructed point x-value vs. track projected x-value
  TH1* mH2F_PointYProjY = 0;    //! Histogram of reconstructed point y-value vs. track projected y-value

  TH1* mH2F_hiteVtrkdist = 0;   ///< Histogram of hit energy vs. distance from parent track
  TH1* mH2F_hiteVtrktaxid = 0;  ///< Histogram of hit energy vs. Taxicab distance from parent track
  TH1* mH2F_hiteVtrkchebd = 0;  ///< Histogram of hit energy vs. Chebyshev distance from parent track
  TH1* mH2F_TrkhitfracVdist = 0;///< Histogram of fraction of track energy in a hit vs. distance to parent track

  TH1* mH1F_ClusSigMax = 0;     ///< Histogram of cluster sigma max
  TH1* mH1F_ClusSigMin = 0;     ///< Histogram of cluster sigma min
  TH1* mH2F_ClusSigMaxEn = 0;   ///< Histogram of cluster sigma max vs. cluster energy
  TH1* mH2F_ClusSigMinEn = 0;   ///< Histogram of cluster sigma min vs. cluster energy
  TH1* mH2F_clusmeanyVx = 0;    ///< Histogram of cluster mean y vs. x using STAR XYZ coordinates
  TH1* mH1F_ClusMeanDTrk = 0;   ///< Histogram of distance between cluster mean and parent track projected onto Fcs Shower Max Z
  //TH1* mH2F_cluseVclusmeantrkd = 0;   ///< Histogram of distance between cluster mean and parent track projected onto Fcs Shower Max Z
  //TH1* mH1F_ClusMeanXTrkX = 0;  ///< Histogram of cluster x mean from STAR XYZ coordinates minus parent track X
  //TH1* mH1F_ClusMeanYTrkY = 0;  ///< Histogram of cluster y mean from STAR XYZ coordinates minus parent track Y

  TH1* mH1F_primid = 0;         ///< Histogram of primary track geant id
  TH1* mH1F_parentid = 0;       ///< Histogram of parent track geant id
  TH1* mH1F_NParClusPhotons = 0;///< Histogram of point NParentClusterPhotons
  TH1* mH2F_npoiVnclus = 0;     ///< Histogram of number of points vs number of clusters
  TH1* mH2F_cluseVlore = 0;     ///< Histogram of cluster energy vs. cluster lorentz energy (This was to check if cluser->energy() and cluster->fourmomentum()->e() give same result which they do)
  TH1* mH2F_trkeVpoie = 0;      ///< Histogram of primary track energy vs. point energy
  TH1* mH1F_invmasspoi = 0;     ///< Histogram of invariant mass with two highest energy points
  TH1* mH1F_invmasstrk = 0;     ///< Histogram of invariant mass of two highest tracks
  TH1* mH1F_dpoitrk = 0;        ///< Histogram of magnitude of difference between reconstructed point and track
  TH1* mH2F_massVdgg = 0;       ///< Histogram of 2 highest energy points invariant vs. d_{gg} the distance between them
  TH1* mH2F_trkmassVdgg = 0;    ///< Histogram of 2 tracks corresponding to 2 highest energy points invariant mass vs. d_{gg} the distance between them

  TH1* mH2F_parprojyVprojx = 0; ///< Histogram of parent tracks' x,y projected onto FCS planes
  TH1* mH1F_NPrimTrks = 0;      ///< Histogram of number of primary tracks/event
  TH1* mH1F_NParTrks = 0;       ///< Histogram of number of parent tracks/event

  TH1* mH3F_Showers = 0;        ///< Histogram of hit local x,y,energy where the x,y is shifted by the cluster center and the energy is normalized by the total cluster energy. Only ecal clusters.
  TH1* mH2F_NhitsPerClus = 0;   ///< 2D Histogram of number of hits per cluster vs. detector id

  TObjArray* mHC2F_PointLocalyVx = 0; ///< 2D Histogram collection of PointLocalXvY for different eta and phi bins of reconstructed points. 8 bins in Phi (pi/4 width), and 6 bins in eta (width 0.3) starting with 2.4

private:
  double mEnCut = -1;   ///< energy to cut on for both clusters and points

  //Rtools::ClonesArrTree* mDataTree = 0; ///< Tree to hold information
  TTree* mDataTree = 0; ///< Tree to hold information
  HistManager* mHistMan = 0;             ///< Histogram manager to hold

  TClonesArray* mFcsHitsArr    = 0;//mDataTree->GetBranchArr("FcsHit");
  TClonesArray* mFcsClusArr    = 0;//mDataTree->GetBranchArr("FcsCluster");
  TClonesArray* mFcsPointArr   = 0;//mDataTree->GetBranchArr("FcsPoint");
  TClonesArray* mG2tPrimArr = 0;//mDataTree->GetBranchArr("G2tPrim");
  TClonesArray* mG2tParArr  = 0;//mDataTree->GetBranchArr("G2tParent");

  //std::ofstream* mTestFile = 0;
  
  ClassDef(StFcsShowerAnaMaker,3);
};

#endif
