/***************************************************************************
 *
 * $Id: StFgtCosmicTrackPlots.h,v 1.2 2012/01/31 23:37:17 avossen Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 18 2011 
 *
 ***************************************************************************
 *
 * Description:  Plotting class for different histograms concerning 
 * cosmicstand tracks
 *
 ***************************************************************************
 *
 * $Log: StFgtCosmicTrackPlots.h,v $
 * Revision 1.2  2012/01/31 23:37:17  avossen
 * fixed StFgtCosmicTrackMaker paths
 *
 * Revision 1.1  2012/01/31 23:25:35  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.1  2012/01/17 20:03:06  sgliske
 * moved from StFgtQaMakers
 *
 * Revision 1.10  2011/12/07 17:19:59  ckriley
 * minor update
 *
 * Revision 1.9  2011/11/25 20:20:04  ckriley
 * added statusmaker functionality
 *
 * Revision 1.8  2011/11/09 21:03:20  ckriley
 * working version with current containers
 *
 * Revision 1.7  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.5  2011/10/25 20:43:32  ckriley
 * added more plots
 *
 * Revision 1.4  2011/10/25 15:40:39  ckriley
 * more plots
 *
 * Revision 1.3  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_COSMIC_TRACK_PLOTS_
#define _ST_FGT_COSMIC_TRACK_PLOTS_

#include <string>
#include <TH1F.h>
#include <TH2F.h>
#include <TProfile.h>
#include <TProfile2D.h>
#include "StMaker.h"
#include "StRoot/StFgtPool/StFgtCosmicTrackMaker/StFgtCosmicTrack.h"

class StFgtCosmicTrackPlots : public StMaker {
 public:
   // constructors
   StFgtCosmicTrackPlots( const Char_t* name = "FGT_TrackQA_Residual",
                          const Char_t* cosmicTrackerName = "fgtCosmicTrackMaker",
                          Short_t discId = 0,
                          Short_t quadId = 0,
                          const Char_t* quadName = "000" );
   StFgtCosmicTrackPlots(const StFgtCosmicTrackPlots&);

   // deconstructor
   ~StFgtCosmicTrackPlots();

   // equals operator
   StFgtCosmicTrackPlots& operator=(const StFgtCosmicTrackPlots&);

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   void Clear( Option_t *opt = "" );

   // modifiers
   void setDisc( Short_t discId );
   void setQuad( Short_t quadId );
   void setQuadName( const Char_t* quadName );
   void setXbins( Int_t nbins );
   void setXrange( Float_t low, Float_t high );
   void setBinFactors( Int_t factorX );
   void setFilenameBase( const Char_t* filenameBase );
   void setFilenameKey( const Char_t* filenameKey );
   void setPath( const Char_t* path );
   void setRand( Float_t rndm=1 );

   // accessor
   TH1F* getHistX(int,int);
   TH1F* getHistY(int,int);
   TH1F* getHist1Dcluster(int);
   TH1F* getHist1Dclusters();
   TH2F* getHistTracks();
   TH2F* getHistRealTracks();
   TProfile2D* getProfileEfficiency();
   TH2F* getHist2Dcluster();
   TProfile2D* getProfileChi2();
   TProfile* getProfileX();
   TProfile* getProfileY();

 protected:
   // for accessing the data
   std::string mFgtTrackMakerName;
   StFgtCosmicTrackVec* mTrackVecPtr;

   // for knowing what & how to plot
   Short_t mDiscId, mQuadId;
   Int_t mXbins;
   Float_t mXmin, mXmax;
   Int_t mBinFactorX;

   // for labeling the plot
   std::string mQuadName;

   // for saving the plot
   std::string mPath, mFileNameBase, mFileNameKey;

   // the plots
   TH1F *mHistX[4][4];          // 1D deltaX histograms
   TH1F *mHistY[4][4];          // 1D deltaY histograms
   TH1F *mHist1Dcluster[2];     // 1D cluster energy histograms for r & phi
   TH1F *mHist1Dclusters;       // 1D cluster energy histograms
   TH2F *mHistTracks;           // reconstructed tracks on middle quad
   TH2F *mHistRealTracks;       // reconstructed registered tracks on mid quad
   TH2F *mHist2Dpoint;          // for calculating efficiency
   TProfile2D *mProfileEfficiency; // efficiency plots
   TH2F *mHist2Dcluster;        // cluster points hist of isTrack=True tracks
   TProfile2D *mProfileChi2;    // chi2 profile on middle quad
   TProfile *mProfileX;
   TProfile *mProfileY;

 private:   
  
   ClassDef(StFgtCosmicTrackPlots,1);
}; 

// inline functions

// modifiers
inline void StFgtCosmicTrackPlots::setDisc( Short_t discId ){ mDiscId = discId; };
inline void StFgtCosmicTrackPlots::setQuad( Short_t quadId ){ mQuadId = quadId; };
inline void StFgtCosmicTrackPlots::setQuadName( const Char_t* quadName ){ mQuadName = quadName; };
inline void StFgtCosmicTrackPlots::setXbins( Int_t nbins ){ mXbins = nbins; };
inline void StFgtCosmicTrackPlots::setXrange( Float_t low, Float_t high ){ mXmin = low; mXmax = high; };
inline void StFgtCosmicTrackPlots::setBinFactors( Int_t factorX ){
   mBinFactorX = factorX;
};
inline void StFgtCosmicTrackPlots::setFilenameBase( const Char_t* filenameBase ){ mFileNameBase = filenameBase; };
inline void StFgtCosmicTrackPlots::setFilenameKey( const Char_t* filenameKey ){ mFileNameKey = filenameKey; };
inline void StFgtCosmicTrackPlots::setPath( const Char_t* path ){ mPath = path; };

// accessor
inline TH1F* StFgtCosmicTrackPlots::getHistX(int i, int j){
   return mHistX[i][j];
};
inline TH1F* StFgtCosmicTrackPlots::getHistY(int i, int j){
   return mHistY[i][j];
};
inline TH1F* StFgtCosmicTrackPlots::getHist1Dcluster( int i ){
   return mHist1Dcluster[i];
};
inline TH1F* StFgtCosmicTrackPlots::getHist1Dclusters(){
   return mHist1Dclusters;
};
inline TH2F* StFgtCosmicTrackPlots::getHistTracks(){
   return mHistTracks;
};
inline TH2F* StFgtCosmicTrackPlots::getHistRealTracks(){
   return mHistRealTracks;
};
inline TProfile2D* StFgtCosmicTrackPlots::getProfileEfficiency(){
   return mProfileEfficiency;
};
inline TH2F* StFgtCosmicTrackPlots::getHist2Dcluster(){
   return mHist2Dcluster;
};
inline TProfile2D* StFgtCosmicTrackPlots::getProfileChi2(){
   return mProfileChi2;
};
inline TProfile* StFgtCosmicTrackPlots::getProfileX(){
   return mProfileX;
};
inline TProfile* StFgtCosmicTrackPlots::getProfileY(){
   return mProfileY;
};

#endif
