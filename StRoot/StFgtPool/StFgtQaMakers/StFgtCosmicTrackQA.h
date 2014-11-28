/***************************************************************************
 *
 * $Id: StFgtCosmicTrackQA.h,v 1.1 2012/01/31 09:26:16 sgliske Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 20 2011 
 *
 ***************************************************************************
 *
 * Description:  input QA for creating cosmictracks
 *
 ***************************************************************************
 *
 * $Log: StFgtCosmicTrackQA.h,v $
 * Revision 1.1  2012/01/31 09:26:16  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.7  2011/12/07 17:19:59  ckriley
 * minor update
 *
 * Revision 1.6  2011/11/25 20:20:04  ckriley
 * added statusmaker functionality
 *
 * Revision 1.5  2011/11/09 21:03:20  ckriley
 * working version with current containers
 *
 * Revision 1.4  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.2  2011/10/25 20:43:32  ckriley
 * added more plots
 *
 * Revision 1.1  2011/10/25 15:41:53  ckriley
 * creation of StFgtCosmicTrackQA
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_COSMIC_TRACK_QA_
#define _ST_FGT_COSMIC_TRACK_QA_

#include <string>
#include <TH1F.h>
#include <TH2F.h>
#include <TProfile.h>
#include "StFgtQaMaker.h"

class StFgtCosmicTrackQA : public StFgtQaMaker {
 public:
   // constructors
   StFgtCosmicTrackQA( const Char_t* name = "FGT_TrackQA_Residual",
                       Short_t discId = 0,
                       Short_t quadId = 0,
                       const Char_t* quadName = "000" );
   StFgtCosmicTrackQA(const StFgtCosmicTrackQA&);

   // deconstructor
   ~StFgtCosmicTrackQA();

   // equals operator
   StFgtCosmicTrackQA& operator=(const StFgtCosmicTrackQA&);

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   void Clear( Option_t *opt = "" );

   // modifiers
   void setFilenameBase( const Char_t* filenameBase );
   void setFilenameKey( const Char_t* filenameKey );
   void setPath( const Char_t* path );

   // accessor
   TH1F* getHistAPV(int);
   TH1F* getHist1Dcluster(int);
   TH1F* getHist1Dclusters();
   TH2F *getHist2Dpoint();

   TH1F* getTestHist(int);

 protected:
   // for saving the plot
   std::string mPath, mFileNameBase, mFileNameKey;

   TH1F *mHistAPV[10];            // adc-ped histograms for all APV chips
   TH1F *mHist1Dcluster[2];       // 1D cluster energy histograms for r & phi
   TH1F *mHist1Dclusters;         // 1D cluster energy histograms
   TH2F *mHist2Dpoint;            // cluster points histogram

   TH1F *testHist[2];             // testing hot channels

 private:   

   ClassDef(StFgtCosmicTrackQA,1);
}; 

// inline functions

// modifiers
inline void StFgtCosmicTrackQA::setFilenameBase( const Char_t* filenameBase ){ mFileNameBase = filenameBase; };
inline void StFgtCosmicTrackQA::setFilenameKey( const Char_t* filenameKey ){ mFileNameKey = filenameKey; };
inline void StFgtCosmicTrackQA::setPath( const Char_t* path ){ mPath = path; };

// accessor
inline TH1F* StFgtCosmicTrackQA::getHistAPV( int i ){
   return mHistAPV[i];
};
inline TH1F* StFgtCosmicTrackQA::getHist1Dcluster( int i ){
   return mHist1Dcluster[i];
};
inline TH1F* StFgtCosmicTrackQA::getHist1Dclusters(){
   return mHist1Dclusters;
};
inline TH2F* StFgtCosmicTrackQA::getHist2Dpoint(){
   return mHist2Dpoint;
};
inline TH1F* StFgtCosmicTrackQA::getTestHist( int i ){
   return testHist[i];
};


#endif
