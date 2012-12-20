///
#ifndef _ST_FGT_STRAIGHT_TRACK_MAKER_
#define _ST_FGT_STRAIGHT_TRACK_MAKER_


#include "StMaker.h"
//#include "StFgtQaMaker.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TF1.h>
#include "StFgtGeneralBase.h"
#include "StFgtGenAVEMaker.h"
//#include "StRoot/StEvent/StFgtCollection.h"


class StFgtCollection;

class StFgtStraightTrackMaker : public StMaker {
 public:
  void SetEffDisk(Int_t disk);
  StFgtStraightTrackMaker(const Char_t* name="FgtStraightTrackMaker");
  pair<Double_t,Double_t> findCluChargeSize(Int_t iD,Char_t layer, Double_t ordinate);
  virtual ~StFgtStraightTrackMaker();
   Int_t Init();
   Int_t Make();
   Int_t Finish();
   void setUseChargeMatch(Bool_t use=true);
   //   Bool_t checkPulse(StFgtHit* pClus);
   vector<AVTrack>& getTracks();

   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtStraightTrackMaker.h,v 1.1 2012/12/20 20:32:07 avossen Exp $ built "__DATE__" "__TIME__ ; return cvs;}
 protected:
   Int_t m_effDisk;
   vector<generalCluster>** pClusters;
   vector<generalStrip>* pStrips;
   vector<vector<AVPoint>* > vvPoints;
   vector<TH2D*> v_hClusP;
   vector<TH2D*> v_hClusR;
   Bool_t useChargeMatch;
   Int_t printCounter;
   Int_t fitCounter;
   ofstream* outTxtFile;
   ofstream* cluNotFoundTxt;
   Short_t getQuadFromCoo(Double_t x, Double_t y);
   pair<Double_t,Double_t> getChargeRatio(Float_t r, Float_t phi, Int_t iD, Int_t iq);


   Bool_t getTrack(vector<AVPoint>& points, Double_t ipZ);
   pair<double,double> getDca(  vector<AVTrack>::iterator it);
   vector<AVTrack> m_tracks;
   // for accessing the data
   StFgtCollection *mFgtCollectionPtr;
   Double_t getRPhiRatio(vector<generalCluster>::iterator hitIterBegin, vector<generalCluster>::iterator hitIterEnd);
   Double_t findClosestPoint(float mx, float bx, float my, float by, double xE, double yE, Int_t iD);

   // for knowing what & how to plot


   // threshold, in units of # sigma above average
   Float_t mPedThres;
   //   Double_t getRPhiRatio(StSPtrVecFgtHitConstIterator hitIterBegin, StSPtrVecFgtHitConstIterator hitIterEnd);
   //   Double_t getRPhiRatio();
   
   int pulseCounterP;
   int pulseCounterR;

   int pulseCounterTP;
   int pulseCounterTR;

   int runningEvtNr;
   int hitCounter;
   int hitCounterR;
   //THD2** 
 private:   
      ClassDef(StFgtStraightTrackMaker,1);

};
inline void StFgtStraightTrackMaker::SetEffDisk(Int_t disk)
  {
    m_effDisk=disk;
  }
inline void StFgtStraightTrackMaker::setUseChargeMatch(Bool_t use){useChargeMatch=use;};
inline    vector<AVTrack>& StFgtStraightTrackMaker::getTracks(){return m_tracks;};
#endif

