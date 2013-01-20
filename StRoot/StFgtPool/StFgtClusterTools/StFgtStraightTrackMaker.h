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
   void setPrint(Bool_t print);
   void setMinNumFitPoints(Int_t numFP);
   void addMultiplePoints(Bool_t addMult);
   void setMaxClusters(Int_t maxC);
   void setMaxDistChi(Float_t maxDChi);
   void setMaxDist2(Float_t d);
   void setFitWithVertex(Bool_t f);
   void setRefitWithVertex(Bool_t f);
   void setMaxPhiDiff(Float_t pd);

   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtStraightTrackMaker.h,v 1.3 2013/01/20 02:05:32 avossen Exp $ built "__DATE__" "__TIME__ ; return cvs;}
 protected:
   Bool_t isCosmic;
   Float_t maxPhiDiff;//for points belonging to same track
   Int_t maxClusters; //per disk
   Float_t vertexCut;
   Bool_t doPrint;
   Bool_t pulseCondition;
   Bool_t lenCondition;
   Float_t maxDistStrip_R;
   Float_t maxDistStrip_Phi;
   Bool_t doFitWithVertex;
   Bool_t doRefitWithVertex;
   Bool_t doAddMultiplePoints;
   Bool_t isMuDst;
   Float_t maxDist2;
   Float_t maxDistChi;
   Float_t maxDist2Eff;
   Int_t minNumFitPoints;

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

inline void StFgtStraightTrackMaker::setMaxPhiDiff(Float_t pd)
{
  maxPhiDiff=pd;
};

inline void StFgtStraightTrackMaker::SetEffDisk(Int_t disk)
  {
    m_effDisk=disk;
  }
inline void StFgtStraightTrackMaker::setUseChargeMatch(Bool_t use){useChargeMatch=use;};
inline    vector<AVTrack>& StFgtStraightTrackMaker::getTracks(){return m_tracks;};


  inline void StFgtStraightTrackMaker::setPrint(Bool_t print)
{
  doPrint=print;
}
inline   void StFgtStraightTrackMaker::setMinNumFitPoints(Int_t numFP)
{
  minNumFitPoints=numFP;
}
inline   void StFgtStraightTrackMaker::addMultiplePoints(Bool_t addMult)
{
  doAddMultiplePoints=addMult;
}
inline   void StFgtStraightTrackMaker::setMaxClusters(Int_t maxC)
{
  maxClusters=maxC;
}
inline   void StFgtStraightTrackMaker::setMaxDistChi(Float_t maxDChi)
{
  maxDistChi=maxDChi;
}
inline   void StFgtStraightTrackMaker::setMaxDist2(Float_t d)
{
  maxDist2=d;
}
inline   void StFgtStraightTrackMaker::setFitWithVertex(Bool_t f)
{
  doFitWithVertex=f;
}
inline   void StFgtStraightTrackMaker::setRefitWithVertex(Bool_t f)
{
  doRefitWithVertex=f;
}

#endif

