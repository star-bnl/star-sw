#ifndef StJetFinderKonst_h
#define StJetFinderKonst_h
#include "TObject.h"

//*-- Author:
//*-- Aleksei Pavlinov (WSU)
//*   Only UA1 now 
// *** What to do
// 1. Reorder jets with taking to the account particles cell(?)
// 2. Hist for control of quality.

#include "StJetFinder.h"

#define MAXTRACKS_KJF 1000
#define MAXJETS_KJF 8

class TH1;
class TH1F;
class TH2F;
class TFile;
class TCanvas;

class  StJetFinderKonst {

  enum {size = MAXJETS_KJF};
  enum {part_size = MAXTRACKS_KJF};
  Double_t PtJ[size],EtaJ[size],PhiJ[size],PtPartM[size],EJet[size],Dz[size][part_size];
  Int_t NJets,NpartJ[size];

 public:
   StJetFinderKonst(); 
   virtual ~StJetFinderKonst();

   void setDebug(Int_t var)         {mDebug=var;};
   void setConeRadius(Double_t var) {mConeRadius = var;};
   void setEtSeed(Double_t var)     {mEtSeed = var;};
   void setMinJetEt(Double_t var)   {mMinJetEt = var;};
   void setMinCellEt(Double_t var)  {mMinCellEt = var;};
   void setmModeInfoCluster(Int_t var)  {mModeInfoCluster = var;};
   void setParameters(Double_t coneRadius, Double_t etSeed, Double_t minJetEt,Double_t minCellEt){ 
        setConeRadius(coneRadius);
        setEtSeed(etSeed);
        setMinJetEt(minJetEt);
        setMinCellEt(minCellEt);
   };
   void setNBinEta(Int_t nBins) { mNbinEta = nBins; };
   void setEtaMin(Double_t etaMin) { mEtaMin = etaMin; };
   void setEtaMax(Double_t etaMax) { mEtaMax = etaMax; };
   void setNBinPhi(Int_t nBins) { mNbinPhi = nBins; };
   void setPhiMin(Double_t phiMin) { mPhiMin = phiMin; };
   void setPhiMax(Double_t phiMax) { mPhiMax = phiMax; };
   void setPtMax(Double_t ptMax) { mPtMax = ptMax; };

   const Double_t getConeRadius(void) {return mConeRadius;};
   const Double_t getEtSeed(void) {return mEtSeed;};
   const Double_t getMinJetEt(void) {return mMinJetEt;};
   const Double_t getMinCellEt(void) {return mMinCellEt;};
   const Int_t getmModeInfoCluster(void) {return mModeInfoCluster;};
   Int_t getNBinEta(void) { return mNbinEta; };
   Double_t getEtaMin(void) { return mEtaMin; };
   Double_t getEtaMax(void) { return mEtaMax; };
   Int_t getNBinPhi(void) { return mNbinPhi; };
   Double_t getPhiMin(void) { return mPhiMin; };
   Double_t getPhiMax(void) { return mPhiMax; };
   Double_t getPtMax(void) { return mPtMax; };

   TH1F*    getHstat(void) {return mHstat;};
   TH2F*    getHlego(void) {return mHlego;}; 

   Int_t FindCone(Int_t ntrk, Float_t *pt, Float_t *eta, 
       Float_t *Phi, Float_t *ms, StProtoJet* jets, StProtoJet** tracks);// return number of jets
   Int_t FindKt(Int_t ntrk, Float_t *pt, Float_t *eta, 
       Float_t *Phi, Float_t *ms, StProtoJet* jets, StProtoJet** tracks);// return number of jets
   //
   void fillControlHists();

   void print();              //    *MENU*
   void printStatistics();    //    *MENU*

   void drawControlHists();   //    *MENU*
   static void drawHist(TH1* hid=0, Int_t lineWidth=2, Int_t lineColor=1, Int_t lineStyle=1, char* opt="");
 protected:
   Int_t    mDebug;
   // Parameters for jet finder itself 
   Double_t mConeRadius;      // jet radisu in (eta,phi) space
   Double_t mEtSeed;          // min. Et for jet seed
   Double_t mMinJetEt;        // min. Et of  jet 
   Double_t mMinCellEt;       // Min Et in on cell (particle)
   Double_t mPtMax;           // max Pt of particle to be considered?
   // Calorimeter grid
   Int_t    mNbinEta;
   Double_t mEtaMin;
   Double_t mEtaMax;
   Int_t    mNbinPhi;
   Double_t mPhiMin;
   Double_t mPhiMax;
   TH2F*    mHlego;            //!
   // control histogramms
   TH1F*    mHjetEt;           //!
   TH1F*    mHjetEta;          //!
   TH1F*    mHjetPhi;          //!
   TH1F*    mHjetPart;         //!
   TH1F*    mHdiffEt;          //!
   TH1F*    mHdiffEta;         //!
   TH1F*    mHdiffPhi;         //!
   TH1F*    mHPtPartM;
   TH2F*    mHPhiEta;
   TH2F*    mHNpartR;
   TH1F*    mHDz;
   TH1F*    mHEjet;
   TFile*   file1; 
   // 
   Int_t    mModeInfoCluster; // as mstu(54) in PYTHIA

   TH1F *mHstat;              //! histograms for statistics

 private:
   Int_t npintb[MAXTRACKS_KJF], ninjet[MAXTRACKS_KJF];
   TCanvas *mC1;
   ClassDef(StJetFinderKonst,0)
};                                                                    

#endif                                                                
