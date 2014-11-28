#ifndef STAR_StEmcMipMaker
#define STAR_StEmcMipMaker

#include <TString.h>
#include <TF1.h>
#include <TH2.h>
#include <TH1.h>

#include <StMaker.h>

#include "StEmcCalibrationMaker.h"
#include "StEmcCalibMaker.h"

#define MAXTRACK 10000
#define MAXBEMC 4
#define MAXCHANNEL 18000

class StEmcEqualMaker;

class StEmcMipMaker : public StEmcCalibMaker 
{
  protected: 		
    TH1F                        *mMipPos;
    TH1F                        *mMipPosErr;
    TH1F                        *mMipWid;
    TH1F                        *mMipWidErr;
    TH1F                        *mGain;
    TH1F                        *mGainDistr;
    TH1F                        *mChi2;
    TH1F                        *mIntegral;
    
    float                       mPmin;
    int                         mNPoints;
    
    int                         mNFailed[10];
                                
  public:
     
                                StEmcMipMaker(const char *name="EmcMip");
   virtual                      ~StEmcMipMaker();
   virtual    Int_t             Init();
   virtual    Int_t             Make();
   virtual    Int_t             Finish();
   virtual    void              Clear(Option_t *option="");  
              
              TH1F*             compareToDb(char*,int=0);
              void              saveToDb(char*);
              
              void              mipCalib();
              void              mipCalib(int,int,int,StEmcEqualMaker*, bool = false);
              
              TH1F*             findMip(int,int=1,bool=true);
              TH1F*             findMip(int,int,StEmcEqualMaker*);
              float             findGain(int,bool=true);
              
              float             getMipPosition(int id) {return mMipPos->GetBinContent(id);}
              float             getMipPositionError(int id) {return mMipPosErr->GetBinContent(id);}
              float             getMipWidth(int id) {return mMipWid->GetBinContent(id);}
              float             getMipWidthError(int id) {return mMipWidErr->GetBinContent(id);}
              float             getGain(int id) {return mGain->GetBinContent(id);}
              float             getChi2(int id) {return mChi2->GetBinContent(id);}
              float             getIntegral(int id) {return mIntegral->GetBinContent(id);}
              
              int               getNFailed(int flag) { return mNFailed[flag];}
             
              TH1F*             getGainHist() { return mGain;}
              TH1F*             getGainDistrHist() { return mGainDistr;}
              TH1F*             getChi2Hist() { return mChi2;}
              TH1F*             getIntegralHist() { return mIntegral;}
              TH1F*             getMipPosHist() { return mMipPos;}
              TH1F*             getMipWidHist() { return mMipWid;}
              
              void              fit(TH1F*);
                            
              TF1*              funcFit;       
              TF1*              funcFitPeak;       
              TF1*              funcFitBack;       
              
   ClassDef(StEmcMipMaker, 1)  
};

#endif
