
/***************************************************************************
 *
 * $Id: StResidualMaker.h,v 1.1.1.1 2001/03/15 00:22:23 lisa Exp $
 *
 * Author: malisa
 ***************************************************************************
 *
 * Extract hit residual distributions from primary and global tracks
 *
 ***************************************************************************
 *
 * $Log: StResidualMaker.h,v $
 * Revision 1.1.1.1  2001/03/15 00:22:23  lisa
 * Making new StTpcPool area and putting StResidualMaker into it
 *
 *
 **************************************************************************/

//
#ifndef StResidualMaker_hh     
#define StResidualMaker_hh

#include "StMaker.h"
#include <string>

#include "StThreeVectorD.hh"

class StEvent;
class StTrack;
class TFile;
class TNtuple;
class TF2;
class TCanvas;
#ifndef ST_NO_NAMESPACES
using std::string;
#endif


class StResidualMaker : public StMaker {
public:

    StResidualMaker(const Char_t *name="ResidualAnalysis");
    ~StResidualMaker();                                    
    
    void Clear(Option_t *option="");    
    Int_t  Init();                      
    Int_t  Make();                      
    Int_t  Finish();                    

    bool accept(StEvent*);            // this method serves as an event filter
    bool accept(StTrack*);            // and this is used to select tracks

    void SetNbinsDrift(int);
    void SetNbinsAlpha(int);
    void SetNbinsLambda(int);
    void SetNbinsResid(int);
    void SetResidMax(float);

    TCanvas* fitRMShisto(TH1* rmshisto); //!
    TH1* mPrimaryHistos[14];  //!
    TH1* mGlobalHistos[14];   //!

    void ShowProjections(char PrimaryOrGlobal, char InnerOrOuter, char XorZ);
    void ShowChiSquares();

    void writeHistoFile();
    void readHistoFile(char* FileName);  // note: this method overwrites all the maker's histos with those of the file!

    // track cuts...
    void SetNTpcHitsRange(int Low, int High);
    void SetPtRange(float Low, float High);

 private:
    int mNbinsDrift;
    int mNbinsAlpha;
    int mNbinsLambda;
    int mNbinsResid;
    float mResidMax;
    long mEventCounter;
    double mBfield;

    int mNTpcHits[2];
    float mPt[2];

    void fill3Dhistos(StTrack*,TH1**);
    void makeRMShisto(TH1*, TH1*, TH1*);

    StThreeVectorD mPadRowPlanes[25][46];  //! planes of padrows (sector and padrow numbering starts at 1 (ONE)
    double mSectorAngle[25];               //! directional angle for each sector (starting at 1).
                                           //  (note this is a bit redundant with mPadRowPlanes, but helps speed a bit
    //
    ClassDef(StResidualMaker,0)
};
#endif

inline void StResidualMaker::SetNTpcHitsRange(int Low, int High){mNTpcHits[0]=Low; mNTpcHits[1]=High;}
inline void StResidualMaker::SetPtRange(float Low, float High){mPt[0]=Low; mPt[1]=High;}
//
inline void StResidualMaker::SetNbinsDrift(int n){mNbinsDrift=n;}
inline void StResidualMaker::SetNbinsAlpha(int n){mNbinsAlpha=n;}
inline void StResidualMaker::SetNbinsLambda(int n){mNbinsLambda=n;}
inline void StResidualMaker::SetNbinsResid(int n){mNbinsResid=n;}
inline void StResidualMaker::SetResidMax(float max){mResidMax=max;}
