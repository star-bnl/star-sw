#ifndef STAR_StFttQAMaker_H
#define STAR_StFttQAMaker_H


/***************************************************************************                                          
 *                                                                                                                    
 * $Id: StFttQAMaker.h,v 0.1 2017/02/21 17:50:32 tlusty Exp $                                                    
 * StFttQAMaker - class to fille the StEvent from DAQ reader                                                        
 *--------------------------------------------------------------------------                                          
 *                                                                                                                    
 ***************************************************************************/
#include "StMaker.h"

// ROOT
#include "TH1.h"
#include "TH2.h"
#include "TH2Poly.h"
#include "TTree.h"
#include "TCanvas.h"
#include "TString.h"

// STL
#include <vector>

class TFile;
class StEvent;
class StFttCollection;
class StFttRawHit;
class StFttDb;

const Int_t FTT_MAX_HITS = 50000;
const Int_t FTT_MAX_CLUSTERS = 10000;
const Int_t FTT_MAX_POINTS = 4000;
struct FttData
{
    // event information
    Int_t    EVT;
    Int_t    RUN;
    UShort_t N;

    //channel information
    UChar_t    sec[FTT_MAX_HITS];
    UChar_t    rdo[FTT_MAX_HITS];
    UChar_t    plane[FTT_MAX_HITS];
    UChar_t    quad[FTT_MAX_HITS];
    UChar_t    feb[FTT_MAX_HITS];
    UChar_t    febvmm[FTT_MAX_HITS];
    UChar_t    vmm[FTT_MAX_HITS];
    UChar_t    ch[FTT_MAX_HITS];
    UShort_t   bcid[FTT_MAX_HITS];
    Short_t    dbcid[FTT_MAX_HITS];
    Short_t    time[FTT_MAX_HITS];
    UShort_t   adc[FTT_MAX_HITS];
    Short_t    tb[FTT_MAX_HITS];
    UChar_t    row[FTT_MAX_HITS];
    UChar_t    strip[FTT_MAX_HITS];
    UChar_t    dir[FTT_MAX_HITS];

    UShort_t cN;
    UChar_t    cplane[FTT_MAX_CLUSTERS];
    UChar_t    cquad[FTT_MAX_CLUSTERS];
    UChar_t    crow[FTT_MAX_CLUSTERS];
    UChar_t    cdir[FTT_MAX_CLUSTERS];
    UShort_t   csumadc[FTT_MAX_CLUSTERS];
    Float_t    cx[FTT_MAX_CLUSTERS];
    Float_t    csigma[FTT_MAX_CLUSTERS];
    Float_t    cnstrips[FTT_MAX_CLUSTERS];
    Float_t    csatfrac[FTT_MAX_CLUSTERS];


    UShort_t pN;
    UChar_t  pplane[FTT_MAX_POINTS];
    UChar_t  pquad[FTT_MAX_POINTS];
    Float_t  px[FTT_MAX_POINTS];
    Float_t  py[FTT_MAX_POINTS];

    Float_t  pgx[FTT_MAX_POINTS];
    Float_t  pgy[FTT_MAX_POINTS];
    Float_t  pgz[FTT_MAX_POINTS];


    UChar_t  prowh[FTT_MAX_POINTS];
    UChar_t  prowv[FTT_MAX_POINTS];
    UShort_t psumadc[FTT_MAX_POINTS];
    UChar_t  pnstripsh[FTT_MAX_POINTS];
    UChar_t  pnstripsv[FTT_MAX_POINTS];
    

};


class StFttQAMaker: public StMaker
{
private:

public:

/// Default constructor                                                                                          
    StFttQAMaker(const char *name="fttQA");

    ~StFttQAMaker();


    Int_t  Init();
    Int_t  InitRun(Int_t);
    Int_t  FinishRun(Int_t);
    Int_t  Finish();
    Int_t  Make();

    void MakeRawHitQA();
    void MakeClusterQA();
    void MakePointQA();
    void PlotClusterWithHits( vector<StFttRawHit*> hits );


    void WriteHistograms();
    void BookHistograms();
    void BookTree();

    StEvent*             mEvent;
    StFttCollection*     mFttCollection;
    StFttDb*             mFttDb;

    std::map< string, TH1* >    mH1d;
    std::map< string, TH2* >    mH2d;
    std::map< string, TH2Poly*> mH2p;
    TTree * mFttTree;
    FttData mFttData;
    TFile* mFile;

    size_t histCounter = 0;
    size_t f1CluCounter = 0;
    TCanvas *mCanvas = nullptr;
    bool mDebug = false;

    static const size_t maxClusterViz = 1000;
    TString mClusterPdfName;

    ClassDef(StFttQAMaker, 1)

};

#endif
