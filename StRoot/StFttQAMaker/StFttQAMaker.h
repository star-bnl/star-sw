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

// STL
#include <vector>

class TFile;
class StEvent;
class StFttCollection;
class StFttRawHit;

const Int_t mMax = 10000;
struct FttData
{
    // event information
    Int_t    EVT;
    Int_t    N;

    //channel information
    Int_t    sec[mMax];
    Int_t    rdo[mMax];
    Int_t    plane[mMax];
    Int_t    quad[mMax];
    Int_t    feb[mMax];
    Int_t    febvmm[mMax];
    Int_t    vmm[mMax];
    Int_t    ch[mMax];
    Int_t    bcid[mMax];
    Int_t    adc[mMax];
    Int_t    tb[mMax];
    Int_t    row[mMax];
    Int_t    strip[mMax];
    Int_t    dir[mMax];

    Int_t    cN;
    Int_t    cplane[mMax];
    Int_t    cquad[mMax];
    Int_t    crow[mMax];
    Int_t    cdir[mMax];
    Int_t    csumadc[mMax];
    Float_t  cx[mMax];
    Float_t  csigma[mMax];
    Float_t  cnstrips[mMax];

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
    void PlotClusterWithHits( vector<StFttRawHit*> hits );


    void WriteHistograms();
    void BookHistograms();
    void BookTree();

    StEvent*             mEvent;
    StFttCollection*     mFttCollection;

    std::map< string, TH1* >    mH1d;
    std::map< string, TH2* >    mH2d;
    std::map< string, TH2Poly*> mH2p;
    TTree * mFttTree;
    FttData mFttData;
    TFile* mFile;

    size_t histCounter = 0;
    size_t f1CluCounter = 0;

    ClassDef(StFttQAMaker, 1)

};

#endif
