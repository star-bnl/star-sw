#ifndef _multStruct
#define _multStruct

#include <stdio.h>
#include <TObject.h>

const int NPHIBINS = 24;
const int NETABINS = 16;
const int NPTBINS  =  5;

class multStruct : public TObject {
    public :
    multStruct( int nPtBins );
    virtual ~multStruct();
    int    NewEvent(float xv, float yv, float zv);
    int    GetRefMult();
    void   SetRefMult(int refMult);
    int    AddTrack(int phiBin, int etaBin, int iPt, int sign, double pt);

    int   mNPtBins;
    bool  mCalcRefMult;
    int   mRefMult;
    float mXVertex;
    float mYVertex;
    float mZVertex;
    double mTrackBinPlus[NPTBINS][NPHIBINS][NETABINS][3];
    double mTrackBinMinus[NPTBINS][NPHIBINS][NETABINS][3];

    // Following routines return counts in individual bins.
    double GetNSum(   int iPhi, int iEta, int iPt );
    double GetNPlus(  int iPhi, int iEta, int iPt );
    double GetNMinus( int iPhi, int iEta, int iPt );
    double GetNDiff(  int iPhi, int iEta, int iPt );
    double GetPtSum(   int iPhi, int iEta, int iPt );
    double GetPtPlus(  int iPhi, int iEta, int iPt );
    double GetPtMinus( int iPhi, int iEta, int iPt );
    double GetPtSqSum(   int iPhi, int iEta, int iPt );
    double GetPtSqPlus(  int iPhi, int iEta, int iPt );
    double GetPtSqMinus( int iPhi, int iEta, int iPt );

    ClassDef( multStruct, 1 )  // macro for rootcint
};

#endif

#ifdef multStruct_cxx

multStruct::multStruct( int nPtBins ) {
    if (nPtBins >= NPTBINS) {
        printf("!!!!!!!!!! You are asking for more Pt bins than multStruct can handle -> Recompile\n");
        printf("!!!!!!!!!! For this run we use lowest %i distinct bins and lump all rest in last bin.\n", nPtBins-1);
        mNPtBins = NPTBINS-1;
    } else {
        mNPtBins = nPtBins;
    }
}

multStruct::~multStruct() {
}

#endif // #ifdef multStruct_cxx

