#ifndef _multStruct
#define _multStruct

#include <TObject.h>

const int NPHIBINS = 24;
const int NETABINS = 16;
const int NPTBINS  =  2;

class multStruct : public TObject {
    public :
    multStruct();
    virtual ~multStruct();
    int    NewEvent(float xv, float yv, float zv);
    int    GetRefMult();
    void   SetRefMult(int refMult);
    int    AddTrack(int phiBin, int etaBin, int iPt, int sign, float pt);

    bool  mCalcRefMult;
    int   mRefMult;
    float mXVertex;
    float mYVertex;
    float mZVertex;
    float mTrackBinPlus[NPTBINS][NPHIBINS][NETABINS][2];
    float mTrackBinMinus[NPTBINS][NPHIBINS][NETABINS][2];
    float mPtSqPlus[NPTBINS];
    float mPtSqMinus[NPTBINS];

    // Following routines return counts in individual bins.
    float GetNSum(   int iPhi, int iEta, int iPt );
    float GetNPlus(  int iPhi, int iEta, int iPt );
    float GetNMinus( int iPhi, int iEta, int iPt );
    float GetNDiff(  int iPhi, int iEta, int iPt );
    float GetPSum(   int iPhi, int iEta, int iPt );
    float GetPPlus(  int iPhi, int iEta, int iPt );
    float GetPMinus( int iPhi, int iEta, int iPt );
    float GetPtSqSum(   int iPt );
    float GetPtSqPlus(  int iPt );
    float GetPtSqMinus( int iPt );

    ClassDef( multStruct, 1 )  // macro for rootcint
};

#endif

#ifdef multStruct_cxx

multStruct::multStruct() {
}

multStruct::~multStruct() {
}

#endif // #ifdef multStruct_cxx

