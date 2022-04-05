#ifndef STAR_StClusterDisplay
#define STAR_StClusterDisplay
#include "StMaker.h"
#include <TH2.h>
#include "Stiostream.h"
#include "TString.h"
#include "StEmcRawMaker/defines.h"
#include "TCanvas.h"

#define N1 5
#define N2 3
class StEmcGeom;

class StClusterDisplay : public StMaker
{
private:

protected:
    TH1F*          mHist1D[N1][MAXDETBARREL];
    TH2F*          mHist2D[N2][MAXDETBARREL];
    TCanvas*       mCanvas[MAXDETBARREL];
    StEmcGeom*     mGeo[MAXDETBARREL];
    Bool_t         mDraw;
    Float_t        mTh[MAXDETBARREL];

public:

    StClusterDisplay(const char *name="Teste");
    virtual         ~StClusterDisplay();
    virtual Int_t   Init();
    virtual Int_t   Make();
    virtual Int_t   Finish();
    virtual Int_t   SaveHist();
    void    setDraw(Bool_t a)
    {
        mDraw = a;
    }
    void    setHitThreshold(Int_t det, Float_t th)
    {
        mTh[det-1] = th;
    }

    ClassDef(StClusterDisplay, 1)
};

#endif
