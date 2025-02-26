#ifndef ST_FWD_CLOSURE_MAKER_H
#define ST_FWD_CLOSURE_MAKER_H

#include "TClonesArray.h"
#ifndef __CINT__
#include "GenFit/Track.h"
#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrack.h"
#endif

#include "StChain/StMaker.h"
#include "TTree.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "StEvent/StEnumerations.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"

#include <map>


class StMuDstMaker;
class StMuDst;
class StMuFwdTrackCollection;
class StMuFcsCollection;
class StFwdTrackMaker;
class StEvent;

class StFwdClosureMaker : public StMaker {

    ClassDef(StFwdClosureMaker, 0);

  public:
    StFwdClosureMaker();
    ~StFwdClosureMaker(){/* nada */};

    int Init();
    int Finish();
    int Make();
    void Clear(const Option_t *opts = "");

#ifndef __CINT__
    TVector3 raster(TVector3 p0);
    TMatrixDSym makeSiCovMat(TVector3 hit);
#endif

  // protected:

    float mPVal = 1e-3;
    float mBlowUp = 1e3;
    int   mMaxIt = 40;
    float mRelChi2 = 0.1;

    // Primary Vertex resolutions
    double mPrimaryVertexSigXY = 10.1; // in cm (e.g. 0.01 = 100 microns)
    double mPrimaryVertexSigZ = 10.1; // in cm (e.g. 0.01 = 100 microns)
    
    // FST resolutions
    double mRasterR = 3.0;
    double mRasterPhi = 0.004;

    // use FTT in tracking?
    int mNumFttToUse = 4;
    enum FttMode { kStrip, kPoint};
    FttMode mFttMode = kPoint;

    TString mOutFile = "fwdClosure.root";
};


#endif
