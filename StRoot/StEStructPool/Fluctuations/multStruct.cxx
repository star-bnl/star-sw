#define multStruct_cxx

#include "TStyle.h"
#include "TCanvas.h"
#include "multStruct.h"

ClassImp( multStruct )

int multStruct::NewEvent(float xv, float yv, float zv) {
    mCalcRefMult = true;
    mRefMult = 0;
    mXVertex = xv;
    mYVertex = yv;
    mZVertex = zv;
    for (int iPt=0;iPt<NPTBINS;iPt++) {
        for (int iPhi=0;iPhi<NPHIBINS;iPhi++) {
            for (int iEta=0;iEta<NETABINS;iEta++) {
                mTrackBinPlus[iPt][iPhi][iEta][0]  = 0;
                mTrackBinPlus[iPt][iPhi][iEta][1]  = 0;
                mTrackBinMinus[iPt][iPhi][iEta][0] = 0;
                mTrackBinMinus[iPt][iPhi][iEta][1] = 0;
            }
        }
        mPtSqPlus[iPt]  = 0;
        mPtSqMinus[iPt] = 0;
    }
    return 1;
}
int multStruct::GetRefMult() {
    return mRefMult;
}
void multStruct::SetRefMult(int refMult) {
    mCalcRefMult = false;
    mRefMult = refMult;
}
int multStruct::AddTrack(int phiBin, int etaBin, int iPt, int sign, float pt) {
    if (mCalcRefMult) {
        mRefMult++;
    }
    if (sign > 0) {
        ++mTrackBinPlus[iPt][phiBin][etaBin][0];
        mTrackBinPlus[iPt][phiBin][etaBin][1] += pt;
        mPtSqPlus[iPt] += pt*pt;
    } else if (sign < 0) {
        ++mTrackBinMinus[iPt][phiBin][etaBin][0];
        mTrackBinMinus[iPt][phiBin][etaBin][1] += pt;
        mPtSqMinus[iPt] += pt*pt;
    }
    return 1;
}
float multStruct::GetNSum( int iPhi, int iEta, int iPt ) {
    if (iPhi < 0) {
        return -1;
    } else if (iPhi >= NPHIBINS) {
        return -2;
    } else if (iEta < 0) {
        return -3;
    } else if (iEta >= NETABINS) {
        return -4;
    } else if (iPt < 0) {
        return -5;
    } else if (iPt >= NPTBINS) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][0] + mTrackBinMinus[iPt][iPhi][iEta][0];
    }
}
float multStruct::GetNPlus( int iPhi, int iEta, int iPt ) {
    if (iPhi < 0) {
        return -1;
    } else if (iPhi >= NPHIBINS) {
        return -2;
    } else if (iEta < 0) {
        return -3;
    } else if (iEta >= NETABINS) {
        return -4;
    } else if (iPt < 0) {
        return -5;
    } else if (iPt >= NPTBINS) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][0];
    }
}
float multStruct::GetNMinus( int iPhi, int iEta, int iPt ) {
    if (iPhi < 0) {
        return -1;
    } else if (iPhi >= NPHIBINS) {
        return -2;
    } else if (iEta < 0) {
        return -3;
    } else if (iEta >= NETABINS) {
        return -4;
    } else if (iPt < 0) {
        return -5;
    } else if (iPt >= NPTBINS) {
        return -6;
    } else {
        return mTrackBinMinus[iPt][iPhi][iEta][0];
    }
}
float multStruct::GetNDiff( int iPhi, int iEta, int iPt ) {
    if (iPhi < 0) {
        return -1;
    } else if (iPhi >= NPHIBINS) {
        return -2;
    } else if (iEta < 0) {
        return -3;
    } else if (iEta >= NETABINS) {
        return -4;
    } else if (iPt < 0) {
        return -5;
    } else if (iPt >= NPTBINS) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][0] - mTrackBinMinus[iPt][iPhi][iEta][0];
    }
}
float multStruct::GetPSum( int iPhi, int iEta, int iPt ) {
    if (iPhi < 0) {
        return -1;
    } else if (iPhi >= NPHIBINS) {
        return -2;
    } else if (iEta < 0) {
        return -3;
    } else if (iEta >= NETABINS) {
        return -4;
    } else if (iPt < 0) {
        return -5;
    } else if (iPt >= NPTBINS) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][1] + mTrackBinMinus[iPt][iPhi][iEta][1];
    }
}
float multStruct::GetPPlus( int iPhi, int iEta, int iPt ) {
    if (iPhi < 0) {
        return -1;
    } else if (iPhi >= NPHIBINS) {
        return -2;
    } else if (iEta < 0) {
        return -3;
    } else if (iEta >= NETABINS) {
        return -4;
    } else if (iPt < 0) {
        return -5;
    } else if (iPt >= NPTBINS) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][1];
    }
}
float multStruct::GetPMinus( int iPhi, int iEta, int iPt ) {
    if (iPhi < 0) {
        return -1;
    } else if (iPhi >= NPHIBINS) {
        return -2;
    } else if (iEta < 0) {
        return -3;
    } else if (iEta >= NETABINS) {
        return -4;
    } else if (iPt < 0) {
        return -5;
    } else if (iPt >= NPTBINS) {
        return -6;
    } else {
        return mTrackBinMinus[iPt][iPhi][iEta][1];
    }
}
float multStruct::GetPtSqSum( int iPt ) {
    if (iPt < 0) {
        return -1;
    } else if (iPt >= NPTBINS) {
        return -2;
    } else {
        return mPtSqPlus[iPt] + mPtSqMinus[iPt];
    }
}
float multStruct::GetPtSqPlus( int iPt ) {
    if (iPt < 0) {
        return -1;
    } else if (iPt >= NPTBINS) {
        return -2;
    } else {
        return mPtSqPlus[iPt];
    }
}
float multStruct::GetPtSqMinus( int iPt ) {
    if (iPt < 0) {
        return -1;
    } else if (iPt >= NPTBINS) {
        return -2;
    } else {
        return mPtSqMinus[iPt];
    }
}
