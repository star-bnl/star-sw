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
                mTrackBinPlus[iPt][iPhi][iEta][2]  = 0;
                mTrackBinMinus[iPt][iPhi][iEta][0] = 0;
                mTrackBinMinus[iPt][iPhi][iEta][1] = 0;
                mTrackBinMinus[iPt][iPhi][iEta][2] = 0;
            }
        }
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
int multStruct::AddTrack(int phiBin, int etaBin, int iPt, int sign, double pt) {
    if (mCalcRefMult) {
        mRefMult++;
    }
    int jPt = iPt;
    if (jPt < 0) {
        iPt = mNPtBins;
    } else if (jPt >= mNPtBins) {
        printf("!!!!! in multStruct: Trying to store into Pt bin %i. Larger than number of bins %i\n", iPt, mNPtBins);
        iPt = mNPtBins;
    }
    if (sign > 0) {
        mTrackBinPlus[iPt][phiBin][etaBin][0] += 1;
        mTrackBinPlus[iPt][phiBin][etaBin][1] += pt;
        mTrackBinPlus[iPt][phiBin][etaBin][2] += pt*pt;
    } else if (sign < 0) {
        mTrackBinMinus[iPt][phiBin][etaBin][0] += 1;
        mTrackBinMinus[iPt][phiBin][etaBin][1] += pt;
        mTrackBinMinus[iPt][phiBin][etaBin][2] += pt*pt;
    }
    return 1;
}
double multStruct::GetNSum( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][0] + mTrackBinMinus[iPt][iPhi][iEta][0];
    }
}
double multStruct::GetNPlus( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][0];
    }
}
double multStruct::GetNMinus( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinMinus[iPt][iPhi][iEta][0];
    }
}
double multStruct::GetNDiff( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][0] - mTrackBinMinus[iPt][iPhi][iEta][0];
    }
}
double multStruct::GetPtSum( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][1] + mTrackBinMinus[iPt][iPhi][iEta][1];
    }
}
double multStruct::GetPtPlus( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][1];
    }
}
double multStruct::GetPtMinus( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinMinus[iPt][iPhi][iEta][1];
    }
}
double multStruct::GetPtSqSum( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][2] + mTrackBinMinus[iPt][iPhi][iEta][2];
    }
}
double multStruct::GetPtSqPlus( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinPlus[iPt][iPhi][iEta][2];
    }
}
double multStruct::GetPtSqMinus( int iPhi, int iEta, int iPt ) {
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
    } else if (iPt > mNPtBins) {
        return -6;
    } else {
        return mTrackBinMinus[iPt][iPhi][iEta][2];
    }
}
