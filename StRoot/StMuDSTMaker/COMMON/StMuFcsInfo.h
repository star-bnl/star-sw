/***************************************************************************
 *
 * $Id: StMuFcsInfo.h,v 1.0 2021/11/17 16:07:31 jdb Exp $
 *
 * Author: Daniel Brandenburg, 2021
 ***************************************************************************
 *
 * Description: StMuFcsInfo is event level data for Fcs. 
 *  Stores hit indices for each detector
 *
 ***************************************************************************/
#ifndef STROOT_STMUDSTMAKER_STMUFCSINFO_H
#define STROOT_STMUDSTMAKER_STMUFCSINFO_H


#include "TObject.h"
#include "StEvent/StEnumerations.h"
#include <iostream>


class StMuFcsInfo : public TObject
{
public:
	StMuFcsInfo();
	~StMuFcsInfo();

	Int_t fcsReconstructionFlag() {return mFcsReconstructionFlag;}
    void setFcsReconstructionFlag(Int_t v){mFcsReconstructionFlag=v;}

    void setHitIndex( UInt_t idet, UInt_t index ) { mHitIndex[idet % (kFcsNDet + 2)] = index; }
    void setClusterIndex( UInt_t idet, UInt_t index ) { mClusterIndex[idet % (kFcsNDet + 1)] = index; }
    void setPointIndex( UInt_t idet, UInt_t index ) { mPointIndex[idet % (kFcsNDet + 1)] = index; }

    UInt_t hitIndex( UInt_t idet ) { return mHitIndex[idet % (kFcsNDet + 2)]; }
    UInt_t clusterIndex( UInt_t idet ) { return mClusterIndex[idet % (kFcsNDet + 1)]; }
    UInt_t pointIndex( UInt_t idet ) { return mPointIndex[idet % (kFcsNDet + 1)]; }

private:
	Int_t mFcsReconstructionFlag;
    UInt_t mHitIndex[kFcsNDet+2];   // these are intentionally 1 longer than you expect, 
    UInt_t mClusterIndex[kFcsNDet+1]; // since the last index stores the final index,
    UInt_t mPointIndex[kFcsNDet+1];   // so that the length is known

	ClassDef(StMuFcsInfo, 1)
};


#endif