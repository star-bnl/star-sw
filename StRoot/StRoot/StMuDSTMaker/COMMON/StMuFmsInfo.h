#ifndef STROOT_STMUDSTMAKER_STMUFMSINFO_H
#define STROOT_STMUDSTMAKER_STMUFMSINFO_H


#include "TObject.h"

#include <iostream>


class StMuFmsInfo : public TObject
{
public:
	StMuFmsInfo();
	~StMuFmsInfo();

	Int_t fmsReconstructionFlag() {return mFmsReconstructionFlag;}
    void setFmsReconstructionFlag(Int_t v){mFmsReconstructionFlag=v;}

private:
	Int_t mFmsReconstructionFlag;

	ClassDef(StMuFmsInfo, 1)
};


#endif