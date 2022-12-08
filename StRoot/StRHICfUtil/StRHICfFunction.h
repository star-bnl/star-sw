/***************************************************************************
 * Author: Seunghwan Lee
 ***************************************************************************
 *
 * Description: RHICf Useful class for calibration and reconstruction
 *
 ***************************************************************************
 * $Id: StRHICfFunction.h,v 1.0 2022/08/25 2:51:00 SeunghwanLee Exp $
 * $Log: StRHICfFunction.h,v $
 ***************************************************************************/

#ifndef StRHICfFunction_H
#define StRHICfFunction_H

#include <iostream>
#include <TString.h>
#include "StRoot/StEvent/StEnumerations.h"

class StEnumerations;

enum RHICfRunType
{
	kRHICfTL = 0, 
	kRHICfTS = 1, 
	kRHICfTOP = 2 
};

enum RHICfIsValue
{
	kRHICfOk = true,
	kRHICfFatal = false
};

enum RHICfParticle
{
    kRHICfPhoton = 0,
    kRHICfHadron = 1
};

class StRHICfFunction 
{
	public: 
		StRHICfFunction();
		virtual ~StRHICfFunction();

        void initChecker();

		void setRunType(Int_t type){mRunType = type;}
		Int_t getRunType(){return mRunType;}
		Int_t checkRunTypeForRHICf2017(Int_t runNum);
		Int_t checkGSOBarSize(Int_t tower); // 0 = small, 1 = large

		Float_t rescaleEnergyFactor(Int_t tower, Int_t layer);
        
		Bool_t checkGSOBarEnergy(Float_t val);
		Bool_t checkGSOBarTable(Float_t val);
		Bool_t checkPlateEnergy(Float_t val);
		Bool_t checkRecoValue(TString opt, Float_t val);
		Bool_t checkLeakageTable(TString opt, Bool_t val);

	private:
		Int_t mRunType;
		Int_t mGSOBarNum;
		Int_t mGSOBarTableNum;
		Int_t mPlateNum;
		Int_t mValueForRecoNum[5]; // in StRHICfRecoEnergy, check setting parameter[ResultHitPos, ResultHitNum, MultiHitPos, MultiPeakHeight, Overlap]
		Int_t mLeakageNum[3];

    ClassDef(StRHICfFunction,0);
};

#endif
