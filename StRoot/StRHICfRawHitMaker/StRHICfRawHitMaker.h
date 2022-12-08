/***************************************************************************
 * Author: Akio Ogawa, Minho Kim, Seunghwan Lee
 ***************************************************************************
 *
 * Description: This is the Hit maker for the RHICf data. 
 *
 ***************************************************************************
 * $Id: StRHICfRawHitMaker.cxx,v 1.4 2017/01/30 18:10:16 akio Exp $
 * $Log: StRHICfRawHitMaker.cxx,v $
 ***************************************************************************/

#ifndef StRHICfRawHitMaker_H
#define StRHICfRawHitMaker_H

#include "StChain/StRTSBaseMaker.h"
#include "StRHICfUtil/StRHICfFunction.h"

class StRHICfDbMaker;
class StRHICfCollection;
class StRHICfRawHit;

class StRHICfRawHitMaker : public StRTSBaseMaker, StRHICfFunction
{
	public: 
		StRHICfRawHitMaker(const Char_t* name="RHICfRawHit");
		~StRHICfRawHitMaker();

		Int_t InitRun(Int_t runNumber);
		Int_t Make();
		Int_t Finish();

	private:
		Int_t setupRHICfCollection();

		StRHICfDbMaker* mRHICfDbMaker = 0; 
		StRHICfCollection* mRHICfCollection = 0; 
		StRHICfRawHit* mRHICfRawHitColl = 0;

	ClassDef(StRHICfRawHitMaker,0);
};

#endif
