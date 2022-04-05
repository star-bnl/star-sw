#include "StMuMtdRawHit.h"
#include "StEvent/StEvent.h"
#include "StEvent/StMtdHit.h"
#include "StEvent/StMtdRawHit.h"

ClassImp(StMuMtdRawHit)

StMuMtdRawHit::StMuMtdRawHit() : mFlag(-1), mBackLeg(-1), mChannel(-1), mTdc(-1)
{	
  // default constructor
}

StMuMtdRawHit::StMuMtdRawHit(const StMtdRawHit *hit){

	mFlag = hit->flag();;
	mBackLeg = hit->backleg();
	mChannel = hit->channel();
	mTdc = hit->tdc();	
    	
}

bool StMuMtdRawHit::leadingEdge() const { return (mFlag>0);}
bool StMuMtdRawHit::trailingEdge() const{ return (mFlag<0);}
int StMuMtdRawHit::fiberId() const { return abs(static_cast<int>(mFlag)) - 1;}
int StMuMtdRawHit::flag() const {return mFlag;}
int StMuMtdRawHit::backleg()  const {return mBackLeg;}
int StMuMtdRawHit::channel()  const{return mChannel;}
unsigned int StMuMtdRawHit::tdc()  const { return mTdc;}
