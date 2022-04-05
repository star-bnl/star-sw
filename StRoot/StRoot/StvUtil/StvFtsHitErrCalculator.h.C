#ifndef __StvFtsHitErrCalculatorulator_h_
#define __StvFtsHitErrCalculatorulator_h_
#include "StEvent/StEnumerations.h"
#include "StvUtil/StvHitErrCalculator.h"

class StvHit;

class StvFtsHitErrCalculator : public StvHitErrCalculator {

public:	
  StvFtsHitErrCalculator(const char *name="FtsHitErr"):StvHitErrCalculator(name,2){};
int CalcDcaErrs(const StvHit* hit,double hRR[3]);
int CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
int CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3])
   {return StvHitErrCalculator::CalcDcaErrs(hiPos,hiDir,hRR);}

protected:
enum {
kRxyErr  	=0, 	/* Intrinsic resolution, padrow or rxy direction		*/
kPhiErr  	=1}; 	/* Intrinsic resolution, Phi direction			*/

static double mgRPhiErrs[2]; 	//these errors are used only for CalcDetErrs
				//when the concrete hit is not defined


ClassDef(StvFtsHitErrCalculator,0)
};

#endif
