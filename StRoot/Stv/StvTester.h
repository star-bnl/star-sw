#ifndef __StvTester_h_
#define __StvTester_h_
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "TNamed.h"
#include "StvUtil/StvNodePars.h"

class StvELossTrak;
class StvTrack;
class StvNode;
class StvTester : public TNamed {
private:
StvTester(const char *name="DefaultTester");
public:	
static StvTester *Inst();	
int TestIt(const char *tit,const StvTrack *tk);
private:
double DeltaLen(const StvNode* curNode,const StvNode* preNode,double *lenXY=0) 	const;
double PLoss   (const StvNode* curNode,const StvNode* preNode,double *len  =0) 	const;

protected:
      char         mBeg[1];
 StvELossTrak     *mEl;
      char         mEnd[1];
static StvTester *mgTester;
ClassDef(StvTester,0);
};

#endif //__StvTester_h_


