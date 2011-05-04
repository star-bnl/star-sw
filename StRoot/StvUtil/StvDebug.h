#ifndef __STVDEBUG_h_
#define __STVDEBUG_h_
#include "TObject.h"
//------------------------------------------------------------------------------
class TH1;
class StvDebug
{
public:	
  StvDebug(){}
static void Break(int kount);
static void Count(const char *key,double val=0);
static void Sumary();
static void Reset();
private:
static void Draw(int nH,TH1** H);
#if 0
ClassDef(StvDebug,0)
#endif
};
#endif
