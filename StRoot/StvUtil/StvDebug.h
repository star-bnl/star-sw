#ifndef __STVDEBUG_h_
#define __STVDEBUG_h_
#include "TObject.h"
//------------------------------------------------------------------------------
class TH1;
class StvDebug
{
public:	
  StvDebug(){}
static  int Break(int kount);
static  int Break(double x,double y,double z);
static  int Debug() {return mgDebug;};
static void Count(const char *key,double val=0);
static void Sumary();
static void Reset();
static const char *Env(const char *key);
static        int  Inv(const char *key);
static int& Flag(const char *key);
private:
static void Draw(int nH,TH1** H);
public:
static int mgDebug;
static int mgRecov;
static int mgCheck;


#if 0
ClassDef(StvDebug,0)
#endif
};
#endif
