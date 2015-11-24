#ifndef __STVDEBUG_h_
#define __STVDEBUG_h_
#include "TObject.h"
//------------------------------------------------------------------------------
class TObject;
class TH1;
class StvDebug
{
public:	
  StvDebug(){}
static  int Break(int kount);
static  int Break(double x,double y,double z);
static  int Debug() {return mgDebug;};
static void SetDebug(int idb) {mgDebug=idb;};
static void Count(const char *key,double val=0);
static void Count(const char *key,double valx,double valy);
static void Sumary();
static void Reset();
static const char *Env(const char *key);
static        int  Inv(const char *key);
static int    iFlag(const char *flagName, int    dflt=0);
static double dFlag(const char *flagName, double dflt=0);
static void Browse(const char *name, TObject *obj);
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
