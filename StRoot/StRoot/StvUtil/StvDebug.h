#ifndef __STVDEBUG_h_
#define __STVDEBUG_h_
#include "TObject.h"
//------------------------------------------------------------------------------
class TObject;
class TH1;
class StvGrappa;
class StvTrack;
class StvDebug
{
public:	
  StvDebug(){}
static  int Break(int kount);
static  int Break(double x,double y,double z);
static  int Debug() {return mgDebug;};
static void SetDebug(int idb) {mgDebug=idb;};
static void Count(const char *key,double val);
static void Count(const char *key,const char *val);
static void Count(const char *key,const char *val,double valy);
static void Count(const char *key,double val,double left,double rite);
static void Count(const char *key,double valx,double valy);
static void Count(const char *key,double valx, double valy
                                 ,double leftx,double ritex
				 ,double lefty,double ritey);
static void SaveAll();
static void Sumary(int inPage=4);
static void Reset();
static const char *Env(const char *key);
static        int  Inv(const char *key);
static int    iFlag(const char *flagName, int    dflt=0);
static double dFlag(const char *flagName, double dflt=0);
static void Browse(const char *name, TObject *obj);
//		StvGrappa
static void AddGra(double x,double y,double z,int iObj);
static void ClearGra();
static void ShowGra();
static void Show(const StvTrack*);
static void SetActGra(int akt = 1);

private:
static void Draw(int nH,TH1** H);
public:
static int mgDebug;
static StvGrappa *mgGrappa;
static StvGrappa *mgGrappaTmp;


#if 0
ClassDef(StvDebug,0)
#endif
};
#endif
