/***************************************************************************
 *
 * $Id: St.h,v 1.2 2021/06/20 00:50:56 perev Exp $
 *
 * Author: Victor Perev, Jun 2021
 ***************************************************************************
 *
 * Description:
 * Helper for Cint/Cling
 * Method Call - call script from script without full recompilation of all scripts
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef ST_H
#define ST_H
#include "RtypesCore.h"
#include "TObject.h"

class St 
{
public:
enum { kIN = 9 };
static ULong64_t Call(const char* fun);
static ULong64_t Call(const char* fun,const char* C1);
static ULong64_t Call(const char* fun,const char* C1,const char* C2);
static ULong64_t Call(const char* fun,const char* C1,const char* C2,const char* C3);

static ULong64_t Call(const char* fun,int I0);
static ULong64_t Call(const char* fun,int I0,const char* C1);
static ULong64_t Call(const char* fun,int I0,const char* C1,const char* C2);
static ULong64_t Call(const char* fun,int I0,const char* C1,const char* C2,const char* C3);


private:
    St(){};
   ~St(){};
static ULong64_t MyCall();

static int mIn,mCn;
static int mI[kIN+1];
static const char* mC[kIN+1];

ClassDef(St,0)

};

#endif
