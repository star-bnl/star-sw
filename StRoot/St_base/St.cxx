/***************************************************************************
 *
 * $Id: St.cxx,v 1.5 2021/06/21 13:21:40  Perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include "St.h"
#include "TString.h"
#include "TROOT.h"
int St::mIn,St::mCn;
int St::mI[kIN+1];
const char* St::mC[kIN+1];

//_______________________________________________________________________________
ULong64_t St::Call(const char* fun)
{ 
  mIn=0; mCn=0; mC[kIN]=fun;
  return MyCall();
}
//_______________________________________________________________________________
ULong64_t St::Call(const char* fun,const char* C0)
{ 
  mIn = 0; mCn=1; mC[kIN]=fun;
  mC[0] = C0;
  return MyCall();
}
//_______________________________________________________________________________
ULong64_t St::Call(const char* fun,const char* C0,const char* C1)
{ 
  mIn = 0; mCn=2; mC[kIN]=fun;
  mC[0] = C0; mC[1] = C1;
  return MyCall();
}
//_______________________________________________________________________________
ULong64_t St::Call(const char* fun,const char* C0,const char* C1,const char* C2)
{ 
  mIn = 0; mCn=3; mC[kIN]=fun;
  mC[0] = C0; mC[1] = C1; mC[2] = C2;
  return MyCall();
}
//_______________________________________________________________________________
ULong64_t St::Call(const char* fun,int I0)
{ 
  mIn = 1; mCn=0; mC[kIN]=fun;
  mI[0] = I0;
  return MyCall();
}
//_______________________________________________________________________________
ULong64_t St::Call(const char* fun,int I0,const char* C0)
{ 
  mIn = 1; mCn=1; mC[kIN]=fun;
  mC[0] = C0; 
  mI[0] = I0;
  return MyCall();
}
//_______________________________________________________________________________
ULong64_t St::Call(const char* fun,int I0,const char* C0,const char* C1)
{ 
  mIn = 1; mCn=2; mC[kIN]=fun;
  mC[0] = C0; mC[1] = C1;
  mI[0] = I0;
  return MyCall();
}
//_______________________________________________________________________________
ULong64_t St::Call(const char* fun,int I0,const char* C0,const char* C1,const char* C2)
{ 
   mIn = 1; mCn=3; mC[kIN]=fun;
   mC[0] = C0; mC[1] = C1; mC[2] = C2;
   mI[0] = I0;
  return MyCall();
}
//_______________________________________________________________________________
ULong64_t St::MyCall()
{
  TString ts(".x "); ts += mC[kIN]; ts+="(";
  for (int ii = 0; ii<mIn; ii++)
  {
    ts += mI[ii]; ts +=",";
  }
  
  for (int ic = 0; ic<mIn; ic++)
  {
    ts += "(const char*)0x";
    ts += TString::ULLtoa((ULong64_t)mC[ic],16); ts +=",";
  }
  ts[ts.Length()-1] = ')';
  return (ULong64_t)gROOT->ProcessLine(ts.Data());  
}
