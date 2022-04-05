#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "StFileI.h"

ClassImp(StFileI)
//______________________________________________________________________________
StUKey::StUKey(const char *name,UInt_t *uk,int nk)
{
  if (name) SetName(name);
  SetUrr(uk,nk);
}
//______________________________________________________________________________
StUKey::StUKey(const char *name,UInt_t uk)
{
  if (name) SetName(name);
  SetUrr(&uk,1);
}
//______________________________________________________________________________
StUKey::StUKey(UInt_t uRun,UInt_t uEvent)
{
  UInt_t u[2]; u[0]=uRun; u[1]=uEvent;
  int n = 1; if (u[1]) n=2;
  SetUrr(u,n);
}
//______________________________________________________________________________
void StUKey::SetUrr(const UInt_t *uk,int nk)
{
  int n;
  fNUrr = 1;
  fUrr[0] = 0;
  if (!uk) return;
  for (n=1;n<nk && uk[n]; n++){}
  fNUrr = n;
  memcpy(fUrr,uk,nk*sizeof(UInt_t));
}
//______________________________________________________________________________
StUKey &StUKey::operator=( const StUKey &from)
{
  SetName(from.GetName());
  SetUrr(from.fUrr,from.fNUrr);
  return *this;
}
//______________________________________________________________________________
StUKey &StUKey::operator=( UInt_t from)
{
  SetUrr(&from,1);
  return *this;
}
//______________________________________________________________________________
StUKey &StUKey::operator=( const char *from)
{
  SetName(from);
  return *this;
}
//______________________________________________________________________________
void  StUKey::Update( const StUKey &from, const char *name)
{
  SetUrr(from.fUrr,from.fNUrr);
  if (name) SetName(name);
}
//______________________________________________________________________________
TString StUKey::GetKey() const
{
  char ubuf[12];
  TString tk(fName);
  for (int i=0;i<fNUrr;i++){
   tk +=".";
   sprintf(ubuf,"%010u",fUrr[i]);
   tk +=ubuf;}
  return tk;
}
//______________________________________________________________________________
void StUKey::SetKey(const char *key)
{
  const char *cc;
  int n = strchr(key,'.') - key;
  assert(n>0 && n<100);
  fName.Replace(0,999,key,n);
  for (fNUrr = 0,cc=key+n;*cc=='.'; cc+=11)
    fUrr[fNUrr++] = strtoul(cc+1,0,10);
}
//______________________________________________________________________________
UInt_t  StUKey::GetSum() const
{
  UInt_t s = fUrr[0];
  for (int i=1;i<fNUrr;i++) s^=fUrr[i];
  return s;
}
//______________________________________________________________________________
StUKey StFileI::GetNextEvent()
{
  UInt_t u[9] = {0,0,0,0,0,0,0,0,0};
  int ret = GetNextEvent(u);
  if (ret) u[0]=kUMAX;
  StUKey uk(0,u,9);
  return uk;
}

