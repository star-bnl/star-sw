#include <stdio.h>
#include <assert.h>
#include "TObject.h"
#include "TNamed.h"
#include "TObjArray.h"
#include "TString.h"
#include "StMkDeb.h"

int StMkDeb::fgCurr = -2003;
TObjArray *StMkDeb::fgArr=0;

//_____________________________________________________________________________
int StMkDeb::Register  (const TObject *mk)
{
  if (!fgArr) fgArr = new TObjArray(100);
  fgArr->Add((TObject*)mk);
  return fgArr->GetLast();
}

//_____________________________________________________________________________
int StMkDeb::Register  (StMaker *mk)
{
  int id = Register  ((TObject*)mk);
  ((TObject*)mk)->TObject::SetUniqueID(id+1);
  return id;
} 

//_____________________________________________________________________________
int StMkDeb::SetCurrent(const TObject *mk, int kind)
{
  if (fgCurr<-1) Ready();
  fgCurr = (fgArr->IndexOf(mk))*100+kind;
  return fgCurr;
}

//_____________________________________________________________________________
int StMkDeb::SetCurrent(const StMaker *mk, int kind)
{
  if (fgCurr<-1) Ready();
  fgCurr = int(((TObject*)mk)->TObject::GetUniqueID())-1;
  assert(fgCurr>=0);
  fgCurr = fgCurr*100+kind;
  return fgCurr;
}

//_____________________________________________________________________________
int StMkDeb::SetCurrent(int curr)
{
  if (fgCurr<-1) Ready();
  if (curr>=0) fgCurr = curr;
  return fgCurr;
}

//_____________________________________________________________________________
void StMkDeb::Ready()
{
  if (fgCurr>-2003) return;
  fgCurr=0;
  int lst = fgArr->GetLast();
  for (int i=0;i<=lst; i++) {
    TObject *to = fgArr->At(i);
    TString ts(to->ClassName());
    ts += "::";
    ts += to->GetName();
    ts += "#";
    ts += i;
    TNamed *tn = new TNamed(ts.Data(),"");
    fgArr->AddAt(tn,i);
  }
}
//_____________________________________________________________________________
const char *StMkDeb::GetName(int id)  
{
   static TString ts;
   int lst = fgArr->GetLast();
   if (id     < 0  ) return "";
   if (id/100 > lst) return "";
   ts = fgArr->At(id/100)->GetName();
   ts +="kind=";
   ts += (id%100);
   return ts.Data();
}
//_____________________________________________________________________________
int StMkDeb::SetUser(TObject *us) 
{
  us->TObject::SetUniqueID(fgCurr+1);
  return fgCurr;
}
//_____________________________________________________________________________
const char *StMkDeb::GetUser(const TObject *us)  
{
   return GetName(us->TObject::GetUniqueID()-1);
}
//_____________________________________________________________________________
void StMkDeb::Pause(const char *tit)  
{
  if (tit && *tit) printf("Pause %s\n",tit);
}
//_____________________________________________________________________________
int  StMkDeb::GetCurrent(){return fgCurr;} 




