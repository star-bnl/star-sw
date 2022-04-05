#include <stdio.h>
#include <assert.h>
#include "TObject.h"
#include "TNamed.h"
#include "TObjArray.h"
#include "TString.h"
#include "StMkDeb.h"

int StMkDeb::fgCurr  = -2003;
int StMkDeb::fgStage = 0;
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
void StMkDeb::Cancel(const TObject *mk)
{
  if (!fgArr) return;
  if (!mk   ) return;
  int i = fgArr->IndexOf(mk);
  fgArr->AddAt(0,i);
} 
//_____________________________________________________________________________
void StMkDeb::Cancel(StMaker *mk)
{
  if (!fgArr) return;
  if (!mk   ) return;
  int id = int(((TObject*)mk)->TObject::GetUniqueID())-1;
  if (id<0) return;
  ((TObject*)mk)->TObject::SetUniqueID(0);
  TObject *to = fgArr->At(id);
  TObject *tmk= (TObject *) mk;
  if (to != tmk)  delete to;
  fgArr->AddAt(0,id);
} 
//_____________________________________________________________________________
int StMkDeb::SetCurrent(const TObject *mk, int kind)
{
  if (fgCurr<-1) Ready();
  fgCurr = kind+100*(fgArr->IndexOf(mk));
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
void StMkDeb::SetStage(int stage)
{
   fgStage = stage;
}
//_____________________________________________________________________________
void StMkDeb::Ready()
{
  if (fgCurr>-2003) 	return;
  if (!fgArr) 		return;
  fgCurr=0;
  int lst = fgArr->GetLast();
  for (int i=0;i<=lst; i++) {
    TObject *to = fgArr->At(i);
    if (!to) continue;
    if (!to->TObject::TestBit(TObject::kNotDeleted)) {fgArr->AddAt(0,i); continue;}
    if (to->IsA() == TNamed::Class()) continue;
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
   if (!fgArr) 	return 0;
   static TString ts;
   int lst = fgArr->GetLast();
   if (id     < 0  ) return "";
   int stage = id%100; id/=100;
   int kind  = id%100; id/=100;
   if (id > lst) return "";
   ts = fgArr->At(id)->GetName();
   ts += " kind=" ;ts += kind ;
   ts += " stage=";ts += stage;
   return ts.Data();
}
//_____________________________________________________________________________
int StMkDeb::SetUser(TObject *us) 
{
  if (!fgArr) 	return 0;
  us->TObject::SetUniqueID(fgCurr*100+fgStage+1);
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




