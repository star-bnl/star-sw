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
  ((TObject*)mk)->SetUniqueID(id+1);
  return id;
} 

//_____________________________________________________________________________
int StMkDeb::SetCurrent(const TObject *mk)
{
  if (fgCurr<-1) Ready();
  fgCurr = fgArr->IndexOf(mk);
  return fgCurr;
}

//_____________________________________________________________________________
int StMkDeb::SetCurrent(const StMaker *mk)
{
  if (fgCurr<-1) Ready();
  fgCurr = int(((TObject*)mk)->GetUniqueID())-1;
  assert(fgCurr>=0);
  return fgCurr;
}

//_____________________________________________________________________________
void StMkDeb::Ready()
{
  if (fgCurr>-2003) return;
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
   int lst = fgArr->GetLast();
   if (id < 0  ) return "TOO_NEGATIVE ODHAKO";
   if (id > lst) return "TOO_BIG ODHAKO";
   return fgArr->At(id)->GetName();
}
//_____________________________________________________________________________
int StMkDeb::SetUser(TObject *us) 
{
  us->SetUniqueID(fgCurr+1);
  return fgCurr;
}
//_____________________________________________________________________________
const char *StMkDeb::GetUser(const TObject *us)  
{
   return GetName(us->GetUniqueID()-1);
}







