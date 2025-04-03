/***************************************************************************
 *
 * $Id: TAttr.cxx,v 1.10 2017/08/08 19:37:37 perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "TAttr.h"
#include "TClass.h"
Int_t TAttr::_debug = 0;
ClassImp(TAttr)
//______________________________________________________________________________
TAttr::TAttr(const char *name)
{
   if (!name) name = "";
   SetName(name);
}
//______________________________________________________________________________
TAttr::~TAttr()
{
  Delete();
}
//_____________________________________________________________________________
/*!
 * SetAttr(const char *key, const char *val)
 * key is a keyname, val is a value
 * key is any character string. Spaces and case are ignored
 * The "." at the beginning is reserved for the frame work only
 * Like ".privilege" makes maker to be priveleged, i.e to skip event
 **/
void TAttr::SetAttr(const char *key, const char *val)
{

   TString tk(key);tk.ToLower();tk.ReplaceAll(" ","");tk.ReplaceAll("\t","");
   if (!val) val ="";
   TString tv(val);tv = tv.Strip(TString::kBoth)     ;tv.ReplaceAll("\t","");
   if (tv == ".remove") {
     TObject *t = FindObject(tk.Data());
     if (t) {Remove(t); delete t;}
   } else {
     TNamed *t = new TNamed(tk.Data(),tv.Data());
     t->SetUniqueID(0);
     AddFirst(t);
   }
   if (_debug)
     Info("SetAttr","(\"%s\",\"%s\",\")",tk.Data(),tv.Data());

}
//_____________________________________________________________________________
int TAttr::SetAttr(const TAttr *att)
{
   TListIter iter(att,kIterBackward);
   int add=0; const TNamed *tn=0; 
   while ((tn = (const TNamed*)iter())) {AddFirst(new TNamed(*tn));add++;}
   return add;
}
//_____________________________________________________________________________
void TAttr::SetAttr(const char *key, Long_t val)
{
   TString ts; ts+=val; SetAttr(key, ts.Data());
}
//_____________________________________________________________________________
void TAttr::SetAttr(const char *key, ULong_t val)
{
   TString ts; ts+=val; return SetAttr(key, ts.Data());
}
//_____________________________________________________________________________
void TAttr::SetAttr(const char *key, double val)
{
   TString ts; ts+=val; return SetAttr(key, ts.Data());
}

//_____________________________________________________________________________
const char *TAttr::SAttr(const char *key) const
{
   TString tey(key);
   tey.ToLower(); tey.ReplaceAll(" ",""); tey.ReplaceAll("\t","");
   TObject *att = FindObject(tey.Data());
   if (att) { // we found the attribut
     int n = att->GetUniqueID();
     att->SetUniqueID(n+1);
     if (n<13) Info("Found","%s = %s\n",att->GetName(),att->GetTitle());
   }
   return (att)? att->GetTitle():"";
}   
//_____________________________________________________________________________
Long_t TAttr::IAttr(const char *key) const
{
   const char *val = SAttr(key);
   if (!val || !val[0]) 	return 0;
   if (isdigit(*val))return strtol(val,0,10);
   return strtoul(val,0,10);
}
//_____________________________________________________________________________
ULong_t TAttr::UAttr(const char *key) const
{
   return (UInt_t)IAttr(key);
}
//_____________________________________________________________________________
double TAttr::DAttr(const char *key) const
{
   const char *val = SAttr(key);
   if (!val) 	return 0;
   if (!val[0]) return 0;
   return strtod(val,0);
}
//_____________________________________________________________________________
void TAttr::PrintAttr() const
{
   if (!First()) return;
   TIter next(this);
   printf("PrintAttr() for %s::%s\n",ClassName(),GetName());
   TObject *object;
   int n=0;
   while ((object = next())) {
      n++;
      printf(" %2d - %s = %s\n",n,object->GetName(),object->GetTitle());
   }
   printf("PrintAttr() ==============================================\n");
}

