#include <assert.h>
#include <stdlib.h>
#include "StArray.h"
#include "TDatime.h"
#include "TBrowser.h"

//______________________________________________________________________________

ClassImp(StObjArray)
//______________________________________________________________________________
Int_t StObjArray::getEntries() const
{
   int sz = size();
   int en = 0;
   for (int i=0;i<sz;i++) 
   {
     TObject *to = fV[i];
     if (!to) 			continue;
     if (to->IsZombie())	continue;
     en++;
   }
   return en;
}
//______________________________________________________________________________
void StObjArray::Streamer(TBuffer &b)
{

   // Stream all objects in the array to or from the I/O buffer.

   Int_t nobjects;
   if (b.IsReading()) {
      Version_t v = b.ReadVersion(); if (v){/*touch*/}
      clear();
      b >> nobjects;
      if (!nobjects) return;
      TObject *obj;
      for (Int_t i = 0; i < nobjects; i++) {
         b >> obj;  push_back(obj);}
   } else {
      b.WriteVersion(IsA());
      nobjects = getEntries();
      b << nobjects;

      for (Int_t i = 0; nobjects; i++) {
         TObject *to = at(i);
         if (to && !to->IsZombie()) {b << to; nobjects--;}
      }
   }
}



//______________________________________________________________________________
void StObjArray::Browse(TBrowser *b)
{
  enum { maxBrowsable =  50 };

   // Browse this collection (called by TBrowser).
   // If b=0, there is no Browse call TObject::Browse(0) instead.
   //         This means TObject::Inspect() will be invoked indirectly
 
   if (!b) return;
   TObject *obj=0;
    
   Int_t counter = 0;
   Int_t totalSize = size();
   for (int i=0; i<totalSize && ++counter <  maxBrowsable ; i++) {
       obj = at(i); if (!obj) continue;
       TString browseName(obj->GetName());
       if (browseName.IsNull()) browseName = obj->ClassName();
       char buffer[100];
       sprintf(buffer,"_%d(%d)",counter,totalSize);
       browseName += buffer;
       b->Add(obj,browseName.Data());
   }
}
//______________________________________________________________________________
Bool_t StObjArray::IsFolder() const 
{ return size();}
   
//______________________________________________________________________________
void StObjArray::random_shuffle(int start,int end)
{
  
  int lst = size();
  if (start >= lst)	return;
  if (end   > lst) 	end = lst;
  ::random_shuffle(begin()+start,begin()+end);

}
//______________________________________________________________________________
ClassImp(StRefArray)
StRefArray::StRefArray(Int_t sz):StObjArray(sz){};
StRefArray::StRefArray(const StRefArray &from):StObjArray(from){};

//______________________________________________________________________________
void StRefArray::Streamer(TBuffer &R__b)
{ 

   // Stream all objects in the array to or from the I/O buffer.

   Int_t nobjects;
   if (R__b.IsReading()) {
      Version_t v = R__b.ReadVersion(); if (v){/*touch*/}
      clear();
      R__b >> nobjects;
      if (!nobjects) return;
      TObject *obj;
      for (Int_t i = 0; i < nobjects; i++) {
         R__b >> obj;  push_back(obj);}
   } else {
      R__b.WriteVersion(IsA());
      nobjects = getEntries();
      R__b << nobjects;

      for (Int_t i = 0; nobjects; i++) {
         TObject *to = at(i);
         if (to && !to->IsZombie()) {R__b.WriteObject(to,1); nobjects--;}
      }
   }
}

//______________________________________________________________________________
ClassImp(StStrArray)
//______________________________________________________________________________
StStrArray::StStrArray(const Char_t *name,Int_t sz):StObjArray(sz)
{ 
//NONMONO  StRegistry::SetColl(this);
  if (name) SetName(name); 
}
//______________________________________________________________________________
StStrArray::StStrArray(Int_t sz):StObjArray(sz){}
//______________________________________________________________________________
 void StStrArray::operator=(const StStrArray &a)
{
  int n,i; TObject *sto;
  
  clear();
  SetName(a.GetName());
  SetIDName(0);
  n = a.size();
  for (i=0; i<n; i++)
  {
    sto = a[i];       
    if (sto) sto = ((StObject*)sto)->clone(); 
    push_back(sto);
  }
} 
//______________________________________________________________________________
 VecTObjIter StStrArray::erase(VecTObjIter fst,VecTObjIter lst)
{
   VecTObjIter it;
   if(!lst) lst = fst;
   for (it = fst; it < lst; it++) delete *it;
   return fV.erase(fst,lst);
}
//______________________________________________________________________________
 void StStrArray::clear()
{ 
  erase(begin(),end());
  fV.clear();
} 
StStrArray::~StStrArray()
{
  clear(); 
//NONMONO StRegistry::RemColl(this);
}

//______________________________________________________________________________
const Char_t *StStrArray::GetIDName() const 
{
  return GetTitle();
}
 
//______________________________________________________________________________
void StStrArray::SetIDName(const Char_t *idname)
{
}
//______________________________________________________________________________
void StStrArray::SetName(const char *name)
{ fName = name;}
//______________________________________________________________________________
const char *StStrArray::GetName() const
{ return fName;}
//______________________________________________________________________________
void StStrArray::Streamer(TBuffer &R__b)
{
   // Stream an object of class StStrArray.
   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      StObjArray::Streamer(R__b);
   } else {
      R__b.WriteVersion(StStrArray::IsA());
      SetIDName(0);
      StObjArray::Streamer(R__b);
   }

}
//______________________________________________________________________________
void StStrArray::makeZombie() 
{
   StObject::makeZombie();
   int n = size();
   for (int i=0;i<n;i++)
   {  StObject *o = (StObject*)at(i); if (o) o->makeZombie();}
}
