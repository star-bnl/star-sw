#include <assert.h>
#include <stdlib.h>
#include "StArray.h"
#include "TDatime.h"
#include "TBrowser.h"
//______________________________________________________________________________
ClassImp(StObjLink)
//______________________________________________________________________________
void StObjLink::Browse(TBrowser* b) {b->Add(fLink);}
//______________________________________________________________________________
void StObjLink::Streamer(TBuffer &R__b)
{

   if (R__b.IsReading()) {
     fLink = 0;
     UInt_t ubj;
     R__b >> ubj ;
     if (!ubj) 	return;
     StProxyUrr *urr = new StProxyUrr(&fLink);
     urr->push_back(ubj);
     StXRefManager::fgManager->AddColl(urr);

   } else {

     if (!fLink || fLink->IsZombie()) R__b << UInt_t(0);
     else        fLink->Ztreamer(R__b);
   }
}

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
TObject** StObjArray::Erase(TObject** it,int del) 
{
   int i = it-&fV[0];
   if (del) delete fV[i];
   return &(*(fV.erase(fV.begin()+i)));		
}		
//______________________________________________________________________________
TObject** StObjArray::Erase(TObject** fst,TObject** lst,int del)
{
   int ifst = fst-&fV[0];
   int ilst = lst-&fV[0];
   if (del) {for (int i=ifst;i<ilst;i++) delete fV[i];}
   return &(*(fV.erase(fV.begin()+ifst,fV.begin()+ilst)));		
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
      b.WriteVersion(Class());
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
  enum { maxBrowsable =  5000};

   // Browse this collection (called by TBrowser).
   // If b=0, there is no Browse call TObject::Browse(0) instead.
   //         This means TObject::Inspect() will be invoked indirectly
 
   if (!b) return;
   TObject *obj=0;
    
   Int_t counter = 0;
   Int_t totalSize = size();
   for (int i=0; i<totalSize && ++counter <  maxBrowsable ; i++) {
       obj = at(i); if (!obj || ((UInt_t)obj)&1) continue;
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
      if (v <=2 || nobjects>0) {	//OLD non split format
        TObject *obj;
        for (Int_t i = 0; i < nobjects; i++) {
         R__b >> obj;  push_back(obj);}
      } else {		//SPLIT format 
        UInt_t ubj;
        nobjects = -nobjects;
        StProxyUrr *urr = new StProxyUrr(this);
        for (Int_t i = 0; i < nobjects; i++) {
         R__b >> ubj ; urr->push_back(ubj);
        }
        StXRefManager::fgManager->AddColl(urr);
      }

   } else {
      R__b.WriteVersion(Class());
      int nobjs = nobjects = getEntries();
      if (StObject::fgTally) nobjs=-nobjs;
      R__b << nobjs;
      if (!nobjects) 	return;
      TObject *to;
      for (Int_t i = 0; nobjects; i++) {
        to = at(i);
        if (!to) 		continue;
        if (to->IsZombie())	continue; 
        nobjects--;
        if (nobjs<0)  {((StObject*)to)->Ztreamer(R__b);}             
        else          { R__b << to;                    }
      }
   }
}

//______________________________________________________________________________
ClassImp(StStrArray)
//______________________________________________________________________________
StStrArray::StStrArray(Int_t sz):StObjArray(sz){}
#if 0
//______________________________________________________________________________
 void StStrArray::operator=(const StStrArray &a)
{
  int n,i; TObject *sto;
  
  clear();
  n = a.size();
  for (i=0; i<n; i++)
  {
    sto = a[i];       
    if (sto) sto = ((StObject*)sto)->clone(); 
    push_back(sto);
  }
} 
#endif //0
//______________________________________________________________________________
StStrArray::StStrArray(const StStrArray &from){ *this = from;}
//______________________________________________________________________________
 void StStrArray::clear()
{ 
  int n,i;
  
  n = fV.size();
  for (i=0; i<n; i++){delete fV[i];}
  fV.clear();
} 
StStrArray::~StStrArray()
{
  clear(); 
}

//______________________________________________________________________________
void StStrArray::Streamer(TBuffer &R__b)
{
   // Stream an object of class StStrArray.
   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      StObjArray::Streamer(R__b);
      if (R__v >=3) 
         StXRefManager::fgManager->AddColl(this);
   } else {
      R__b.WriteVersion(Class());
      StObjArray::Streamer(R__b);
         StXRefManager::fgManager->AddColl(this);//temporaryVP
   }

}
//______________________________________________________________________________
void StStrArray::makeZombie(int flg) 
{
   StObject::makeZombie(flg);
   int n = size();
   for (int i=0;i<n;i++)
   {  StObject *o = (StObject*)at(i); if (o) o->makeZombie(flg);}
}
