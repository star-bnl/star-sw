#include <assert.h>
#include <stdlib.h>
#include "StArray.h"
#include "TDatime.h"
#include "TBrowser.h"
#include "StMkDeb.h"

enum {kBelongs = BIT(22)};
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
const static UInt_t kMustBeOne  = kIsOnHeap  | kNotDeleted;
const static UInt_t kMustBeZero = kCanDelete | kObjInCanvas  | kHasUUID
                                | kCannotPick| kNoContextMenu| kZombie;
     if (!fLink || fLink->TestBits(kMustBeZero|kMustBeOne) !=(int)kMustBeOne)
          fLink = 0;
     if (!fLink) R__b << UInt_t(0);
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
   if (del) {delete fV[i]; fV[i]=0;}
   return &(*(fV.erase(fV.begin()+i)));		
}		
//______________________________________________________________________________
TObject** StObjArray::Erase(TObject** fst,TObject** lst,int del)
{
   int ifst = fst-&fV[0];
   int ilst = lst-&fV[0];
   if (del) {for (int i=ifst;i<ilst;i++) {delete fV[i];fV[i]=0;}}
   return &(*(fV.erase(fV.begin()+ifst,fV.begin()+ilst)));		
}		
//______________________________________________________________________________
TObject* StObjArray::find(const char *name) const
{
   int n  = fV.size();
   for (int i=0;i<n;i++) { 
     if (!fV[i]) continue;
     if (!strcmp(name,fV[i]->GetName())) return (TObject*)fV[i];
   }
   return 0;
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
      resize(nobjects);
      TObject *obj;
      for (Int_t i = 0; i < nobjects; i++) {
         b >> obj;  fV[i] =(obj);}
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
       obj = at(i); if (!obj || ((ULong_t)obj)&1) continue;
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
void StObjArray::ls(const char *tit) const
{
  if (!tit) tit ="";
  int n = fV.size();
  Info("ls","%s(%s)[%d]",ClassName(),tit,n);
  for (int i=0;i<n;i++) { 
    TObject *to = fV[i]; if (!to) continue;
    printf("%4d - %p %s(%s)\n",i, (void*)to,to->ClassName(),to->GetName());
  }
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
//______________________________________________________________________________
 void StStrArray::operator=(const StStrArray &a)
{
  int n,i; TObject *sto;
  
  clear();
  n = a.size();
  resize(n);
  for (i=0; i<n; i++)
  {
    sto = a[i];       
    if (sto) sto = ((StObject*)sto)->clone(); 
    sto->ResetBit(kBelongs);
    put_at(sto,i);
  }
} 
//______________________________________________________________________________
StStrArray::StStrArray(const StStrArray &from){ *this = from;}
//______________________________________________________________________________
 void StStrArray::clear()
{ 
  assert(!fV.size() || !fV[0] || ((TObject*)fV[0])->TestBit(TObject::kNotDeleted));
  int n,i;
  
  n = fV.size();
  for (i=0; i<n; i++){
    TObject *to = fV[i];
    if (!to) continue;
    if (to->TObject::TestBit(TObject::kNotDeleted)) {
      delete fV[i];
    } else {
      Warning("clear","Object[%d]=%p is already deleted",i,to);
#if 0
      const char *mk=0;
      mk = StMkDeb::GetName(StMkDeb::GetCurrent());
      if (mk && *mk) printf("*** Now we are in %s ***\n",mk);
      mk = StMkDeb::GetUser(to);
      if (mk && *mk) printf("*** It was deleted in maker %s ***\n",mk);
#endif
//    assert(0);
    }
    fV[i]=0;
  }
  fV.clear();
} 
StStrArray::~StStrArray()
{
  clear(); 
}
//______________________________________________________________________________
void StStrArray::put_at(TObject *obj,int i)
{
  assert(i>=0 && i<int(size()));
  if (obj) {   
     if (fV[i]==obj) return;
     assert(!obj->TestBit(kBelongs) && obj->TestBit(TObject::kNotDeleted));
     obj->SetBit(kBelongs);
  }
  fV[i]=obj;}
//______________________________________________________________________________
void StStrArray::push_back(const TObject * const to)
{
  TObject* obj = (TObject*)to;
  if (obj) {   
     assert(!obj->TestBit(kBelongs) && obj->TestBit(TObject::kNotDeleted));
     obj->SetBit(kBelongs);
  }

  fV.push_back(obj);
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
//______________________________________________________________________________




