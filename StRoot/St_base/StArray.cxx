#include <assert.h>
#include <stdlib.h>
#include "StArray.h"
#include "TDatime.h"
#include "TBrowser.h"

StObjArray StRegistry::fgColl[2];
StRegistry RegInit;
//______________________________________________________________________________

ClassImp(StRegistry)
//______________________________________________________________________________
StRegistry::StRegistry()
{
  assert(!fgColl[0].size());
  assert(!fgColl[1].size());
  fgColl[0].push_back(0);
  fgColl[1].push_back(0);

}
//______________________________________________________________________________
void StRegistry::CheckIn(const StObjArray *Arr)
{
  StObjArray *arr = (StObjArray*)Arr;
  int kind = arr->IsArr()-1;
  assert(kind>=0);
  StObjArray &book = fgColl[kind];
  int ifree = (int)book[0];
  if (ifree) {
    ifree /=2; book[0] = book[ifree];
    book[ifree] = arr;
  } else {
    book.push_back(arr);
    ifree = book.size()-1;
  }
  UInt_t u = 0xffff & (ifree);
  arr->SetUniqueID(u);
}

//______________________________________________________________________________
void StRegistry::CheckIn(const TObject *Obj, const StObjArray *Arr)
{
  if (!Obj) return;
  StObjArray *arr = (StObjArray*)Arr;
  StObject   *obj = (StObject*)Obj;
  UInt_t uarr,uobj;
  uobj = obj->GetUniqueID();
  uarr = arr->GetUniqueID();
  int kArr = arr->IsArr();
  int kObj = obj->IsArr();
    
  if (kObj==0 && kArr==1) {	//regular case
    assert(!uobj);
    uobj = 0xffff & uarr;
  } else     {	//weak case
  
    if ( uobj&0xffff0000 ) {	//was already registered
      if ( (uobj&0xffff0000)==0xffff0000)	return;
      uobj |= 0xffff0000; 			return;
    }
    uobj |= uarr << 17 ;
    if (kArr==2) uobj |= 010000;
  } 
  
  obj->SetUniqueID(uobj);
}
//______________________________________________________________________________
Int_t StRegistry::Where(const TObject *Obj, StObjArray *arr, Int_t &idx)
{
  StObject *obj = (StObject*)Obj;
  
  StObjArray &book = fgColl[0];
  int nbook = book.size();
  idx = -1; arr = 0;
  UInt_t uobj = 0xffff & obj->GetUniqueID();
  if (!uobj) return -1;
  int kind = obj->IsArr();
  switch (kind) {
  
    case 0:
    for (int ibook=(int)uobj;ibook < nbook; ibook += 0x10000)
    {
      arr = (StObjArray*)fgColl[0][ibook];
      if (! arr)			continue;
      if (((ULong_t)arr)&1)		continue; 
      if (arr->TestBit(kInvalidObject))	continue;
      for (StObjArrayIter it = arr->begin(); it !=arr->end(); it++)
      {
        if (obj != (StObject*)(*it))	continue;
        idx = it - arr->begin();
        return 0;
      }
    } 
    break;
    
    case 1:
    case 2:
    arr = fgColl+kind-2;
    for (int ibook=(int)uobj;ibook < nbook; ibook += 0x10000)
    {
      if (obj != (*arr)[ibook]) 	continue;      
      idx = ibook;
      return kind;
    } 
 }       
    arr = 0; return -1;
}    

//______________________________________________________________________________
void StRegistry::Remove(const TObject *Obj)  
{
  StObject *obj = (StObject*)Obj;
  StObjArray *arr,*a;
  TObject *o;
  Int_t idx,knd,k;

  knd = Where(obj,arr,idx);
  if (idx >0 ) { 	//found it
    (*arr)[idx] = 0;
    if (knd) { 
      (*arr)[idx] = (*arr)[0];
      (*arr)[0  ] = (TObject*)((idx<<1)|1);
  }  }
  UInt_t u = obj->GetUniqueID()>>16;
  if (!u) return;
  int ka = 0,na=2,start=1,step =1;
  if (u!=0xffff) {
    if (u&1) ka = 1; na = ka+1; start = u>>1; step = 0x1000;}
    
  for (k=ka; k < na; k++) {
    arr = fgColl+k;
    for (StObjArrayIter it = arr->begin()+start; it < arr->end(); it+=step) {
      o = *it;
      if (! o || 1&((ULong_t)o))	continue;
      if (o->TestBit(kInvalidObject))	continue;
      a = (StObjArray*)o;      
      for (StObjArrayIter jt = a->begin(); jt != a->end(); jt++) {
        if (obj != *jt) continue;
        *jt = 0;
  } } }

}
//______________________________________________________________________________
void StRegistry::ls(int nmax) 
{
   printf("\n StRegistry ls \n\n");    
   fgColl[0].ls();
   fgColl[1].ls();
}
//______________________________________________________________________________
void StRegistry::Streamer(TBuffer &b){assert(0);}  

//______________________________________________________________________________

ClassImp(StObjArray)

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
      b.WriteVersion(IsA());//
      nobjects = size();
      b << nobjects;

      for (Int_t i = 0; i < nobjects; i++) {
         b << (*this)[i];
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
Bool_t StObjArray::IsFolder(){ return size();}
   
//______________________________________________________________________________
void StObjArray::random_shuffle(int start,int end)
{
  
  int lst = size();
  if (start >= lst)	return;
  if (end   > lst) 	end = lst;
  ::random_shuffle(begin()+start,begin()+end);

}
//______________________________________________________________________________
void StObjArray::ls(int nmax) const
{
   printf("\n Container %s::%s(%d)\n",ClassName(),GetName(),size());    
   int num = size();
   if (num > nmax) num = nmax;
   for (int i=0; i<num; i++)
   {
     const TObject *to = at(i);
     if (!to) 	continue;
     printf("%8d - ",i);
     if ((UInt_t)to <1000000 || ((UInt_t)to)&1 ) 
       printf("%10d\n",(int)to);
     else {
       printf("%p  %s::%s 0x%8x\n",to,to->ClassName(),to->GetName(),to->GetUniqueID());
     }
   }
   printf("\n");
}       




//______________________________________________________________________________
ClassImp(StRefArray)
StRefArray::StRefArray(Int_t sz):StObjArray(sz)                
{  
  SetBit(kStRRR);
  StRegistry::CheckIn(this);
}
StRefArray::StRefArray(const StRefArray &from):StObjArray(from)
{
  SetBit(kStRRR);
  StRegistry::CheckIn(this);
};

//______________________________________________________________________________
void StRefArray::Streamer(TBuffer &R__b)
#ifdef NONMONO
{
   // Stream all objects in the array to or from the I/O buffer.  

   Int_t nobjects,i,objidx,colidx;
   TObject *obj;

   colidx = 0;
   char cbuf[100];
   if (R__b.IsReading()) {
      R__b.ReadVersion();   // Version_t v = R__b.ReadVersion();
      R__b >> nobjects;
      Clear();
      StStrArray *ar =0;
      for (i = 0; i < nobjects; i++) {
        R__b >> objidx;
        if (!objidx) 	continue;
        if (objidx<0) {	//Read the name
          objidx = -objidx;
          R__b.ReadString(cbuf,100);
          colidx = StRegistry::GetColl(cbuf);
//          StStrArray *ar = 0;
//          R__b >> (TObject *&)ar;
          colidx = StRegistry::GetColl(cbuf);
          if (!colidx) Warning("Streamer"," StrArray %s NOT found ",cbuf );
          if (!colidx) 	continue;
          ar = StRegistry::GetColl(colidx); 
          assert(ar);
        }
        obj = ar->At(objidx-1);
        assert(obj);
        Add(obj);
      }


   } else {

      R__b.WriteVersion(StRefArray::IsA());
      nobjects = GetLast()+1;
      R__b << nobjects;
      int lastColl = -1999;
//		fill array of different collections
      for (i=0;i< nobjects; i++) {
        colidx = lastColl; objidx = 0; obj = At(i);            
        if (obj) StRegistry::Ident(obj->GetUniqueID(),colidx,objidx);
        if (colidx != lastColl) objidx = -objidx;	//new Str Collection
        lastColl = colidx;
        R__b << objidx; 
        if (objidx < 0) {
          R__b << StRegistry::GetCollName(colidx);
          StStrArray *ar = StRegistry::GetColl(colidx);
          assert(ar);
//        R__b << ar;               
        }
      }
   }
}
#endif 
#ifndef NONMONO
{ StObjArray::Streamer(R__b);}
#endif

//______________________________________________________________________________
ClassImp(StStrArray)
//______________________________________________________________________________
StStrArray::StStrArray(const Char_t *name,Int_t sz):StObjArray(sz)
{ 
//NONMONO  StRegistry::SetColl(this);
  if (name) SetName(name); 
  StRegistry::CheckIn(this);
}
//______________________________________________________________________________
StStrArray::StStrArray(Int_t sz):StObjArray(sz)
{
  StRegistry::CheckIn(this);
}
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
//______________________________________________________________________________
//NONMONO void StStrArray::Book(TObject* obj,int idx)
//NONMONO{ 
//NONMONO   Int_t kolidx,colidx;
//NONMONO   StRegistry::Ident(GetUniqueID(),kolidx,colidx);
//NONMONO   assert(kolidx==1999);
//NONMONO   obj->SetUniqueID(StRegistry::Ident(colidx,idx));
//NONMONO}

//______________________________________________________________________________
//void StStrArray::ShowMembers(TMemberInspector &, char *){}
//______________________________________________________________________________
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
#if 0
  if (idname) 
  {
    SetTitle(idname); 
  } else if (!*GetTitle()) {

    char buf[100]; strcpy(buf,ClassName()); strcat(buf," ");
    
    sprintf(buf+strlen(buf),"%p", this);
    TDatime dt;   
    sprintf(buf+strlen(buf),"%x", dt.Get()); 
    SetTitle(buf);
  }
#endif /*0*/
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
// $Id: StArray.cxx,v 1.30 2000/07/30 01:40:11 perev Exp $
// $Log: StArray.cxx,v $
// Revision 1.30  2000/07/30 01:40:11  perev
// StMem class added
//
// Revision 1.29  2000/07/03 02:07:58  perev
// StEvent: vector<TObject*>
//
// Revision 1.28  2000/06/19 01:28:25  perev
// STL StEvent
//
//
