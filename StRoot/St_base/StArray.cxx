#include <assert.h>
#include <stdlib.h>
#include "StArray.h"
#include "TDatime.h"
#include "TBrowser.h"
#include "TRandom.h"

//TObjArray *StRegistry::fgReg  = 0;
//Int_t      StRegistry::fgFree = 0;
//______________________________________________________________________________

#if 0
ClassImp(StRegistry)
//______________________________________________________________________________
void StRegistry::Clear()
{
  if (fgReg) fgReg->Clear();
  fgFree = 0;
}
//______________________________________________________________________________
UInt_t StRegistry::Ident(Int_t colidx,Int_t objidx)
{
  UInt_t u;
  int kase = 0;
  if (colidx > 0x7fff  || objidx > 0xffff) kase =1;
  if (kase && colidx > objidx ) kase +=2;
  switch (kase) {

    case 0:	// col=15bit obj=16bit
    u = (colidx<<17)|(objidx<<1)|0;
    break;

    case 1: 	// col=12bit obj=18bit
    assert(colidx <   0xfff);
    assert(objidx < 0x3ffff);
    u = (colidx<<20)|(objidx<<2)|kase;        
    break;

    case 3: 	// col=18bit obj=12bit
    assert(colidx < 0x3ffff);
    assert(objidx <   0xfff);
    u = (colidx<<14)|(objidx<<2)|kase;        
    break;

    default: assert(0);
  }
//  Int_t colqq,objqq;
//  StRegistry::Ident(u,colqq,objqq);
//  assert(colqq==colidx && objqq == objidx);
  return u;

}
//______________________________________________________________________________
void StRegistry::Ident(UInt_t ident,Int_t &colidx,Int_t &objidx)
{
  int kase = ident&3;
  switch (kase) {

    case 0:; case 2:
      objidx = (ident>>1)&0xffff; 
      colidx = ident>>17;
      { UInt_t u = StRegistry::Ident(colidx,objidx);
        assert(u==ident); }
      
      return;

    case 1:;
      objidx = (ident>>2)&0x3ffff;
      colidx = ident>>20;
      return;
    case 3:;  
      objidx = (ident>>2)&0xfff;
      colidx = ident>>14;
      return;
    default: assert(0);  
  }
}  
//______________________________________________________________________________
Int_t StRegistry::GetNColl(){return (fgReg) ? fgReg->GetLast()+1:0;}				// Number of collections
//______________________________________________________________________________
void StRegistry::Streamer(TBuffer &){assert(0);}

//______________________________________________________________________________
Int_t StRegistry::SetColl(StStrArray *coll) 		
// Register new container
{
  Int_t n;
  assert(coll);
  if (!fgReg) fgReg = new TObjArray(100);
  
  if (!fgFree) {	//No free places
    fgReg->AddLast(coll);
    n = fgReg->GetLast()+1;
  } else {
    n = fgFree;
    fgFree = -(int)fgReg->At(n-1);
    fgReg->AddAt(coll,n-1);
  }
  coll->SetUniqueID(StRegistry::Ident(1999,n));
  
  return n;
}
//______________________________________________________________________________
void StRegistry::RemColl(StStrArray *coll)  
{
  UInt_t id;
  Int_t colIdx,objIdx;
  assert(fgReg); 		//Reg collection does not exist
  id = coll->GetUniqueID();
  assert(id);			//Collection removed twice
  StRegistry::Ident(id, colIdx, objIdx);
  assert(objIdx); 
  assert(colIdx==1999);		//No collection for collection (???)
  assert( fgReg->At(objIdx-1)==coll);
  fgReg->RemoveAt(objIdx-1);
  if (objIdx-1 < fgReg->GetLast()) {
    fgReg->AddAt((TObject*)(-fgFree),objIdx-1);
    fgFree = objIdx;
  }

  coll->SetUniqueID(0);
  
}
//______________________________________________________________________________
Int_t StRegistry::GetColl(const char *collname)  
// get index of container
{
  assert(collname);
  if (!fgReg) return 0;

  int i=0,n=fgReg->GetLast();  
  for (i=0;i<=n;i++) 
  {
    StStrArray *koll = (StStrArray*)fgReg->At(i);
    if (!koll) continue;
    if(strcmp(collname,koll->GetIDName())) continue;
    return (i+1);
  }
  return 0;
}
//______________________________________________________________________________
StStrArray *StRegistry::GetColl (Int_t idx )
// get collection  by index
{ assert(fgReg); return (StStrArray*)fgReg->At(idx-1);} 

//______________________________________________________________________________
const char *StRegistry::GetCollName (Int_t idx ) 
// get name of collection by index
{
 StStrArray *coll = (StStrArray*)GetColl(idx); assert(coll); 
 return coll->GetIDName();
}
 
//______________________________________________________________________________
void StRegistry::List() 
{

  printf("\n\tList of StRegistry\n");
  if (!fgReg) return;
  int n = fgReg->GetLast();
  for (int i=1;i<=n;i++) {
    StObjArray *coll = GetColl(i);
    if (!coll) continue;
    int size = coll->GetSize();
    printf("Collection.%02d = %s Size = %d Address=%p\n"
    ,i,coll->GetName(),size,coll);   
  }
  printf("\n");
}
#endif /*0*/
//______________________________________________________________________________

ClassImp(StObjArray)

//______________________________________________________________________________
void StObjArray::Streamer(TBuffer &b)
{

   // Stream all objects in the array to or from the I/O buffer.

   Int_t nobjects;
   if (b.IsReading()) {
      Version_t v = b.ReadVersion(); if (v){/*touch*/}
      fName.Streamer(b);
      clear();
      b >> nobjects;
      if (!nobjects) return;
      TObject *obj;
      for (Int_t i = 0; i < nobjects; i++) {
         b >> obj;  push_back(obj);}
   } else {
      b.WriteVersion(IsA());
      fName.Streamer(b);
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
  enum { maxBrowsable =  10 };

   // Browse this collection (called by TBrowser).
   // If b=0, there is no Browse call TObject::Browse(0) instead.
   //         This means TObject::Inspect() will be invoked indirectly
 
   if (!b) return;
   TObject *obj;
    
   Int_t counter = 0;
   Int_t totalSize = size();
   for (int i=0; i<totalSize && ++counter <  maxBrowsable ; i++) {
       TString browseName = obj->GetName();
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
  static TRandom *ran = 0;
  if(!ran) ran = new TRandom();
  int lst = size()-1;
  if (start > lst)	return;
  if (end   > lst) 	end = lst;
  if (start >= end)	return;

  for (int i=start; i<end; i++) {
    int j = i + (int)(ran->Rndm()*(end-i+1)); 
    if (i==j) 	continue;
    TObject *ti = (*this)[i];
    TObject *tj = (*this)[j];
    (*this)[j] = ti;
    (*this)[i] = tj;
  }    
}
//______________________________________________________________________________
ClassImp(StRefArray)

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
StStrArray::StStrArray(const Char_t *name)
{ 
//NONMONO  StRegistry::SetColl(this);
  if (name) SetName(name); 
}
//______________________________________________________________________________
StStrArray::StStrArray(const StStrArray &from )
{ 
  *this = from;
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
   return VecTObj::erase(fst,lst);
}
//______________________________________________________________________________
 void StStrArray::clear()
{ 
  erase(begin(),end());
  VecTObj::clear();
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
// $Id: StArray.cxx,v 1.28 2000/06/19 01:28:25 perev Exp $
// $Log: StArray.cxx,v $
// Revision 1.28  2000/06/19 01:28:25  perev
// STL StEvent
//
//
