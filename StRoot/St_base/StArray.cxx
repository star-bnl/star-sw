#include <assert.h>
#include <stdlib.h>
#include "StArray.h"
#include "TDatime.h"
#include "TBrowser.h"
#include "TRandom.h"

TObjArray *StRegistry::fgReg  = 0;
Int_t      StRegistry::fgFree = 0;
//______________________________________________________________________________
ClassImp(StTObjArray)
//______________________________________________________________________________
void StTObjArray::Streamer(TBuffer &b)
{
  char nonEmpty;
  if (b.IsReading()){
    Clear(); fName = ""; fArr->SetName("");
    b >> nonEmpty;
    if (nonEmpty==0) return; 
    fName.Streamer(b);
    if (nonEmpty==1) return; 
    fArr->Streamer(b);
  } else { 
    nonEmpty = 0;
    if (!fName.IsNull()) 	nonEmpty = 1;    
    if (fArr->GetEntries()) 	nonEmpty = 2;
    b << nonEmpty;
    if (nonEmpty==0) 		return;
    fName.Streamer(b);
    if (nonEmpty==1) 		return;
    fArr->Streamer(b);
}
}
//______________________________________________________________________________
void StTObjArray::SetTitle(const char *title)
{ fArr->SetName(title);}
//______________________________________________________________________________
const char *StTObjArray::GetTitle() const
{ return fArr->GetName();}

//______________________________________________________________________________
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
//______________________________________________________________________________

ClassImp(StObjArray)
UInt_t  StObjArray::size() const {return GetLast()+1;}
//______________________________________________________________________________
void    StObjArray::resize(Int_t num){Resize(num);}
//______________________________________________________________________________
Int_t 	StObjArray::capacity() const {return Capacity();}
//______________________________________________________________________________
TObject* StObjArray::Back() const {return Last();}
//______________________________________________________________________________
TObject* StObjArray::Front() const {return First();}
//______________________________________________________________________________
Bool_t 	StObjArray::empty() const {return IsEmpty();}
//______________________________________________________________________________
TObject** StObjArray::GetCell(Int_t idx) const{return &((*fArr)[idx]);}

//______________________________________________________________________________
void StObjArray::Streamer(TBuffer &b)
{StTObjArray::Streamer(b);}
//______________________________________________________________________________
const TIterator *StObjArray::Begin() const
{ 
  static  TIterator *iter=0;
  if (iter) delete iter;
  iter =  MakeIterator();
  return iter;
}
//______________________________________________________________________________
void StObjArray::Browse(TBrowser *b)
{
  enum { maxBrowsable =  10 };

   // Browse this collection (called by TBrowser).
   // If b=0, there is no Browse call TObject::Browse(0) instead.
   //         This means TObject::Inspect() will be invoked indirectly
 
   if (!b) return;
   TIter next(fArr);
   TObject *obj;
    
   Int_t counter = 0;
   Int_t totalSize = GetEntries();
   while ((obj = next()) && ++counter <  maxBrowsable ) {
       TString browseName = obj->GetName();
       char buffer[100];
       sprintf(buffer,"_%d(%d)",counter,totalSize);
       browseName += buffer;
       b->Add(obj,browseName.Data());
   }
}
//______________________________________________________________________________
Bool_t StObjArray::IsFolder(){ return GetEntries();}
   
//______________________________________________________________________________
const TIterator *StObjArray::End() const
{ 
  static  TIterator *iter=0;  
  if (iter) delete iter; 
  iter =  MakeIterator();
  ((StObjArrayIter*)iter)->SetCursor(GetLast()+1);
  return iter;
}
//______________________________________________________________________________
void StObjArray::Resize(Int_t num)
{
  if (num > Capacity() ) Expand(num+Capacity());
  if (num <0 ) num = 0;
  SetLast(num-1);
}
TIterator* StObjArray::MakeIterator(Bool_t dir) const
{
  return (TIterator*)(new StObjArrayIter(this,dir));
}
//______________________________________________________________________________
StObjArrayIter *StObjArray::Erase(StObjArrayIter *fist,StObjArrayIter *last,int flag)
{
    
    static StObjArrayIter *iteret = 0;
    
    int idxf = fist->GetCursor();
    int idxl = idxf+1;
    if (last) idxl = last->GetCursor();      
    if (idxl > GetLast()+1)  idxl  = GetLast()+1; 
    if (idxf >= idxl) return 0;
    for (int i=idxf; i<idxl; i++) {
      TObject *to = At(i); 
      if (flag && to) delete to;
      AddAt(0,i);    
    }
    Compress();
    delete iteret;
    iteret = (StObjArrayIter*)MakeIterator();
    iteret->SetCursor(idxf);
    
    return iteret;
}

//______________________________________________________________________________
void StObjArray::random_shuffle(int start,int end)
{
  static TRandom *ran = 0;
  if(!ran) ran = new TRandom();
  int lst = GetLast();
  if (start > lst)	return;
  if (end   > lst) 	end = lst;
  if (start <= end)	return;

  for (int i=start; i<end; i++) {
    int j = i + (int)(ran->Rndm()*(end-i+1)); 
    if (i==j) 	continue;
    TObject *ti = At(i);
    TObject *tj = At(j);
    AddAt(ti,j);
    AddAt(tj,i);
  }    
}
//______________________________________________________________________________
void random_shuffle(StObjArrayIter &start,StObjArrayIter &end)
{
  StObjArray *coll =  (StObjArray *)start.GetCollection();
  StObjArray *koll =  (StObjArray *)end  .GetCollection();
  assert(coll==koll);
  int istart = start.GetCursor();
  int iend   = end.  GetCursor();
  coll->random_shuffle(istart,iend);
}
//______________________________________________________________________________
ClassImp(StObjArrayIter)
//______________________________________________________________________________
// void StObjArrayIter::ShowMembers(TMemberInspector &, char *){}
//______________________________________________________________________________
void StObjArrayIter::SetCursor(Int_t kursor)
{
  *((int*)((int*)((void*)(this))+2)) = kursor;
}
//______________________________________________________________________________
void StObjArrayIter::Streamer(TBuffer& ){}
//______________________________________________________________________________
Int_t   StObjArrayIter::GetCursor() const
{
  return *((int*)((int*)((void*)(this))+2));
}
//______________________________________________________________________________
void   StObjArrayIter::SetDirection(Bool_t dir)
{
  *((Bool_t*)((int*)((void*)(this))+3)) = dir;
}
//______________________________________________________________________________
Bool_t   StObjArrayIter::GetDirection() const
{
  return *((Bool_t*)((int*)((void*)(this))+3));
}
//______________________________________________________________________________
void   StObjArrayIter::SetCollection(const TCollection *coll)
{
  *((const TCollection**)((int*)((void*)(this))+1)) = coll;
  assert(GetCollection()==coll);
}
//______________________________________________________________________________
TObject* StObjArrayIter::GetObject() const
{return ((TObjArray*)GetCollection())->At(GetCursor());}

//______________________________________________________________________________
StObjArrayIter &StObjArrayIter::operator++()
{ Int_t kursor=GetCursor(); SetCursor(++kursor); return *this;}
//______________________________________________________________________________
StObjArrayIter &StObjArrayIter::operator++(int)
{ Int_t kursor=GetCursor(); SetCursor(++kursor); return *this;}
//______________________________________________________________________________
StObjArrayIter &StObjArrayIter::operator--()
{ Int_t kursor=GetCursor(); SetCursor(--kursor); return *this;}
//______________________________________________________________________________
StObjArrayIter &StObjArrayIter::operator--(int)
{ Int_t kursor=GetCursor(); SetCursor(--kursor); return *this;}

//______________________________________________________________________________
Bool_t StObjArrayIter::operator==(const StObjArrayIter &iter) const
{
  if (GetCursor() != iter.GetCursor()) return 0;
  if (GetCollection() != iter.GetCollection()) return 0;
  return 1;
}  
//______________________________________________________________________________
Bool_t StObjArrayIter::operator!=(const StObjArrayIter &iter) const
{
  if (GetCursor() != iter.GetCursor()) return 1;
  if (GetCollection() != iter.GetCollection()) return 1;
  return 0;
}  

//______________________________________________________________________________
void StObjArrayIter::operator=(const StObjArrayIter &iter)
{
 SetCollection(iter.GetCollection());
 SetCursor(iter.GetCursor());
 SetDirection(iter.GetDirection());
} 

//______________________________________________________________________________
ClassImp(StRefArray)

//______________________________________________________________________________
void StRefArray::Streamer(TBuffer &R__b)
#ifdef NONMOMO
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
StStrArray::StStrArray(const Char_t *name, Int_t s):StObjArray(s)
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
  
  if (fArr) fArr->Delete(); delete fArr;
  SetName(a.GetName());
  SetIDName(0);
  n = a.GetSize();
  fArr = new TObjArray(n);
  for (i=0; i<n; i++)
  {
    sto = a.At(i);       
    if (sto) sto = ((StObject*)sto)->clone(); 
    Add(sto);
  }
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
 void StStrArray::AddFirst(TObject* obj)
{ 
  StObjArray::AddFirst(obj);
//NONMONO  Book(obj,1);
}

//______________________________________________________________________________
void StStrArray::AddLast(TObject* obj)
{ 
  StObjArray::AddLast(obj);
//NONMONO  Book(obj,GetLast()+1);
}

//______________________________________________________________________________
 void StStrArray::AddAt(TObject* obj, Int_t idx)
{
  StObjArray::AddAt(obj,idx);
//NONMONO Book(obj,idx+1);
}
 
//______________________________________________________________________________
 void StStrArray::AddAtAndExpand(TObject* obj, Int_t idx)
 {
  StObjArray::AddAtAndExpand(obj,idx);
//NONMONO  Book(obj,idx+1);
 };
//______________________________________________________________________________
//void StStrArray::ShowMembers(TMemberInspector &, char *){}
//______________________________________________________________________________
StStrArray::~StStrArray()
{
  Delete(); 
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
}
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
// $Id: StArray.cxx,v 1.26 2000/05/20 01:12:20 perev Exp $
// $Log: StArray.cxx,v $
// Revision 1.26  2000/05/20 01:12:20  perev
// Big cleanup of StArray
//
// Revision 1.25  2000/05/09 22:05:21  fine
// re-invent wheel to fix the memeory leak
//
// Revision 1.24  2000/04/23 20:37:18  perev
// random shuffle and one byte for empty collection I/O
//
// Revision 1.23  2000/04/23 01:00:45  perev
// StEvent monolitic I/O
//
// Revision 1.21  2000/04/18 02:57:25  perev
// StEvent browse
//
// Revision 1.20  2000/01/28 20:37:03  perev
// Home made SetLast removed
//
// Revision 1.19  1999/11/19 20:40:12  perev
// StObjArray::Streamer==TCollection::Streamer(b)
//
// Revision 1.18  1999/11/17 14:22:09  perev
// bug in dtor fix
//
// Revision 1.17  1999/11/15 23:09:10  perev
// Streamer for StrArray and auto remove
//
// Revision 1.16  1999/11/06 18:01:47  perev
// StArray cleanup
//
// Revision 1.15  1999/10/30 02:10:54  perev
// CleanUp of StArray for new StEvent
//
// Revision 1.14  1999/10/22 22:24:00  perev
// Minor fixes
//
// Revision 1.13  1999/10/21 00:13:58  perev
// Version of StArray for new StEvent
//
// Revision 1.12  1999/07/30 01:12:05  perev
// StArray const(antisation)
//
// Revision 1.11  1999/07/17 19:00:12  perev
// fix destructor of StStrArray
//
// Revision 1.10  1999/06/23 20:31:04  perev
// StArray I/O + browser
//
// Revision 1.7  1999/05/11 01:10:50  fine
// StArray::Browse() method jas been introduced
//
// Revision 1.6  1999/05/10 19:19:35  fisyak
// Add Valery's update for Browser
//
// Revision 1.5  1999/05/04 22:45:22  perev
// Default ctr for StArray
//
// Revision 1.4  1999/04/30 13:15:55  fisyak
// Ad StObject, modification StArray for StRootEvent
//
// Revision 1.2  1999/01/21 21:14:43  fisyak
// assert
//
