// $Id: StArray.cxx,v 1.18 1999/11/17 14:22:09 perev Exp $
// $Log: StArray.cxx,v $
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
#include <assert.h>
#include <stdlib.h>
#include "StArray.h"
#include "TDatime.h"
#include "TBrowser.h"

TObjArray *StRegistry::fReg  = 0;
TList     *StRegistry::fNon  = 0;
Int_t      StRegistry::fFree = 0;
//______________________________________________________________________________
ClassImp(StTObjArray)
//______________________________________________________________________________
ClassImp(StRegistry)
//______________________________________________________________________________
void StRegistry::Clear()
{
  if (fReg) fReg->Clear();
  fFree = 0;
}
//______________________________________________________________________________
UInt_t StRegistry::Ident(UInt_t colidx,UInt_t objidx)
{
 assert(colidx<=0xffff && objidx <= 0xffff);
 return (colidx<<16)|objidx;
}
//______________________________________________________________________________
void    StRegistry::Ident(UInt_t ident,UInt_t &colidx,UInt_t &objidx)
{
  colidx = ident>>16;  objidx = ident & 0x0000ffff;
}  
//______________________________________________________________________________
Int_t StRegistry::GetNColl(){return (fReg) ? fReg->GetLast()+1:0;}				// Number of collections
//______________________________________________________________________________
void StRegistry::Streamer(TBuffer &){assert(0);}

//______________________________________________________________________________
Int_t StRegistry::SetColl(StStrArray *coll) 		
// Register new container
{
  Int_t n;
  assert(coll);
  if (!fReg) fReg = new TObjArray(10);
  
  if (!fFree) {	//No free places
    fReg->AddLast(coll);
    n = fReg->GetLast()+1;
  } else {
    n = fFree;
    fFree = -(int)fReg->At(n-1);
    fReg->AddAt(coll,n-1);
  }
  coll->SetUniqueID(StRegistry::Ident(0,n));
  
  return n;
}
//______________________________________________________________________________
void StRegistry::RemColl(StStrArray *coll)  
{
  UInt_t colIdx,objIdx;
  assert(fReg); 
  StRegistry::Ident(coll->GetUniqueID(), colIdx, objIdx);
  assert(objIdx);  assert(!colIdx);
  assert( fReg->At(objIdx-1)==coll);
  fReg->RemoveAt(objIdx-1);
  if (objIdx-1 < (UInt_t)fReg->GetLast()) {
    fReg->AddAt((TObject*)(-fFree),objIdx-1);
    fFree = objIdx;
  }

  coll->SetUniqueID(0);
  
}
//______________________________________________________________________________
Int_t StRegistry::GetColl(const char *collname)  
// get index of container
{
  assert(collname);
  if (!fReg) return 0;

  int i=0,n=fReg->GetLast();  
  for (i=0;i<=n;i++) 
  {
    StStrArray *koll = (StStrArray*)fReg->At(i);
    if (!koll) continue;
    if(strcmp(collname,koll->GetIDName())) continue;
    return (i+1);
  }
  return 0;
}
//______________________________________________________________________________
StStrArray *StRegistry::GetColl (Int_t idx )
// get collection  by index
{ assert(fReg); return (StStrArray*)fReg->At(idx-1);} 

//______________________________________________________________________________
const char *StRegistry::GetCollName (Int_t idx ) 
// get name of collection by index
{
 StStrArray *coll = (StStrArray*)GetColl(idx); assert(coll); 
 return coll->GetIDName();
}
 
//______________________________________________________________________________
 void  StRegistry::AddNon(StRefArray *coll)
{
  if (!fNon) fNon = new TList();
  fNon->Add(coll);
}
//______________________________________________________________________________
 void  StRegistry::Init()
{
  if (!fNon) return;
  StRefArray *coll;
  while((coll = (StRefArray*)fNon->First())) 
  { coll->Decode(); fNon->Remove(coll); }
  delete fNon; fNon = 0;
  
}
//______________________________________________________________________________
void StRegistry::List() 
{

  printf("\n\tList of StRegistry\n");
  if (!fReg) return;
  int n = fReg->GetLast();
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
//______________________________________________________________________________
void 	StObjArray::push_back(const TObject *obj) {AddLast((TObject*)obj);}
//______________________________________________________________________________
void 	StObjArray::pop_back() {RemoveAt(GetLast());}
//______________________________________________________________________________
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
void 	StObjArray::clear(){Clear();}
//______________________________________________________________________________
Bool_t 	StObjArray::empty() const {return IsEmpty();}
//______________________________________________________________________________
TObject** StObjArray::GetCell(Int_t idx) const{return &((*fArr)[idx]);}

//______________________________________________________________________________
void StObjArray::Streamer(TBuffer &)
{;}
//______________________________________________________________________________
// void StObjArray::ShowMembers(TMemberInspector &, char *){}
//______________________________________________________________________________
const TIterator *StObjArray::Begin() const
{ 
//  TIterator *(StObjArray::*mkit)(Bool_t dir) const = (TIterator *(StObjArray::*)(Bool_t) const)&MakeIterator;
  static  TIterator *iter=0;
  if (iter) delete iter;
  iter =  MakeIterator();
  return iter;
}
//______________________________________________________________________________
void StObjArray::Browse(TBrowser *b)
{
   // Browse this collection (called by TBrowser).
   // If b=0, there is no Browse call TObject::Browse(0) instead.
   //         This means TObject::Inspect() will be invoked indirectly
 
   if (!b) return;
   StRegistry::Init();
   const Int_t maxBrowsable =  10;
   TIter next(this);
   TObject *obj;
    
   Int_t counter = 0;
   Int_t totalSize = GetEntries();
   while ((obj = next()) && ++counter <  maxBrowsable ) {
       TString browseName = obj->GetName();
       char buffer[100];
       sprintf(buffer,"_%d_of_%d",counter,totalSize);
       browseName += buffer;
       b->Add(obj,browseName.Data());
   }
}
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
void StObjArray::SetLast(Int_t last)
{
  *((int*)((int*)((void*)(this))+8)) = last;
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
void StObjArray::Clean(StObject *obj)
{
  if (!obj) Clear(); else Remove(obj);
}  
//______________________________________________________________________________
void StObjArray::Clean(StObjArrayIter *iter)
{
    int idx = iter->GetCursor(); AddAt(0,idx);
}
//______________________________________________________________________________
void StObjArray::Erase(StObject *obj)
{
  if (!obj) {Delete();return;}
  Remove(obj); delete obj;
}

//______________________________________________________________________________
void StObjArray::Erase(StObjArrayIter *iter)
{ 
  TObject *obj = iter->GetObject();
  if (!obj) return;
  Clean(iter); delete obj;
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
//void StRefArray::ShowMembers(TMemberInspector &, char *){}
//______________________________________________________________________________
void StRefArray::Streamer(TBuffer &R__b)
{
   // Stream all objects in the array to or from the I/O buffer.  

   Int_t nobjects,newcol,i;
   UInt_t ulong,objidx,colidx,kolidx,nkoll=0; 
   TObject *obj; TString *colname;

   if (R__b.IsReading()) {
      R__b.ReadVersion();   // Version_t v = R__b.ReadVersion();
      R__b >> nobjects;
//    R__b >> fLowerBound;
      char cbuf[100];
      for (i = 0; i < nobjects; i++) {
        R__b >> ulong;
        colname = 0;
        if (ulong) {; 
          StRegistry::Ident(ulong,kolidx,objidx);
          if (kolidx > nkoll) {
            R__b.ReadString(cbuf,100);
            colname = new TString(cbuf);
            nkoll = kolidx;
          }
        }
        AddLast((TObject*)ulong);
        if (colname) Add((TObject*)colname);
      }
      StRegistry::AddNon(this);
   } else {

      R__b.WriteVersion(StRefArray::IsA());
      Int_t ncoll = StRegistry::GetNColl();
      Int_t *icolls = (Int_t*)calloc(ncoll+1,sizeof(Int_t)); 
      memset(icolls,0,sizeof(Int_t)*(ncoll+1));
      nobjects = GetLast()+1;
      R__b << nobjects;
//    R__b << fLowerBound;
//		First pass, collect different collections

      for (i=0;i<nobjects;i++)
      {
        ulong=0; newcol=0; obj = At(i); 
        if (obj) {
          StRegistry::Ident(obj->GetUniqueID(),colidx,objidx);
          if (!icolls[colidx]) { newcol = 1999; icolls[colidx]=++nkoll;}
          ulong = StRegistry::Ident(icolls[colidx],objidx);
        }
      
        R__b << ulong;
        if (newcol) //Write Name of collection
          R__b << StRegistry::GetCollName(colidx);
      }
      free(icolls);
   }
}
//______________________________________________________________________________
void StRefArray::Decode()
{
  int i,j,colidx;
  UInt_t ulong,kolidx,objidx,nkoll=0;
  int nobjects = GetLast() + 1 ;
  TString *colname;
  int mcolls =  StRegistry::GetNColl(); 
  int *icolls = (int *)calloc(mcolls,sizeof(int));
  TObject *obj;
  StStrArray *coll;
  
  for (i = 0,j=0; i < nobjects; i++) {
    ulong = (UInt_t)At(i); AddAt(0,i);
    if (! ulong) continue; 
    StRegistry::Ident(ulong,kolidx,objidx);
    if (kolidx > nkoll) {
      colname= (TString*)At(++i);  AddAt(0,i); assert(colname);
      colidx = StRegistry::GetColl((const char*)*colname);
      if(!colidx) Warning("Decode","Collection %s Not Loaded.",(const char*)colname);
      delete colname; colname=0;
      assert(colidx<mcolls);      
      icolls[kolidx]=colidx; nkoll = kolidx;
    }
    colidx = icolls[kolidx];  assert(colidx);
    coll = StRegistry::GetColl(colidx); 
    assert(coll);
    obj = coll->At(objidx);
    AddAt(obj, j++);
  }
  free(icolls);
}
//______________________________________________________________________________
ClassImp(StStrArray)
//______________________________________________________________________________
StStrArray::StStrArray(const Char_t *name, Int_t s):StObjArray(s)
{ 
  if (name) SetName(name); SetIDName(0); 
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
 void StStrArray::Book(TObject* obj,int idx)
 { obj->SetUniqueID(StRegistry::Ident(fIdx,idx));}

//______________________________________________________________________________
 const Char_t *StStrArray::GetName() const {return fName;}
//______________________________________________________________________________
 void          StStrArray::SetName(const char* name) {fName = name;} 
//______________________________________________________________________________
 void StStrArray::AddFirst(TObject* obj)
 { StObjArray::AddFirst(obj); Book(obj,0);}

//______________________________________________________________________________
 void StStrArray::AddLast(TObject* obj)
 { StObjArray::AddLast(obj); Book(obj,GetLast());}

//______________________________________________________________________________
 void StStrArray::AddAt(TObject* obj, Int_t idx)
 { StObjArray::AddAt(obj,idx); Book(obj,idx);}
 
//______________________________________________________________________________
 void StStrArray::AddAtAndExpand(TObject* obj, Int_t idx)
 { StObjArray::AddAtAndExpand(obj,idx); Book(obj,idx);};
//______________________________________________________________________________
//void StStrArray::ShowMembers(TMemberInspector &, char *){}
//______________________________________________________________________________
StStrArray::~StStrArray()
{
  Delete(); 
  StRegistry::RemColl(this);
}

//______________________________________________________________________________
const Char_t *StStrArray::GetIDName() const 
{
  assert(!fIDName.IsNull()); return fIDName;
}
 
//______________________________________________________________________________
void StStrArray::SetIDName(const Char_t *idname)
{
  int isNull = fIDName.IsNull();
  if (idname) 
  {
    fIDName = idname; 
  } else {

    char buf[20]; sprintf(buf,"%p",this); fIDName = buf;

    fIDName += " ";
    TDatime dt; fIDName += dt.AsString();
  }

  if (isNull) fIdx=StRegistry::SetColl(this);
}
//______________________________________________________________________________
void StStrArray::Streamer(TBuffer &R__b)
{
   // Stream an object of class StStrArray.

   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      TString name; name.Streamer(R__b);SetName(name);
      name.Streamer(R__b);SetIDName(name);
      StObjArray::Streamer(R__b);
   } else {
      R__b.WriteVersion(StStrArray::IsA());
      fName.Streamer(R__b);
      fIDName.Streamer(R__b);
      StObjArray::Streamer(R__b);
   }

}
