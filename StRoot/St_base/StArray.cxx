// $Id: StArray.cxx,v 1.9 1999/06/17 12:50:50 fisyak Exp $
// $Log: StArray.cxx,v $
// Revision 1.9  1999/06/17 12:50:50  fisyak
// Make StArray classes visible in RootCint, remove ShowMembers
//
// Revision 1.8  1999/05/22 17:46:45  perev
// StVectorInt class added
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
#include "StArray.h"
#include "TDatime.h"
#include "TBrowser.h"

TObjArray *StRegistry::fReg = 0;

ClassImp(StVectorInt)
//______________________________________________________________________________
 void StVectorInt::Streamer(TBuffer &b){}
//______________________________________________________________________________
 void StVectorInt::Set(Int_t n)
{
   // Set array size of TArrayI object to n integers.
   // If n<0 leave array unchanged.

   if (n < 0) return;
   Int_t *arrSav = fArray;
   if (n) {
     fArray = new Int_t[n];
     memset(fArray,0,sizeof(Int_t)*n);
     if (arrSav) memcpy(fArray,arrSav,sizeof(Int_t)*fN);
   }
   fN = n;
   if (arrSav) delete [] arrSav;
   if (fLast >=fN) fLast=fN-1;
}
//______________________________________________________________________________
 void StVectorInt::Add(const Int_t &c )
{ if (++fLast >=fN) Set(fN*2);
  fArray[fLast]=c;
}




//______________________________________________________________________________
Int_t StRegistry::SetColl (StStrArray *coll) 		
// Register new container
{
  assert(coll);
  if (!fReg) fReg = new TObjArray(10);
  
  const char *collname = coll->GetIDName();
  int i=0,n=fReg->GetLast();
  
  for (i=0;i<=n;i++) 
  {
    StStrArray *koll = (StStrArray*)fReg->At(i);
    if (!koll) continue;
    assert(strcmp(collname,koll->GetIDName()));
  }
  fReg->AddLast(coll);
  return fReg->GetLast();
}
//______________________________________________________________________________
void StRegistry::RemColl(StStrArray *coll)  
{
  assert(fReg); fReg->Remove(coll);
  
}
//______________________________________________________________________________
Int_t StRegistry::GetColl(const char *collname)  
// get index of container
{
  assert(collname);
  if (fReg) return 0;

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
 StObjArray *coll = GetColl(idx-1); assert(coll); return coll->GetName();
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
void StObjArray::Streamer(TBuffer &)
{;}
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
 
   const Int_t maxBrowsable =  10;
   TIter next(this);
   TObject *obj;
    
   if (b) {
      Int_t counter = 0;
      Int_t totalSize = size();
      while ((obj = next()) && ++counter <  maxBrowsable ) {
          TString browseName = obj->GetName();
          char buffer[100];
          sprintf(buffer,"_%d_of_%d",counter,totalSize);
          browseName += buffer;
          b->Add(obj,browseName.Data());
      }
   }
   else
      TObject::Browse(b);
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
  *((int*)((int*)((void*)(fObjArr))+8)) = last;
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
ClassImp(StObjArrayIter)
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
void   StObjArrayIter::SetCollection(TObjArray *coll)
{
  *((TObjArray**)((int*)((void*)(this))+1)) = coll;
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
Bool_t StObjArrayIter::operator==(const StObjArrayIter &iter)
{
  if (GetCursor() != iter.GetCursor()) return 0;
  if (GetCollection() != iter.GetCollection()) return 0;
  return 1;
}  
//______________________________________________________________________________
Bool_t StObjArrayIter::operator!=(const StObjArrayIter &iter)
{
  if (GetCursor() != iter.GetCursor()) return 1;
  if (GetCollection() != iter.GetCollection()) return 1;
  return 0;
}  

//______________________________________________________________________________
void StObjArrayIter::operator=(const StObjArrayIter &iter)
{
 SetCollection((TObjArray*)iter.GetCollection());
 SetCursor(iter.GetCursor());
 SetDirection(iter.GetDirection());
} 

//______________________________________________________________________________
ClassImp(StRefArray)
//______________________________________________________________________________
void StRefArray::Streamer(TBuffer &R__b)
{
   // Stream all objects in the array to or from the I/O buffer.  

   Int_t nobjects,newcol,i;
   Int_t icolls[100]; 
   ULong_t ulong,objidx,colidx,kolidx,nkoll=0; 
   TObject *obj; TString colname;
   StObjArray *coll;

   if (R__b.IsReading()) {
      R__b.ReadVersion();   // Version_t v = R__b.ReadVersion();
      R__b >> nobjects;
//    R__b >> fLowerBound;
      
      for (i = 0; i < nobjects; i++) {
        R__b >> ulong;
        if (! ulong) continue; 
        StRegistry::Ident(ulong,kolidx,objidx);
        if (kolidx > nkoll) {
          colname.Streamer(R__b);  
          colidx = StRegistry::GetColl(colname);
          if(!colidx) Warning("Input Streamer","Collection %s Not Loaded.",(const char*)colname);
          icolls[kolidx]=colidx; nkoll = kolidx;
        }
        colidx = icolls[kolidx];
        if (!colidx) continue;
        coll = StRegistry::GetColl(colidx); 
        assert(coll);
        obj = coll->At(objidx);
        
        AddAtAndExpand(obj, i);
      }
   } else {

      R__b.WriteVersion(StRefArray::IsA());
      Int_t ncoll = StRegistry::GetNColl();
      assert(ncoll <= 100);
      memset(icolls,0,sizeof(Int_t)*ncoll);
      nobjects = GetSize();
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
   }
}

ClassImp(StStrArray)
//______________________________________________________________________________
StStrArray::StStrArray(const Char_t *name, Int_t s):StObjArray(s)
{ 
  if (!name) return;
  SetName(name); SetIDName(0); 
}
//______________________________________________________________________________
StStrArray::~StStrArray()
{StRegistry::RemColl(this);}

//______________________________________________________________________________
const Char_t *StStrArray::GetIDName() const 
{
  assert(!fIDName.IsNull()); return fIDName;
}
 
//______________________________________________________________________________
void StStrArray::SetIDName(const Char_t *idname)
{
  assert(fIDName.IsNull()) ;
  if (idname) 
  {
    fIDName = idname; 
  } else {

    char buf[20]; sprintf(buf,"%p",this); fIDName = buf;

    fIDName += " ";
    TDatime dt; fIDName += dt.AsString();
  }

  fIdx=StRegistry::SetColl(this);
}
//______________________________________________________________________________
void StStrArray::Streamer(TBuffer &R__b)
{
   // Stream an object of class StStrArray.

   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      TString name; name.Streamer(R__b);SetName(name);
      name.Streamer(R__b);SetIDName(name);
      R__b >> fObjArr;
   } else {
      R__b.WriteVersion(StStrArray::IsA());
      fName.Streamer(R__b);
      fIDName.Streamer(R__b);
      R__b << fObjArr;
   }

}
#if 0
ClassImp(QWERTY)

ClassImp(QWERTYIter)
//______________________________________________________________________________
void QWERTYIter::Streamer(TBuffer& ){}

//______________________________________________________________________________
ClassImp(QWERTYRef)
void QWERTYRef::Streamer(TBuffer& b){StRefArray::Streamer(b);}

TIterator* QWERTYRef::MakeIterator(Bool_t dir) const
{return (TIterator*)new QWERTYIter(this,dir);}

const QWERTYIter QWERTYRef::begin() const { return *((QWERTYIter*)Begin());};
const QWERTYIter QWERTYRef::end()   const { return *((QWERTYIter*)End());};

//______________________________________________________________________________
ClassImp(QWERTYStr)
void QWERTYStr::Streamer(TBuffer& b){StStrArray::Streamer(b);}

TIterator* QWERTYStr::MakeIterator(Bool_t dir) const
{return (TIterator*)new QWERTYIter(this,dir);}

const QWERTYIter QWERTYStr::begin() const { return *((QWERTYIter*)Begin());};
const QWERTYIter QWERTYStr::end()   const { return *((QWERTYIter*)End());};



void testqwe()
{

printf("TEST 1: QWERTYRef\n\n");

  int i,n;
  QWERTY *to;

  QWERTYRef ar(100);
  ar.resize(20);
  for (i=0;i<20;i++)
  { 
    to = new QWERTY; to->SetUniqueID(i);
    ar[i] = to;
  }

  n = ar.size(); printf("size = %d\n",n);
  for (i=0;i<n;i++)
  { 
    to = ar[i]; 
    printf(" %d == %d\n",i,to->GetUniqueID());
  }

  printf("TEST 2: QWERTYRef\n\n");

  for (QWERTYIter it = ar.begin(); !(it==ar.end()); ++it)
  {
    to = *it; printf(" %d == %d\n",i,to->GetUniqueID());

  }
}
#endif
