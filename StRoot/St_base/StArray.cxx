// $Id: StArray.cxx,v 1.3 1999/04/15 19:44:44 fine Exp $
// $Log: StArray.cxx,v $
// Revision 1.3  1999/04/15 19:44:44  fine
// St_DataSetIter::FindObject bug has been fixed. aliases FindByName and FindByPath  introduced
//
// Revision 1.2  1999/01/21 21:14:43  fisyak
// assert
//
#include <assert.h>
#include "StArray.h"
#include "TDatime.h"

TObjArray *StRegistry::fReg = 0;

//______________________________________________________________________________
Int_t StRegistry::SetColl (TObjArray *coll) 		
// Register new container
{
  assert(coll);
  if (!fReg) fReg = new TObjArray(10,1);
  
  const char *collname = coll->GetName();
  assert( !fReg->FindObject(collname));
  fReg->AddLast(coll);
  return fReg->GetLast();
}
//______________________________________________________________________________
Int_t StRegistry::GetColl(const char *collname)  
// get index of container
{
  assert(collname);
  if (fReg) return 0;
  TObject *coll = fReg->FindObject(collname);
  if (!coll) return 0;
  return fReg->IndexOf(coll)+1;
}
//______________________________________________________________________________
TObjArray *StRegistry::GetColl (Int_t idx )
// get collection  by index
{ assert(fReg); return (TObjArray*)fReg->At(idx-1);} 

//______________________________________________________________________________
const char *StRegistry::GetCollName (Int_t idx ) 
// get name of collection by index
{
 TObjArray *coll = GetColl(idx-1); assert(coll); return coll->GetName();
}
 
//______________________________________________________________________________
void StRegistry::List() 
{

  printf("\n\tList of StRegistry\n");
  if (!fReg) return;
  int n = fReg->GetLast();
  for (int i=1;i<=n;i++) {
    TObjArray *coll = GetColl(i);
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
void StObjArray::Streamer(TBuffer &R__b)
{TObjArray::Streamer(R__b);}

//______________________________________________________________________________
const StObjArrayIter StObjArray::begin() const
{ 
  const StObjArrayIter iter(this);
  return iter;
}
//______________________________________________________________________________
const StObjArrayIter StObjArray::Begin() const {return begin();}
//______________________________________________________________________________
const StObjArrayIter StObjArray::end() const
{ 
  StObjArrayIter iter(this);
  iter.SetCursor(GetLast()+1);
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
//______________________________________________________________________________
const StObjArrayIter StObjArray::End() const {return end();}

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
TObject* StObjArrayIter::operator*()
{return ((TObjArray*)GetCollection())->At(GetCursor());}

//______________________________________________________________________________
StObjArrayIter &StObjArrayIter::operator++()
{ Int_t kursor=GetCursor(); SetCursor(++kursor); return *this;}
//______________________________________________________________________________
StObjArrayIter &StObjArrayIter::operator--()
{ Int_t kursor=GetCursor(); SetCursor(--kursor); return *this;}

//______________________________________________________________________________
Bool_t StObjArrayIter::operator==(const StObjArrayIter &iter)
{
  if (GetCursor() != iter.GetCursor()) return 0;
  if (GetCollection() != iter.GetCollection()) return 0;
  return 1;
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
   TObjArray *coll;

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
StStrArray::StStrArray(const char *name)
{ if (!name) return; SetName(name); }
 
void StStrArray::SetName(const char *name)
{ 
 fName = name; 
 if (!strstr(name," At ")) {//add datetime
   fName += " At ";
   TDatime dt; fName += dt.AsString();
 }
 fIdx=StRegistry::SetColl(this);
}

//______________________________________________________________________________
void StStrArray::Streamer(TBuffer &R__b)
{
   // Stream an object of class StStrArray.

   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      TObjArray::Streamer(R__b);
      TString name; name.Streamer(R__b);
      SetName(name);
   } else {
      R__b.WriteVersion(StStrArray::IsA());
      TObjArray::Streamer(R__b);
      fName.Streamer(R__b);
   }

}
void testqwe()
{
StObjArray ar;
StObjArrayIter it = ar.begin();
}

