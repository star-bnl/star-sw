#include "assert.h"
#include "StArray.h"

TObjArray *StRegistry::fReg = 0;

Int_t StRegistry::SetColl (TSeqCollection *coll) 		
// Register new container
{
  assert(coll);
  if (!fReg) fReg = new TObjArray(10,1);
  
  const char *collname = coll->GetName();
  assert( !fReg->FindObject(collname));
  fReg->AddLast(coll);
  return fReg->GetLast();
}
Int_t StRegistry::GetColl (const char *collname)     
// get index of container
{
  assert(collname);
  if (fReg) return 0;
  TObject *coll = fReg->FindObject(collname);
  if (!coll) return 0;
  return fReg->IndexOf(coll);
}
TSeqCollection *StRegistry::GetColl (Int_t idx )
// get collection  by index
{ assert(fReg); return (TSeqCollection*)fReg->At(idx);} 

const char *StRegistry::GetCollName (Int_t idx )
// get name of collection by index
{
 TSeqCollection *coll = GetColl(idx); assert(coll); return coll->GetName();
}
 
void StRegistry::List()
{

  printf("\n\tList of StRegistry\n");
  if (!fReg) return;
  int n = fReg->GetLast();
  for (int i=1;i<=n;i++) {
    TSeqCollection *coll = GetColl(i);
    if (!coll) continue;
    int size = coll->GetSize();
    printf("Collection.%02d = %s Size = %d Address=%p\n"
    ,i,coll->GetName(),size,coll);   
  }
  printf("\n");
}

void StArray::Expand(Int_t n)
{
  if (n == fNEnt) return;
  if (n<=0) {if (fArray) delete [] fArray;  fArray=0; fNEnt=0; fLast=0; return;} 
  if (n > fNEnt && n < 2*fNEnt) n *=2; 

  char *array = new char(n*fUnit);
  memset(array,0,n*fUnit);
  int nn = (n > fNEnt ) ? fNEnt : n;
  memcpy(array,fArray,nn*fUnit);
  if (fArray) delete [] fArray;
  fArray = array; fNEnt = n;
  if (fLast>=fNEnt) fLast=fNEnt-1;
}
void StArray::AddAt(void *v,Int_t idx,Int_t size)
{ if (!size) size = fUnit;
  if (idx >= fNEnt) Expand(idx+1);
  memcpy(fArray+idx*fUnit,v,size);
  if (fLast < idx ) fLast=idx;
}

void StRefArray::AddAt(TObject *obj,Int_t idx)
{
  ULong_t u = 0;
  if (obj) {u = obj->GetUniqueID(); assert(u);}
  StArray::AddAt(u,idx);
}  
TObject *StRefArray::At(Int_t idx) const
{
  ULong_t u,objidx,colidx;
  assert(idx>=0);
  u = AtI(idx); if (!u || u & 0x80000000) 	return 0;
  StRegistry::Ident(u,colidx,objidx);
  TSeqCollection *col = StRegistry::GetColl(colidx);
  if (!col) 					return 0;
  return col->At(idx);
}

void test()
{
StStrArray *s=0;
StRefArray *r=0;
TObject *t;

TIter snext(s);
TIter rnext(r);
while(t=snext()) {};  
while(t=rnext()) {};  
}
