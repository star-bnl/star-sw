#include "TMath.h"
#include "GtHash.h"

class GtCradle : public TObject 
{
public:
  Int_t  	fNWords; 		// number of words in array
  ULong_t	*fArray; 		// array
  void 		*fPointer;		// user pointer

GtCradle(int n); 
~GtCradle(){ if (fArray) delete [] fArray;};

protected:
  virtual Bool_t  IsEqual(const TObject* obj) const;
  virtual ULong_t Hash() const;
};

GtCradle::GtCradle(int n)
{
 fNWords = n;
 fArray  = new ULong_t[fNWords]; 
 fPointer=0;
}
 
Bool_t GtCradle::IsEqual(const TObject* obj) const
{ 
  GtCradle *He = (GtCradle*)obj;
  if (fNWords != He->fNWords) return 0; 
  ULong_t *me  = fArray;
  ULong_t *he  = He->fArray;
  if (me[0]   != he[0]) return 0; 
  for (int i=1; i<fNWords; i++) if (me[i]!=he[i]) return 0;
  return 1;
}
ULong_t GtCradle::Hash() const
{ 
  ULong_t *me = (ULong_t *)fArray;
  ULong_t ret = fNWords;
//VP  for (int i=0; i<fNWords; i++) ret ^= me[i];
  ret = TMath::Hash(me,fNWords*sizeof(ULong_t));

  return ret;
}
GtHash::~GtHash(){Delete(); if (fPoka) delete fPoka;};

void *GtHash::GetPointer(void *array,Int_t narray)
{
  if (!fPoka || fPoka->fNWords!=narray) {
    if (fPoka) delete fPoka;
    fPoka = new GtCradle(narray);}

  memcpy(fPoka->fArray,array,narray*sizeof(ULong_t));
  
  fFound = (GtCradle*)FindObject(fPoka);
  if (!fFound) return 0;
  return fFound->fPointer;
}

void GtHash::SetPointer(void *ptr)
{
  assert (ptr);
  assert (!fFound);
  assert (fPoka);
  fPoka->fPointer = ptr;
  Add(fPoka);
  fPoka = 0;
}

Int_t GtHash::GetNParams()
{ return (fFound) ? fFound->fNWords : 0 ; }

const void *GtHash::GetParams() 
{ return (fFound) ? fFound->fArray  : 0 ; }  



  
