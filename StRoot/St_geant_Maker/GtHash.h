#include <assert.h>
#include "TString.h"
#include "THashTable.h"

class GtCradle;
class GtHash : public THashTable 
{
protected:
  void 	      *fPointer;       // user pointer
  GtCradle    *fFound;	       // Cradle pointer
  GtCradle    *fPoka;	       // Cradle pointer

public:
GtHash():THashTable(100,3){};
~GtHash();

void *GetPointer(void *array,Int_t narray);
void  SetPointer(void *ptr);
Int_t GetNParams();
const void *GetParams();

};
