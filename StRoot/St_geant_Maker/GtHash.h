#include <assert.h>
#include "TString.h"
#include "THashTable.h"

class GtCradle;
class GtHash : public THashTable 
{
 public:
  GtHash():THashTable(100,3),fPoka(0),fFound(0),fPointer(0){};
  ~GtHash();
  void *GetPointer(void *array,Int_t narray);
  void  SetPointer(void *ptr);
  Int_t GetNParams();
  const void *GetParams();
 protected:
  GtCradle    *fFound;	       // Cradle pointer
  GtCradle    *fPoka;	       // Cradle pointer
  void 	      *fPointer;       // user pointer
};
