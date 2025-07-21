#include <assert.h>
#include "TString.h"
#include "THashTable.h"

class GtCradle;
/**
 * @class GtHash
 * @brief A specialized hash table for storing and retrieving pointers associated with arrays of data.
 *
 * This class extends THashTable to provide a mechanism for mapping a raw data array
 * (of type `void *`) to a user-defined pointer. It uses an internal helper class,
 * GtCradle, to wrap the data array for hashing and comparison. This is useful
 * for caching or memoization where the result of a calculation (the pointer)
 * depends on a set of input parameters (the array).
 */
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
  GtCradle    *fPoka;	       //! Cradle pointer
  GtCradle    *fFound;	       //! Cradle pointer
  void 	      *fPointer;       //! user pointer
  ClassDef(GtHash,1)
};
