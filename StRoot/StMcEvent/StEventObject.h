#ifndef STAR_STEVENTOBJECT
#define STAR_STEVENTOBJECT


//! base class for all StMcEvent objects
#include "StObject.h"

template <class T>
class StEventObject : public StObject {
  protected:
    T fData;
  
  public:
    int operator==(const StEventObject&h) const
    {  return *h.fData == *fData;       }

    int operator!=(const StEventObject&h) const
    {  return !(*this == h);            }
    
    StEventObject() : fData() {};
    StEventObject(T data) : fData(data) {};
    virtual ~StEventObject() {}
    T  Data()             { return fData;}
    const T Data() const { return fData;}
    void SetData( T data) { fData = data; }
};

#endif
