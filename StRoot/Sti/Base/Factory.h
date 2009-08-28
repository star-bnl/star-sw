#ifndef Factory_H
#define Factory_H 

#include "Sti/Base/Named.h"
#include <cassert>

/*!
  Abstract base class defining a  factory mechanism
  <p>
  This class defines the concept of factory, an agent responsible for the
  creation or instantiation of a given type of class. The class is templated.
  The template represents the base class to be intanstiated and served by the
  factory. Implementation (derived class) may serve objects from class derived
  based on the "Abstract" template class.
*/
class BFactory : public Named
{
protected:
  
  BFactory(const string& name);

  virtual ~BFactory()
    {;}

public:
  ///Clear/delete all objects owned by this factory
  virtual void clear()=0;

  ///Reset this factory
  virtual void reset()=0;

  ///Free an object for reuse 
  virtual void free(void *obj)=0;

  ///Free an object for reuse 
  static void Free(void *obj);

  void setFastDelete()	{fFastDel=1;}
  void setMaxIncrementCount(int maxCount)	{fMaxCount=maxCount;}
  int getMaxIncrementCount() const		{return fMaxCount;  }
  int getCurrentSize()  const 			{return fCurCount;  }
  int getCurrentCount() const 			{return fCurCount;  }
protected:
  int fMaxCount;
  int fCurCount;
  int fUseCount;
  int fFastDel;
  int fInstCount;
  int fFreeCount;
static double fgTotal;  
};

template <class Abstract>
class Factory : public BFactory
{
public:
  
  Factory(const string& name): BFactory(name)
    {;}

  virtual ~Factory()
    {;}

  ///Free an object for reuse 
  virtual void free(Abstract *obj)=0;
  virtual void free(void *obj)=0;

  ///Get a pointer to instance of objects served by this factory.
  virtual Abstract *getInstance()=0;

};

inline void BFactory::Free(void *obj)
{
   long *v = ((long*)obj) - 1;
   if (!*v) v--;
   assert((*v)&1L);
   BFactory *f = (BFactory*)((*v)-1);
   f->free(obj);
}

#endif
