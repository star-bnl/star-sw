#ifndef Factory_H
#define Factory_H 

#include "TNamed.h"
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
class FactoryB : public TNamed
{
protected:
  
  FactoryB(const char *name);

  virtual ~FactoryB()
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
  static  int Alive(void *obj);

  void setFastDelete()	{fFastDel=1;}
  void setMaxIncrementCount(uint maxCount)	{fMaxCount=maxCount;}
  uint getMaxIncrementCount() const		{return fMaxCount;  }
  uint getCurrentSize()  const 			{return fCurCount;  }
  uint getCurrentCount() const 			{return fCurCount;  }
protected:
  uint fMaxCount;
  uint fCurCount;
  uint fUseCount;
  uint fFastDel;
  uint fInstCount;
  uint fFreeCount;
static double fgTotal;  
};

template <class Abstract>
class FactoryT : public FactoryB
{
public:
  
  FactoryT(const char *name): FactoryB(name)
    {;}

  virtual ~FactoryT()
    {;}

  ///Free an object for reuse 
  virtual void free(Abstract *obj)=0;
  virtual void free(void *obj)=0;

  ///Get a pointer to instance of objects served by this factory.
  virtual Abstract *getInstance()=0;

};

inline void FactoryB::Free(void *obj)
{
   void **v = ((void**)obj) - 1;
   if (!*v) v--;
   assert((*v)!=(void*)0xFF);
   assert(((*(int*)v)&3)==0);
   FactoryB *f = (FactoryB*)((*v));
   f->free(obj);
}
inline int FactoryB::Alive(void *obj)
{
   void **v = ((void**)obj) - 1;
   if (!*v) v--;
   return ((*v)!=(void*)0xFF);
}

#endif
