#ifndef Factory_H
#define Factory_H 

#include "Sti/Base/Named.h"

/*!
  Abstract base class defining a  factory mechanism
  <p>
  This class defines the concept of factory, an agent responsible for the
  creation or instantiation of a given type of class. The class is templated.
  The template represents the base class to be intanstiated and served by the
  factory. Implementation (derived class) may serve objects from class derived
  based on the "Factorized" template class.
*/
template<class Factorized>
class Factory : public Named
{
 public:
  
  Factory(const string& name)
  : Named(name)
    {}

  virtual ~Factory()
    {}

  ///Initialize this factory
  virtual void initialize()=0;

  ///Reset this factory
  virtual void reset()=0;

  ///Clear/delete all objects owned by this factory
  virtual void clear()=0;

  ///Get a pointer to instance of objects served by this factory.
  virtual Factorized * getInstance()=0;
};

#endif
