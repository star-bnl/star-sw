#ifndef Factory_H
#define Factory_H 

#include "Named.h"

/*!
  Abstract base class defining a  factory mechanism
  <p>
  This class defines the concept of factory, an agent responsible for the
  creation or instantiation of a given type of class. The class is templated.
  The template represents the class to be intanstiated and served by the
  factory.
*/
template<class Factorized>
class Factory : public Named
{
 public:
  
  Factory(const string& name);
  virtual ~Factory();
  
  virtual void initialize()=0;
  virtual void reset()=0;
  virtual Factorized * getInstance()=0;
};

template<class Factorized>
Factory<Factorized>::Factory(const string& name)
  : Named(name)
{}

template<class Factorized>
Factory<Factorized>::~Factory()
{}
  
#endif
