#ifndef TABLEACCEPTOR_HH
#define TABLEACCEPTOR_HH


#include "typeAcceptor.hh"
#include <iostream.h>

class tableAcceptor : public typeAcceptor {

public:
 
  virtual ~tableAcceptor(){};

  virtual void streamHeader(const char* name) = 0;
  virtual void streamAccessor() = 0;
  virtual void endAccessor() = 0;
  virtual void streamTail() = 0;

};

#endif


