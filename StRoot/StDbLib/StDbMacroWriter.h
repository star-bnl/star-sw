#ifndef STDBMACROWRITER_HH
#define STDBMACROWRITER_HH

#include "tableAcceptor.hh"
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <strstream.h>
#include <string.h>

#include "TObject.h"

class StDbMacroWriter : public tableAcceptor {

protected:

  ofstream* os;//!

public:

  StDbMacroWriter() {};
  StDbMacroWriter(ofstream& ofs){ os=&ofs;};
  virtual ~StDbMacroWriter(){};

  virtual void streamHeader(const char* name);
  virtual void streamAccessor();
  virtual void endAccessor();
  virtual void streamTail();

  virtual void pass(char* name, int& i, unsigned int size);  
  virtual void pass(char* name, long& i, unsigned int size);  
  virtual void pass(char* name, float& i, unsigned int size);
  virtual void pass(char* name, double& i, unsigned int size);
  virtual void pass(char* name, char* i, unsigned int size);
  virtual void pass(char* name, int* i, unsigned int size);  
  virtual void pass(char* name, long* i, unsigned int size);  
  virtual void pass(char* name, float* i, unsigned int size);
  virtual void pass(char* name, double* i, unsigned int size);

ClassDef(StDbMacroWriter,0)

};

#endif


