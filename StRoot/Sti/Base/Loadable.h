#ifndef LOADABLE_H_INCLUDED
#define LOADABLE_H_INCLUDED
#include <stdexcept>
#include <string>
#include "StMaker.h"
#include "TDataSet.h"
#include "Stiostream.h"
using std::iostream;
using std::ifstream;

///\class Loadable
///This class encapsulates the notion of an object being load-able from various sources.
///Two sources are currently instrumented. 
///(1) Load from ascii file
///(2) Load from STAR db
///See .cxx file for more documentation
///\author Claude A Pruneau
class Loadable  
{
 public:
 
  Loadable(){}
  virtual ~Loadable(){}
  void load(const string & userFileName, StMaker & source);
  void loadM(StMaker & source);
  void loadS(const string & userFileName);
  virtual void loadDS(TDataSet&ds);
  virtual void loadFS(ifstream &);
  virtual void setDefaults();
};

#endif 
