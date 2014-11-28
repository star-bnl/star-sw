#ifndef StMuException_hh
#define StMuException_hh

#include <string>
#include "Stiostream.h"
#include <typeinfo>
#include "StMuDebug.h"

#ifndef ST_NO_NAMESPACES
using namespace std;
#endif



enum StMuExceptionTypes {kUnknown=0, kNullPointer, kBadFlag, kBadValue, kEOF};
/** 
    \class StMuException

    Just a small helper class (and a few macros) to easily create a set of exceptions.
    Using the "THROW(...)" macro the exception's datamember mIn will hold the name of 
    the scope that was throwing the exception.
*/

class StMuException {
protected:
  StMuExceptionTypes mException;
  string mMessage;
  string mIn;
public:
  StMuException(StMuExceptionTypes=kUnknown, const char* m="", const char* in="???") : mMessage(m), mIn(in) { /* no-op */ }
  virtual ~StMuException() {}
  virtual string message() {return mMessage; }
  virtual void print() { cout << "*** StMuException #" <<  (unsigned long)mException << " *** " << mIn << " *** " << message() << " ***" << endl; }
  virtual StMuExceptionTypes type() { return mException; }
};


#define THROW(key,text) StMuException##key(text,__PRETTYF_)
#define EXE(x) class StMuException##x : public StMuException { public: StMuException##x (const char* m="", const char* in="???") : StMuException(k##x, m, in) { /* no-op */ } };
		       

EXE(Unknown);
EXE(NullPointer);
EXE(BadValue);
EXE(BadFlag);
EXE(EOF);





#endif

