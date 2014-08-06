#include <iostream>
#include <string>
#include <typeinfo>

#ifndef StMuException_hh
#define StMuException_hh

#define PF __PRETTY_FUNCTION__

#define THROW(key,text) StMuException##key(text,__PRETTY_FUNCTION__)
#define EXE(x) class StMuException##x : public StMuException { public: StMuException##x (const char* m="", const char* in="???") : StMuException(k##x, m, in) { /* no-op */ } };
		       

enum StMuExceptionTypes {kUnknown=0, kNullPointer, kBadFlag, kBadValue, kEOF};

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

// class StMuExceptionUnknown : public StMuException {
//   StMuExceptionUnknown() : StMuException(kUnknown, const char* m="", const char* in="???") { /* no-op */ }
// };
// class StMuExceptionNullPointer : public StMuException {
//   StMuExceptionNullPointer() : StMuException(kNullPointer, const char* m="", const char* in="???") { /* no-op */ }
// };
// class StMuExceptionBadValue : public StMuException {
//   StMuExceptionBadValue() : StMuException(kBadValue, const char* m="", const char* in="???") { /* no-op */ }
// };
// class StMuExceptionkBadFlag : public StMuException {
//   StMuExceptionBadFlag() : StMuException(kBadFlag, const char* m="", const char* in="???") { /* no-op */ }
// };
// class StMuExceptionEOF : public StMuException {
//   StMuExceptionEOF() : StMuException(kEOF, const char* m="", const char* in="???") { /* no-op */ }
// };
  

EXE(Unknown);
EXE(NullPointer);
EXE(BadValue);
EXE(BadFlag);
EXE(EOF);





#endif

