#include <iostream>
#include <string>
#include <typeinfo>

#ifndef StarMuException_hh
#define StarMuException_hh

#define PF __PRETTY_FUNCTION__

#define THROW(key,text) StarMuException##key(text,__PRETTY_FUNCTION__)
#define EXE(x) class StarMuException##x : public StarMuException { public: StarMuException##x##(const char* m="", const char* in="???") : StarMuException(k##x, m, in) { /* no-op */ } };
		       

enum StarMuExceptionTypes {kUnknown=0, kNullPointer, kBadFlag, kBadValue, kEOF};

class StarMuException {
protected:
  StarMuExceptionTypes mException;
  string mMessage;
  string mIn;
public:
  StarMuException(StarMuExceptionTypes=kUnknown, const char* m="", const char* in="???") : mMessage(m), mIn(in) { /* no-op */ }
  virtual ~StarMuException() {}
  virtual string message() {return mMessage; }
  virtual void print() { cout << "*** StarMuException #" <<  (unsigned long)mException << " *** " << mIn << " *** " << message() << " ***" << endl; }
  virtual StarMuExceptionTypes type() { return mException; }
};

// class StarMuExceptionUnknown : public StarMuException {
//   StarMuExceptionUnknown() : StarMuException(kUnknown, const char* m="", const char* in="???") { /* no-op */ }
// };
// class StarMuExceptionNullPointer : public StarMuException {
//   StarMuExceptionNullPointer() : StarMuException(kNullPointer, const char* m="", const char* in="???") { /* no-op */ }
// };
// class StarMuExceptionBadValue : public StarMuException {
//   StarMuExceptionBadValue() : StarMuException(kBadValue, const char* m="", const char* in="???") { /* no-op */ }
// };
// class StarMuExceptionkBadFlag : public StarMuException {
//   StarMuExceptionBadFlag() : StarMuException(kBadFlag, const char* m="", const char* in="???") { /* no-op */ }
// };
// class StarMuExceptionEOF : public StarMuException {
//   StarMuExceptionEOF() : StarMuException(kEOF, const char* m="", const char* in="???") { /* no-op */ }
// };
  

EXE(Unknown);
EXE(NullPointer);
EXE(BadValue);
EXE(BadFlag);
EXE(EOF);





#endif

