#include "Stiostream.h"
#include <string>
#include <typeinfo>

#ifndef StExceptions_hh
#define StExceptions_hh

class StException {
private:
  std::string mName;
  std::string mMessage;
public:
  StException(const char* c="unknown", const char* m="") : mName(c),mMessage(m) { /* no-op */ }
  virtual ~StException() {}
  virtual std::string name() {return mName;}
  virtual std::string message() {return mMessage; }
  virtual void print() { cout << "*** StException * " << name() << " *** " << message() << endl; }
};

class StExceptionNullPointer : public StException {
private:
public:
  StExceptionNullPointer(const char* message="") : StException("nullPointer",message) { /* -no-op */ }
  virtual ~StExceptionNullPointer() {}
};

class StExceptionBadFlag : public StException {
private:
public:
  StExceptionBadFlag(const char* message="") : StException("badFlag",message) { /* no-op */ }
  virtual ~StExceptionBadFlag() {}
};

class StExceptionBadValue : public StException {
private:
  float mValue;
public:
  StExceptionBadValue(const char* message="", float value=0) : StException("badValue",message) , mValue(value) { /* no-op */ }
  virtual ~StExceptionBadValue() {}
  virtual void print() { cout << "*** StException * " << name() << " *** " << message() << " " << mValue << endl; }
};

class StExceptionEOF : public StException {
private:
public:
  StExceptionEOF(const char* message="") : StException("end of file",message) { /* no-op */ }
  virtual ~StExceptionEOF() {}
};

#endif

