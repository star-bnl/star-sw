#ifndef TYPEACCEPTOR_HH
#define TYPEACCEPTOR_HH

#include "enumType.hh" // only for _ByteSwap_

class typeAcceptor {

public:

  virtual ~typeAcceptor(){};
  virtual void pass(char* name, short& i, int len) = 0;  
  virtual void pass(char* name, int& i, int len) = 0;  
  virtual void pass(char* name, long& i, int len) = 0;  
  virtual void pass(char* name, unsigned short& i, int len) = 0;  
  virtual void pass(char* name, unsigned int& i, int len) = 0;  
  virtual void pass(char* name, unsigned long& i, int len) = 0;  
  virtual void pass(char* name, float& i, int len) = 0;
  virtual void pass(char* name, double& i, int len) = 0;

  virtual void pass(char* name, char* i, int len) = 0;
  virtual void pass(char* name, unsigned char* i, int len) = 0;

  virtual void pass(char* name, short* i, int len) = 0;  
  virtual void pass(char* name, int* i, int len) = 0;  
  virtual void pass(char* name, long* i, int len) = 0;  
  virtual void pass(char* name, unsigned short* i, int len) = 0;  
  virtual void pass(char* name, unsigned int* i, int len) = 0;  
  virtual void pass(char* name, unsigned long* i, int len) = 0;  
  virtual void pass(char* name, float* i, int len) = 0;
  virtual void pass(char* name, double* i, int len) = 0;

};

#endif







