/***************************************************************************
 *
 * $Id: typeAcceptor.hh,v 1.3 1999/09/30 02:06:16 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: buffer for file I/O for StDbTable (e.g. XmlReader/Writer)
 *
 ***************************************************************************
 *
 * $Log: typeAcceptor.hh,v $
 * Revision 1.3  1999/09/30 02:06:16  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef TYPEACCEPTOR_HH
#define TYPEACCEPTOR_HH


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







