#ifndef STDBXmlReader_HH
#define STDBXmlReader_HH

#include "typeAcceptor.hh"
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <strstream.h>
#include <string.h>

#include "TObject.h"

class dbTable;
class elem;
class accessor;

class StDbXmlReader : public typeAcceptor {

protected:

  //  ofstream* os;//!

  char* loca[256];//!
  dbTable* tab;//!

   void buildDbTable();
   void buildStruct();
   void fillElements(accessor* a);
   elem* findElement(char* name);

public:

  StDbXmlReader();
  //  StDbXmlReader(ofstream& ofs){ os=&ofs;};
  virtual ~StDbXmlReader();

  void readTable(ifstream &is);

  virtual void pass(char* name, short& i, int len) ;  
  virtual void pass(char* name, int& i, int len);  
  virtual void pass(char* name, long& i, int len);  
  virtual void pass(char* name, unsigned short& i, int len) ;  
  virtual void pass(char* name, unsigned int& i, int len) ;  
  virtual void pass(char* name, unsigned long& i, int len) ;  

  virtual void pass(char* name, float& i, int len);
  virtual void pass(char* name, double& i, int len);
  virtual void pass(char* name, char* i, int len);
  virtual void pass(char* name, unsigned char* i, int len) ;
  virtual void pass(char* name, short* i, int len) ;  
  virtual void pass(char* name, int* i, int len);  
  virtual void pass(char* name, long* i, int len);  
  virtual void pass(char* name, unsigned short* i, int len) ;  
  virtual void pass(char* name, unsigned int* i, int len) ;  
  virtual void pass(char* name, unsigned long* i, int len) ;  
  virtual void pass(char* name, float* i, int len);
  virtual void pass(char* name, double* i, int len);

  //ClassDef(StDbXmlReader,1)

};

#endif






