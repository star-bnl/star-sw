#ifndef MYSQLTYPER_HH
#define MYSQLTYPER_HH

#include "typeAcceptor.hh"
#include "mysql.h"
#include "mysql_com.h"
#include <iostream.h>
#include <stdlib.h>
#include <strstream.h>

class mysqlTypeR : public typeAcceptor {

  MYSQL_ROW mrow;
  MYSQL_FIELD* mfields;
  int mcolumn;
  int mnum_fields;
  unsigned long* mcol_length;

public:
 
  mysqlTypeR(){};
  ~mysqlTypeR(){};

  virtual bool initArray(MYSQL_ROW row, MYSQL_FIELD* fields, int column, int max_column, unsigned long* col_length);

  virtual void pass(char* name, int& i,  unsigned int size);  
  virtual void pass(char* name, long& i,  unsigned int size);  
  virtual void pass(char* name, float& i, unsigned int size);
  virtual void pass(char* name, double& i, unsigned int size);
  virtual void pass(char* name, char* i, unsigned int size);
  virtual void pass(char* name, int* i, unsigned int size);  
  virtual void pass(char* name, long* i, unsigned int size);  
  virtual void pass(char* name, float* i, unsigned int size);
  virtual void pass(char* name, double* i, unsigned int size);

};

#endif








