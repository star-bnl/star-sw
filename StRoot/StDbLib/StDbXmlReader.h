/***************************************************************************
 *
 * $Id: StDbXmlReader.h,v 1.4 2000/01/10 20:37:55 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  implement typeAcceptor for READING XML files of DB-tables
 *
 ***************************************************************************
 *
 * $Log: StDbXmlReader.h,v $
 * Revision 1.4  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.3  1999/12/03 17:03:24  porter
 * added multi-row support for the Xml reader & writer
 *
 * Revision 1.2  1999/09/30 02:06:12  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBXmlReader_HH
#define STDBXmlReader_HH

#include "typeAcceptor.hh"
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <strstream.h>
#include <string.h>


class dbTable;
class elem;
class accessor;

class StDbXmlReader : public typeAcceptor {

protected:

  //  ofstream* os;//!

  char* loca[20024];//!
  dbTable* tab;//!
  int maxlines;

   void buildDbTable();
   void buildStruct();
   void fillElements(accessor* a);
   elem* findElement(char* name);

public:

  StDbXmlReader();
  //  StDbXmlReader(ofstream& ofs){ os=&ofs;};
  virtual ~StDbXmlReader();

  void readTable(ifstream &is);

  virtual void pass(char* name, short& i, int& len) ;  
  virtual void pass(char* name, int& i, int& len);  
  virtual void pass(char* name, long& i, int& len);  
  virtual void pass(char* name, unsigned short& i, int& len) ;  
  virtual void pass(char* name, unsigned int& i, int& len) ;  
  virtual void pass(char* name, unsigned long& i, int& len) ;  

  virtual void pass(char* name, float& i, int& len);
  virtual void pass(char* name, double& i, int& len);
  virtual void pass(char* name, char*& i, int& len);
  virtual void pass(char* name, unsigned char& i, int& len) ;
  virtual void pass(char* name, unsigned char*& i, int& len) ;
  virtual void pass(char* name, short*& i, int& len) ;  
  virtual void pass(char* name, int*& i, int& len);  
  virtual void pass(char* name, long*& i, int& len);  
  virtual void pass(char* name, unsigned short*& i, int& len) ;  
  virtual void pass(char* name, unsigned int*& i, int& len) ;  
  virtual void pass(char* name, unsigned long*& i, int& len) ;  
  virtual void pass(char* name, float*& i, int& len);
  virtual void pass(char* name, double*& i, int& len);

  //ClassDef(StDbXmlReader,1)

};

#endif






