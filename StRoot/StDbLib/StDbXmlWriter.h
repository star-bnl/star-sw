/***************************************************************************
 *
 * $Id: StDbXmlWriter.h,v 1.4 1999/12/03 17:03:24 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  implement typeAcceptor for WRITING XML files of DB-tables
 *
 ***************************************************************************
 *
 * $Log: StDbXmlWriter.h,v $
 * Revision 1.4  1999/12/03 17:03:24  porter
 * added multi-row support for the Xml reader & writer
 *
 * Revision 1.3  1999/09/30 02:06:12  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBXMLWRITER_HH
#define STDBXMLWRITER_HH

#include "tableAcceptor.hh"
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <strstream.h>
#include <string.h>

class StDbTable;

class StDbXmlWriter : public tableAcceptor {

protected:

  ostream* os;//!

public:

  StDbXmlWriter(): os(0) {};
  StDbXmlWriter(ostream& ofs){ os=&ofs;};
  virtual ~StDbXmlWriter(){};

  virtual void streamHeader(const char* name);
  virtual void streamTableName(const char* name);
  virtual void streamEndTableName();
  virtual void streamAccessor();
  virtual void endAccessor();
  virtual void streamRow(int row);
  virtual void streamEndRow();
  virtual void streamTail();
  virtual void ioTable(StDbTable* table);

  virtual void pass(char* name, short& i, int len=0) ;  
  virtual void pass(char* name, int& i, int len=0);  
  virtual void pass(char* name, long& i, int len=0);  
  virtual void pass(char* name, unsigned short& i, int len=0) ;  
  virtual void pass(char* name, unsigned int& i, int len=0) ;  
  virtual void pass(char* name, unsigned long& i, int len=0) ;  

  virtual void pass(char* name, float& i, int len=0);
  virtual void pass(char* name, double& i, int len=0);
  virtual void pass(char* name, char*& i, int len);
  virtual void pass(char* name, unsigned char& i, int len) ;
  virtual void pass(char* name, unsigned char*& i, int len) ;
  virtual void pass(char* name, short*& i, int len) ;  
  virtual void pass(char* name, int*& i, int len);  
  virtual void pass(char* name, long*& i, int len);  
  virtual void pass(char* name, unsigned short*& i, int len) ;  
  virtual void pass(char* name, unsigned int*& i, int len) ;  
  virtual void pass(char* name, unsigned long*& i, int len) ;  
  virtual void pass(char* name, float*& i, int len);
  virtual void pass(char* name, double*& i, int len);

  //ClassDef(StDbXmlWriter,1)

};

#endif


