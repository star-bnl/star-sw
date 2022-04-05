/***************************************************************************
 *
 * $Id: StDbXmlWriter.h,v 1.9 2004/01/15 00:02:25 fisyak Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  implement typeAcceptor for WRITING XML files of DB-tables
 *
 ***************************************************************************
 *
 * $Log: StDbXmlWriter.h,v $
 * Revision 1.9  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.8  2003/09/16 22:44:18  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.7  2003/09/02 17:57:50  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.6  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.5  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
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

#include <stdlib.h>
#include <string.h>
#include "stdb_streams.h"
#include "tableAcceptor.hh"

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

  virtual void pass(char* name, short& i, int& len) ;  
  virtual void pass(char* name, int& i, int& len);  
  virtual void pass(char* name, long& i, int& len);  
  virtual void pass(char* name, unsigned short& i, int& len) ;  
  virtual void pass(char* name, unsigned int& i, int& len) ;  
  virtual void pass(char* name, unsigned long& i, int& len) ;  
  virtual void pass(char* name, long long& i, int& len) ;  

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
  virtual void pass(char* name, long long*& i, int& len) ;  
  virtual void pass(char* name, float*& i, int& len);
  virtual void pass(char* name, double*& i, int& len);

  //ClassDef(StDbXmlWriter,1)

};

#endif


