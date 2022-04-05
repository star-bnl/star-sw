/***************************************************************************
 *
 * $Id: StDbFastSqlWriter.h,v 1.4 2004/01/15 00:02:25 fisyak Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  implement typeAcceptor for WRITING XML files of DB-tables
 *
 ***************************************************************************
 *
 * $Log: StDbFastSqlWriter.h,v $
 * Revision 1.4  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.3  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.2  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2003/04/11 22:47:36  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
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
#include "tableAcceptor.hh"
#include "stdb_streams.h"

class StString;
class StDbTable;

class StDbFastSqlWriter : public tableAcceptor {

protected:

  StString* os;//!

public:

  StDbFastSqlWriter(): os(0) {};
  StDbFastSqlWriter(StString& ofs){ os=&ofs;};
  virtual ~StDbFastSqlWriter(){};

  virtual void streamHeader(const char* name) {/* no-op */ };
  virtual void streamTableName(const char* name){/* no-op */ };
  virtual void streamEndTableName(){/* no-op */ };
  virtual void streamAccessor(){/* no-op */ };
  virtual void endAccessor(){/* no-op */ };
  virtual void streamRow(int row){/* no-op */ };
  virtual void streamEndRow(){/* no-op */ };
  virtual void streamTail(){/* no-op */ };
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

  //ClassDef(StDbFastSqlWriter,1)

};

inline void StDbFastSqlWriter::pass(char* name, short& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, int& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, long& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, long long& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, unsigned short& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, unsigned char& i, int& len){ *os<<(int)i; };
inline void StDbFastSqlWriter::pass(char* name, unsigned int& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, unsigned long& i, int& len){ *os<<","<<i; };
//inline void StDbFastSqlWriter::pass(char* name, unsigned long long& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, float& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, double& i, int& len){ *os<<","<<i; };
inline void StDbFastSqlWriter::pass(char* name, char*& i, int& len){ 
*os<<",'"<<i<<"'"; };



#endif










