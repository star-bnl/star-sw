/***************************************************************************
 *
 * $Id: tableAcceptor.hh,v 1.6 2003/09/10 19:47:06 perev Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  buffer for file I/O for StDbTable (e.g. XmlReader/Writer)
 *
 ***************************************************************************
 *
 * $Log: tableAcceptor.hh,v $
 * Revision 1.6  2003/09/10 19:47:06  perev
 * ansi corrs
 *
 * Revision 1.5  2003/09/02 17:57:50  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2001/02/09 23:06:26  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.3  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
 * Revision 1.2  1999/09/30 02:06:15  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef TABLEACCEPTOR_HH
#define TABLEACCEPTOR_HH


#include "typeAcceptor.hh"

class StDbTable;

class tableAcceptor : public typeAcceptor {

public:
 
  virtual ~tableAcceptor(){};

  virtual void streamHeader(const char* name) = 0;
  virtual void streamTableName(const char* name) = 0;
  virtual void streamEndTableName() = 0;
  virtual void streamAccessor() = 0;
  virtual void endAccessor() = 0;
  virtual void streamRow(int row) = 0;
  virtual void streamEndRow() = 0;
  virtual void streamTail() = 0;

  virtual void ioTable(StDbTable* table) = 0;  

};

#endif


