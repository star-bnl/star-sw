/***************************************************************************
 *
 * $Id: tableAcceptor.hh,v 1.2 1999/09/30 02:06:15 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  buffer for file I/O for StDbTable (e.g. XmlReader/Writer)
 *
 ***************************************************************************
 *
 * $Log: tableAcceptor.hh,v $
 * Revision 1.2  1999/09/30 02:06:15  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef TABLEACCEPTOR_HH
#define TABLEACCEPTOR_HH


#include "typeAcceptor.hh"
#include <iostream.h>

class tableAcceptor : public typeAcceptor {

public:
 
  virtual ~tableAcceptor(){};

  virtual void streamHeader(const char* name) = 0;
  virtual void streamAccessor() = 0;
  virtual void endAccessor() = 0;
  virtual void streamTail() = 0;

};

#endif


