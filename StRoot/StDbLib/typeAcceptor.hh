/***************************************************************************
 *
 * $Id: typeAcceptor.hh,v 1.6 2001/10/24 04:05:20 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: buffer for file I/O for StDbTable (e.g. XmlReader/Writer)
 *
 ***************************************************************************
 *
 * $Log: typeAcceptor.hh,v $
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
 * Revision 1.4  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
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
  virtual void pass(char* name, short& i, int& len) = 0;  
  virtual void pass(char* name, int& i, int& len) = 0;  
  virtual void pass(char* name, long& i, int& len) = 0;  
  virtual void pass(char* name, unsigned short& i, int& len) = 0;  
  virtual void pass(char* name, unsigned int& i, int& len) = 0;  
  virtual void pass(char* name, unsigned long& i, int& len) = 0;  
  virtual void pass(char* name, long long& i, int& len) = 0;  
  virtual void pass(char* name, float& i, int& len) = 0;
  virtual void pass(char* name, double& i, int& len) = 0;

  virtual void pass(char* name, char*& i, int& len) = 0;
  virtual void pass(char* name, unsigned char& i, int& len) = 0;
  virtual void pass(char* name, unsigned char*& i, int& len) = 0;

  virtual void pass(char* name, short*& i, int& len) = 0;  
  virtual void pass(char* name, int*& i, int& len) = 0;  
  virtual void pass(char* name, long*& i, int& len) = 0;  
  virtual void pass(char* name, unsigned short*& i, int& len) = 0;  
  virtual void pass(char* name, unsigned int*& i, int& len) = 0;  
  virtual void pass(char* name, unsigned long*& i, int& len) = 0;  
  virtual void pass(char* name, long long*& i, int& len) = 0;  
  virtual void pass(char* name, float*& i, int& len) = 0;
  virtual void pass(char* name, double*& i, int& len) = 0;

};

#endif







