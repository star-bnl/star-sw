/***************************************************************************
 *
 * $Id: dbStruct.hh,v 1.10 2016/05/25 20:17:51 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Simple dictionary for XML file tags
 *
 ***************************************************************************
 *
 * $Log: dbStruct.hh,v $
 * Revision 1.10  2016/05/25 20:17:51  dmitry
 * coverity - uninit ctor
 *
 * Revision 1.9  2000/01/27 05:54:35  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.8  1999/12/28 21:31:42  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.7  1999/12/07 21:25:25  porter
 * some fixes for linux warnings
 *
 * Revision 1.6  1999/12/03 17:03:24  porter
 * added multi-row support for the Xml reader & writer
 *
 * Revision 1.5  1999/09/30 02:06:13  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef DBSTRUCT_HH
#define DBSTRUCT_HH

#include <vector>


class basic {

 public:

  virtual ~basic(){};
 char startKey[20];
 char endKey[20];
 int istart = 0;
 int iend = 0;
  virtual void setStartKey(const char* key){ strcpy(startKey,key); };
  virtual void setEndKey(const char* key){ strcpy(endKey,key); };


};

class stsize : public basic {

public:
 int isize;
   stsize(): isize(0) { setStartKey("<length>");
   setEndKey("</length>") ;};


};

class datav : public basic {

public:
 char* data = 0;
   datav() { setStartKey("<value>");
   setEndKey("</value>") ;};


};

class elem : public basic {

 public:

  datav val;
  char* name = 0;
  char* type = 0;
  stsize size;

   elem() { setStartKey("<db");
            setEndKey("</db") ;};

};

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<elem*, allocator<elem*> > elemVec;
#else
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif
typedef vector<elem*> elemVec;
#endif

class accessor : public basic {

 public:

 elemVec e;
 int nelems = 0;

   accessor() { setStartKey("<StDbAccessor>");
                setEndKey("</StDbAccessor>");};
  virtual ~accessor(){};

 
};


class dbRow : public accessor {

public:
  
  int rowNumber = 0;
  int rowID = 0;

  dbRow() { setStartKey("<TabRow>");
            setEndKey("</TabRow>"); };
  virtual ~dbRow(){};
};

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<dbRow*, allocator<dbRow*> > rowVec;
#else
typedef vector<dbRow*> rowVec;
#endif

class dbTable : public basic {
  
 public: 

  accessor a;
  rowVec row;
  int curRow = 0;
  int numRows = 0;
  //  elemVec e;
  //  int nelems;
  char* name = 0;

   dbTable() { setStartKey("<StDbTable>");
               setEndKey("</StDbTable>");};
  virtual ~dbTable(){};

};


#endif







