/***************************************************************************
 *
 * $Id: dbStruct.hh,v 1.7 1999/12/07 21:25:25 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Simple dictionary for XML file tags
 *
 ***************************************************************************
 *
 * $Log: dbStruct.hh,v $
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
 int istart;
 int iend;
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
 char* data;
   datav() { setStartKey("<value>");
   setEndKey("</value>") ;};


};

class elem : public basic {

 public:

  datav val;
  char* name;
  char* type;
  stsize size;

   elem() { setStartKey("<db");
            setEndKey("</db") ;};

};

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<elem*, allocator<elem*> > elemVec;
#else
typedef vector<elem*> elemVec;
#endif

class accessor : public basic {

 public:

 elemVec e;
 int nelems;

   accessor() { setStartKey("<StDbAccessor>");
                setEndKey("</StDbAccessor>");};
  virtual ~accessor(){};

 
};


class dbRow : public accessor {

public:
  
  int rowNumber;
  int rowID;

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
  int curRow;
  int numRows;
  //  elemVec e;
  //  int nelems;
  char* name;

   dbTable() { setStartKey("<StDbTable>");
               setEndKey("</StDbTable>");};
  virtual ~dbTable(){};

};


#endif



