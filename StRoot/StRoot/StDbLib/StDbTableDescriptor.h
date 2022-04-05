/***************************************************************************
 *
 * $Id: StDbTableDescriptor.h,v 1.13 2016/05/25 20:17:51 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Class implement table-descriptor (memory/name of data-elements)
 *              this descriptor is loaded from database
 *
 ***************************************************************************
 *
 * $Log: StDbTableDescriptor.h,v $
 * Revision 1.13  2016/05/25 20:17:51  dmitry
 * coverity - uninit ctor
 *
 * Revision 1.12  2009/09/28 19:14:10  dmitry
 * row size is not *static* anymore, tables have different row sizes
 *
 * Revision 1.11  2009/09/10 18:06:08  dmitry
 * struct alignment fix, does not rely on fixed 4 byte cap anymore - runtime align calculation is now in use
 *
 * Revision 1.10  2007/09/03 05:39:03  fisyak
 * Anti Cint corrections
 *
 * Revision 1.9  2005/09/07 22:04:02  deph
 * update to correct padding issue for packed tables
 *
 * Revision 1.8  2001/12/05 17:16:35  porter
 * stand-alone make file no longer had "DLINUX" in compile but this is still needed
 * and returned. Also retrieve elementID list  in query by whereClause for plotting
 * many row instances.
 *
 * Revision 1.7  2001/01/22 18:38:00  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 * Revision 1.6  2000/03/28 17:03:19  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.5  1999/12/03 19:02:02  porter
 * modified descriptor to accept tableDescriptor once this St_base object
 * has been updated to have longer name lengths.
 *
 * Revision 1.4  1999/10/19 14:30:40  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.3  1999/09/30 02:06:10  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBTABLEDESCRIPTOR_HH
#define STDBTABLEDESCRIPTOR_HH

#include <string.h>
#include "StTableDescriptorI.h"
#ifndef __CINT__
#include "StDbBuffer.h"
#else
class StDbBuffer;
#endif
struct tableDescriptor {
  StTypeE type = Stchar;      // enumerated basic type
  char name[60];     //  element name
  unsigned int size = 0; //  element size in bytes
  unsigned int  offset = 0; // byte offset in table to this element
  unsigned int  dimensionlen[4]; // len per dimension if multi-D array
};


class StDbTableDescriptor : public StTableDescriptorI {

protected:

 unsigned int mnumElements = 0;
 unsigned int mtableSize = 0;
 int offsetToNextEmptyByte = 0;
 int offsetToLast4Bytes = 0;
 StTypeE lastType = Stchar;
 int padsize = 0;

 unsigned int mAlign[Stmaxtype];

 tableDescriptor* mcols = 0;
 int mMax = 0;
 int mCur = 0;

 // db unique ids
 int mstructID = 0;
 int mschemaID = 0;
 int rowSizeTT = 0;
 bool misValid = false;  // schema has been filled
 bool mhasDouble = false;
 unsigned int maxAlign = 0;

  void init();
  virtual void reSize();
  virtual void fillSizeAndOffset(char* length, int elementNum);
  virtual void fillLengths(char* length, int elementNum);
  virtual StTypeE getType(char* type);
  virtual unsigned int getSize(StTypeE type);
  virtual unsigned int getAlign(StTypeE type);

public:

  StDbTableDescriptor();
  StDbTableDescriptor(int structID, int schemaID);
  StDbTableDescriptor(StDbTableDescriptor& d);
  virtual ~StDbTableDescriptor() {if(mcols) delete [] mcols; }
  virtual void fillElement(StDbBuffer* buff, int tableID);

  virtual StTableDescriptorI* getCpy();
  virtual tableDescriptor* getTableDescriptor() const;
  virtual unsigned int getNumElements() const;
  virtual unsigned int getTotalSizeInBytes() const;
  virtual char* getElementName(int elementNum) const;
  virtual unsigned int getElementOffset(int elementNum) const;
  virtual unsigned int getElementSize(int elementNum)  const;
  virtual StTypeE getElementType(int elementNum) const;
  virtual unsigned int* getElementDimensions(int elementNum) const;
  virtual unsigned int getElementLength(int elementNum) const;
  virtual unsigned int getElementNumDimensions(int elementNum) const;
  virtual unsigned int getElementIndexLength(int elementNum, int dimensionNum) const;
  
   
  int getSchemaID() const;
  int getStructID() const;
  void setSchemaID(int id);
  void setStructID(int id);
  bool IsValid() const;
  int getCurrentInternalSize();
  int getTrowSize() ;  
  void endRowPadding();
  void storeRowSize(int rowSize);
  unsigned int getMaxAlign();
  
};
#ifndef __CINT__
inline unsigned int
StDbTableDescriptor::getNumElements() const {return mnumElements;}

inline unsigned int
StDbTableDescriptor::getTotalSizeInBytes() const {return mtableSize;}

inline char*
StDbTableDescriptor::getElementName(int elementNum) const { 
char* retVal = new char[strlen(mcols[elementNum].name)+1];
strcpy(retVal,mcols[elementNum].name);
return retVal;
}

inline unsigned int
StDbTableDescriptor::getElementOffset(int elementNum) const { 
return mcols[elementNum].offset;
}

inline unsigned int StDbTableDescriptor::getElementSize(int elementNum) const{ 
return mcols[elementNum].size;
}

inline StTypeE StDbTableDescriptor::getElementType(int elementNum) const { 
return mcols[elementNum].type;
}

inline unsigned int* 
StDbTableDescriptor::getElementDimensions(int elementNum) const { 
return &mcols[elementNum].dimensionlen[0];
}

///////////////////////////////////////////////////////////////

inline unsigned int 
StDbTableDescriptor::getElementLength(int elementNum) const { 

int retVal=1;
int j;
  int k= (int)(sizeof(mcols[elementNum].dimensionlen)/sizeof(j));
  for(j=0;j<k;j++)retVal *= mcols[elementNum].dimensionlen[j]; 
return retVal;
}

///////////////////////////////////////////////////////////////////

inline unsigned int
StDbTableDescriptor::getElementNumDimensions(int elementNum) const { 
int retVal=1;
int j;
  int k= (int)(sizeof(mcols[elementNum].dimensionlen)/sizeof(j));
  for(j=0;j<k;j++){
    if(mcols[elementNum].dimensionlen[j]>1)retVal=j+1;  // last dimension >1
  }
return retVal;
}

inline unsigned int
StDbTableDescriptor::getElementIndexLength(int elementNum, int dimensionNum) const { 
return mcols[elementNum].dimensionlen[dimensionNum];
}

inline unsigned int StDbTableDescriptor::getSize(StTypeE type){ return mycsize[type];}
inline unsigned int StDbTableDescriptor::getAlign(StTypeE type){ return mAlign[type];}
inline int  StDbTableDescriptor::getSchemaID() const { return mschemaID; }
inline int  StDbTableDescriptor::getStructID() const { return mstructID; }
inline void StDbTableDescriptor::setSchemaID(int id) { mschemaID=id; }
inline void StDbTableDescriptor::setStructID(int id) { mstructID=id; }
inline bool StDbTableDescriptor::IsValid() const { return misValid; }
inline int  StDbTableDescriptor::getCurrentInternalSize() { return mMax; };
inline void StDbTableDescriptor::storeRowSize(int rowSize) { rowSizeTT = rowSize; }
inline int  StDbTableDescriptor::getTrowSize() { return rowSizeTT; };
inline unsigned int  StDbTableDescriptor::getMaxAlign() { return maxAlign; };
#endif

#endif






