/***************************************************************************
 *
 * $Id: StDbTableDescriptor.h,v 1.6 2000/03/28 17:03:19 porter Exp $
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
#include "StDbBuffer.h"


struct tableDescriptor {
  StTypeE type;      // enumerated basic type
  char name[60];     //  element name
  unsigned int size; //  element size in bytes
  unsigned int  offset; // byte offset in table to this element
  unsigned int  dimensionlen[4]; // len per dimension if multi-D array
};


class StDbTableDescriptor : public StTableDescriptorI {

protected:

 unsigned int mnumElements;
 unsigned int mtableSize;
 int offsetToNextEmptyByte;
 int offsetToLast4Bytes;
 StTypeE lastType;
 int padsize;

 tableDescriptor* mcols;
 int mMax;
 int mCur;


  virtual void reSize();
  virtual void fillSizeAndOffset(char* length, int elementNum);
  virtual void fillLengths(char* length, int elementNum);
  virtual StTypeE getType(char* type);
  virtual unsigned int getSize(StTypeE type);

public:

  StDbTableDescriptor();
  //  StDbTableDescriptor(_Descriptor* d, unsigned int numElements, unsigned int sizeOfStruct); // descriptor from StDbBroker
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

};

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

inline unsigned int
StDbTableDescriptor::getElementSize(int elementNum) const { 
return mcols[elementNum].size;
}

inline StTypeE
StDbTableDescriptor::getElementType(int elementNum) const { 
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

inline unsigned int
StDbTableDescriptor::getSize(StTypeE type){ return mycsize[type];}


#endif





