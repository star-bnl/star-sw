/***************************************************************************
 *
 * $Id: StTableDescriptorI.h,v 1.2 1999/09/30 02:06:12 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Abstract class defining access to a table-description
 *
 ***************************************************************************
 *
 * $Log: StTableDescriptorI.h,v $
 * Revision 1.2  1999/09/30 02:06:12  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef StTableDescriptorI_HH
#define StTableDescriptorI_HH

#ifdef SOLARIS
typedef int bool;
#ifndef false
#define false 0
#define true 1
#endif
#endif

#include "StTypeEnum.h"

class StTableDescriptorI {

public:

  virtual ~StTableDescriptorI(){};
  virtual StTableDescriptorI* getCpy()                               = 0;
  virtual unsigned int getNumElements()                        const = 0;
  virtual unsigned int getTotalSizeInBytes()                   const = 0;
  virtual char* getElementName(int elementNum)                 const = 0;
  virtual unsigned int getElementOffset(int elementNum)        const = 0;
  virtual unsigned int getElementSize(int elementNum)          const = 0;
  virtual StTypeE getElementType(int elementNum)               const = 0;
  virtual unsigned int* getElementDimensions(int elementNum)   const = 0;
  virtual unsigned int getElementLength(int elementNum)        const = 0;
  virtual unsigned int getElementNumDimensions(int elementNum) const = 0;
  virtual unsigned int getElementIndexLength(int elementNum, int dimensionNum) const = 0;


};

#endif

