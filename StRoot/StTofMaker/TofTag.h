/**************************************************************************
*
* $Id: TofTag.idl,v 1.1 2001/10/10 15:18:21 geurts Exp $
*
***************************************************************************
*
* Description: The tof tag table. 
*
***************************************************************************
*
* $Log: TofTag.idl,v $
* Revision 1.1  2001/10/10 15:18:21  geurts
* TofTag introduced
*
*
**************************************************************************/
#ifndef __TofTag__
#define __TofTag__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
struct TofTag_st {
  long tofEventType; // differentiates Strobe Events from Physics Events
};
class St_TofTag : public TTable {
 public:
  ClassDefTable(St_TofTag,TofTag_st)
  ClassDef(St_TofTag,1) //C++ container for chain/makers status 
};
#endif /* __TofTag__ */
