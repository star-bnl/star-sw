#ifndef St_GlobalTag_h 
#define St_GlobalTag_h
//
// $Id: St_GlobalTag.h,v 1.1 2004/07/29 22:57:52 fisyak Exp $
//
// Global tag
//
// $Log: St_GlobalTag.h,v $
// Revision 1.1  2004/07/29 22:57:52  fisyak
// Add Global tags
//
//
#include "TTable.h"
typedef struct GlobalTag {                      // Global:
  UInt_t   TriggerId[32];                       // Trigger Id's satisfied by an event
  UInt_t   uncorrectedNumberOfPrimaries;        // TPC StuRefMult
  UInt_t   uncorrectedNumberOfFtpcEastPrimaries;// FTPC StuFtpcRefMult
  UInt_t   uncorrectedNumberOfFtpcWestPrimaries;// FTPC StuFtpcRefMult
  Double_t xPrimVertex;                         // Primary vertex (x)
  Double_t yPrimVertex;                         //                (y)
  Double_t zPrimVertex;                         //                (z)
  UShort_t vertexFlag;                          // with some flag !=0 if not found
  UShort_t zdcHardSum;                          // trigData->zdcAtChannel(10)
  Float_t  CTBsum;                              // CTB sum mMips[mMaxTrays][mMaxSlats][0]
 } GlobalTag_st ;

class St_GlobalTag : public TTable
{
  public:
    ClassDefTable(St_GlobalTag,GlobalTag_st)
    ClassDef(St_GlobalTag,1) 
};

#endif
