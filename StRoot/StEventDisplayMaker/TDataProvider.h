// @(#)root/g3d:$Name:  $:$Id: TDataProvider.h,v 1.1 2006/08/23 21:24:47 fine Exp $
// Author: Valeri Fine   21/12/04

#ifndef ROOT_TDataProvider
#define ROOT_TDataProvider


////////////////////////////////////////////////////////////////////////////
//                                                                        //
// TDataProvider                                                          //
//                                                                        //
// TEmcTower is a phi segment of a tube. It has 5 parameters, the same 3  //
// TUBE plus the phi limits. The segment start at first limit and         //
// includes increasing phi value up to the second limit or that plus      //
// 360 degrees.                                                           //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TTUBS
#include "TTUBS.h"
#endif

class TDataProvider : public TObject {
   protected:     
      const UShort_t *fDataSource;
      unsigned char  *fIsAvailable;
      const UShort_t *fDataSourceLength;
      
   public:
     TDataProvider(UShort_t *src=0, unsigned char *available=0, UShort_t *len=0) 
     : fDataSource(src), fIsAvailable(available), fDataSourceLength(len){}
     virtual ~TDataProvider() {}
     virtual Int_t  Attribute(Int_t nSegments,Int_t nSectors) {
       return Int_t (fDataSource[nSegments*120 + nSectors]);
     }
     virtual void   ComputerScale() {}
     inline  Bool_t IsAvailable() const { return fIsAvailable ? *fIsAvailable : kTRUE ;}
     virtual Int_t  NextAttribute() { return 0;}
     inline  UShort_t DataLength() const { return fDataSourceLength ?  *fDataSourceLength : 0; }
     virtual void   ResetCounter(){;} 
     virtual void   ResetAvailable(unsigned char available=0) {*fIsAvailable = available;}
};

#endif


