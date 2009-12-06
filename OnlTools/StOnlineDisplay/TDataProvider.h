// @(#)root/g3d:$Name:  $:$Id: TDataProvider.h,v 1.1 2009/12/06 06:47:52 fine Exp $
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

#include "TObject.h"
struct btow_t;
class TDataProvider : public TObject {
   protected:     
      btow_t   **fDataSource;
      void           **fIsAvailable;
      const Int_t *fDataSourceLength;
      
   public:
     TDataProvider(btow_t **src=0, void **available=0, Int_t *len=0) 
     : fDataSource(src), fIsAvailable(available), fDataSourceLength(len){}
     virtual ~TDataProvider() {}
     virtual void   ComputerScale() {}
     inline  Bool_t IsAvailable() const { return fIsAvailable ? (Bool_t)*fIsAvailable : kTRUE ;}
     virtual Int_t  NextAttribute() { return 0;}
     inline  UShort_t DataLength() const { return fDataSourceLength ?  *fDataSourceLength : 0; }
     virtual void   ResetCounter(){;} 
     virtual void   ResetAvailable(void *available=0) {*fIsAvailable = available;}
};

#endif


