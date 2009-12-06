
// $Id: TModule.h,v 1.2 2009/12/06 06:47:52 fine Exp $
// $Log: TModule.h,v $
// Revision 1.2  2009/12/06 06:47:52  fine
// Move the Online display package
//
// Revision 1.1  2009/12/01 01:33:33  fine
// Move online display udner OnlTools
//

#ifndef STAR_TModule
#define STAR_TModule

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TModule virtual base class for Modules                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include "StMaker.h"

class  TModule : public StMaker {
  protected:
      TIter   *fNxtModule;
      StMaker *fCurrentMaker;
  public:
    TModule(const char *name="",const char *dummy=0);
    virtual ~TModule();
     
   virtual Int_t Make();
   virtual void  ResetModule(bool deleteOnly=false);
   TIter *Iter() const { return fNxtModule;}
   const StMaker *CurrentMaker() const { return fCurrentMaker;}


   virtual Int_t        GetIventNumber() const ;
   virtual void         SetIventNumber(Int_t iv);
   virtual Int_t        GetEventNumber() const ;
   virtual Int_t        GetRunNumber() const ;
   virtual UInt_t       GetTriggerMask() const;
   virtual const Char_t *GetEventType() const ;
   ClassDef(TModule,0)
};
#endif
