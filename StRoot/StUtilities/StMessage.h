// $Id: StMessage.h,v 1.1 1999/06/23 15:17:47 genevb Exp $
// $Log: StMessage.h,v $
// Revision 1.1  1999/06/23 15:17:47  genevb
// Introduction of StMessageManager
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessage                                                            //
//                                                                      //
// This is the class of messages used by StMessageManager in STAR.      //
// Messages have a type and message specified at instantiation,         //
// and also include a time-date stamp and options for printing.         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStMessage
#define ClassStMessage

#include <iostream.h>
#include "TString.h"
#include <strstream.h>
#include "TDatime.h"
#include "StMessTypeList.h"

class StMessage : public ostrstream {
   friend ostream& operator<<(ostream& ,StMessage*);

 private:

 protected:
   TString leader;
   const Char_t* type;
   TString location;
   unsigned long runNumber;
//   pair<long, long> eventId;
   TString option;
   const TDatime* messTime; //!

 public:
   StMessage(Char_t* mess="", Char_t* ty="I", Char_t* opt="O");
   StMessage(const StMessage&);
   virtual ~StMessage();
   virtual           void PrintInfo();
   virtual          Int_t Print(Int_t nChars=0);
   virtual const TDatime* GetTime() const {return messTime;}
   virtual const  Char_t* GetType() const {return type;}
   virtual        Char_t* GetMessage() {return str();}
   virtual        Char_t* GetOptions() {return option.Data();}
   virtual           void SetOption(Char_t* opt) {option = opt;}
   ClassDef(StMessage,1)
};

// Globalpointer:
R__EXTERN StMessage* gMessage;
R__EXTERN StMessage* endm;

#endif
