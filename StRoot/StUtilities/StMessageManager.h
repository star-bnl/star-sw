// $Id: StMessageManager.h,v 1.3 1999/06/24 23:23:59 genevb Exp $
// $Log: StMessageManager.h,v $
// Revision 1.3  1999/06/24 23:23:59  genevb
// Added message call for compatibility with old fortran code
//
// Revision 1.2  1999/06/24 16:30:43  genevb
// Fixed some memory leaks
//
// Revision 1.1  1999/06/23 15:17:53  genevb
// Introduction of StMessageManager
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessageManager                                                     //
//                                                                      //
// This class manages the messages in STAR software. It is a singleton. //
// Messages are stored in a vector, and come in several types           //
// (i.e. info, error). The types "I" (info), "W" (warning) and          //
// "E" (error) are predefined. Message finding and summary tools are    //
//  also available.                                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStMessageManager
#define ClassStMessageManager

#ifndef __CINT__
#include "fortranc.h"
#define Message_ F77_NAME(message,MESSAGE)
#define StMessage_ F77_NAME(stmessage,STMESSAGE)
#define StInfo_ F77_NAME(stinfo,STINFO)
#define StWarning_ F77_NAME(stwarning,STWARNING)
#define StError_ F77_NAME(sterror,STERROR)
#define StMessAddType_ F77_NAME(stmessaddtype,StMESSADDTYPE)
extern "C" {
R__EXTERN  void type_of_call Message_(Char_t* mess="", int lines=1, int id=-1);
R__EXTERN  void type_of_call StMessage_(Char_t* mess="", Char_t* type="I", Char_t* opt="O");
R__EXTERN  void type_of_call StInfo_(Char_t* mess="", Char_t* opt="O");
R__EXTERN  void type_of_call StWarning_(Char_t* mess="", Char_t* opt="O");
R__EXTERN  void type_of_call StError_(Char_t* mess="", Char_t* opt="O");
R__EXTERN  void type_of_call StMessAddType_(const Char_t* type, const Char_t* text);
}
#endif

#include "StMessage.h"
#include "StMessTypeList.h"
#include "StMessageCounter.h"

typedef StVector(StMessage*) messVec;
typedef StVector(StMessage*)::iterator messVecIter;
typedef StVector(messVec*) messTypeVec;


class StMessageManager {
 private:
   static StMessageManager* mInstance;
   StMessage* messObj;
   StMessTypeList* messTypeList;
   StMessageCounter* messCounter;

 protected:
   StMessageManager();
   StMessageManager(const StMessageManager&);
   messVec messList;
   messTypeVec messCollection;
   virtual messVecIter FindMessageIter(const Char_t* s1, Char_t* s2="",
         Char_t* s3="", Char_t* s4="", messVec* list=0);
 
 public:
   virtual ~StMessageManager();
   static StMessageManager* Instance();

// Generic Messages:
   virtual StMessage& Message(Char_t* mess="", Char_t* type="I", Char_t* opt="O");
   virtual        int Print() {return messObj->Print();}
   virtual        int PrintList(messVec* list);
   virtual        int PrintAll() {return PrintList(&messList); }
   virtual StMessage& GetCurrent() {return (*messObj);}
   virtual const messVec& GetAll() {return messList;}
   virtual StMessage* FindMessage(const Char_t* s1, Char_t* s2="",
         Char_t* s3="", Char_t* s4="", messVec* list=0);
   virtual   messVec& FindMessageList(const Char_t* s1, Char_t* s2="",
         Char_t* s3="", Char_t* s4="", messVec* list=0);
   virtual        int RemoveMessage(StMessage* mess);
   virtual        int RemoveMessage(const Char_t* s1, Char_t* s2="",
         Char_t* s3="", Char_t* s4="")
         {return RemoveMessage(FindMessage(s1,s2,s3,s4));}
   virtual       void SetLimit(Char_t* str, Int_t n=0) {messCounter->SetLimit(str,n);}
   virtual       void ListLimits() {messCounter->ListLimits();}
   virtual       void RemoveLimit(Char_t* str) {SetLimit(str,-1);}
   virtual       void SwitchOff(Char_t* str) {SetLimit(str,0);}
   virtual       void SwitchOn(Char_t* str) {RemoveLimit(str);}
   virtual       void Summary(Int_t nTerms=1);
   virtual        int AddType(const Char_t* type, const Char_t* text);
   virtual        int ListTypes() {return messTypeList->ListTypes();}

// Info Messages:
   virtual StMessage& Info(Char_t* mess="", Char_t* opt="O")
         { return Message(mess, "I", opt);}
   virtual        int PrintInfos() {return PrintList(messCollection[1]); }
   virtual const messVec& GetInfos() {return *(messCollection[1]);}
   virtual StMessage* FindInfo(const Char_t* s1, Char_t* s2="", Char_t* s3="",
         Char_t* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[1]);}
   virtual messVec& FindInfoList(const Char_t* s1, Char_t* s2="", Char_t* s3="",
         Char_t* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[1]);}

// Warning Messages:
   virtual StMessage& Warning(Char_t* mess="", Char_t* opt="O")
         { return Message(mess, "W", opt);}
   virtual        int PrintWarnings() {return PrintList(messCollection[2]); }
   virtual const messVec& GetWarnings() {return *(messCollection[2]);}
   virtual StMessage* FindWarning(const Char_t* s1, Char_t* s2="", Char_t* s3="",
         Char_t* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[2]);}
   virtual messVec& FindWarningList(const Char_t* s1, Char_t* s2="", Char_t* s3="",
         Char_t* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[2]);}

// Error Messages:
   virtual StMessage& Error(Char_t* mess="", Char_t* opt="O")
         { return Message(mess, "E", opt);}
   virtual        int PrintErrors() {return PrintList(messCollection[3]); }
   virtual const messVec& GetErrors() {return *(messCollection[3]);}
   virtual StMessage* FindError(const Char_t* s1, Char_t* s2="", Char_t* s3="",
         Char_t* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[3]);}
   virtual messVec& FindErrorList(const Char_t* s1, Char_t* s2="", Char_t* s3="",
         Char_t* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[3]);}

   virtual       void PrintInfo();
   ClassDef(StMessageManager,0)
};

// Globalpointer:
R__EXTERN StMessageManager* gMessMgr;

#endif
