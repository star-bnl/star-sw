// $Id: StMessageManager.h,v 1.12 1999/07/23 16:56:40 genevb Exp $
// $Log: StMessageManager.h,v $
// Revision 1.12  1999/07/23 16:56:40  genevb
// Fix extern C prototypes, default options for omitted types, Linux bug with multi-line messages
//
// Revision 1.11  1999/07/17 00:23:24  genevb
// Fixed bug when option fields are empty in FORTRAN, and let type limits be set before types are even added
//
// Revision 1.10  1999/07/08 22:58:18  genevb
// Created an abstract interface with StMessMgr.h hiding template implementation from others, a few other small fixes
//
// Revision 1.9  1999/07/01 23:32:53  genevb
// Change default message typing
//
// Revision 1.8  1999/07/01 01:24:46  genevb
// Fixed FORTRAN character string bug on linux, removed a memory leak from Summary()
//
// Revision 1.7  1999/06/29 23:32:42  genevb
// Handle multi-line calls to fortran routines better
//
// Revision 1.5  1999/06/28 15:42:13  genevb
// Added Debug message class
//
// Revision 1.4  1999/06/28 02:40:56  genevb
// Additional backward compatibilit with MSG (msg_enable, msg_enabled, msg_disable
//
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
// It inherits from StMessMgr, which provides the external interface.   //
// Messages are stored in a vector, and come in several types           //
// (i.e. info, error, debug ). The types "I" (info), "W" (warning),     //
// "E" (error), and "D" (debug) are predefined. Message finding         //
// and summary tools are also available.                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStMessageManager
#define ClassStMessageManager


#include "StMessage.h"
#include "StMessTypeList.h"
#include "StMessageCounter.h"

#define ClassMessVec
typedef StVector(StMessage*) messVec;
typedef StVector(StMessage*)::iterator messVecIter;
typedef StVector(messVec*) messTypeVec;

#include "StMessMgr.h"

class StMessageManager : public StMessMgr {

 private:
   static StMessMgr* mInstance;       //!
   StMessTypeList* messTypeList;      //!
   StMessageCounter* messCounter;     //!
   char* curType;                     //!
   char* curOpt;                      //!

 protected:
   StMessageManager();
   StMessageManager(const StMessageManager&);
   messVec messList;
   messTypeVec messCollection;
   virtual messVecIter FindMessageIter(const char* s1, char* s2="",
         char* s3="", char* s4="", messVec* list=0);
   virtual        void BuildMessage(char* mess="", char* type="", char* opt=0);
 
 public:
   virtual ~StMessageManager();
   static StMessMgr* Instance();      //!

// Generic Messages:
   virtual StMessMgr& Message(char* mess="", char* type="", char* opt=0);
   virtual       void Print();
   virtual        int PrintList(messVec* list);
   virtual        int PrintAll() {return PrintList(&messList); }
   virtual const messVec* GetAll() {return (&messList);}
   virtual StMessage* FindMessage(const char* s1, char* s2="",
         char* s3="", char* s4="", messVec* list=0);
   virtual   messVec* FindMessageList(const char* s1, char* s2="",
         char* s3="", char* s4="", messVec* list=0);
   virtual        int RemoveMessage(StMessage* mess);
   virtual        int RemoveMessage(const char* s1, char* s2="",
         char* s3="", char* s4="")
         {return RemoveMessage(FindMessage(s1,s2,s3,s4));}
   virtual       void SetLimit(char* str, int n=0) {messCounter->SetLimit(str,n);}
   virtual        int GetLimit(char* str) {return messCounter->GetLimit(str);}
   virtual       void ListLimits() {messCounter->ListLimits();}
   virtual       void RemoveLimit(char* str) {SetLimit(str,-1);}
   virtual       void SwitchOff(char* str) {SetLimit(str,0);}
   virtual       void SwitchOn(char* str) {RemoveLimit(str);}
   virtual       void Summary(size_t nTerms=1);
   virtual        int AddType(const char* type, const char* text);
   virtual        int ListTypes() {return messTypeList->ListTypes();}

// Info Messages:
   virtual StMessMgr& Info(char* mess="", char* opt="O")
         { return Message(mess, "I", opt);}
   virtual        int PrintInfos() {return PrintList(messCollection[1]); }
   virtual const messVec* GetInfos() {return (messCollection[1]);}
   virtual StMessage* FindInfo(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[1]);}
   virtual messVec* FindInfoList(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[1]);}

// Warning Messages:
   virtual StMessMgr& Warning(char* mess="", char* opt="E")
         { return Message(mess, "W", opt);}
   virtual        int PrintWarnings() {return PrintList(messCollection[2]); }
   virtual const messVec* GetWarnings() {return (messCollection[2]);}
   virtual StMessage* FindWarning(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[2]);}
   virtual messVec* FindWarningList(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[2]);}

// Error Messages:
   virtual StMessMgr& Error(char* mess="", char* opt="E")
         { return Message(mess, "E", opt);}
   virtual        int PrintErrors() {return PrintList(messCollection[3]); }
   virtual const messVec* GetErrors() {return (messCollection[3]);}
   virtual StMessage* FindError(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[3]);}
   virtual messVec* FindErrorList(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[3]);}

// Debug Messages:
   virtual StMessMgr& Debug(char* mess="", char* opt="O")
         { return Message(mess, "D", opt);}
   virtual        int PrintDebug() {return PrintList(messCollection[4]); }
   virtual const messVec* GetDebugs() {return (messCollection[4]);}
   virtual StMessage* FindDebug(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[4]);}
   virtual messVec* FindDebugList(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[4]);}

   virtual       void PrintInfo();
#ifdef __ROOT__
   ClassDef(StMessageManager,0)
#endif
};


#endif
