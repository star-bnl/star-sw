// $Id: StMessageManager.h,v 1.8 1999/07/01 01:24:46 genevb Exp $
// $Log: StMessageManager.h,v $
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
// Messages are stored in a vector, and come in several types           //
// (i.e. info, error, debug ). The types "I" (info), "W" (warning),     //
// "E" (error), and "D" (debug) are predefined. Message finding         //
// and summary tools are also available.                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStMessageManager
#define ClassStMessageManager

#ifndef __CINT__
#include "fortranc.h"
#define Message_ F77_NAME(message,MESSAGE)
#define Msg_Enable_ F77_NAME(msg_enable,MSG_ENABLE)
#define Msg_Enabled_ F77_NAME(msg_enabled,MSG_ENABLED)
#define Msg_Disable_ F77_NAME(msg_disable,MSG_DISABLE)
#define StMessage_ F77_NAME(stmessage,STMESSAGE)
#define StInfo_ F77_NAME(stinfo,STINFO)
#define StWarning_ F77_NAME(stwarning,STWARNING)
#define StError_ F77_NAME(sterror,STERROR)
#define StDebug_ F77_NAME(stdebug,STDEBUG)
#define StMessAddType_ F77_NAME(stmessaddtype,StMESSADDTYPE)
extern "C" {
R__EXTERN  void type_of_call Message_(char* mess="", int *lines=0, int *id=0,
                                  size_t len=0);
R__EXTERN  void type_of_call Msg_Enable_(char* mess="",
                                  size_t len=0);
R__EXTERN   int type_of_call Msg_Enabled_(char* mess="", int *id=0,
                                  size_t len=0);
R__EXTERN  void type_of_call Msg_Disable_(char* mess="",
                                  size_t len=0);
R__EXTERN  void type_of_call StMessage_(char* mess="", char* type="I", char* opt="O",
                                  size_t len1=0, size_t len2=1, size_t len3=1);
R__EXTERN  void type_of_call StInfo_(char* mess="", char* opt="O",
                                  size_t len1=0, size_t len2=1);
R__EXTERN  void type_of_call StWarning_(char* mess="", char* opt="O",
                                  size_t len1=0, size_t len2=1);
R__EXTERN  void type_of_call StError_(char* mess="", char* opt="O",
                                  size_t len1=0, size_t len2=1);
R__EXTERN  void type_of_call StDebug_(char* mess="", char* opt="O",
                                  size_t len1=0, size_t len2=1);
R__EXTERN  void type_of_call StMessAddType_(const char* type, const char* text,
                                  size_t len1=0, size_t len2=0);
}
#endif

#include "StMessage.h"
#include "StMessTypeList.h"
#include "StMessageCounter.h"
#include <strstream.h>

typedef StVector(StMessage*) messVec;
typedef StVector(StMessage*)::iterator messVecIter;
typedef StVector(messVec*) messTypeVec;


class StMessageManager : public ostrstream {
   friend ostream& operator<<(ostream& ,StMessage*);

 private:
   static StMessageManager* mInstance;
   StMessTypeList* messTypeList;
   StMessageCounter* messCounter;
   char* curType;
   char* curOpt;

 protected:
   StMessageManager();
   StMessageManager(const StMessageManager&);
   messVec messList;
   messTypeVec messCollection;
   virtual messVecIter FindMessageIter(const char* s1, char* s2="",
         char* s3="", char* s4="", messVec* list=0);
   virtual        void BuildMessage(char* mess="", char* type="I",
                                                      char* opt="O");
 
 public:
   virtual ~StMessageManager();
   static StMessageManager* Instance();

// Generic Messages:
   virtual StMessageManager& Message(char* mess="", char* type="I",
                                                      char* opt="O");
   virtual       void Print();
   virtual        int PrintList(messVec* list);
   virtual        int PrintAll() {return PrintList(&messList); }
   virtual const messVec& GetAll() {return messList;}
   virtual StMessage* FindMessage(const char* s1, char* s2="",
         char* s3="", char* s4="", messVec* list=0);
   virtual   messVec& FindMessageList(const char* s1, char* s2="",
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
   virtual StMessageManager& Info(char* mess="", char* opt="O")
         { return Message(mess, "I", opt);}
   virtual        int PrintInfos() {return PrintList(messCollection[1]); }
   virtual const messVec& GetInfos() {return *(messCollection[1]);}
   virtual StMessage* FindInfo(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[1]);}
   virtual messVec& FindInfoList(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[1]);}

// Warning Messages:
   virtual StMessageManager& Warning(char* mess="", char* opt="O")
         { return Message(mess, "W", opt);}
   virtual        int PrintWarnings() {return PrintList(messCollection[2]); }
   virtual const messVec& GetWarnings() {return *(messCollection[2]);}
   virtual StMessage* FindWarning(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[2]);}
   virtual messVec& FindWarningList(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[2]);}

// Error Messages:
   virtual StMessageManager& Error(char* mess="", char* opt="O")
         { return Message(mess, "E", opt);}
   virtual        int PrintErrors() {return PrintList(messCollection[3]); }
   virtual const messVec& GetErrors() {return *(messCollection[3]);}
   virtual StMessage* FindError(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[3]);}
   virtual messVec& FindErrorList(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[3]);}

// Debug Messages:
   virtual StMessageManager& Debug(char* mess="", char* opt="O")
         { return Message(mess, "D", opt);}
   virtual        int PrintDebug() {return PrintList(messCollection[4]); }
   virtual const messVec& GetDebugs() {return *(messCollection[4]);}
   virtual StMessage* FindDebug(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessage(s1,s2,s3,s4,messCollection[4]);}
   virtual messVec& FindDebugList(const char* s1, char* s2="", char* s3="",
         char* s4="") {return FindMessageList(s1,s2,s3,s4,messCollection[4]);}

   virtual       void PrintInfo();
#ifdef __ROOT__
   ClassDef(StMessageManager,0)
#endif
};

// Globalpointers:
R__EXTERN StMessageManager* gMessMgr;
R__EXTERN StMessage* gMessage;
R__EXTERN StMessage* endm;


#endif
