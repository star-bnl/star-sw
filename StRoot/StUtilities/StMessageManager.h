// $Id: StMessageManager.h,v 1.17 2000/03/30 16:12:55 genevb Exp $
// $Log: StMessageManager.h,v $
// Revision 1.17  2000/03/30 16:12:55  genevb
// Add NoLimits() capability to turn off message limiting.
//
// Revision 1.16  2000/02/29 16:41:57  genevb
// Fortran-compliant interface
//
// Revision 1.15  2000/01/25 16:01:29  fisyak
// Devorce with StAF
//
// Revision 1.14  2000/01/05 19:53:46  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
// Revision 1.13  1999/08/10 22:07:35  genevb
// Added QAInfo message types
//
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
// "E" (error), "D" (debug), and "Q" (QAInfo) are predefined. Message   //
// finding and summary tools are also available.                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStMessageManager
#define ClassStMessageManager

#ifndef __CINT__
#include "StarCallf77.h"
#define Message_ F77_NAME(message,MESSAGE)
#define Msg_Enable_ F77_NAME(msg_enable,MSG_ENABLE)
#define Msg_Enabled_ F77_NAME(msg_enabled,MSG_ENABLED)
#define Msg_Disable_ F77_NAME(msg_disable,MSG_DISABLE)
#define StMessage_ F77_NAME(stmessage,STMESSAGE)
#define StInfo_ F77_NAME(stinfo,STINFO)
#define StWarning_ F77_NAME(stwarning,STWARNING)
#define StError_ F77_NAME(sterror,STERROR)
#define StDebug_ F77_NAME(stdebug,STDEBUG)
#define QAInfo_ F77_NAME(qainfo,QAINFO)
#define StInfoOpt_ F77_NAME(stinfoopt,STINFOOPT)
#define StWarningOpt_ F77_NAME(stwarningopt,STWARNINGOPT)
#define StErrorOpt_ F77_NAME(sterroropt,STERROROPT)
#define StDebugOpt_ F77_NAME(stdebugopt,STDEBUGOPT)
#define QAInfoOpt_ F77_NAME(qainfoopt,QAINFOOPT)
#define StMessAddType_ F77_NAME(stmessaddtype,STMESSADDTYPE)
extern "C" {
void type_of_call Message_(const char* mess, int *lines, int *id, size_t len);
void type_of_call Msg_Enable_(const char* mess, size_t len);
 int type_of_call Msg_Enabled_(const char* mess, int *id, size_t len);
void type_of_call Msg_Disable_(const char* mess, size_t len);
void type_of_call MessageOut(const char* msg, int *lines, size_t len);
void type_of_call StMessage_(const char* mess, const char* type,
                                  const char* opt, size_t len1,
				  size_t len2, size_t len3);
void type_of_call StInfo_(const char* mess, size_t len);
void type_of_call StWarning_(const char* mess, size_t len);
void type_of_call StError_(const char* mess, size_t len);
void type_of_call StDebug_(const char* mess, size_t len);
void type_of_call QAInfo_(const char* mess, size_t len);
void type_of_call StInfoOpt_(const char* mess, const char* opt,
                                  size_t len1, size_t len2);
void type_of_call StWarningOpt_(const char* mess, const char* opt,
                                  size_t len1, size_t len2);
void type_of_call StErrorOpt_(const char* mess, const char* opt,
                                  size_t len1, size_t len2);
void type_of_call StDebugOpt_(const char* mess, const char* opt,
                                  size_t len1, size_t len2);
void type_of_call QAInfoOpt_(const char* mess, const char* opt,
                                  size_t len1, size_t len2);
void type_of_call StMessAddType_(const char* type, const char* text,
                                  size_t len1, size_t len2);
}
#endif


#include "StMessage.h"
#include "StMessTypeList.h"
#include "StMessageCounter.h"

#ifndef ClassMessVec
#define ClassMessVec
typedef StVector(StMessage*) messVec;
typedef StVector(StMessage*)::iterator messVecIter;
typedef StVector(messVec*) messTypeVec;
#endif

#include "StMessMgr.h"

class StMessageManager : public StMessMgr {

 private:
   static StMessMgr* mInstance;       //!
   StMessTypeList* messTypeList;      //!
   StMessageCounter* messCounter;     //!
   char* curType;                     //!
   char* curOpt;                      //!
   int building;

 protected:
   StMessageManager();
   StMessageManager(const StMessageManager&);
   messVec messList;
   messTypeVec messCollection;
   virtual messVecIter FindMessageIter(const char* s1, const char* s2="",
         const char* s3="", const char* s4="", messVec* list=0);
   virtual        void BuildMessage(const char* mess="", const char* type="",
         const char* opt=0);
 
 public:
   virtual ~StMessageManager();
   static StMessMgr* Instance();      //!

// Generic Messages:
   virtual StMessMgr& Message(const char* mess="", const char* type="",
         const char* opt=0);
   virtual       void Print();
   virtual        int PrintList(messVec* list);
   virtual        int PrintAll() {return PrintList(&messList); }
   virtual const messVec* GetAll() {return (&messList);}
   virtual StMessage* FindMessage(const char* s1, const char* s2="",
         const char* s3="", const char* s4="", messVec* list=0);
   virtual   messVec* FindMessageList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="", messVec* list=0);
   virtual        int RemoveMessage(StMessage* mess);
   virtual        int RemoveMessage(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
         {return RemoveMessage(FindMessage(s1,s2,s3,s4));}
   virtual       void SetLimit(const char* str, int n=0)
         {messCounter->SetLimit(str,n);}
   virtual        int GetLimit(const char* str)
         {return messCounter->GetLimit(str);}
   virtual       void ListLimits() {messCounter->ListLimits();}
   virtual       void RemoveLimit(const char* str) {SetLimit(str,-1);}
   virtual       void SwitchOff(const char* str) {SetLimit(str,0);}
   virtual       void SwitchOn(const char* str) {RemoveLimit(str);}
   virtual       void NoLimits() {messCounter->NoLimits();}
   virtual       void Summary(size_t nTerms=1);
   virtual        int AddType(const char* type, const char* text);
   virtual        int ListTypes() {return messTypeList->ListTypes();}

// Info Messages:
   virtual StMessMgr& Info(const char* mess="", const char* opt="O")
         { return Message(mess, "I", opt);}
   virtual        int PrintInfos() {return PrintList(messCollection[1]); }
   virtual const messVec* GetInfos() {return (messCollection[1]);}
   virtual StMessage* FindInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[1]);}
   virtual messVec* FindInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[1]);}

// Warning Messages:
   virtual StMessMgr& Warning(const char* mess="", const char* opt="E")
         { return Message(mess, "W", opt);}
   virtual        int PrintWarnings() {return PrintList(messCollection[2]); }
   virtual const messVec* GetWarnings() {return (messCollection[2]);}
   virtual StMessage* FindWarning(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[2]);}
   virtual messVec* FindWarningList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[2]);}

// Error Messages:
   virtual StMessMgr& Error(const char* mess="", const char* opt="E")
         { return Message(mess, "E", opt);}
   virtual        int PrintErrors() {return PrintList(messCollection[3]); }
   virtual const messVec* GetErrors() {return (messCollection[3]);}
   virtual StMessage* FindError(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[3]);}
   virtual messVec* FindErrorList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[3]);}

// Debug Messages:
   virtual StMessMgr& Debug(const char* mess="", const char* opt="O")
         { return Message(mess, "D", opt);}
   virtual        int PrintDebug() {return PrintList(messCollection[4]); }
   virtual const messVec* GetDebugs() {return (messCollection[4]);}
   virtual StMessage* FindDebug(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[4]);}
   virtual messVec* FindDebugList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[4]);}

// QAInfo Messages:
   virtual StMessMgr& QAInfo(const char* mess="", const char* opt="OTS")
         { return Message(mess, "Q", opt);}
   virtual        int PrintQAInfo() {return PrintList(messCollection[5]); }
   virtual const messVec* GetQAInfos() {return (messCollection[5]);}
   virtual StMessage* FindQAInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[5]);}
   virtual messVec* FindQAInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[5]);}

   virtual       void PrintInfo();
#ifdef __ROOT__
   ClassDef(StMessageManager,0)
#endif
};


#endif
