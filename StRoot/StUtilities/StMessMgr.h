// $Id: StMessMgr.h,v 1.5 1999/08/10 22:07:35 genevb Exp $
// $Log: StMessMgr.h,v $
// Revision 1.5  1999/08/10 22:07:35  genevb
// Added QAInfo message types
//
// Revision 1.4  1999/07/23 16:56:39  genevb
// Fix extern C prototypes, default options for omitted types, Linux bug with multi-line messages
//
// Revision 1.3  1999/07/17 00:23:22  genevb
// Fixed bug when option fields are empty in FORTRAN, and let type limits be set before types are even added
//
// Revision 1.2  1999/07/15 05:15:02  genevb
// Fixed an odd bug with seekp(0) on an empty stream buffer
//
// Revision 1.1  1999/07/08 22:59:33  genevb
// Created an abstract interface with StMessMgr.h hiding template implementation from others
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessMgr                                                            //
//                                                                      //
// This class provides the interface for STAR offline messaging.        //
// It is an abstract class which is inherited by the StMessageManager   //
// class. StMessageManager implements the functionality for managing    //
// messages, which are instances of the class StMessage. For more       //
// details refer to the StMessageManager class.                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStMessMgr
#define ClassStMessMgr

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
#define QAInfo_ F77_NAME(qainfo,QAINFO)
#define StMessAddType_ F77_NAME(stmessaddtype,STMESSADDTYPE)
extern "C" {
void type_of_call Message_(char* mess="", int *lines=0, int *id=0,
                                  size_t len=0);
void type_of_call Msg_Enable_(char* mess="",
                                  size_t len=0);
 int type_of_call Msg_Enabled_(char* mess="", int *id=0,
                                  size_t len=0);
void type_of_call Msg_Disable_(char* mess="",
                                  size_t len=0);
void type_of_call StMessage_(char* mess="", char* type="", char* opt=0,
                                  size_t len1=0, size_t len2=0, size_t len3=0);
void type_of_call StInfo_(char* mess="", char* opt="O",
                                  size_t len1=0, size_t len2=1);
void type_of_call StWarning_(char* mess="", char* opt="E",
                                  size_t len1=0, size_t len2=1);
void type_of_call StError_(char* mess="", char* opt="E",
                                  size_t len1=0, size_t len2=1);
void type_of_call StDebug_(char* mess="", char* opt="O",
                                  size_t len1=0, size_t len2=1);
void type_of_call QAInfo_(char* mess="", char* opt="OTS",
                                  size_t len1=0, size_t len2=3);
void type_of_call StMessAddType_(const char* type, const char* text,
                                  size_t len1=0, size_t len2=0);
void type_of_call MessageOut(const char* msg);
}
#endif

class StMessage;

#ifndef ClassMessVec
class messVec;
#endif

#include <strstream.h>


class StMessMgr : public ostrstream {
   friend ostream& operator<<(ostream& ,StMessage*);

 private:

 protected:
 
 public:
   StMessMgr() : ostrstream(new char[1024],1024,ios::out) {}
   StMessMgr(const StMessMgr&);
   virtual ~StMessMgr() =0;

// Generic Messages:
   virtual StMessMgr& Message(char* mess="", char* type="", char* opt=0) =0;
   virtual       void Print() =0;
   virtual        int PrintList(messVec* list) =0;
   virtual        int PrintAll() =0;
   virtual StMessage* FindMessage(const char* s1, char* s2="",
         char* s3="", char* s4="", messVec* list=0) =0;
   virtual   messVec* FindMessageList(const char* s1, char* s2="",
         char* s3="", char* s4="", messVec* list=0) =0;
   virtual        int RemoveMessage(StMessage* mess) =0;
   virtual        int RemoveMessage(const char* s1, char* s2="",
         char* s3="", char* s4="") =0;
   virtual       void SetLimit(char* str, int n=0) =0;
   virtual        int GetLimit(char* str) =0;
   virtual       void ListLimits() =0;
   virtual       void RemoveLimit(char* str) =0;
   virtual       void SwitchOff(char* str) =0;
   virtual       void SwitchOn(char* str) =0;
   virtual       void Summary(size_t nTerms=1) =0;
   virtual        int AddType(const char* type, const char* text) =0;
   virtual        int ListTypes() =0;

// Info Messages:
   virtual StMessMgr& Info(char* mess="", char* opt="O") =0;
   virtual        int PrintInfos() =0;
   virtual const messVec* GetInfos() =0;
   virtual StMessage* FindInfo(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;
   virtual messVec* FindInfoList(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;

// Warning Messages:
   virtual StMessMgr& Warning(char* mess="", char* opt="E") =0;
   virtual        int PrintWarnings() =0;
   virtual const messVec* GetWarnings() =0;
   virtual StMessage* FindWarning(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;
   virtual messVec* FindWarningList(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;

// Error Messages:
   virtual StMessMgr& Error(char* mess="", char* opt="E") =0;
   virtual        int PrintErrors() =0;
   virtual const messVec* GetErrors() =0;
   virtual StMessage* FindError(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;
   virtual messVec* FindErrorList(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;

// Debug Messages:
   virtual StMessMgr& Debug(char* mess="", char* opt="O") =0;
   virtual        int PrintDebug() =0;
   virtual const messVec* GetDebugs() =0;
   virtual StMessage* FindDebug(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;
   virtual messVec* FindDebugList(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;

// QAInfo Messages:
   virtual StMessMgr& QAInfo(char* mess="", char* opt="OTS") =0;
   virtual        int PrintQAInfo() =0;
   virtual const messVec* GetQAInfos() =0;
   virtual StMessage* FindQAInfo(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;
   virtual messVec* FindQAInfoList(const char* s1, char* s2="", char* s3="",
         char* s4="") =0;

   virtual       void PrintInfo() =0;
#ifdef __ROOT__
   ClassDef(StMessMgr,0)
#endif
};

// Global pointers:
R__EXTERN StMessMgr* gMessMgr;
R__EXTERN StMessage* gMessage;
R__EXTERN StMessage* endm;


#endif
