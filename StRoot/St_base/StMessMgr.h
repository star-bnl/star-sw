/*!
  \class StMessMgr
  \author G. Van Buren, BNL

  This class provides the interface for STAR offline messaging.
  It is an abstract class which is inherited by the StMessageManager
  class. StMessageManager implements the functionality for managing
  messages, which are instances of the class StMessage. For more
  details refer to the StMessageManager class.
  \sa http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/StUtilities/doc/StMessMgr.html

*/

#ifndef ClassStMessMgr
#define ClassStMessMgr

#ifdef __ROOT__
#include "Rtypes.h"
#endif


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
void type_of_call MessageOut(const char* msg);
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



class StMessage;

#ifndef ClassMessVec
class messVec;
#endif

#include <Stsstream.h>
#include <Stiostream.h>

class StMessMgr : public ostrstream {
   friend ostream& operator<<(ostream& ,StMessage*);
   friend ostream& operator++(StMessMgr&);
   friend ostream& operator-(StMessMgr&);
   friend ostream& operator--(StMessMgr&);
   friend ostream& operator~(StMessMgr&);
   friend class StBFChain;

 private:

 protected:
   virtual void IgnoreRepeats() =0;
   virtual void AllowRepeats() =0;
 
 public: 
   StMessMgr();
   StMessMgr(const StMessMgr&){;}
   virtual ~StMessMgr(){;}
// operator <<

   virtual std::ostream& OperatorShift(std::ostream& os, StMessage* stm) = 0;

// Generic Messages:
   virtual StMessMgr& Message(const char* mess="", const char* type="",
         const char* opt=0,const char *sourceFileName=0, int lineNumber=-1)= 0;
   virtual       void Print() =0;
   virtual        int PrintList(messVec* list) =0;
   virtual        int PrintAll() =0;
   virtual StMessage* FindMessage(const char* s1, const char s2[]="",
         const char* s3="", const char* s4="", messVec* list=0) =0;
   virtual   messVec* FindMessageList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="", messVec* list=0) =0;
   virtual        int RemoveMessage(StMessage* mess) =0;
   virtual        int RemoveMessage(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual       void SetLimit(const char* str, int n=0) =0;
   virtual        int GetLimit(const char* str) =0;
   virtual       void ListLimits() =0;
   virtual       void RemoveLimit(const char* str) =0;
   virtual       void SwitchOff(const char* str) =0;
   virtual       void SwitchOn(const char* str) =0;
   virtual       void FixOn(const char* str) =0;
   virtual       void NoLimits() =0;
   virtual       void Summary(size_t nTerms=1) =0;
   virtual       void MemorySummary() =0;
   virtual       void MemoryOn() =0;
   virtual       void MemoryOff() =0;
   virtual        int AddType(const char* type, const char* text) =0;
   virtual        int ListTypes() =0;

// Info Messages:
   virtual StMessMgr& Info(const char* mess="", const char* opt="O",const char *sourceFileName=0, int lineNumber=-1)=0;
   virtual        int PrintInfos() =0;
   virtual const messVec* GetInfos() =0;
   virtual StMessage* FindInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Warning Messages:
   virtual StMessMgr& Warning(const char* mess="", const char* opt="E",const char *sourceFileName=0, int lineNumber=-1)= 0;
   virtual        int PrintWarnings() =0;
   virtual const messVec* GetWarnings() =0;
   virtual StMessage* FindWarning(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindWarningList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Error Messages:
   virtual StMessMgr& Error(const char* mess="", const char* opt="E",const char *sourceFileName=0, int lineNumber=-1) = 0;
   virtual        int PrintErrors() =0;
   virtual const messVec* GetErrors() =0;
   virtual StMessage* FindError(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindErrorList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Debug Messages:
   virtual StMessMgr& Debug(const char* mess="", const char* opt="OT",const char *sourceFileName=0, int lineNumber=-1)= 0;
   virtual        int PrintDebug() =0;
   virtual const messVec* GetDebugs() =0;
   virtual StMessage* FindDebug(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindDebugList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// QAInfo Messages:
   virtual StMessMgr& QAInfo(const char* mess="", const char* opt="OS",const char *sourceFileName=0, int lineNumber=-1) = 0;
   virtual        int PrintQAInfo() =0;
   virtual const messVec* GetQAInfos() =0;
   virtual StMessage* FindQAInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindQAInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// "As is" Messages:
   virtual StMessMgr& out(const char* mess="") = 0;
   virtual StMessMgr& err(const char* mess="") = 0;

   virtual       void PrintInfo() =0;
   // Fatal Messages:
   virtual StMessMgr& Fatal(const char* mess="", const char* opt="OT",const char *sourceFileName=0, int lineNumber=-1)= 0;

#ifdef __ROOT__
   ClassDef(StMessMgr,0)
#endif
};

// Global pointers:
R__EXTERN StMessMgr* gMessMgr;
R__EXTERN StMessage* gMessage;
R__EXTERN StMessage* endm;
// R__EXTERN StMessMgr& gMess;

//______________________________________________________________________________
inline ostream& operator<<(ostream& os, StMessage* stm) {
   return gMessMgr->OperatorShift(os,stm);
}

//______________________________________________________________________________
inline ostream& operator++(StMessMgr&) {
  return gMessMgr->Info();
}
//______________________________________________________________________________
inline ostream& operator--(StMessMgr&) {
  return gMessMgr->Error();
}
//______________________________________________________________________________
inline ostream& operator~(StMessMgr&) {
  return gMessMgr->out();
}
//______________________________________________________________________________
inline ostream& operator-(StMessMgr&) {
  return gMessMgr->err();
}




#endif

// $Id: StMessMgr.h,v 1.1 2004/04/15 16:01:59 fine Exp $
