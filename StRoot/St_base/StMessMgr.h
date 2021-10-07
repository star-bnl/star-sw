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
#ifdef LOGGERMESSAGE
#error An attempt to redefine the LOGGERMESSAGE macro
#else
#  define LOGGERMESSAGE(MESSAGELEVEL)                                    \
   if (StMessMgr::CurrentMessager()->_NAME3_(is,MESSAGELEVEL,Enabled)()) \
   StMessMgr::CurrentMessager()->MESSAGELEVEL("","O",__FUNCTION__, __LINE__) 

#  define LOG_INFO  LOGGERMESSAGE(Info)
#  define LOG_WARN  LOGGERMESSAGE(Warning)
#  define LOG_ERROR LOGGERMESSAGE(Error)
#  define LOG_FATAL LOGGERMESSAGE(Fatal)
#  define LOG_DEBUG LOGGERMESSAGE(Debug)
#  define LOG_QA    LOGGERMESSAGE(QAInfo)
#  define LOG_UCM   LOGGERMESSAGE(UCMInfo)

#define STAR_INFO(name) \
   GetLogger(_QUITE_(name))->MESSAGELEVEL(__FUNCTION__, __LINE__) 

#define MSG_INFO(name) \
   GetLogger(_QUITE_(name))->MESSAGELEVEL(__FUNCTION__, __LINE__) 
#endif
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
#define UCMInfo_ F77_NAME(ucminfo,UCMINFO)
#define StInfoOpt_ F77_NAME(stinfoopt,STINFOOPT)
#define StWarningOpt_ F77_NAME(stwarningopt,STWARNINGOPT)
#define StErrorOpt_ F77_NAME(sterroropt,STERROROPT)
#define StDebugOpt_ F77_NAME(stdebugopt,STDEBUGOPT)
#define QAInfoOpt_ F77_NAME(qainfoopt,QAINFOOPT)
#define UCMInfoOpt_ F77_NAME(ucminfoopt,UCMINFOOPT)
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
void type_of_call UCMInfo_(const char* mess, size_t len);
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
void type_of_call UCMInfoOpt_(const char* mess, const char* opt,
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
   // fake operator= to follish the bug in the RootCint
  StMessMgr& operator=(const StMessMgr&){ return *this;}
 public: 
    enum ESTARMakerErrorLevels {
       kAll=-5, kFatal, kError, kWarning, kInfo, kDefault, kDebug, kDebug2 
    }; 
   StMessMgr();
   StMessMgr(const StMessMgr&){;}
   virtual ~StMessMgr(){;}
// operator <<

   virtual std::ostream& OperatorShift(std::ostream& os, StMessage* stm) = 0;

// Generic Messages:
   virtual ostrstream& Message(const char* mess="", const char* type="",
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
   virtual       void SetLevel(Int_t logLevel)       =0;
   virtual      Int_t GetLevel(Int_t logLevel) const =0;
   virtual const char *GetName() const = 0;
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
   virtual       void Close() {} // Close the messenger streams
//Optimization
       /*
        *  <p>If you are worried about speed, then you should write
        *  <pre>
        *        if(logger->isDebugEnabled()) {
        *          logger->debug("debug message");
        *        }
        *  </pre>
        *
        *  <p>This way you will not incur the cost of parameter
        *  construction if debugging is disabled for <code>logger</code>. On
        *  the other hand, if the <code>logger
              </code> is debug enabled, you
        *  will incur the cost of evaluating whether the logger is debug
        *  enabled twice. Once in <code>isDebugEnabled</code> and once in
        *  the <code>debug</code>.  This is an insignificant overhead
        *  since evaluating a logger takes about 1%% of the time it
        *  takes to actually log.
        *
        *  @return bool - <code>true</code> if this logger is debug
        *  enabled, <code>false</code> otherwise.
        *   */
  virtual bool isDebugEnabled()  const;
  virtual bool isWarnEnabled()   const;
  virtual bool isWarningEnabled() const;
  virtual bool isErrorEnabled()  const;
  virtual bool isInfoEnabled()   const;
  virtual bool isFatalEnabled()  const;
  virtual bool isEnabledFor()    const;
  virtual bool isQAInfoEnabled() const;
  virtual bool isUCMInfoEnabled()const;

  
   
//  Manager factory
public:
   static StMessMgr*  CurrentMessager();
   static StMessMgr*  Instance();
   static StMessMgr*  Instance(const char *);
   static StMessMgr*  SetCurrentMessager(StMessMgr *mgr=0);
protected:
   virtual StMessMgr*  Instantiate();
   virtual StMessMgr*  Instantiate(const char *);
         
public:
// Info Messages:
   virtual ostrstream& Info(const char* mess="", const char* opt="O",const char *sourceFileName=0, int lineNumber=-1)=0;
   virtual        int PrintInfos() =0;
   virtual const messVec* GetInfos() =0;
   virtual StMessage* FindInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Warning Messages:
   virtual ostrstream& Warning(const char* mess="", const char* opt="E",const char *sourceFileName=0, int lineNumber=-1)= 0;
   virtual        int PrintWarnings() =0;
   virtual const messVec* GetWarnings() =0;
   virtual StMessage* FindWarning(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindWarningList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Error Messages:
   virtual ostrstream& Error(const char* mess="", const char* opt="E",const char *sourceFileName=0, int lineNumber=-1) = 0;
   virtual        int PrintErrors() =0;
   virtual const messVec* GetErrors() =0;
   virtual StMessage* FindError(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindErrorList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Debug Messages:
   virtual ostrstream& Debug(const char* mess="", const char* opt="OT",const char *sourceFileName=0, int lineNumber=-1)= 0;
   virtual        int PrintDebug() =0;
   virtual const messVec* GetDebugs() =0;
   virtual StMessage* FindDebug(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindDebugList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// QAInfo Messages:
   virtual ostrstream& QAInfo(const char* mess="", const char* opt="OS",const char *sourceFileName=0, int lineNumber=-1) = 0;
   virtual        int PrintQAInfo() =0;
   virtual const messVec* GetQAInfos() =0;
   virtual StMessage* FindQAInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindQAInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// UCMInfo Messages:
   virtual ostrstream& UCMInfo(const char* mess="", const char* opt="OS",const char *sourceFileName=0, int lineNumber=-1) = 0;
   virtual        int PrintUCMInfo() =0;
   virtual const messVec* GetUCMInfos() =0;
   virtual StMessage* FindUCMInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindUCMInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// "As is" Messages:
   virtual ostrstream& out(const char* mess="") = 0;
   virtual ostrstream& err(const char* mess="") = 0;

   virtual       void PrintInfo() =0;
   // Fatal Messages:
   virtual ostrstream& Fatal(const char* mess="", const char* opt="OT",const char *sourceFileName=0, int lineNumber=-1)= 0;

#ifdef __ROOT__
   ClassDef(StMessMgr,0)
#endif
};

//______________________________________________________________________________
//
//  StTurnLogger - an aux class to simply "save/restore the "current" logger
//______________________________________________________________________________
class StTurnLogger
{
   private:
     StMessMgr* fMessager; // hold the messager to restore it at dtor
   public:
      StTurnLogger(StMessMgr* msg=0);
      StTurnLogger(const StTurnLogger& push);
     ~StTurnLogger();
};
   
// Global pointers:
R__EXTERN StMessMgr* gMessMgr;
R__EXTERN StMessage* gMessage;
R__EXTERN StMessage* endm;
// R__EXTERN StMessMgr& gMess;

//______________________________________________________________________________
inline StTurnLogger::StTurnLogger(StMessMgr* msg): fMessager(0) 
{
  if (msg) fMessager = StMessMgr::SetCurrentMessager(msg);
}

//______________________________________________________________________________
inline StTurnLogger::StTurnLogger(const StTurnLogger& push) : fMessager(push.fMessager) 
{ ((StTurnLogger*)&push)->fMessager=0; }

//______________________________________________________________________________
inline StTurnLogger::~StTurnLogger()
{if (fMessager) StMessMgr::SetCurrentMessager(fMessager);} 

     
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

// $Id: StMessMgr.h,v 1.13 2009/06/22 22:36:02 fine Exp $
