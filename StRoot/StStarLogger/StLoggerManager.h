/*!
  \class StLoggerManager
  \author G. Van Buren, BNL

  This class manages the messages in STAR software. It is a singleton.
  It inherits from StMessMgr, which provides the external interface.
  Messages are stored in a vector, and come in several types
  (i.e. info, error, debug ). The types "I" (info), "W" (warning),
  "E" (error), "D" (debug), and "Q" (QAInfo) are predefined.
  Message finding and summary tools are also available.
  \sa http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/StUtilities/doc/StMessMgr.html

*/

#ifndef ClassStLoggerManager
#define ClassStLoggerManager

#ifndef __CINT__
# include <log4cxx/logger.h>
# include <vector>
#endif

#include "StMessMgr.h"

//#include "StMessage.h"
//#include "StMessTypeList.h"
//#include "StMessageCounter.h"

class StMessTypeList;
class StMessageCounter;
class StMessTypeList;

#ifndef ClassMessVec
#define ClassMessVec
  //typedef StVector(StMessage*) messVec;
  //typedef StVector(StMessage*)::iterator messVecIter;
  //typedef StVector(messVec*) messTypeVec;
#endif

class StLoggerManager : public StMessMgr {

 private:
#ifndef __CINT__
   log4cxx::LoggerPtr fLogger;        //!  Logger to server the old STAR MessageManager interface
   static log4cxx::LoggerPtr fgQALogger;      //!  Logger to server QA stream
#endif

   static StMessMgr* mInstance;       //!
   static const char *fgLevels;       //!
   // StMessTypeList* messTypeList;      //!
   // StMessageCounter* messCounter;     //!
   char* fCurType;                     //!
   char* fCurOpt;                      //!
#ifndef __CINT__
   std::vector<std::string>  fSourceFileNames;
   std::string fLastMessage;
#endif
   int   fLineNumbers[10];
   int   fAllowRepeat;        // the total number one and the same message can be printed out
   int   fLastRepeatCounter;
   // int building;
   // int remember;


 protected:
   // messVec     messList;
   // messTypeVec messCollection;
    static inline int LevelIndex(char level);

 protected:
   StLoggerManager(const char *loggerName="BFC");
   StLoggerManager(const StLoggerManager&);
   virtual        void BuildMessage(const char* mess="", const char* type="",
         const char* opt=0,const char *sourceFileName=0, int lineNumber=-1);

protected:
   virtual void IgnoreRepeats();
   virtual void AllowRepeats();
   virtual void AllowRepeats(int nRepeats);

   virtual        int PrintList(messVec* list);
   virtual const messVec* GetAll();

 public:
   virtual ~StLoggerManager();
   virtual std::ostream& OperatorShift(std::ostream& os, StMessage* stm);
   static StMessMgr* Instance();      //!
   static StMessMgr* Instance(const char *loggerName);


// Generic Messages:
   virtual StMessMgr& Message(const char* mess="", const char* type="",
         const char* opt=0,const char *sourceFileName=0, int lineNumber=-1);
   virtual       void Print();
//    virtual        int PrintList(messVec* list);
   virtual        int PrintAll();
  // virtual const messVec* GetAll();
   virtual StMessage* FindMessage(const char* s1, const char* s2="",
         const char* s3="", const char* s4="", messVec* list=0);
   virtual   messVec* FindMessageList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="", messVec* list=0);
   virtual        int RemoveMessage(StMessage* mess);
   virtual        int RemoveMessage(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual       void SetLimit(const char* str, int n=0);         
   virtual        int GetLimit(const char* str);         
   virtual       void ListLimits();
   virtual       void RemoveLimit(const char* str);
   virtual       void SwitchOff(const char* str);
   virtual       void SwitchOn(const char* str);
   virtual       void FixOn(const char* str);
   virtual       void NoLimits();
   virtual       void Summary(size_t nTerms=1);
   virtual       void MemorySummary();
   virtual       void MemoryOn();
   virtual       void MemoryOff();
   virtual        int AddType(const char* type, const char* text);
   virtual        int ListTypes();

// Info Messages:
   virtual StMessMgr& Info(const char* mess="", const char* opt="O"
                          ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintInfos();
   virtual const messVec* GetInfos();
   virtual StMessage* FindInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// Warning Messages:
   virtual StMessMgr& Warning(const char* mess="", const char* opt="E"
                             ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintWarnings();
   virtual const messVec* GetWarnings();
   virtual StMessage* FindWarning(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindWarningList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// Error Messages:
   virtual StMessMgr& Error(const char* mess="", const char* opt="E"
                           ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintErrors();
   virtual const messVec* GetErrors();
   virtual StMessage* FindError(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindErrorList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// Debug Messages:
   virtual StMessMgr& Debug(const char* mess="", const char* opt="OT"
                           ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintDebug();
   virtual const messVec* GetDebugs();
   virtual StMessage* FindDebug(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindDebugList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// QAInfo Messages:
   virtual StMessMgr& QAInfo(const char* mess="", const char* opt="OS"
                            ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintQAInfo();
   virtual const messVec* GetQAInfos();
   virtual StMessage* FindQAInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindQAInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// "As is" Messages:
   virtual StMessMgr& out(const char* mess="");
   virtual StMessMgr& err(const char* mess="");

   virtual       void PrintInfo();

   // Fatal Messages:
   virtual StMessMgr& Fatal(const char* mess="", const char* opt="F",const char *sourceFileName=0, int lineNumber=-1);

   //  "Extra Logger" methods
   void PrintLogger(const char* mess, const char* type, const char* opt, const char *sourceFileName=0, int lineNumber=-1);

#ifdef __ROOT__
   ClassDef(StLoggerManager,0)
#endif
};

//_____________________________________________________________________________
inline int StLoggerManager::LevelIndex(char level)
{ 
   const char *thisLevel = strchr(StLoggerManager::fgLevels,level);
   assert(thisLevel);
   return thisLevel-StLoggerManager::fgLevels;
}
#endif

// $Id: StLoggerManager.h,v 1.1 2004/05/11 20:58:48 fine Exp $
