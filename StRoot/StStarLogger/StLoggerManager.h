/*!
  \class StLoggerManager
  \author G. Van Buren, BNL

  This class manages the messages in STAR software. It is a singleton.
  It inherits from StMessMgr, which provides the external interface.
  Messages are stored in a vector, and come in several types
  (i.e. info, error, debug ). The types "I" (info), "W" (warning),
  "E" (error), "D" (debug), "Q" (QAInfo), "U" (UCMInfo) are predefined.
  Message finding and summary tools are also available.
  \sa http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/StUtilities/doc/StMessMgr.html

*/

#ifndef ClassStLoggerManager
#define ClassStLoggerManager
#include "StMessMgr.h"

#ifndef __CINT__
# include <log4cxx/logger.h>
# include <vector>
# include <cassert>
# include "StarOptionFilter.h"
#endif


//#include "StMessage.h"
//#include "StMessTypeList.h"
//#include "StMessageCounter.h"

class StMessTypeList;
class StMessageCounter;
class StMessTypeList;
//class StarOptionFilter;

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
   static log4cxx::LoggerPtr &fgUCMLogger;    //!  Logger to server UCM stream
#endif

   static StMessMgr* mInstance;       //!
   static const char *fgLevels;       //!
   // StMessTypeList* messTypeList;   //!
   // StMessageCounter* messCounter;  //!
   unsigned char  fCurType;           //!
   char* fCurOpt;                     //!
#ifndef __CINT__
   std::vector<std::string>  fSourceFileNames;
   std::string fLastMessage;
   ostrstream fStreams[7];
#endif
   int   fLineNumbers[10];
   int   fAllowRepeat;        // the total number one and the same message can be printed out
   int   fLastRepeatCounter;
#ifndef __CINT__
   log4cxx::varia::StarOptionFilterPtr  fStarOptionFilter;
   log4cxx::LevelPtr fDefaultLevel;
#endif
   // int building;
   // int remember;


 protected:
   // messVec     messList;
   // messTypeVec messCollection;
    static inline int LevelIndex(char level);
    static bool   mColorEnabled;
 protected:
   StLoggerManager(const char *loggerName="BFC");
   StLoggerManager(const StLoggerManager&);
   virtual        void BuildMessage(const char* mess="", unsigned char type=0,
         const char* opt=0,const char *sourceFileName=0, int lineNumber=-1);
#ifndef __CINT__
  void SetStarOptionFilter(const log4cxx::varia::StarOptionFilterPtr& filter);
  const log4cxx::varia::StarOptionFilterPtr& GetStarOptionFilter() const;
  log4cxx::varia::StarOptionFilterPtr& GetStarOptionFilter();
  ostrstream &Stream();
#endif

protected:
   friend class StChain;
   virtual void IgnoreRepeats();
   virtual void AllowRepeats();
   virtual void AllowRepeats(int nRepeats);

   virtual        int PrintList(messVec* list);
   virtual const messVec* GetAll();
   static  void       DestroyInstance();                       //!

 public:
   virtual ~StLoggerManager();
   virtual ostream& OperatorShift(ostream& os, StMessage* stm);
   static  StMessMgr* StarLoggerInit();                       //!
   static  StMessMgr* StarLoggerInit(const char *loggerName); //!   
   virtual StMessMgr* Instantiate();      //!
   virtual StMessMgr* Instantiate(const char *loggerName);
   
   virtual bool isDebugEnabled()  const;
   virtual bool isWarnEnabled()   const;
   virtual bool isErrorEnabled()  const;
   virtual bool isInfoEnabled()   const;
   virtual bool isFatalEnabled()  const;
   virtual bool isEnabledFor()    const;
   virtual bool isQAInfoEnabled() const;
   virtual bool isUCMInfoEnabled()const;
   static  bool isColorEnabled()  {return mColorEnabled;}
   static  void setColorEnabled(bool t = true) {mColorEnabled = t;}

// Generic Messages:
   virtual ostrstream& Message(const char* mess="", const char* type="",
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
   virtual       void SetLevel(Int_t );
   virtual      Int_t GetLevel(Int_t ) const;
   virtual const char *GetName() const { return 0;}
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
   virtual       void Close(); // Close the messenger streams
//   virtual const char *GetName();

// Info Messages:
   virtual ostrstream& Info(const char* mess="", const char* opt="O"
                          ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintInfos();
   virtual const messVec* GetInfos();
   virtual StMessage* FindInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// Warning Messages:
   virtual ostrstream& Warning(const char* mess="", const char* opt="E"
                             ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintWarnings();
   virtual const messVec* GetWarnings();
   virtual StMessage* FindWarning(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindWarningList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// Error Messages:
   virtual ostrstream& Error(const char* mess="", const char* opt="E"
                           ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintErrors();
   virtual const messVec* GetErrors();
   virtual StMessage* FindError(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindErrorList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// Debug Messages:
   virtual ostrstream& Debug(const char* mess="", const char* opt="OT"
                           ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintDebug();
   virtual const messVec* GetDebugs();
   virtual StMessage* FindDebug(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindDebugList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// QAInfo Messages:
   virtual ostrstream& QAInfo(const char* mess="", const char* opt="OS"
                            ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintQAInfo();
   virtual const messVec* GetQAInfos();
   virtual StMessage* FindQAInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindQAInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");

// UCMInfo Messages:
   virtual ostrstream& UCMInfo(const char* mess="", const char* opt="OS"
                            ,const char *sourceFileName=0, int lineNumber=-1);
   virtual        int PrintUCMInfo();
   virtual const messVec* GetUCMInfos();
   virtual StMessage* FindUCMInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   virtual messVec* FindUCMInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="");
   
// "As is" Messages:
   virtual ostrstream& out(const char* mess="");
   virtual ostrstream& err(const char* mess="");

   virtual       void PrintInfo();

   // Fatal Messages:
   virtual ostrstream& Fatal(const char* mess="", const char* opt="F",const char *sourceFileName=0, int lineNumber=-1);

   //  "Extra Logger" methods
   void PrintLogger(const char* mess, unsigned char type, const char* opt, const char *sourceFileName=0, int lineNumber=-1);

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
#ifndef __CINT__
//_____________________________________________________________________________
inline void StLoggerManager::SetStarOptionFilter(const log4cxx::varia::StarOptionFilterPtr& filter)
{  fStarOptionFilter = filter;  }
//_____________________________________________________________________________
inline const log4cxx::varia::StarOptionFilterPtr&  StLoggerManager::GetStarOptionFilter() const
{ return fStarOptionFilter;} 

//_____________________________________________________________________________
inline log4cxx::varia::StarOptionFilterPtr&  StLoggerManager::GetStarOptionFilter()
{ return fStarOptionFilter;} 

#endif
#endif

// $Id: StLoggerManager.h,v 1.17 2012/10/04 23:32:29 fisyak Exp $
