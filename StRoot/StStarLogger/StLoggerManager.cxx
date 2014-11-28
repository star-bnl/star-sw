///////////////////////////////////////////////////////////////////////////
///                                                                      //
/// StLoggerManager                                                      //
///                                                                      //
/// This class manages the messages in STAR software. It is a singleton. //
/// It inherits from StMessMgr, which provides the external interface.   //
/// Messages are stored in a vector, and come in several types           //
/// (i.e. info, error, debug ). The types "I" (info), "W" (warning),     //
/// "E" (error), "D" (debug), and "Q" (QAInfo) are predefined. Message   //
/// finding and summary tools are also available.                        //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
#include <assert.h>
#include <stdlib.h>

#ifdef __ROOT__
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"
#include "TError.h"
#include "TEnv.h"
#include "TVirtualMutex.h"
#endif

#ifndef  _NO_IMPLEMENTATION_
#define  _NO_IMPLEMENTATION_   {       \
   const char *text = __FUNCTION__;    \
   NDC::push(_T("NO IMPLEMENTATION")); \
   PrintLogger(text,'D',"");           \
   NDC::pop();                         \
}
#endif
//           echo -e "\033[31m ME \033[0m COOL"
#define COLOR_NORMAL "\033[0m"
#define COLOR_RED    "\033[31m"
#define COLOR_GREEN  "\033[32m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_BLUE   "\033[34m"
#define COLOR_PINK   "\033[35m"

#include "StLoggerManager.h"
bool StLoggerManager::mColorEnabled = kFALSE;

#include <log4cxx/basicconfigurator.h>
#include <log4cxx/propertyconfigurator.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/ndc.h>
#include <log4cxx/consoleappender.h>
#include <log4cxx/patternlayout.h>
#include <log4cxx/layout.h>
#include <log4cxx/xml/domconfigurator.h>
//#include <log4cxx/varia/stringmatchfilter.h>
// #include <log4cxx/varia/denyallfilter.h>

#include "StStarLogger/StUCMAppender.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace log4cxx::varia;
using namespace log4cxx::db;
#if (STAR_LOG4CXX_VERSION == 10)
   using namespace log4cxx::filter;
#endif
using namespace std;
class  StMessage  {
  public: StMessage(){}
};

log4cxx::LoggerPtr StLoggerManager::fgQALogger;      //!  Logger to server QA stream
log4cxx::LoggerPtr &StLoggerManager::fgUCMLogger = *(new log4cxx::LoggerPtr());    //!  Logger to server UCM stream

const char *StLoggerManager::fgLevels = "FEWIDQU";
#ifdef __ROOT__
//    ROOT Error handling subrotuine
//______________________________________________________________________________
static void Log4cxx4RootErrorHandler(Int_t level, Bool_t abort, const char *location, const char *msg)
{
   // This is derived from  the ROOT "default error handler function" TError.cxx
   // The default error handler function. It prints the message on stderr and
   // if abort is set it aborts the application.


 //  The level is defined by the current logger level
   NDC::push("ROOT");

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,00,0)
if (gErrorIgnoreLevel == kUnset) {

       R__LOCKGUARD2(gErrorMutex);
#else
       {
#endif

      gErrorIgnoreLevel = 0;
      if (gEnv) {
         TString level = gEnv->GetValue("Root.ErrorIgnoreLevel", "Info");
         if (!level.CompareTo("Info",TString::kIgnoreCase))
            gErrorIgnoreLevel = kInfo;
         else if (!level.CompareTo("Warning",TString::kIgnoreCase))
            gErrorIgnoreLevel = kWarning;
         else if (!level.CompareTo("Error",TString::kIgnoreCase))
            gErrorIgnoreLevel = kError;
         else if (!level.CompareTo("Break",TString::kIgnoreCase))
            gErrorIgnoreLevel = kBreak;
         else if (!level.CompareTo("SysError",TString::kIgnoreCase))
            gErrorIgnoreLevel = kSysError;
         else if (!level.CompareTo("Fatal",TString::kIgnoreCase))
            gErrorIgnoreLevel = kFatal;
      }
   }

   if (level < gErrorIgnoreLevel)
      return;


   if (level >= kFatal) {
        LOG_FATAL << location << msg << endm;
     } else if (level >= kSysError) {
        LOG_FATAL << location << " : " << msg << endm;
     } else if (level >= kBreak) {
        LOG_FATAL << location << " : " << msg << endm;
     } else if (level >= kError) {
        LOG_ERROR << location << " : " << msg << endm;
//   else if (level >= kWarning)
//        LOG_WARN << location << " : " << msg << endm;
     } else if (level >= kInfo) {
        LOG_INFO << location << " : " << msg << endm;
     }

   if (abort) {
      if (gSystem) {
         gSystem->StackTrace();
         gSystem->Abort();
      } else
         ::abort();
   }
   NDC::pop();
}
#endif
//________________________________________
std::ostream& StLoggerManager::OperatorShift(std::ostream& os, StMessage* stm) {
// std::ostream& operator<<(std::ostream& os, StMessage* stm) {
  ostrstream &thisStream =  Stream();
  if ( (&thisStream == &os) && (stm == endm) ) {
    // There was a StMessage terminator
     os << ends;
     StMessMgr::CurrentMessager()->Print();
     os.seekp(0);os<< ends;os.seekp(0);
  } else {
     assert(0);
  }
  return os;
}

//static const char defaultMessType = 'I';
//static char emptyString[] = "";
//static char oOpt[] = "O";
//static char otsOpt[] = "OTS";
//static char eOpt[] = "E";
//static char nullMess[] = "Null message!!!";

StMessMgr* StLoggerManager::mInstance = 0;
//
// C++ routines:
//_____________________________________________________________________________
#ifdef __ROOT__
ClassImp(StLoggerManager)
#endif
//_____________________________________________________________________________
StLoggerManager::StLoggerManager(const char *loggerName)
                : StMessMgr(), fCurType(0),fAllowRepeat(-1),fLastRepeatCounter(0),fStarOptionFilter(0)
{
//
// Constructor - only called once when the library is loaded to
// instantiate the global message manager.
//
// Format
//   %c  - logger name %c{2} = b.c  ( a.b.c. )
//   %d  - date        %d{%H:%M:%S} or %d{%d %b %Y %H:%M:%S}.
//                      a - short week date name, A - full week date name
//                      b - short month name,     B - full month name  m - month number (1-12)
//                      d - day of the month
//                      H - hours (0-23)          I - hours ( 1 - 12)
//   %F  - the file name where the logging request was issued
//   %l  - location information of the caller which generated the logging event
//   %L  - the line number from where the logging request was issued
//   %m  - the application supplied message associated with the logging event
//   %n  - the platform dependent line separator character or characters
//   %p  - the level(priority) of the logging event
//   %r  - the number of milliseconds elapsed since the start
//   %x  - the NDC (nested diagnostic context)
//
//   This is essentially the TTCC layout.
//   %-6r [%15.15t] %-5p %30.30c %x - %m


  unsigned int i;
  for (i=0;i<strlen(fgLevels);i++){
     fSourceFileNames.push_back("");
     fLineNumbers[i] = -1;
  }
  fLogger   = Logger::getLogger(_T(loggerName));
  fDefaultLevel = fLogger->getLevel();
//  fCurType     = new char[ 2];  fCurType[0] = 0; fCurType[1] = 0;
  fCurOpt      = new char[32];  fCurOpt [0] = 0;
  SwitchOff("D");
  MemoryOn();
}

//_____________________________________________________________________________
StLoggerManager::~StLoggerManager() {
//
// Destructor - must delete the message listsy
//

  fCurType = 0;
  if (fCurOpt) delete [] fCurOpt;  fCurOpt = 0;

  if (StLoggerManager::Instance() == this) {
      // DestroyInstance();
      gMessMgr = 0;
   }

}
//_____________________________________________________________________________
StMessMgr* StLoggerManager::Instantiate()
{return StLoggerManager::StarLoggerInit(); }

//_____________________________________________________________________________
StMessMgr* StLoggerManager::Instantiate(const char *loggerName)
{return StLoggerManager::StarLoggerInit(loggerName); }

//_____________________________________________________________________________
StMessMgr* StLoggerManager::StarLoggerInit(const char *loggerName)
{
   return  new StLoggerManager(loggerName);
}
//_____________________________________________________________________________
StMessMgr* StLoggerManager::StarLoggerInit() {
//
//   Allows anyone to get a pointer to the single message manager:
//   StLoggerManager::Instance()->(member function)
//
  if (!mInstance) {
    // BasicConfigurator::configure();
    const char *proEnv = 0;
#ifndef __ROOT__
    String propertyFile = "log4j.xml";
#else
    TString Color = gEnv->GetValue("Logger.Color","no");
    mColorEnabled = Color.CompareTo("no",TString::kIgnoreCase);
    TString fullPropertyFileName = gEnv->GetValue("Logger.Configuration","log4j.xml");
    gSystem->ExpandPathName(fullPropertyFileName);
    String propertyFile = (const char*)fullPropertyFileName;
    proEnv = gSystem->Getenv("STAR_LOGGER_PROPERTY");
#endif
    StarOptionFilterPtr filter;
    if (proEnv && proEnv[0] ) propertyFile=proEnv;
    if (!gSystem->AccessPathName(propertyFile.c_str())) {
       xml::DOMConfigurator::configure(propertyFile);
//             PropertyConfigurator::configure(propertyFile);
    } else {
     	// BasicConfigurator::configure();
       LoggerPtr root = Logger::getRootLogger();
       // Add the STAR default appender
       ConsoleAppenderPtr appender = new ConsoleAppender(
		      new PatternLayout("%-3c{2}:%-5p - %m%n"));
//		      new PatternLayout(PatternLayout::TTCC_CONVERSION_PATTERN)));
       appender->setName(_T("defaultAppender"));
       root->addAppender(appender);
       //Set the default threashold to be
       root->setLevel(LOG4CXX_LEVEL_INFO);
    }
    LoggerPtr root = Logger::getRootLogger();

    fgQALogger  = Logger::getLogger("QA");
    fgUCMLogger = Logger::getLogger("UCM");
    //#if 1
    // Check the mandatory UCM appender
    TString ucmenv = gSystem->Getenv("LOGGING");
    if (ucmenv == "UCM" && gSystem->Getenv("JOBINDEX") && gSystem->Getenv("REQUESTID") ) {
       StUCMAppenderPtr appender(new StUCMAppender(ucmenv.Data()));
       appender->setLayout(new PatternLayout("%m"));
       appender->setName(_T("UCM"));
       fgUCMLogger->addAppender(appender);
       StringMatchFilterPtr filter(new StringMatchFilter());
       filter->setStringToMatch(_T("StageID="));
       filter->setAcceptOnMatch(true);
       appender->addFilter(filter);
       appender->addFilter( DenyAllFilterPtr(new DenyAllFilter));

       //Set the default threashold to be
       fgUCMLogger->setLevel(LOG4CXX_LEVEL_DEBUG);
    }
    // #endif
    //Almost all QA messages are on the info level
    NDC::push(_T(":"));

    mInstance = StarLoggerInit("BFC");
   ((StLoggerManager *)mInstance)->SetStarOptionFilter(filter);
    // if (gMessMgr) delete gMessMgr; gMessMgr = 0;
    gMessMgr  = mInstance;
    // Set the ROOT ErrorHanlding via logger as well
    // See: $ROOTSYS/include/TError.h
    SetErrorHandler(Log4cxx4RootErrorHandler);
//    Connect(gApplication,"Terminate(Int_t)",mInstance);
  }
  return mInstance;
}
//______________________________________________________________________________
ostrstream &StLoggerManager::Stream()
{
   // return the stream allocated for the particular "fCurType"
   // Create the new stream if there was none.
   // ( I am too lazy to intreodcue the enumare here :-(()
   switch (fCurType)  {
      case 'F': return fStreams[0];
      case 'E': return fStreams[1];
      case 'W': return fStreams[2];
      case 'I': return fStreams[3];
      case 'D': return fStreams[4];
      case 'Q': return fStreams[5];
      default:  return fStreams[6];
   };
}

//______________________________________________________________________________
bool  StLoggerManager::isDebugEnabled()  const{ return fLogger->isDebugEnabled(); }
//______________________________________________________________________________
bool  StLoggerManager::isWarnEnabled()   const{ return fLogger->isWarnEnabled(); }
//______________________________________________________________________________
bool  StLoggerManager::isErrorEnabled()  const{ return fLogger->isErrorEnabled(); }
//______________________________________________________________________________
bool  StLoggerManager::isInfoEnabled()   const{ return fLogger->isInfoEnabled(); }
//______________________________________________________________________________
bool  StLoggerManager::isFatalEnabled()  const{ return fLogger->isFatalEnabled(); }
//______________________________________________________________________________
bool  StLoggerManager::isEnabledFor()    const{ return true; /*fLogger->isEnabledFor();*/ }
//______________________________________________________________________________
bool  StLoggerManager::isQAInfoEnabled() const{ return fgQALogger? fgQALogger->isInfoEnabled():false; }
//______________________________________________________________________________
bool  StLoggerManager::isUCMInfoEnabled() const{ return fgUCMLogger? fgUCMLogger->isInfoEnabled():false; }
//_____________________________________________________________________________
ostrstream& StLoggerManager::Message(const char* mess, const char* type,
  const char* opt,const char *sourceFileName,int lineNumber) {
//
// Message declarator - creates a new message if mess is not empty,
// otherwise, prepares for << input.
//
//
// Instantiate an StMessage and place on the lists of known messages
// type =
//   'E' - error
//   'W' - warning
//   'I' - info
//   'D' - debug
//   'Q' - QA
//   'U' - UCM
//------------------------------
//
// opt =
//   'O' - stdout
//   'E' - stderr
//   'S' - skip "St"
//   'P' - plain output no decoration
//   'T' - print timestampt
//------------------------------
  unsigned char typeChar = 'I';
  if (type && type[0]) typeChar = type[0];
  if (!opt) opt = "";
  // size_t messSize = (mess && mess[0]) ? strlen(mess) : 0;
//  *fCurType = typeChar;
  fCurType = typeChar;
  strcpy(fCurOpt,opt);
//  if (tellp() > 0 ) *this << endm;  // print out the previous line if any

  if (sourceFileName && sourceFileName[0] ) fSourceFileNames[LevelIndex(fCurType)] = sourceFileName;
  else fSourceFileNames[LevelIndex(fCurType)].clear();
  fLineNumbers[LevelIndex(fCurType)]     = lineNumber;

  if (mess && std::char_traits<char>::length(mess) > 0) {
    BuildMessage(mess, typeChar, opt, sourceFileName, lineNumber);
  }

//  if (messSize > 0) *this << mess << endm; // print out the previous this message if present
//   return *((StMessMgr*) this);
  return Stream();
}
//_____________________________________________________________________________
void StLoggerManager::BuildMessage(const char* mess, unsigned char type,
  const char* opt,const char *sourceFileName, int lineNumber) {
//
// Instantiate an StMessage and place on the lists of known messages
// type =
//   'F' - fatal
//   'E' - error
//   'W' - warning
//   'I' - info
//   'D' - debug
//   'Q' - QA
//   'U' - UCM
//------------------------------
//
// opt =
//   'O' - stdout
//   'E' - stderr
//   'S' - skip "St"
//   'P' - plain output no decoartion
//   'T' - print timestampt
//------------------------------

     if (!opt) opt = "";
     PrintLogger(mess,type,opt,sourceFileName,lineNumber);
}
//_____________________________________________________________________________
void StLoggerManager::PrintLogger(const char* mess, unsigned char type,
    const char* opt,const char *sourceFileName, int lineNumber)
{
  if (!opt) opt = "";
  unsigned char typeChar = 'I';
  //  if (type && type[0]) typeChar = type[0];
  if (type) typeChar = type;
  if (!(sourceFileName || fSourceFileNames[LevelIndex(typeChar)].empty() ) )
    {
      sourceFileName = fSourceFileNames[LevelIndex(typeChar)].c_str();
    }

  if ( lineNumber == -1)
    lineNumber = fLineNumbers[LevelIndex(typeChar)];
  bool canPrint = true;
  if (fAllowRepeat >= 0 ) {
    // compare the current message with the buffered
    if ( mess == fLastMessage ) {
      fLastRepeatCounter++;
      canPrint = (fAllowRepeat >= fLastRepeatCounter);
    } else {
      fLastRepeatCounter= 0;
      fLastMessage = mess;
    }
  }
  if (canPrint) {
    if ( (mess == 0) || (mess[0] == 0)) mess = "."; // logger doesn't like the empty messages
    // #if __GNUC__ > 3
#if (STAR_LOG4CXX_VERSION >= 10)
    if (mColorEnabled) 
      {
	TString Mess(mess);
	switch (typeChar)  {
	case 'F': fLogger->fatal   (_T(COLOR_PINK   + Mess + COLOR_NORMAL),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'E': fLogger->error   (_T(COLOR_RED    + Mess + COLOR_NORMAL),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'W': fLogger->warn    (_T(COLOR_YELLOW + Mess + COLOR_NORMAL),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'I': fLogger->info    (_T(COLOR_GREEN  + Mess + COLOR_NORMAL),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'D': fLogger->debug   (_T(COLOR_BLUE   + Mess + COLOR_NORMAL),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'Q': fgQALogger->info (_T(COLOR_GREEN  + Mess + COLOR_NORMAL),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'U': fgUCMLogger->info(_T(COLOR_NORMAL + Mess + COLOR_NORMAL),LocationInfo(sourceFileName,"",lineNumber)); break;
	default: fLogger->info     (_T(COLOR_NORMAL + Mess + COLOR_NORMAL),LocationInfo(sourceFileName,"",lineNumber)); break;
	};
      } else 
#endif
      {
	switch (typeChar)  {
	case 'F': fLogger->fatal   (_T(mess),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'E': fLogger->error   (_T(mess),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'W': fLogger->warn    (_T(mess),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'I': fLogger->info    (_T(mess),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'D': fLogger->debug   (_T(mess),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'Q': fgQALogger->info (_T(mess),LocationInfo(sourceFileName,"",lineNumber)); break;
	case 'U': fgUCMLogger->info(_T(mess),LocationInfo(sourceFileName,"",lineNumber)); break;
	default: fLogger->info     (_T(mess),LocationInfo(sourceFileName,"",lineNumber)); break;
	};
      }
  }
  //   seekp(0);
  if (lineNumber != -1) { // restore the context
    NDC::pop();
  }
}
//_____________________________________________________________________________
void StLoggerManager::Print() {
//
// Empty the buffer into the current message and print it.
// If not currenty building a message, print the last one created.
//
  string message = Stream().str();
  BuildMessage(message.c_str(),fCurType,fCurOpt);
}
//_____________________________________________________________________________
int StLoggerManager::PrintList(messVec* list) {
//
// Print a list of messages from a messVec list.
//
 _NO_IMPLEMENTATION_;
 return -1;
}
//_____________________________________________________________________________
StMessage* StLoggerManager::FindMessage(const char* s1, const char* s2,
               const char* s3, const char* s4, messVec* list) {
 _NO_IMPLEMENTATION_;
 return 0;
}
//_____________________________________________________________________________
messVec* StLoggerManager::FindMessageList(const char* s1, const char* s2,
               const char* s3, const char* s4, messVec* list) {
 _NO_IMPLEMENTATION_;
 return 0;
}
//_____________________________________________________________________________
int StLoggerManager::RemoveMessage(StMessage* mess) {
_NO_IMPLEMENTATION_;
 return 0;
}
//_____________________________________________________________________________
void StLoggerManager::Summary(size_t nTerms) {
//
// Output a summary of the messages printed so far.
//   nTerms - number of tokens (text separated by spaces) to use in
//            comparisons between messages (default is 1). Messages are
//            compared only if they are of the same message type, and
//            only the message string (not the message type) is included
//            in the number of tokens compared. For example, Summary(0)
//            would give the number of each type of message with no
//            differentiation based on the message string.
//
_NO_IMPLEMENTATION_;
}
//_____________________________________________________________________________
void StLoggerManager::MemorySummary() {
//
// Output a summary of the memory usage of the message manager so far.
// Loops over all stored messages to calculate message sizes and overhead.
//
  _NO_IMPLEMENTATION_;
}
//_____________________________________________________________________________
int StLoggerManager::AddType(const char* type, const char* text) {
//
// Add an additional message type. ListTypes() will return a list of all
// currently defined message types. AddType returns a integer which is zero
// if type has already been defined, otherwise returns the number of types
// defined (including the new one).
//   type - a single character string which will represent the message type.
//   text - a short charact string which will be printed with all messages
//          of this type.
//
 _NO_IMPLEMENTATION_;
 return 0;
}
//_____________________________________________________________________________
void StLoggerManager::PrintInfo() {
   fLogger->info("**************************************************************\n");
   fLogger->info("* $Id: StLoggerManager.cxx,v 1.50 2013/07/23 20:03:05 dmitry Exp $\n");
   //  printf("* %s    *\n",m_VersionCVS);
   fLogger->info("**************************************************************\n");
}

//_____________________________________________________________________________

//_____________________________________________________________________________
//
// Info Messages:
//_____________________________________________________________________________
ostrstream& StLoggerManager::Info(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
{ return Message(mess, "I", opt,sourceFileName,lineNumber);}
//_____________________________________________________________________________
int StLoggerManager::PrintInfos()
{
   _NO_IMPLEMENTATION_;
   return 0;
   // return PrintList(messCollection[1]);
}
//_____________________________________________________________________________
const messVec* StLoggerManager::GetInfos()
{
   _NO_IMPLEMENTATION_;
   return 0;
   // return (messCollection[1]);
}
//_____________________________________________________________________________
StMessage* StLoggerManager::FindInfo(const char* s1, const char* s2,
                                     const char* s3, const char* s4)
{
   _NO_IMPLEMENTATION_;
   return 0;
   // return FindMessage(s1,s2,s3,s4,messCollection[1]);
}
//_____________________________________________________________________________
messVec* StLoggerManager::FindInfoList(const char* s1, const char* s2,
                                       const char* s3, const char* s4)
{
   _NO_IMPLEMENTATION_;
   return 0;
   // return FindMessageList(s1,s2,s3,s4,messCollection[1]);
}

//_____________________________________________________________________________
//
// Warning Messages:
//_____________________________________________________________________________
ostrstream& StLoggerManager::Warning(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
{ return Message(mess, "W", opt,sourceFileName,lineNumber);}
//_____________________________________________________________________________
int StLoggerManager::PrintWarnings()
{
   _NO_IMPLEMENTATION_;
   return 0;
   // return PrintList(messCollection[2]);
}
//_____________________________________________________________________________
const messVec* StLoggerManager::GetWarnings()
{
   _NO_IMPLEMENTATION_;
   return 0;
   //   return (messCollection[2]);
}
//_____________________________________________________________________________
StMessage* StLoggerManager::FindWarning(const char* s1, const char* s2,
                                        const char* s3, const char* s4)
{
   _NO_IMPLEMENTATION_;
   return 0;
    // return FindMessage(s1,s2,s3,s4,messCollection[2]);
}
//_____________________________________________________________________________
messVec* StLoggerManager::FindWarningList(const char* s1, const char* s2,
                                          const char* s3, const char* s4)
{
   _NO_IMPLEMENTATION_;
   return 0;
    // return FindMessageList(s1,s2,s3,s4,messCollection[2]);
}

//_____________________________________________________________________________
//
// Error Messages:
//_____________________________________________________________________________
ostrstream& StLoggerManager::Error(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
{ return Message(mess, "E", opt,sourceFileName,lineNumber);}
//_____________________________________________________________________________
int StLoggerManager::PrintErrors()
{
   _NO_IMPLEMENTATION_;
   return 0;
   //return PrintList(messCollection[3]);
}
//_____________________________________________________________________________
const messVec* StLoggerManager::GetErrors()
{
   _NO_IMPLEMENTATION_;
   return 0;
 // return (messCollection[3]);
}
//_____________________________________________________________________________
StMessage* StLoggerManager::FindError(const char* s1, const char* s2,
                                      const char* s3, const char* s4)
{
   _NO_IMPLEMENTATION_;
   return 0;
    // return FindMessage(s1,s2,s3,s4,messCollection[3]);
}
//_____________________________________________________________________________
messVec* StLoggerManager::FindErrorList(const char* s1, const char* s2,
                                        const char* s3, const char* s4)
{
   _NO_IMPLEMENTATION_;
   return 0;
    // return FindMessageList(s1,s2,s3,s4,messCollection[3]);
}

//_____________________________________________________________________________
//
// Debug Messages:
//_____________________________________________________________________________
ostrstream& StLoggerManager::Debug(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
{ return Message(mess, "D", opt,sourceFileName,lineNumber);}
//_____________________________________________________________________________
int StLoggerManager::PrintDebug()
{
   _NO_IMPLEMENTATION_;
   return 0;
   //   return PrintList(messCollection[4]);
}
//_____________________________________________________________________________
const messVec* StLoggerManager::GetDebugs()
{
_NO_IMPLEMENTATION_;   return 0;

//   return (messCollection[4]);
}
//_____________________________________________________________________________
StMessage* StLoggerManager::FindDebug(const char* s1, const char* s2,
                                      const char* s3, const char* s4)
{
   _NO_IMPLEMENTATION_; return 0;
   // return FindMessage(s1,s2,s3,s4,messCollection[4]);
}
//_____________________________________________________________________________
messVec* StLoggerManager::FindDebugList(const char* s1, const char* s2,
                                        const char* s3, const char* s4)
{
_NO_IMPLEMENTATION_;   return 0;
    // return FindMessageList(s1,s2,s3,s4,messCollection[4]);
}

//_____________________________________________________________________________
//
// QAInfo Messages:
//_____________________________________________________________________________
ostrstream& StLoggerManager::QAInfo(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
{ return Message(mess, "Q", opt,sourceFileName,lineNumber);}
//_____________________________________________________________________________
//
// UCMInfo Messages:
//_____________________________________________________________________________
ostrstream& StLoggerManager::UCMInfo(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
{ return Message(mess, "U", opt,sourceFileName,lineNumber);}
//_____________________________________________________________________________
void StLoggerManager::IgnoreRepeats()
{
    AllowRepeats(0);
}

//_____________________________________________________________________________
void StLoggerManager::AllowRepeats(int nRepeats)
{
   // fAllowRepeat   > 0 the max number to repeat one and the same message
   //               == 0 the repating is not allowed
   //               == -1 there is no limit;
   fAllowRepeat       = nRepeats;
   fLastRepeatCounter = 0;
   StarOptionFilterPtr& filter = ((StLoggerManager *)mInstance)->GetStarOptionFilter();
   if (filter != 0 )
       filter->setRepeatCounterOption(nRepeats);

}


//_____________________________________________________________________________
void StLoggerManager::AllowRepeats()
{
   AllowRepeats(-1);
}


//_____________________________________________________________________________
int StLoggerManager::PrintQAInfo()
{
_NO_IMPLEMENTATION_;   return 0;
    // return PrintList(messCollection[5]);
}
//_____________________________________________________________________________
const messVec* StLoggerManager::GetQAInfos()
{
_NO_IMPLEMENTATION_;   return 0;
  // return (messCollection[5]);
}
//_____________________________________________________________________________
StMessage* StLoggerManager::FindQAInfo(const char* s1, const char* s2,
                                       const char* s3, const char* s4)
{
_NO_IMPLEMENTATION_;    return 0;
    // return FindMessage(s1,s2,s3,s4,messCollection[5]);
}
//_____________________________________________________________________________
messVec* StLoggerManager::FindQAInfoList(const char* s1, const char* s2,
                                         const char* s3, const char* s4)
{
_NO_IMPLEMENTATION_;   return 0;
   // return FindMessageList(s1,s2,s3,s4,messCollection[5]);
}

//_____________________________________________________________________________
int StLoggerManager::PrintUCMInfo()
{
_NO_IMPLEMENTATION_;   return 0;
    // return PrintList(messCollection[5]);
}
//_____________________________________________________________________________
const messVec* StLoggerManager::GetUCMInfos()
{
_NO_IMPLEMENTATION_;   return 0;
  // return (messCollection[5]);
}
//_____________________________________________________________________________
StMessage* StLoggerManager::FindUCMInfo(const char* s1, const char* s2,
                                       const char* s3, const char* s4)
{
_NO_IMPLEMENTATION_;    return 0;
    // return FindMessage(s1,s2,s3,s4,messCollection[5]);
}
//_____________________________________________________________________________
messVec* StLoggerManager::FindUCMInfoList(const char* s1, const char* s2,
                                         const char* s3, const char* s4)
{
_NO_IMPLEMENTATION_;   return 0;
   // return FindMessageList(s1,s2,s3,s4,messCollection[5]);
}

//_____________________________________________________________________________
ostrstream& StLoggerManager::Fatal(const char* mess, const char* opt,const char *sourceFileName, int lineNumber)
{ return Message(mess,"F",opt,sourceFileName,lineNumber);}

// "As is" Messages:
//_____________________________________________________________________________
ostrstream& StLoggerManager::out(const char* mess)
{return Message(mess,"I","OP-");}
//_____________________________________________________________________________
ostrstream& StLoggerManager::err(const char* mess)
{return Message(mess,"E","EP-");}



// Dummy methods
//_____________________________________________________________________________
int StLoggerManager::PrintAll()
{
_NO_IMPLEMENTATION_;   return 0;
  //  return PrintList(&messList);
}

//_____________________________________________________________________________
const messVec* StLoggerManager::GetAll()
{
_NO_IMPLEMENTATION_;   return 0;

//   return (&messList);
}

//_____________________________________________________________________________
int StLoggerManager::RemoveMessage(const char* s1, const char* s2,
         const char* s3, const char* s4)
{  return RemoveMessage(FindMessage(s1,s2,s3,s4));             }

//_____________________________________________________________________________
void StLoggerManager::SetLimit(const char* str, int n)
{
     if (str && str[0] && str[1]) {
        LoggerPtr root = Logger::getRootLogger();
         AppenderPtr defaultAppender = root->getAppender(_T("defaultAppender"));
        if (defaultAppender != 0){
           StarOptionFilterPtr filter = new StarOptionFilter();
           filter->setRepeatCounterOption(n);
           filter->setOption(_T("StringToCount"),str);
//           cout << endl<< "--  Appedner " << defaultAppender->getName() << " -* -* -* -* -* -* -* -* "
//                 << str << "n = " << n << " -* -* -* -* -* -* -* -* " << endl << endl;
           defaultAppender->addFilter(filter);
        }
    }

//   messCounter->SetLimit(str,n);
}
//_____________________________________________________________________________
int StLoggerManager::GetLimit(const char* str)
{
_NO_IMPLEMENTATION_;   return -1;
   // return messCounter->GetLimit(str);
}
//_____________________________________________________________________________
void StLoggerManager::ListLimits()
{
_NO_IMPLEMENTATION_;
// messCounter->ListLimits();
}
//_____________________________________________________________________________
void StLoggerManager::Close()
{
 // Close the messenger streams
 // Close the QA-related appenders to flush its buffers
   if (fgQALogger)  fgQALogger->closeNestedAppenders();
   if (fgUCMLogger) fgUCMLogger->closeNestedAppenders();
}
//_____________________________________________________________________________
void StLoggerManager::RemoveLimit(const char* str)
{  SetLimit(str,-1);                                           }
//_____________________________________________________________________________
void StLoggerManager::SwitchOff(const char* str)
{ SetLimit(str,0);                                             }
//_____________________________________________________________________________
void StLoggerManager::SwitchOn(const char* str)
{  RemoveLimit(str);                                           }
//_____________________________________________________________________________
void StLoggerManager::FixOn(const char* str)
{  SetLimit(str,-5);                                           }
//_____________________________________________________________________________
void StLoggerManager::NoLimits()
{
_NO_IMPLEMENTATION_;
// messCounter->NoLimits();
}
//_____________________________________________________________________________
void StLoggerManager::MemoryOn()
{
_NO_IMPLEMENTATION_;
//   remember=1;
}
//_____________________________________________________________________________
void StLoggerManager::MemoryOff()
{
_NO_IMPLEMENTATION_;
// remember=0;
}
//_____________________________________________________________________________
int StLoggerManager::ListTypes()
{
_NO_IMPLEMENTATION_;   return 5;
//   return messTypeList->ListTypes();
}

//_____________________________________________________________________________
void StLoggerManager::SetLevel(Int_t level)
{
   // Map STAR level to the logger level and set the logger level
   switch (level) {
      case kFatal:
         fLogger->setLevel(LOG4CXX_LEVEL_FATAL);
         break;
      case kError:
         fLogger->setLevel(LOG4CXX_LEVEL_ERROR);
         break;
      case kWarning:
         fLogger->setLevel(LOG4CXX_LEVEL_WARN);
         break;
      case kInfo:
         fLogger->setLevel(LOG4CXX_LEVEL_INFO);
         break;
      case kAll:
      case kDebug:
      case kDebug2:
         fLogger->setLevel(LOG4CXX_LEVEL_DEBUG);
         break;
      case kDefault:
         // restore the default level (defined the XML configuration if present)
         fLogger->setLevel(fDefaultLevel);
         break;
      default:
         fLogger->setLevel(LOG4CXX_LEVEL_DEBUG);
         break;
   };
}
//_____________________________________________________________________________
Int_t StLoggerManager::GetLevel(Int_t) const
{
   // Map the current logger level to the STAR one
#if 0
   const LevelPtr &level = fLogger->getLevel();
        if (level == &LOG4CXX_LEVEL_DEBUG)  return kDebug;
   else if (level == &LOG4CXX_LEVEL_FATAL)  return kFatal;
   else if (level == &LOG4CXX_LEVEL_ERROR)  return kError;
   else if (level == &LOG4CXX_LEVEL_WARN )  return kWarning;
   else if (level == &LOG4CXX_LEVEL_INFO )  return kInfo;
#endif
   return kAll;
}

//_____________________________________________________________________________
void StLoggerManager:: DestroyInstance()
{
    if (mInstance) {
       // There is no method to chech ref. do it blindly.
       // fgQALogger->releaseRef(); fgQALogger->releaseRef();
       fgUCMLogger->releaseRef();
       // fgUCMLogger->releaseRef();
       // Logger::getRootLogger()->releaseRef();
       // StMessMgr *thisPtr = mInstance;
       mInstance = 0; gMessMgr = 0;
       // delete thisPtr;
    }
}
#if 0
//_____________________________________________________________________________
const char *GetName()
{
  // Returns the name of the current logger
  CurrentMessager()
}
#endif

//_____________________________________________________________________________
  // Instantiate the (singleton) class upon loading
  //
// static StMessMgr* temp=StLoggerManager::Instance();
// ostrstream& gMess = *(StMessMgr *)StLoggerManager::Instance();

//_____________________________________________________________________________
// $Id: StLoggerManager.cxx,v 1.50 2013/07/23 20:03:05 dmitry Exp $
// $Log: StLoggerManager.cxx,v $
// Revision 1.50  2013/07/23 20:03:05  dmitry
// missing backwards compatibility feature restored
//
// Revision 1.49  2012/12/04 15:31:03  jeromel
// Replaced GNUC by STAR_LOG4CXX_VERSION for consistency, comment if 1
//
// Revision 1.48  2012/11/26 23:02:17  fisyak
// Add color by demand for __GNUC__ > 3
//
// Revision 1.47  2012/11/26 12:26:18  jeromel
// Undo (again!)
//
// Revision 1.45  2012/10/05 17:16:33  jeromel
// [no real change, trim trailing spaces]
//
// Revision 1.44  2012/10/05 17:14:32  jeromel
// Made no color the default (set yes to Logger.color to yes if one wants to enable)
//
// For now, restored no color as fLogger->XXX does not take TSring but char * under
// older log4cxx version (compilation would break). Other solution to be found later.
//
// A side note that a xml based setting for colors (including which color goes to
// what level of messages) is likely the proper approach to avoid mixing config
// approach.
//
// Revision 1.43  2012/10/05 14:16:48  fisyak
// Change default from color => non color
//
// Revision 1.42  2012/10/04 23:32:29  fisyak
// allow to switch off color by putting in .rootrc line with Logger.Color  no
//
// Revision 1.41  2012/10/04 15:44:20  fisyak
// Add colors
//
// Revision 1.40  2012/06/11 14:58:55  fisyak
// std namespace
//
// Revision 1.39  2010/04/27 21:31:44  fine
// remove the logger destruction side effect
//
// Revision 1.38  2010/04/23 22:39:11  fine
//  RT #1911. Make interface log4cxx compliant. Remove the dtor dead-lock and add post-mortim clean up
//
// Revision 1.37  2009/09/09 00:05:13  fine
// Merge log4cxx version 9 and 10
//
// Revision 1.36  2009/06/23 19:37:33  fine
// replace QA logger with the dedicated UCM one
//
// Revision 1.35  2009/06/23 19:21:21  fine
// Add the mandatory UCM filters
//
// Revision 1.34  2009/06/22 22:36:01  fine
// Add the new dedicated UCM logger, It should force the recompilation of many STAR packages
//
// Revision 1.33  2009/06/22 01:12:08  fine
// Disable the default UCM. Use log4j.xml instead, for the time being
//
// Revision 1.32  2009/06/19 22:18:36  fine
// pick the technology from the LOGGING var
//
// Revision 1.31  2009/06/17 22:11:59  fine
// Add UCM appender to the StStarLogger and activate it as soon as the env LOGGING is set to UCM
//
// Revision 1.30  2008/05/19 15:08:20  fine
// allow complex formating with several logger stream concurrently. Thanx Victor
//
// Revision 1.29  2008/05/15 23:40:23  fine
// Change the abstarct class return type to separate the different STAR streams
//
// Revision 1.28  2007/08/08 20:50:22  fine
//  Fix bug: some messages submitted via the old interface were lost
//
// Revision 1.27  2007/08/03 21:34:51  fine
// fix StStarLogger for Sl 4.4
//
// Revision 1.26  2007/02/13 22:07:26  perev
// Add the lost part of the ROOT message - location
//
// Revision 1.25  2007/01/30 20:48:57  fine
// Make the deault level for all loggers INFO
//
// Revision 1.24  2007/01/30 19:25:51  fine
// Set the deafult level for QA to b INFO
//
// Revision 1.23  2007/01/25 18:36:38  fine
// Acivate logger level StMaker level run-time adjustment
//
// Revision 1.22  2007/01/23 22:27:14  fine
// Set the dwefault logger level to WARN
//
// Revision 1.21  2006/07/01 01:19:17  fine
// Add new jiob tracking option code
//
// Revision 1.20  2006/06/05 00:21:40  fine
// class the new StMessMgr method to flush the logger buffers
//
// Revision 1.19  2006/05/19 21:42:30  fine
// remove the debug print outs
//
// Revision 1.18  2005/12/20 20:51:15  fine
// fix typo
//
// Revision 1.17  2005/10/10 20:24:17  fine
// Bug fix: add the brackets around the if statements
//
// Revision 1.16  2005/09/14 15:35:25  fine
// Clean up the code
//
// Revision 1.15  2005/08/21 20:20:18  perev
// fix typo in comment
//
// Revision 1.14  2005/08/19 21:01:05  fine
// Manage the ROOT messages with the StarLogger
//
// Revision 1.13  2005/08/05 19:39:14  fine
// Make use of the DOM Configurator instead of PropertyConfigurator
//
// Revision 1.12  2005/03/07 23:17:09  fine
// Fix a bug to allow mnay logger insatnces to print out without any mess
//
// Revision 1.11  2004/11/15 17:25:59  fine
// Make Star filer workign properly
//
// Revision 1.10  2004/11/13 00:28:16  fine
// teach StarOption filter to count the messages
//
// Revision 1.9  2004/11/05 20:24:43  fine
// Remove some redundant lines, clean up
//
// Revision 1.8  2004/11/05 20:24:00  fine
// Replace the obsolete StLoggerManager::Instance with tMessMgr::CurrentMessager()
//
// Revision 1.7  2004/11/03 16:39:32  fine
// add extra method to checl STAR QA info logger
//
// Revision 1.6  2004/11/03 01:33:22  fine
// add the implemantions of the new based methods and clean up
//
// Revision 1.5  2004/09/16 00:13:05  fine
// remove the implict StStarLogger object. We should give log4cxx a chance to complete the intialization
//
// Revision 1.4  2004/05/11 23:48:19  fine
// update the default logger pattern
//
// Revision 1.3  2004/05/11 23:21:58  fine
// add the default logger description
//
// Revision 1.2  2004/05/11 22:42:27  fine
// Add the default configurator
//
// Revision 1.1  2004/05/11 20:58:48  fine
// first implemantation of the abstarct STAR messeger via log4cxx package
//
