//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StLoggerManager                                                     //
//                                                                      //
// This class manages the messages in STAR software. It is a singleton. //
// It inherits from StMessMgr, which provides the external interface.   //
// Messages are stored in a vector, and come in several types           //
// (i.e. info, error, debug ). The types "I" (info), "W" (warning),     //
// "E" (error), "D" (debug), and "Q" (QAInfo) are predefined. Message   //
// finding and summary tools are also available.                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#ifdef __ROOT__
#include "TROOT.h"
#include "TSystem.h"
#endif

#ifndef  _NO_IMPLEMENTATION_
#define  _NO_IMPLEMENTATION_   {       \
   const char *text = __FUNCTION__;    \
   NDC::push(_T("NO IMPLEMENTATION")); \
   PrintLogger(text,"D","");           \
   NDC::pop();                         \
}
#endif 

#include "StLoggerManager.h"

// #include <log4cxx/logger.h>
#include <log4cxx/basicconfigurator.h>
#include <log4cxx/propertyconfigurator.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/ndc.h>
#include <log4cxx/consoleappender.h>
#include <log4cxx/patternlayout.h>
#include <log4cxx/layout.h>


using namespace log4cxx;
using namespace log4cxx::helpers;
// using namespace log4cxx::xml;

class  StMessage  {
  public: StMessage(){}
};

const char *StLoggerManager::fgLevels = "FEWIDQ";

StMessMgr* gMessMgr = 0;
StMessage* endm     = 0;

//________________________________________
std::ostream& StLoggerManager::OperatorShift(std::ostream& os, StMessage* stm) {
// std::ostream& operator<<(std::ostream& os, StMessage* stm) {
  if (((&os) == (std::ostream*) StLoggerManager::Instance()) && (stm == endm)) {
    // There was a StMessage terminator
    *this << ends;
    StLoggerManager::Instance()->Print();                
  } else {
     assert(0);
    // if (stm) os << stm->GetMessage();  // Output this message to the ostream
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
                : StMessMgr(), fAllowRepeat(-1),fLastRepeatCounter(0)
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
  
  fCurType     = new char[ 2];  fCurType[0] = 0; fCurType[1] = 0;
  fCurOpt      = new char[32];  fCurOpt [0] = 0; 
  SwitchOff("D");
  MemoryOn();
}

//_____________________________________________________________________________
StLoggerManager::~StLoggerManager() {
//
// Destructor - must delete the message lists
//
  if (StLoggerManager::Instance() == this) gMessMgr = 0;
  delete [] fCurType; fCurType = 0;
  delete [] fCurOpt;  fCurOpt = 0;
}
log4cxx::LoggerPtr StLoggerManager::fgQALogger;      //!  Logger to server QA stream
//_____________________________________________________________________________
StMessMgr* StLoggerManager::Instance(const char *loggerName) 
{
   return  new StLoggerManager(loggerName);
}
//_____________________________________________________________________________
StMessMgr* StLoggerManager::Instance() {
//
// Allows anyone to get a pointer to the single message manager:
//   StLoggerManager::Instance()->(member function)
//
  if (!mInstance) {
    // BasicConfigurator::configure();
    String propertyFile = "log4j.xml";
    const char *proEnv = gSystem->Getenv("STAR_LOGGER_PROPERTY");
    if (proEnv && proEnv[0] ) propertyFile=proEnv;
    if (!gSystem->AccessPathName(propertyFile.c_str())) {
       PropertyConfigurator::configure(propertyFile);
    } else {       
     	// BasicConfigurator::configure();
       LoggerPtr root = Logger::getRootLogger();
      	root->addAppender(new ConsoleAppender(
		      new PatternLayout("%-3c{2}:%-5p - %m%n")));
//		      new PatternLayout(PatternLayout::TTCC_CONVERSION_PATTERN)));

    }
    Logger::getRootLogger();
    fgQALogger = Logger::getLogger("QA");
    NDC::push(_T(":"));

    mInstance = (StMessMgr*) new StLoggerManager;
    // if (gMessMgr) delete gMessMgr; gMessMgr = 0;
    gMessMgr  = mInstance;
  }
  return mInstance;
}
//_____________________________________________________________________________
StMessMgr& StLoggerManager::Message(const char* mess, const char* type,
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
//   'D' - debugS
//   'Q' - QA 
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
  size_t messSize = (mess && mess[0]) ? strlen(mess) : 0;
  seekp(0);
  *fCurType = typeChar;
  strcpy(fCurOpt,opt);
  if (messSize && lineNumber == -1 && (!sourceFileName)) {
     BuildMessage(mess, type, opt,sourceFileName, lineNumber);     // comes back with fCurType=0
  } else {
//    building = 1;
     if (messSize>0) {
        // Add the message to the logger context
        assert(0);
        NDC::push(_T("mess"));
        mess = 0;
     }
    if (sourceFileName && sourceFileName[0] ) fSourceFileNames[LevelIndex(*fCurType)] = sourceFileName;
    else fSourceFileNames[LevelIndex(*fCurType)].clear();
    fLineNumbers[LevelIndex(*fCurType)]     = lineNumber;
  }
  return *((StMessMgr*) this);
}
//_____________________________________________________________________________
void StLoggerManager::BuildMessage(const char* mess, const char* type,
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
void StLoggerManager::PrintLogger(const char* mess, const char* type,
    const char* opt,const char *sourceFileName, int lineNumber) 
{
   if (!opt) opt = "";
   unsigned char typeChar = 'I';
   if (type && type[0]) typeChar = type[0];
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
      // fprintf(stderr, " **** LOGGER **** %c %s\n", typeChar, mess);
      switch (typeChar)  {
        case 'F': fLogger->fatal  (_T(mess),sourceFileName,lineNumber); break;
        case 'E': fLogger->error  (_T(mess),sourceFileName,lineNumber); break;
        case 'W': fLogger->warn   (_T(mess),sourceFileName,lineNumber); break;
        case 'I': fLogger->info   (_T(mess),sourceFileName,lineNumber); break;
        case 'D': fLogger->debug  (_T(mess),sourceFileName,lineNumber); break;
        case 'Q': fgQALogger->info(_T(mess),sourceFileName,lineNumber); break;
        default: fLogger->info(_T(mess),sourceFileName,lineNumber);     break;
      };
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
  std::string message = std::ostringstream::str();
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
   fLogger->info("* $Id: StLoggerManager.cxx,v 1.4 2004/05/11 23:48:19 fine Exp $\n");
   //  printf("* %s    *\n",m_VersionCVS);
   fLogger->info("**************************************************************\n");
}

//_____________________________________________________________________________

//_____________________________________________________________________________
//
// Info Messages:
//_____________________________________________________________________________
StMessMgr& StLoggerManager::Info(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
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
StMessMgr& StLoggerManager::Warning(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
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
StMessMgr& StLoggerManager::Error(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
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
StMessMgr& StLoggerManager::Debug(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
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
StMessMgr& StLoggerManager::QAInfo(const char* mess, const char* opt,const char *sourceFileName,int lineNumber)
{ return Message(mess, "Q", opt,sourceFileName,lineNumber);}
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
StMessMgr& StLoggerManager::Fatal(const char* mess, const char* opt,const char *sourceFileName, int lineNumber)
{ return Message(mess,"F",opt,sourceFileName,lineNumber);}

// "As is" Messages:
//_____________________________________________________________________________
StMessMgr& StLoggerManager::out(const char* mess)
{return Message(mess,"I","OP-");}
//_____________________________________________________________________________
StMessMgr& StLoggerManager::err(const char* mess)
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
_NO_IMPLEMENTATION_;
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
   
   // Instantiate the (singleton) class upon loading
//
static StMessMgr* temp=StLoggerManager::Instance();
// StMessMgr& gMess = *(StMessMgr *)StLoggerManager::Instance();

//_____________________________________________________________________________
// $Id: StLoggerManager.cxx,v 1.4 2004/05/11 23:48:19 fine Exp $
// $Log: StLoggerManager.cxx,v $
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
