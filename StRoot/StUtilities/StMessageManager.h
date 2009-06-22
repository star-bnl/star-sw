/*!
  \class StMessageManager
  \author G. Van Buren, BNL

  This class manages the messages in STAR software. It is a singleton.
  It inherits from StMessMgr, which provides the external interface.
  Messages are stored in a vector, and come in several types
  (i.e. info, error, debug ). The types "I" (info), "W" (warning),
  "E" (error), "D" (debug), and "Q" (QAInfo) are predefined.
  Message finding and summary tools are also available.
  \sa http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/StUtilities/doc/StMessMgr.html

*/

#ifndef ClassStMessageManager
#define ClassStMessageManager

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
   int remember;

 protected:
   StMessageManager();
   StMessageManager(const StMessageManager&);
   messVec messList;
   messTypeVec messCollection;
   virtual messVecIter FindMessageIter(const char* s1, const char* s2="",
         const char* s3="", const char* s4="", messVec* list=0);
   virtual        void BuildMessage(const char* mess="", const char* type="",
         const char* opt=0);
   virtual void IgnoreRepeats() { StMessage::IgnoreRepeats(); }
   virtual void AllowRepeats() { StMessage::AllowRepeats(); }


 public:
   virtual ~StMessageManager();
   static StMessMgr* Instance();      //!

   virtual std::ostream& OperatorShift(std::ostream& os, StMessage* stm);

// Generic Messages:
   virtual ostrstream& Message(const char* mess="", const char* type="",
         const char* opt=0,const char *sourceFileName=0, int lineNumber=-1);
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
   virtual       void SetLevel(Int_t logLevel);
   virtual      Int_t GetLevel(Int_t logLevel) const;
   virtual const char *GetName() const;
   virtual       void SwitchOff(const char* str) {SetLimit(str,0);}
   virtual       void SwitchOn(const char* str) {RemoveLimit(str);}
   virtual       void FixOn(const char* str) {SetLimit(str,-5);}
   virtual       void NoLimits() {messCounter->NoLimits();}
   virtual       void Summary(size_t nTerms=1);
   virtual       void MemorySummary();
   virtual       void MemoryOn() {remember=1;}
   virtual       void MemoryOff() {remember=0;}
   virtual        int AddType(const char* type, const char* text);
   virtual        int ListTypes() {return messTypeList->ListTypes();}

// Info Messages:
   virtual ostrstream& Info(const char* mess="", const char* opt="O",const char *sourceFileName=0, int lineNumber=-1)
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
   virtual ostrstream& Warning(const char* mess="", const char* opt="E",const char *sourceFileName=0, int lineNumber=-1)
         { return Message(mess, "W", opt,sourceFileName,lineNumber);}
   virtual        int PrintWarnings() {return PrintList(messCollection[2]); }
   virtual const messVec* GetWarnings() {return (messCollection[2]);}
   virtual StMessage* FindWarning(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[2]);}
   virtual messVec* FindWarningList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[2]);}

// Error Messages:
   virtual ostrstream& Error(const char* mess="", const char* opt="E",const char *sourceFileName=0, int lineNumber=-1)
         { return Message(mess, "E", opt,sourceFileName,lineNumber);}
   virtual        int PrintErrors() {return PrintList(messCollection[3]); }
   virtual const messVec* GetErrors() {return (messCollection[3]);}
   virtual StMessage* FindError(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[3]);}
   virtual messVec* FindErrorList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[3]);}

// Debug Messages:
   virtual ostrstream& Debug(const char* mess="", const char* opt="OT",const char *sourceFileName=0, int lineNumber=-1)
         { return Message(mess, "D", opt,sourceFileName,lineNumber);}
   virtual        int PrintDebug() {return PrintList(messCollection[4]); }
   virtual const messVec* GetDebugs() {return (messCollection[4]);}
   virtual StMessage* FindDebug(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[4]);}
   virtual messVec* FindDebugList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[4]);}

// QAInfo Messages:
   virtual ostrstream& QAInfo(const char* mess="", const char* opt="OS",const char *sourceFileName=0, int lineNumber=-1)
         { return Message(mess, "Q", opt,sourceFileName,lineNumber);}
   virtual        int PrintQAInfo() {return PrintList(messCollection[5]); }
   virtual const messVec* GetQAInfos() {return (messCollection[5]);}
   virtual StMessage* FindQAInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[5]);}
   virtual messVec* FindQAInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[5]);}

// UCMInfo Messages:
   virtual ostrstream& UCMInfo(const char* mess="", const char* opt="OS",const char *sourceFileName=0, int lineNumber=-1)
         { return Message(mess, "U", opt,sourceFileName,lineNumber);}
   virtual        int PrintUCMInfo() {return PrintList(messCollection[6]); }
   virtual const messVec* GetUCMInfos() {return (messCollection[6]);}
   virtual StMessage* FindUCMInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessage(s1,s2,s3,s4,messCollection[6]);}
   virtual messVec* FindUCMInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="")
	 {return FindMessageList(s1,s2,s3,s4,messCollection[6]);}

// "As is" Messages:
   virtual ostrstream& out(const char* mess="")
	 {return Message(mess,"I","OP-");}
   virtual ostrstream& err(const char* mess="")
	 {return Message(mess,"E","EP-");}

   virtual       void PrintInfo();
// Fatal Messages:
   virtual ostrstream& Fatal(const char* mess="", const char* opt="E",const char *sourceFileName=0, int lineNumber=-1)
   { return Message(mess, "E", opt,sourceFileName,lineNumber);}

};


#endif

// $Id: StMessageManager.h,v 1.29 2009/06/22 22:36:01 fine Exp $
// $Log: StMessageManager.h,v $
// Revision 1.29  2009/06/22 22:36:01  fine
// Add the new dedicated UCM logger, It should force the recompilation of many STAR packages
//
// Revision 1.28  2008/05/15 23:40:24  fine
// Change the abstarct class return type to separate the different STAR streams
//
// Revision 1.27  2007/01/25 06:28:06  fine
// connect Logger and Maker debug levels
//
// Revision 1.26  2007/01/25 06:11:37  fine
// Add the new StMess abstarct interfaces GetLevel/SetLevel
//
// Revision 1.25  2004/04/15 21:28:02  fine
// Remove the redundant StMessageManager RootCint dictionary. User shoudl use the base StMessMgr class anyway
//
// Revision 1.24  2004/04/15 16:03:38  fine
// move StMessMgr class to St_base and change the interface
//
// Revision 1.22  2004/01/28 00:09:14  genevb
// Messages (except Debug) default to no time-date stamp
//
// Revision 1.21  2003/09/25 21:19:22  genevb
// Some new cout-like functions and friend functions, some doxygen-ization
//
// Revision 1.20  2001/05/14 20:53:20  genevb
// Add features to examine memory use, switch from TDatime to time_t
//
// Revision 1.19  2000/06/07 00:05:36  genevb
// Added FixOn(), enforcing no limits on a specific message type/string
//
// Revision 1.18  2000/05/23 19:03:38  genevb
// Correct interface for MessageOut(), update docs
//
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
