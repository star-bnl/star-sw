// $Id: StMessMgr.h,v 1.11 2000/06/07 00:05:35 genevb Exp $
// $Log: StMessMgr.h,v $
// Revision 1.11  2000/06/07 00:05:35  genevb
// Added FixOn(), enforcing no limits on a specific message type/string
//
// Revision 1.10  2000/03/30 16:12:55  genevb
// Add NoLimits() capability to turn off message limiting.
//
// Revision 1.9  2000/01/05 19:53:46  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
// Revision 1.8  1999/09/16 15:50:24  genevb
// Fixed a bug in over-writing memory when calling from FORTRAN, use char=0 instead of strcpy
//
// Revision 1.7  1999/09/14 15:42:02  genevb
// Some bug fixes, workaround for nulls in strings
//
// Revision 1.6  1999/08/18 18:28:32  fine
// Various bugs have been fixed. share lib was not loaded under HP
//
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

#ifdef __ROOT__
#include "Rtypes.h"
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
   StMessMgr();
   StMessMgr(const StMessMgr&){;}
   virtual ~StMessMgr(){;}

// Generic Messages:
   virtual StMessMgr& Message(const char* mess="", const char* type="",
         const char* opt=0)= 0;
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
   virtual        int AddType(const char* type, const char* text) =0;
   virtual        int ListTypes() =0;

// Info Messages:
   virtual StMessMgr& Info(const char* mess="", const char* opt="O")=0;
   virtual        int PrintInfos() =0;
   virtual const messVec* GetInfos() =0;
   virtual StMessage* FindInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Warning Messages:
   virtual StMessMgr& Warning(const char* mess="", const char* opt="E")= 0;
   virtual        int PrintWarnings() =0;
   virtual const messVec* GetWarnings() =0;
   virtual StMessage* FindWarning(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindWarningList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Error Messages:
   virtual StMessMgr& Error(const char* mess="", const char* opt="E") = 0;
   virtual        int PrintErrors() =0;
   virtual const messVec* GetErrors() =0;
   virtual StMessage* FindError(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindErrorList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// Debug Messages:
   virtual StMessMgr& Debug(const char* mess="", const char* opt="O")= 0;
   virtual        int PrintDebug() =0;
   virtual const messVec* GetDebugs() =0;
   virtual StMessage* FindDebug(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindDebugList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

// QAInfo Messages:
   virtual StMessMgr& QAInfo(const char* mess="", const char* opt="OTS") = 0;
   virtual        int PrintQAInfo() =0;
   virtual const messVec* GetQAInfos() =0;
   virtual StMessage* FindQAInfo(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;
   virtual messVec* FindQAInfoList(const char* s1, const char* s2="",
         const char* s3="", const char* s4="") =0;

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
