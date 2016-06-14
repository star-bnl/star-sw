//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessageManager                                                     //
//                                                                      //
// This class manages the messages in STAR software. It is a singleton. //
// It inherits from StMessMgr, which provides the external interface.   //
// Messages are stored in a vector, and come in several types           //
// (i.e. info, error, debug ). The types "I" (info), "W" (warning),     //
// "E" (error), "D" (debug), "Q" (QAInfo), and "U" (UCMInfo) 
// are predefined.
//
// Message finding and summary tools are also available.                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#ifdef __ROOT__
#include "TROOT.h"
#endif

#include "StMessageManager.h"
#include "StMessageStream.h"
#include <string.h>
#ifdef __linux__
#include <math.h>
#endif

//________________________________________
// ostream& operator<<(ostream& os, StMessage* stm) 
std::ostream& StMessageManager::OperatorShift(std::ostream& os, StMessage* stm)
{
  if (((&os) == (ostream*) gMessMgr) && (stm == endm)) {
    gMessMgr->Print();                 // This was a StMessage terminator
  } else {
    if (stm) os << stm->GetMessage();  // Output this message to the ostream
  }
  return os;
}

static const char defaultMessType = 'I';
static char emptyString[] = "";
static char oOpt       [] = "O";
// static char otsOpt     [] = "OTS";
static char eOpt       [] = "E";

#ifdef __linux__
static int sMessLength;
static const int maxLOMP = 65536;
static const int maxLOMP1 = (maxLOMP-1);
static int lastLOMP = 0;
static char* listOfMessPtrs[maxLOMP];
static size_t listOfMessLens[maxLOMP];
#endif

StMessMgr* StMessageManager::mInstance = 0;

//
// C++ routines:
//_____________________________________________________________________________
StMessageManager::StMessageManager() : StMessMgr()
{
//
// Constructor - only called once when the library is loaded to
// instantiate the global message manager.
//
//  assert(0);
  messTypeList=0;     
  messCounter =0;
  curType     = new char[ 2];  curType[0] = 0; curType[1] = 0;
  curOpt      = new char[32];  curOpt [0] = 0; 
  building    =0;
  remember    =0;
  gMessMgr = (StMessMgr*) this;
  messTypeList = StMessTypeList::Instance();
  messCounter = StMessageCounter::Instance();
  messCounter->AddType(emptyString);
  // First messVec on collection is list of all messages
  messCollection.push_back(&messList);
  AddType("I","Info");
  AddType("W","Warning");
  AddType("E","Error");
  AddType("D","Debug");
  AddType("Q","QAInfo");
  AddType("U","UCMInfo");
  SwitchOff("D");
#ifdef __linux__
  memset(listOfMessPtrs,0,(maxLOMP * sizeof(char*)));
#endif
  // Initialize buffer with 1024 bytes
  *this << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64
        << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64;
}
//_____________________________________________________________________________
StMessageManager::~StMessageManager() {
//
// Destructor - must delete the message lists
//
  messVecIter current;
  for (current=messList.begin(); current!=messList.end(); current++)
    delete (*current);
  for (size_t i=1; i<messCollection.size(); i++)
    delete (messCollection[i]);
  myout << "WARNING!!! DELETING StMessageManager!" << endl;
  gMessMgr = 0;
}
//_____________________________________________________________________________
StMessMgr* StMessageManager::Instance() {
//
// Allows anyone to get a pointer to the single message manager:
//   StMessageManager::Instance()->(member function)
//
  if (gMessMgr) return gMessMgr;
  if (!mInstance) {
    mInstance = (StMessMgr*) new StMessageManager;
  }
  return mInstance;
}
//_____________________________________________________________________________
ostrstream& StMessageManager::Message(const char* mess, const char* type,
  const char* opt,const char *,int) {
//
// Message declarator - creates a new message if mess is not empty,
// otherwise, prepares for << input.
//
  if (!opt) opt = "";
  size_t messSize = strlen(mess);
  if (messSize) {
#ifdef __linux__
    if (sMessLength == 1) {                // Check for re-used mess ptr
      for (int i=lastLOMP; i>=0; i--) {
        if (mess == listOfMessPtrs[i]) {   // mess ptr found
          (listOfMessPtrs[i])[(listOfMessLens[i]-1)]=0;
          lastLOMP = i;
          break;
        }
        if (i==0) {                        // mess ptr not found
          if (lastLOMP==maxLOMP1) {
            BuildMessage("StMessageManager - maximum depth reached on ptr list",
               "W","E");
          } else {
            lastLOMP++;
            listOfMessPtrs[lastLOMP] = const_cast<char*> (mess);
            listOfMessLens[lastLOMP] = messSize;
          }
        }
      }
    }
#endif
    BuildMessage(mess, type, opt);     // comes back with curType=0
  } else {
    building = 1;
    *curType = *type;
    strcpy(curOpt,opt);
    seekp(0);
  }
  return *((StMessMgr*) this);
}
//_____________________________________________________________________________
void StMessageManager::BuildMessage(const char* mess, const char* type,
  const char* opt) {
//
// Instantiate an StMessage and place on the lists of known messages
//
  if (!opt) opt = "";
  if (!(*type)) {
    static char dash = '-';                   // if no type supplied,
    const char* cptr = strchr(mess,dash);     // search for type within message
    if (cptr)
      *curType = *(++cptr);
    else
      *curType = defaultMessType;
  } else {
    if (!building) *curType = *type;
  }
  if (!(opt[0])) {
    if ((*type == 'E') || (*type == 'W'))     // Error and Warning messages
      strcpy(curOpt,eOpt);                    // default to stderr,
    else                                      // otherwise
      strcpy(curOpt,oOpt);                    // default to stdout
  } else
    if (!building) strcpy(curOpt,opt);
  int typeN = messTypeList->FindTypeNum(curType);
  if (!typeN) {
    *curType = defaultMessType;               // default type is Info
    typeN = 1;                                // type number for Info is 1
  }
  if ((!remember) || strchr(curOpt,'-')) {
    StMessage tmp(mess, curType, curOpt);
    gMessage = endm;
  } else {
  gMessage = new StMessage(mess, curType, curOpt);
#ifndef i386_redhat60
    messList.push_back(gMessage);             // add to message lists
    messCollection[typeN]->push_back(gMessage);
#endif
    endm = gMessage;
  }
}
//_____________________________________________________________________________
void StMessageManager::Print() {
//
// Empty the buffer into the current message and print it.
// If not currenty building a message, print the last one created.
//
  if (building) {
    *this << std::ends;

    if (fail()) {                   // Check to see if input has failed
      int pos0 = tellp();
      int pos1 = pos0 - 160;
      clear();
      if (pos1 >= 0) {
        seekp(pos1);
        *this << "\n\n WARNING!!!  ATTEMPT TO INPUT PAST END OF BUFFER"
          << " IN StMessageManager!\n Buffer Size = " << pos0
          << ".  IGNORING REMAINDER OF MESSAGE...";
      } else {
        seekp(0);
        myerr << "StMessage: ERROR!!! StMessageManager BUFFER TOO SMALL!"
          << endl;
      }
      *this << std::ends;
    }

    BuildMessage(str(), curType, curOpt);
    building = 0;

  } else {

    if (gMessage) {
      gMessage->Print(-1);
    } else {
      myout << "No current message." << endl;
    }
  }
}
//_____________________________________________________________________________
int StMessageManager::PrintList(messVec* list) {
//
// Print a list of messages from a messVec list.
//
  messVecIter current;
  int i=0;
  for (current=list->begin(); current!=list->end(); current++)
    {(*current)->Print(-1); i++;}
  return i;
}
//_____________________________________________________________________________
messVecIter StMessageManager::FindMessageIter(const char* s1, const char* s2,
               const char* s3, const char* s4, messVec* list) {
//
// Obtain an iterator for a particular StMessage on a list of messages.
// First message which contains matches to the strings s1,s2,s3,s4 is returned.
//
  if (!list) list = &messList;
  messVecIter current;
  const char* curMess;
  for (current=list->begin(); current!=list->end(); current++) {
    curMess = (*current)->GetMessage();
    if ((strstr(curMess,s1)) && (strstr(curMess,s2)) &&
        (strstr(curMess,s3)) && (strstr(curMess,s4))) return current;
  }
  return list->end();
}
//_____________________________________________________________________________
StMessage* StMessageManager::FindMessage(const char* s1, const char* s2,
               const char* s3, const char* s4, messVec* list) {
//
// Obtain a pointer to a particular StMessage on a list of messages.
// First message which contains matches to the strings s1,s2,s3,s4 is returned.
//
  messVecIter current = FindMessageIter(s1,s2,s3,s4,list);
  return  (current!=list->end()) ? (*current) : 0 ;
}
//_____________________________________________________________________________
messVec* StMessageManager::FindMessageList(const char* s1, const char* s2,
               const char* s3, const char* s4, messVec* list) {
//
// Obtain a list (messVec) of StMessage pointers for all messages
// which contain matches to the strings s1,s2,s3,s4.
//
  size_t s1_len = strlen(s1);
  if ((s1_len==1) && (!list)) {
    int typeN = messTypeList->FindTypeNum(s1);
    if (typeN) {
      list = messCollection[typeN];
      s1_len = 0;
    }
  }
  if (!list) list = &messList;
  if (!((s1_len) || (strlen(s2)) || (strlen(s3)) || (strlen(s4))))
    return list;
  messVec* newList = new messVec();
  messVecIter current;
  const char* curMess;
  for (current=list->begin(); current!=list->end(); current++) {
    curMess = (*current)->GetMessage();
    if ((strstr(curMess,s1)) && (strstr(curMess,s2)) &&
        (strstr(curMess,s3)) && (strstr(curMess,s4)))
      newList->push_back(*current);
  }
  return newList;
}
//_____________________________________________________________________________
int StMessageManager::RemoveMessage(StMessage* mess) {
//
// Delete a message and remove it from all lists.
//
  if (!mess) return 3;
  const char* curMess = mess->GetMessage();
  messVecIter current = FindMessageIter(curMess);
  if (current==messList.end()) return 1;
  messList.erase(current);
  int typeN = messTypeList->FindTypeNum(mess->GetType());
  current = FindMessageIter(curMess,emptyString,emptyString,
                                        emptyString,messCollection[typeN]);
  if (current==messCollection[typeN]->end()) return 2;
  messCollection[typeN]->erase(current);
  delete mess;
  if (mess==gMessage) gMessage = 0;
  return 0;
}
//_____________________________________________________________________________
void StMessageManager::Summary(size_t nTerms) {
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
  const size_t mmax = 81;
  size_t max = mmax - 12;
  size_t messmax = mmax - 1;
  size_t nMess = messList.size();
  intVector done;
  typedef StVector(char*) CharPtrVec;
  CharPtrVec mType;
  CharPtrVec messBlocks;
  StVector(CharPtrVec) toks;
  size_t i;
  size_t j;
  size_t k;
  int agree;
  char* temp;
  myout << "  ***** StMessageManager message summary *****" << endl;
  toks.resize(nMess);
  for (i=0; i<nMess; i++) {
    done.push_back(0);
    temp = const_cast<char*> (messList[i]->GetType());
    mType.push_back(temp);
    messBlocks.push_back(new char[mmax]);
    temp = strncpy(messBlocks[i],(messList[i]->GetMessage()),messmax);
    temp = strtok(temp, " ");
    toks[i].push_back(temp);
    while (temp != NULL) {
      temp = strtok(NULL, " ");
      toks[i].push_back(temp);
    }
    for (j=toks[i].size(); j<nTerms; j++) toks[i].push_back(temp);
  }
  for (i=0; i<nMess; i++) {
    int count = 1;
    if (!(done[i])) {
      for (j=(i+1); j<nMess; j++) {
        if ((*(mType[i]))==(*(mType[j]))) {
          agree = 1;
          for (k=0; k<nTerms; k++) {
            if (toks[i][k] != NULL) {
              if ((toks[j][k] == NULL) ||
                        strcmp(toks[i][k],toks[j][k])) agree = 0;
            }
            else if (toks[j][k] != NULL) agree = 0;
          }
          if (agree) {
            done[j] = 1;
            count++;
          }
        }
      }
      done[i] = 1;
      for (j = messList[i]->Print(max); j<max; j++) myout << ".";
      myout << "..";
      seekp(0);
      *this << count << std::ends;
      if (tellp() > 6) {
        myout << ">999999";
      } else {
        for (j=tellp(); j<6; j++) myout << ".";
        myout << " " << count << endl;
      }
    }
    mType[i] = NULL;
    memset(messBlocks[i],0,mmax);
    delete [] messBlocks[i];
  }
  MemorySummary();
  return;
}
//_____________________________________________________________________________
void StMessageManager::MemorySummary() {
//
// Output a summary of the memory usage of the message manager so far.
// Loops over all stored messages to calculate message sizes and overhead.
//
  unsigned int gsize=0;
  size_t nMess = messList.size();
  for (size_t i=0; i<nMess; i++) {
    gsize += (messList[i]->GetMemoryUsage() + 2*sizeof(messList[i]));
  }
  printf("  ***** StMessageManager memory usage = %u bytes (%u kb) *****\n",
                      gsize,(gsize/1024));
  return;
}
//_____________________________________________________________________________
int StMessageManager::AddType(const char* type, const char* text) {
//
// Add an additional message type. ListTypes() will return a list of all
// currently defined message types. AddType returns a integer which is zero
// if type has already been defined, otherwise returns the number of types
// defined (including the new one).
//   type - a single character string which will represent the message type.
//   text - a short charact string which will be printed with all messages
//          of this type.
//
  int typeN = messTypeList->AddType(type,text);
  if (typeN) {
    messVec* temp = new messVec();   // Add a new messVec for each message type
    messCollection.push_back(temp);
    messCounter->AddType(type);
  }
  return typeN;
}
//_____________________________________________________________________________
void StMessageManager::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMessageManager.cxx,v 1.51 2016/06/14 06:25:45 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}
//_____________________________________________________________________________
void StMessageManager::SetLevel(Int_t)
{ 
   fprintf(stderr,"StMessageManager class provides no implementation SetLevel method\n");
} 
//_____________________________________________________________________________
Int_t StMessageManager::GetLevel(Int_t) const
{   
   fprintf(stderr,"StMessageManager class provides no implementation GetLevel method\n");
   return -999;
} 
//_____________________________________________________________________________
const char *StMessageManager::GetName() const
{   
   fprintf(stderr,"StMessageManager class provides no implementation GetName method\n");
   return 0;
} 


// Instantiate the (singleton) class upon loading
//
static StMessMgr* temp=StMessageManager::Instance();
// StMessMgr& gMess = (*temp);


//_____________________________________________________________________________
// $Id: StMessageManager.cxx,v 1.51 2016/06/14 06:25:45 genevb Exp $
// $Log: StMessageManager.cxx,v $
// Revision 1.51  2016/06/14 06:25:45  genevb
// Infrequently used memory leak (Coverity)
//
// Revision 1.50  2015/05/21 21:22:40  genevb
// Fix memory leak: unnecessary char array in FindMessageList()
//
// Revision 1.49  2012/06/11 15:05:34  fisyak
// std namespace
//
// Revision 1.48  2009/06/22 22:36:01  fine
// Add the new dedicated UCM logger, It should force the recompilation of many STAR packages
//
// Revision 1.47  2008/05/15 23:40:24  fine
// Change the abstarct class return type to separate the different STAR streams
//
// Revision 1.46  2007/01/25 06:28:06  fine
// connect Logger and Maker debug levels
//
// Revision 1.45  2007/01/25 06:11:37  fine
// Add the new StMess abstarct interfaces GetLevel/SetLevel
//
// Revision 1.44  2005/02/04 21:35:09  genevb
// Fixed bug with remember from v. 1.43 (used pointer before assignment)
//
// Revision 1.43  2004/09/25 03:01:04  perev
// Improved correction with remember messages
//
// Revision 1.42  2004/09/16 02:25:54  perev
// typo fixed, memory leak off
//
// Revision 1.41  2004/04/15 21:28:02  fine
// Remove the redundant StMessageManager RootCint dictionary. User shoudl use the base StMessMgr class anyway
//
// Revision 1.40  2004/04/15 16:03:38  fine
// move StMessMgr class to St_base and change the interface
//
// Revision 1.39  2003/10/28 20:58:32  perev
//  Linux ==> __linux__
//
// Revision 1.38  2003/10/10 23:28:43  genevb
// Trust line length argument from Fortran, works better than algorithm now
//
// Revision 1.37  2003/10/01 20:06:50  genevb
// Initialize and test ostrstream buffer sizes (support for gcc before 3.2)
//
// Revision 1.36  2003/09/25 21:19:22  genevb
// Some new cout-like functions and friend functions, some doxygen-ization
//
// Revision 1.35  2003/09/24 22:02:48  perev
// Back to Gene solution of operator<<
//
// Revision 1.34  2003/09/22 01:30:41  perev
//  some cleanup
//
// Revision 1.33  2003/09/02 17:59:20  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.32  2001/05/14 20:53:20  genevb
// Add features to examine memory use, switch from TDatime to time_t
//
// Revision 1.31  2000/05/23 19:03:38  genevb
// Correct interface for MessageOut(), update docs
//
// Revision 1.30  2000/03/01 05:54:59  genevb
// Further refinements to FORTRAN routines
//
// Revision 1.29  2000/02/29 16:41:57  genevb
// Fortran-compliant interface
//
// Revision 1.28  2000/01/05 19:53:46  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
// Revision 1.27  1999/12/07 19:47:02  genevb
// Increased length of mess ptr list for __linux__
//
// Revision 1.26  1999/10/28 16:06:58  genevb
// Fixed bug in C msg_enable routine - same as earlier fix for StMessage routines
//
// Revision 1.25  1999/09/16 15:50:25  genevb
// Fixed a bug in over-writing memory when calling from FORTRAN, use char=0 instead of strcpy
//
// Revision 1.24  1999/09/14 16:57:56  genevb
// Forgot to remove a debug print statement
//
// Revision 1.23  1999/09/14 15:42:03  genevb
// Some bug fixes, workaround for nulls in strings
//
// Revision 1.22  1999/09/10 21:05:55  genevb
// Some workarounds for RedHat6.0
//
// Revision 1.21  1999/08/12 22:34:28  genevb
// Additional crash protection for __linux__ when omitting parameter strings (opt=strlen(mess))
//
// Revision 1.20  1999/08/10 22:07:35  genevb
// Added QAInfo message types
//
// Revision 1.19  1999/07/25 05:27:45  genevb
// Better protection against empty option strings in FORTRAN
//
// Revision 1.18  1999/07/23 16:56:40  genevb
// Fix extern C prototypes, default options for omitted types, __linux__ bug with multi-line messages
//
// Revision 1.17  1999/07/22 00:19:31  genevb
// Add MessageOut(), fix __linux__ bugs with character array lengths passed from FORTRAN
//
// Revision 1.16  1999/07/17 00:23:24  genevb
// Fixed bug when option fields are empty in FORTRAN, and let type limits be set before types are even added
//
// Revision 1.15  1999/07/08 22:58:18  genevb
// Created an abstract interface with StMessMgr.h hiding template implementation from others, a few other small fixes
//
// Revision 1.14  1999/07/01 23:32:52  genevb
// Change default message typing
//
// Revision 1.13  1999/07/01 15:58:44  genevb
// Fixed __linux__ crash with Summary
//
// Revision 1.12  1999/07/01 01:24:46  genevb
// Fixed FORTRAN character string bug on __linux__, removed a memory leak from Summary()
//
// Revision 1.11  1999/06/30 17:24:50  genevb
// Better limit management, remove Bool_t
//
// Revision 1.10  1999/06/30 04:18:45  genevb
// Fixes: summary wrap-around, unsigned ints, last character of message, <> for time; no KNOWN remaining bugs
//
// Revision 1.9  1999/06/29 23:32:42  genevb
// Handle multi-line calls to fortran routines better
//
// Revision 1.8  1999/06/29 17:37:31  genevb
// Lots of fixes...
//
// Revision 1.7  1999/06/28 15:42:12  genevb
// Added Debug message class
//
// Revision 1.6  1999/06/28 02:40:56  genevb
// Additional backward compatibilit with MSG (msg_enable, msg_enabled, msg_disable
//
// Revision 1.5  1999/06/26 00:24:53  genevb
// Fixed const type mismatches
//
// Revision 1.4  1999/06/25 22:57:58  genevb
// Fixed a small bug in MSG compatibiliti
//
// Revision 1.3  1999/06/24 23:23:58  genevb
// Added message call for compatibility with old fortran code
//
// Revision 1.2  1999/06/24 16:30:42  genevb
// Fixed some memory leaks
//
// Revision 1.1  1999/06/23 15:17:52  genevb
// Introduction of StMessageManager
//
