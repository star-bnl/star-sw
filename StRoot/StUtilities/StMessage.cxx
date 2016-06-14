//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessage                                                            //
//                                                                      //
// This is the class of messages used by StMessageManager in STAR.      //
// Messages have a type and message specified at instantiation,         //
// and also include a time-date stamp and options for printing.         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifdef __ROOT__
#include "TROOT.h"
#endif
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include "StMessage.h"
#include "StMessageCounter.h"
#include "Stsstream.h"
#include "StMessageStream.h"
using namespace std;
static StMessageCounter* messCounter = StMessageCounter::Instance();

static ostrstream messBuffer;
static char space = ' ';
static char tab = '\t';

int StMessage::repeats=1;
static ostrstream lastMessBuffer;

#ifdef __ROOT__
ClassImp(StMessage)
#endif

//_____________________________________________________________________________
StMessage::StMessage(const char *mess, const char *ty, const char* opt) {
  time(&messTime);
  *type = *ty;
  type[1] = 0;
  SetOptions(opt);
//  location = "Unknown";
//  runNumber = 0;
  int len = strlen(mess);
  while (mess[--len] == space) {}     // remove trailing spaces
  message = new char[(++len + 1)];
  for (int i=0;i<len;i++) {
    message[i]=mess[i];
    if (mess[i]=='\n') 		continue;
    if (mess[i]=='\t') 		continue;
    if (!iscntrl(mess[i]))	continue;
    assert(0);
  }
  message[len]=0;
  Print(0);
}
//_____________________________________________________________________________
StMessage::~StMessage() {
  delete [] message;
}
//_____________________________________________________________________________
int StMessage::Print(int nChars) {
//_nChars_:___mode__:_printing_
//   <0   : view    : as intended, no limits, use options
//   =0   : normal  : as intended, use limits, use options
//   >0   : summary : stdout upto nChars/first endofline, no limits, no options
  static const char* leader = "St";
  static const char* insert0 = "";
  static const char* insert1 = ": ";
  static const char* insert2 = " <";
  static const char* insert3 = ">";
  static const char* endofline = "\n";
  int printIt=1;
  if (!nChars) {
    printIt = messCounter->CheckLimit(message,type);
  }
  messBuffer.seekp(0);
  if (printIt) {
    const char* insert;
    if (!(option & kMessOptP)) {
      if (!(option & kMessOptS)) messBuffer << leader;   // "No St" option
      const char* temp(StMessTypeList::Instance()->Text(type));
      if (temp) messBuffer << temp;
      insert = insert1;
    } else insert = insert0;
    if ((nChars != 0) && (strchr(message,tab))) {        // If nChars, replace
      char* message2 = new char[strlen(message)+1];      // tabs with spaces
      char* m0 = message;
      char* m1 = message2;
      while (*m0) {                                      // Go to end of string
        if (*m0==tab) *m1 = space;                       // tab => space,
        else *m1 = *m0;                                  // otherwise copy
        m0++; m1++;
      }
      *m1 = 0; 
      messBuffer << insert << message2;                  // ": ",message
      delete [] message2;
    } else {
      messBuffer << insert << message;                   // ": ",message
    }
    if (nChars<=0) {
      if (option & kMessOptT) {                          // "time" option
        char* temp2 = (ctime(&messTime) + 4);            // First 4 are day
        *(temp2 + 20) = 0;                               // 24th is end-line
        messBuffer << insert2 << temp2 << insert3 ;      // " (",time,")"
      }
      messBuffer << endofline;                           // "\n" end-line
    }
  }
  const char* addedMessage=0;
  if (nChars == 0) {
    addedMessage = messCounter->str();                   // Any limit message
  } else {
    if (nChars>0) {
      if (messBuffer.tellp() >= nChars)
        messBuffer.seekp(nChars-1);   // set end-of-string at nChars
      int noReturns = strcspn(messBuffer.str(),endofline);
      if (noReturns < messBuffer.tellp()) messBuffer.seekp(noReturns);
    } else
      nChars = 0;
  }
  messBuffer << ends;
  if (!repeats) {
    if (!strcmp(messBuffer.str(),lastMessBuffer.str())) {
      return messBuffer.tellp();
    } else {
      lastMessBuffer.seekp(0);
      lastMessBuffer << messBuffer.str() << ends;
    }
  }
  if ((option & kMessOptO) || (nChars != 0)) {
    myout << messBuffer.str();
    if (addedMessage) myout << addedMessage;
    myout.flush();
  }
  if ((option & kMessOptE) && (nChars == 0)) {
    myerr << messBuffer.str();
    if (addedMessage) myerr << addedMessage;
    myerr.flush();
  }
  return messBuffer.tellp();
}
//_____________________________________________________________________________
char* StMessage::GetOptions() const {
  static char optStr[32]; // Use single, static instance to avoid memory leak
  char* sPtr = optStr;
  if (option & kMessOptO) sprintf(sPtr++,"%c",'O');
  if (option & kMessOptE) sprintf(sPtr++,"%c",'E');
  if (option & kMessOptS) sprintf(sPtr++,"%c",'S');
  if (option & kMessOptT) sprintf(sPtr++,"%c",'T');
  if (option & kMessOptP) sprintf(sPtr++,"%c",'P');
  if (option & kMessOptDash) sprintf(sPtr++,"%c",'-');
  (*sPtr) = 0; // Terminate string
  return optStr;
}
//_____________________________________________________________________________
void StMessage::SetOptions(const char* opt) {
  option = kMessOptNone;
  if (!opt) return;
  int len = strlen(opt);
  while (len--) {
    switch (toupper(opt[len])) {
      case ('O') : { option |= kMessOptO; break; }
      case ('E') : { option |= kMessOptE; break; }
      case ('S') : { option |= kMessOptS; break; }
      case ('T') : { option |= kMessOptT; break; }
      case ('P') : { option |= kMessOptP; break; }
      case ('-') : { option |= kMessOptDash; break; }
      default : {}
    }
  }
}
//_____________________________________________________________________________
size_t StMessage::GetMemoryUsage() {
  size_t msize = strlen(message) + 1;
  msize += sizeof(*this);                   // Determine overhead
  return msize;
}
//_____________________________________________________________________________
void StMessage::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMessage.cxx,v 1.29 2016/06/14 06:24:54 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}
//_____________________________________________________________________________
int StMessage::InitBuffer() {
  // Initialize buffer with 1088 bytes.
  messBuffer << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64
     << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64 << ch64;
  return messBuffer.tellp();
}

int tmpp = StMessage::InitBuffer();

//_____________________________________________________________________________
// $Id: StMessage.cxx,v 1.29 2016/06/14 06:24:54 genevb Exp $
// $Log: StMessage.cxx,v $
// Revision 1.29  2016/06/14 06:24:54  genevb
// Very old cut-and-paste typo (Coverity)
//
// Revision 1.28  2012/06/11 15:05:34  fisyak
// std namespace
//
// Revision 1.27  2004/04/02 22:17:14  genevb
// Added protected Ignore/AllowRepeats() for friend StBFChain class
//
// Revision 1.26  2004/03/01 17:52:13  fisyak
// Add include for assert (osf required)
//
// Revision 1.25  2004/01/28 00:09:14  genevb
// Messages (except Debug) default to no time-date stamp
//
// Revision 1.24  2003/10/01 20:06:50  genevb
// Initialize and test ostrstream buffer sizes (support for gcc before 3.2)
//
// Revision 1.23  2003/09/25 21:18:14  genevb
// Changed option storage
//
// Revision 1.22  2003/09/22 01:30:41  perev
//  some cleanup
//
// Revision 1.21  2003/09/02 17:59:20  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.20  2001/05/14 20:53:20  genevb
// Add features to examine memory use, switch from TDatime to time_t
//
// Revision 1.19  2000/01/05 19:53:46  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
// Revision 1.18  1999/12/28 21:29:55  porter
// added 'using std::vector' and a (char*) cast to allow compilation
// using solaris CC5.  Many warnings of const char* vs char* still
// exist but will await Gene's return to fix these
//
// Revision 1.17  1999/09/16 15:50:25  genevb
// Fixed a bug in over-writing memory when calling from FORTRAN, use char=0 instead of strcpy
//
// Revision 1.16  1999/09/14 15:42:03  genevb
// Some bug fixes, workaround for nulls in strings
//
// Revision 1.15  1999/09/11 23:12:23  fisyak
// Add cast for HP
//
// Revision 1.14  1999/09/10 21:05:55  genevb
// Some workarounds for RedHat6.0
//
// Revision 1.13  1999/08/10 22:07:35  genevb
// Added QAInfo message types
//
// Revision 1.12  1999/07/22 00:17:47  genevb
// make messBuffer static
//
// Revision 1.11  1999/07/15 05:15:06  genevb
// Fixed an odd bug with seekp(0) on an empty stream buffer
//
// Revision 1.10  1999/07/08 22:58:18  genevb
// Created an abstract interface with StMessMgr.h hiding template implementation from others, a few other small fixes
//
// Revision 1.9  1999/07/01 01:24:46  genevb
// Fixed FORTRAN character string bug on linux, removed a memory leak from Summary()
//
// Revision 1.7  1999/06/30 04:18:45  genevb
// Fixes: summary wrap-around, unsigned ints, last character of message, <> for time; no KNOWN remaining bugs
//
// Revision 1.6  1999/06/29 23:32:41  genevb
// Handle multi-line calls to fortran routines better
//
// Revision 1.5  1999/06/29 19:17:14  genevb
// Lots of fixes..
//
// Revision 1.3  1999/06/26 00:24:52  genevb
// Fixed const type mismatches
//
// Revision 1.2  1999/06/24 16:30:41  genevb
// Fixed some memory leaks
//
// Revision 1.1  1999/06/23 15:17:46  genevb
// Introduction of StMessageManager
//
// Revision 1.0  1999/01/27 10:28:29  genevb
//
