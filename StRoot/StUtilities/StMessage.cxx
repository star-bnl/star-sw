// $Id: StMessage.cxx,v 1.19 2000/01/05 19:53:46 genevb Exp $
// $Log: StMessage.cxx,v $
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
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
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
#include <ctype.h>
#include <string.h>
#include "StMessage.h"
#include "StMessageCounter.h"
#include <strstream.h>

static StMessageCounter* messCounter = StMessageCounter::Instance();

static int messBuffSize = 1024;
char* tempPtr = new char[messBuffSize];
static char* messBuff = strcpy(tempPtr,"Empty\n");
static ostrstream messBuffer(messBuff,messBuffSize,ios::out);
static char space = ' ';
static char tab = '\t';

#ifdef __ROOT__
ClassImp(StMessage)
#endif

//_____________________________________________________________________________
StMessage::StMessage(const char *mess, const char *ty, const char* opt) :
messTime() {
  *type = *ty;
  type[1] = 0;
  size_t len = strlen(opt);
  option = new char[(len+1)];
  option[len] = 0;
  while (len--)
    option[len] = toupper(opt[len]);  // capitalize while copying
//  location = "Unknown";
//  runNumber = 0;
  len = strlen(mess);
  while (mess[--len] == space) {}     // remove trailing spaces
  message = new char[(++len + 1)];
  strncpy(message,mess,len);
  message[len]=0;
  Print(0);
}
//_____________________________________________________________________________
StMessage::~StMessage() {
  delete [] message;
  delete [] option;
}
//_____________________________________________________________________________
int StMessage::Print(int nChars) {
  static const char* leader = "St";
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
    if (!strchr(option,'S')) messBuffer << leader;       // "No St" option
    const char* temp(StMessTypeList::Instance()->Text(type));
    if (temp) messBuffer << temp;
    if ((nChars) && (strchr(message,tab))) {             // If nChars, replace
      char* message2 = new char[strlen(message)+1];      // tabs with spaces
      char* m0 = message;
      char* m1 = message2;
      while (*m0) {                                      // Go to end of string
        if (*m0==tab) *m1 = space;                       // tab => space,
        else *m1 = *m0;                                  // otherwise copy
        m0++; m1++;
      }
      *m1 = 0; 
      messBuffer << insert1 << message2;                 // ": ",message
      delete [] message2;
    } else {
      messBuffer << insert1 << message;                  // ": ",message
    }
    if (nChars<=0) {
      if (!strchr(option,'T')) {                         // "No time" option
        char* temp2 = strchr((char*)messTime.AsString(),' ');
        messBuffer << insert2 << (++temp2) << insert3 ;  // " (",time,")"
      }
      messBuffer << endofline;                           // "\n" end-line
    }
  }
  char* addedMessage=0;
  if (!nChars) {
    addedMessage = messCounter->str();
  } else {
    if (nChars>0) {
      if (messBuffer.tellp() >= nChars)
        messBuffer.seekp(nChars-1);   // set end-of-string at nChars
      int noReturns = strcspn(messBuff,endofline);
      if (noReturns < messBuffer.tellp()) messBuffer.seekp(noReturns);
    } else
      nChars = 0;
  }
  messBuffer << ends;
  if ((strchr(option,'O')) || (nChars)) {
    cout << messBuff;
    if (addedMessage) cout << addedMessage;
    cout.flush();
  }
  if ((strchr(option,'E')) && !(nChars)) {
    cerr << messBuff;
    if (addedMessage) cout << addedMessage;
    cerr.flush();
  }
  return messBuffer.tellp();
}
//_____________________________________________________________________________
void StMessage::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMessage.cxx,v 1.19 2000/01/05 19:53:46 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}
