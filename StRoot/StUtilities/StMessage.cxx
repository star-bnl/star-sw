// $Id: StMessage.cxx,v 1.8 1999/06/30 17:24:49 genevb Exp $
// $Log: StMessage.cxx,v $
// Revision 1.8  1999/06/30 17:24:49  genevb
// Better limit management, remove Bool_t
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
#include "StMessTypeList.h"
#include "TDatime.h"
#include <strstream.h>

static StMessageCounter* messCounter = StMessageCounter::Instance();

ostrstream messBuffer(new char[1024],1024);

#ifdef __ROOT__
ClassImp(StMessage)
#endif

//_____________________________________________________________________________
StMessage::StMessage(char *mess, char *ty, char* opt) :
type(new char(toupper(*ty))),
messTime() {
  static char space = ' ';
  size_t len = strlen(opt);
  option = new char[len];
  while (len--)
    option[len] = toupper(opt[len]);
//  location = "Unknown";
//  runNumber = 0;
  len = strlen(mess);
  while (mess[--len] == space) {}    // remove trailing spaces
  message = new char[(++len + 1)];
  strncpy(message,mess,len);
  strcpy(&(message[len]),"");
  Print();
}
//_____________________________________________________________________________
StMessage::~StMessage() {
}
//_____________________________________________________________________________
int StMessage::Print(int nChars) {
  static const char* leader = "St";
  static const char* insert1 = ": ";
  static const char* insert2 = " <";
  static const char* insert3 = ">";
  static const char* endofline = "\n";
  messBuffer.seekp(0);
  int printIt=1;
  if (!nChars) {
    printIt = messCounter->CheckLimit(message,type);
  }
  if (printIt) {
    messBuffer << leader;
    const char* temp(StMessTypeList::Instance()->Text(type));
    if (temp) messBuffer << temp;
    messBuffer << insert1 << message;                    // ": ",message
    if (nChars<=0) {
      if (!strchr(option,'T')) {
        char* temp2 = strchr(messTime.AsString(),' ');
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
      if (messBuffer.tellp() > nChars)
        messBuffer.seekp(nChars);   // set end-of-string at nChars
      int noReturns = strcspn(messBuffer.str(),endofline);
      if (noReturns < messBuffer.tellp()) messBuffer.seekp(noReturns);
    } else
      nChars = 0;
  }
  messBuffer << ends;
  if ((strchr(option,'O')) || (nChars)) {
    cout << messBuffer.str();
    if (addedMessage) cout << addedMessage;
    cout.flush();
  }
  if ((strchr(option,'E')) && !(nChars)) {
    cerr << messBuffer.str();
    if (addedMessage) cout << addedMessage;
    cerr.flush();
  }
  return messBuffer.tellp();
}
//_____________________________________________________________________________
void StMessage::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMessage.cxx,v 1.8 1999/06/30 17:24:49 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}
