// $Id: StMessageManager.cxx,v 1.21 1999/08/12 22:34:28 genevb Exp $
// $Log: StMessageManager.cxx,v $
// Revision 1.21  1999/08/12 22:34:28  genevb
// Additional crash protection for Linux when omitting parameter strings (opt=strlen(mess))
//
// Revision 1.20  1999/08/10 22:07:35  genevb
// Added QAInfo message types
//
// Revision 1.19  1999/07/25 05:27:45  genevb
// Better protection against empty option strings in FORTRAN
//
// Revision 1.18  1999/07/23 16:56:40  genevb
// Fix extern C prototypes, default options for omitted types, Linux bug with multi-line messages
//
// Revision 1.17  1999/07/22 00:19:31  genevb
// Add MessageOut(), fix Linux bugs with character array lengths passed from FORTRAN
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
// Fixed linux crash with Summary
//
// Revision 1.12  1999/07/01 01:24:46  genevb
// Fixed FORTRAN character string bug on linux, removed a memory leak from Summary()
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
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessageManager                                                     //
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
#endif

#include "StMessageManager.h"
#include <ctype.h>
#include <string.h>
#ifdef LINUX
#include <math.h>
#endif

StMessMgr* gMessMgr = 0;
StMessage* gMessage=0;
StMessage* endm=0;
ostream& operator<<(ostream& os, StMessage*) {
  gMessMgr->Print();
  return os;
}
static const char defaultMessType[] = "I";
static char emptyString[] = "";
static char oOpt[] = "O";
static char otsOpt[] = "OTS";
static char eOpt[] = "E";
static const size_t oSize = 25;
#ifdef LINUX
static int sMessLength;
static const int maxLOMP = 1024;
static const int maxLOMP1 = (maxLOMP-1);
static char* listOfMessPtrs[maxLOMP];
static size_t listOfMessLens[maxLOMP];
#endif

StMessMgr* StMessageManager::mInstance = 0;
//
// C and Fortran routines:
//________________________________________
void type_of_call Message_(char* mess, int* lines, int*, size_t len) {
  static char space = ' ';
  static const char* messReturnChar = "\n";
  size_t messSize = strlen(mess);
  if (*lines>1) {
#ifdef LINUX
    // Linux sometimes makes messSize too big. This algorithm seems to work.
    float lineSize2 = ((float) messSize)/((float) *lines);
    float lineSize1 = floor(lineSize2 - 0.51);
    if ((lineSize2-lineSize1) <= 1.0) lineSize1++;
    int lineSize = (int) lineSize1;
#else
    int lineSize = messSize/(*lines);
#endif
    char* mess1 = mess;
    char* mess2 = new char[messSize];    // Build a new version of the
    strcpy(mess2,emptyString);           // message with trailing spaces
    for (int i=(*lines); i>0; i--) {     // removed, and \n's inserted.
      int clen = lineSize;
      while (mess1[--clen] == space) {}
      strncat(mess2,mess1,(++clen));
      if (i>1) {
        strcat(mess2,messReturnChar);
        mess1 = &(mess1[lineSize]);
      }
    }
    strcat(mess2,emptyString);
    gMessMgr->Message(mess2);
    delete [] mess2;
  } else {
#ifdef LINUX
    sMessLength = len;
#endif
    if ((len>1) && (messSize > len)) strcpy(&(mess[len]),emptyString);
    gMessMgr->Message(mess);
  }
}
//________________________________________
void type_of_call Msg_Enable_(char* mess, size_t len) {
  if (strlen(mess) > len) strcpy(&(mess[len]),emptyString);
  gMessMgr->SwitchOn(mess);
}
//________________________________________
int type_of_call Msg_Enabled_(char* mess, int*, size_t len) {
  if (strlen(mess) > len) strcpy(&(mess[len]),emptyString);
  if ((gMessMgr->GetLimit(mess))==0) return 0;
  return 1;
}
//________________________________________
void type_of_call Msg_Disable_(char* mess, size_t len) {
  if (strlen(mess) > len) strcpy(&(mess[len]),emptyString);
  gMessMgr->SwitchOff(mess);
}
//________________________________________
void type_of_call StMessage_(char* mess, char* type, char* opt,
                             size_t len1, size_t len2, size_t len3) {
#ifdef LINUX
    sMessLength = len1;
#endif
  size_t messlen = strlen(mess);
  if ((len1>1) && (messlen > len1)) strcpy(&(mess[len1]),emptyString);
  if ((((size_t) type)==messlen) || (len2<=0) || (len2>oSize)) type=emptyString;
  else if (strlen(type) > len2) strcpy(&(type[len2]),emptyString);
  if ((((size_t) opt)==messlen) || (((size_t) opt)==(messlen+1)) ||
      (((size_t) opt)==1) || (len3<=0) || (len3>oSize)) opt=oOpt;
  else if ((opt) && (strlen(opt) > len3)) strcpy(&(opt[len3]),emptyString);
  gMessMgr->Message(mess,type,opt);
}
//________________________________________
void type_of_call StInfo_(char* mess, char* opt, size_t len1, size_t len2) {
#ifdef LINUX
    sMessLength = len1;
#endif
  size_t messlen = strlen(mess);
  if ((len1>1) && (messlen > len1)) strcpy(&(mess[len1]),emptyString);
  if ((((size_t) opt)==messlen) || (len2<=0) || (len2>oSize)) opt=oOpt;
  else if (strlen(opt) > len2) strcpy(&(opt[len2]),emptyString);
  gMessMgr->Message(mess,"I",opt);
}
//________________________________________
void type_of_call StWarning_(char* mess, char* opt, size_t len1, size_t len2) {
#ifdef LINUX
    sMessLength = len1;
#endif
  size_t messlen = strlen(mess);
  if ((len1>1) && (messlen > len1)) strcpy(&(mess[len1]),emptyString);
  if ((((size_t) opt)==messlen) || (len2<=0) || (len2>oSize)) opt=eOpt;
  else if (strlen(opt) > len2) strcpy(&(opt[len2]),emptyString);
  gMessMgr->Message(mess,"W",opt);
}
//________________________________________
void type_of_call StError_(char* mess, char* opt, size_t len1, size_t len2) {
#ifdef LINUX
    sMessLength = len1;
#endif
  size_t messlen = strlen(mess);
  if ((len1>1) && (messlen > len1)) strcpy(&(mess[len1]),emptyString);
  if ((((size_t) opt)==messlen) || (len2<=0) || (len2>oSize)) opt=eOpt;
  else if (strlen(opt) > len2) strcpy(&(opt[len2]),emptyString);
  gMessMgr->Message(mess,"E",opt);
}
//________________________________________
void type_of_call StDebug_(char* mess, char* opt, size_t len1, size_t len2) {
#ifdef LINUX
    sMessLength = len1;
#endif
  size_t messlen = strlen(mess);
  if ((len1>1) && (messlen > len1)) strcpy(&(mess[len1]),emptyString);
  if ((((size_t) opt)==messlen) || (len2<=0) || (len2>oSize)) opt=oOpt;
  else if (strlen(opt) > len2) strcpy(&(opt[len2]),emptyString);
  gMessMgr->Message(mess,"D",opt);
}
//________________________________________
void type_of_call QAInfo_(char* mess, char* opt, size_t len1, size_t len2) {
#ifdef LINUX
    sMessLength = len1;
#endif
  size_t messlen = strlen(mess);
  if ((len1>1) && (messlen > len1)) strcpy(&(mess[len1]),emptyString);
  if ((((size_t) opt)==messlen) || (len2<=0) || (len2>oSize)) opt=otsOpt;
  else if (strlen(opt) > len2) strcpy(&(opt[len2]),emptyString);
  gMessMgr->Message(mess,"Q",opt);
}
//________________________________________
void type_of_call StMessAddType_(const char* type, const char* text,
                                                 size_t len1, size_t len2) {
  if (strlen(type) > len1) strcpy(&((const_cast<char*> (type))[len1]),emptyString);
  if (strlen(text) > len2) strcpy(&((const_cast<char*> (text))[len2]),emptyString);
 gMessMgr->AddType(type,text);
}
//________________________________________
void type_of_call MessageOut( const char *msg ) {
  StMessage_(const_cast<char*> (msg));
}

//
// C++ routines:
//_____________________________________________________________________________
#ifdef __ROOT__
ClassImp(StMessageManager)
#endif
//_____________________________________________________________________________
StMessageManager::StMessageManager() : StMessMgr() {
//
// Constructor - only called once when the library is loaded to
// instantiate the global message manager.
//
  curType = 0;
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
  SwitchOff("D");
#ifdef LINUX
  memset(listOfMessPtrs,0,(maxLOMP * sizeof(char*)));
#endif
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
  cout << "WARNING!!! DELETING StMessageManager!" << endl;
  gMessMgr = 0;
}
//_____________________________________________________________________________
StMessMgr* StMessageManager::Instance() {
//
// Allows anyone to get a pointer to the single message manager:
//   StMessageManager::Instance()->(member function)
//
  if (!mInstance) {
    mInstance = (StMessMgr*) new StMessageManager;
  }
  return mInstance;
}
//_____________________________________________________________________________
StMessMgr& StMessageManager::Message(char* mess, char* type, char* opt) {
//
// Message declarator - creates a new message if mess is not empty,
// otherwise, prepares for << input.
//
  size_t messSize = strlen(mess);
  if (messSize) {
#ifdef LINUX
    if (sMessLength == 1) {                // Check for re-used mess ptr
      for (int i=0; i<maxLOMP; i++) {
        if (!(listOfMessPtrs[i])) {        // mess ptr not found
          listOfMessPtrs[i] = mess;
          listOfMessLens[i] = messSize;
          break;
        }
        if (mess == listOfMessPtrs[i]) {   // mess ptr found
          strcpy(&(mess[(listOfMessLens[i]-1)]),emptyString);
          while ((listOfMessPtrs[++i]) && (i<maxLOMP)) 
            listOfMessPtrs[i] = 0;
          break;
        }
        if (i==maxLOMP1) {
          BuildMessage("StMessageManager - maximum depth reached on ptr list",
             "W","E");
        }
      }
    }
#endif
    BuildMessage(mess, type, opt);     // comes back with curType=0
  } else {
    curType = type;
    curOpt = opt;
    seekp(0);
  }
  return *((StMessMgr*) this);
}
//_____________________________________________________________________________
void StMessageManager::BuildMessage(char* mess, char* type, char* opt) {
//
// Instantiate an StMessage and place on the lists of known messages
//
  if (!(strlen(type))) {
    static char dash = '-';            // if no type supplied,
    char* cptr = strchr(mess,dash);    // search for type within message
    curType = new char[2];
    type = strcpy(curType,defaultMessType);
    if (cptr) curType[0] = cptr[1];
  } else {
    curType = 0;
  }
  if (!opt) {
    if ((*type == 'E') || (*type == 'W'))     // Error and Warning messages
      opt = eOpt;                             // default to stderr,
    else                                      // otherwise
      opt = oOpt;                             // default to stdout
  }
  int typeN = messTypeList->FindTypeNum(type);
  if (!typeN) {
    strcpy(type,defaultMessType);      // default type is Info
    typeN = 1;                         // type number for Info is 1
  }
  gMessage = new StMessage(mess, type, opt);
  endm = gMessage;
  if (!strchr(opt,'-')) {
    messList.push_back(gMessage);
    messCollection[typeN]->push_back(gMessage);
  }
  if (curType) {
    delete [] curType;
    curType = 0;
  }
}
//_____________________________________________________________________________
void StMessageManager::Print() {
//
// Empty the buffer into the current message and print it.
// If not currenty building a message, print the last one created.
//
  if (curType) {  // curType is the type of the current message (0=no message)
    operator<<(ends);
    BuildMessage(str(), curType, curOpt);
    curType = 0;
  } else {
    if (gMessage) {
      gMessage->Print(-1);
    } else {
      cout << "No current message." << endl;
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
messVecIter StMessageManager::FindMessageIter(const char* s1, char* s2,
               char* s3, char* s4, messVec* list) {
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
  return 0;
}
//_____________________________________________________________________________
StMessage* StMessageManager::FindMessage(const char* s1, char* s2,
               char* s3, char* s4, messVec* list) {
//
// Obtain a pointer to a particular StMessage on a list of messages.
// First message which contains matches to the strings s1,s2,s3,s4 is returned.
//
  messVecIter current = FindMessageIter(s1,s2,s3,s4,list);
  return ( (current) ? (*current) : 0 );
}
//_____________________________________________________________________________
messVec* StMessageManager::FindMessageList(const char* s1, char* s2,
               char* s3, char* s4, messVec* list) {
//
// Obtain a list (messVec) of StMessage pointers for all messages
// which contain matches to the strings s1,s2,s3,s4.
//
  char* s1a = new char[strlen(s1)];
  strcpy(s1a,s1);
  if ((strlen(s1)==1) && (!list)) {
    int typeN = messTypeList->FindTypeNum(s1);
    if (typeN) {
      list = messCollection[typeN];
      strcpy(s1a,emptyString);
    }
  }
  if (!list) list = &messList;
  if (!(strlen(s1a) || strlen(s2) || strlen(s3) || strlen(s4)))
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
  if (!current) return 1;
  messList.erase(current);
  int typeN = messTypeList->FindTypeNum(mess->GetType());
  current = FindMessageIter(curMess,emptyString,emptyString,
                                        emptyString,messCollection[typeN]);
  if (!current) return 2;
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
  cout << "  ***** StMessageManager message summary *****" << endl;
  for (i=0; i<nMess; i++) {
    done.push_back(0);
    temp = const_cast<char*> (messList[i]->GetType());
    mType.push_back(temp);
    toks.push_back(*(new CharPtrVec));
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
      for (j = messList[i]->Print(max); j<max; j++) cout << ".";
      cout << "..";
      seekp(0);
      *this << count << ends;
      if (tellp() > 6) {
        cout << ">999999";
      } else {
        for (j=tellp(); j<6; j++) cout << ".";
        cout << " " << count << endl;
      }
    }
    mType[i] = NULL;
    memset(messBlocks[i],0,mmax);
    delete [] messBlocks[i];
  }
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
  printf("* $Id: StMessageManager.cxx,v 1.21 1999/08/12 22:34:28 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}


//
// Instantiate the (singleton) class upon loading
//
StMessMgr* temp=StMessageManager::Instance();
