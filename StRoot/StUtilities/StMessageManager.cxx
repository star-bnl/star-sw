// $Id: StMessageManager.cxx,v 1.30 2000/03/01 05:54:59 genevb Exp $
// $Log: StMessageManager.cxx,v $
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
// Increased length of mess ptr list for Linux
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
static const char defaultMessType = 'I';
static char emptyString[] = "";
static char oOpt[] = "O";
static char otsOpt[] = "OTS";
static char eOpt[] = "E";
static char nullMess[] = "Null message!!!";
#ifdef LINUX
static int sMessLength;
static const int maxLOMP = 65536;
static const int maxLOMP1 = (maxLOMP-1);
static int lastLOMP = 0;
static char* listOfMessPtrs[maxLOMP];
static size_t listOfMessLens[maxLOMP];
#endif

StMessMgr* StMessageManager::mInstance = 0;
//
// C and Fortran routines:
//________________________________________
void type_of_call Message_(const char* mess, int* lines, int*, size_t len) {
  static char space = ' ';
  static const char* messReturnChar = "\n";
  size_t messSize = strlen(mess);
  char* mess2=const_cast<char*> (mess);
  int del_mess = 0;
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
    char* mess1 = const_cast<char*> (mess);
    mess2 = new char[messSize];    // Build a new version of the
    *mess2 = 0;                          // message with trailing spaces
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
    del_mess = 1;
  } else {
#ifdef LINUX
    sMessLength = len;
#endif
    if ((len>1) && (messSize > len)) {
      mess2 = new char[(len+1)];
      strncpy(mess2,mess,len);
      mess2[len]=0;
      del_mess = 1;
    }
  }
  gMessMgr->Message(mess2);
  if (del_mess) delete [] mess2;
}
//________________________________________
void type_of_call Msg_Enable_(const char* mess, size_t len) {
  size_t messlen = strlen(mess);
  if ((len>1) && (messlen > len)) {
    char* mess2 = new char[(len+1)];
    strncpy(mess2,mess,len);
    mess2[len] = 0;
    gMessMgr->SwitchOn(mess2);
    delete [] mess2;
  } else {
    gMessMgr->SwitchOn(mess);
  }
}
//________________________________________
int type_of_call Msg_Enabled_(const char* mess, int*, size_t len) {
  size_t messlen = strlen(mess);
  int ret_val = 1;
  if ((len>1) && (messlen > len)) {
    char* mess2 = new char[(len+1)];
    strncpy(mess2,mess,len);
    mess2[len] = 0;
    if ((gMessMgr->GetLimit(mess2))==0) ret_val = 0;
    delete [] mess2;
  } else {
    if ((gMessMgr->GetLimit(mess))==0) ret_val = 0;
  }
  return ret_val;
}
//________________________________________
void type_of_call Msg_Disable_(const char* mess, size_t len) {
  size_t messlen = strlen(mess);
  if ((len>1) && (messlen > len)) {
    char* mess2 = new char[(len+1)];
    strncpy(mess2,mess,len);
    mess2[len] = 0;
    gMessMgr->SwitchOff(mess2);
    delete [] mess2;
  } else {
    gMessMgr->SwitchOff(mess);
  }
}
//________________________________________
void type_of_call MessageOut( const char *msg, int* lines, size_t len ) {
  Message_(msg,lines,0,len);
}
//________________________________________
void type_of_call StCaller(const char* mess, const char* typString,
                           const char* opt, size_t len) {
#ifdef LINUX
  sMessLength = len;
#endif
  if (mess[0]==0) {
    gMessMgr->Message(nullMess,"E",eOpt);
    return;
  }

  size_t messlen = strlen(mess);
  if ((len>1) && (messlen > len)) messlen = len;
  char* mess2 = new char[(messlen+1)];
  strncpy(mess2,mess,messlen);
  mess2[messlen] = 0;
  
  gMessMgr->Message(mess2,typString,opt);
  delete [] mess2;
}
//________________________________________
void type_of_call StCallerOpt(const char* mess, const char* typString,
                              const char* opt, size_t len1, size_t len2,
                              char* optString) {
  char* opt2=const_cast<char*> (opt);
  int del_opt=0;

  if (len2<=0) {
    opt2=optString;
  } else if (strlen(opt) > len2) {
    opt2 = new char[(len2+1)];
    strncpy(opt2,opt,len2);
    opt2[len2]=0;
    del_opt = 1;
  }

  StCaller(mess,typString,opt2,len1);
  if (del_opt) delete [] opt2;
}
//________________________________________
void type_of_call StMessage_(const char* mess, const char* type,
                             const char* opt, size_t len1,
			     size_t len2, size_t len3) {
  char* type2=const_cast<char*> (type);
  int del_type=0;

  if (len2<=0) {
    type2=emptyString;
  } else if (strlen(type) > len2) {
    type2 = new char[(len2+1)];
    strncpy(type2,type,len2);
    type2[len2]=0;
    del_type = 1;
  }

  StCallerOpt(mess,type2,opt,len1,len3,oOpt);
  if (del_type) delete [] type2;
}
//________________________________________
void type_of_call StInfo_(const char* mess, size_t len) {
  StCaller(mess,"I",oOpt,len);
}
//________________________________________
void type_of_call StWarning_(const char* mess, size_t len) {
  StCaller(mess,"W",eOpt,len);
}
//________________________________________
void type_of_call StError_(const char* mess, size_t len) {
  StCaller(mess,"E",eOpt,len);
}
//________________________________________
void type_of_call StDebug_(const char* mess, size_t len) {
  StCaller(mess,"D",oOpt,len);
}
//________________________________________
void type_of_call QAInfo_(const char* mess, size_t len) {
  StCaller(mess,"Q",otsOpt,len);
}
//________________________________________
void type_of_call StInfoOpt_(const char* mess, const char* opt,
                             size_t len1, size_t len2) {
  StCallerOpt(mess,"I",opt,len1,len2,oOpt);
}
//________________________________________
void type_of_call StWarningOpt_(const char* mess, const char* opt,
                                size_t len1, size_t len2) {
  StCallerOpt(mess,"W",opt,len1,len2,eOpt);
}
//________________________________________
void type_of_call StErrorOpt_(const char* mess, const char* opt,
                              size_t len1, size_t len2) {
  StCallerOpt(mess,"E",opt,len1,len2,eOpt);
}
//________________________________________
void type_of_call StDebugOpt_(const char* mess, const char* opt,
                              size_t len1, size_t len2) {
  StCallerOpt(mess,"D",opt,len1,len2,oOpt);
}
//________________________________________
void type_of_call QAInfoOpt_(const char* mess, const char* opt,
                             size_t len1, size_t len2) {
  StCallerOpt(mess,"Q",opt,len1,len2,otsOpt);
}
//________________________________________
void type_of_call StMessAddType_(const char* type, const char* text,
                                 size_t len1, size_t len2) {
  if (strlen(type) > len1) (const_cast<char*> (type))[len1] = 0;
  if (strlen(text) > len2) (const_cast<char*> (text))[len2] = 0;
  gMessMgr->AddType(type,text);
}

//
// C++ routines:
//_____________________________________________________________________________
#ifdef __ROOT__
ClassImp(StMessageManager)
#endif
//_____________________________________________________________________________
StMessageManager::StMessageManager() : StMessMgr(),
curType(new char[2]),
building(0) {
//
// Constructor - only called once when the library is loaded to
// instantiate the global message manager.
//
  curType[0] = 0;
  curType[1] = 0;
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
StMessMgr& StMessageManager::Message(const char* mess, const char* type,
  const char* opt) {
//
// Message declarator - creates a new message if mess is not empty,
// otherwise, prepares for << input.
//
  size_t messSize = strlen(mess);
  if (messSize) {
#ifdef LINUX
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
    curOpt = const_cast<char*> (opt);
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
  if (!opt) {
    if ((*type == 'E') || (*type == 'W'))     // Error and Warning messages
      curOpt = eOpt;                          // default to stderr,
    else                                      // otherwise
      curOpt = oOpt;                          // default to stdout
  } else
    if (!building) curOpt = const_cast<char*> (opt);
  int typeN = messTypeList->FindTypeNum(curType);
  if (!typeN) {
    *curType = defaultMessType;               // default type is Info
    typeN = 1;                                // type number for Info is 1
  }
  gMessage = new StMessage(mess, curType, curOpt);
  if (strchr(curOpt,'-')) {
    if (gMessage) delete gMessage;
    gMessage = endm;
  } else {
#ifndef i386_redhat60
    messList.push_back(gMessage);
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
    operator<<(ends);
    BuildMessage(str(), curType, curOpt);
    building = 0;
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
  return 0;
}
//_____________________________________________________________________________
StMessage* StMessageManager::FindMessage(const char* s1, const char* s2,
               const char* s3, const char* s4, messVec* list) {
//
// Obtain a pointer to a particular StMessage on a list of messages.
// First message which contains matches to the strings s1,s2,s3,s4 is returned.
//
  messVecIter current = FindMessageIter(s1,s2,s3,s4,list);
  return ( (current) ? (*current) : 0 );
}
//_____________________________________________________________________________
messVec* StMessageManager::FindMessageList(const char* s1, const char* s2,
               const char* s3, const char* s4, messVec* list) {
//
// Obtain a list (messVec) of StMessage pointers for all messages
// which contain matches to the strings s1,s2,s3,s4.
//
  size_t s1_len = strlen(s1);
  char* s1a = new char[(s1_len+1)];
  strcpy(s1a,s1);
  if ((s1_len==1) && (!list)) {
    int typeN = messTypeList->FindTypeNum(s1);
    if (typeN) {
      list = messCollection[typeN];
      *s1a = 0;
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
  printf("* $Id: StMessageManager.cxx,v 1.30 2000/03/01 05:54:59 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}


//
// Instantiate the (singleton) class upon loading
//
StMessMgr* temp=StMessageManager::Instance();
