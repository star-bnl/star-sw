// $Id: StMessageManager.cxx,v 1.3 1999/06/24 23:23:58 genevb Exp $
// $Log: StMessageManager.cxx,v $
// Revision 1.3  1999/06/24 23:23:58  genevb
// Added message call for compatibility with old fortran code
//
// Revision 1.2  1999/06/24 16:30:42  genevb
// Fixed some memory leaks
//
// Revision 1.1  1999/06/23 15:17:52  genevb
// Introduction of StMessageManager
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessageManager                                                     //
//                                                                      //
// This class manages the messages in STAR software. It is a singleton. //
// Messages are stored in a vector, and come in several types           //
// (i.e. info, error). The types "I" (info), "W" (warning) and          //
// "E" (error) are predefined. Message finding and summary tools are    //
//  also available.                                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//
//
//      ***************************************************************
//      ***************************************************************
//      ****   StMessageManager: The STAR offline message manager  ****
//      ***************************************************************
//      ***************************************************************
//
//
// Table of Contents:
//
// Section I. Basic usage: creating a message
// I-1.   C++ and CINT usage
// I-2.   Fortran usage
//
// Section II. Advanced features
// II-1.  Message summary
// II-2.  Specifying output streams
// II-3.  Non-accounted messages
// II-4.  Turning off printing of the time-date stamp
// II-5.  Access to the current message
// II-6.  Finding a message
// II-7.  Finding a list of messages
// II-8.  Finding messages within a list
// II-9.  Printing a list of messages
// II-10. Formatting output
// II-11. Limiting message counts
//
//______________________________________________________________________
//
//
// Section I.
// ***************************************************
// ********* Basic usage: creating a message *********
// ***************************************************
//
//
// *** I-1.   C++ and CINT usage
//
// In C++ programs, one must include StMessageManager.h.
// A global pointer (gMessMgr) points to the single message managing class.
// A generic message is created as follows:
//
//   gMessMgr->Message("This is the text of the message.");
//     or
//   Char_t* myText = "hello";
//   gMessMgr->Message(myText);
//
// The default action here is to create an "Info" message.
// "Info" is one of three predefined message types (the other
// two are "Warning" and "Error"). The type of the message can
// be specified as a second field in the message declaration:
//
//   gMessMgr->Message("This is an error message.","E");
//
// The message type is specified with a single letter:
// "E" = "Error"
// "I" = "Info"
// "W" = "Warning"
// Additional message types can be declared with AddType():
//
//   gMessMgr->AddType("F","Fatal");
//   gMessMgr->Message("Crashing now...","F");
//
// The second field in the AddType() call specifies the text
// string that will be printed out with each message of this
// type. The available types can be listed with ListTypes();
// AddType() returns an integer which indicates the total number
// of types defined so far, or equals zero if the letter specified
// to represent the new type is already taken.
//
// A shortcut has been provided for declaration of the three
// pre-defined types which is not available for new types:
//
//   gMessMgr->Info("Hello world.");
//     is the same as
//   gMessMgr->Message("Hello world.","I");
//
// Similarly, Error() and Warning() also exist.
//
// If you would like to add variables to your message output,
// you can use the stream version of the message declaration:
//
//   gMessMgr->Info() << "The number is: " << 3.42 << endm;
//   gMessMgr->Message("","W") << big_num << " seems too big." << endm;
//   gMessMgr->Error() << "Alarm #" << alarmNo << "!!!";
//   gMessMgr->Print();
//
// In the first example above, nothing was specified by the call to Info().
// The message is declared afterwards and is terminated by "endm" (similar
// to "endl" for cout). The second example shows how to use the stream
// version for non-predefined types where the type must be specified - to
// do so, the message inside the Message() call must be empty (""). The
// third example shows another way of ending the message and printing it
// to the output: gMessMgr->Print(). This is the option which must be used
// in CINT as "endm" is not recognized properly there.
//
//
// *** I-2.   Fortran usage
//
// No special statements are needed in fortran to include message
// functionality. Messages are declared with a call as follows:
//
//   call StMessage('message text')
//
// As with C++, a second string can denote a message type. New message types
// can be declared with StMessAddType():
//
//   call StMessAddType('A','Abort')
//
// The three predefined types also have associated declaration calls:
//
//   call StInfo('info text')
//   call StError('error message')
//   call StWarning('better not')
//
// Format statements can also be used with character strings:
//
//   character*60 myString
//   ...
//   write(myString,300) 5.6, 6
//   300 format('This first number is ',F5.2,' and the second ',I3)
//   call StInfo(myString)
//
// Notice that using a large number for the character string will cause
// spaces to be printed which may cause the string to wrap around lines.
//
//
//
// Section II.
// *************************************
// ********* Advanced features *********
// *************************************
//
//
// *** II-1.  Message summary
//
// A message summary can be displayed with:
//
//   gMessMgr->Summary(n)
//
// This function compares messages by type and by the first n "tokens"
// (text separated by spaces) of their message strings. By default, n=1.
// Using n=0 would summarize messages by type.
//
//
// *** II-2.  Specifying output streams
//
// Message declarations have a third field (second field for the predefined
// declarations) for an options string. By default, this string contains only
// the letter "O". This means that when the message is printed, it will go
// to stdout (like cout). One can also specify the letter "E" so that the
// message goes to stderr (like cerr). One can even use both: "OE".
//
//
// *** II-3.  Non-accounted messages
//
// Using the additional option "-" means that a message will not be saved
// by the message manager. This means it will not show up in subsequent
// summaries or prints. The message will, however, be printed to the
// stdout or stderr if (and only if) "O" and/or "E" is included in the option
// string.
//
//
// *** II-4.  Turning off printing of the time-date stamp
//
// Use the option "T". Remember, "O" or "E" must also be in the option
// string for the printout.
//
//
// *** II-5.  Access to the current message
//
// A global pointer, gMessage, exists as a pointer to the last StMessage.
// StMessage allows access to the attributes of a message:
//
//   gMessage->GetType();        // message type
//   gMessage->GetMessage();     // message text string
//   gMessage->GetOptions();     // message options
//   gMessage->GetTime();        // time-date stamp
//   gMessage->Print();          // outputs a message
//
//
// *** II-6.  Finding a message
//
// The message manager can find the first message whose text string contains
// matches for up to four search strings:
//
//   StMessage* myMess = gMessMgr->FindMessage("dst","full");
//
//
// *** II-7.  Finding a list of messages
//
// Message lists in the form of a vector of StMessage pointers are called
// a messVec. A list of messages can be found which match up to four strings:
//
//   messVec& myList = gMessMgr->FindMessageList("sun","moon","planet")
//
//
// *** II-8.  Finding messages within a list
//
// Both FindMessage() and FindMessageList() take a fifth argument which is
// messVec pointer.
//
//
// *** II-9.  Printing a list of messages
//
// Call PrintList() with a pointer to the messVec list of messages:
//
//   gMessMgr->PrintList(&(myList));
//
//
// *** II-10. Formatting output
//
// StMessage is an ostream, like cout. It can therefore be used to format
// like cout:
//
//   gMessMgr->Info() << "Here, n=";
//   gMessage->width(5)
//   gMessage* << x << endm;
//
// Notice that once an StMessage gets printed (either by a Print() call
// or the use of "endm"), a message is closed to further streamed input.
//
// One should not forget that character strings can also be formatted by
// sprintf() before adding them to the message.
//
//
// *** II-11. Limiting message counts
//
// The message manager provide message limiting in two ways:
//
//   gMessMgr->SetLimit("full disk",10);    // by message string
//   gMessMgr->SetLimit("I",150);           // by message type
//
// Notice that the only thing that differentiates these calls is
// that a single letter for the first argument means a message type
// limit. The message string limit affects all messages which contain
// the string. A "limit reached" message is output with the last printed
// message.
//
// Limits can be changed again after being set once. The exact same string
// or type must be specified. ListLimits() will list all the limits on
// message strings.
//
// A negative limit removes limiting. There are also a couple shortcuts
// to limiting message counts:
// - RemoveLimit(string/type) sets the limit on a string/type to -1, thereby
//   effectively removing the limit.
// - SwitchOff(string/type) sets the limit to zero.
// - SwitchOn(string/type) sets the limit to -1, removing any limit.
//
///////////////////////////////////////////////////////////////////////////
 
#include "TROOT.h"
#include "StMessageManager.h"
#include <ctype.h>
#include <string.h>

StMessageManager* gMessMgr=0;

StMessageManager* StMessageManager::mInstance = 0;

//
// C and Fortran routines:
//________________________________________
void type_of_call Message_(Char_t* mess, int lines, int id) {
  char* cptr = strchr(mess,'-');
  char* type = "I";
  strncpy(type,cptr,1);
  StMessageManager::Instance()->Message(mess,type);
}
//________________________________________
void type_of_call StMessage_(Char_t* mess, Char_t* type, Char_t* opt) {
  StMessageManager::Instance()->Message(mess,type,opt);
}
//________________________________________
void type_of_call StInfo_(Char_t* mess, Char_t* opt) {
  StMessage_(mess,"I",opt);
}
//________________________________________
void type_of_call StWarning_(Char_t* mess, Char_t* opt) {
  StMessage_(mess,"W",opt);
}
//________________________________________
void type_of_call StError_(Char_t* mess, Char_t* opt) {
  StMessage_(mess,"E",opt);
}
//________________________________________
void type_of_call StMessAddType_(const Char_t* type, const Char_t* text) {
  StMessageManager::Instance()->AddType(type,text);
}

//
// C++ routines:
//_____________________________________________________________________________
ClassImp(StMessageManager)
//_____________________________________________________________________________
StMessageManager::StMessageManager() {
//
// Constructor - only called once when the library is loaded to
// instantiate the global message manager.
//
  gMessMgr = this;
  messTypeList = StMessTypeList::Instance();
  messCounter = StMessageCounter::Instance();
  messCounter->AddType();
  // First messVec on collection is list of all messages
  messCollection.push_back(&messList);
  AddType("I","Info");
  AddType("W","Warning");
  AddType("E","Error");
}
//_____________________________________________________________________________
StMessageManager::~StMessageManager() {
//
// Destructor - must delete the message lists
//
  messVecIter current;
  for (current=messList.begin(); current!=messList.end(); current++)
    delete (*current);
  for (Int_t i=1; i<messCollection.size(); i++)
    delete (messCollection[i]);
  cout << "WARNING!!! DELETING StMessageManager!" << endl;
  gMessMgr = 0;
}
//_____________________________________________________________________________
StMessageManager* StMessageManager::Instance() {
//
// Allows anyone to get a pointer to the single message manager:
//   StMessageManager::Instance()->(member function)
//
  if (!mInstance) {
    mInstance = new StMessageManager;
  }
  return mInstance;
}
//_____________________________________________________________________________
StMessage& StMessageManager::Message(Char_t* mess, Char_t* type, Char_t* opt) {
//
// Message declarator - creates a new message (of class StMessage) and places
// it on the list of known messages.
//
  messObj = new StMessage(mess, type, opt);
  if (!strchr(opt,'-')) {
    messList.push_back(messObj);
    Char_t ty = toupper(type[0]);
    int typeN = messTypeList->FindTypeNum(&ty);
    if (!typeN) typeN = 1;                          // default is Info
    messCollection[typeN]->push_back(messObj);
  }
  return (*messObj);
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
messVecIter StMessageManager::FindMessageIter(const Char_t* s1, Char_t* s2,
               Char_t* s3, Char_t* s4, messVec* list) {
//
// Obtain an iterator for a particular StMessage on a list of messages.
// First message which contains matches to the strings s1,s2,s3,s4 is returned.
//
  if (!list) list = &messList;
  messVecIter current;
  const Char_t* curMess;
  for (current=list->begin(); current!=list->end(); current++) {
    curMess = (*current)->GetMessage();
    if ((strstr(curMess,s1)) && (strstr(curMess,s2)) &&
        (strstr(curMess,s3)) && (strstr(curMess,s4))) return current;
  }
  return 0;
}
//_____________________________________________________________________________
StMessage* StMessageManager::FindMessage(const Char_t* s1, Char_t* s2,
               Char_t* s3, Char_t* s4, messVec* list) {
//
// Obtain a pointer to a particular StMessage on a list of messages.
// First message which contains matches to the strings s1,s2,s3,s4 is returned.
//
  messVecIter current = FindMessageIter(s1,s2,s3,s4,list);
  return ( (current) ? (*current) : 0 );
}
//_____________________________________________________________________________
messVec& StMessageManager::FindMessageList(const Char_t* s1, Char_t* s2,
               Char_t* s3, Char_t* s4, messVec* list) {
//
// Obtain a list (messVec) of StMessage pointers for all messages
// which contain matches to the strings s1,s2,s3,s4.
//
  Char_t* s1a = new Char_t[strlen(s1)];
  strcpy(s1a,s1);
  if ((strlen(s1)==1) && (!list)) {
    int typeN = messTypeList->FindTypeNum(s1);
    if (typeN) {
      list = messCollection[typeN];
      strcpy(s1a,"");
    }
  }
  if (!list) list = &messList;
  if (!(strcmp(s1a,"") || strcmp(s2,"") || strcmp(s3,"") || strcmp(s4,"")))
    return (*list);
  messVec* newList = new messVec();
  messVecIter current;
  const Char_t* curMess;
  for (current=list->begin(); current!=list->end(); current++) {
    curMess = (*current)->GetMessage();
    if ((strstr(curMess,s1)) && (strstr(curMess,s2)) &&
        (strstr(curMess,s3)) && (strstr(curMess,s4)))
      newList->push_back(*current);
  }
  return (*newList);
}
//_____________________________________________________________________________
int StMessageManager::RemoveMessage(StMessage* mess) {
//
// Delete a message and remove it from all lists.
//
  if (!mess) return 3;
  const Char_t* curMess = mess->GetMessage();
  messVecIter current = FindMessageIter(curMess);
  if (!current) return 1;
  messList.erase(current);
  int typeN = messTypeList->FindTypeNum(mess->GetType());
  current = FindMessageIter(curMess,"","","",messCollection[typeN]);
  if (!current) return 2;
  messCollection[typeN]->erase(current);
  delete mess;
  if (mess==gMessage) gMessage = 0;
  return 0;
}
//_____________________________________________________________________________
void StMessageManager::Summary(Int_t nTerms) {
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
  int max = 67;
  int nMess = messList.size();
  StVector(Bool_t) done;
  typedef StVector(Char_t*) CharPtrVec;
  CharPtrVec mType;
  StVector(CharPtrVec) toks;
  int i;
  int j;
  int k;
  Bool_t agree;
  Char_t* temp;
  cout << "  ***** StMessageManager message summary *****" << endl;
  for (i=0; i<nMess; i++) {
    done.push_back(kFALSE);
    mType.push_back(temp = messList[i]->GetType());
    toks.push_back(*(new CharPtrVec));
    temp = new Char_t[81];
    temp = strncpy(temp,(messList[i]->GetMessage()),80);
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
          agree = kTRUE;
          for (k=0; k<nTerms; k++) {
            if ((toks[i])[k] != NULL) {
              if (((toks[j])[k] == NULL) ||
                        strcmp((toks[i])[k],(toks[j])[k])) agree = kFALSE;
            }
            else if ((toks[j])[k] != NULL) agree = kFALSE;
          }
          if (agree) {
            done[j] = kTRUE;
            count++;
          }
        }
      }
      done[i] = kTRUE;
      for (j = messList[i]->Print(max); j<max; j++) cout << ".";
      cout << ".. " << count << endl;
    }
  }
  return;
}
//_____________________________________________________________________________
int StMessageManager::AddType(const Char_t* type, const Char_t* text) {
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
    messCounter->AddType();
  }
  return typeN;
}
//_____________________________________________________________________________
void StMessageManager::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMessageManager.cxx,v 1.3 1999/06/24 23:23:58 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}


//
// Instantiate the (singleton) class upon loading
//
StMessageManager* temp=StMessageManager::Instance();
