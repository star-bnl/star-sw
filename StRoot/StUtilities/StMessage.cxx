// $Id: StMessage.cxx,v 1.2 1999/06/24 16:30:41 genevb Exp $
// $Log: StMessage.cxx,v $
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

#include <ctype.h>
#include <string.h>
#include "TROOT.h"
#include "StMessage.h"
#include "StMessageCounter.h"

StMessage* gMessage=0;
StMessage* endm=0;
ostream& operator<<(ostream& os, StMessage* mm) {
  mm->Print();
  return os;
}
static Char_t* leader = "St";
static StMessageCounter* messCounter = StMessageCounter::Instance();

ClassImp(StMessage)

//_____________________________________________________________________________
StMessage::StMessage(Char_t *mess, Char_t *ty, Char_t* opt) : ostrstream(),
type(new Char_t(toupper(*ty))), messTime(new TDatime()) {
  Int_t len = strlen(opt);
  option = new Char_t[len];
  while (len--)
    option[len] = toupper(opt[len]);
//  location = "Unknown";
//  runNumber = 0;
  operator<<(mess);
  if (strcmp(mess,""))
    Print();
  gMessage = this;
  endm = this;
}
//_____________________________________________________________________________
StMessage::~StMessage() {
  delete messTime;
}
//_____________________________________________________________________________
Int_t StMessage::Print(Int_t nChars) {
  operator<<(ends);
  TString outstr;
  int printIt=1;
  if (!nChars) {
    printIt = messCounter->CheckLimit(str(),type);
  }
  if (printIt) {
    outstr = leader;
    const Char_t* temp(StMessTypeList::Instance()->Text(type));
    if (temp) outstr += temp;
    outstr += ": ";
    outstr += str();
    if (nChars<=0) {
      if (!strchr(option,'T')) {
        outstr += " (";
        Char_t* temp2 = strchr(messTime->AsString(),' ');
        outstr += ++temp2;
        outstr += ")";
      }
      outstr += "\n";
    }
  }
  if (!nChars) {
    outstr += messCounter->GetOutMessage();
  } else {
    if (nChars>0) 
      outstr = outstr(0,nChars);
    else
      nChars = 0;
  }
  if ((strchr(option,'O')) || (nChars)) {
    cout << outstr;
    cout.flush();
  }
  if ((strchr(option,'E')) && !(nChars)) {
    cerr << outstr;
    cerr.flush();
  }
  return outstr.Length();
}
//_____________________________________________________________________________
void StMessage::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMessage.cxx,v 1.2 1999/06/24 16:30:41 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}
