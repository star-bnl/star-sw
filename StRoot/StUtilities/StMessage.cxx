// $Id: StMessage.cxx,v 1.1 1999/06/23 15:17:46 genevb Exp $
// $Log: StMessage.cxx,v $
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

ClassImp(StMessage)

//_____________________________________________________________________________
StMessage::StMessage(Char_t *mess, Char_t *ty, Char_t* opt) : ostrstream(),
type(new Char_t), messTime(new TDatime()) {
  leader = "St";
  option = opt;
  option.ToUpper();
  location = "Unknown";
  runNumber = 0;
  Char_t t0 = toupper(*ty);
  if (StMessTypeList::Instance()->FindType(&t0))
    strncpy(type,&t0,1);
  else
    strncpy(type,"I",1);       // default is Info
  operator<<(mess);
  if (strcmp(mess,""))
    Print();
  gMessage = this;
  endm = this;
}
//_____________________________________________________________________________
StMessage::~StMessage() {
}
//_____________________________________________________________________________
Int_t StMessage::Print(Int_t nChars) {
  operator<<(ends);
  TString outstr;
  Char_t* limString;
  if (!nChars) {
    limString = StMessageCounter::Instance()->CheckLimit(str(),type);
  } else {
    limString = "";
    if (nChars==-1) nChars=0;
  }
  if (strncmp(limString,"<||>",4)) {
    outstr = leader;
    const Char_t* temp(StMessTypeList::Instance()->Text(type));
    if (temp) outstr += temp;
    outstr += ": ";
    outstr += str();
    if (!nChars) {
      if (!strchr(option.Data(),'T')) {
        outstr += " (";
        Char_t* temp2 = strchr(messTime->AsString(),' ');
        outstr += ++temp2;
        outstr += ")";
      }
      outstr += "\n";
    }
  } else {       // If comparison is zero (is there), do not print message
    limString = &(limString[4]);
  }
  outstr += limString;
  if (nChars>0) outstr = outstr(0,nChars);
  if ((strchr(option.Data(),'O')) || (nChars)) {
    cout << outstr;
    cout.flush();
  }
  if ((strchr(option.Data(),'E')) && !(nChars)) {
    cerr << outstr;
    cerr.flush();
  }
  return outstr.Length();
}
//_____________________________________________________________________________
void StMessage::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMessage.cxx,v 1.1 1999/06/23 15:17:46 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}
