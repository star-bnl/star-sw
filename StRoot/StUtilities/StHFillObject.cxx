// $Id: StHFillObject.cxx,v 1.2 1999/08/03 02:29:35 genevb Exp $
// $Log: StHFillObject.cxx,v $
// Revision 1.2  1999/08/03 02:29:35  genevb
// Re-implemented using TMethodCall's
//
// Revision 1.1  1999/07/29 23:27:34  genevb
// Introduction of new class
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StHFillObject allows member functions to be histogrammed             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "StHFillObject.h"
#include "TH1.h"
#include "TMethodCall.h"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>


Stat_t StHFillStat = 1;
static Char_t space[] = " ";
static Char_t semic[] = ";";
static Char_t endol[] = "\n";

StHFillVars::StHFillVars() {
  memset(buffer,0,bufSize);
  memset(buffer1,0,bufSize);
  memset(methods,0,(nsMax*dimMax*sizeof(TMethodCall*)));
  memset(histo,0,(nsMax*sizeof(TH1*)));
}
StHFillVars::~StHFillVars() {
  for (Int_t j=0; j<nsMax; j++) {
    for (Int_t i=0; i<dimMax; i++) {
      if (methods[j][i]) delete (methods[j][i]);
    }
  }
}
void StHFillVars::ClearBuffer() {
  memset(buffer,0,bufSize);
}
static StHFillVars& vars = * (new StHFillVars());


//_____________________________________________________________________________
StHFillObject::StHFillObject() : TObject() {
}
//_____________________________________________________________________________
StHFillObject::~StHFillObject() {
}
//_____________________________________________________________________________
void StHFillObject::Reset() {
  vars.ClearBuffer();
}
//_____________________________________________________________________________
void StHFillObject::Setup(Option_t* option, Int_t hists) {
  strcpy(vars.buffer,option);
  strcpy(vars.buffer1,option);
  Char_t* tokenPtr = vars.buffer1;
  Char_t* token = 0;
  if (strstr(tokenPtr,space))
    token = space;
  else if (strstr(tokenPtr,semic))
    token = semic;
  else if (strstr(tokenPtr,endol))
    token = endol;
  Int_t j=1;
  vars.opt[0] = tokenPtr;
  if (token) {
    tokenPtr = strtok(tokenPtr,token);
    tokenPtr = strtok(NULL,token);
    while (tokenPtr != NULL) {
      memset((--tokenPtr),0,1);
      vars.opt[j++] = (++tokenPtr);
      tokenPtr = strtok(NULL,token);
    }
  }
  vars.nsets = j;

  TClass* thisClass = gROOT->GetClass(this->GetName());
  for (j=0; j<vars.nsets; j++) {
    if (hists) {
      Char_t* histName = strtok(vars.opt[j],":");
      if (histName == NULL)
        vars.histo[j] = 0;
      else
        vars.histo[j] = (TH1*) gROOT->FindObject(histName);
      tokenPtr = strtok(NULL,":");
    } else {
      tokenPtr = strtok(vars.opt[j],":");
    }
    size_t i=0;
    while (tokenPtr != NULL) {
      Char_t* bracket = strchr(tokenPtr,'[');
      if (bracket) {
        vars.indices[j][i] = atoi(++bracket);
        strcpy(--bracket,"");
      } else {
        vars.indices[j][i] = -1;
      }
      if (vars.methods[j][i]) delete (vars.methods[j][i]);
      vars.methods[j][i] = new TMethodCall(thisClass,tokenPtr,"");
      tokenPtr = strtok(NULL,":");
      if ((++i)==dimMax) break;
    }
    vars.dims[j] = i;
  }
}  
//_____________________________________________________________________________
void StHFillObject::GetValues(Int_t printIt) {
  TMethodCall* meth;
  Long_t retLong;
  Double_t retDouble;
  for (int j=0; j<vars.nsets; j++) {
    for (int i=0; i<vars.dims[j]; i++) {
      meth = vars.methods[j][i];
      if (vars.indices[j][i] < 0) {
        meth->Execute(this,retDouble);
        vars.values[j][i] = retDouble;
      } else {
        meth->Execute(this,retLong);
        vars.values[j][i] = ((Float_t*) retLong)[vars.indices[j][i]];
      }
      if (printIt) {
        if (i)
          cout << " : ";
        else if (vars.nsets>1)
          cout << (j+1) << ": ";
        cout.width(10);
        cout << vars.values[j][i];
      }
    }
    if (printIt) cout << endl;
  }
}
//_____________________________________________________________________________
void StHFillObject::Draw(Option_t* option) {
  if (strcmp(vars.buffer,option)) {
    Reset();
    Setup(option,1);
  }
  GetValues(0);
  for (Int_t j=0; j<vars.nsets; j++) {
    if (!vars.histo[j]) return;
    switch (vars.dims[j]) {
      case 1:
        vars.histo[j]->Fill((Axis_t) vars.values[j][0],
                           StHFillStat);
        break;
      case 2:
        vars.histo[j]->Fill((Axis_t) vars.values[j][0],
                           (Axis_t) vars.values[j][1],
                           StHFillStat);
        break;
      case 3:
        vars.histo[j]->Fill((Axis_t) vars.values[j][0],
                           (Axis_t) vars.values[j][1],
                           (Axis_t) vars.values[j][2],
                           StHFillStat);
        break;
      default:
        cout << "Dimensions not understood" << endl;
    }
  }
  return;
}
//_____________________________________________________________________________
void StHFillObject::Print(Option_t* option) {
  if (strcmp(vars.buffer,option)) {
    Reset();
    Setup(option,0);
  }
  GetValues(1);
  return;
}
//_____________________________________________________________________________
void StHFillObject::ls(Option_t* option) {
  if ((option) && !(strcmp(option,"Update")))
    Update();
  else
    TObject::ls(option);
}
//_____________________________________________________________________________
void StHFillObject::Update() {
  cout << "Update() has not been implemented for the derived class!";
  cout << endl;
}
ClassImp(StHFillObject)
