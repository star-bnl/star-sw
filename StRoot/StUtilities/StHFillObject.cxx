// $Id: StHFillObject.cxx,v 1.1 1999/07/29 23:27:34 genevb Exp $
// $Log: StHFillObject.cxx,v $
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
#include "TList.h"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>

TList* StHFillObject::knownMembers=0;

static const size_t dimMax = 8;
static const size_t bufSize = 1024;
static Char_t StHFillOptBuffer[bufSize];
static Char_t StHFillOptBuffer1[bufSize];
static TMemOff StHFillOffsets[dimMax];
static Int_t StHFillIndices[dimMax];
static Float_t StHFillValues[dimMax];
static Int_t StHFillDims=0;
static TH1* StHFillHisto = 0;
static Stat_t StHFillStat = 1;

//_____________________________________________________________________________
StHFillObject::StHFillObject() : TObject() {
}
//_____________________________________________________________________________
StHFillObject::~StHFillObject() {
}
//_____________________________________________________________________________
TMemOff StHFillObject::GetOffset(Char_t* member){ 
  StMemOff* temp = (StMemOff*) knownMembers->FindObject(member);
  if (!temp) {
    temp = (StMemOff*) knownMembers->First();
    cout << "StHFillObject: " << member << " not understood, using " <<
      temp->GetName() << endl;
  }
  return (temp->offset);
}
//_____________________________________________________________________________
void StHFillObject::LearnMember(Char_t* member, TMemOff offset) {
  if (!knownMembers) knownMembers = new TList();
  knownMembers->Add((TObject*) new StMemOff(member,offset));
}
//_____________________________________________________________________________
void StHFillObject::Reset() {
  if (knownMembers) {
    delete knownMembers;
    knownMembers=0;
  }
  memset(StHFillOptBuffer,0,bufSize);
}
//_____________________________________________________________________________
void StHFillObject::Setup(Option_t* option, Int_t hists) {
  strcpy(StHFillOptBuffer,option);
  strcpy(StHFillOptBuffer1,option);
  Char_t* tokenPtr = 0;
  if (hists) {
    Char_t* histName = strtok(StHFillOptBuffer1,":");
    if (histName == NULL)
      StHFillHisto = 0;
    else
      StHFillHisto = (TH1*) gROOT->FindObject(histName);
    tokenPtr = strtok(NULL,":");
  } else {
    tokenPtr = strtok(StHFillOptBuffer1,":");
  }
  size_t i=0;
  while (tokenPtr != NULL) {
    Char_t* bracket = strchr(tokenPtr,'[');
    if (bracket) {
      StHFillIndices[i] = atoi(++bracket);
      strcpy(--bracket,"");
    } else {
      StHFillIndices[i] = -1;
    }
    StHFillOffsets[i] = GetOffset(tokenPtr);
    tokenPtr = strtok(NULL,":");
    if ((++i)==dimMax) break;
  }
  StHFillDims = i;
}  
//_____________________________________________________________________________
void StHFillObject::GetValues(Int_t printIt) {
  for (int i=0; i<StHFillDims; i++) {
    TMemOff tempOff = StHFillOffsets[i];
    if (StHFillIndices[i] < 0) {
      StHFillValues[i] = (this->*tempOff) ();
    } else {
      StHFillValues[i] = ((this->*((TMemOffP) tempOff)) ())[StHFillIndices[i]];
    }
    if (printIt) {
      if (i) cout << " : ";
      cout.width(10);
      cout << StHFillValues[i];
    }
  }
}
//_____________________________________________________________________________
void StHFillObject::Draw(Option_t* option) {
  if (strcmp(StHFillOptBuffer,option)) {
    Reset();
    LearnMembers();
    Setup(option,1);
  }
  if (!StHFillHisto) return;
  GetValues(0);
  switch (StHFillDims) {
    case 1:
      StHFillHisto->Fill((Axis_t) StHFillValues[0],
                         StHFillStat);
      break;
    case 2:
      StHFillHisto->Fill((Axis_t) StHFillValues[0],
                         (Axis_t) StHFillValues[1],
                         StHFillStat);
      break;
    case 3:
      StHFillHisto->Fill((Axis_t) StHFillValues[0],
                         (Axis_t) StHFillValues[1],
                         (Axis_t) StHFillValues[2],
                         StHFillStat);
      break;
    default:
      cout << "Dimensions not understood" << endl;
  }     
  return;
}
//_____________________________________________________________________________
void StHFillObject::Print(Option_t* option) {
  if (strcmp(StHFillOptBuffer,option)) {
    Reset();
    LearnMembers();
    Setup(option,0);
  }
  GetValues(1);
  cout << endl;
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
  if (!knownMembers) knownMembers = new TList();
}
ClassImp(StHFillObject)
