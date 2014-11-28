///////////////////////////////////////////////////////////////////////////////
//
// StHbtTagMaker.hh
//
// Author List: 
//  Frank Laue, The Ohio State University, laue@bnl.gov
//
//////////////////////////////////////////////////////////////////////
//
// Description:  Maker to Fill the Hbt EbyE Tag database
//
//////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
//#include <stdlib.h>
#include <math.h>
#include "StHbtTagMaker.h"
#define PR(x) cout << "##### HbtTag: " << (#x) << " = " << (x) << endl;

ClassImp(StHbtTagMaker)

//-------------------------------------------------------------
StHbtTagMaker::StHbtTagMaker(const Char_t* name) : StMaker(name) {
  mHbtTag    = 0;
  mSt_HbtTag = 0;
  mShowTags=0;
  mTagWriter = StHbtTagWriter::Instance();
}

//-------------------------------------------------------------

StHbtTagMaker::~StHbtTagMaker() {
}
//-------------------------------------------------------------
Int_t StHbtTagMaker::Init() {
  return StMaker::Init();
}
//-------------------------------------------------------------
void StHbtTagMaker::Clear(Option_t* opt) {
    if (mTagWriter) mTagWriter->Clear();
    cout << " StHbtTagMaker::Clear() " << endl;
}
//-------------------------------------------------------------
Int_t StHbtTagMaker::Make() {
  // Create a new tag

  // instantiate new St_HbtTag class
  mSt_HbtTag = new St_HbtTag("HbtTag",1);      // table header
  // set the size of the table
  mSt_HbtTag->SetNRows(1);
  // add HbtTag table to the root .data directory
  AddData(mSt_HbtTag,".data");
  // get a pointer to the c-struct containing the variables
  mHbtTag = mSt_HbtTag->GetTable();             // table structure

  // print pointer to hbttag 
  if (Debug()) cout << "StTagPointer: " << mSt_HbtTag << endl;
  if (Debug()) cout << "TagPointer: " << mHbtTag << endl;

  // fill the Hbt Tags 
  if (mTagWriter) {
    cout << "StHbtTagMaker::Make() - fill tags now " << endl;
    Fill(mTagWriter);
  }
  else {
    cout << "StHbtTagMaker::Make() -  no tags written " << endl;
  }

  if ( mShowTags>0) PrintTag();
  return kStOK;
}

//-------------------------------------------------------------
void StHbtTagMaker::Fill(StHbtTagWriter* writer) {
  *mHbtTag = writer->mHbtTag;
}
//-------------------------------------------------------------

void StHbtTagMaker::PrintInfo() {
  if (Debug()) StMaker::PrintInfo();
}

//-------------------------------------------------------------

void StHbtTagMaker::PrintTag(ostream& os) {
  os << "    HbtTag:   " << endl;
  os << " ************ " << endl;
  int nFloats = sizeof(*mHbtTag) / sizeof(float);
  
  for ( int i=0; i<nFloats; i++) {
    if (i%5==0) os << endl;
    os << " " << ((float*)mHbtTag)[i];
  }
  os << endl;
}

//-------------------------------------------------------------

Int_t StHbtTagMaker::Finish() {
  return StMaker::Finish();
}


