/***************************************************************************
 *
 * $Id: 
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when reading/writing
 *       ASCII-based HBT microDSTs
 *
 ***************************************************************************
 *
 * $Log:
 **************************************************************************/


#include <Stiostream.h>
#include "Stiostream.h"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"
#include "StHbtMaker/Reader/StHbtAsciiReader.h"

#ifdef __ROOT__
ClassImp(StHbtAsciiReader)
#endif


//_______________________________
StHbtAsciiReader::StHbtAsciiReader() : mInputStream(0), mOutputStream(0){
  mFileName = "HbtAsciiFile";  // default name
  mReaderStatus = 0;           // means "good"
  mStHbtEventVersion = mStHbtTrackVersion = mStHbtV0Version = 1;
}

//_______________________________
StHbtAsciiReader::StHbtAsciiReader(char* file) : mInputStream(0), mOutputStream(0), mFileName(file)
{
  mReaderStatus = 0;           // means "good"
  mStHbtEventVersion = mStHbtTrackVersion = mStHbtV0Version = 1;
}

//_______________________________
StHbtAsciiReader::~StHbtAsciiReader(){
  if (!mInputStream){
    delete mInputStream;
    mInputStream = 0;
  }
  if (!mOutputStream){
    delete mOutputStream;
    mOutputStream = 0;
  }
}

//_______________________________
StHbtEvent* StHbtAsciiReader::ReturnHbtEvent(){
  if (!mInputStream){
    cout << "StHbtAsciiReader::ReturnHbtEvent() - there is no input stream!";
    mReaderStatus = 1;           // 0 means "good"
    return (0);
  }
  if (!(*mInputStream)){
    cout << "StHbtAsciiReader::ReturnHbtEvent() - input stream in bad state!" << endl;
    cout << "State is " << mInputStream->rdstate() << endl;
    mReaderStatus = 1;           // 0 means "good"
    return (0);
  }
  StHbtEvent* event = new StHbtEvent;
  (*mInputStream) >> (*event);
  if (!(mInputStream->good())){
    cout << "StHbtAsciiReader::ReturnHbtEvent() - input stream in bad state!" <<endl;
    cout << "State is " << mInputStream->rdstate() << endl;
    mReaderStatus = 1;           // 0 means "good"
    return (0);
    //mInputStream->clear();
  }
  return event;
}
//_______________________________
int StHbtAsciiReader::WriteHbtEvent(StHbtEvent* event){

  if (!mOutputStream){    // checks for existence of stream
    cout << "\n StHbtAsciiReader::WriteHbtEvent() - There is no Output Stream -- I am NOT writing !!! \n\n";
    return (1);
  }

  if (!(*mOutputStream)){  // checks for good state of stream
    cout << "\n StHbtAsciiReader::WriteHbtEvent() - Output Stream in bad state -- I am NOT writing !!! \n\n";
    return (2);
  }

  
  if ( !mEventCut || mEventCut->Pass(event) ) {  
    cout << "StHbtAsciiReader: eventCut passed" << endl;
    StHbtEvent newEvent(*event, mTrackCut, mV0Cut);
    (*mOutputStream) << (newEvent);
  }
  return (0);
}

//_______________________________
//StHbtString StHbtAsciiReader::Report(){
//  StHbtString temp = "\n This is StHbtAsciiReader calling the base class Report()";
///  temp += this->Report();
//  return temp;
//}
//_______________________________
int StHbtAsciiReader::Init(const char* ReadWrite, StHbtString& Message){
  cout << " *\n *\n *\n StHbtAsciiReader::Init() being called*\n *\n";
  mReaderStatus = 0;           // means "good"
  //  if ((ReadWrite=="r")|| (ReadWrite=="R")){  // this object will be a reader
  if (((*ReadWrite)=='r')|| ((*ReadWrite)=='R')){  // this object will be a reader
    mInputStream = new ifstream;
    mInputStream->open(mFileName);
    if (!(*mInputStream)){
      cout << "StHbtAsciiReader::Init() - Cannot open input file! " << endl;
      return (1);
    }
    cout << "StHbtAsciiReader::Init() - being configured as a Reader" << ReadWrite << endl;
    // extract Input reader Report...
    char temp[200] = "";
    string stemp;
    do {
      Message += temp;
      Message += "\n";
      mInputStream->getline(temp,200);
      stemp = temp;
    } while (stemp != "-*-*-*-* End of Input Reader Report");
    cout << "Here is the message that was at the beginning of the file...\n";
    cout << Message.c_str();
    (*mInputStream) >> mStHbtEventVersion >> mStHbtTrackVersion >> mStHbtV0Version;
    cout << " StHbtEventVersion=" << mStHbtEventVersion;
    cout << " StHbtTrackVersion=" << mStHbtTrackVersion;
    cout << " StHbtV0Version=" << mStHbtV0Version << endl;
  }
  else{                                      // this object will be a writer
    mOutputStream = new ofstream;
    //    mOutputStream->open(mFileName,ios::noreplace);  // that last bit means do NOT overwrite files...
                                                          // (But then it doesn't work???)
    mOutputStream->open(mFileName);
    if (!(*mOutputStream)){
      cout << "StHbtAsciiReader::Init() - Cannot open output file! " << endl;
      return (1);
    }
    cout << "StHbtAsciiReader::Init() - being configured as a Writer" << ReadWrite << endl;
    (*mOutputStream) << Message.c_str();
    (*mOutputStream) << endl;
    (*mOutputStream) << "-*-*-*-* End of Input Reader Report" << endl;  // write THIS out even if there is no report
    (*mOutputStream) << mStHbtEventVersion << " " << mStHbtTrackVersion << " " << mStHbtV0Version << endl;
  }
  return (0);
}

//_______________________________
void StHbtAsciiReader::Finish(){
  if (mInputStream) mInputStream->close();
  if (mOutputStream) mOutputStream->close();
}

