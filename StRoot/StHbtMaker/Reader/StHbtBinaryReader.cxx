/***************************************************************************
 *
 *  
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when reading/writing
 *       BINARY-based HBT microDSTs
 *       It is been developted out of a copy of the StHbtAsciiReader class 
 *       on 11/18/1999 
 *
 ***************************************************************************
 *
 * 
 **************************************************************************/

#include <string>
#include "StHbtMaker/Reader/StHbtBinaryReader.h"
//#include "StChain.h"
//#include "StHbtMaker/Infrastructure/StHbtIO.cc"
#include "StHbtMaker/Infrastructure/StHbtIOBinary.hh"

#ifdef __ROOT__
ClassImp(StHbtBinaryReader)
#endif

void wait(int n, const char* c) { 
  for (int i = 0; i< 1e6*n; i++) {
    cout << c;
  }
}
//_______________________________
StHbtBinaryReader::StHbtBinaryReader() : mInputStream(0), mOutputStream(0){
  mFileName = "HbtBinaryFile";  // default name
  mReaderStatus = 0;           // means "good"
  
}

//_______________________________
StHbtBinaryReader::StHbtBinaryReader(char* file) : mInputStream(0), mOutputStream(0), mFileName(file)
{
  mReaderStatus = 0;           // means "good"
}
 
//_______________________________
StHbtBinaryReader::~StHbtBinaryReader(){
}

//_______________________________
StHbtEvent* StHbtBinaryReader::ReturnHbtEvent(){
  StHbtEvent* event = new StHbtEvent;
  int ret = binaryIO->readEvent(*event);
  if (!ret) cout << " error #" << ret << " while reading" << endl;
  cout << " StHbtBinaryReader::ReturnHbtEvent() *** bytes read : " << binaryIO->bytesRead() << endl;
  return event;
}

//_______________________________
StHbtString StHbtBinaryReader::Report(){
  StHbtString temp = "\n This is the StHbtBinaryReader - no Early Cuts applied\n";
  return temp;
}

//_______________________________
int StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event){
  int ret = binaryIO->writeEvent(*event);
  if (!ret) cout << " error #" << ret << " while writing" << endl;
  cout << " StHbtBinaryReader::ReturnHbtEvent() *** bytes written : " << binaryIO->bytesWritten() << endl;;
  return (0);
}

//_______________________________
int StHbtBinaryReader::Init(const char* ReadWrite, StHbtString Message){
  cout << " *\n *\n *\n StHbtBinaryReader::Init() being called*\n *\n";
  mReaderStatus = 0;           // means "good"
  //  if ((ReadWrite=="r")|| (ReadWrite=="R")){  // this object will be a reader
  if (((*ReadWrite)=='r')|| ((*ReadWrite)=='R')){  // this object will be a reader
    // create input object and open file 
    binaryIO = new StHbtIOBinary(mFileName,"r");
    binaryIO->readString(Message);
    cout << Message.c_str() << endl;
  }
  else{                                      // this object will be a writer
    // create output object and open file
    binaryIO = new StHbtIOBinary(mFileName,"w");
    // output file header (Message);
    binaryIO->writeString(Message);
  }
  cout << " *\n *\n *\n StHbtBinaryReader::Init() end *\n";
 return (0);
}

//_______________________________
void StHbtBinaryReader::Finish(){
}

