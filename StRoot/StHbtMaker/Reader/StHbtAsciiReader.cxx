/***************************************************************************
 *
 *  
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
 * 
 **************************************************************************/

#include "StHbtMaker/Reader/StHbtAsciiReader.h"
//#include "StChain/StChain.h"
//#include "StHbtMaker/Infrastructure/StHbtIO.cc"


ClassImp(StHbtAsciiReader)



//_______________________________
StHbtAsciiReader::StHbtAsciiReader() : mInputStream(0), mOutputStream(0){
  mFileName = "HbtAsciiFile";  // default name
}

//_______________________________
StHbtAsciiReader::StHbtAsciiReader(char* file) : mInputStream(0), mOutputStream(0), mFileName(file)
{  /* no-op */ }

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
    return (0);
  }
  if (!(*mInputStream)){
    cout << "StHbtAsciiReader::ReturnHbtEvent() - input stream in bad state!" << endl;
    return (0);
  }
  StHbtEvent* event = new StHbtEvent;
  (*mInputStream) >> (*event);
  if (!(mInputStream->good())){
    cout << "StHbtAsciiReader::ReturnHbtEvent() - input stream in bad state!" <<endl;
    cout << "State is " << mInputStream->rdstate() << " - attempting to clear and move on " << endl;
    //    return (0);
    mInputStream->clear();
  }
  return event;
}

//_______________________________
StHbtString StHbtAsciiReader::Report(){
  StHbtString temp = "\n This is the StHbtAsciiReader - no Early Cuts applied\n";
  return temp;
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

  (*mOutputStream) << (*event);
  return (0);
}

//_______________________________
int StHbtAsciiReader::Init(const char* ReadWrite, StHbtString Message){
  cout << " *\n *\n *\n StHbtAsciiReader::Init() being called*\n *\n";

  //  if ((ReadWrite=="r")|| (ReadWrite=="R")){  // this object will be a reader
  if (((*ReadWrite)=='r')|| ((*ReadWrite)=='R')){  // this object will be a reader
    mInputStream = new ifstream;
    mInputStream->open(mFileName);
    if (!(*mInputStream)){
      cout << "StHbtAsciiReader::Init() - Cannot open input file! " << endl;
      return (1);
    }
    cout << "StHbtAsciiReader::Init() - being configured as a Reader" << ReadWrite << endl;
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
  }
  return (0);
}

//_______________________________
void StHbtAsciiReader::Finish(){
  if (mInputStream) mInputStream->close();
  if (mOutputStream) mOutputStream->close();
}

