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

#include "StHbtMaker/Reader/StHbtBinaryReader.h"
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
StHbtBinaryReader::StHbtBinaryReader(const char* dir, const char* file, const char* appendix) {
  init(dir, file, appendix);
#ifdef __ROOT__
  mIOMaker =0;
#endif
}
//_______________________________
#ifdef __ROOT__
StHbtBinaryReader::StHbtBinaryReader(StIOMaker* ioMaker, const char* dir, const char* file, const char* appendix) {
  init(dir, file, appendix);
  mRetrieve = 1;
  mIOMaker = ioMaker;
#ifdef STHBTDEBUG
  cout << " StHbtBinaryReader::StHbtBinaryReader() -  mIOMaker : " << mIOMaker << endl;
#endif
}
#endif
//_______________________________
void StHbtBinaryReader::init(const char* dir, const char* file, const char* appendix) {
  cout << dir << " " << file << " " << appendix << endl;
  mDirName=dir;
  mFileName=file;
  mAppendix=appendix;
  if ( strstr(mFileName,".lis") ) { 
      cout <<  " AddFileList " << endl;
      AddFileList(file);
  }  

  mReaderStatus = ioOK;
  mRetrieve = 0;
  mStHbtEventVersion = 1;
  mStHbtTrackVersion = 2,
  mStHbtV0Version = 2;
}
//_______________________________
StHbtBinaryReader::~StHbtBinaryReader(){
  delete mFileName;
  delete mFileList;
}
//_______________________________
StHbtEvent* StHbtBinaryReader::ReturnHbtEvent(){
  StHbtEvent* event = new StHbtEvent;
  if (mReaderStatus == ioOK ) mReaderStatus = binaryIO->read(*event,mStHbtEventVersion,mStHbtTrackVersion,mStHbtV0Version);
  if (mReaderStatus != ioOK) {
    cout << " StHbtBinaryReader::ReturnHbtEvent() -  event read with status " << mReaderStatus << endl;
    cout << " StHbtBinaryReader::ReturnHbtEvent() -  fileName: " << mFileName << endl;
  }
  if (mReaderStatus == ioEOF || mReaderStatus == ioERR ) {  // end of file reached
    if (mFileList) {
      delete event; event = new StHbtEvent; // in case we read an incomplete event
      if ( binaryIO ) delete binaryIO; // this closes the file
      mReaderStatus = NextFile();      // write next file from list into mFileName
      if (mReaderStatus == ioOK ) mReaderStatus = Init("r",mTheMessage); // instantiate new reader, open file mFileName
      if (mReaderStatus == ioOK ) mReaderStatus = binaryIO->read(*event,mStHbtEventVersion,mStHbtTrackVersion,mStHbtV0Version);
    }
  }
  if (mReaderStatus != ioOK) {
    cout << " StHbtBinaryReader::ReturnHbtEvent() -  event read with status " << mReaderStatus << endl;
    cout << " StHbtBinaryReader::ReturnHbtEvent() -  fileName: " << mFileName << endl;
    delete event; // we do not return events when reader status is not ioOk
    return 0;
  }    
#ifdef STHBTDEBUG
  cout << " StHbtBinaryReader::ReturnHbtEvent() -  bytes read : " << binaryIO->bytesRead() << endl;
#endif
  return event;
}

//_______________________________
int StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event){
#ifdef __ROOT__
  if (mIOMaker && mRetrieve==1) {
    if ( strcmp(mCurrentFile.c_str(),mIOMaker->GetFile()) ) {
      cout << " StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event)  " << endl;
      cout << "   current file : " << mCurrentFile.c_str() << endl;
      cout << "   new     file : " << mIOMaker->GetFile() << endl;
      mCurrentFile = mIOMaker->GetFile();
      mFileName = (mCurrentFile).c_str();
      cout << "   open file    : " << mFileName << endl;
      if ( binaryIO ) delete binaryIO; // this closes the file
      mReaderStatus = Init("w",mTheMessage);  // instantiate new writer, open file <mFileName>			
    }
  }
#endif
  if (mReaderStatus == ioOK ) { // > means o.k (number of bytes)
    if (!mEventCut || mEventCut->Pass(event)) {
#ifdef STHBTDEBUG
      cout << " StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event) - eventCut passed" << endl;
#endif
      StHbtEvent newEvent(*event, mTrackCut, mV0Cut);  // apply cuts while copying event
      mReaderStatus = binaryIO->write(newEvent,mStHbtEventVersion,mStHbtTrackVersion,mStHbtV0Version);
    }
  }
  if (mReaderStatus != ioOK) { //  > means o.k (number of bytes)
    cout << " StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event) - error# ";
    cout << mReaderStatus << " while writing" << endl;
  }
#ifdef STHBTDEBUG
  cout << " StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event) - bytes written : " << binaryIO->bytesWritten() << endl;
#endif
  return (mReaderStatus);
}

//_______________________________
int StHbtBinaryReader::Init(const char* ReadWrite, StHbtString& Message){
  cout << " StHbtBinaryReader::Init(const char* ReadWrite, StHbtString& Message) - being called with filename: ";
  cout << mFileName << endl;
  mReaderStatus = ioOK;
  if (((*ReadWrite)=='r')|| ((*ReadWrite)=='R')){  // this object will be a reader
    binaryIO = new StHbtIOBinary( mDirName,mFileName, mAppendix ,"r");   // create input object and open file 
    cout << " StHbtBinaryReader::Init() - inputStreamStatus = " << binaryIO->inputStreamStatus();
    cout << " StHbtBinaryReader::Init() - now read message " << endl;
    mReaderStatus = binaryIO->readHeader(Message);                 // read file header
    if (mTheMessage!=Message) {
      mTheMessage = Message;
      cout << Message.c_str() << endl;
    }
    cout << " mReaderStatus " << binaryIO->outputStreamStatus() << endl;

    cout << " StHbtBinaryReader::Init() - now read versions " << endl;
    mReaderStatus = binaryIO->read(mStHbtEventVersion);
    cout << " mReaderStatus " << binaryIO->outputStreamStatus();
    mReaderStatus = binaryIO->read(mStHbtTrackVersion);
    cout << " mReaderStatus " << binaryIO->outputStreamStatus();
    mReaderStatus = binaryIO->read(mStHbtV0Version);
    cout << " mReaderStatus " << binaryIO->outputStreamStatus();
    cout << " StHbtEventVersion=" << mStHbtEventVersion;
    cout << " StHbtTrackVersion=" << mStHbtTrackVersion;
    cout << " StHbtV0Version=" << mStHbtV0Version << endl;
  }
  else{                                            // this object will be a writer
    mTheMessage = Message;
    binaryIO = new StHbtIOBinary(mDirName, mFileName, mAppendix,"w");   // create output object and open file
    cout << " mReaderStatus " << binaryIO->outputStreamStatus();
    mReaderStatus = binaryIO->writeHeader(Message);                // output file header (Message);
    cout << " mReaderStatus " << mReaderStatus << endl;
    mReaderStatus = binaryIO->write(mStHbtEventVersion);
    cout << " mReaderStatus " << mReaderStatus << endl;
    mReaderStatus = binaryIO->write(mStHbtTrackVersion);
    cout << " mReaderStatus " << mReaderStatus << endl;
    mReaderStatus = binaryIO->write(mStHbtV0Version);
    cout << " mReaderStatus " << mReaderStatus << endl;
    cout << " StHbtEventVersion=" << mStHbtEventVersion;
    cout << " StHbtTrackVersion=" << mStHbtTrackVersion;
    cout << " StHbtV0Version=" << mStHbtV0Version << endl;
  }
  cout << " StHbtBinaryReader::Init(const char* ReadWrite, StHbtString& Message) - mReaderStatus: " << mReaderStatus << endl;
  return mReaderStatus;
}

//_______________________________
void StHbtBinaryReader::Finish(){
}
//_______________________________
int StHbtBinaryReader::NextFile() {
  mFileName="";
  delete (mFileList->front());              // remove current file from list
  mFileList->pop_front();                   // remove current file from list
  if ( mFileList->empty() ) return ioEOL;
  mFileName = mFileList->front()->c_str();  // get next file
#ifdef STHBTDEBUG 
  cout << " StHbtBinaryReader::NextFile() - mFileName: " << mFileName << endl;
#endif
  return ioOK;
}
//_______________________________
void StHbtBinaryReader::SetFileName(const char* file){mFileName=(char*)file;}
void StHbtBinaryReader::SetDirName(const char* dir){mDirName=(char*)dir;}
void StHbtBinaryReader::SetAppendix(const char* appendix){mAppendix=(char*)appendix;}
//_______________________________
void StHbtBinaryReader::AddFileList(const char* fileList) {
  cout << " StHbtBinaryReader::AddFileList(char* fileList)"<< endl;
  if (!mFileList) mFileList = new fileCollection;
  ifstream* inputStream = new ifstream;
  inputStream->open(fileList);
  if (!(inputStream)){
    cout << " StHbtBinaryReader::AddFileList(char* fileList) - Cannot open input file! " << endl;
    mReaderStatus = ioERROpen;
    return;
  }
  char* temp;
  cout << " inputStream->good() : " << inputStream->good() << endl;
  for (;inputStream->good();) {
    temp = new char[200];
    inputStream->getline(temp,200);
    cout << temp << endl;
    StHbtString* newFile = new StHbtString(temp);
    if ( newFile->length()>0 ) { 
      mFileList->push_back(newFile);
      cout << "    file " << newFile->c_str() << " added to file list " << endl;
    }
  }
  cout << " StHbtBinaryReader::FillFileList(char* fileList) - Constructing input file list done " << endl;
  if (!mFileList->empty())
    mFileName = mFileList->front()->c_str();
}

//__________________
StHbtString StHbtBinaryReader::Report(){
  StHbtString temp = "\n This is the StHbtBinaryEventReader\n";
  temp += "---> EventCuts in Reader: ";
  if (mEventCut) {
    temp += mEventCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> TrackCuts in Reader: ";
  if (mTrackCut) {
    temp += mTrackCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> V0Cuts in Reader: ";
  if (mV0Cut) {
    temp += mV0Cut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";
  return temp;
}
