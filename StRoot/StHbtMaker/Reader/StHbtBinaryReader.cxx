/***************************************************************************
 *
 * $Id: 
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
 * $Log:
 **************************************************************************/


#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"

#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowAnalysisMaker/StFlowAnalysisMaker.h"
#include "StFlowMaker/StFlowSelection.h"

#include "StHbtMaker/Infrastructure/StHbtIOBinary.hh"
#include "StHbtMaker/Reader/StHbtBinaryReader.h"

#ifdef __ROOT__
ClassImp(StHbtBinaryReader)
#endif

//_______________________________
StHbtBinaryReader::StHbtBinaryReader(const char* dir, const char* file, const char* appendix) {
  init(dir, file, appendix);
#ifdef __ROOT__
  mIOMaker =0;
#endif
  mFlowMaker = 0;
  mFlowAnalysisMaker = 0;
}
//_______________________________
#ifdef __ROOT__
StHbtBinaryReader::StHbtBinaryReader(StIOMaker* ioMaker, const char* dir, const char* file, const char* appendix) {
  init(dir, file, appendix);
  mRetrieve = 1;
  mIOMaker = ioMaker;
  if (mDebug) cout << " StHbtBinaryReader::StHbtBinaryReader() -  mIOMaker : " << mIOMaker << endl;
  mFlowMaker = 0;
  mFlowAnalysisMaker = 0;
}
#endif
//_______________________________
void StHbtBinaryReader::init(const char* dir, const char* file, const char* appendix) {
  if (mDebug) cout << dir << " " << file << " " << appendix << endl;
  mDirName=dir;
  mFileName=file;
  mAppendix=appendix;
  if ( strstr(mFileName,".lis") ) { 
      if (mDebug) cout <<  " AddFileList " << endl;
      AddFileList(file);
  }  

  mReaderStatus = ioOK;
  mRetrieve = 0;
  mStHbtEventVersion = 2;
  mStHbtTrackVersion = 2,
  mStHbtV0Version = 3;
  mFlowMaker = 0;
  mFlowAnalysisMaker = 0;
}
//_______________________________
StHbtBinaryReader::~StHbtBinaryReader(){
  if (mFileList) delete mFileList;
}
//_______________________________
StHbtEvent* StHbtBinaryReader::ReturnHbtEvent(){
  if (mDebug>1) cout << " StHbtBinaryReader::ReturnHbtEvent() " << endl;
  StHbtEvent* event = new StHbtEvent;
  if (mReaderStatus == ioOK ) mReaderStatus = binaryIO->read(*event,mStHbtEventVersion,mStHbtTrackVersion,mStHbtV0Version);
  if (mReaderStatus != ioOK) {
  if (mDebug>1)  cout << " StHbtBinaryReader::ReturnHbtEvent() -  event read with status " << mReaderStatus << endl;
  if (mDebug>1)  cout << " StHbtBinaryReader::ReturnHbtEvent() -  fileName: " << mFileName << endl;
  }
  if (mReaderStatus == ioEOF || mReaderStatus == ioERR ) {  // end of file reached
    if (mFileList) {
      while (mReaderStatus !=ioEOL && mReaderStatus !=ioOK ) {
	if (event) delete event; event = new StHbtEvent; // in case we read an incomplete event
	if ( binaryIO ) delete binaryIO; // this closes the file
	mReaderStatus = NextFile();      // write next file from list into mFileName
	if (mDebug>1) cout << mReaderStatus << endl;
	if (mReaderStatus == ioOK ) mReaderStatus = Init("r",mTheMessage); // instantiate new reader, open file mFileName
	if (mReaderStatus == ioOK ) mReaderStatus = binaryIO->read(*event,mStHbtEventVersion,mStHbtTrackVersion,mStHbtV0Version);
      }
    }
  }
  if (mReaderStatus != ioOK) {
    if (mDebug>1) cout << " StHbtBinaryReader::ReturnHbtEvent() -  event read with status " << mReaderStatus << endl;
    if (mDebug>1) cout << " StHbtBinaryReader::ReturnHbtEvent() -  fileName: " << mFileName << endl;
    if (event) delete event; event=0;// we do not return events when reader status is not ioOk
    return 0;
  }    
  // parse event throu event cut
  if (mEventCut && event ) {
    if ( mEventCut->Pass(event)==0 ) {
      delete event;
      event=0;
    }
  }

  // Pass through track cut if there is one
  if( mTrackCut && event){
    StHbtTrackIterator pIter;
    StHbtTrack* pParticle;
    StHbtTrackCollection NewTrackCollection;

    for (pIter=event->TrackCollection()->begin();
	 pIter!=event->TrackCollection()->end();pIter++){
      pParticle = *pIter;
      bool tmpPassParticle = mTrackCut->Pass(pParticle);
      if (tmpPassParticle){
	NewTrackCollection.push_back(pParticle);
      }
      else{
	delete *pIter;
      }
    }
    event->TrackCollection()->clear();

    for (pIter=NewTrackCollection.begin();
	 pIter!=NewTrackCollection.end();pIter++){
      event->TrackCollection()->push_back(*pIter);
    }
    NewTrackCollection.clear();
  }

 //Pass through v0 cut if there is one
  if( mV0Cut && event){
    StHbtV0Iterator pIter;
    StHbtV0* pParticle;
    StHbtV0Collection NewV0Collection;
   
    for (pIter=event->V0Collection()->begin();
	 pIter!=event->V0Collection()->end();pIter++){
      pParticle = *pIter;
      bool tmpPassParticle = mV0Cut->Pass(pParticle);
      if (tmpPassParticle){
	NewV0Collection.push_back(pParticle);
      }
      else{
	delete *pIter;
      }
    }
    event->V0Collection()->clear();
    for (pIter=NewV0Collection.begin();
	 pIter!=NewV0Collection.end();pIter++){
      event->V0Collection()->push_back(*pIter);
    }
    NewV0Collection.clear();
  }

  // to get RP from FlowMaker
  if ( mFlowMaker && event ) {
//     mFlowMaker->FillFlowEvent(event);
//     // First get RP for whole event
//     mFlowMaker->FlowSelection()->SetSubevent(-1);
//     double reactionPlane = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
//     cout << "Reaction Plane " << reactionPlane << endl;
//     event->SetReactionPlane(reactionPlane);
//     // Sub event RPs
//     mFlowMaker->FlowSelection()->SetSubevent(0);
//     double RP1 = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
//     mFlowMaker->FlowSelection()->SetSubevent(1);
//     double RP2 = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
//     event->SetReactionPlaneSubEventDifference(RP1-RP2);
//     if (mFlowAnalysisMaker) mFlowAnalysisMaker->Make();
  }

  if (mDebug>1) {
    cout << " StHbtBinaryReader::ReturnHbtEvent() - current filename: " << mFileName << endl;
    cout << " StHbtBinaryReader::ReturnHbtEvent() -  bytes read : " << binaryIO->bytesRead() << endl;
    if (event){
      cout << " StHbtBinaryReader::ReturnHbtEvent() - #tracks/#V0s : " << event->TrackCollection()->size() << "/";
      cout << event->V0Collection()->size() << endl;
    }
    else{
      cout << "StHbtBinaryReader::ReturnHbtEvent() - failed cut - no StHbtEvent returned" << endl;
    }
  }
  return event;
}

//_______________________________
int StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event){
#ifdef __ROOT__
  if (mIOMaker && mRetrieve==1) {
    if ( strcmp(mCurrentFile.c_str(),mIOMaker->GetFile()) ) {
      if (mDebug) {
	cout << " StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event)  " << endl;
	cout << "   current file : " << mCurrentFile.c_str() << endl;
	cout << "   new     file : " << mIOMaker->GetFile() << endl;
      }
      mCurrentFile = mIOMaker->GetFile();
      mFileName = (mCurrentFile).c_str();
      if (mDebug) cout << "   open file    : " << mFileName << endl;
      if ( binaryIO ) delete binaryIO; // this closes the file
      mReaderStatus = Init("w",mTheMessage);  // instantiate new writer, open file <mFileName>			
    }
  }
#endif
  if (mReaderStatus == ioOK ) { // > means o.k (number of bytes)
    if (!mEventCut || mEventCut->Pass(event)) {
      if (mDebug>1) cout << " StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event) - eventCut passed" << endl;
      //      StHbtEvent* newEvent = new StHbtEvent(*event, mTrackCut, mV0Cut);  // apply cuts while copying event
      //      mReaderStatus = binaryIO->write(*newEvent,mStHbtEventVersion,mStHbtTrackVersion,mStHbtV0Version);
      //      delete newEvent;
      StHbtEvent newEvent(*event, mTrackCut, mV0Cut);  // apply cuts while copying event
      mReaderStatus = binaryIO->write(newEvent,mStHbtEventVersion,mStHbtTrackVersion,mStHbtV0Version);
    }
  }
  if (mReaderStatus != ioOK) { //  > means o.k (number of bytes)
    if (mDebug) cout << " StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event) - error# ";
    if (mDebug) cout << mReaderStatus << " while writing" << endl;
  }
  if (mDebug>1) cout << " StHbtBinaryReader::WriteHbtEvent(StHbtEvent* event) - bytes written : " << binaryIO->bytesWritten() << endl;
  return (mReaderStatus);
}

//_______________________________
int StHbtBinaryReader::Init(const char* ReadWrite, StHbtString& Message){
  if (mDebug>1) cout << " StHbtBinaryReader::Init(const char* ReadWrite, StHbtString& Message) - being called with filename: ";
  cout << mFileName << endl;
  mReaderStatus = ioOK;
  if (((*ReadWrite)=='r')|| ((*ReadWrite)=='R')){  // this object will be a reader
    binaryIO = new StHbtIOBinary( mDirName,mFileName, mAppendix ,"r");   // create input object and open file 
    if (mDebug>1) cout << " StHbtBinaryReader::Init() - inputStreamStatus = " << binaryIO->inputStreamStatus();
    if (mDebug>1) cout << " StHbtBinaryReader::Init() - now read message " << endl;
    mReaderStatus = binaryIO->readHeader(Message);                 // read file header
    if (mTheMessage!=Message) {
      mTheMessage = Message;
      cout << Message.c_str() << endl;
    }
    if (mDebug>1) cout << " mReaderStatus " << binaryIO->outputStreamStatus() << endl;

    if (mDebug>1) cout << " StHbtBinaryReader::Init() - now read versions " << endl;
    mReaderStatus = binaryIO->read(mStHbtEventVersion);
    if (mDebug>1) cout << " mReaderStatus " << binaryIO->outputStreamStatus();
    mReaderStatus = binaryIO->read(mStHbtTrackVersion);
    if (mDebug>1) cout << " mReaderStatus " << binaryIO->outputStreamStatus();
    mReaderStatus = binaryIO->read(mStHbtV0Version);
    if (mDebug>1) {
      cout << " mReaderStatus " << binaryIO->outputStreamStatus();
      cout << " StHbtEventVersion=" << mStHbtEventVersion;
      cout << " StHbtTrackVersion=" << mStHbtTrackVersion;
      cout << " StHbtV0Version=" << mStHbtV0Version << endl;
    }
  }
  else{                                            // this object will be a writer
    mTheMessage = Message;
    binaryIO = new StHbtIOBinary(mDirName, mFileName, mAppendix,"w");   // create output object and open file
    if (mDebug>1) cout << " mReaderStatus " << binaryIO->outputStreamStatus();
    mReaderStatus = binaryIO->writeHeader(Message);                // output file header (Message);
    if (mDebug>1) cout << " mReaderStatus " << mReaderStatus << endl;
    mReaderStatus = binaryIO->write(mStHbtEventVersion);
    if (mDebug>1) cout << " mReaderStatus " << mReaderStatus << endl;
    mReaderStatus = binaryIO->write(mStHbtTrackVersion);
    if (mDebug>1) cout << " mReaderStatus " << mReaderStatus << endl;
    mReaderStatus = binaryIO->write(mStHbtV0Version);
    if (mDebug>1) {
      cout << " mReaderStatus " << mReaderStatus << endl;
      cout << " StHbtEventVersion=" << mStHbtEventVersion;
      cout << " StHbtTrackVersion=" << mStHbtTrackVersion;
      cout << " StHbtV0Version=" << mStHbtV0Version << endl;
    }
  }
  if (mDebug>1) cout << " StHbtBinaryReader::Init(const char* ReadWrite, StHbtString& Message) - mReaderStatus: " << mReaderStatus << endl;
  return mReaderStatus;
}

//_______________________________
void StHbtBinaryReader::Finish(){
}
//_______________________________
int StHbtBinaryReader::NextFile() {
  mFileName="\0";
  delete (mFileList->front());              // remove current file from list
  mFileList->pop_front();                   // remove current file from list
  if ( mFileList->empty() ) return ioEOL;
  mFileName = mFileList->front()->c_str();  // get next file
  if (mDebug) cout << " StHbtBinaryReader::NextFile() - mFileName: " << mFileName << endl;
  return ioOK;
}
//_______________________________
void StHbtBinaryReader::SetFileName(const char* file){mFileName=(char*)file;}
void StHbtBinaryReader::SetDirName(const char* dir){mDirName=(char*)dir;}
void StHbtBinaryReader::SetAppendix(const char* appendix){mAppendix=(char*)appendix;}
//_______________________________
void StHbtBinaryReader::AddFileList(const char* fileList) {
  if (mDebug>1) cout << " StHbtBinaryReader::AddFileList(char* fileList)"<< endl;
  if (!mFileList) mFileList = new fileCollection;
  ifstream* inputStream = new ifstream;
  inputStream->open(fileList);
  if (!(inputStream)){
    if (mDebug) cout << " StHbtBinaryReader::AddFileList(char* fileList) - Cannot open input file! " << endl;
    mReaderStatus = ioERROpen;
    return;
  }
  char* temp;
  if (mDebug>1) cout << " inputStream->good() : " << inputStream->good() << endl;
  for (;inputStream->good();) {
    temp = new char[200];
    inputStream->getline(temp,200);
    cout << temp << endl;
    StHbtString* newFile = new StHbtString(temp);
    if ( newFile->length()>0 ) { 
      mFileList->push_back(newFile);
      if (mDebug>1) cout << "    file " << newFile->c_str() << " added to file list " << endl;
    }
  }
  if (mDebug) cout << " StHbtBinaryReader::FillFileList(char* fileList) - " << mFileList->empty() << " files in list " << endl;
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

