/**********************************************************
 *
 * Here are defined the input/output stream operators for
 *  StHbtEvent and associated classes
 *
 *********************************************************/
#include "StHbtMaker/Infrastructure/StHbtIOBinary.hh"

typedef unsigned int colSizeType;

StHbtIOBinary::StHbtIOBinary(const char* dirName, const char* fileName, const char* appendix, const char* readWrite) {
  //cout << " StHbtIOBinary(const char* fileName, const char* readWrite " << endl;
  byteCounterTotal=0;
  byteCounterEvent=0;
  if (((*readWrite)=='w')|| ((*readWrite)=='W')){  // this object will be a writer
    mOStream = new ofstream;
    mOStream->open( parseDirFile(dirName, fileName, appendix) );
    if (!mOStream){
      cout << "StHbtIOBinary::Init() - Cannot open output file! " << endl;
      exit(-1);
    }
    cout << "StHbtIOBinary::Init() - being configured as a Writer " << readWrite << endl;
  }
  else {
    mIStream = new ifstream;
    mIStream->open( parseDirFile( dirName, fileName, appendix) ); 
    if (!mIStream){
      cout << "StHbtIOBinary::Init() - Cannot open input file! " << endl;
      exit(-1);
    }
    cout << fileName << " open " << endl;
    cout << "StHbtIOBinary::Init() - being configured as a Reader " << readWrite << endl;
  }
}

StHbtIOBinary::~StHbtIOBinary() {
  cout << " StHbtIOBinary::~StHbtIOBinary() " << endl;
  if (mOStream) {
    mOStream->close();
    delete mOStream;
  }  
  if (mIStream) {
    mIStream->close();
    delete mIStream;
  }
}

int StHbtIOBinary::bytesWritten() { return byteCounterTotal; }
int StHbtIOBinary::bytesRead() { return byteCounterTotal; }

//------------------------- string ----------------------------------
void StHbtIOBinary::writeString(StHbtString& Message){
  *mOStream << Message.c_str();
  *mOStream << endl;
  *mOStream << "-*-*-*-* End of Input Reader Report" << endl;  // write THIS out even if there is no report

}
void StHbtIOBinary::readString(StHbtString& Message){
  char temp[200] = "";
  string stemp;
  do {
    Message += temp;
    Message += "\n";
    mIStream->getline(temp,200);
    stemp = temp;
  } while (stemp != "-*-*-*-* End of Input Reader Report");
  cout << "Here is the message that was at the beginning of the file...\n";
  cout << Message.c_str();
}
//------------------------- StHbtTrack -----------------------------------
int StHbtIOBinary::writeTrack(StHbtTrack& trk){
  mOStream->write( (char*)&trk, sizeof(trk) );
  byteCounterEvent+=sizeof(trk);
  //cout << " track size " << sizeof(trk) << endl;
  return ioOK;
}
int StHbtIOBinary::readTrack(StHbtTrack& trk){
  mIStream->read( (char*)&trk, sizeof(trk) );
  byteCounterEvent+=sizeof(trk);
  //cout << " track size " << sizeof(trk) << endl;
  return ioOK;
}
//------------------------- StHbtV0 --------------------------------------
int StHbtIOBinary::writeV0(StHbtV0& v0){
  mOStream->write( (char*)&v0, sizeof(v0) );
  byteCounterEvent+=sizeof(v0);
  //cout << " track size " << sizeof(v0) << endl;
  return ioOK;
}
int StHbtIOBinary::readV0(StHbtV0& v0){
  mIStream->read( (char*)&v0, sizeof(v0) );
  byteCounterEvent+=sizeof(v0);
  //cout << " track size " << sizeof(v0) << endl;
  return ioOK;
}
//------------------------- StHbtEvent -----------------------------------
int StHbtIOBinary::writeEvent(StHbtEvent& ev){
  cout << "StHbtIOBinary::writeEvent(StHbtEvent& ev) " << endl;
  byteCounterEvent=0;
  //event properties
  byteCounterEvent += binaryWrite( mOStream, ev.EventNumber()        );
  byteCounterEvent += binaryWrite( mOStream, ev.CtbMult()            );
  byteCounterEvent += binaryWrite( mOStream, ev.ZdcAdcEast()         );
  byteCounterEvent += binaryWrite( mOStream, ev.ZdcAdcWest()         );
  byteCounterEvent += binaryWrite( mOStream, ev.NumberOfTpcHits()    );
  byteCounterEvent += binaryWrite( mOStream, ev.NumberOfTracks()     );
  byteCounterEvent += binaryWrite( mOStream, ev.NumberOfGoodTracks() );
  byteCounterEvent += binaryWrite( mOStream, ev.ReactionPlane()      );
  byteCounterEvent += binaryWrite( mOStream, ev.ReactionPlaneError() );
  byteCounterEvent += binaryWrite( mOStream, ev.PrimVertPos()        );
  // tracks
  colSizeType colSize;
  colSize = (colSizeType) ev.TrackCollection()->size();
  byteCounterEvent += binaryWrite( mOStream,  colSize );
  StHbtTrack trk;
  for (StHbtTrackIterator iter=ev.TrackCollection()->begin();
       iter != ev.TrackCollection()->end(); iter++){
    writeTrack(**iter);
  } 
  // v0 tracks 
  colSize = (colSizeType) ev.V0Collection()->size();
  byteCounterEvent += binaryWrite( mOStream,  colSize );
  StHbtV0 v0;
  for (StHbtV0Iterator iterv0=ev.V0Collection()->begin();
       iterv0 != ev.V0Collection()->end(); iterv0++){
    writeV0(**iterv0);
  } 
  byteCounterTotal+=byteCounterEvent;
  return ioOK;
}

//------------------------- StHbtEvent -----------------------------------
int StHbtIOBinary::readEvent(StHbtEvent& ev){
  cout << "StHbtIOBinary::readEvent(StHbtEvent& ev) " << endl;
  byteCounterEvent=0;

  unsigned short eventNumber;           //
  unsigned short ctbMultiplicity;       // Central Trigger Barrel
  unsigned short zdcAdc[2];       // Zero-degree calorimeter 
                                         //values east/west
  int tpcNhits;                         // number of TPC hits
  unsigned short numberOfTracks;     // total number of TPC tracks
  unsigned short numberOfGoodTracks; // number of "good" tracks
  float reactionPlane[2]; //reaction plane/error  //   
  StHbtThreeVector primVertPos;

  byteCounterEvent += binaryRead( mIStream, eventNumber);
  if (mIStream->eof()) {
    cout << "Hit end of file " << endl;
    return ioEOF; 
  } 
  ev.SetEventNumber(eventNumber);

  byteCounterEvent += binaryRead( mIStream, ctbMultiplicity);  ev.SetCtbMult(ctbMultiplicity);
  byteCounterEvent += binaryRead( mIStream, zdcAdc[0]);            ev.SetZdcAdcEast(zdcAdc[0]);
  byteCounterEvent += binaryRead( mIStream, zdcAdc[1]);            ev.SetZdcAdcWest(zdcAdc[1]);
  byteCounterEvent += binaryRead( mIStream, tpcNhits);             ev.SetNumberOfTpcHits(tpcNhits);
  byteCounterEvent += binaryRead( mIStream, numberOfTracks);       ev.SetNumberOfTracks(numberOfTracks);
  byteCounterEvent += binaryRead( mIStream, numberOfGoodTracks);   ev.SetNumberOfGoodTracks(numberOfGoodTracks);
  byteCounterEvent += binaryRead( mIStream, reactionPlane[0]);     ev.SetReactionPlane(reactionPlane[0]);
  byteCounterEvent += binaryRead( mIStream, reactionPlane[1]);     ev.SetReactionPlaneError(reactionPlane[1]);
  byteCounterEvent += binaryRead( mIStream, primVertPos);          ev.SetPrimVertPos(primVertPos);

  // 
  //  OK, time to read in Track and V0 collections
  //
  if (!(mIStream->good())){
    cout << "StHbtEvent input operator finds stream in bad state ! " << endl;
    return ioERR;
  } 
  //  ev.mTrackCollection = new StHbtTrackCollection; <-- NO!
  //  the TrackCollection is instantiated by constructor!!
  //
  // since this should *overwrite* any StHbtTracks in the
  // StHbtTrackCollection, let's erase any that might be there
  //
  StHbtTrackIterator iter;
  for (iter=ev.TrackCollection()->begin();iter!=ev.TrackCollection()->end();iter++){
    delete *iter;
  }
  // ok, now we have gotten rid of the tracks themselves.  Let's lose the pointers to those deleted tracks
  ev.TrackCollection()->clear();  // if this doesn't work then just delete the collection and make a new one.

  colSizeType NtracksInCollection;
  byteCounterEvent += binaryRead( mIStream, NtracksInCollection);
  cout << " reading " << NtracksInCollection << " tracks " << endl;
  for (unsigned int itrk=0; itrk <NtracksInCollection; itrk++){
    StHbtTrack* trk = new StHbtTrack;
    if ( readTrack(*trk) ){
      cout << "StHbtEvent input operator finds stream in bad state during track read ! ";
      cout << itrk << " of " << NtracksInCollection << " intended" << endl;
      return ioERR;
    }
    //cout << " track read " << endl;
    ev.TrackCollection()->push_back(trk);  // ?ok?
    //cout << " " << itrk << "/" << NtracksInCollection;
    //cout << " track pushed " << endl;
    //wait(10,"");
  }

  // now we should do the v0 collection...
  // since this should *overwrite* any StHbtV0s in the
  // StHbtV0Collection, let's erase any that might be there
  //
  StHbtV0Iterator iterv0;
  for (iterv0=ev.V0Collection()->begin();iterv0!=ev.V0Collection()->end();iterv0++){
   delete *iterv0;
  }
  // ok, now we have gotten rid of the v0s themselves.  Let's lose the pointers to those deleted v0ss
  ev.V0Collection()->clear();  // if this doesn't work then just delete the collection and make a new one.

  colSizeType NV0sInCollection;
  byteCounterEvent += binaryRead( mIStream, NV0sInCollection);
  cout << " reading " << NV0sInCollection << " V0s " << endl;
  if ( !(mIStream->good()) ) {
    cout << "StHbtEvent input operator finds stream in bad state ! " << endl;
    return ioERR;
  }
 
  for (unsigned int iv0=0; iv0<NV0sInCollection; iv0++){
    StHbtV0* v0 = new StHbtV0;
    if ( readV0(*v0) ){
      cout << "StHbtEvent input operator finds stream in bad state during v0 read ! ";
      cout << iv0 << " of " << NV0sInCollection << " intended" << endl;
      return ioERR;
    }
    ev.V0Collection()->push_back(v0);  // ?ok?
    //cout << " " << iv0;
  }
  byteCounterTotal+=byteCounterEvent;
  return ioOK;
} 

// **********************************************************************
char* StHbtIOBinary::parseDirFile(const char* dir, const char* file, const char* appendix) {
  if (dir) cout << dir << endl;
  if (file) cout << file << endl;
  if (appendix) cout << appendix << endl;

  if (!dir || !appendix) return file;  // no dir specified
  StHbtString theDir = (char*)dir;
  StHbtString theFile = (char*)file;
  StHbtString theAppendix = (char*)appendix;
  while ( theFile.find("/") != string::npos ) {
    cout << theFile.c_str() << " ";
    string::size_type pos =  theFile.find("/");
    cout << pos << endl;
    theFile.erase(0, pos+1 );
    cout << theFile.c_str() << endl;
  }
  cout << (theDir+theFile+theAppendix).c_str() << endl;
  return (theDir+theFile+theAppendix).c_str();
}
