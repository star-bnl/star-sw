/**********************************************************
 *
 * Here are defined the input/output stream operators for
 *  StHbtEvent and associated classes
 *
 *********************************************************/

#include "StHbtMaker/Infrastructure/StHbtIOBinary.hh"


StHbtIOBinary::StHbtIOBinary(const char* dirName, const char* fileName, const char* appendix, const char* readWrite) 
  : mDebug(0), mOStream(0), mIStream(0) {
  //cout << " StHbtIOBinary(const char* fileName, const char* readWrite " << endl;
  byteCounterTotal=0;
  byteCounterEvent=0;
  if (((*readWrite)=='w')|| ((*readWrite)=='W')){  // this object will be a writer
    mOStream = new ofstream( parseDirFile(dirName, fileName, appendix) );
    if ( !mOStream ){
      cout << "StHbtIOBinary::Init() - Cannot open output file! " << endl;
      exit(ioERROpen);
    }
    cout << "StHbtIOBinary::Init() - being configured as a Writer " << readWrite << endl;
  }
  else {
    mIStream = new ifstream( parseDirFile( dirName, fileName, appendix) ); 
    if ( !mIStream ){
      cout << "StHbtIOBinary::Init() - Cannot open input file! " << endl;
      exit(ioERROpen);
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

int StHbtIOBinary::readHeader(StHbtString& header){
  return readString( header );
}

int StHbtIOBinary::writeHeader(const StHbtString& header){
  return writeString( header );
}

int StHbtIOBinary::bytesWritten() { return byteCounterTotal; }
int StHbtIOBinary::bytesRead() { return byteCounterTotal; }

// **********************************************************************
const char* StHbtIOBinary::parseDirFile(const char* dir, const char* file, const char* appendix) {
  if (dir)      cout << " StHbtIOBinary::parseDirFile() - dir      " << dir << endl;
  if (file)     cout << " StHbtIOBinary::parseDirFile() - file     " << file << endl;
  if (appendix) cout << " StHbtIOBinary::parseDirFile() - appendix " << appendix << endl;

  if (!dir || !appendix) return file;  // no dir specified
  StHbtString theDir = (char*)dir;
  StHbtString theFile = (char*)file;
  StHbtString theAppendix = (char*)appendix;
  while ( theFile.find("/") != string::npos ) {
#ifdef STHBTDEBUG
    cout << theFile.c_str() << " ";
#endif
    string::size_type pos =  theFile.find("/");
#ifdef STHBTDEBUG
    cout << pos << endl;
#endif
    theFile.erase(0, pos+1 );
#ifdef STHBTDEBUG
    cout << theFile.c_str() << endl;
#endif
  }
  if(mDebug) cout << " StHbtIOBinary::parseDirFile() ---" << (theDir+theFile+theAppendix).c_str() << "---" <<  endl;
  return (theDir+theFile+theAppendix).c_str();
}

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "SystemOfUnits.h"
//------------------------- trackList ----------------------------------
int StHbtIOBinary::readTrackList(StHbtEvent& ev, unsigned short trVersion){
  int iret;
  ev.TrackCollection()->clear();
  colSizeType NtracksInCollection;
  iret =  read(NtracksInCollection);
  if(mDebug) cout << " reading " << NtracksInCollection << " tracks " << endl;
  if (NtracksInCollection > 1e6) {
    for ( int i=0; i<10; i++) {
      cout << " StHbtIOBinaryReader::readTrackList(...) - unreasonable number of tracks, returning ioERR " << endl;
    }
    return(ioERR);
  }
  StHbtThreeVector rVertexPosition(ev.PrimVertPos());
  StHbtThreeVector p;
  for (colSizeType itrk=0; itrk <NtracksInCollection; itrk++){
    StHbtTrack* trk = new StHbtTrack;
    iret =  read(  *trk, trVersion);
    ev.TrackCollection()->push_back(trk);  // ?ok?
    if(mDebug) {
      cout << " track read " << *trk << endl;
      cout << " " << itrk << "/" << NtracksInCollection;
      cout << " track pushed " << endl;
    }
  }
  
  return iret;
}
int StHbtIOBinary::writeTrackList(const StHbtEvent& ev, unsigned short trVersion){
  int iret;
  colSizeType colSize = (colSizeType) ev.TrackCollection()->size();
  iret =  write(  colSize );
  for (StHbtTrackIterator iter=ev.TrackCollection()->begin(); iter != ev.TrackCollection()->end(); iter++){
    iret =  write( **iter, trVersion);
  } 
  return iret;
}
//------------------------- v0list ----------------------------------
int StHbtIOBinary::readV0List(StHbtEvent& ev, unsigned short v0Version){
  int iret;
  ev.V0Collection()->clear(); 
  colSizeType NV0sInCollection;
  iret =  read( NV0sInCollection);
  if (mDebug) cout << " reading " << NV0sInCollection << " V0s " << endl;
  if ( !(mIStream->good()) ) {
    cout << "StHbtEvent input operator finds stream in bad state ! " << endl;
    return ioERR;
  }
  if (NV0sInCollection > 1e6) {
    for ( int i=0; i<10; i++) {
      cout << " StHbtIOBinaryReader::readV0List(...) - unreasonable number of V0s, returning ioERR " << endl;
    }
    return(ioERR);
  }
  for (unsigned int iv0=0; iv0<NV0sInCollection; iv0++){
    StHbtV0* v0 = new StHbtV0;
    iret =  read(  *v0, v0Version);
    ev.V0Collection()->push_back(v0);
  }
  return iret;
}
int StHbtIOBinary::writeV0List(const StHbtEvent& ev, unsigned short v0Version){
  int iret;
  colSizeType colSize = (colSizeType) ev.V0Collection()->size();
  iret =  write(  colSize );
  for (StHbtV0Iterator iterv0=ev.V0Collection()->begin(); iterv0 != ev.V0Collection()->end(); iterv0++){
    iret =  write( **iterv0, v0Version);
  } 
  return iret;
}


//------------------------- string ----------------------------------
int StHbtIOBinary::writeString(const StHbtString& Message){
  *mOStream << Message.c_str();
  *mOStream << endl;
  *mOStream << "-*-*-*-* End of Input Reader Report" << endl;  // write THIS out even if there is no report
  byteCounterEvent += Message.size();
  return (!mOStream->good());
}
int StHbtIOBinary::readString(StHbtString& Message){
  char temp[200] = "";
  Message = "";
  string stemp;
  do {
    Message += stemp;
    Message += "\n";
    mIStream->getline(temp,200);
    stemp = temp;
    //    cout << stemp.c_str() << endl;
  } while (stemp != "-*-*-*-* End of Input Reader Report" && mIStream->good() );
  cout << "Here is the message that was at the beginning of the file...\n";
  cout << Message.c_str();
  byteCounterEvent += Message.size();
  return (!mIStream->good());
}

//------------------------- StHbtEvent -----------------------------------
int StHbtIOBinary::read(StHbtEvent& ev, unsigned short evVersion, unsigned short trVersion, unsigned short v0Version){
 if (mDebug) cout << " StHbtIOBinary::read(StHbtEvent& ev, ...) - Versions: " << evVersion << " " << trVersion << " " << v0Version << endl;
  int iret;
  byteCounterEvent = 0;
  switch ( (int)evVersion ) {
  case 0: iret = read_V0( ev, trVersion, v0Version); break;
  case 1: iret = read_V1( ev, trVersion, v0Version); break;
  case 2: iret = read_V2( ev, trVersion, v0Version); break;
  default: 
    iret = ioERR;
    cout << "  StHbtIOBinary::read(StHbtEvent& ev, ...) -  can not read this event version " << endl;
    break;
  }
  byteCounterTotal += byteCounterEvent;
 if (mDebug) cout << " StHbtIOBinary::read(StHbtEvent& ev, ...) - byteCounterTotal= " << byteCounterTotal << "  byteCounterEvent = " << byteCounterEvent << endl;
  return iret;
} 

//------------------------- StHbtEvent -----------------------------------
int StHbtIOBinary::write( const StHbtEvent& ev, unsigned short evVersion, unsigned short trVersion, unsigned short v0Version){
  if (mDebug) cout << " StHbtIOBinary::write(StHbtEvent& ev, ...) -  Versions: " << evVersion << " " << trVersion << " " << v0Version << endl;
  int iret;
  byteCounterEvent = 0;
  switch ( (int)evVersion ) {
  case 0: iret = write_V0( ev, trVersion, v0Version); break;
  case 1: iret = write_V1( ev, trVersion, v0Version); break;
  case 2: iret = write_V2( ev, trVersion, v0Version); break;
  default: 
    iret = ioERR; 
    cout << "  StHbtIOBinary::write(StHbtEvent& ev, ...) - can not write this event version " << endl; 
    break;
  }
  byteCounterTotal += byteCounterEvent;
  if (mDebug) cout << "  StHbtIOBinary::write(StHbtEvent& ev, ...) - byteCounterTotal= " << byteCounterTotal << "  byteCounterEvent = " << byteCounterEvent << endl;
  return iret;
} 

//------------------------- StHbtTrack -----------------------------------
int StHbtIOBinary::read( StHbtTrack& x, unsigned short version){
  if (mDebug>1) cout << " StHbtIOBinary::readTrack() - Version : " << version << endl;
  int iret;
  switch ( (int)version ) {
  case 0: 
  case 1: iret = read_V1( x ); break;
  case 2: iret = read_V2( x ); break;
  default: 
    cout << " can not write this track version " << endl;
    return ioERR;
  }

  return ioOK;
}

//------------------------- StHbtTrack -----------------------------------
int StHbtIOBinary::write( const StHbtTrack& x, unsigned short version){
  if (mDebug>1)  cout << " StHbtIOBinary::writeTrack() - Version : " << version << endl;
  int iret;
  switch ( (int)version ) {
  case 0: 
  case 1: iret = write_V1( x ); break;
  case 2: iret = write_V2( x ); break;
  default: 
    cout << " can not write this track version " << endl;
    return ioERR;
  }
  return ioOK;
}

//------------------------- StHbtV0 -----------------------------------
int StHbtIOBinary::read( StHbtV0& x, unsigned short version){
  if (mDebug>1)  cout << " StHbtIOBinary::readV0(...) - Version : " << version << endl;
  int iret;
  switch ( (int)version ) {
  case 0: 
  case 1: iret = read_V1( x ); break;
  case 2: iret = read_V2( x);  break;
  case 3: iret = read_V3( x);  break;
  default: 
    cout << " can not read this V0 version " << endl;
    return ioERR;
  }

  return ioOK;
}

//------------------------- StHbtV0 -----------------------------------
int StHbtIOBinary::write(const StHbtV0& x, unsigned short version){
  if (mDebug>0)  cout << " StHbtIOBinary::writeV0(...) - Version : " << version << endl;
  int iret;
  switch ( (int)version ) {
  case 0: 
  case 1: iret = write_V1( x ); break;
  case 2: iret = write_V2( x ); break;
  case 3: iret = write_V3( x ); break;
  default: 
    cout << " can not write this V0 version " << endl;
    return ioERR;
  }
  return ioOK;
}


//------------------------- StHbtEvent Versions -----------------------------------
//------------------------- StHbtEvent Versions -----------------------------------
//------------------------- StHbtEvent Versions -----------------------------------
//------------------------- StHbtEvent Versions -----------------------------------
//------------------------- StHbtEvent Versions -----------------------------------
int StHbtIOBinary::read_V0(StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version ) {
  if (mDebug)  cout << " StHbtIOBinary::read_V0(StHbtEvent& event, unsigned short trVersion, unsigned short v0Version )" << endl;
  int iret;
  int idummy = 0;
  iret =  read( ev.mEventNumber);
  if (mIStream->eof()) {
    cout << "Hit end of file " << endl;
    return ioEOF; 
  } 
  iret =  read( ev.mCtbMultiplicity );
  iret =  read( ev.mZdcAdc[0]);
  iret =  read( ev.mZdcAdc[1]);
  iret =  read( ev.mTpcNhits);
  iret =  read( ev.mNumberOfTracks);
  iret =  read( ev.mNumberOfGoodTracks);
  iret =  read( ev.mReactionPlane[0]);
  iret =  read( ev.mReactionPlane[1]);
  iret =  read( idummy              );
  iret =  read( idummy              );
  iret =  read( idummy              );
  iret =  read( ev.mPrimVertPos);
  //#ifdef STHBTDEBUG
  cout << ev << endl;
  //#endif
  //  OK, time to read in Track and V0 collections
  if (!(mIStream->good())){
    cout << "StHbtEvent input operator finds stream in bad state ! " << endl;
    return ioERR;
  } 
  // tracks & v0s
  iret = readTrackList(ev,trVersion); if (iret!=ioOK) return iret;
  iret = readV0List(ev,v0Version);    if (iret!=ioOK) return iret;
  return ioOK;
};
int StHbtIOBinary::write_V0(const StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version){
 if (mDebug)  cout << " StHbtIOBinary::write_V0(const StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version) " << endl;
  int iret;
  int idummy = 0;
  //event properties
  iret =  write( ev.mEventNumber        );
  iret =  write( ev.mCtbMultiplicity    );
  iret =  write( ev.mZdcAdc[0]          );
  iret =  write( ev.mZdcAdc[1]          );
  iret =  write( ev.mTpcNhits           );
  iret =  write( ev.mNumberOfTracks     );
  iret =  write( ev.mNumberOfGoodTracks );
  iret =  write( ev.mReactionPlane[0]   );
  iret =  write( ev.mReactionPlane[1]   );
  iret =  write( idummy                 );
  iret =  write( idummy                 );
  iret =  write( idummy                 );
  iret =  write( ev.mPrimVertPos        );
  // tracks & v0s
  iret = writeTrackList(ev,trVersion);
  iret = writeV0List(ev,v0Version);
  return ioOK;
};
int StHbtIOBinary::read_V1(StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version ) {
  if (mDebug) cout << " StHbtIOBinary::read_V1(StHbtEvent& event, unsigned short trVersion, unsigned short v0Version )" << endl;
  int iret;
  iret =  read( ev.mEventNumber);
  if (mIStream->eof()) {
    cout << " StHbtIOBinary::read_V1(...) - Hit end of file " << endl;
    return ioEOF; 
  } 
  iret =  read( ev.mCtbMultiplicity );
  iret =  read( ev.mZdcAdc[0]);
  iret =  read( ev.mZdcAdc[1]);
  iret =  read( ev.mTpcNhits);
  iret =  read( ev.mNumberOfTracks);
  iret =  read( ev.mNumberOfGoodTracks);
  iret =  read( ev.mReactionPlane[0]);
  iret =  read( ev.mReactionPlane[1]);
  iret =  read( ev.mPrimVertPos);
#ifdef STHBTDEBUG
  cout << ev << endl;
#endif
  //  OK, time to read in Track and V0 collections
  if (!(mIStream->good())){
    cout << " StHbtIOBinary::read_V1(...) - StHbtEvent input operator finds stream in bad state ! " << endl;
    return ioERR;
  } 
  // tracks & v0s
  iret = readTrackList(ev,trVersion); if (iret!=ioOK) return iret;
  iret = readV0List(ev,v0Version);    if (iret!=ioOK) return iret;
  return ioOK;
};
int StHbtIOBinary::write_V1(const StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version){
  if (mDebug)  cout << " StHbtIOBinary::write_V1(const StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version) " << endl;
  int iret;
  //event properties
  iret =  write( ev.mEventNumber        );
  iret =  write( ev.mCtbMultiplicity    );
  iret =  write( ev.mZdcAdc[0]          );
  iret =  write( ev.mZdcAdc[1]          );
  iret =  write( ev.mTpcNhits           );
  iret =  write( ev.mNumberOfTracks     );
  iret =  write( ev.mNumberOfGoodTracks );
  iret =  write( ev.mReactionPlane[0]   );
  iret =  write( ev.mReactionPlane[1]   );
  iret =  write( ev.mPrimVertPos        );
  // tracks & v0s
  iret = writeTrackList(ev,trVersion);
  iret = writeV0List(ev,v0Version);
  return ioOK;
};
//------------------------- StHbtTrack Versions -----------------------------------
//------------------------- StHbtTrack Versions -----------------------------------
//------------------------- StHbtEvent Versions -----------------------------------
//------------------------- StHbtEvent Versions -----------------------------------
//------------------------- StHbtEvent Versions -----------------------------------
//------------------------- StHbtEvent Versions -----------------------------------
int StHbtIOBinary::read_V2(StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version ) {
  if (mDebug) cout << " StHbtIOBinary::read_V2(StHbtEvent& event, unsigned short trVersion, unsigned short v0Version )" << endl;
  int iret;
  iret =  read( ev.mEventNumber);
  if (mIStream->eof()) {
    cout << "Hit end of file " << endl;
    return ioEOF; 
  } 
  iret =  read( ev.mCtbMultiplicity );
  iret =  read( ev.mZdcAdc[0]);
  iret =  read( ev.mZdcAdc[1]);
  iret =  read( ev.mTpcNhits);
  iret =  read( ev.mNumberOfTracks);
  iret =  read( ev.mNumberOfGoodTracks);
  iret =  read( ev.mUncorrectedNumberOfPositivePrimaries);
  iret =  read( ev.mUncorrectedNumberOfNegativePrimaries);
  iret =  read( ev.mReactionPlane[0]);
  iret =  read( ev.mReactionPlane[1]);
  iret =  read( ev.mPrimVertPos);
#ifdef STHBTDEBUG
  cout << ev << endl;
#endif
  //  OK, time to read in Track and V0 collections
  if (!(mIStream->good())){
    cout << "StHbtEvent input operator finds stream in bad state ! " << endl;
    return ioERR;
  } 
  // tracks & v0s
  iret = readTrackList(ev,trVersion); if (iret!=ioOK) return iret;
  iret = readV0List(ev,v0Version);    if (iret!=ioOK) return iret;
  return ioOK;
};
int StHbtIOBinary::write_V2(const StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version){
  if (mDebug)  cout << " StHbtIOBinary::write_V2(const StHbtEvent& ev, unsigned short trVersion, unsigned short v0Version)" << endl;
  int iret;
  //event properties
  iret =  write( ev.mEventNumber        );
  iret =  write( ev.mCtbMultiplicity    );
  iret =  write( ev.mZdcAdc[0]          );
  iret =  write( ev.mZdcAdc[1]          );
  iret =  write( ev.mTpcNhits           );
  iret =  write( ev.mNumberOfTracks     );
  iret =  write( ev.mNumberOfGoodTracks );
  iret =  write( ev.mUncorrectedNumberOfPositivePrimaries);
  iret =  write( ev.mUncorrectedNumberOfNegativePrimaries);
  iret =  write( ev.mReactionPlane[0]   );
  iret =  write( ev.mReactionPlane[1]   );
  iret =  write( ev.mPrimVertPos        );
  // tracks & v0s
  iret = writeTrackList(ev,trVersion);
  iret = writeV0List(ev,v0Version);
  return ioOK;
};
//------------------------- StHbtTrack Versions -----------------------------------
//------------------------- StHbtTrack Versions -----------------------------------
//------------------------- StHbtTrack Versions -----------------------------------
//------------------------- StHbtTrack Versions -----------------------------------
int StHbtIOBinary::read_V1(StHbtTrack& x) {
  if (mDebug>1) cout << " StHbtIOBinary::read_V1(StHbtTrack&)  " << endl;
  return read(x);  // works only with in root
};
int StHbtIOBinary::write_V1(const StHbtTrack& x){
  if (mDebug>1) cout << " StHbtIOBinary::write_V1(const StHbtTrack&)" << endl;
  return write(x); // works only with in root
};
//------------------------- StHbtTrack Versions -----------------------------------
//------------------------- StHbtTrack Versions -----------------------------------
//------------------------- StHbtTrack Versions -----------------------------------
//------------------------- StHbtTrack Versions -----------------------------------
int StHbtIOBinary::read_V2(StHbtTrack& x) {
  int iret;
  iret = read( x.mCharge );
  iret = read( x.mNHits );
  iret = read( x.mNHitsPoss );
  iret = read( x.mNSigmaElectron  );
  iret = read( x.mNSigmaPion );
  iret = read( x.mNSigmaKaon );
  iret = read( x.mNSigmaProton );
  iret = read( x.mdEdx );
  iret = read( x.mDCAxy );
  iret = read( x.mDCAz );
  iret = read( x.mChiSqXY );
  iret = read( x.mChiSqZ );
  iret = read( x.mMap[0] );
  iret = read( x.mMap[1] );
  iret = read( x.mTrackId );
  iret = read( x.mPt );
  iret = read( x.mP );        // use template specialisation
  iret = read( x.mHelix );    // use template specialisation
  return ioOK;
};
int StHbtIOBinary::write_V2(const StHbtTrack& x) {
  int iret;
  iret = write( x.mCharge );
  iret = write( x.mNHits );
  iret = write( x.mNHitsPoss );
  iret = write( x.mNSigmaElectron  );
  iret = write( x.mNSigmaPion );
  iret = write( x.mNSigmaKaon );
  iret = write( x.mNSigmaProton );
  iret = write( x.mdEdx );
  iret = write( x.mDCAxy );
  iret = write( x.mDCAz );
  iret = write( x.mChiSqXY );
  iret = write( x.mChiSqZ );
  iret = write( x.mMap[0] );
  iret = write( x.mMap[1] );
  iret = write( x.mTrackId );
  iret = write( x.mPt );
  iret = write( x.mP );       // use template specialisation
  iret = write( x.mHelix );   // use template specialisation
  return ioOK;
};
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
int StHbtIOBinary::read_V1(StHbtV0& x) {
  if (mDebug>1) cout << " StHbtIOBinary::read_V1(StHbtV0&) " << endl;
  return read(x);  // works only with in root
};
int StHbtIOBinary::write_V1(const StHbtV0& x){
  if (mDebug>1)  cout << " StHbtIOBinary::write_V1(const StHbtV0&) " << endl;
  return write(x); // works only with in root
};
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
int StHbtIOBinary::read_V2(StHbtV0& x) {
  int iret;
  iret = read( x.mDecayLengthV0 );
  iret = read( x.mDecayVertexV0 );  
  iret = read( x.mDcaV0Daughters );
  iret = read( x.mDcaV0ToPrimVertex );
  iret = read( x.mDcaPosToPrimVertex );
  iret = read( x.mDcaNegToPrimVertex );
  iret = read( x.mMomPos );
  iret = read( x.mMomNeg );     
  iret = read( x.mTpcHitsPos );
  iret = read( x.mTpcHitsNeg );               
  iret = read( x.mRapLambda );
  iret = read( x.mRapK0Short );
  iret = read( x.mCTauLambda );
  iret = read( x.mCTauK0Short );
  iret = read( x.mKeyPos );
  iret = read( x.mKeyNeg );
  iret = read( x.mTrackTopologyMapPos[0] );
  iret = read( x.mTrackTopologyMapPos[1] );
  iret = read( x.mTrackTopologyMapNeg[0] );
  iret = read( x.mTrackTopologyMapNeg[1] );
  //  cout << " pos track id = " << x.midPos << "   neg track id = " << x.midNeg << endl; 
  x.UpdateV0();
  return ioOK;
};
int StHbtIOBinary::write_V2(const StHbtV0& x) {
  int iret;
  iret = write( x.mDecayLengthV0 );
  iret = write( x.mDecayVertexV0 );  
  iret = write( x.mDcaV0Daughters );
  iret = write( x.mDcaV0ToPrimVertex );
  iret = write( x.mDcaPosToPrimVertex );
  iret = write( x.mDcaNegToPrimVertex );
  iret = write( x.mMomPos );
  iret = write( x.mMomNeg );     
  iret = write( x.mTpcHitsPos );
  iret = write( x.mTpcHitsNeg );               
  iret = write( x.mRapLambda );
  iret = write( x.mRapK0Short );
  iret = write( x.mCTauLambda );
  iret = write( x.mCTauK0Short );
  iret = write( x.mKeyPos );
  iret = write( x.mKeyNeg );
  iret = write( x.mTrackTopologyMapPos[0] );
  iret = write( x.mTrackTopologyMapPos[1] );
  iret = write( x.mTrackTopologyMapNeg[0] );
  iret = write( x.mTrackTopologyMapNeg[1] );
  //  cout << " pos track id = " << x.midPos << "   neg track id = " << x.midNeg << endl; 
  return ioOK;
};
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
//------------------------- StHbtV0 Versions -----------------------------------
int StHbtIOBinary::read_V3(StHbtV0& x) {
  int iret;
  iret = read( x.mDecayLengthV0 );
  iret = read( x.mDecayVertexV0 );  
  iret = read( x.mDcaV0Daughters );
  iret = read( x.mDcaV0ToPrimVertex );
  iret = read( x.mDcaPosToPrimVertex );
  iret = read( x.mDcaNegToPrimVertex );
  iret = read( x.mMomPos );
  iret = read( x.mMomNeg );     
  iret = read( x.mTpcHitsPos );
  iret = read( x.mTpcHitsNeg );               
  iret = read( x.mRapLambda );
  iret = read( x.mRapK0Short );
  iret = read( x.mCTauLambda );
  iret = read( x.mCTauK0Short );
  iret = read( x.mKeyPos );
  iret = read( x.mKeyNeg );
  iret = read( x.mTrackTopologyMapPos[0] );
  iret = read( x.mTrackTopologyMapPos[1] );
  iret = read( x.mTrackTopologyMapNeg[0] );
  iret = read( x.mTrackTopologyMapNeg[1] );
  iret = read( x.mDedxPos );
  iret = read( x.mDedxNeg );
  x.UpdateV0();
  return ioOK;
};
int StHbtIOBinary::write_V3(const StHbtV0& x) {
  int iret;
  iret = write( x.mDecayLengthV0 );
  iret = write( x.mDecayVertexV0 );  
  iret = write( x.mDcaV0Daughters );
  iret = write( x.mDcaV0ToPrimVertex );
  iret = write( x.mDcaPosToPrimVertex );
  iret = write( x.mDcaNegToPrimVertex );
  iret = write( x.mMomPos );
  iret = write( x.mMomNeg );     
  iret = write( x.mTpcHitsPos );
  iret = write( x.mTpcHitsNeg );               
  iret = write( x.mRapLambda );
  iret = write( x.mRapK0Short );
  iret = write( x.mCTauLambda );
  iret = write( x.mCTauK0Short );
  iret = write( x.mKeyPos );
  iret = write( x.mKeyNeg );
  iret = write( x.mTrackTopologyMapPos[0] );
  iret = write( x.mTrackTopologyMapPos[1] );
  iret = write( x.mTrackTopologyMapNeg[0] );
  iret = write( x.mTrackTopologyMapNeg[1] );
  iret = write( x.mDedxPos );
  iret = write( x.mDedxNeg );
  //  cout << " pos track id = " << x.midPos << "   neg track id = " << x.midNeg << endl; 
  return ioOK;
};








