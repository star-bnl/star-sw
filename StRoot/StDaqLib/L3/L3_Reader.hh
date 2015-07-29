/***************************************************************************
 *
 * $Id: L3_Reader.hh,v 1.10 2015/07/29 01:34:57 smirnovd Exp $
 *
 * Author: Christof Struck, struck@star.physics.yale.edu
 ***************************************************************************
 *
 * Description: L3_Reader provides interface to access L3 raw data
 *              in a similar way to the other detector readers
 *
 *
 * change log:
 *   06 Jun 00 CS initial version
 *   25 Jul 00 CS added i960 cluster reader
 *   11 Sep 00 CS removed memory leak in L3_Reader
 *
 ***************************************************************************
 *
 * $Log: L3_Reader.hh,v $
 * Revision 1.10  2015/07/29 01:34:57  smirnovd
 * C++11 requires a space between literal and identifier
 *
 * Revision 1.9  2001/09/24 21:42:56  struck
 * cs: changed vertex info to float (unit [cm]) in Bank_L3_GTD
 *
 * Revision 1.8  2001/08/20 05:37:45  struck
 * removed naming conflicts with 'Stl3Utils/foreign/L3Formats.h'
 *
 * Revision 1.7  2001/07/17 19:16:11  struck
 * update to 2001 data format (backwards compatible)Z
 *
 * Revision 1.6  2001/06/21 19:24:50  struck
 * corrected typo in filenames
 *
 * Revision 1.5  2000/09/11 16:31:39  struck
 * removed memory leak in L3_Reader
 *
 * Revision 1.4  2000/07/26 02:12:28  struck
 * added i960 cluster reader
 *
 * Revision 1.3  2000/07/06 18:16:01  ward
 * Install L3 code from Christof Struck.
 *
 *
 **************************************************************************/
#ifndef L3_READER_HH
#define L3_READER_HH


//////////////////////////////////////////////  includes  ///////////
#include <string>
#if !defined ST_NO_NAMESPACES
using std::string;
#endif

#include <stdio.h>
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
#include "StDaqLib/GENERIC/Error.hh"
#include "L3_Banks.hh"

//////////////////////////////////  classes and structures  /////////

class Bank_L3_P;
class Bank_L3_SUMD;
class Bank_L3_GTD;
class Bank_L3_SECP;
class Bank_L3_SECTP;
class Bank_L3_SECCD;
class Bank_TPCSECLP;
class Bank_TPCRBCLP;
class Bank_TPCMZCLD;
class Gl3AlgorithmReader;
class GlobalTrackReader;
class Sl3ClusterReader;
class Sl3TrackReader;
class L3_Reader;

#define L3ERROR(x, text) {errnum = x; sprintf(errstr0,"ERROR: " text " %s::%d",__FILE__,__LINE__); }
#define L3secERROR(x, text, s) {errnum = x; sprintf(errstr0,"ERROR: " text " in sector %d, %s::%d",s,__FILE__,__LINE__); }
#define pL3secERROR(x, text, s) {mL3->errnum = x; sprintf(mL3->errstr0,"ERROR: " text " in sector %d, %s::%d",s,__FILE__,__LINE__); }

#define maxClusterPerSector 100000



//##########################################################
// -------------- Gl3AlgorithmReader -----------------------
//##########################################################

class Gl3AlgorithmReader {

public:
  Gl3AlgorithmReader(L3_Reader *l3);
  ~Gl3AlgorithmReader() {};

  int getNumberofAlgorithms() { return mNAlg; }
  unsigned int getNumberOfProcessedEvents() { return mNProcessed; }
  unsigned int getNumberOfReconstructedEvents() { return mNReconstructed; }
  Algorithm_Data *getAlgorithmData() { return mAlgData; }
  int initialize();

private:
  Bank_L3_SUMD *mL3SUMD;
  unsigned int mNProcessed;
  unsigned int mNReconstructed;
  int mNAlg;
  Algorithm_Data *mAlgData;

  L3_Reader *mL3r;
};



//##########################################################
// -------------- GlobalTrackReader ------------------------
//##########################################################

class GlobalTrackReader {

public:
  globalTrack *getTrackList() { return mTracks; }
  int getNumberOfTracks() { return mNTracks; }
  int getNumberOfHits() { return mNHits; }
  vertex getVertex() { return mGlobalVertex; }

  int initialize();

  GlobalTrackReader(L3_Reader *l3r);
  ~GlobalTrackReader() {};
  
private:
  Bank_L3_GTD *mL3GTD;
  globalTrack *mTracks;
  int mNTracks;
  int mNHits;
  vertex mGlobalVertex;

  L3_Reader *mL3;
};



//##########################################################
// ------------- Sl3ClusterReader -------------------------
//##########################################################

class Sl3ClusterReader {

public:
  L3_Cluster *getClusterList() { return mCluster; }
  int getNumberOfClusters() { return mNCluster; }

  int initialize(int sector);

  Sl3ClusterReader(L3_Reader *l3r);
  ~Sl3ClusterReader() {};

private:
  Bank_L3_SECCD *mL3SECCD;
  L3_Cluster *mCluster;
  int mNCluster;
  int mSector;
  L3_Reader *mL3;
};



//##########################################################
// ------------- Sl3TrackReader ---------------------------
//##########################################################

class Sl3TrackReader {

public:
  localTrack *getLocalTrackList() { return mTracks; }
  int getNumberOfTracks() { return mNTracks; }
  int getNumberOfHits() { return mNHits; }
  int getCpuTime() { return mCpuTime; }
  int getRealTime() { return mRealTime; }
  int getParameterSetId() {return mParaSet; }
  vertex getVertex() { return mSectorVertex; }

  int initialize(int sector);

  Sl3TrackReader(L3_Reader *l3r);
  ~Sl3TrackReader() {};

private:
  Bank_L3_SECTP *mL3SECTP;
  Bank_L3_LTD *mL3LTD;
  localTrack *mTracks;
  int mNTracks;
  int mNHits;
  int mCpuTime;
  int mRealTime;
  int mParaSet;
  vertex mSectorVertex;
  int mSector;
  L3_Reader *mL3;

};



//##########################################################
//-------------- I960ClusterReader ------------------------
//##########################################################

class I960ClusterReader {

public:
  L3_Cluster *getClusterList() { return mCluster; }
  int getNumberOfClusters() { return mNCluster; }

  int initialize(int sector);

  I960ClusterReader(L3_Reader *l3r);
  ~I960ClusterReader();

private:
  Bank_TPCMZCLD *mBankTPCMZCLD[12][3];  // pointers to banks of one sector
  L3_Cluster *mCluster;
  int mNCluster;
  int mSector;
  L3_Reader *mL3;

};



//##########################################################
//-------------- L3_Reader --------------------------------
//##########################################################

class L3_Reader {
  friend class EventReader;

public:
  L3_Reader(EventReader *er, Bank_L3_P *pL3P);
  ~L3_Reader();

  Bank_L3_P     *getL3_P() { return mBankL3P; }
  Bank_L3_SUMD  *getL3_SUMD();
  Bank_L3_GTD   *getL3_GTD();
  Bank_L3_SECP  *getL3_SECP(int sector);     // numbering conv. sector = 1...24
  Bank_L3_SECTP *getL3_SECTP(int sector);
  Bank_L3_SECCD *getL3_SECCD(int sector);
  Bank_TPCSECLP *getTPCSECLP(int sector);
  Bank_TPCRBCLP *getTPCRBCLP(int sector, int rb);
  Bank_TPCMZCLD *getTPCMZCLD(int sector, int rb, int mz);
  int *getSVT_Bank(int sector);       // 1...5 = 4 svt 'sectors' + 1 ssd
  int *getFTPC_Bank(int sector);      // 1...2 = east/west (or west/east??)
  int *getEMC_Bank();

  GlobalTrackReader  *getGlobalTrackReader();
  Gl3AlgorithmReader *getGl3AlgorithmReader();
  Sl3ClusterReader   *getSl3ClusterReader(int sector);
  Sl3TrackReader     *getSl3TrackReader(int sector);
  I960ClusterReader  *getI960ClusterReader(int sector);

  unsigned char getL3PFormatNumber() { return mBankL3P->header.FormatNumber; }
  unsigned int getTime() { return mTime; }
  unsigned int getGl3Id() { return mGl3Id; }
  L3_Summary *getL3_Summary() { return mL3sum; }

  int errorNo() { return errnum; };
  string errstr() { return string(errstr0); };

  int errnum;
  char errstr0[250];

protected:
  // bank pointer, only pBankL3P is set by the constructor
  // the sector banks point to the last sector which was asked for
  Bank_L3_P     *mBankL3P;
  Bank_L3_SUMD  *mBankL3SUMD;
  Bank_L3_GTD   *mBankL3GTD;
  Bank_L3_SECP  *mBankL3SECP;
  Bank_L3_SECCD *mBankL3SECCD;
  Bank_L3_SECTP *mBankL3SECTP;
  Bank_TPCSECLP *mBankTPCSECLP;
  Bank_TPCRBCLP *mBankTPCRBCLP;
  Bank_TPCMZCLD *mBankTPCMZCLD;
  // provide pointer to SVT/SSD, FTPC and EMC, nothing else
  int *mSVT;
  int *mFTPC;
  int *mEMC;

private:
  Gl3AlgorithmReader *mAlr;
  GlobalTrackReader  *mGtr;
  Sl3ClusterReader   *mScr;
  Sl3TrackReader     *mStr;
  I960ClusterReader  *mIcr;

  L3_Summary  *mL3sum;
  unsigned int mGl3Id;
  unsigned int mTime;


};


L3_Reader *getL3Reader(EventReader *er);


#endif
