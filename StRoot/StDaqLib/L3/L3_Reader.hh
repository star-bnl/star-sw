/*
 * L3 Reader
 *      
 *
 *   change log
 *
 *************************************************************************** 
*/
#ifndef L3_READER_HH
#define L3_READER_HH


//////////////////////////////////////////////  includes  //////////////////////
#include <string.h>
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
#include "L3.Banks.hh"

//////////////////////////////////  classes and structures  /////////

class Bank_L3_P;
class Bank_L3_GTD;
class Bank_L3_SECP;
class Bank_L3_SECTP;
class Bank_L3_SECCD;
class GlobalTrackReader;
class Sl3ClusterReader;
class Sl3TrackReader;
class L3_Reader;


// -------------- GlobalTrackReader ------------------------

class GlobalTrackReader {

public:
  globalTrack *getTrackList () { return tracks; };
  int getNumberOfTracks () { return nTracks; };
  int getNumberOfHits () { return nHits; };
  vertex getVertex () { return glbVertex; };

  int initialize ();

  GlobalTrackReader (L3_Reader *l3r);
  ~GlobalTrackReader () {};
  
private:
  Bank_L3_GTD *pL3GTD;
  globalTrack *tracks;
  int nTracks;
  int nHits;
  vertex glbVertex;

  L3_Reader *l3;
};


// ------------- Sl3ClusterReader -------------------------

class Sl3ClusterReader {

public:
  l3_cluster *getClusterList () { return cluster; }
  int getNumberOfClusters () { return nCluster; }

  int initialize ();

  Sl3ClusterReader (int sector, L3_Reader *l3r);
  ~Sl3ClusterReader () {};

private:
  Bank_L3_SECCD *pL3SECCD;
  l3_cluster *cluster;
  int nCluster;
  int sector;
  L3_Reader *l3;
};


// ------------- Sl3TrackReader ---------------------------

class Sl3TrackReader {

public:
  localTrack *getLocalTrackList () { return tracks; }
  int getNumberOfTracks () { return nTracks; }
  int getNumberOfHits () { return nHits; }
  int getCpuTime () { return cpuTime; }
  int getRealTime () { return realTime; }
  int getParameterSetId () {return paraSet; }
  vertex getVertex () { return locVertex; }

  int initialize ();
  Sl3TrackReader (int sector, L3_Reader *l3r);
  ~Sl3TrackReader () {};

private:
  Bank_L3_SECTP *pL3SECTP;
  Bank_L3_LTD *pL3LTD;
  localTrack *tracks;
  int nTracks;
  int nHits;
  int cpuTime;
  int realTime;
  int paraSet;
  vertex locVertex;
  int sector;
  L3_Reader *l3;

};


//-------------- L3_Reader --------------------------------

class L3_Reader {
  friend class EventReader;

public:
  L3_Reader(EventReader *er, Bank_L3_P *pL3P);
  ~L3_Reader(){};

  Bank_L3_P     *getL3_P () { return pBankL3P; };
  Bank_L3_GTD   *getL3_GTD ();
  Bank_L3_SECP  *getL3_SECP (int sector);     // numbering conv. sector = 1...24
  Bank_L3_SECTP *getL3_SECTP (int sector);
  Bank_L3_SECCD *getL3_SECCD (int sector);

  GlobalTrackReader *getGlobalTrackReader ();
  Sl3ClusterReader  *getSl3ClusterReader (int sector);
  Sl3TrackReader    *getSl3TrackReader (int sector);

  int errno();
  string errstr();

protected:
  // bank pointer, only pBankL3P is set by the constructor
  // the sector banks point to the last sector which was asked for
  Bank_L3_P *pBankL3P;
  Bank_L3_GTD *pBankL3GTD;
  Bank_L3_SECP *pBankL3SECP;
  Bank_L3_SECCD *pBankL3SECCD;
  Bank_L3_SECTP *pBankL3SECTP;

};


L3_Reader *getL3Reader(EventReader *er);


#endif
