/***************************************************************************
 *
 * $Id: L3_Reader.hh,v 1.3 2000/07/06 18:16:01 ward Exp $
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
 *
 ***************************************************************************
 *
 * $Log: L3_Reader.hh,v $
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

#define L3ERROR(x, text) {errnum = x; sprintf(errstr0,"ERROR: "text" %s::%d",__FILE__,__LINE__); }
#define L3secERROR(x, text, s) {errnum = x; sprintf(errstr0,"ERROR: "text" in sector %d, %s::%d",s,__FILE__,__LINE__); }
#define pL3secERROR(x, text, s) {l3->errnum = x; sprintf(l3->errstr0,"ERROR: "text" in sector %d, %s::%d",s,__FILE__,__LINE__); }

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

  int errorNo() { return errnum; };
  string errstr() { return string(errstr0); };

  int errnum;
  char errstr0[250];

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
