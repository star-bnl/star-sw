//:>------------------------------------------------------------------
//: FILE:       gl3Event.h
//: HISTORY:
//:              3dec1999 version 1.00
//:              3feb2000 add sector to addTracks of type 3
//:              6jul2000 add l3CoordinateTransformer
//:             17jul2000 move glR3HistoContainer to gl3Conductor
//:             10aug2000 remove bField, to be kept in one place only (in FtfPara para)
//:             13aug2000 replace trackMerging with maxSectorNForTrackMerging
//:<------------------------------------------------------------------
#ifndef GL3EVENT 
#define GL3EVENT 

#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "l3Coordinates.h"
#include "l3CoordinateTransformer.h"

#include "sizes.h"
#include "daqFormats.h"
//#include "trgStructures.h"

#include "l3TrgReader.h"

//#include "l3GeneralHeaders.h"
#include "FtfGeneral.h"
#include "gl3Track.h"
#include "gl3Hit.h"
#include "gl3Sector.h"
#include "FtfPara.h"
#include "gl3LMVertexFinder.h"
#include "gl3Histo.h"

#include "gl3EMC.h"
#ifdef OLD_DAQ_READER
#include <evpReader.hh>
#else /* OLD_DAQ_READER */
#include <DAQ_READER/daqReader.h>
#endif /* OLD_DAQ_READER */
#include "sizes.h"

#define GL3_READ_TPC_TRACKS   0x01
#define GL3_READ_TPC_CLUSTERS 0x02
#define GL3_READ_ALL          0xffff

class gl3Event {
 public:

    gl3Event( l3CoordinateTransformer* inTrans,
	      l3EmcCalibration* inBemcCalibint,
	      l3EmcCalibration* inEemcCalibint,
	      int mxHits=szGL3_default_mHits, int mxTracks=szGL3_default_mTracks);

    ~gl3Event( );

    int  setup ( int mHits=szGL3_default_mHits, int mTracks = szGL3_default_mTracks);
    int  resetEvent ( );
  
    // bField is taken from the datafile
    // if the datafile sc bank is invalid, 
    // defaultbField is used.
    //
    // if bField is set != 1000 
    // then it overrides the value in the datafile
    //
    int readFromEvpReader(daqReader *rdr, 
			  float bField=1000);

    void readClustersFromEvpReader(daqReader *rdr, int sector);
    int readITPCClustersFromEvpReader(daqReader *rdr, int sector);
	
    int sectorFirstHit[25];

    short       getBusy   ( ) { return busy; };
    gl3Track*   getTrack  ( int n );
    gl3Hit*     getHit    ( int n );
    gl3Sector*  getSector ( int n );
    
    int getNTracks       ( ) { return nTracks; };
    int getNMergedTracks ( ) { return nMergedTracks; };
    int getNBadTracks    ( ) { return nBadTracks; };
    int getNHits         ( ) { return nHits  ; };
    
    l3TrgReader *getTrgData() { return &trgData; };

    l3xyzCoordinate getVertex() {return vertex;};
    l3xyzCoordinate getLMVertex() {return lmVertex;};
    
    l3CoordinateTransformer* getCoordinateTransformer()
	{ return coordinateTransformer; };

    int getTrgCmd();
    int getTrgWord();
    int getCTB(int n);
    int getZDC(int n);
    double getZDCVertex();

    int getToken() { return trgData.token; };
    void setToken(int tk) {trgData.token = tk;};


    unsigned int getBXingLo();
    unsigned int getBXingHi();
    unsigned long long getBXing();

    void setHitProcessing ( int hitPro ) { hitProcessing = hitPro; };
    void setVertexFinderMethod ( int _in )   { vertexFinder = _in; };
    void setLMVertexFinder ( gl3LMVertexFinder* _in ) { lmv = _in; };

    void setMaxSectorNForTrackMerging  ( int _in ) 
	{ maxSectorNForTrackMerging  = _in; };

    void setBField ( float _bField ) 
	{ 
	  para.bField = fabs(_bField); 
	  para.bFieldPolarity = int(_bField/fabs(_bField));
	};

    void setCoordinateTransformer ( l3CoordinateTransformer* in )
	{ coordinateTransformer = in; };


    //int  readEventDescriptor ( EventDescriptor *descr);
    int  readL3Data  (L3_P* buffer);

/* #if ( FORMAT_VERSION == 0x12 ) */
/*     int  readEventDescriptor ( EventDescriptor *descr); */
/*     int  readTrgData (TrgSumData* trgSum, RawTrgDet* rawTrg); */
/* #elif ( FORMAT_VERSION == 0x20 ) */
/*     int  readEventDescriptor ( EvtDescData *descr); */
/*     int  readTrgData (TrgSumData* trgSum, RawTrgDet* rawTrg); */
/* #endif */

    int  finalizeReconstruction();

    int  readSectorHits   ( char* buffer, int nSectorTracks );
    int  readSectorTracks ( char* buffer );


    void addTracks ( short sector, int nTracks, local_track* track );
    int  makeVertex ();

    int fillTracks ( int maxBytes, char* buffer, unsigned int token );

    int  nMergableTracks;

    gl3EMC emc;
    
 public:

    // ############################################################
    // # Control parameters 
    // ############################################################
    int   hitProcessing;  // 0=does read hits
                          // 1=reassigns trackId in hits to
                          //   pass that info downstream
                          // 2=full hit unpacking for module use

    int  vertexFinder;    // which vertex finder to run:
                          // 0: none
                          // 1: default (internal) (author: Jens Berger)
                          // 2: low mult vertex finder (author: Jan 
                          //      Balewski)
                          // 3: both


    // Parameters for the internal vertex finder (Jens Berger)
    int minNoOfHitsOnTrackUsedForVertexCalc;
    double minMomUsedForVertexCalc;


    // ############################################################
    // # Data structures
    // ############################################################
    
    gl3Hit*    hit;
    gl3Track*  track;
    int*       trackIndex;     // to keep track of relationship between 
                               // orig. tracks and final tracks
    int        busy;
    int        maxSectorNForTrackMerging;
    int        maxTracks;
    int        maxTracksSector;
    int        nTracks;
    int        maxHits;
    int        nHits;
    int        nMergedTracks;
    int        nBadTracks;

    // trigger
/*     struct { */
/* 	int            token; */
/* 	unsigned int   trgCmd; */
/* 	unsigned int   trgWord; */
/* 	unsigned int   ZDC[16];  */
/* 	unsigned int   CTB[256]; */
/* 	unsigned int   bx_hi, bx_lo; */
/*     } trgData; */

    l3TrgReader trgData;

    // vertex calc results
    l3xyzCoordinate vertex, lmVertex;

    // ############################################################
    // # Other stuff
    // ############################################################
    
    FtfPara          para;
    FtfContainer*    trackContainer;
    gl3Sector        sectorInfo[24];
   
    // histos used for the vertex determination
    gl3Histo* hVz ;
    gl3Histo* hVx ;
    gl3Histo* hVy ;
    
    // Helper objects
    l3CoordinateTransformer* coordinateTransformer;
    gl3LMVertexFinder *lmv;

    static const int nSectors = 24;
   
};
#endif

