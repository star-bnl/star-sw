//:>------------------------------------------------------------------
//: FILE:       gl3Event.h
//: HISTORY:
//:              3dec1999 version 1.00
//:              3feb2000 add sector to addTracks of type 3
//:              6jul2000 add St_l3_Coordinate_Transformer
//:             17jul2000 move glR3HistoContainer to gl3Conductor
//:             10aug2000 remove bField, to be kept in one place only (in FtfPara para)
//:             13aug2000 replace trackMerging with maxSectorNForTrackMerging
//:<------------------------------------------------------------------
#ifndef GL3EVENT 
#define GL3EVENT 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* #include <sys/types.h> */
/* #include <netinet/in.h> */
/* #include <sys/socket.h> */
/* #include <sys/wait.h> */
/* #include <unistd.h> */
/* #include <fcntl.h> */

#include "Stl3Util/base/St_l3_Coordinates.h"
#include "Stl3Util/base/St_l3_Coordinate_Transformer.h"

// DIFFERENT INCLUDE STATEMENTS FOR ON-/OFFLINE

// online
//#include "daqFormats.h"
//#include "trgStructures.h"

// offline
#include "Stl3Util/foreign/daqFormats.h"
#include "StDaqLib/TRG/trgStructures.h"

//#include "l3GeneralHeaders.h"
#include "Stl3Util/ftf/FtfGeneral.h"
#include "Stl3Util/ftf/FtfPara.h"

#include "Stl3Util/gl3/gl3Track.h"
#include "Stl3Util/gl3/gl3Hit.h"
#include "Stl3Util/gl3/gl3Sector.h"
#include "Stl3Util/gl3/gl3Histo.h"



class gl3Event {
 public:

    gl3Event(int mxHits=500000, int mxTracks=20000,
	     St_l3_Coordinate_Transformer* inTrans=0 );

    ~gl3Event( );

    int  setup ( int mHits=600000, int mTracks = 20000 );
    int  resetEvent ( );
    
    short       getBusy   ( ) { return busy; };
    gl3Track*   getTrack  ( int n );
    gl3Hit*     getHit    ( int n );
    gl3Sector*  getSector ( int n );
    
    int getNTracks       ( ) { return nTracks; };
    int getNMergedTracks ( ) { return nMergedTracks; };
    int getNBadTracks    ( ) { return nBadTracks; };
    int getNHits         ( ) { return nHits  ; };
    
    St_l3_xyz_Coordinate getVertex() {return vertex;};
    
    St_l3_Coordinate_Transformer* getCoordinateTransformer()
	{ return coordinateTransformer; };

    int getTrgCmd();
    int getTrgWord();
    int getCTB(int n);
    int getZDC(int n);

    void setHitProcessing ( int hitPro ) 
	{ hitProcessing = hitPro; };

    void setMaxSectorNForTrackMerging  ( int _in ) 
	{ maxSectorNForTrackMerging  = _in; };

    void setBField ( float _bField ) 
	{ 
	    para.bField = _bField; 
	    para.bFieldPolarity = int(_bField/fabs(_bField));
	};

    void setCoordinateTransformer ( St_l3_Coordinate_Transformer* in )
	{ coordinateTransformer = in; };
    
    int  readEventDescriptor ( EventDescriptor *descr);
    int  readL3Data  (L3_P* buffer);
    int  readTrgData (TrgSumData* trgSum, RawTrgDet* rawTrg);

    int  readSectorHits   ( char* buffer, int nSectorTracks );
    int  readSectorTracks ( char* buffer );
    void addTracks ( short sector, int nTracks, local_track* track );
    void makeVertex ();
    
    int fillTracks ( int maxBytes, char* buffer, unsigned int token );

    int  nMergableTracks;
    

private:
    static const int NSECTORS = 24;

    int       hitProcessing;  // 0=does read hits
                              // 1=reassigns trackId in hits to
                              //   pass that info downstream
                              // 2=full hit unpacking for module use
   gl3Hit*    hit;
   gl3Track*  track;
   int*       trackIndex;     // to keep track of relation ship between 
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

   struct trgDataType {
       int trgCmd;
       int trgWord;
       int ZDC[16]; 
       int CTB[240];
   } trgData;


   St_l3_Coordinate_Transformer* coordinateTransformer;
   
   FtfPara          para;
   FtfContainer*    trackContainer;
   gl3Sector        sectorInfo[NSECTORS];
   
   // ******************************************************************
   // ol'fuckin vertex stuff
   // JB 07/03/01
   // ******************************************************************

   int minNoOfHitsOnTrackUsedForVertexCalc;
   double minMomUsedForVertexCalc;
    
   // nice weighted mean helper function, cause we DON'T want to fit!
   double getWeightedMean(gl3Histo* hist, double sigmaWidthBins);   
   
   // histos used for the vertex determination
   gl3Histo* hVz ;
   gl3Histo* hVx ;
   gl3Histo* hVy ;
    
    // 3d coordinate 
    St_l3_xyz_Coordinate vertex;
};
#endif

