/***************************************************************************
 * StFttDb.h
 * jdb Feb, 2022
 ***************************************************************************
 *
 * Description: FTT DB Utility
 ***************************************************************************/

#ifndef STFTTDB_H
#define STFTTDB_H

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StEvent/StEnumerations.h"
#include <stdint.h>
#include <map>


class StFttRawHit;
class StFttCluster;
class StFttPoint;

struct FttDataWindow {
    UChar_t uuid;
    UChar_t mode;
    Short_t min;
    Short_t max;
    Short_t anchor;
};


class St_fttHardwareMap;
class St_fttDataWindows;

class StFttDb : public TDataSet {

public: 
  StFttDb(const char *name="fttDb");
  virtual       ~StFttDb();
  int  Init();
  int  InitRun(int runNumber);
 
  void SetDebug(int v=1) {mDebug=v;}  //! debug level
  void setDebug(int v=1) {mDebug=v;}  //! debug level
  void setDbAccess(int v=1);  //! enable(1) or disable(0) offline DB access
  void setRun(int run);       //! set run# 

  static size_t uuid( StFttRawHit * h, bool includeStrip = false ) ;
  static size_t uuid( StFttCluster * c ) ;

  // HARDWARE Mapping StFttRawHits
    uint16_t packKey( int feb, int vmm, int ch ) const;
    void unpackKey( int key, int &feb, int &vmm, int &ch ) const;
    uint16_t packVal( int row, int strip ) const;
    void unpackVal( int val, int &row, int &strip ) const;
    void loadHardwareMapFromFile( std::string fn );
    bool loadStripCenterFromFile( std::string fn );
    bool loadStripEdgeFromFile( std::string fn );
    void loadHardwareMapFromDb( St_fttHardwareMap * );
    void loadDataWindowsFromFile( std::string fn );
    void loadDataWindowsFromDb( St_fttDataWindows * );


    UChar_t plane( StFttRawHit * hit );
    UChar_t quadrant( StFttRawHit * hit );
    UChar_t fob( StFttRawHit * hit );
    UChar_t rob( StFttRawHit * hit );
    UChar_t rob( StFttCluster * clu );

    static double stripPitch; // mm
    static double rowLength; // mm
    static double HVStripShift; // mm
    static double DiagStripShift; // mm
    static double lowerQuadOffsetX; //mm
    static double idealPlaneZLocations[4];

    static const size_t nPlane        = 4;
    static const size_t nQuadPerPlane = 4;
    static const size_t nFobPerQuad   = 6;
    static const size_t nVMMPerFob    = 4;
    static const size_t nChPerVMM     = 64;

    static const size_t nQuad = nQuadPerPlane * nPlane; // 4 * 4 = 16 Total number of Quadrants
    static const size_t nRob = nQuad;

    static const size_t nFobPerPlane = nFobPerQuad * nQuadPerPlane; // 4 * 6 = 24 Total number of FOB
    static const size_t nFob         = nFobPerPlane * nPlane; // 24 * 4 = 96 total FOB;

    static const size_t nVMMPerQuad  = nFobPerQuad * nVMMPerFob; // 6 * 4 = 24 VMM per Quad
    static const size_t nVMMPerPlane = nVMMPerQuad * nQuadPerPlane; // 24 * 4 = 96 VMM per Plane
    static const size_t nVMM         = nVMMPerPlane * nPlane; // 96 * 4 = 384 VMM total

    static const size_t nChPerFob    = nVMMPerFob * nChPerVMM; // 4 * 64 = 256 VMM per Plane
    static const size_t nChPerQuad   = nChPerFob * nFobPerQuad; // 256 * 6 = 1536 VMM per Plane
    static const size_t nChPerPlane  = nChPerQuad * nQuadPerPlane; // 1536 * 4 = 6144 Ch per Plane
    static const size_t nCh          = nChPerPlane * nPlane; // 6144 * 4 = 24576 Ch per Plane

    static const size_t maxADC       = 1024 + 1; // 10 bits;
    static const size_t maxBCID      = 4096 + 1; // 12 bits;
    static const int minTb           = -32768 - 1000; // get the under and overflow
    static const int maxTb           = 32768 + 1000;
    
    static const size_t nRowsPerQuad       = 5; // 0 - 2 (Horizontal or Vertical) + 3, 4 for Diagonal
    static const size_t nStripOrientations = 5; // real orientations (4) + 1 for unkown                       = 5 total
    static const size_t maxStripPerRow     = 166; // max number of strips in a row (only row 0 has this many)
    
    static vector<string> orientationLabels;



    UChar_t orientation( StFttRawHit * hit );
    UChar_t getOrientation( int rob, int feb, int vmm, int row ) const;
    bool hardwareMap( int rob, int feb, int vmm, int ch, int &row, int &strip, UChar_t &orientation ) const;
    bool hardwareMap( StFttRawHit * rawHit ) const;

    void getGloablOffset( UChar_t plane, UChar_t quad, float &dx, float &sx, float &dy, float &sy, float &dz, float &sz );

    enum TimeCutMode {
      CalibratedBunchCrossingMode = 0,
      TimebinMode = 1
    };

    void setTimeCut(TimeCutMode mode, int low=-70, int high=30)
    {
        mUserDefinedTimeCut = true;
        mTimeCutMode=mode; 
        mTimeCutLow=low; 
        mTimeCutHigh=high;
    }

    void getTimeCut( StFttRawHit * hit, int &mode, int &l, int &h ){
        mode = mTimeCutMode;
        l = mTimeCutLow;
        h = mTimeCutHigh;
        if (mUserDefinedTimeCut)
            return;

        // load calibrated data windows from DB 
        size_t hit_uuid = uuid( hit );
        if ( dwMap.count( hit_uuid ) ){
            mode = dwMap[ hit_uuid ].mode;
            l = dwMap[ hit_uuid ].min;
            h = dwMap[ hit_uuid ].max;
        }
    
    }

 private:
  int   mDbAccess=1;                     //! enable(1) or disabe(0) DB access
  int   mRun=0;                          //! run#
  int   mDebug=0;                        //! >0 dump tables to text files    

  bool mUserDefinedTimeCut = false;
  TimeCutMode mTimeCutMode;
  int mTimeCutLow, mTimeCutHigh;

  std :: map< uint16_t , uint16_t > mMap;
  std :: map< uint16_t , uint16_t > rMap; // reverse map 
  //  data windows map
  std :: map< uint16_t , FttDataWindow > dwMap;
  std :: map< int , Float_t > scMapXY; // strip center map 
  std :: map< int , Float_t > scMapDiag; // strip center map 
  std :: map< int , Float_t > seMapDiagLeft; // strip left edge map, just for the diagonal strips 
  std :: map< int , Float_t > seMapDiagRight; // strip right edge map, just for the diagonal strips

  ClassDef(StFttDb,1)   //StAF chain virtual base class for Makers        
};

#endif
  

