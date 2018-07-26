/***************************************************************************
 *
 * $Id: StETofGeometry.h,v 1.1 2018/07/25 14:34:40 jeromel Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: Collection of geometry classes for the eTOF:
 *              - eTOF geometry constants
 *              - StETofNode: generic eTOF geometry object initialized via
 *                TGeoManager
 *              - StETofModule, StETofCounter inherit from StETofNode
 *              - StETofGeometry builds the geometry and features all
 *                necessary methods to match track helices with eTOF hits
 *
 ***************************************************************************
 *
 * $Log: StETofGeometry.h,v $
 * Revision 1.1  2018/07/25 14:34:40  jeromel
 * First version, reviewed Raghav+Jerome
 *
 *
 ***************************************************************************/
#ifndef STETOFGEOMETRY_H
#define STETOFGEOMETRY_H

#include <vector>
#include <string>

#include "TGeoManager.h"

#include "StThreeVectorD.hh"
#include "StHelixD.hh"

class TNamed;
//class TObject;

class StMaker;
class StETofGeomCounter;

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// ETofGeomConstants
// =================
//
///////////////////////////////////////////////////////////////////////////////////////////////////
namespace ETofGeomConst {

const int nSectors  = 12;
const int nPlanes   =  3;
const int nModules  = nSectors * nPlanes;
const int nCounters =  3;
const int nStrips   =  32;

const int sectorStart  = 13;
const int sectorStop   = 24;
const int zPlaneStart  =  1;
const int zPlaneStop   =  3; 
const int counterStart =  1;
const int counterStop  =  3; 
const int stripStart   =  1;
const int stripStop    =  32; 

// copied from ETofGeo0.xml
// these are the zplanes of the three modules starting with the closest to IP
const float zplanes[ 3 ] = { 209.0 - 489.01, 209.0 - 501.01, 209.0 - 513.01 };

}



///////////////////////////////////////////////////////////////////////////////////////////////////
//
// StETofNode
// ==========
//
///////////////////////////////////////////////////////////////////////////////////////////////////
class StETofNode : public TObject {

public:
    StETofNode() {}
    ~StETofNode() {}

    StETofNode( const TGeoPhysicalNode& gpNode );
    StETofNode( const TGeoPhysicalNode& gpNode, const float& dx, const float& dy );

    void	convertPos( StETofNode* from, const double* pos_from, StETofNode* to, double* pos_to );

    void    local2Master( const double* local,  double* master );
    void    master2Local( const double* master, double* local  );

    bool    isLocalPointIn(  const double* local );
    bool    isGlobalPointIn( const StThreeVectorD& global );

    void    buildMembers();        // function to fill member variables like center position, min/max eta or phi of the node

    double  calcPhi( const double& rel_local_y, const double& rel_local_x ); 
    double  calcEta( const double& rel_local_x );

    StThreeVectorD  calcCenterPos();
    StThreeVectorD  calcXYPlaneNormal();

    bool    helixCross( const StHelixD& helix, double& pathLength, StThreeVectorD& cross, double& theta );

    TGeoHMatrix*    geoMatrix()     const;
    TGeoBBox*       box()           const;
    double          phiMin()        const;
    double          phiMax()        const;
    double          etaMin()        const;
    double          etaMax()        const;
    StThreeVectorD  centerPos()     const;
    StThreeVectorD  xyPlaneNormal() const;

    
    static  void    debugOn();
    static  void    debugOff();
    static  bool    isDebugOn();

    virtual void    print( const Option_t* opt = "" ) const;

private:
    TGeoHMatrix*    mGeoMatrix; // 4x4 matrix containing the rotation matrix (3x3) and the translation vector
    TGeoBBox*       mBox;       // shape (bounding box) of the node
    double          mPhiMin;    // minimal phi at lower edge of node (closest to beamline) from -pi to pi
    double          mPhiMax;    // maximal phi -- " --
    double          mEtaMin;
    double          mEtaMax;
    StThreeVectorD  mCenter;
    StThreeVectorD  mNormal;
    // TODO: add apropriate get and set functions

    static bool     mDebug;     // control message printing of this class

    ClassDef( StETofNode, 1 )
};

    
inline TGeoHMatrix*     StETofNode::geoMatrix()     const { return mGeoMatrix; }
inline TGeoBBox*        StETofNode::box()           const { return mBox;       }

inline double           StETofNode::phiMin()        const { return mPhiMin; }
inline double           StETofNode::phiMax()        const { return mPhiMax; }
inline double           StETofNode::etaMin()        const { return mEtaMin; }
inline double           StETofNode::etaMax()        const { return mEtaMax; }

inline StThreeVectorD   StETofNode::centerPos()     const { return mCenter; }
inline StThreeVectorD   StETofNode::xyPlaneNormal() const { return mNormal; }


inline void  StETofNode::debugOn()   { mDebug = true;  }
inline void  StETofNode::debugOff()  { mDebug = false; }
    
inline bool  StETofNode::isDebugOn() { return mDebug;  }


///////////////////////////////////////////////////////////////////////////////////////////////////
//
// StETofGeomModule
// ================
//
///////////////////////////////////////////////////////////////////////////////////////////////////
class StETofGeomModule : public StETofNode {

public:
    StETofGeomModule() {}
    ~StETofGeomModule() {}

    StETofGeomModule( const TGeoPhysicalNode& gpNode, const int moduleId );
    
    void  addCounter( const TGeoPhysicalNode& gpNode, const int moduleId, const int counterId );  
    void  addCounter( const TGeoPhysicalNode& gpNode, const float& dx, const float& dy, const int moduleId, const int counterId );

    StETofGeomCounter* counter( const unsigned int i ) const;  

    int   numberOfCounters()  const;

    int   calcSector( const int moduleId );
    int   calcPlane(  const int moduleId );
    

    int   moduleIndex() const;
    int   sector()      const;
    int   plane()       const;


    static void debugOn();
    static void debugOff();
    static bool isDebugOn();

    virtual void    print( const Option_t* opt="" ) const;

private:
    int     mModuleIndex; // module index number (0-35)
    int     mSector;      // sector (13-24)
    int	    mPlane;       // z-plane (1-3)

    vector< StETofGeomCounter* > mETofCounter;	

    static bool mDebug;     // control message printing of this class


	ClassDef( StETofGeomModule, 1 )
};


inline int   StETofGeomModule::numberOfCounters() const { return mETofCounter.size(); }
inline int   StETofGeomModule::moduleIndex()      const { return mModuleIndex;        }
inline int   StETofGeomModule::sector()           const { return mSector;             }
inline int   StETofGeomModule::plane()            const { return mPlane;              }

inline void  StETofGeomModule::debugOn()   { mDebug = true;  }
inline void  StETofGeomModule::debugOff()  { mDebug = false; }
    
inline bool  StETofGeomModule::isDebugOn() { return mDebug;  }


///////////////////////////////////////////////////////////////////////////////////////////////////
//
// StETofGeomCounter
// =================
//
///////////////////////////////////////////////////////////////////////////////////////////////////
class StETofGeomCounter : public StETofNode {

public:
    StETofGeomCounter() {}
    ~StETofGeomCounter() {}

    StETofGeomCounter( const TGeoPhysicalNode& gpNode, const int moduleId, const int counterId );
    StETofGeomCounter( const TGeoPhysicalNode& gpNode, const float& dx, const float& dy, const int moduleId, const int counterId );
    
    void createGeomStrips();
    
    int  findStrip( const double* local );


    int calcSector( const int moduleId );
    int calcPlane(  const int moduleId );
    

    int     moduleIndex()   const;
    int     sector()        const;
    int     plane()         const;
    int     counterIndex()  const;

    static void debugOn();
    static void debugOff();
    static bool isDebugOn();

    virtual void    print( const Option_t *opt="" ) const;

private:
    int     mModuleIndex;   // module index number (0-35)
    int     mSector;        // sector (13-24)
    int     mPlane;         // z-plane (1-3)
    int     mCounterIndex;  // counter index (0-2)   
    
    float   mStripX[ ETofGeomConst::nStrips + 1 ]; // X range of strips


    static bool mDebug;   // control message printing of this class


    ClassDef( StETofGeomCounter, 1 )
};
    
inline int   StETofGeomCounter::moduleIndex()    const { return mModuleIndex;  }
inline int   StETofGeomCounter::sector()         const { return mSector;       }
inline int   StETofGeomCounter::plane()          const { return mPlane;        }
inline int   StETofGeomCounter::counterIndex()   const { return mCounterIndex; }

inline void  StETofGeomCounter::debugOn()   { mDebug = true;  }
inline void  StETofGeomCounter::debugOff()  { mDebug = false; }
    
inline bool  StETofGeomCounter::isDebugOn() { return mDebug;  }



///////////////////////////////////////////////////////////////////////////////////////////////////
//
// StETofGeometry
// ==============
//
///////////////////////////////////////////////////////////////////////////////////////////////////
class StETofGeometry : public TNamed {

public:
    StETofGeometry( const char* name = "etofGeo", const char* title = "simplified ETOF Geometry" );
    ~StETofGeometry();

    void init( StMaker* maker, TGeoManager* geoManager );

    bool isInitDone() const;
    bool setInitFlag( const bool initFlag );

    std::string formTGeoPath( const TGeoManager* geoManager, int plane, int sector, int counter = -1 );
    std::string formTGeoPath( const TGeoManager* geoManager, int plane, int sector, int counter, int gap );
    
    int calcModuleIndex( const int& sector, const int& plane ); 
    int calcVolumeIndex( const int& sector, const int& plane, const int& counter, const int& strip );

    StETofNode* findETofNode( const int moduleId, const int counter );

    void hitLocal2Master( const int moduleId, const int counter, const double* local,  double* master );

    StThreeVectorD helixCrossETofPlane( const StHelixD& helix );
    vector< int >  helixCrossSector( const StHelixD& helix );
    vector< int >  sectorAtPhi( const double& angle );

    bool    helixCrossCounter( const StHelixD& helix, vector< int >& idVec );  

    void    logPoint( const char* text, const StThreeVectorD& point );


    StETofGeomModule* module( const unsigned int i );
    unsigned int      nValidModules() const;



    static void debugOn();
    static void debugOff();
    static bool isDebugOn();


private:
    StETofGeomModule* mETofModule[ ETofGeomConst::nModules ];

    unsigned int      mNValidModules;  // amount of loaded modules
    bool              mInitFlag;       // flag of initialization, true if done


    static bool mDebug;   // control message printing of this class


    ClassDef( StETofGeometry, 1 )
};


inline bool         StETofGeometry::isInitDone()    const { return mInitFlag;      }
inline unsigned int StETofGeometry::nValidModules() const { return mNValidModules; }

inline bool  StETofGeometry::setInitFlag( const bool initFlag ) { return mInitFlag = initFlag; }



inline void  StETofGeometry::debugOn()   { mDebug = true;  }
inline void  StETofGeometry::debugOff()  { mDebug = false; }
    
inline bool  StETofGeometry::isDebugOn() { return mDebug;  }




#endif /// STETOFGEOMETRY_H
