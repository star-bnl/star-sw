/***************************************************************************
 *
 * $Id: StETofGeometry.h,v 1.4 2019/12/10 16:03:43 fseck Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: Collection of geometry classes for the eTOF:
 *              - StETofNode: generic eTOF geometry object initialized via
 *                TGeoManager
 *              - StETofGeomModule, StETofGeomCounter inherit from StETofNode
 *              - StETofGeometry builds the geometry and features all
 *                necessary methods to match track helices with eTOF hits
 *
 ***************************************************************************
 *
 * $Log: StETofGeometry.h,v $
 * Revision 1.4  2019/12/10 16:03:43  fseck
 * added handling of StPicoHelix in extrapolation & step-wise extrapolation in changing magnetic field
 *
 * Revision 1.3  2019/04/23 23:49:08  fseck
 * added support for StPicoHelix
 *
 * Revision 1.2  2019/02/19 20:20:11  fseck
 * update after second part of eTOF code review
 *
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
#include "StPhysicalHelixD.hh"

#include "TVector3.h"
#include "StPicoEvent/StPicoPhysicalHelix.h"
#include "StPicoEvent/StPicoHelix.h"

#include "StETofUtil/StETofConstants.h"

class TNamed;

class StETofHit;
class StMuETofHit;
class StPicoETofHit;

class StETofGeomCounter;
class StarMagField;


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

    void    convertPos( StETofNode* from, const double* pos_from, StETofNode* to, double* pos_to );

    void    local2Master( const double* local,  double* master );
    void    master2Local( const double* master, double* local  );

    bool    isLocalPointIn(  const double* local );
    bool    isGlobalPointIn( const StThreeVectorD& global );
    bool    isGlobalPointIn( const TVector3& global );

    void    buildMembers(); // function to fill member variables like center position, min/max eta or phi of the node

    double  calcPhi( const double& rel_local_y, const double& rel_local_x ); 
    double  calcEta( const double& rel_local_x );

    StThreeVectorD  calcCenterPos();
    StThreeVectorD  calcXYPlaneNormal();

    bool    helixCross( const StHelixD& helix, double& pathLength, StThreeVectorD& cross, double& theta );
    bool    helixCross( const StPicoHelix& helix, double& pathLength, TVector3& cross, double& theta );

    TGeoHMatrix*    geoMatrix()     const;
    TGeoBBox*       box()           const;
    double          phiMin()        const;
    double          phiMax()        const;
    double          etaMin()        const;
    double          etaMax()        const;
    StThreeVectorD  centerPos()     const;
    StThreeVectorD  xyPlaneNormal() const;

    void            setSafetyMargins( const double* margins );
    double          safetyMarginX() const;
    double          safetyMarginY() const;

    void            debugOn();
    void            debugOff();
    bool            isDebugOn()     const;

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
    double          mSafetyMarginX;
    double          mSafetyMarginY;

    bool            mDebug;     // control message printing of this class

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

inline double           StETofNode::safetyMarginX() const { return mSafetyMarginX; };
inline double           StETofNode::safetyMarginY() const { return mSafetyMarginY; };


inline void  StETofNode::debugOn()  { mDebug = true;  }
inline void  StETofNode::debugOff() { mDebug = false; }
    
inline bool  StETofNode::isDebugOn() const { return mDebug; }


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
    void  addCounter( const TGeoPhysicalNode& gpNode, const float& dx, const float& dy, const int moduleId, const int counterId, const double* safetyMargins );

    StETofGeomCounter* counter( const unsigned int i ) const;  

    void  clearCounters(); 

    int   numberOfCounters()  const;

    int   calcSector( const int moduleId );
    int   calcPlane(  const int moduleId );


    int   moduleIndex() const;
    int   sector()      const;
    int   plane()       const;

    void  debugOn();
    void  debugOff();
    bool  isDebugOn()   const;

    virtual void    print( const Option_t* opt="" ) const;

private:
    int     mModuleIndex; // module index number (0-35)
    int     mSector;      // sector (13-24)
    int	    mPlane;       // z-plane (1-3)

    std::vector< StETofGeomCounter* > mETofCounter;	

    bool    mDebug;       // control message printing of this class


    ClassDef( StETofGeomModule, 1 )
};


inline int   StETofGeomModule::numberOfCounters() const { return mETofCounter.size(); }
inline int   StETofGeomModule::moduleIndex()      const { return mModuleIndex;        }
inline int   StETofGeomModule::sector()           const { return mSector;             }
inline int   StETofGeomModule::plane()            const { return mPlane;              }

inline void  StETofGeomModule::debugOn()  { mDebug = true;  }
inline void  StETofGeomModule::debugOff() { mDebug = false; }
    
inline bool  StETofGeomModule::isDebugOn() const { return mDebug; }


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

    void    debugOn();
    void    debugOff();
    bool    isDebugOn()     const;

    virtual void    print( const Option_t *opt="" ) const;

private:
    int     mModuleIndex;   // module index number (0-35)
    int     mSector;        // sector (13-24)
    int     mPlane;         // z-plane (1-3)
    int     mCounterIndex;  // counter index (0-2)   

    float   mStripX[ eTofConst::nStrips + 1 ]; // X range of strips

    bool    mDebug;         // control message printing of this class


    ClassDef( StETofGeomCounter, 1 )
};
    
inline int   StETofGeomCounter::moduleIndex()    const { return mModuleIndex;  }
inline int   StETofGeomCounter::sector()         const { return mSector;       }
inline int   StETofGeomCounter::plane()          const { return mPlane;        }
inline int   StETofGeomCounter::counterIndex()   const { return mCounterIndex; }

inline void  StETofGeomCounter::debugOn()  { mDebug = true;  }
inline void  StETofGeomCounter::debugOff() { mDebug = false; }
    
inline bool  StETofGeomCounter::isDebugOn() const { return mDebug; }



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

    void init( TGeoManager* geoManager );
    void init( TGeoManager* geoManager, const double* safetyMargins );
    void init( TGeoManager* geoManager, const double* safetyMargins, const bool& useHelixSwimmer );

    void reset();

    bool isInitDone() const;
    bool setInitFlag( const bool initFlag );

    std::string formTGeoPath( const TGeoManager* geoManager, int plane, int sector, int counter = -1 );
    std::string formTGeoPath( const TGeoManager* geoManager, int plane, int sector, int counter, int gap );

    int calcModuleIndex( const int& sector, const int& plane ); 
    int calcVolumeIndex( const int& sector, const int& plane, const int& counter, const int& strip );

    void decodeVolumeIndex( const int& volumeId, int& sector, int& plane, int& counter, int& strip );

    StETofNode* findETofNode( const int moduleId, const int counter );

    void           hitLocal2Master( const int moduleId, const int counter, const double* local,  double* master );
    StThreeVectorD hitLocal2Master( StETofHit* hit );
    StThreeVectorD hitLocal2Master( StMuETofHit* hit );
    TVector3       hitLocal2Master( StPicoETofHit* hit );



    std::vector< int >  sectorAtPhi( const double& angle );

    //--------------------------------------------------------------------
    // for the use in the MatchMaker or in running over StEvent/MuDst input
    StThreeVectorD helixCrossETofPlane( const StHelixD& helix );
    StThreeVectorD helixCrossPlane( const StHelixD& helix, const double& z );

    void    helixSwimmer( const StPhysicalHelixD& helix, StPhysicalHelixD& swimmerHelix, const double& z, double& pathlength );

    std::vector< int >  helixCrossSector( const StHelixD& helix );

    void    helixCrossCounter( const StPhysicalHelixD& helix, std::vector< int >& idVec, std::vector< StThreeVectorD >& crossVec,
                               std::vector< StThreeVectorD >& localVec, std::vector< double >& thetaVec, std::vector< double >& pathLenVec );

    void    logPoint( const char* text, const StThreeVectorD& point );
    //--------------------------------------------------------------------

    //--------------------------------------------------------------------
    // for the use in running over picoDst input
    TVector3 helixCrossETofPlane( const StPicoHelix& helix );
    TVector3 helixCrossPlane( const StPicoHelix& helix, const double& z );

    void     helixSwimmer( const StPicoPhysicalHelix& helix, StPicoPhysicalHelix& swimmerHelix, const double& z, double& pathlength );

    std::vector< int >  helixCrossSector( const StPicoHelix& helix );

    void    helixCrossCounter( const StPicoPhysicalHelix& helix, std::vector< int >& idVec, std::vector< TVector3 >& crossVec,
                               std::vector< TVector3 >& localVec, std::vector< double >& thetaVec );

    void    logPoint( const char* text, const TVector3& point );
    //--------------------------------------------------------------------


    //--------------------------------------------------------------------
    // magnetic field related functions
    StThreeVectorD getField( const StThreeVectorD& pos );
    TVector3       getField( const TVector3& pos );

    double getFieldZ( const StThreeVectorD& pos );
    double getFieldZ( const TVector3&       pos );
    double getFieldZ( const double& x, const double& y, const double& z );
    //--------------------------------------------------------------------


    StETofGeomModule* module( const unsigned int i );
    unsigned int      nValidModules() const;

    void debugOn();
    void debugOff();
    bool isDebugOn() const;


private:
    StETofGeomModule* mETofModule[ eTofConst::nModules ];

    unsigned int      mNValidModules;  // amount of loaded modules
    bool              mInitFlag;       // flag of initialization, true if done
    bool              mDebug;          // control message printing of this class

    StarMagField*     mStarBField;

    ClassDef( StETofGeometry, 1 )
};


inline bool         StETofGeometry::isInitDone()    const { return mInitFlag;      }
inline unsigned int StETofGeometry::nValidModules() const { return mNValidModules; }

inline bool  StETofGeometry::setInitFlag( const bool initFlag ) { return mInitFlag = initFlag; }


inline void  StETofGeometry::debugOn()  { mDebug = true;  }
inline void  StETofGeometry::debugOff() { mDebug = false; }

inline bool  StETofGeometry::isDebugOn() const { return mDebug; }



#endif /// STETOFGEOMETRY_H