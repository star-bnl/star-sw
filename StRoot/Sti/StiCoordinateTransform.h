/**
 * @file StiCoordinateTransform.h
 * @class StiCoordinateTransform
 * @brief Definition of the StiLocal<->Global coordinate transforms
 *
 * Encapsulates all coordinate transformations to and from the Sti Local
 * system.  The Sti local system is defined as followed:
 * <ul>
 * <li>z points in the direction of global (star magnet iron) z
 * <li>x points radially outward from the center of the detector plane
 * <li>y follows the right hand rule.
 * <li>origin = global origin
 * </ul>
 *
 * Currently, all the transformations are "ideal", in that they assume
 * all detectors are centered at (0, 0, 0) in global coords and have
 * their z axis along global z.  This is done because the Sti track model
 * assumes ideal rotations when going between detectors.
 * 
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_COORDINATE_TRANSFORM_H
#define STI_COORDINATE_TRANSFORM_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StTpcCoordinateTransform;
class StSvtCoordinateTransform;
class StGlobalCoordinate;
class StiLocalCoordinate;
#include "StarClassLibrary/StThreeVector.hh"

class StiCoordinateTransform {
public:

    static StiCoordinateTransform* instance();
    static void kill();

    /// rotates global point in tpc to sti local coords based on the
    /// given tpc sector [1-24]
    void globalTpcToSti(const StGlobalCoordinate& global, 
                        StiLocalCoordinate& local,
                        unsigned int iSector) const;
    /// rotates global point in svt to sti local coords based on the
    /// given svt barrel [1-3] and ladder [1-16]
    void globalSvtToSti(const StGlobalCoordinate& global, 
                        StiLocalCoordinate& local,
                        unsigned int iBarrel, unsigned int iLadder) const;
    /// rotates global point to sti local coords using the given angle
    void globalToSti(const StGlobalCoordinate& global,
                     StiLocalCoordinate& local,
                     double dRefAngle) const ;
    /// rotates global point to sti local coords using given cosine
    /// & sine.
    void globalToSti(const StGlobalCoordinate& global,
                     StiLocalCoordinate& local,
                     double dCosRefAngle, double dSinRefAngle) const;

    /// returns the azimuthal angle [-pi, pi) for tpc sector [1-24]
    double phiForTpcSector(unsigned int iSector) const;
    /// returns the azimuthal angle [-pi, pi) for svt barrel [1-3]
    /// and ladder [1-16]
    double phiForSvtBarrelLadder(unsigned int iBarrel, 
                                 unsigned int iLadder) const;

    // generic transforms
    /// phi in [-pi,pi] for sector in [1, nSectors] (z>0)
    double phiForWestSector(unsigned int iSector, unsigned int nSectors) const;
    /// phi in [-pi,pi] for sector in [nSectors + 1, 2*nSectors] (z<0)
    double phiForEastSector(unsigned int iSector, unsigned int nSectors) const;
    /// phi in [-pi,pi] for sector in [1, 2*nSectors]
    double phiForSector(unsigned int iSector, unsigned int nSectors) const;

    /// radius of tpc padrow (cm) [1-45]
    double positionForTpcPadrow(unsigned int iPadrow) const;
    /// radius of svt barrel (cm) [1-3]
    double positionForSvtBarrel(unsigned int iBarrel) const;
    /// radius of svt layer (cm) [1-6]
    double positionForSvtLayer(unsigned int iLayer) const;

    /// tpc padrow [1-45] for global position
    unsigned int tpcPadrowForGlobal(const StThreeVector<double> &vec) const;
    /// tpc sector [1-45] for global position
    unsigned int tpcSectorForGlobal(const StThreeVector<double> &vec) const;

    /// svt barrel [1-3] for global position
    unsigned int svtBarrelForGlobal(const StThreeVector<double> &vec) const;
    /// svt ladder [1-16] for global position
    unsigned int svtLadderForGlobal(const StThreeVector<double> &vec) const;

protected:
    StiCoordinateTransform();
    virtual ~StiCoordinateTransform();

    /// svt barrel [1-3] for global position (quick & dirty)
    unsigned int _svtBarrelForGlobal(const StThreeVector<double> &vec) const;
    /// svt ladder [1-16] for global position (quick & dirty)
    unsigned int _svtLadderForGlobal(const StThreeVector<double> &vec) const;


    // cosine & sine of detector reference angles
    vector<double>           m_vdCosForTpcSector;
    vector<double>           m_vdSinForTpcSector;
    vector< vector<double> > m_vvdCosForSvtBarrelLadder;
    vector< vector<double> > m_vvdSinForSvtBarrelLadder;

    // radial position of detector reference angles
    vector<double> m_vdPositionForTpcPadrow;
    vector<double> m_vdPositionForSvtBarrel;
    vector<double> m_vdPositionForSvtLayer;

    // detector counts
    unsigned int         m_nTpcSectors;
    unsigned int         m_nTpcPadrows;
    unsigned int         m_nSvtBarrels;
    vector<unsigned int> m_vnSvtLadders;

    // transformation objects
    StTpcCoordinateTransform *m_pTpcCoordinateTransform;
    StSvtCoordinateTransform *m_pSvtCoordinateTransform;

    static StiCoordinateTransform *s_pInstance;
    
};

inline double StiCoordinateTransform::phiForTpcSector(
    unsigned int iSector) const{
  if(iSector<1 || iSector>m_nTpcSectors){
    cout << "phiForTpcSector(" << iSector << "): invalid sector" << endl;
  }
  return phiForSector(iSector, m_nTpcSectors/2);
} // phiForTpcSector

inline double StiCoordinateTransform::phiForSvtBarrelLadder(
    unsigned int iBarrel, unsigned int iLadder) const{
  if(iBarrel<1 || iBarrel>m_nSvtBarrels){
    cout << "phiForSvtBarrelLadder(" << iBarrel << ", " << iLadder
         << "): invalid barrel" << endl;
  }
  if(iLadder<1 || iLadder>m_vnSvtLadders[iBarrel]){
    cout << "phiForSvtBarrelLadder(" << iBarrel << ", " << iLadder
         << "): invalid ladder" << endl;
  }
  return phiForSector(iLadder, m_vnSvtLadders[iBarrel]);
} // phiForSvtBarrelLadder

inline void StiCoordinateTransform::globalTpcToSti(
    const StGlobalCoordinate& global, StiLocalCoordinate& local,
    unsigned int iSector) const{
  globalToSti(global, local, m_vdCosForTpcSector[iSector],
              m_vdSinForTpcSector[iSector]);
} // globalTpcToSti

inline void StiCoordinateTransform::globalSvtToSti(
    const StGlobalCoordinate& global, StiLocalCoordinate& local,
    unsigned int iBarrel, unsigned int iLadder) const{
  globalToSti(global, local,
              m_vvdCosForSvtBarrelLadder[iBarrel][iLadder],
              m_vvdSinForSvtBarrelLadder[iBarrel][iLadder]);
} // globalSvtToSti

inline void StiCoordinateTransform::globalToSti(
    const StGlobalCoordinate& global, StiLocalCoordinate& local,
    double dRefAngle) const{
  globalToSti(global, local, cos(dRefAngle), sin(dRefAngle));
} // globalToSti

inline double StiCoordinateTransform::positionForTpcPadrow(
    unsigned int iPadrow) const{
  return m_vdPositionForTpcPadrow[iPadrow];
} // positionForTpcPadrow
inline double StiCoordinateTransform::positionForSvtBarrel(
    unsigned int iBarrel) const{
  return m_vdPositionForSvtBarrel[iBarrel];
} // positionForSvtBarrel
inline double StiCoordinateTransform::positionForSvtLayer(
    unsigned int iLayer) const{
  return m_vdPositionForSvtLayer[iLayer];
} // positionForSvtLayer

#endif
