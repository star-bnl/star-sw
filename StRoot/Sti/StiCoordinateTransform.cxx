#include "StiCoordinateTransform.h"
#include "StiLocalCoordinate.h"

#include "StTpcDb/StTpcDb.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StSvtDbMaker/St_SvtDb_Reader.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StarClassLibrary/StThreeVector.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StTpcLocalCoordinate.hh"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"

#include <math.h>

StiCoordinateTransform* StiCoordinateTransform::s_pInstance = 0;

StiCoordinateTransform* StiCoordinateTransform::instance(){
  return (s_pInstance==0) ? new StiCoordinateTransform : s_pInstance;
} // instance

void StiCoordinateTransform::kill(){
    if (s_pInstance) {
	delete s_pInstance;
	s_pInstance = 0;
    }
} // kill

StiCoordinateTransform::StiCoordinateTransform(){
  
  // get transformation objects for the different detectors
  m_pSvtCoordinateTransform = new StSvtCoordinateTransform;
  St_SvtDb_Reader *pSvtDb_Reader = gStSvtDbMaker->get_SvtDb_Reader();
  StSvtConfig *pSvtConfig = pSvtDb_Reader->getConfiguration();
  StSvtGeometry *pSvtGeometry = pSvtDb_Reader->getGeometry();
  StSvtHybridCollection *pDriftVelocity = pSvtDb_Reader->getDriftVelocity();
  m_pSvtCoordinateTransform->setParamPointers(pSvtGeometry, pSvtConfig,
                                              pDriftVelocity);

  m_pTpcCoordinateTransform = new StTpcCoordinateTransform(gStTpcDb);

  //---------------------------
  // cache detector rotations

  // tpc
  m_nTpcSectors = gStTpcDb->Dimensions()->numberOfSectors();
  m_vdCosForTpcSector = vector<double>(m_nTpcSectors + 1);
  m_vdSinForTpcSector = vector<double>(m_nTpcSectors + 1);

  for (unsigned int iSector = 1; iSector<=m_nTpcSectors; iSector++){
    double dPhi = phiForTpcSector(iSector);
    m_vdCosForTpcSector[iSector] = cos(dPhi);
    m_vdSinForTpcSector[iSector] = sin(dPhi);
  } // for iSector

  // svt
  m_nSvtBarrels = pSvtConfig->getNumberOfBarrels();
  m_vnSvtLadders = vector<unsigned int>(m_nSvtBarrels + 1);
  m_vvdCosForSvtBarrelLadder = vector< vector<double> >(m_nSvtBarrels + 1);
  m_vvdSinForSvtBarrelLadder = vector< vector<double> >(m_nSvtBarrels + 1);

  for (unsigned int iBarrel = 1; iBarrel<=m_nSvtBarrels; iBarrel++){

    m_vnSvtLadders[iBarrel] = pSvtConfig->getNumberOfLadders(iBarrel);

    // initialize vectors for each barrel
    m_vvdCosForSvtBarrelLadder[iBarrel] = 
        vector<double>(m_vnSvtLadders[iBarrel] + 1);
    m_vvdSinForSvtBarrelLadder[iBarrel] = 
        vector<double>(m_vnSvtLadders[iBarrel] + 1);

    for(unsigned int iLadder = 1; iLadder<=m_vnSvtLadders[iBarrel]; iLadder++){
      double dPhi = phiForSvtBarrelLadder(iBarrel, iLadder);
      m_vvdCosForSvtBarrelLadder[iBarrel][iLadder] = cos(dPhi);
      m_vvdSinForSvtBarrelLadder[iBarrel][iLadder] = sin(dPhi);
    } // for iLadder
  } // for iBarrel

  //-----------------------------------
  // cache detector positions (radii)

  // tpc
  m_nTpcPadrows = gStTpcDb->PadPlaneGeometry()->numberOfRows();
  m_vdPositionForTpcPadrow = vector<double>(m_nTpcPadrows + 1);

  for(unsigned int iPadrow = 1; iPadrow<m_nTpcPadrows; iPadrow++){
    m_vdPositionForTpcPadrow[iPadrow] = 
        gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(iPadrow);
  } // for iPadrow

  // svt
  m_vdPositionForSvtLayer = vector<double>(2*m_nSvtBarrels + 1);
  m_vdPositionForSvtBarrel = vector<double>(m_nSvtBarrels + 1);

  for (unsigned int iBarrel = 1; iBarrel<=m_nSvtBarrels; iBarrel++){
    m_vdPositionForSvtLayer[2*iBarrel - 1] = 
        pSvtGeometry->getBarrelRadius(2*iBarrel - 1);  
    m_vdPositionForSvtLayer[2*iBarrel] = 
        pSvtGeometry->getBarrelRadius(2*iBarrel);
    // barrel radius is defined as average of component layer radii
    m_vdPositionForSvtBarrel[iBarrel] = 
        (m_vdPositionForSvtLayer[2*iBarrel - 1] + 
         m_vdPositionForSvtLayer[2*iBarrel])/2.;
  } // for iBarrel
  
} // StiCoordinateTransform
   
StiCoordinateTransform::~StiCoordinateTransform(){
  delete m_pSvtCoordinateTransform;
  delete m_pTpcCoordinateTransform;
} // ~StiCoordinateTransform

unsigned int StiCoordinateTransform::svtBarrelForGlobal(
    const StThreeVector<double> &vec) const{
  // convert to svt local
  StSvtLocalCoordinate local;
  StGlobalCoordinate global(vec);
  m_pSvtCoordinateTransform->operator()(global, local);

  return (local.layer() + 1)/2;
} // svtBarrelForGlobal

unsigned int StiCoordinateTransform::svtLadderForGlobal(
    const StThreeVector<double> &vec) const{
  // convert to svt local
  StSvtLocalCoordinate local;
  StGlobalCoordinate global(vec);
  m_pSvtCoordinateTransform->operator()(global, local);

  return local.ladder();
} // svtLadderForGlobal

unsigned int StiCoordinateTransform::tpcPadrowForGlobal(
    const StThreeVector<double> &vec) const{

  // convert to tpc local
  StTpcLocalCoordinate local;
  StGlobalCoordinate global(vec);
  m_pTpcCoordinateTransform->operator()(global, local);

  // get sector & padrow
  unsigned int iSector = m_pTpcCoordinateTransform->sectorFromCoordinate(
      local.position());  
  return m_pTpcCoordinateTransform->rowFromLocal(
      m_pTpcCoordinateTransform->rotateToLocal(local.position(), iSector));
} // tpcPadrowForGlobal

unsigned int StiCoordinateTransform::tpcSectorForGlobal(
    const StThreeVector<double> &vec) const{
  // convert to tpc local
  StTpcLocalCoordinate local;
  StGlobalCoordinate global(vec);
  m_pTpcCoordinateTransform->operator()(global, local);
  // get sector from tpc local
  return m_pTpcCoordinateTransform->sectorFromCoordinate(local.position());
} // tpcPadrowForGlobal

void StiCoordinateTransform::globalToSti(
    const StGlobalCoordinate& global, StiLocalCoordinate& local,
    double dCosRefAngle, double dSinRefAngle) const{

  const StThreeVector<double>& globalPosition = global.position();
  StThreeVector<double>& localPosition = local.position();

  localPosition.setX(dCosRefAngle*globalPosition.x() + 
                     dSinRefAngle*globalPosition.y());
  localPosition.setY(-dSinRefAngle*globalPosition.x() + 
                     dCosRefAngle*globalPosition.y());
  localPosition.setZ(globalPosition.z());
} // globalToSti

// returns the reference angle for the given sector number (out of the 
// given total).  This assumes the star convention where the highest
// numbered sector is at "12 o'clock", or pi/2, and the sector numbering
// _decreases_ with increasing phi.  [I guess this must have seemed like
// a good idea at the time....]
//
// returns in [-pi, pi)
//
// nSectors is the number of sectors in the west half of the detector,
// not both halves.
double StiCoordinateTransform::phiForWestSector(
    unsigned int iSector, unsigned int nSectors) const{

  int offset = nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;

  // make phi ~ sector (not -sector) and correct offset
  double dPhi = (offset - static_cast<int>(iSector))*deltaPhi;
  while(dPhi >=  M_PI){ dPhi -= 2.*M_PI; }
  while(dPhi <  -M_PI){ dPhi += 2.*M_PI; }
  return dPhi;
    
} // phiForWestSector

// as above, but numbering _increases_ with increasing phi.
double StiCoordinateTransform::phiForEastSector(
    unsigned int iSector, unsigned int nSectors) const{

    int offset = 3*nSectors/4;
    double deltaPhi = 2.*M_PI/nSectors;

    // correct offset
    double dPhi = (static_cast<int>(iSector) - offset)*deltaPhi;
    while(dPhi >=  M_PI){ dPhi -= 2.*M_PI; }
    while(dPhi <  -M_PI){ dPhi += 2.*M_PI; }

    return dPhi;

} // phiForEastSector

/// nSectors is the number of sectors in 360 degrees (one half of the
/// TPC or all of the SVT, for example)
double StiCoordinateTransform::phiForSector(
    unsigned int iSector, unsigned int nSectors) const{
  
  if(iSector<0 || iSector>2*nSectors){
    cerr << "StiCoordinateTransform::phiForSector(" << iSector << ", "
         << nSectors << "):  Error, invalid sector" << endl;
  }
  return (iSector <= nSectors) ? 
      phiForWestSector(iSector, nSectors) :
      phiForEastSector(iSector, nSectors);

} // phiForSector
