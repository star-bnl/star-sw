#ifndef STI_DETECTOR_BUILDER_H
#define STI_DETECTOR_BUILDER_H

#include <map>
#include <vector>
#include <string>
#include <stdexcept>
#include <math.h>
#include "Sti/StiMapUtilities.h"
#include "Sti/Base/Named.h"
#include "StThreeVector.hh"
class StiDetector;
class StiMaterial;
class StiShape;
class Messenger;
template<class Factorized>class Factory;

// Set up stl maps for by-name lookup of shapes and materials.
// Not used for placements because they are unique to each detector.
typedef map<NameMapKey, StiMaterial*> materialMap;
typedef materialMap::value_type materialMapValType;
typedef map<NameMapKey, StiShape*> shapeMap;
typedef shapeMap::value_type shapeMapValType;
typedef map<NameMapKey, StiDetector*> detectorMap;
typedef detectorMap::const_iterator detectorIterator;
typedef detectorMap::value_type detectorMapValType;
/*!
  Class defines the notion of a detector builder. It creates the various components of
  a detector and set their shape, placement, and material properties.
  <p>
  \author Ben Norman (Kent State University) Aug 1, 2001
  \author Claude Pruneau (Wayne State University) Oct 16, 2002
*/
class StiDetectorBuilder : public Named
{

public:


  StiDetectorBuilder(const string & name);
  virtual ~StiDetectorBuilder(); 
  
  detectorMap getDetectors(){ return mDetectorMap; }
  
  virtual StiMaterial * add(StiMaterial *material);
  virtual StiShape    * add(StiShape *shape);
  virtual StiDetector * add(StiDetector *detector);
  virtual StiDetector * add(unsigned int row, unsigned int sector, StiDetector *detector);
  virtual StiMaterial * findMaterial(const string& szName) const;
  virtual StiShape    * findShape(const string& szName) const;
  virtual StiDetector * findDetector(const string& szName) const;
  virtual StiDetector * getDetector(unsigned int layer, unsigned int sector) const;
  virtual void setDetector(unsigned int layer, unsigned int sector, StiDetector * detector);
  virtual unsigned int  getNRows() const;
  virtual unsigned int  getNSectors(unsigned int row=0) const;
  virtual void setNRows(unsigned int nRows);
  virtual void setNSectors(unsigned int row, unsigned int nSectors);

  virtual bool hasMore() const;
  virtual StiDetector* next();
  virtual void build();
  virtual void buildMaterials();
  virtual void buildShapes();
  virtual void buildDetectors();
  virtual void loadDb();
  double nice(double angle) const;
  friend class StiHit;
  
  double phiForSector(unsigned int iSector, 
		      unsigned int nSectors) const;
  double phiForWestSector(unsigned int iSector, 
			  unsigned int nSectors) const;
  double phiForEastSector(unsigned int iSector, 
			  unsigned int nSectors) const;
  void setGroupId(int id);
  int  getGroupId() const;
  
 protected:
  
  materialMap         mMaterialMap;
  shapeMap            mShapeMap;
  detectorMap         mDetectorMap;
  detectorIterator    mDetectorIterator; 
  unsigned int        _nRows;
  vector< unsigned int> _nSectors;
  vector< vector<StiDetector*> > _detectors;
  Factory<StiDetector>*_detectorFactory;
  Messenger&           _messenger;
  int                  _groupId;

};

///Returns the number of active rows in the detector
///Rows can be counted radially or longitudinally
inline unsigned int  StiDetectorBuilder::getNRows() const
{
  return _nRows;
}

///Returns the number of sectors (or segments) in a the
///given row. Sector are expected to be azimuthally
///distributed.
inline unsigned int  StiDetectorBuilder::getNSectors(unsigned int row=0) const
{
  if (row>_nRows)
    {
      string message = "StiDetectorBuilder::getNSectors() - ERROR - argument row out of bound:";
      message+=row;
      message+=">";
      message+=_nRows;
      throw runtime_error(message.c_str());
    }
  return _nSectors[row];
}

///Sets the number of the number of rows of active
///detectors.
inline void StiDetectorBuilder::setNRows(unsigned int nRows)
{
  if (nRows==0 || nRows>100)
    { 
      string message = "StiDetectorBuilder::setNSectors() - ERROR - argument nRow==0 ||nRow>100; nRow:";
      message+=nRows;
      throw runtime_error(message.c_str());
    }
  _nRows = nRows;
  _nSectors = vector< unsigned int >(_nRows);
  _detectors = vector< vector<StiDetector*>  >(_nRows);
} 
 
inline void StiDetectorBuilder::setNSectors(unsigned int row, unsigned int nSectors)
{
  if (row>_nRows)
    { 
      string message = "StiDetectorBuilder::setNSectors() - ERROR - row==";
      message+=row;
      message+=">_nRows==";
      message+=_nRows;
      throw runtime_error(message.c_str());
    }
  if (nSectors==0 || nSectors>100)
    { 
      string message = "StiDetectorBuilder::setNSectors() - ERROR - nSectors==";
      message+=nSectors;
      message+="seems inappropriate...";
      throw runtime_error(message.c_str());
    }
  _nSectors[row] = nSectors;
  _detectors[row] = vector<StiDetector*>(nSectors);
}

inline double StiDetectorBuilder::nice(double angle) const 
{
  while(angle >=  M_PI){ angle -= 2.*M_PI; }
  while(angle <  -M_PI){ angle += 2.*M_PI; }
  return angle;
}

inline  StiDetector * StiDetectorBuilder::getDetector(unsigned int row, unsigned int sector) const
{
  if (row>_nRows)
    {
      string message = "StiDetectorBuilder::getDetector() - ERROR - argument row out of bound:";
      throw runtime_error(message.c_str());
    }
  if (sector>_nSectors[row])
    {
      string message = "StiDetectorBuilder::getDetector() - ERROR - argument sector out of bound";
      throw runtime_error(message.c_str());
    }
  return _detectors[row][sector];
}


inline  void StiDetectorBuilder::setDetector(unsigned int row, unsigned int sector, StiDetector *detector)
{
  if (row>_nRows)
    {
      string message = "StiDetectorBuilder::setDetector() - ERROR - argument row out of bound:";
      throw runtime_error(message.c_str());
    }
  if (sector>_nSectors[row])
    {
      string message = "StiDetectorBuilder::setDetector() - ERROR - argument sector out of bound";
      throw runtime_error(message.c_str());
    }
  _detectors[row][sector] = detector;
}



/// nSectors is the number of sectors in 360 degrees (one half of the
/// TPC or all of the SVT, for example)
inline double StiDetectorBuilder::phiForSector(unsigned int iSector, 
					unsigned int nSectors) const
{
  if(iSector<0 || iSector>=2*nSectors)
    {
      cerr << "StiDetectorBuilder::phiForSector(" << iSector << ", "
	   << nSectors << "):  Error, invalid sector" << endl;
    }
  return (iSector < nSectors) ? 
    phiForWestSector(iSector, nSectors) :
    phiForEastSector(iSector, nSectors);
}

/// returns the reference angle for the given sector number (out of the 
/// given total).  This assumes the star convention where the highest
/// numbered sector is at "12 o'clock", or pi/2, and the sector numbering
/// _decreases_ with increasing phi.  [I guess this must have seemed like
/// a good idea at the time....]
///
/// returns in [-pi, pi)
///
/// nSectors is the number of sectors in the west half of the detector,
/// not both halves.
inline double StiDetectorBuilder::phiForWestSector(unsigned int iSector, 
					    unsigned int nSectors) const
{
  int offset = nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;
  
  // make phi ~ sector (not -sector) and correct offset
  double dPhi = (offset - static_cast<int>(iSector+1))*deltaPhi;
  return nice(dPhi);  
} // phiForWestSector

/// as above, but numbering _increases_ with increasing phi.
inline double StiDetectorBuilder::phiForEastSector(unsigned int iSector, 
					    unsigned int nSectors) const
{
  int offset = 3*nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;
  double dPhi = (static_cast<int>(iSector+1) - offset)*deltaPhi;
  return nice(dPhi);  
} // phiForEastSector


inline void StiDetectorBuilder::setGroupId(int id)
{
  _groupId = id;
}

inline int  StiDetectorBuilder::getGroupId() const
{
  return _groupId;
}


#endif // ifndef STI_DETECTOR_BUILDER_H
