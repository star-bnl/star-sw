#ifndef STI_DETECTOR_BUILDER_H
#define STI_DETECTOR_BUILDER_H

#include <map>
#include <vector>
#include <string>
#include <stdexcept>
#include <math.h>
#include "StDetectorDbMaker/StiTrackingParameters.h"
#include "Sti/StiMapUtilities.h"
#include "Sti/Base/Named.h"
#include "StThreeVector.hh"
#include "StiVMCToolKit.h"
#include "TMath.h"
using namespace std;
class StiDetector;
class StiMaterial;
class StiShape;
class StMaker; 
class TDataSet;
class StiPlanarShape;
class StiCylindricalShape;
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
typedef std::pair<NameMapKey, StiDetector*> DetectorMapPair;
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

  StiDetectorBuilder(const string & name,bool active);
  virtual ~StiDetectorBuilder(); 
  detectorMap getDetectors(){ return mDetectorMap; }
  virtual StiMaterial * add(StiMaterial *material);
  virtual StiShape    * add(StiShape *shape);
  virtual StiDetector * add(StiDetector *detector);
  virtual StiDetector * add(UInt_t row, UInt_t sector, StiDetector *detector);
  virtual        void   del(UInt_t row, UInt_t sector);
  virtual StiMaterial * findMaterial(const string& szName) const;
  virtual StiShape    * findShape(const string& szName) const;
  virtual StiDetector * findDetector(const string& szName) const;
  virtual StiDetector * getDetector(UInt_t layer, UInt_t sector) const;
  virtual void setDetector(UInt_t layer, UInt_t sector, StiDetector * detector);
///Returns the number of active rows in the detector
///Rows can be counted radially or longitudinally
  virtual void setNRows(UInt_t nRows) {if (_detectors.size() < nRows) _detectors.resize(nRows);}
  virtual UInt_t  getNRows() const {return _detectors.size();}
  virtual UInt_t  getNSectors(UInt_t row=0) const;
  virtual void setNSectors(UInt_t row, UInt_t nSectors) {
    setNRows(row+1);if (_detectors[row].size() < nSectors) _detectors[row].resize(nSectors);
  }
  virtual bool hasMore() const;
  virtual StiDetector* next();
  virtual void build(StMaker&source);
  virtual void buildDetectors(StMaker&source);

  double nice(double angle) const;
  void setGroupId(int id) 				{ _groupId = id;}
  int  getGroupId() const 				{return _groupId;}
  StiTrackingParameters *getTrackingParameters() 	{ return  _trackingParameters;}
  Factory<StiDetector>* getDetectorFactory() 		{return _detectorFactory;}
  void SetCurrentDetectorBuilder(StiDetectorBuilder *m) {fCurrentDetectorBuilder = m;}
  virtual void AverageVolume(TGeoPhysicalNode *nodeP);
  virtual void useVMCGeometry() {}
  void    setGasMat(StiMaterial *m) 			{_gasMat = m;}
  StiMaterial *getGasMat()   				{return _gasMat;}
  void setSplit(double relThick=0.5, int maxSplit=20)	{mThkSplit=relThick;mMaxSplit=maxSplit;}
//		Static methodes
  static void setDebug(int m = 0) 			{_debug = m;}
  static int  debug() {return _debug;}
  static StiDetectorBuilder *GetCurrentDetectorBuilder(){return fCurrentDetectorBuilder;}
  static  void MakeAverageVolume(TGeoPhysicalNode *nodeP) 
              {if (fCurrentDetectorBuilder) fCurrentDetectorBuilder->AverageVolume(nodeP);}
  void Print() const;

  friend ostream& operator<<(ostream &os, const DetectorMapPair &detMapEntry);

 protected:
  float               mThkSplit;	//wide/thickness/mThkSplit = nSplits
  int                 mMaxSplit;	//max number of splittings allowed 
  int                 _groupId;
  bool                _active;
  materialMap         mMaterialMap;
  shapeMap            mShapeMap;
  detectorMap         mDetectorMap;
  detectorIterator    mDetectorIterator; 
  vector< vector<StiDetector*> > _detectors;
  Factory<StiDetector>*_detectorFactory;
  StiTrackingParameters *_trackingParameters;
  static StiDetectorBuilder* fCurrentDetectorBuilder;
  StiMaterial    * _gasMat; // Mother Volume material
  static   int     _debug;
};

inline double StiDetectorBuilder::nice(double angle) const 
{
  while(angle >=  M_PI){ angle -= 2.*M_PI; }
  while(angle <  -M_PI){ angle += 2.*M_PI; }
  return angle;
}


#endif // ifndef STI_DETECTOR_BUILDER_H
