/*!
 * \class StiSsdDetectorBuilder
 * \author Christelle Roy
 * \date 02/27/04

This class is the description of the StiSsdDetectorBuilder
*/
#ifndef StiSsdDetectorBuilder_H
#define StiSsdDetectorBuilder_H
#include "StSsdUtil/StSsdGeometry.hh"
#include "StSsdUtil/StSsdConfig.hh"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiHitErrorCalculator.h"
#include "StThreeVector.hh"

class StiSsdDetectorBuilder : public StiDetectorBuilder
{

 private :
  StSsdConfig*   mSsdConfig;    //!
  StSsdGeometry* mSsdGeom;      //!
 public:
        StiSsdDetectorBuilder(bool active,const string & inputFile);
	virtual ~StiSsdDetectorBuilder(); 
	virtual void buildDetectors(StMaker& source);
	virtual void setDefaults();
  
 protected:
        float phiForSsdLadder(unsigned int iLadder) const;
        float radiusForSsdLadder(unsigned int iLadder) const;
	StiMaterial *_gasMat;        
	StiMaterial *_siMat;
	StiMaterial *_hybridMat;
	StiPlanarShape * _waferShape[1];
	StiPlanarShape * _hybridShape[1];
	StSsdConfig   * _config;
	StSsdGeometry * _geometry;
	StSsdGeometry * _dimensions;
	StiDefaultHitErrorCalculator _hitCalculator;
};
#endif 
