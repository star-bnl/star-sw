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


class svg_geom_st;
class StiSsdDetectorBuilder : public StiDetectorBuilder
{

 public:
        StiSsdDetectorBuilder(bool active,const string & inputFile);
	virtual ~StiSsdDetectorBuilder(); 
	virtual void buildDetectors(StMaker& source);
	virtual void setDefaults();
  
 protected:
	//! test 1
        double phiForSsdLadder(unsigned int iLadder) const;
        //double radiusForSsdLadder(unsigned int iLadder) const;
	StiMaterial *_gasMat;        //!< 
	StiMaterial *_siMat;
	StiPlanarShape * _waferShape[1];
	StSsdConfig   * _config;
	StSsdGeometry * _geometry;
	StiDefaultHitErrorCalculator _hitCalculator;
};
#endif 
