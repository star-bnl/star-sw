#ifndef StiSvtDetectorBuilder_H
#define StiSvtDetectorBuilder_H
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StThreeVector.hh"
#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"

class StiSvtDetectorBuilder : public StiDetectorBuilder
{
 public:
	StiSvtDetectorBuilder(bool active);
	virtual ~StiSvtDetectorBuilder(); 

	virtual void buildDetectors(StMaker& source);	
	virtual void useVMCGeometry();		
	void    setSiMat(StiMaterial     *m) {_siMat = m;}
	void    setHybridMat(StiMaterial *m) {_hybridMat = m;}
	StiMaterial *getSiMat()    {return _siMat;}
	StiMaterial *getHybridMat(){return _hybridMat;}
 protected:
	StiMaterial    * _siMat;
	StiMaterial    * _hybridMat;
	StiPlanarShape * _waferShape[6];
	StiPlanarShape * _hybridShape[6];
	StSvtConfig    * _config;
	StSvtGeometry  * _geometry;
};
#endif 
