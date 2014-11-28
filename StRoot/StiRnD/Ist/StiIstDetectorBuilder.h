// $Id: StiIstDetectorBuilder.h,v 1.9 2014/08/22 17:49:39 perev Exp $
// 
// $Log: StiIstDetectorBuilder.h,v $
// Revision 1.9  2014/08/22 17:49:39  perev
// Remove never used input file
//
// Revision 1.8  2009/03/16 13:51:00  fisyak
// Move out all Sti Chairs into StDetectorDb
//
// Revision 1.7  2009/02/06 21:26:49  wleight
// UPGR15 Update
//
// Revision 1.6  2008/04/03 20:04:21  fisyak
// Straighten out DB access via chairs
//
// Revision 1.5  2007/04/23 00:44:58  wleight
// Made all layers, not just inner ones, active
//
// Revision 1.4  2006/12/14 22:01:48  wleight
// Changed hit errors so that they are obtained from the database and are different for each layer
//
// Revision 1.3  2006/10/20 18:43:12  wleight
// Changes to make perfect hits in the IST work with UPGR05
//
// Revision 1.10  2006/06/28 18:51:46  fisyak
// Add loading of tracking and hit error parameters from DB
//
// Revision 1.9  2005/06/21 16:35:01  lmartin
// DetectorBuilder updated with the correct methods from StIstUtil
//
// Revision 1.8  2005/06/21 15:31:47  lmartin
// CVS tags added
//
/*!
 * \class StiIstDetectorBuilder
 * \author Christelle Roy
 * \date 02/27/04

This class is the description of the StiIstDetectorBuilder
*/
#ifndef StiIstDetectorBuilder_H
#define StiIstDetectorBuilder_H
//#include "StSsdUtil/StSsdGeometry.hh"
//#include "StSsdUtil/StSsdConfig.hh"
#include "Sti/StiDetectorBuilder.h"
//#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StThreeVector.hh"

class StiIstDetectorBuilder : public StiDetectorBuilder
{

 private :
   //StSsdConfig*   mSsdConfig;    //!
   //StSsdGeometry* mSsdGeom;      //!
 public:
        StiIstDetectorBuilder(bool active);
	virtual ~StiIstDetectorBuilder(); 
	virtual void buildDetectors(StMaker& source);
	virtual void AverageVolume(TGeoPhysicalNode *nodeP);
	virtual void loadDS(TDataSet&);
	//virtual void setDefaults();
	virtual void useVMCGeometry();		
	void    setSiMat(StiMaterial     *m) {_siMat = m;}
	void    setHybridMat(StiMaterial *m) {_hybridMat = m;}
	StiMaterial *getSiMat()    {return _siMat;}
	StiMaterial *getHybridMat(){return _hybridMat;}
 
 protected:
        //float phiForSsdLadder(unsigned int iLadder) const;
        //float radiusForSsdLadder(unsigned int iLadder) const;
	StiMaterial *_siMat;
	StiMaterial *_hybridMat;
	StiPlanarShape * _waferShape[1];
	StiPlanarShape * _hybridShape[1];
	//StSsdConfig   * _config;
	//StSsdGeometry * _geometry;
	//StSsdGeometry * _dimensions;
	//StiDefaultHitErrorCalculator _hitCalculator1;
	//StiDefaultHitErrorCalculator _hitCalculator2;
	//StiDefaultHitErrorCalculator _hitCalculator3;
};
#endif 
