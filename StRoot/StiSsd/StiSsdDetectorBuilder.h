// $Id: StiSsdDetectorBuilder.h,v 1.14 2014/08/22 17:51:47 perev Exp $
// 
// $Log: StiSsdDetectorBuilder.h,v $
// Revision 1.14  2014/08/22 17:51:47  perev
// Remove never used input file
//
// Revision 1.13  2009/03/16 13:50:09  fisyak
// Move out all Sti Chairs into StDetectorDb
//
// Revision 1.12  2008/04/03 20:04:22  fisyak
// Straighten out DB access via chairs
//
// Revision 1.11  2006/10/09 15:47:59  fisyak
// use Normal represantation, remove StiDedxCalculator
//
// Revision 1.10  2006/06/28 18:51:46  fisyak
// Add loading of tracking and hit error parameters from DB
//
// Revision 1.9  2005/06/21 16:35:01  lmartin
// DetectorBuilder updated with the correct methods from StSsdUtil
//
// Revision 1.8  2005/06/21 15:31:47  lmartin
// CVS tags added
//
/*!
 * \class StiSsdDetectorBuilder
 * \author Christelle Roy
 * \date 02/27/04

This class is the description of the StiSsdDetectorBuilder
*/
#ifndef StiSsdDetectorBuilder_H
#define StiSsdDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StThreeVector.hh"
class ssdWafersPosition_st;
class St_ssdWafersPosition;
class StiSsdDetectorBuilder : public StiDetectorBuilder
{

 public:
        StiSsdDetectorBuilder(bool active);
	virtual ~StiSsdDetectorBuilder(); 
	virtual void buildDetectors(StMaker& source);
	virtual void useVMCGeometry();		
	void         setSiMat(StiMaterial     *m) {_siMat = m;}
	void         setHybridMat(StiMaterial *m) {_hybridMat = m;}
	StiMaterial *getSiMat()    {return _siMat;}
	StiMaterial *getHybridMat(){return _hybridMat;}
 
 protected:
	StiMaterial *_siMat;
	StiMaterial *_hybridMat;
	ssdWafersPosition_st *ssdWafersPosition(Int_t Id, St_ssdWafersPosition *wafers);
};
#endif 
