// $Id: vmcAppLinkDef.h,v 1.3 2004/07/16 22:52:53 potekhin Exp $
// $Log: vmcAppLinkDef.h,v $
// Revision 1.3  2004/07/16 22:52:53  potekhin
// Incremental changes
//
// Revision 1.2  2004/07/13 19:13:37  potekhin
// removed tpc classes, added log tag
//

#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
 
#pragma link C++ class StarGenerator+;
#pragma link C++ class StarParticle+;
#pragma link C++ class StarKineGenerator+;
#pragma link C++ class StarMCApplication+;
#pragma link C++ class StarRootManager+;
#pragma link C++ class StarStack+;
#pragma link C++ class StarDetectorConstruction+;
#pragma link C++ class StarChamberParametrization+;
#pragma link C++ class StarHit+;
#pragma link C++ class StarRndm+;
#pragma link C++ class StarConfiguration+;
#pragma link C++ class StarModule+;
#pragma link C++ class StarDetector+;
#pragma link C++ class StarGeometry+;
#pragma link C++ class StarMaterial+;
#pragma link C++ class StarMedium+;
#pragma link C++ class StarRotation+;
#pragma link C++ class StarVolume+;
#pragma link C++ class StarCaveGeometry+;
#pragma link C++ class StarTpcGeometry+;
#pragma link C++ class StarMCDisplay+;
#pragma link C++ class StarMCEvent+;

#pragma link C++ enum   FileMode;

#endif





