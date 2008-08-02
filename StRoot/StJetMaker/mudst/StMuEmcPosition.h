//******************************************************************************
  //                                                                            
  // StMuEmcPosition.h
  //
  // Authors: Marcia Maria de Moura
  //
  // Initial version: 2001/12/21
  //
  //******************************************************************************

    /*! \class StMuEmcPosition
      \author Marcia M. de Moura (tweaked for MuDst by M.L. Miller)

      This class has a set of track projection utilities for EMC and a few tools to get neighbor
      towers and real eta/phi positions considering the collision vertex.

    */

#ifndef STMUEMCPOSITION_H
#define STMUEMCPOSITION_H

#include "TObject.h"

#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

class StMuTrack;
class StMcTrack;
class StEmcGeom;
class StVertex;
class StMcVertex;

class StMuEmcPosition : public TObject
{
public:            

    StMuEmcPosition();
    virtual  ~StMuEmcPosition();

    bool projTrack(StThreeVectorD*,StThreeVectorD*,const StMuTrack*,double,double=225.405,int=1); ///< StjTrack projection utility
    bool projTrack(StThreeVectorD*,StThreeVectorD*,StMcTrack*,double,double=225.405,int=1); ///< StjTrack projection utility
    
    bool trackOnEmc(StThreeVectorD*,StThreeVectorD*,const StMuTrack*,double,double=225.405); ///< StjTrack projection utility
    bool trackOnEmc(StThreeVectorD*,StThreeVectorD*,StMcTrack*,double,double=225.405); ///< StjTrack projection utility

    bool trackOnBEmc(StThreeVectorD*,StThreeVectorD*,const StMuTrack*,double,double=225.405); ///< StjTrack projection utility
    bool trackOnBEmc(StThreeVectorD*,StThreeVectorD*,StMcTrack*,double,double=225.405); ///< StjTrack projection utility
    bool trackOnEEmc(StThreeVectorD* position, StThreeVectorD* momentum, const StMuTrack* track, double magField = 0.5, double z = kEEmcZSMD) const; ///< Project track into EEMC at SMD depth (magnetic field must be in Tesla)
    
    int getTowerEtaPhi(double, double, float*, float*); ///< Return tower eta/phi

    int getNextTowerId(float, float, int, int); ///< Return neighbor tower id's
    int getNextTowerId(int,int,int); ///< Return neighbor tower id's
    int getNextTowerId(int,int,int,int,int); ///< Return neighbor tower id's
    int getNextId(int,int,int,int,int,int); ///< Return neighbor id (works for all detectors 1=bemc, 2=bprs, 3=bsmde, 4=bsmdp)

    float getDistTowerToTrack(double, double, int, int); ///< Return distance from track to center of one tower

    StThreeVectorF getPosFromVertex(const StThreeVectorF&,int);        ///< Return Position from collision vertex
    StThreeVectorF getPosFromVertex(StMcVertex*,int);      ///< Return position from collision vertex
    float getThetaFromVertex(const StThreeVectorF&,int);      ///< Return theta of the tower considering the collision vertex
    float getThetaFromVertex(StMcVertex*,int);    ///< Return theta of the tower considering the collision vertex
    float getEtaFromVertex(const StThreeVectorF&,int);        ///< Return eta of the tower considering the collision vertex
    float getEtaFromVertex(StMcVertex*,int);      ///< Return eta of the tower considering the collision vertex
    float getPhiFromVertex(const StThreeVectorF&,int);        ///< Return phi of the tower considering the collision vertex
    float getPhiFromVertex(StMcVertex*,int);      ///< Return phi of the tower considering the collision vertex

protected:     

    StEmcGeom* mGeom[4];   //!

    ClassDef(StMuEmcPosition,1)

	};
#endif // STMUEMCPOSITION_H
