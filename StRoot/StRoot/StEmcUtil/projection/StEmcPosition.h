//******************************************************************************
//                                                                            
// StEmcPosition.h
//
// Authors: Marcia Maria de Moura
//
// Initial version: 2001/12/21
//
//******************************************************************************

/*! \class StEmcPosition
\author Marcia M. de Moura

This class has a set of track projection utilities for EMC and a few tools to get neighbor
towers and real eta/phi positions considering the collision vertex.

*/

#ifndef StEmcPosition_H
#define StEmcPosition_H

#include "TObject.h"

#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"

class StTrack;
class StMuTrack;
class StMcTrack;
class StEmcGeom;
class StVertex;
class StMcVertex;

class StEmcPosition : public TObject
{
  public:            

             StEmcPosition();
    virtual  ~StEmcPosition();

    Bool_t            projTrack(StThreeVectorD* atFinal,StThreeVectorD* momentumAtFinal,const StTrack* const track,Double_t magField,Double_t radius=225.405,Int_t option=1) const;    ///< Track projection utility
    Bool_t            projTrack(StThreeVectorD* atFinal,StThreeVectorD* momentumAtFinal,const StMuTrack* const track,Double_t magField,Double_t radius=225.405,Int_t option=1) const;    ///< Track projection utility
    Bool_t            projTrack(StThreeVectorD* atFinal,StThreeVectorD* momentumAtFinal,const StMcTrack* const mcTrack,Double_t magField,Double_t radius=225.405,Int_t option=1) const;  ///< Track projection utility
    Bool_t            projTrack(StThreeVectorD* atFinal,StThreeVectorD* momentumAtFinal,const StPhysicalHelixD* const helix,Double_t magField,Double_t radius=225.405,Int_t option=1) const;  ///< Track projection utility
    
    Bool_t            trackOnEmc(StThreeVectorD* position,StThreeVectorD* momentum,const StTrack* const track,Double_t magField,Double_t emcRadius=225.405) const;   ///< Track projection utility
    Bool_t            trackOnEmc(StThreeVectorD* position,StThreeVectorD* momentum,const StMuTrack* const track,double magField,double emcRadius=225.405) const;   ///< Track projection utility
    Bool_t            trackOnEmc(StThreeVectorD* position,StThreeVectorD* momentum,const StMcTrack* const mcTrack,Double_t magField,Double_t emcRadius=225.405) const; ///< Track projection utility
    
    Int_t             getTowerEtaPhi(Double_t eta, Double_t phi, Float_t* towerEta, Float_t* towerPhi) const; ///< Return tower eta/phi

    Int_t             getNextTowerId(Float_t eta, Float_t phi, Int_t nTowersdEta, Int_t nTowersdPhi) const; ///< Return neighbor tower id's
    Int_t             getNextTowerId(Int_t softId, Int_t nTowersdEta, Int_t nTowersdPhi) const; ///< Return neighbor tower id's
    Int_t             getNextTowerId(Int_t m, Int_t e, Int_t s, Int_t nTowersdEta, Int_t nTowersdPhi) const; ///< Return neighbor tower id's
	Int_t             getNextId(Int_t det, Int_t m, Int_t e, Int_t s, Int_t nEta, Int_t nPhi) const; ///< Return neighbor id (works for all detectors 1=bemc, 2=bprs, 3=bsmde, 4=bsmdp)
	Int_t getNextId(Int_t det, Int_t softId, Int_t nEta, Int_t nPhi)const;///< Return neighbor id (works for all detectors 1=bemc, 2=bprs, 3=bsmde, 4=bsmdp)

	Float_t           getDistTowerToTrack(Double_t trackEta, Double_t trackPhi, Int_t nTowersdEta, Int_t nTowersdPhi) const; ///< Return distance from track to center of one tower

    StThreeVectorF    getPosFromVertex(const StVertex* const vertex, Int_t TowerId) const; ///< Return Position from collision vertex
    StThreeVectorF    getPosFromVertex(const StThreeVectorF& position, Int_t TowerId) const; ///< Return Position from collision vertex
    StThreeVectorF    getPosFromVertex(const StMcVertex* const vertex, Int_t TowerId) const; ///< Return position from collision vertex
    Float_t           getThetaFromVertex(const StVertex* const vertex, Int_t TowerId) const; ///< Return theta of the tower considering the collision vertex
    Float_t           getThetaFromVertex(const StThreeVectorF& vertex, Int_t TowerId) const; ///< Return theta of the tower considering the collision vertex
    Float_t           getThetaFromVertex(const StMcVertex* const vertex, Int_t TowerId) const; ///< Return theta of the tower considering the collision vertex
    Float_t           getEtaFromVertex(const StVertex* const vertex, Int_t TowerId) const; ///< Return eta of the tower considering the collision vertex
    Float_t           getEtaFromVertex(const StThreeVectorF& vertex, Int_t TowerId) const; ///< Return eta of the tower considering the collision vertex
    Float_t           getEtaFromVertex(const StMcVertex* const vertex, Int_t TowerId) const; ///< Return eta of the tower considering the collision vertex
    Float_t           getPhiFromVertex(const StVertex* const vertex, Int_t TowerId) const; ///< Return phi of the tower considering the collision vertex
    Float_t           getPhiFromVertex(const StThreeVectorF& vertex, Int_t TowerId) const; ///< Return phi of the tower considering the collision vertex
    Float_t           getPhiFromVertex(const StMcVertex* const vertex, Int_t TowerId) const; ///< Return phi of the tower considering the collision vertex

  protected:     

    StEmcGeom* mGeom[4];   //!

  ClassDef(StEmcPosition,2)

};
#endif
