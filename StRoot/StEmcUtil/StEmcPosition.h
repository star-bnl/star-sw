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

class StTrack;
class StMcTrack;
class StEmcGeom;
class StVertex;
class StMcVertex;

class StEmcPosition : public TObject
{
  public:            

             StEmcPosition();
    virtual  ~StEmcPosition();

    Bool_t            projTrack(StThreeVectorD*, StThreeVectorD*, StTrack*, Double_t, Double_t = 225.405);    ///< Track projection utility
    Bool_t            projTrack(StThreeVectorD*, StThreeVectorD*, StMcTrack*, Double_t, Double_t = 225.405);  ///< Track projection utility
    
    Bool_t            trackOnEmc(StThreeVectorD*, StThreeVectorD*, StTrack*, Double_t, Double_t = 225.405);   ///< Track projection utility
    Bool_t            trackOnEmc(StThreeVectorD*, StThreeVectorD*, StMcTrack*, Double_t, Double_t = 225.405); ///< Track projection utility
    
    Int_t             getTowerEtaPhi(Double_t, Double_t, Float_t*, Float_t*);                                 ///< Return tower eta/phi
    Int_t             getNextTowerId(Float_t, Float_t, Int_t, Int_t);                                         ///< Return neighbor tower id's
    Float_t           getDistTowerToTrack(Double_t, Double_t, Int_t, Int_t);                                  ///< Return distance from track to center of one tower
    
    StThreeVectorF    getPosFromVertex(StVertex*,Int_t);        ///< Return Position from collision vertex
    StThreeVectorF    getPosFromVertex(StMcVertex*,Int_t);      ///< Return position from collision vertex
    Float_t           getThetaFromVertex(StVertex*,Int_t);      ///< Return theta of the tower considering the collision vertex
    Float_t           getThetaFromVertex(StMcVertex*,Int_t);    ///< Return theta of the tower considering the collision vertex
    Float_t           getEtaFromVertex(StVertex*,Int_t);        ///< Return eta of the tower considering the collision vertex
    Float_t           getEtaFromVertex(StMcVertex*,Int_t);      ///< Return eta of the tower considering the collision vertex
    Float_t           getPhiFromVertex(StVertex*,Int_t);        ///< Return phi of the tower considering the collision vertex
    Float_t           getPhiFromVertex(StMcVertex*,Int_t);      ///< Return phi of the tower considering the collision vertex

  protected:     

    StEmcGeom* mBemcGeom;  //!

  ClassDef(StEmcPosition,1)

};
#endif
