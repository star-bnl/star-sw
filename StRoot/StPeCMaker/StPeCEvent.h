//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCEvent.h,v 1.1 2000/03/24 22:36:56 nystrand Exp $
// $Log: StPeCEvent.h,v $
// Revision 1.1  2000/03/24 22:36:56  nystrand
// First version of StPeCEvent
//
// Revision 1.0  2000/01/20 23:28:51  nystrand
// First Version of StPeCEvent 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCEvent
//
// Event class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCEvent_h
#define StPeCEvent_h
#include "Rtypes.h"
#include "StPeCTrackCollection.h"
#include "StPeCEnumerations.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#endif /* __CINT__ */
#include "SystemOfUnits.h"

class StPeCEvent{

public:

                                  StPeCEvent();
  virtual                         ~StPeCEvent();

  Long_t                          eventNumber() const;
  Long_t                          runNumber() const;
  Int_t                           globMultiplicity() const;
  Int_t                           primMultiplicity() const;
  Int_t                           qTot() const;
  Float_t                         pT() const;
  Float_t                         yRap() const;
  Float_t                         zVertex() const;
#ifndef __CINT__
  void                            addPeCPrimaryTrack(StTrack* trk) const;
  void                            addPeCNonPrimaryTrack(StTrack* trk) const;
  StPeCPrimaryTrackCollection*    getPeCPrimaryTrackCollection() const;
  StPeCNonPrimaryTrackCollection* getPeCNonPrimaryTrackCollection() const;
  StLorentzVectorF                getEvent4Momentum(StPeCParticle pid) const;
#endif /*__CINT__*/
  Float_t                         mInv(StPeCParticle pid) const;
  Float_t                         yRap(StPeCParticle pid) const;

  void                            setEventNumber(Long_t&);
  void                            setRunNumber(Long_t&);
  void                            setGlobMultiplicity(Int_t&);
  void                            setPrimMultiplicity(Int_t&);
  void                            setQTot(Int_t&);
  void                            setPT(Float_t&);
  void                            setYRap(Float_t&);
  void                            setZVertex(Float_t&);


private:

  Int_t                           mEventNumber;
  Int_t                           mRunNumber;
  Int_t                           mGlobMultiplicity;
  Int_t                           mPrimMultiplicity;
  Int_t                           mQTot;
  Float_t                         mPT;
  Float_t                         mZVertex;
#ifndef __CINT__
  StPeCPrimaryTrackCollection     *pPrim; //!
  StPeCNonPrimaryTrackCollection  *pNonPrim; //!
#endif /*__CINT__*/

  ClassDef(StPeCEvent,1)
};

#endif





