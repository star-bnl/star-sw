//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCPair.h,v 1.7 2003/11/25 01:54:34 meissner Exp $
// $Log: StPeCPair.h,v $
// Revision 1.7  2003/11/25 01:54:34  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.6  2003/02/05 17:14:06  yepes
// Adding bField and pPairs.psi to tree
//
// Revision 1.5  2002/12/16 23:04:02  yepes
// Field comes in KGauss and should be passed to routines in Teslas
// problem pointed out by Vladimir
//
// Revision 1.4  2001/02/21 20:42:14  yepes
// Add ctb signals to tree
//
// Revision 1.3  2001/02/12 21:16:01  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.1  2000/04/21 19:12:25  nystrand
// First Version
//
// Revision 1.1  2000/03/24 22:36:56  nystrand
// First version of StPeCPair
//
// Revision 1.0  2000/01/20 23:28:51  nystrand
// First Version of StPeCPair 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCPair
//
// Pair class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCPair_h
#define StPeCPair_h
#include "Rtypes.h"
#include "TObject.h"
#include "TClonesArray.h"
#include "StPeCEnumerations.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#include "StEvent.h"
#endif /* __CINT__ */
#include "SystemOfUnits.h"
#include "StPeCSpec.h"
#include "StPeCTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

class StPeCPair : public TObject {

public:

                                  StPeCPair();
  virtual                         ~StPeCPair();
  void                            Clear();

  void                            calculatePair4Momentum( ) ;
#ifndef __CINT__
                                  StPeCPair ( StMuTrack *trk1, StMuTrack *trk2, 
				              Bool_t primaryFlag, StMuEvent* event );

                                  StPeCPair ( StTrack *trk1, StTrack *trk2, 
				              Bool_t primaryFlag, StEvent* event );

  Int_t                           fill ( Bool_t primaryFlag, StEvent* event ) ;
  Int_t                           fill ( Bool_t primaryFlag, StMuEvent* event ) ;
  Int_t                           fill ( Bool_t primaryFlag, StEventSummary* summary,
                                         StThreeVectorF& p1, StPhysicalHelixD& h1, short charge1,
                                         StThreeVectorF& p2, StPhysicalHelixD& h2, short charge2,    
                                         StThreeVectorF& primaryVertexPosition ) ;

  void                            setTrack1(StTrack* trk);
  void                            setTrack2(StTrack* trk);

  void                            setTrack1(StMuTrack* trk);
  void                            setTrack2(StMuTrack* trk);

  StTrack*                        getTrack1();
  StTrack*                        getTrack2();

  StMuTrack*                      getMuTrack1();
  StMuTrack*                      getMuTrack2();

  StLorentzVectorF                getPair4Momentum(StPeCSpecies pid) const;
#endif /*__CINT__*/
  Int_t                           getSumCharge() const;
  Float_t                         getSumPt() const;
  Float_t                         getSumPz() const;
  Float_t                         getMInv(StPeCSpecies pid) const;
  Float_t                         getOpeningAngle() const;
  Float_t                         getCosThetaStar(StPeCSpecies pid) const;
  Float_t                         getPartDca ( ) { return pPartDca ; } ;
  Float_t                         getV0Dca ( ) { return pV0Dca ; } ;

private:

  Int_t                           pCharge ;
  Float_t                         pPt ;
  Float_t                         pPz ;
  Float_t                         pPsi ;
  Float_t                         pAngle ;
  Float_t                         pXyAngle ;
  Float_t                         pPtArm ; // Armenteros pt: pt positive along pair momentum
  Float_t                         pAlpha ; // Armerteros alpha:
                                           //  (pl_pos-pl_neg)/(pl_pos+pl_neg)
                                           //  pl_pos(neg) : momentum along pair momentum for positive
					   //               (negative) charge
  Float_t                         pPartDca ; // Distance closest approach between particles
  Float_t                         pV0Dca ;   // Distance closest pair and vertex            
  Float_t                         rV0 ;
  Float_t                         phiV0 ;
  Float_t                         zV0 ;
  
  StPeCTrack                      tr1 ;
  StPeCTrack                      tr2 ;
  
  StPeCSpec                       pionH;
  StPeCSpec                       kaonH;
  StPeCSpec                       protonH;
  StPeCSpec                       electronH;
  StPeCSpec                       muonH;
  
#ifndef __CINT__
  StTrack*                        track1; //!
  StTrack*                        track2; //!
  StMuTrack*                      muTrack1; //!
  StMuTrack*                      muTrack2; //!
#endif /*__CINT__*/

  ClassDef(StPeCPair,1)
};

#endif





