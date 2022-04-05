//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCPair.h,v 1.12 2014/12/23 20:43:18 ramdebbe Exp $
// $Log: StPeCPair.h,v $
// Revision 1.12  2014/12/23 20:43:18  ramdebbe
// copied the fill functionality of method with both inputs to the one with MuDst input. This gives TOF extrapolation in the pPairs branch
//
// Revision 1.11  2013/12/27 16:52:31  ramdebbe
// added a input argument StBTofGeometry to fill method (StMuDst + StEvent) and x y z coordinates of intercept to TOF cylinder
//
// Revision 1.10  2012/07/03 19:37:40  ramdebbe
// raised ClassDef from 1 to 2
//
// Revision 1.9  2012/06/13 15:34:36  ramdebbe
// added tof information to both tracks
//
// Revision 1.8  2004/01/26 23:01:11  perev
// WarnOff
//
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
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcUtil/filters/StEmcFilter.h"
#include "StBTofUtil/StBTofGeometry.h"



typedef vector<Int_t>  IntVec;
typedef vector<UInt_t>  UIntVec;
typedef vector<Double_t>  DoubleVec;
typedef vector<StThreeVector<double> >  PointVec;

class StPeCPair : public TObject {

public:

                                  StPeCPair();
  virtual                         ~StPeCPair();
  void                            Clear(const char* opt=0);

  void                            calculatePair4Momentum( ) ;
#ifndef __CINT__
                                  StPeCPair ( StMuTrack *trk1, StMuTrack *trk2, 
				              Bool_t primaryFlag, StMuEvent* event,  StBTofGeometry * pairTOFgeo );

                                  StPeCPair ( StTrack *trk1, StTrack *trk2, 
				              Bool_t primaryFlag, StEvent* event );

                                  StPeCPair ( StMuTrack *trk1, StMuTrack *trk2, 
				              Bool_t primaryFlag, StMuEvent* event, StEvent* eventP,  StBTofGeometry * pairTOFgeo );

  Int_t                           fill ( Bool_t primaryFlag, StEvent* event ) ;
  Int_t                           fill ( Bool_t primaryFlag, StMuEvent* event ) ;
  Int_t                           fill ( Bool_t primaryFlag, StMuEvent* event,  StEvent* eventP ) ;


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
  Int_t                           tr1_bemcModule;
  Int_t                           tr1_bemcEtabin;
  Float_t                         tr1_bemcEtaValue;
  Float_t                         tr1_bemcPhiValue;
  Int_t                           tr1_bemcSub;  
  Float_t                         tr1_bemcEnergy;
  StPeCTrack                      tr2 ;
  Int_t                           tr2_bemcModule;
  Int_t                           tr2_bemcEtabin;
  Float_t                         tr2_bemcEtaValue;
  Float_t                         tr2_bemcPhiValue;
  Int_t                           tr2_bemcSub;  
  Float_t                         tr2_bemcEnergy;

  Int_t                           tr1_bsmdeModule;
  Int_t                           tr1_bsmdeEtabin;
  Float_t                         tr1_bsmdeEtaValue;
  Float_t                         tr1_bsmdePhiValue;
  Int_t                           tr1_bsmdeSub;  
  Float_t                         tr1_bsmdeEnergy;

  Int_t                           tr2_bsmdeModule;
  Int_t                           tr2_bsmdeEtabin;
  Float_t                         tr2_bsmdeEtaValue;
  Float_t                         tr2_bsmdePhiValue;
  Int_t                           tr2_bsmdeSub;  
  Float_t                         tr2_bsmdeEnergy;

  float                           tr1_timeOfFlight;
  float                           tr1_pathLength;
  float                           tr1_Beta;
  float                           tr2_timeOfFlight;
  float                           tr2_pathLength;
  float                           tr2_Beta;

  float                           tr1_extrapolatedTOF_mX;
  float                           tr1_extrapolatedTOF_mY;
  float                           tr1_extrapolatedTOF_mZ;

  float                           tr2_extrapolatedTOF_mX;
  float                           tr2_extrapolatedTOF_mY;
  float                           tr2_extrapolatedTOF_mZ;

  StBTofGeometry                * pairTOFgeoLocal; //!
  
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
  StEmcGeom *                     mGeom0;
  StEmcGeom *                     mGeom2;    
  StEmcGeom *                     mGeom3;



  Double_t                        bFld;  
#endif /*__CINT__*/

  ClassDef(StPeCPair,2)
};

#endif





