//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCTrack.h,v 1.7 2012/07/03 19:38:23 ramdebbe Exp $
// $Log: StPeCTrack.h,v $
// Revision 1.7  2012/07/03 19:38:23  ramdebbe
// raised ClassDef from 1 to 2
//
// Revision 1.6  2012/06/13 16:09:54  ramdebbe
// Overload constructor and set method to pass vertex information in StMuEvent
//
// Revision 1.5  2003/11/25 01:54:36  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.4  2002/12/19 18:09:53  yepes
// MuDST input added
//
// Revision 1.3  2001/04/23 21:44:38  meissner
// add dEdx z variable to tree, setFormat(1) for tree, use private BetheBloch (temp solution)
//
// Revision 1.2  2001/02/21 20:54:25  yepes
// *** empty log message ***
//
// Revision 1.1  2000/04/21 19:12:25  nystrand
// First Version
//
// Revision 1.1  2000/03/24 22:36:56  nystrand
// First version of StPeCTrack
//
// Revision 1.0  2000/01/20 23:28:51  nystrand
// First Version of StPeCTrack 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCTrack
//
// Pair class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCTrack_h
#define StPeCTrack_h
#include "Rtypes.h"
#include "TObject.h"
#include "TClonesArray.h"
#include "StPeCEnumerations.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#endif /* __CINT__ */
#include "SystemOfUnits.h"

class StPeCTrack : public TObject {

public:

                                  StPeCTrack();
  virtual                         ~StPeCTrack();

  void                            calculatePair4Momentum( ) ;
  Int_t                           fill ( ) ;
#ifndef __CINT__
  StPeCTrack(Int_t _primary, StMuTrack *trk, StMuEvent * mudst);
	void set ( Int_t _primary, StMuTrack* trk, StMuEvent *mudst);
        StPeCTrack ( Int_t _primary, StTrack *trk);
        void set ( Int_t _primary, StTrack* trk);
#endif /*__CINT__*/
  Int_t                           key ;
  Int_t                           charge ;
  Bool_t                          primary ;
  Float_t                         pt ;
  Float_t                         p ;   // momentum
  Float_t                         eta ;
  Float_t                         psi ; 
  Float_t                         phi0 ;
  Float_t                         length ;
  Int_t                           vertexIndex;
  Float_t                         vtxX;
  Float_t                         vtxY;
  Float_t                         vtxZ;
  Float_t                         r0 ;
  Float_t                         z0 ;
  Float_t                         dedx ;
  Float_t                         dedxZel ; // This is not c++ !!
  Float_t                         dedxZmu;
  Float_t                         dedxZpi ;
  Float_t                         dedxZk ;
  Float_t                         dedxZp ;
  Float_t                         nHits ;
  Float_t                         nSigmaEl;
  Float_t                         nSigmaPi;
  Float_t                         nSigmaK;
  Float_t                         nSigmaP;



  

  Float_t                         getZdEdx(Float_t mass);

  ClassDef(StPeCTrack,2)  
};




#endif





