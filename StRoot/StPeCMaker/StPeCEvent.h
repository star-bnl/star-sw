//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCEvent.h,v 1.16 2015/07/22 20:11:43 ramdebbe Exp $
// $Log: StPeCEvent.h,v $
// Revision 1.16  2015/07/22 20:11:43  ramdebbe
// removed RP container copy to output, needs to come back
//
// Revision 1.15  2015/02/25 01:34:26  ramdebbe
// added copy of the Roman Pot StMuRpsCollection to output tree
//
// Revision 1.14  2014/06/18 16:38:30  ramdebbe
// added more variables to event summary
//
// Revision 1.13  2013/12/27 16:49:58  ramdebbe
// added a set method setTOFgeometry to pass pointer to StBTofGeometry
//
// Revision 1.12  2013/01/24 15:41:24  ramdebbe
// added more flags to choose input or output tracks tof etc.
//
// Revision 1.11  2012/07/03 20:44:44  ramdebbe
// removed some comments
//
// Revision 1.10  2012/07/03 19:37:25  ramdebbe
// raised ClassDef from 1 to 2
//
// Revision 1.9  2012/06/13 15:45:06  ramdebbe
// Added flags to include TOF and Vertex branches in tree
//
// Revision 1.8  2003/11/25 01:54:28  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.7  2003/02/05 17:14:05  yepes
// Adding bField and pPairs.psi to tree
//
// Revision 1.6  2002/12/19 18:09:53  yepes
// MuDST input added
//
// Revision 1.5  2002/03/19 22:23:28  meissner
// New variables: zdc unatt., Trigger word, MC tree if Geant Branch, DCA  for primary pairs, all tracks for secondary pairs (Test)
//
// Revision 1.4  2001/04/27 19:32:07  akio
// fixing a #define conflict bug
//
// Revision 1.3  2001/02/12 21:15:44  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.2  2000/04/21 19:10:27  nystrand
// Include StPeCPair class
//
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
#include "StPeCEnumerations.h"
#include "TObject.h"
#include "TClonesArray.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
///#include "StEventTypes.h"
#endif /* __CINT__ */
#include "StPeCPair.h"
#include "StPeCTrack.h"
#include "SystemOfUnits.h"
#include "StEpcMaker/StEpcMaker.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StBTofUtil/StBTofGeometry.h"
#include "StMuDSTMaker/COMMON/StMuRpsCollection.h"


class StPeCEvent: public TObject {

public:

  StPeCEvent(bool useBemc, bool useTOF, bool useVertex, bool useTracks, bool useRP, bool readStMuDst, bool readStEvent, bool readBothInputs);
  virtual                         ~StPeCEvent();


  Long_t                          eventNumber() const;
  Long_t                          runNumber() const;
  Int_t                           getNTot() const;
  Int_t                           getNPrim() const;
  Int_t                           getQTot() const;
  Int_t                           getNPriPairs ( ) { return nPPairs ; } ;
  Int_t                           getNSecPairs ( ) { return nSPairs ; } ;
  StPeCPair*                      getPriPair ( Int_t i ) ;
  StPeCPair*                      getSecPair ( Int_t i ) ;
  Float_t                         getPt() const;
  Float_t                         yRap() const;
  Float_t                         getXVertex() const;
  Float_t                         getYVertex() const;
  Float_t                         getZVertex() const;
#ifndef __CINT__
  StMuDst*                        muDst;
  StEvent*                        eventP ;
  void                            addPair(StPeCPair* pair) const;
  Int_t                           fill ( StEvent* event ) ;
  Int_t                           fill(StMuDst* mudst);
  std::vector<int>                fill(StEvent* event, StMuDst* mudst);
  Int_t                           infoLevel ;
  void                            setInfoLevel ( Int_t in ) { infoLevel = in ; } ;
  void                            setTOFgeometry(StBTofGeometry * mTOFgeo) {mTOFgeoEv = mTOFgeo; };
  StLorentzVectorF                getEvent4Momentum(StPeCSpecies pid) const;
#endif /*__CINT__*/
  void                            clear ( ) ;
  void                            reset ( ) ;

  Float_t                         mInv(StPeCSpecies pid) const;
  Float_t                         yRap(StPeCSpecies pid) const;
  void                            matchTOFhitsToTracks(StMuDst *mudst);

private:
  Int_t                           eventN;
  Int_t                           runN;
  Int_t                           nTot;     //FLK keept for backwards compatibility
  Int_t                           nPrim;    //FLK keept for backwards compatibility
  Int_t                           qTot;     
  Int_t                           nGlobalTracks;
  Int_t                           nPrimaryTracks;
  Int_t                           nPrimaryTPC;
  Int_t                           nPrimaryFTPC;
  Float_t                         bField ;
  Float_t                         pt;
  Float_t                         xVertex;
  Float_t                         yVertex;
  Float_t                         zVertex;
  Float_t                         rVertex;
  Int_t                           nVertices;
  Int_t                           nTracks ;
  Int_t                           nPPairs ;
  Int_t                           nSPairs ;
  //
  //3-JUNE2014 add more information to event summary
  //
  Int_t                           nTOFhitsSum;
  Int_t                           nBtofTriggerHitsSum;
  Int_t                           nTOFtracksSum;
  Float_t                         zdcEastUASum;
  Float_t                         zdcWestUASum;
  Float_t                         zdcCoincidenceRateSum;
  unsigned short                  lastDSM0Sum;
  unsigned short                  lastDSM1Sum;
  TClonesArray                   *pPairs ;
  TClonesArray                   *sPairs ;
  TClonesArray                   *tracks;
  TClonesArray                   *treecalo;
  TClonesArray                   *tofHits;
  TClonesArray                   *tofTracks;
  TClonesArray                   *vertices;
/*   StMuRpsCollection              *romanPots; */

  StBTofGeometry                 *mTOFgeoEv;   //!
  Int_t                           shotCount;
  Bool_t                          useBemcLocal;
  Bool_t                          useTOFlocal;
  Bool_t                          useVertexLocal;
  Bool_t                          useTracksLocal;
  Bool_t                          useRPLocal;

  Bool_t                          readStMuDstLocal;
  Bool_t                          readStEventLocal;
  Bool_t                          readBothInputsLocal;

  ClassDef(StPeCEvent,2)
};

#endif





