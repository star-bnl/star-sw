/*
 * Created by S. Gliske, May 2012
 *
 * Description: McParticle_t, used in the McEEmcTree.
 *
 * Note: this class does not explicitly depend on the STAR framework,
 * and so the trees can be read outside of the STAR framework.  Note:
 * data members are all public, to allow a lighter weight implementation.
 * 
 *
 */

#ifndef McParticle_H_
#define McParticle_H_

#include <Rtypes.h>
#include <TObject.h>
#include <TVector3.h>

//
// McParticle_t
//
class McParticle_t : public TObject {
 public:
   McParticle_t();
   McParticle_t( Int_t pIdx, Int_t v1Idx, Int_t v2Idx, Long_t gPID, Long_t pPID, Float_t e, const TVector3& mom,
                 Short_t sector_, Float_t uPos_, Float_t vPos_, Float_t uE_, Float_t vE_, const TVector3& position,
                 Float_t eTow_, Float_t ePre1_, Float_t ePre2_, Float_t ePost_ );
   virtual ~McParticle_t();

   void Clear( const Option_t* );

   Int_t parentIdx;      // idx in TClonesArray of parent particle
   Int_t startVertexIdx; // idx in TClonesArray of starting vertex
   Int_t stopVertexIdx;  // idx in TClonesArray of stop (ending) vertex

   Long_t gId;           // Geant PID, copied from StMcTrack
   Long_t pId;           // PDG PID, copied from StMcTrack

   Float_t E;            // copied from StMcTrack
   TVector3 momentum;    // copied from StMcTrack

   Short_t sector;       // sector in which the most energy is deposited in the SMD
   Float_t uPos, vPos;   // energy weighted mean strip position in each layer, in the above sector
   Float_t uE, vE;       // energy in the u and v layers, in the specified sector
   TVector3 position;    // where it hits the detector, computed based on uPos, vPos.
                         // Set to 0,0,0 if doesn't hit

   Float_t eTow, ePre1, ePre2, ePost; // energy deposited in each of these layers

 private:
   ClassDef( McParticle_t, 3 );
};

// inline functions

inline McParticle_t::McParticle_t( Int_t pIdx, Int_t v1Idx, Int_t v2Idx, Long_t gPID, Long_t pPID,
                                   Float_t e, const TVector3& mom, Short_t sector_,
                                   Float_t uPos_, Float_t vPos_, Float_t uE_, Float_t vE_,
                                   const TVector3& pos,
                                   Float_t eTow_, Float_t ePre1_, Float_t ePre2_, Float_t ePost_ )
   : parentIdx(pIdx), startVertexIdx(v1Idx), stopVertexIdx(v2Idx),
     gId(gPID), pId(pPID), E(e), momentum(mom), sector(sector_),
     uPos(uPos_), vPos(vPos_), uE(uE_), vE(vE_), position(pos),
     eTow(eTow_), ePre1(ePre1_), ePre2(ePre2_), ePost(ePost_) { /* */ };

inline void McParticle_t::Clear( const Option_t* ){
   parentIdx = startVertexIdx = stopVertexIdx = -1;
   gId = pId = 0;
   E = 0;
   sector = 0;
   uPos = vPos = -1;
   uE = vE = eTow = ePre1 = ePre2 = ePost = 0;

   momentum.SetXYZ(0,0,0);
   position.SetXYZ(0,0,0);
};

#endif

/*
 * $Id: McParticle.h,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: McParticle.h,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
