// $Id: StiKFVertexMaker.h,v 2.5 2018/01/03 21:23:36 smirnovd Exp $
#ifndef STAR_StiKFVertexMaker
#define STAR_StiKFVertexMaker
/*!
 *                                                                     
 * \class  StiKFVertexMaker
 * \author fisyak
 * \date   2018/10/23
 *
 */                                                                      
#include "StKFVertexMaker/StKFVertexMaker.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiKalmanTrack.h"
class StiHit;
class StiKFVertexMaker : public StKFVertexMaker {
 public: 
 StiKFVertexMaker(const Char_t *name="StiKFVertex") : StKFVertexMaker(name) {}
 virtual                        ~StiKFVertexMaker() {}					   		   
 virtual KFParticle 	 	*AddTrackAt(const StiKalmanTrackNode *tNode,Int_t kg);		  		   
 virtual StPrimaryTrack 	*FitTrack2Vertex(StKFVertex *V, StKFTrack* track);	  		   
 virtual void                 	 ReFitToVertex();  // refit Sti Track to primary vertices			   
 virtual void                    UpdateParticleAtVertex(StiKalmanTrack */* kTrack */, KFParticle */* particle */);
 virtual Int_t                   Make();
 private:
 /// Displayed on session exit, leave it as-is please ...
 virtual const char *GetCVS() const {
   static const char cvs[]="Tag $Name:  $ $Id: StiKFVertexMaker.h,v 2.5 2018/01/03 21:23:36 smirnovd Exp $ built " __DATE__ " " __TIME__ ; 
   return cvs;
 }
  ClassDef(StiKFVertexMaker,0)   //StAF chain virtual base class for Makers
};
// $Log: StiKFVertexMaker.h,v $
#endif /* STAR_StiKFVertexMaker */
