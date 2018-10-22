// $Id: StiKFVertexMaker.h,v 2.5 2018/01/03 21:23:36 smirnovd Exp $

#ifndef STAR_StiKFVertexMaker
#define STAR_StiKFVertexMaker

/*!
 *                                                                     
 * \class  StiKFVertexMaker
 * \author fisyak
 * \date   2012/04/18
 * \brief  virtual base class for Maker
 *
 */                                                                      
#include "StKFVertexMaker/StKFVertexMaker.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiKalmanTrack.h"
class StiKFVertexMaker : public StKFVertexMaker {
 public: 
  StiKFVertexMaker(const Char_t *name="SiKFVertex") {}
  virtual                       ~StiKFVertexMaker() {}					   		   
  virtual StPrimaryTrack 	*FitTrack2Vertex(StKFVertex *V, StKFTrack* track);	  		   
  TH1F           	 	*GetVertexZPlots(Int_t pass = 0) {return fVertexZPlots[pass];}	  		   
  virtual Int_t  	 	 Make();							  		   
  void                           ReFitToVertex();  // refit Sti Track to primary vertices			   
  void                   	 UpdateParticleAtVertex(StiKalmanTrack */* kTrack */, KFParticle */* particle */);
 private:
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StiKFVertexMaker.h,v 2.5 2018/01/03 21:23:36 smirnovd Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StiKFVertexMaker,0)   //StAF chain virtual base class for Makers
};
// $Log: StiKFVertexMaker.h,v $
// Revision 2.5  2018/01/03 21:23:36  smirnovd
// StiKFVertexMaker: Added missing include
//
// Revision 2.4  2014/08/06 11:43:59  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 2.3  2013/04/10 22:14:20  fisyak
// Roll back to version 04/04/2013
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StiKFVertexMaker
//
#endif /* STAR_StiKFVertexMaker */
