
#ifndef STAR_StSvtVertF
#define STAR_StSvtVertF
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// SVT private vertex finder                                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_scs_spt;
class St_dst_vertex;
class TH1F; 

class StSvtVertexFinderMaker : public StMaker
{
 public: 
  StSvtVertexFinderMaker(const char *name = "SvtVertF");
  StSvtVertexFinderMaker(StSvtVertexFinderMaker& SvtVertF);
  ~StSvtVertexFinderMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  Int_t MakeHistograms( St_dst_vertex* vtx);

 protected:

  TH1F* mVtxZ;   //! Vertex resolution compared to TPC
  TH1F* mVtxZGe;   //! Vertex resolution compared to GEANT

  ClassDef(StSvtVertexFinderMaker,1)   //virtual base class for Makers

};


#endif
