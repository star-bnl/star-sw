
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
class TH2F; 

class TNtuple;
class TFile;

class StSvtVertexFinderMaker : public StMaker
{
 public: 
  StSvtVertexFinderMaker(const char *name = "SvtVertF");
  StSvtVertexFinderMaker(StSvtVertexFinderMaker& SvtVertF);
  ~StSvtVertexFinderMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  void MakeHistograms( St_dst_vertex* vtx, long event);

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSvtVertexFinderMaker.h,v 1.7 2003/09/10 19:47:35 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}
 
 protected:

  TH1F* mVtxZDiff2;   //! Vertex resolution compared to TPC
  TH1F* mVtxZDiff1;   //! Vertex resolution compared to GEANT
  TH1F* mVtxZTpc;   //! Vertex from TPC
  TH1F* mVtxZGe;   //! Vertex from GEANT
  TH1F* mVtxZSvt;   //! Vertex from SVT
  TH1F* mVtxNSvt;   //! Vertex from SVT
  TH1F** mtemp;   //! Vertex from SVT
  TH2F** mgrid;   //! Vertex from SVT
  TNtuple* ntuple2;
  TNtuple* ntuple3;
  TFile *hfile2;

  ClassDef(StSvtVertexFinderMaker,0)   //virtual base class for Makers

};


#endif
