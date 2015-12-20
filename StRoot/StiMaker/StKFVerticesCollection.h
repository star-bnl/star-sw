#ifndef __StKFVerticesCollection_h__
// $Id: StKFVerticesCollection.h,v 2.3 2015/12/20 01:06:39 fisyak Exp $
#define __StKFVerticesCollection_h__
#include "TObject.h"
#include "TObjArray.h"
#include "TList.h"
#include "StKFVertex.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH1K.h"
class St_vertexSeedC;
class StKFVerticesCollection;
std::ostream&  operator<<(std::ostream& os,  const StKFVerticesCollection& vc);
class StKFVerticesCollection : public TObject  {
public:
  StKFVerticesCollection(Int_t NoPeaks = 0, Double_t *zOfPeaks = 0, Double_t sigmaXY = 1.5, Double_t sigmaZ = 2, St_vertexSeedC *vSeed = 0); 
  void AddVertex(Double_t x, Double_t y, Double_t z, Double_t sigmaXY, Double_t sigmaZ);
  void AddVertex(StKFVertex*vtx) {fVertices.AddLast(vtx);}
  virtual  ~StKFVerticesCollection() {}
  Int_t     NoVertices() const;
  Bool_t    IsEmpty() {return fVertices.IsEmpty();}
  StKFVertex*   Remove(Int_t k=0)           {return (StKFVertex *) fVertices.RemoveAt(k);}
  void      DoTrack2VertexAssociation(const TObjArray &particles, Int_t *Parents); //   associate tracks to vertex
  void      UpdateStVertexTrackAssociation();                 // reassociate tracks to vertex
  void      MergeDuplicatedVertices();
  Double_t  UpdateWeights();
  void      UniqueTracks2VertexAssociation();
  Double_t  Fit(Int_t marker = 0, TCanvas *c1 = 0, TH1* VertexZPlot = 0);
  TList    *Vertices() {return &fVertices;}
  const TList    *Vertices() const {return &fVertices;}
  void operator +=(StKFVerticesCollection &col);
  void      SetMc();
  void      SetParents(Int_t *parents) const;
  virtual void Print(const Option_t*  opt = "") const  {if (opt) {};  std::cout << *this;}
  static  void SetVxPenaltyFactor(Double_t chi2 = 1000) {fgVxPenaltyFactor = chi2;}
 private:
  TList fVertices;
  static Double_t fgVxPenaltyFactor;
 public:
  ClassDef(StKFVerticesCollection,0)
};
class Map_t {
 public:
  Map_t(StKFVertex *v, StKFTrack *t, Double_t W) : vert(v), track(t), Weight(W) {}
    virtual ~Map_t() {}
    StKFVertex *vert;
    StKFTrack  *track;
    Double_t    Weight;
};
// $Log: StKFVerticesCollection.h,v $
// Revision 2.3  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.3  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
// Revision 1.3  2014/01/14 14:49:18  fisyak
// Freeze
//
// Revision 1.2  2013/10/16 13:19:15  fisyak
// Add beam line position to PV guess, add Z error in beam track, relax requirements on vertex seed
//
// Revision 1.1.1.1  2013/08/13 22:20:41  fisyak
// Save m version
//
// Revision 2.2  2012/06/11 15:33:41  fisyak
// std namespace
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.3  2012/03/26 23:42:36  fisyak
// Add beam constrain
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
#endif
