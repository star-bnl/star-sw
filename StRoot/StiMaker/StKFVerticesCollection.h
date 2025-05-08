#ifndef __StKFVerticesCollection_h__
// $Id: StKFVerticesCollection.h,v 2.2 2012/06/11 15:33:41 fisyak Exp $
#define __StKFVerticesCollection_h__
#include "TObject.h"
#include "TObjArray.h"
#include "StKFVertex.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH1K.h"
class StKFVerticesCollection;
std::ostream&  operator<<(std::ostream& os,  const StKFVerticesCollection& vc);
class StKFVerticesCollection : public TObject  {
public:
  StKFVerticesCollection(Int_t NoPeaks = 0, Double_t *zOfPeaks = 0, Double_t sigmaXY = 1.5, Double_t sigmaZ = 2); 
  void AddVertex(Double_t x, Double_t y, Double_t z, Double_t sigmaXY, Double_t sigmaZ);
  void AddVertex(StKFVertex*vtx) {fVertices.AddLast(vtx);}
  virtual  ~StKFVerticesCollection() {}
  Int_t     NoVertices() const  {return fVertices.GetEntriesFast();}
  StKFVertex*   Remove(Int_t k=0)           {return (StKFVertex *) fVertices.RemoveAt(k);}
  Double_t  DoTrack2VertexAssociation(const TObjArray &particles); //   associate tracks to vertex
  Double_t  UpdateStVertexTrackAssociation();                 // reassociate tracks to vertex
  void      CleanDuplicatedVertices();
  void      MergeDuplicatedVertices();
  void      UpdateWeights();
  void      UniqueTracks2VertexAssociation();
  void      Compress() {fVertices.Compress();}
  Double_t  Fit(Int_t marker = 0, TCanvas *c1 = 0, TH1* Vtx = 0);
  StKFVertex *&Vertex(Int_t l) {return (StKFVertex *&) fVertices[l];}
  const StKFVertex *Vertex(Int_t l) const {return (const StKFVertex *) fVertices[l];}
  void operator +=(StKFVerticesCollection &col);
  void     SetMc(Int_t NoMuMcVertex = 0, Int_t NoMuMcTrack = 0, const Float_t *time = 0,
		 const Float_t *x = 0,const Float_t *y = 0,const Float_t *z = 0,
		 const Int_t *NoDaughters = 0,const Int_t *IdParTrk = 0,const Int_t *gePid = 0);
  virtual void Print(const Option_t*  opt = "") const  {if (opt) {};  std::cout << *this;}
  static  void SetVxPenaltyFactor(Double_t chi2 = 1000) {fgVxPenaltyFactor = chi2;}
 private:
  TObjArray fVertices;
  static Double_t fgVxPenaltyFactor;
 public:
  ClassDef(StKFVerticesCollection,0)
};
// $Log: StKFVerticesCollection.h,v $
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
