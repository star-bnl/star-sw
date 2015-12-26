#ifndef __StKFVertex_h__
#define __StKFVertex_h__
// $Id: StKFVertex.h,v 2.5 2015/12/20 01:06:39 fisyak Exp $
#include "Riostream.h"
#include "TObject.h"
#include "StKFTrack.h"
#include "TList.h"
#include "KFParticle/KFVertex.h"
#include "TVector3.h"
class StVertex;

class StKFVertex;
std::ostream&  operator<<(std::ostream& os,  const StKFVertex& v);
class StKFVertex : public KFVertex  {
public:
  StKFVertex() : KFVertex(), fTimeMc(0), fNoDaughtersMc(0), fgePidMc(0)
    { Clear(); Vertex().SetId(++fTotalNoVertices); fKFTracks.SetOwner(kTRUE);}
    StKFVertex(const StKFVertex&);
  virtual     ~StKFVertex() {Clear();}
  void         AddTrack(const StKFTrack *track);
  virtual void Clear(Option_t *opt="")     {fKFTracks.Clear(opt);}
  StKFTrack*   Remove(Int_t k=0)           {return (StKFTrack *) fKFTracks.RemoveAt(k);}
  StKFTrack*   Remove(StKFTrack *track)    {return (StKFTrack *) fKFTracks.Remove(track);}
  StKFTrack*   Remove(KFParticle *particle); 
  Int_t        ID()                  const {return   Vertex().Id();}
  KFVertex     Vertex()              const {return *(KFVertex *) this;}
  KFVertex    &Vertex()                    {return *(KFVertex *) this;}
  TList       &Tracks()                    {return *&fKFTracks;}
  Int_t        NoTracks()            const;
  Int_t        Charge()              const {return   Vertex().GetQ();}
  void         UpdateVertex2TrackChi2();
  Bool_t       IsEmpty()      {return fKFTracks.IsEmpty();}
  Bool_t       Fit(); 
  Int_t        IdTruth() const { return Vertex().IdTruth();}
  Int_t        QaTruth() const { return Vertex().QaTruth(); }
  Int_t        IdParentTk() const {return Vertex().GetParentID();}
  void         SetIdTruth(Int_t idtru,Int_t qatru=0) {Vertex().SetIdTruth(idtru,qatru);}
  void         SetIdParentTk(Int_t id) {Vertex().SetParentID(id);}
  void         SetParent(Int_t *parents) const;
  void         ResetParticles();
  void         CheckBeamConstraint();
  Int_t        Q() const {return Vertex().GetQ();}
  StKFVertex &operator +=(StKFVertex &vtx);
  StKFVertex &operator  =(const StKFVertex &vtx);
  StKFVertex &operator  =(const KFVertex &vtx);
  void Print(Option_t *option="") const {std::cout << option << *this << std::endl; }
  void PrintW(Option_t *option="") const;
  void SetMc(Float_t time, Float_t x, Float_t y, Float_t z, Int_t NoDaughters, Int_t gePid);
  Float_t   TimeMc()   const {return fTimeMc;}
  const TVector3 &XyzMc()    const {return *&fXyzMc;}
  Int_t     NoDaughtersMc() const {return fNoDaughtersMc;}
  Int_t     gePidMc()  const {return fgePidMc;}
  Double_t  Chi2AtVx();
  static void      SetDebug(Int_t k = 0) {_debug = k;}
  static Int_t     Debug() {return _debug;}
  static void      ResetTotalNoVertices(Int_t n = 0) {fTotalNoVertices = n;}

private:
  TList     fKFTracks;
  Float_t   fTimeMc;
  TVector3  fXyzMc;
  Int_t     fNoDaughtersMc;
  Int_t     fgePidMc;
  static Int_t fTotalNoVertices;
 public:
  static Int_t _debug;
  static const Char_t *GeNames[52];
  ClassDef(StKFVertex,0)
};
// $Log: StKFVertex.h,v $
// Revision 2.5  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.5  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
// Revision 2.3  2013/04/08 19:21:41  fisyak
// Adjust for new KFParticle
//
// Revision 2.2  2012/06/11 15:33:41  fisyak
// std namespace
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.3  2012/03/29 23:35:47  fisyak
// Fix problem with multiple beam tracks
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
#endif
