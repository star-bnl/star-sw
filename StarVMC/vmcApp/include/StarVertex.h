// $Id: StarVertex.h,v 1.1 2004/09/03 00:00:49 potekhin Exp $

#ifndef STARVERTEX_H
#define STARVERTEX_H

#include <TObject.h>
#include <TObjArray.h>

#include <TVector3.h>

#include <StarParticle.h>

class StarVertex : public TObject {
 public:

  StarVertex(StarParticle* p_, Float_t x_, Float_t y_, Float_t z_);
  StarVertex(void); // for testing purposes

  virtual ~StarVertex();

  Int_t    GetNumber(void)          const {return _number;}
  Int_t    GetParentNumber(void)    const {return _parent->GetID();}

  Float_t x(void) {return _x;}
  Float_t y(void) {return _y;}
  Float_t z(void) {return _z;}

  void AddParticle  (StarParticle* p_);
  void AddParticleID(Int_t id_);


  static TObjArray* Vertices(void)         {return _vertices;}
  static Int_t      GetNextNumber(void)    {return _nVertices++;}

  static void       resetNumber(void)      {_nVertices=0;}
  static void       Reset(void);
  static void       AddNewVertex(TObjArray* a_, StarVertex* v_);


  TObjArray*      Particles(void)  {return _particles;}


  void Print(void) const;


 private:
  StarParticle*     _parent;

  Float_t           _x;
  Float_t           _y;
  Float_t           _z;

  TObjArray*        _particles;
  Int_t             _particleIDs[1000];
  Int_t             _nPart;
    
  Int_t             _number;
  static Int_t      _nVertices;
  static TObjArray* _vertices;

  ClassDef(StarVertex,1) //StarVertex  
};

#endif //STARVERTEX_H


