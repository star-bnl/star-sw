// $Id: StarParticle.h,v 1.3 2004/09/02 23:29:08 potekhin Exp $
// $Log: StarParticle.h,v $
// Revision 1.3  2004/09/02 23:29:08  potekhin
// Additions
//

#ifndef STARPARTICLE_H
#define STARPARTICLE_H

#include <TObject.h>
#include <TRef.h>
#include <TRefArray.h>
#include <TParticle.h>


class StarVertex;

class StarParticle : public TObject {

 public:
  StarParticle(Int_t id, TParticle* particle);
  StarParticle(Int_t id, TParticle* particle_, StarParticle* mother_, StarVertex* vertex_=0);
  StarParticle();
  virtual ~StarParticle();     

  // methods
  void SetMother(StarParticle* particle_);

  void SetMotherID(Int_t m_) {_motherID=m_;}

  void SetVertex(StarVertex*   vertex_);

  void AddDaughter(StarParticle* particle);
  void Print();
  void PrintDaughters() const;

  Bool_t        GetKeep(void)    {return _keep;}
  void          SetKeep(void)    {_keep=1;}
  void          SetNoKeep(void)  {_keep=0;}

  // get methods  
  Int_t         GetID(void)          const;
  TParticle*    GetParticle(void)    const;
  StarParticle* GetMother(void)      const;
  Int_t         GetMotherID(void)    const;
  Int_t         GetNofDaughters(void)const;
  StarParticle* GetDaughter(Int_t i) const;
  StarVertex*   GetVertex(void) {return _vertex;}

  TObjArray*    GetVertices(void) {return _vertices;}
  TObjArray*    InitVertices(void){_vertices=new TObjArray();return _vertices;}
    
 private:

  Int_t         _ID;
  TParticle*    _particle;
  TRef          _mother;
  Int_t         _motherID;
  StarVertex*   _vertex;
  TRefArray     _daughters;
  TObjArray*    _vertices;
  Bool_t        _keep;

    
  ClassDef(StarParticle,1) // Extended TParticle
};

#endif //STARPARTICLE_H   
   

