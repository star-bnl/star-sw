#ifndef __StKFParticleXPropagator__
#define __StKFParticleXPropagator__
#include "KFParticleXPropagator.h"
#include "KFParticle.h"
#include "TGeoMaterial.h"
#include <vector>
using namespace std;
// External to KFParticle propagator to account the simplified detector geometry, singleton
// External to KFParticle propagator to account the simplified detector geometry, singleton
class MyMat_t {
 public: 
  const Char_t* Material; 
  const Char_t *type;
  Int_t No; 
  Float_t Rmin,Rmax, A, Z, Density, RadLen, Zmin, Zmax;
  TGeoMaterial *mat;
}; 
class StKFParticleXPropagator : public KFParticleXPropagator {
 public: 
  StKFParticleXPropagator();
  virtual ~StKFParticleXPropagator() {}
  Bool_t  Propagete2Radius(const KFParticle &p, Float_t Radius);
  Float_t GetDStoSurface(const KFParticle &p, const double *surf, int nsurf, float dsdr[6] );
  static void Test(const Option_t *opt = "");
  Double_t GetDStoR(Float_t BZ, Double_t R, double stmin,double stmax);
 private:
  Double_t GetDStoSurfaceBz(Float_t B, double stmin,double stmax, const double *s, int nsurf, Int_t nearest); //, Float_t dsdr[6], KFParticle *particle = 0);
  static vector<MyMat_t> fgMaterials;
  static Int_t  SqEqu(double *, double *);
  ClassDef(StKFParticleXPropagator,1)
};
#endif /* __StKFParticleXPropagator__ */
