#ifndef HEEDDELTAELECTRONCS_H
#define HEEDDELTAELECTRONCS_H
#include "heed++/code/HeedMatterDef.h"
#include "heed++/code/ElElasticScat.h"
#include "heed++/code/PairProd.h"
#include "wcpplib/random/PointsRan.h"

/*
Cross sections and various parameters necessary for passing delta-electron.

2003, I. Smirnov
*/

#define USE_MEAN_COEF  // new variant, means that used mean(1-cos(theta))
// for low angle scattering. It seems to be more straightforwad and
// may be very slightly more precise than the old variant
// that is use of sqrt( mean ( square (1-cos(theta)) ) )

namespace Heed {

const long q_angular_mesh = 50;

class HeedDeltaElectronCS : public RegPassivePtr {
 public:

  PassivePtr<HeedMatterDef> hmd;
  PassivePtr<ElElasticScat> ees;
  PassivePtr<ElElasticScatLowSigma> eesls;
  PassivePtr<PairProd> pairprod;  // in eV
  DynLinArr<double> beta;
  DynLinArr<double> beta2;
  DynLinArr<double> momentum;   // MeV/c
  DynLinArr<double> momentum2;  // MeV^2/c

  DynLinArr<double> eLoss;  // MeV/cm, energy losses according to
  // very advanced formula with Bethe-Bloh and density effect as in GEANT3
  // corresponded to centers of intervals of the common mesh

  double mlambda;            // minimum mean lengt of range
                             // multiplied by density. sm*gr/sm**3 = gr/sm**2
  DynLinArr<double> lambda;  // path length (cm)
                             // for sruth == 2 mean free path
                             // index - energy of general mesh
  int sruth;                 // sign of use
                             // 0 - usial multiple scatering formula
                             // 1 - Rutherford cross-section
                             // 2 - ? precise theory?
  double mthetac;            // minimum threshold turn angle
                             //       For Rutherford:
                             // The interactions with less angle will not take
                             // into account. The actual threshold angle can be
                             // larger. The second restriction is going
                             // from restriction of atomic shell.
                             // The third one is from mlamBdel.
                             // 	For usial multiple scatering:
                             // Assuming that sigma = mTetacBdel
                             // the paht lengt is calculating.
  // If mlamBdel/density is less then the last is using.
  DynLinArr<double> rthetac;  // restiction due to atomic shell
  DynLinArr<double> thetac;   // threshold turn angle
  DynLinArr<double> CosThetac12, SinThetac12;
  DynLinArr<int> sisfera;  // sign that scattering is sferical
  DynLinArr<double> msig;  // something related to gauss scattering
                           // perhaps the width, but it is not clear now!
                           //DynLinArr< double > anhde;  // angular mesh
  DynLinArr<double> angular_mesh_c;  // angular mesh, centers
                                     //angles in degrees

  DynLinArr<DynLinArr<double> > smat;
  // cross section for material per one av. atom, in cm^2/rad
  //// cross section for material per one av. atom, in MeV**-2/rad
  // first index - energy mesh
  // second index - angular mesh

  //DynLinArr< double > mean_free_path;  // index - energy mesh
  //DynLinArr< DynLinArr< double > > ismat;
  DynLinArr<PointsRan> angular_points_ran;  // index - energy mesh
                                            //angles in degrees

  DynLinArr<PointsRan> low_angular_points_ran;  // index - energy mesh
  DynLinArr<double> low_lambda;  // path length for low angle scatterings
// this is without multiplication used for coef_low_sigma
// (cm)

// introduce new name mean_... in order to avoid errors at replacement
#ifdef USE_MEAN_COEF
  DynLinArr<double> mean_coef_low_sigma;
#else
  DynLinArr<double> coef_low_sigma;
// index - energy, but (attention!) for mesh defined in ElElasticScat
// this is sigma of cos(theta) - 1.0, supposing that center is 1.0
#endif
  // this is changed inside
  double get_sigma(double energy, double nscat) const;
  // copy of similar thing from ElElasticScatLowSigma

  HeedDeltaElectronCS(void);
  HeedDeltaElectronCS(HeedMatterDef* fhmd, ElElasticScat* fees,
                      ElElasticScatLowSigma* feesls, PairProd* fpairprod,
                      int fsruth = 2, double fmlambda = 0.001 * 4.0e-3,
                      double fmthetac = 0.1);

  virtual void print(std::ostream& file, int l) const;
};

class HeedDeltaElectronCSType {
 public:
  PassivePtr<HeedDeltaElectronCS> hdecs;
  HeedDeltaElectronCSType(void) : hdecs() { ; }
  HeedDeltaElectronCSType(HeedDeltaElectronCS* md) : hdecs(md) { ; }
};
std::ostream& operator<<(std::ostream& file, const HeedDeltaElectronCSType& f);

}

#endif
