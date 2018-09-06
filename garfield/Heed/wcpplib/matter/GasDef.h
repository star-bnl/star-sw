#ifndef GAS_DEF_H
#define GAS_DEF_H

#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/matter/MoleculeDef.h"

namespace Heed {

/// Definition of a gas.
/// haracteristic feature of the Gas class is that it consists of molecules.
/// Additional feature is that the density can be calculated by temperature
/// and pressure. But this is not always, and therefore it is not characteristic
/// feature. Then with only one this feature (consisting of molecules)
/// we can describe as gas also some other substances, for example, the liquids.
///
/// Only the basic information: the data of matter, plus the pressure.
/// Note that the class AtomMixDef undirectly appear twice.
/// It is the base class of matter and molecula. Therefore it is
/// indirectly the base class of GasDef, and the base class
/// of its external elements molech.
///
/// As the base class of GasDef, the class AtomMixDef determines only the
/// relative weights of atoms of different sorts. Also note that
/// the atoms of the same sorts participated in different molecules,
/// included in AtomMixDef as different atoms.
///
/// As the base class of MoleculeDef, the class AtomMixDef determines
/// also only the relative weights of atoms of different sorts in the given
/// molecule, since the class AtomMixDef doesn't have space to encapsulate
/// the number of atoms. But the latter number is also necessary: consider H2,
/// the relative weight of H is 1, and nothing says that there are two atoms.
/// Therefore in the class MoleculeDef there is additional array, which
/// gives the numbers of atoms of each sort, and also there is another
/// parameter giving the total number of atoms in molecule.
///
/// 1998-2004 I. Smirnov

class GasDef : public MatterDef {
  double pressureh = 0.;
  /// Number of different molecules
  long qmolech = 0;
  std::vector<MoleculeDef*> molech;
  std::vector<double> weight_quan_molech;  // sum is 1
  std::vector<double> weight_mass_molech;  // sum is 1
 public:
  double pressure() const { return pressureh; }
  long qmolec() const { return qmolech; }
  const std::vector<MoleculeDef*>& molec() const {
    return molech;
  }
  MoleculeDef* molec(long n) const { return molech[n]; }
  const std::vector<double>& weight_quan_molec() const {
    return weight_quan_molech;
  }
  const std::vector<double>& weight_mass_molec() const {
    return weight_mass_molech;
  }
  double weight_quan_molec(const long n) const {
    return weight_quan_molech[n];
  }
  double weight_mass_molec(const long n) const {
    return weight_mass_molech[n];
  }
  /// Mean charge of molecules in this gas
  double Z_mean_molec() const;

  GasDef();
  // for calculation of density assume ideal gas:
  GasDef(const std::string& fname, const std::string& fnotation, long fqmolec,
         const std::vector<std::string>& fmolec_not,
         const std::vector<double>& fweight_quan_molec, double fpressure,
         double ftemperature, double fdensity = -1.0);
  // for calculation of density assume Van der Waals
  // for the components for which the parameters are defined:
  GasDef(const std::string& fname, const std::string& fnotation, long fqmolec,
         const std::vector<std::string>& fmolec_not,
         const std::vector<double>& fweight_volume_molec, double fpressure,
         double ftemperature, int s1,
         int s2);  // s1 and s2 are to distinguish the constructor
                   // for calculation of density assume ideal gas:
  GasDef(const std::string& fname, const std::string& fnotation,
         const std::string& fmolec_not, double fpressure, double ftemperature,
         double fdensity = -1.0);
  // for calculation of density assume Van der Waals gas:
  GasDef(const std::string& fname, const std::string& fnotation,
         const std::string& fmolec_not, double fpressure, double ftemperature,
         int s1, int s2);
  // for calculation of density assume ideal gas:
  GasDef(const std::string& fname, const std::string& fnotation,
         const std::string& fmolec_not1, double fweight_quan_molec1,
         const std::string& fmolec_not2, double fweight_quan_molec2,
         double fpressure, double ftemperature, double fdensity = -1.0);
  // for calculation of density assume Van der Waals gas:
  GasDef(const std::string& fname, const std::string& fnotation,
         const std::string& fmolec_not1, double fweight_volume_molec1,
         const std::string& fmolec_not2, double fweight_volume_molec2,
         double fpressure, double ftemperature, int s1, int s2);
  // for calculation of density assume ideal gas:
  GasDef(const std::string& fname, const std::string& fnotation,
         const std::string& fmolec_not1, double fweight_quan_molec1,
         const std::string& fmolec_not2, double fweight_quan_molec2,
         const std::string& fmolec_not3, double fweight_quan_molec3,
         double fpressure, double ftemperature, double fdensity = -1.0);
  // for calculation of density assume Van der Waals gas:
  GasDef(const std::string& fname, const std::string& fnotation,
         const std::string& fmolec_not1, double fweight_volume_molec1,
         const std::string& fmolec_not2, double fweight_volume_molec2,
         const std::string& fmolec_not3, double fweight_volume_molec3,
         double fpressure, double ftemperature, int s1, int s2);
  // for calculation of density assume ideal gas:
  GasDef(const std::string& fname, const std::string& fnotation,
         const GasDef& gd, double fpressure, double ftemperature,
         double fdensity = -1.0);

  void print(std::ostream& file, int l = 0) const;
  GasDef* copy() const { return new GasDef(*this); }
};
std::ostream& operator<<(std::ostream& file, const GasDef& f);

}

#endif
