#ifndef MATTER_DEF_H
#define MATTER_DEF_H

#include "wcpplib/matter/AtomDef.h"

namespace Heed {

/// Definition of matter (material or any media).
/// Only the basic information: the name, the notation,
/// the atomic mixture, temperature, density, effective ionization potential.
///
/// The principle of definitions of matters is the same as for atoms:
/// a dictionary or a database. See details there. But the logbook is different,
/// of course.
///
/// 1998-2004 I. Smirnov

class MatterDef : public AtomMixDef {
  std::string nameh;
  std::string notationh;
  double temperatureh;
  double densityh;
  double I_effh;
  // I_eff is still not very reliable.
  // There are too many approximations for this.
  // Here is a simplest and probably not good.
  void calc_I_eff();

 public:
  MatterDef();
  MatterDef(const std::string& fname, const std::string& fnotation, long fqatom,
            const std::vector<std::string>& fatom_not,
            const std::vector<double>& fweight_quan, double fdensity,
            double ftemperature);
  MatterDef(const std::string& fname, const std::string& fnotation,
            const std::string& fatom_not, double fdensity, double ftemperature);
  MatterDef(const std::string& fname, const std::string& fnotation,
            const std::string& fatom_not1, double fweight_quan1,
            const std::string& fatom_not2, double fweight_quan2,
            double fdensity, double ftemperature);
  MatterDef(const std::string& fname, const std::string& fnotation,
            const std::string& fatom_not1, double fweight_quan1,
            const std::string& fatom_not2, double fweight_quan2,
            const std::string& fatom_not3, double fweight_quan3,
            double fdensity, double ftemperature);
  virtual ~MatterDef();

  static void printall(std::ostream& file);
  const std::string& name() const { return nameh; }
  const std::string& notation() const { return notationh; }
  double density() const { return densityh; }
  double temperature() const { return temperatureh; }
  double I_eff() const { return I_effh; }
  /// Check that there is no matter with the same name in the container.
  void verify();
  static void verify(const std::string& fname, const std::string& fnotation);
  /// Initialize the logbook at the first request
  /// and keep it as internal static variable.
  static std::list<MatterDef*>& get_logbook();
  static const std::list<MatterDef*>& get_const_logbook();
  /// Return the adress of the matter with this notation if it is registered.
  /// Otherwise return NULL.
  static MatterDef* get_MatterDef(const std::string& fnotation);

  void print(std::ostream& file, int l) const;
  MatterDef* copy() const { return new MatterDef(*this); }
};
std::ostream& operator<<(std::ostream& file, const MatterDef& f);

}
#endif
