// Copied and modified ComponentAnsys123.hh

#ifndef G_COMPONENT_ELMER_H
#define G_COMPONENT_ELMER_H

#include "ComponentFieldMap.hh"

namespace Garfield {

/// Component for importing field maps computed by Elmer.

class ComponentElmer : public ComponentFieldMap {

 public:
  /// Default constructor
  ComponentElmer();
  /// Constructor with a set of field map files, see Initialise().  
  ComponentElmer(const std::string& header, const std::string& elist, 
                 const std::string& nlist, const std::string& mplist, 
                 const std::string& volt, const std::string& unit);
  /// Destructor
  ~ComponentElmer() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status) override;
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status) override;

  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label) override;
  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label) override;

  Medium* GetMedium(const double x, const double y, const double z) override;

 /** Import a field map from a set of files.
   * \param header name of the header file 
                   (contains the number of elements and nodes).
   * \param elist name of the file that contains the list of mesh elements 
   * \param nlist name of the file that contains the list of mesh nodes
   * \param mplist name of the file that contains the material properties
   * \param volt output of the field solver (list of voltages)
   * \param unit length unit to be used
   */
  bool Initialise(const std::string& header = "mesh.header",
                  const std::string& elist = "mesh.elements",
                  const std::string& nlist = "mesh.nodes",
                  const std::string& mplist = "dielectrics.dat",
                  const std::string& volt = "out.result", 
                  const std::string& unit = "cm");
  /// Import a list of voltages to be used as weighting field.
  bool SetWeightingField(std::string prnsol, std::string label);

 protected:
  // Verify periodicities
  void UpdatePeriodicity() override { UpdatePeriodicityCommon(); }

  double GetElementVolume(const unsigned int i) override;
  void GetAspectRatio(const unsigned int i, 
                      double& dmin, double& dmax) override;
};
}
#endif
