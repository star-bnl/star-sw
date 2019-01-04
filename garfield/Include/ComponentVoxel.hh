#ifndef G_COMPONENT_VOXEL_H
#define G_COMPONENT_VOXEL_H

#include "ComponentBase.hh"

namespace Garfield {

/// Component for interpolating field maps stored in a regular mesh.

class ComponentVoxel : public ComponentBase {

 public:
  /// Constructor
  ComponentVoxel();
  /// Destructor
  ~ComponentVoxel() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status) override;
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status) override;

  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label) override;
  double WeightingPotential(const double x, const double y,
                            const double z, const std::string& label) override;

  void MagneticField(const double x, const double y, const double z,
                     double& bx, double& by, double& bz, int& status) override;

  /// Interpolate between field values at the element centres.
  void EnableInterpolation(const bool on = true) { m_interpolate = on; }

  /// Offset coordinates in the weighting field, such that the
  /// same numerical weighting field map can be used for electrodes at
  /// different positions.
  void SetWeightingFieldOffset(const double x, const double y, const double z);

  Medium* GetMedium(const double x, const double y, const double z) override;

  bool GetVoltageRange(double& vmin, double& vmax) override;
  bool GetElectricFieldRange(double& exmin, double& exmax, double& eymin,
                             double& eymax, double& ezmin, double& ezmax);
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, double& xmax,
                      double& ymax, double& zmax) override;

  /** Define the grid.
    * \param nx,ny,nz number of bins along x, y, z.
    * \param xmin,xmax range along \f$x\f$.
    * \param ymin,ymax range along \f$y\f$.
    * \param zmin,zmax range along \f$z\f$.
    */
  void SetMesh(const unsigned int nx, const unsigned int ny,
               const unsigned int nz, const double xmin, const double xmax,
               const double ymin, const double ymax, const double zmin,
               const double zmax);
  /** Import electric field and potential values from a file.
    * The file is supposed to contain one line for each mesh point starting with
    *   - either two or three floating point numbers,
    *     specifying the coordinates (in cm) of the element centre or
    *   - two or three integers specifying the index of the element in the mesh,
    *
    * followed by
    *   - two or three floating point numbers for the electric field (in V/cm),
    * and (depending on the values of withPotential and withRegion),
    *   - a floating point number specifying the potential (in V), and
    *   - an integer specifying the "region" of the element.
    *
    * Format types are:
    *  - "xy", "xyz": elements are specified by the coordinates of their centres
    *  - "ij", "ijk": elements are specified by their indices
    */
  bool LoadElectricField(const std::string& filename, const std::string& format,
                         const bool withPotential, const bool withRegion,
                         const double scaleX = 1., const double scaleE = 1.,
                         const double scaleP = 1.);
  /// Import magnetic field values from a file. 
  bool LoadMagneticField(const std::string& filename, const std::string& format,
                         const double scaleX = 1., const double scaleB = 1.);

  /// Return the indices of the element at a given point.
  bool GetElement(const double xi, const double yi, const double zi,
                  unsigned int& i, unsigned int& j, unsigned int& k,
                  bool& xMirrored, bool& yMirrored, bool& zMirrored) const;
  /// Return the field for an element with given index.
  bool GetElement(const unsigned int i, const unsigned int j,
                  const unsigned int k, double& v, double& ex, double& ey,
                  double& ez) const;

  /// Set the medium in region i.
  void SetMedium(const unsigned int i, Medium* m);
  /// Get the medium in region i.
  Medium* GetMedium(const unsigned int i) const;
  /// Print all regions.
  void PrintRegions() const;

 private:
  std::vector<Medium*> m_media;
  struct Element {
    double fx, fy, fz;  //< Field
    double v;           //< Potential
  };
  /// Electric field values and potentials at each mesh element.
  std::vector<std::vector<std::vector<Element> > > m_efields;
  /// Magnetic field values at each mesh element.
  std::vector<std::vector<std::vector<Element> > > m_bfields;
  /// Region indices.
  std::vector<std::vector<std::vector<int> > > m_regions;
  // Dimensions of the mesh
  unsigned int m_nX = 0, m_nY = 0, m_nZ = 0;
  double m_xMin = 0., m_yMin = 0., m_zMin = 0.;
  double m_xMax = 0., m_yMax = 0., m_zMax = 0.;
  double m_dx = 0., m_dy = 0., m_dz = 0.;

  bool m_interpolate = false;

  bool m_hasMesh = false;
  bool m_hasPotential = false;
  bool m_hasEfield = false;
  bool m_hasBfield = false;

  // Offset for weighting field
  double m_wField_xOffset = 0.;
  double m_wField_yOffset = 0.;
  double m_wField_zOffset = 0.;

  // Voltage range
  double m_pMin = 0., m_pMax = 0.;

  /// Read data from file.
  bool LoadData(const std::string& filename, std::string format, 
                const bool withPotential, const bool withRegion,
                const double scaleX, const double scaleF, const double scaleP,
                const char field);

  void Reset() override;
  void UpdatePeriodicity() override;

  /// Look up/interpolate the field at a given point.
  bool GetField(const double x, const double y, const double z,
    const std::vector<std::vector<std::vector<Element> > >& field,
    double& fx, double& fy, double& fz, double& p, int& region);
  /// Reduce a coordinate to the basic cell (in case of periodicity).
  double Reduce(const double xin, const double xmin, const double xmax,
                const bool simplePeriodic, const bool mirrorPeriodic,
                bool& isMirrored) const; 
};
}
#endif
