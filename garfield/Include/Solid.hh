// Abstract base class for solids

#ifndef G_SOLID_H
#define G_SOLID_H

namespace Garfield {

class Solid {

 public:
  // Constructor
  Solid() : m_debug(false) {}
  // Destructor
  virtual ~Solid() {}

  virtual bool IsInside(const double& x, const double& y, 
                        const double& z) const = 0;
  virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                              double& xmax, double& ymax, double& zmax) const = 0;
  // Solid type
  virtual bool IsBox() const { return false; }
  virtual bool IsTube() const { return false; }
  virtual bool IsSphere() const { return false; }

  virtual bool GetCenter(double& x, double& y, double& z) const = 0;
  virtual bool GetDimensions(double& l1, double& l2, double& l3) const = 0;
  virtual bool GetOrientation(double& ctheta, double& stheta, double& cphi,
                              double& shpi) const = 0;

  // Switch on/off debugging messages
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 protected:
  bool m_debug;
};
}

#endif
