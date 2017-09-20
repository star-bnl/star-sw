#ifndef BOX_H
#define BOX_H
#include <iostream>
#include "wcpplib/geometry/straight.h"
#include "wcpplib/geometry/plane.h"
#include "wcpplib/geometry/volume.h"
#include "wcpplib/geometry/surface.h"
/*
Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

namespace Heed {

/// Box (three-dimensional rectangle/rectangular parallelogram).
/// Center of coordinate system is meant in the center of the box.
class box : public absvol {
 public:
  vfloat m_dx, m_dy, m_dz;     ///< Lengths of sides
  vfloat m_dxh, m_dyh, m_dzh;  ///< Half-lengths of sides
  ulsvolume m_ulsv;
  std::string m_name;

 public:
  /// Default constructor.
  box(void);
  // Constructor, compute precision from mean of dimensions.
  box(vfloat fdx, vfloat fdy, vfloat fdz, const std::string& fname);
  /// Constructor with user-provided precision.
  box(vfloat fdx, vfloat fdy, vfloat fdz, vfloat fprec,
      const std::string& fname);
  box(box& fb);
  box(const box& fb);
  macro_copy_header(box);
  /// Destructor
  virtual ~box() {}

  void init_prec(void);
  void init_planes(void);

  virtual int check_point_inside(const point& fpt, const vec& dir) const;

  /// Range till exit from given volume or to entry only.
  virtual int range_ext(trajestep& fts, int s_ext) const;
  virtual void income(gparticle* gp);
  virtual void chname(char* nm) const;
  virtual void print(std::ostream& file, int l) const;

 protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
};

// *****   manip_box  ********

class manip_box : public manip_absvol, public box {
 public:
  /// Constructor
  manip_box(void) : manip_absvol(), box() {}
  manip_box(const box& f) : manip_absvol(), box(f) {}
  macro_copy_header(manip_box);
  /// Destructor
  virtual ~manip_box() {}

  virtual absvol* Gavol(void) const;
  virtual void chname(char* nm) const;
  virtual void print(std::ostream& file, int l) const;
};

// *****   sh_manip_box  ********

class sh_manip_box : virtual public sh_manip_absvol, public box {
 public:
  /// Constructor
  sh_manip_box(void) : sh_manip_absvol(), box() {}
  sh_manip_box(const box& f) : sh_manip_absvol(), box(f) {}
  sh_manip_box(const abssyscoor& fcsys, const box& fbx)
      : sh_manip_absvol(fcsys), box(fbx) {}
  macro_copy_header(sh_manip_box);
  /// Destructor
  virtual ~sh_manip_box() { ; }

  virtual absvol* Gavol(void) const;
  virtual void chname(char* nm) const;
  virtual void print(std::ostream& file, int l) const;

 protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
};
}

#endif
