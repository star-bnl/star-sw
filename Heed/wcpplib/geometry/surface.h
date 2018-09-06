#ifndef SURFACE_H
#define SURFACE_H
#include <iostream>
#include <array>
#include <memory>
#include "wcpplib/geometry/polyline.h"
#include "wcpplib/geometry/volume.h"
/*
Volume building from surfaces.

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

/// Surface base class.

class surface : public absref {
 public:
  virtual surface* copy() const = 0;
  virtual ~surface() {}
  virtual int check_point_inside(const point& fpt, const vec& dir,
                                 vfloat fprec) const = 0;
  // If two volumes are exactly adjusted, it may happens that the point
  // belongs to both volumes, to their borders. If dir != dv0,
  // the exiting volume is ignored.

  virtual int check_point_inside1(const point& fpt, int s_ext,
                                  vfloat fprec) const = 0;
  // s_ext=0 - entering
  //       1 - exiting

  virtual int range(const trajestep& fts, vfloat* crange, point* cpt,
                    int* s_ext) const = 0;
  // Does not change fts
  // If no cross or cross father than fts.mrange,
  // returns 0 and does not change fts
  // If there are crosses nearer than fts.mrange,
  // returns number of crosses and assign crange, cpt, and s_ext.
  // crange and cpt should be arranged.
  // s_ext: 0 - entry to inside
  //        1 - exit from inside
  //        2 - uncertain
  // The last crossing are then ignored.
  // The following case should be excluded:
  // The point is approximately on the surface.
  // dir is directed outside from the inside space.
  //
  // if surface is unlimited like a plane, and point is exactly on the plane,
  // the range is 0, s_ext is taken from direction.
  // In case of parallel to border, s_ext=2.

  virtual int cross(const polyline& fpl, point* cntrpt, int& qcntrpt,
                    vfloat prec) const = 0;
  virtual void print(std::ostream& file, int l) const = 0;
};

// **** splane ****

class splane : public surface {
 public:
  plane pn;
  vec dir_ins;  // direction to inside, supposed to be unit length (What for?)
 protected:
  absref_transmit get_components() override;
  static absref(absref::*aref_splane[2]);

 public:
  /// Default constructor
  splane() : pn() {}
  splane(const splane& fsp) : surface(fsp), pn(fsp.pn), dir_ins(fsp.dir_ins) {}
  splane(const plane& fpn, const vec& fdir_ins)
      : pn(fpn), dir_ins(unit_vec(fdir_ins)) {}
  /// Destructor
  virtual ~splane() {}

  int check_point_inside(const point& fpt, const vec& dir, vfloat fprec) const override;
  int check_point_inside1(const point& fpt, int s_ext, vfloat fprec) const override;
  // s_ext=0 - entering
  //       1 - exiting
  // 15.02.2006: Remark on check_point_inside vs. check_point_inside1.
  // check_point_inside1 allows one to override the behaviour when
  // the point is exactly on the surface.
  // If the moving point is entering one surface and simultaneously exiting
  // another one, it would not be recognized as entering the volume.
  // This virtually prohibits the creation of the control surfaces: volumes
  // with zero width, where the particle makes stop in order to be registered
  // in user check_point function.
  // Therefore when the function ulsvolume::range_ext checks that the
  // entry point is inside the volume, it calls check_point_inside1
  // with s_ext == 0, implying that the particle is entering
  // all the surfaces, thus faking the entering even if the particle is
  // actually exiting. This allows to make a stop there.

  int range(const trajestep& fts, vfloat* crange, point* cpt, int* s_ext) const override;
  // Does not change fts
  // If no cross, returns 0 a
  // If there are crosses, returns number of them and
  // assign crange and cpt

  int cross(const polyline& fpl, point* cntrpt, int& qcntrpt,
            vfloat prec) const override {
    polyline* plh = new polyline[fpl.Gqsl()];
    int qplh;
    int i = pn.cross(fpl, cntrpt, qcntrpt, plh, qplh, prec);
    delete[] plh;
    return i;
  }
  void print(std::ostream& file, int l) const override;
  splane* copy() const override { return new splane(*this); }
};

/// Unlimited surfaces volume.

class ulsvolume : public absvol {
  // The surface itself can be not convex.
  // But that part of surface which is border of the volume
  // must be from the right internal side from the other surfaces.
  // It can be formulated by the other way: neigbouring crossing surfaces
  // should cross only in the corners of the shape.
  // This allows to formulate algorithm of finding nearest cross of a track
  // with border of this type of volume:
  // For tracks coming (entering) from outside:
  // Nearest entering crossing point of track with a surface
  // which is from inside of the other surfaces.
  // For tracks exiting from inside:
  // Nearest crossing point of track with a surface for exiting track.
  // For each crossing point we know whether or not the track exits or
  // enters to inside of this surface.
  // This allows to reject that crossing points which are exiting for
  // track going from outside volume.
  // It allows to make cylinders, tubes and many other complicated shapes.

 public:
  static constexpr int pqqsurf = 10; 
  int qsurf = 0;
  std::array<std::shared_ptr<surface>, pqqsurf> surf;
  std::string name = "non-initialized ulsvolume";

 protected:
  // Array of raw pointers used used in get_components.
  surface* adrsurf[pqqsurf];

  absref_transmit get_components() override;

 public:
  /// Default constructor.
  ulsvolume() {};
  /// Constructor from surfaces.
  ulsvolume(const std::vector<std::shared_ptr<surface> >& fsurf, char* fname, vfloat fprec);
  ulsvolume(ulsvolume& f);
  ulsvolume(const ulsvolume& fv);
  /// Destructor
  virtual ~ulsvolume() {}

  int check_point_inside(const point& fpt, const vec& dir) const override;

  int range_ext(trajestep& fts, int s_ext) const override;
  // If no cross, returns 0 and does not change fts
  // If there is cross, returns 1 and assign fts.mrange and fts.mpoint

  void ulsvolume_init(const std::vector<std::shared_ptr<surface> >& fsurf,
                      const std::string& fname, vfloat fprec);

  void income(gparticle* /*gp*/) override {}
  void chname(char* nm) const override {
    strcpy(nm, "ulsvolume: ");
    strcat(nm, name.c_str());
  }
  void print(std::ostream& file, int l) const override;
  ulsvolume* copy() const override { return new ulsvolume(*this); }
};

class manip_ulsvolume : public manip_absvol, public ulsvolume {
 public:
  manip_ulsvolume() : manip_absvol(), ulsvolume() {}
  manip_ulsvolume(const manip_ulsvolume& f);
  manip_ulsvolume(const ulsvolume& f) : manip_absvol(), ulsvolume(f) {}
  /// Destructor
  virtual ~manip_ulsvolume() {}

  virtual absvol* Gavol() const { return (ulsvolume*)this; }
  virtual void chname(char* nm) const {
    strcpy(nm, "manip_ulsvolume: ");
    strcat(nm, name.c_str());
  }
  virtual void print(std::ostream& file, int l) const;
  virtual manip_ulsvolume* copy() const { return new manip_ulsvolume(*this); }
};
}

#endif
