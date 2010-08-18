#ifndef SURFACE_H
#define SURFACE_H
#include <string.h>
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

//             **** surface ****
// surface is not meant to be derivative of volume.
//Thus, the surfaces should be contained in derivative of volume

const int pqqsurf=10;
const int pqcrossurf=4;
//#define pqcntrpt 10  // number of control points

class surface: public absref virt_common_base_pcomma       // abstract class
{public:
  macro_copy_total_zero(surface);
  //virtual surface* copy(void) const =0; 
  virtual int check_point_inside(const point& fpt, 
				 const vec& dir, vfloat fprec) const =0;
  // If two volumes are exactly adjusted, it may happens that the point
  // belongs to both volumes, to their borders. If dir != dv0,
  // the exitting volume is ignored.

  virtual int check_point_inside1(const point& fpt, int s_ext, vfloat fprec) 
    const = 0; 
  // s_ext=0 - entering
  //       1 - exitting

  virtual int range(const trajestep& fts, 
		    vfloat* crange, point* cpt, int* s_ext) const =0;
  //virtual int range(const point& fpt, const vec& dir, 
  //		    vfloat& rng, point& fpte) const =0;
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

  virtual int cross(const polyline& fpl, point *cntrpt, int& qcntrpt, 
		    vfloat prec) const =0;
  virtual void print(ostream& file, int l) const =0;
  virtual ~surface() {;}
};


//             **** splane ****

class splane: public surface
{public:
  plane pn;
  vec dir_ins;  // direction to inside, supposed to be unit length (What for?)
protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
  //virtual void Garef(int& fqaref , absref absref::**&faref, //fixed memory
  //	            int& fqareff, absref **&fareff) // free memory
  //  { 
  //    fqaref=2; fqareff=0; faref=&aref_splane[0]; fareff=NULL; }
  static absref(absref::*aref_splane[2]);
public:

  macro_copy_total(splane);
  //virtual splane* copy(void) const
  //  { 
      //mcerr<<"virtual surface* copy(void):\n";
      //print(mcerr);
      //surface* s=new splane(*this);
      //mcerr<<"virtual surface* copy(void):\n";
      //s->print(mcerr);
      //return s;
  //    return new splane(*this); 
  //  }
      //return new splane(*this); } 
  splane(void):pn(){;}
  splane(const splane &fsp):pn(fsp.pn),dir_ins(fsp.dir_ins){};
  splane(const plane &fpn, const vec& fdir_ins):pn(fpn),
    dir_ins(unit_vec(fdir_ins)){};
  int check_point_inside(const point& fpt, const vec& dir, vfloat fprec) 
    const; 
  int check_point_inside1(const point& fpt, int s_ext, vfloat fprec) 
    const; 
  // s_ext=0 - entering
  //       1 - exitting
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


  int range(const trajestep& fts, 
	    vfloat* crange, point* cpt, int* s_ext) const ;
  // Does not change fts
  // If no cross, returns 0 a
  // If there are crosses, returns number of them and 
  // assign crange and cpt

  int cross(const polyline& fpl, point *cntrpt, int& qcntrpt, vfloat prec) 
    const 
    {
      polyline *plh=new polyline[fpl.Gqsl()];
      int qplh;
      int i = pn.cross(fpl, cntrpt, qcntrpt, plh, qplh, prec);
      delete plh;
      return i;
    }
  virtual void print(ostream& file, int l) const ;
  virtual ~splane() {;}
};


//             **** ulsvolume ****

class ulsvolume: public absvol  // unlimited surfaces volume
// It is volume corstructed by unlimited surfaces.
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
// Nearest crossing point of track with a surface for exitting track.
// For each crossing point we know whether or not the track exits or enters
// to inside of this surface.
// This allows to reject that crossing points which are exiting for 
// track going from outside volume.
// It allows to make cilinders, tubes and many other complicated shapes. 
{public:
  int qsurf;
  ActivePtr<surface>surf[pqqsurf];
  //surface *surf[pqqsurf];
  String name;
protected:
  surface* adrsurf[pqqsurf];  // used only for get_components
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);

  //virtual void Garef(int& fqaref , absref absref::**&faref, //fixed memory
  //	            int& fqareff, absref **&fareff) // free memory
  // { 
  //    fqaref=0; faref=NULL;
  //    fqareff=qsurf;
  //    int n;
  //    for( n=0; n<qsurf; n++)
  //	adrsurf[n]=surf[n].get();
  //    fareff=(absref**)&adrsurf[0];
  //  }
public:
  int check_point_inside(const point& fpt, const vec& dir) const ;

  //int range_ext(const point& fpt, const vec& dir, int s_ext, 
  //		   vfloat& rng, point &fptenr) const ;
  int range_ext(trajestep& fts, int s_ext) const ;
  // If no cross, returns 0 and does not change fts
  // If there is cross, returns 1 and assign fts.mrange and fts.mpoint
  ulsvolume(void);
  ulsvolume(surface *fsurf[pqqsurf], int fqsurf, char* fname, vfloat fprec);
  ulsvolume(ulsvolume& f);
  ulsvolume(const ulsvolume& fv);
  virtual ~ulsvolume() {;}
  //ulsvolume& operator=(const ulsvolume& fv);
  //protected: 
public:
  void ulsvolume_init(surface *fsurf[pqqsurf], 
  		      int fqsurf, 
  		      const String& fname, vfloat fprec);
public:
  //virtual ~ulsvolume() 
  //  {//delete name;
  //  for( int n=0; n<qsurf; n++) delete surf[n];}
  macro_copy_header(ulsvolume);
  //virtual ulsvolume* copy(void) const ;
  virtual void income(gparticle* gp){;}
  virtual void chname(char *nm) const 
#ifdef USE_STLSTRING
    { strcpy(nm,"ulsvolume: "); strcat(nm,name.c_str()); }
#else
    { strcpy(nm,"ulsvolume: "); strcat(nm,name); }
#endif
  virtual void print(ostream& file, int l) const ;
  virtual int mandatory(void) const  {return 0;}
   
};

class manip_ulsvolume: virtual public manip_absvol, public ulsvolume
{public:
  virtual absvol* Gavol(void) const {return (ulsvolume*) this; }
  manip_ulsvolume(void): manip_absvol(), ulsvolume() {;}
  manip_ulsvolume(manip_ulsvolume& f);
  manip_ulsvolume(const manip_ulsvolume& f);
  manip_ulsvolume(const ulsvolume& f):
    manip_absvol(), ulsvolume(f) {;}
  macro_copy_header(manip_ulsvolume);
  //virtual ulsvolume* copy(void) const ;
  //virtual manip_ulsvolume* copy(void) const ;
  virtual ~manip_ulsvolume() {;}
  virtual void chname(char *nm) const 
#ifdef USE_STLSTRING
    { strcpy(nm,"manip_ulsvolume: "); strcat(nm,name.c_str()); }
#else
    { strcpy(nm,"manip_ulsvolume: "); strcat(nm,name); }
#endif
  virtual void print(ostream& file, int l) const ;

};
  

#endif
