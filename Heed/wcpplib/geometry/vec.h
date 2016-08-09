#ifndef VEC_H
#define VEC_H
/*
The base geometry file, determines class for geometrical conversions absref,
vectors (vec), basises, points, and systems of coordinates.
Points differ from vectors in the conversions.
Point is a vector denoting the position with respoct to the center of
coordinates. The vector is notation of direction. The vector is not
changed at parallel translations of basis or system of coordinates,
whereas the point is changed. Other explanations in my preprint
"SpaceMetricLib, geometrical class library for detector modeling in HEP"


Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/safetl/AbsPtr.h"
#include <string.h>
#include "wcpplib/util/String.h"
#define pvecerror(string)                                      \
  mfunname(string);                                            \
  if (vecerror != 0) {                                         \
    mcerr << "vecerror is not zero, program is terminated\n"   \
          << " function detected error is " << string << '\n'; \
    spexit(mcerr);                                             \
  }
#define pvecerrorp(string)                                     \
  mfunnamep(string);                                           \
  if (vecerror != 0) {                                         \
    mcerr << "vecerror is not zero, program is terminated\n"   \
          << " function detected error is " << string << '\n'; \
    spexit(mcerr);                                             \
  }
// pvecerror is put after first line of function.
// It makes up stack of functions names if FUNNAMESTACK is defined.
// To work correctly stackline(string); should not be in any additional {}

#include "wcpplib/geometry/vfloat.h"
/* Introduces type vfloat which is used throughout the geometrical calculations
instead of double. 'double' is meant to be replacable by 'float' for
speeding up, but no consistent research was made to check that it really
works in this way. So now vfloat is synonym of double.
*/

extern int vecerror;

class vec;
class basis;  //It is ortogonal basis
class abssyscoor;
class point;

class absref_transmit;

//             **** absref ****
/*
Abstract reference.
Basis class for any geometrical vector object.
Used for arranging of shift, turn and shange of coordinate system of
vector objects. Four functions down(), up(), turn(), and shift() do that
by calling of the same functions for any vector objects which are parts
of this class. Address of parts lets known by virtual function get_components()
which is reloaded in any derivative class.
Class vec represents 3-vectors and
reloads the functions down(), up(), turn() with proper functions
manipulating with 3-vectors.
Function shift() is also reloaded and does nothing for 3-vector vec,
since it is assumed that 3-vector is characteristic of direction only,
not point in space. We can not shift direction.
For this reason, class point representing point in space
reloads functions down and up again, and, of course, reloads shift.
To make proper shift at switch to coordinate system with shifted center
the point::down() and point::up() functions apply point::shift() function
after or before vec::down() and vec::up().
*/

// referencing not positioned abstract object
class absref {
 public:
  // destructor
  virtual ~absref() {}
  // convert numbering representation of object to basical system of fasc
  virtual void down(const abssyscoor* fasc);
  // convert numbering representation of objects to new system
  virtual void up(const abssyscoor* fasc);
  // turn around axis doing via center of coordinate system along dir.
  virtual void turn(const vec& dir, vfloat angle);
  virtual void shift(const vec& dir);

 private:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
};

// Contains three methods of transmission, the fastest, slower and the slowest
class absref_transmit virt_common_base_col {
 public:
  // For transmiting the members of the class, when
  // their relative addresses are avalable:
  int qaref;  // number of vector objects which are the members of the class
  absref(absref::** aref);  // reference to address of array
                            // containing their relative addresses
                            // as class members.
  // When the relative addresses are not available, in particular
  // when the component object is located in heap memory:
  int qaref_pointer;      // number of vector objects
  absref** aref_pointer;  // reference to address of array
                          // containing addresses of objects.

  // For any method of the object location the pointers can also be
  // transmitted through the function get_other(int n)
  // which receive the index of requested object and returns its address.
  // For this the user should determine the class derived
  // from absref_transmit. This is the most slow method of transmittion.
  int qaref_other;  // objects available though virtual function GetOther

  absref_transmit(void) : qaref(0), qaref_pointer(0), qaref_other(0) { ; }
  absref_transmit(int fqaref, absref absref::** faref)
      : qaref(fqaref), aref(faref), qaref_pointer(0), qaref_other(0) {
    ;
  }
  absref_transmit(int fqaref_pointer, absref** faref_pointer)
      : qaref(0),
        qaref_pointer(fqaref_pointer),
        aref_pointer(faref_pointer),
        qaref_other(0) {
    ;
  }
  absref_transmit(int fqaref, absref absref::** faref, int fqaref_pointer,
                  absref** faref_pointer)
      : qaref(fqaref),
        aref(faref),
        qaref_pointer(fqaref_pointer),
        aref_pointer(faref_pointer),
        qaref_other(0) {
    ;
  }
  absref_transmit(const absref_transmit& f) { *this = f; }
  macro_copy_header(absref_transmit);
  //virtual absref_transmit* copy(void) const ;
  virtual void print(std::ostream& file, int l) const;

  virtual absref* get_other(int n);
  // is meant to  be redefined in derived class to
  // obtain additional address
  // except those contained in aref and aref_pointer
  // This default version always returns NULL.
  virtual ~absref_transmit() { ; }
};

#define ApplyAnyFunctionToVecElements(func)                     \
  {                                                             \
    ActivePtr<absref_transmit> aref_tran_cont;                  \
    get_components(aref_tran_cont);                             \
    absref_transmit* aref_tran = aref_tran_cont.get();          \
    if(aref_tran != NULL) {                                     \
      int n;                                                    \
      int q = aref_tran->qaref;                                 \
      for(n = 0; n < q; n++)(this->*(aref_tran->aref[n])).func; \
      q = aref_tran->qaref_pointer;                             \
      for(n = 0; n < q; n++) aref_tran->aref_pointer[n]->func;  \
      q = aref_tran->qaref_other;                               \
      for(n = 0; n < q; n++) {                                  \
        absref* ar = aref_tran->get_other(n);                   \
        if(ar == NULL) break;                                   \
        else ar->func;                                          \
      }                                                         \
    }                                                           \
  }

//             **** vector ****

//Each vector is presented by 3 components and correspond to some basis.
//The components is a projection of the vector to unit basis vectors.
//If bas==NULL than it is the primary basis.
//So the concept of vector is more primary concept with comparison of
//basis, since one can not postulate basis while vectors do not exist.

class vec : public absref {
 public:
  vfloat x, y, z;

  friend vec operator*(const vec& v, vfloat p) {
    return vec(v.x * p, v.y * p, v.z * p);
  }
  friend vec operator*=(vec& v, vfloat p) {
    v = v * p;
    return v;
  }
  friend vec operator*(vfloat p, const vec& v) {
    return vec(v.x * p, v.y * p, v.z * p);
  }
  vec operator/(vfloat p) const { return vec(x / p, y / p, z / p); }
  friend vec operator/=(vec& v, vfloat p) {
    v = v / p;
    return v;
  }
  friend vec operator+(const vec& r1, const vec& r2) {
    return vec(r1.x + r2.x, r1.y + r2.y, r1.z + r2.z);
  }
  friend vec& operator+=(vec& r1, const vec& r2) {
    r1 = r1 + r2;
    return r1;
  }
  friend vec operator-(const vec& r1, const vec& r2) {
    return vec(r1.x - r2.x, r1.y - r2.y, r1.z - r2.z);
  }
  friend vec operator-=(vec& r1, const vec& r2) {
    r1 = r1 - r2;
    return r1;
  }
  friend vec operator-(const vec& r) { return vec(-r.x, -r.y, -r.z); }
  friend vfloat operator*(const vec& r1, const vec& r2) {
    return r1.x * r2.x + r1.y * r2.y + r1.z * r2.z;
  }
  // vector product
  friend vec operator||(const vec& r1, const vec& r2) {
    return vec(r1.y * r2.z - r1.z * r2.y, r1.z * r2.x - r1.x * r2.z,
               r1.x * r2.y - r1.y * r2.x);
  }
  // return 1 if precisely the same vectors and 0 otherwise
  friend inline int operator==(const vec& r1, const vec& r2);
  // return 0 if precisely the same vectors and 1 otherwise
  friend inline int operator!=(const vec& r1, const vec& r2);
  // return 1 if approximately the same vectors and 0 otherwise
  // Thus r2 may be is cube with side 2*prec with center marked by r1.
  friend inline int apeq(const vec& r1, const vec& r2, vfloat prec);
  // return 0 if approximately the same vectors and 1 otherwise
  friend inline int not_apeq(const vec& r1, const vec& r2, vfloat prec);
  friend vfloat length(const vec& v) {
    return sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
  }
  friend vfloat length2(const vec& v) {
    return (v.x * v.x + v.y * v.y + v.z * v.z);
  }
  friend inline vec unit_vec(const vec& v);
  // cosinus of angle between vectors
  // If one of vectors has zero length, it makes vecerror=1 and returns 0.
  friend vfloat cos2vec(const vec& r1, const vec& r2);
  //  angle between vectors, in interval [0, M_PI]
  // If one of vectors has zero length, it makes vecerror=1.
  friend vfloat ang2vec(const vec& r1, const vec& r2);
  friend vec project_to_plane(const vec& r, const vec& normal);
  // angle between projections of 2 vectors on plane normal to vector normal
  // in interval [0, 2*M_PI]
  // If one of vectors has zero length, it makes vecerror=1.
  friend vfloat ang2projvec(const vec& r1, const vec& r2, const vec& normal);
  // sinus of angle between vectors, 0 or positive.
  // If one of vectors has zero length, it makes vecerror=1.
  friend vfloat sin2vec(const vec& r1, const vec& r2);
  // check whether the vectors are parallel, or anti-parallel.
  // returns: 1 - parallel, -1  - antiparallel, 0 not parallel
  // 0 if one or both vectors have zero length
  // Thus, if angle between vectors < prec, they are parallel
  friend inline int check_par(const vec& r1, const vec& r2, vfloat prec);
  // check whether the vectors are perpendicular.
  // returns: 1 perpendicular, 0 not perpendicular.
  // also 0 if one or both vectors have zero length
  // Thus, if angle between vectors
  // a > 0.5*M_PI - find_max( prec, vprecision )
  // and a < 0.5*M_PI + find_max( prec, vprecision ), they are perpendicular
  friend inline int check_perp(const vec& r1, const vec& r2, vfloat prec);
  friend inline vec switch_xyz(const vec&);  //don't change the vector itself
                                                // constructors
  vec(vfloat xx, vfloat yy, vfloat zz) {
    x = xx;
    y = yy;
    z = zz;
  }
  vec() {
    x = 0;
    y = 0;
    z = 0;
  }
  //vec(      vec &pv): x(pv.x), y(pv.y), z(pv.z) {;}
  //vec(const vec &pv): x(pv.x), y(pv.y), z(pv.z) {;}
  //{        if(this!=NULL){x=pv.x; y=pv.y; z=pv.z;}
  //      else { mcerr<<"vec::vec:NULL pointer\n"; spexit(mcerr);}};
  // destructor
  virtual ~vec() {}
  vec down_new(const basis* fabas);
  void down(const basis* fabas);
  vec up_new(const basis* fabas_new);
  void up(const basis* fabas_new);
  vec down_new(const abssyscoor* fasc);
  void down(const abssyscoor* fasc);
  vec up_new(const abssyscoor* fasc);
  void up(const abssyscoor* fasc);

  // make new turned vector and leave this unchanged
  vec turn_new(const vec& dir, vfloat angle);
  // turn this vector
  void turn(const vec& dir, vfloat angle);
  void shift(const vec& dir);

  // The following things generate random unit vectors, currently with
  // the help of SRANLUX.

  // any direction in plane perpendicular to z-axis
  void random_round_vec(void);
  // any direction in conus
  // with symmetry axis along z-axis
  // and with angle theta (radian)
  void random_conic_vec(double theta);
  // any direction in 3-dim.space
  void random_sfer_vec(void);

};
std::ostream& operator<<(std::ostream& file, const vec& v);

extern vec dex;  // unit vector by x
extern vec dey;  // unit vector by y
extern vec dez;  // unit vector by z
extern vec dv0;  // zero vector

class vecReg : public vec, public RegPassivePtr {
 public:
  vecReg(vfloat xx, vfloat yy, vfloat zz) : vec(xx, yy, zz) {}
  vecReg(void) : vec() {}
};
std::ostream& operator<<(std::ostream& file, const vecReg& v);

#include "wcpplib/geometry/vec.ic"
//             **** basis ****

class basis : public absref virt_common_base_pcomma {
 protected:
  vec ex, ey, ez;  // unit vectors giving directions of cartesian axes.
                   // Supposed to be perpendicular, therefore not public.

  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);

  //virtual void Garef(int& fqaref , absref absref::**&faref, //fixed memory
  //                    int& fqareff, absref **&fareff); // free memory

  //static absref(absref::*aref[3]);
  static absref absref::* aref[3];

 public:
  //char name[12];  // Name for debug printing
  String name;

 public:
  vec Gex() const { return ex; }
  vec Gey() const { return ey; }
  vec Gez() const { return ez; }

  basis switch_xyz(void) const;  // change ex=ez; ey=ex; ez=ey
  basis(void);                   // nominal basis
  basis(const String& pname);    // nominal basis
  //basis(const basis *pabas, const char pname[12]);
  // nominal basis measured in pabas,
  // //used for creation of nbas in abssyscoor.
  basis(const vec& p, const String& fname);  //for longitudinal basis
  // basis(const vec &p, const char* pname);        //for longitudinal basis
  // z-axis is parralel to p
  // y-axis is vector product of z_new and z_old
  // x-axis is vector product of y_new and z_new
  // If p is parallel to z_old, the copy of old basis is created.
  // If p is anti-parallel to z_old, the inverted copy of old basis is created.
  // if(length(p)==0) vecerror=1;

  basis(const vec& p, const vec& c, const String& pname);
  // for more sophisticated basis:
  // ez is parallel to p,                             ez=unit_vec(p)
  // ey is perpendicular to plane which have p and c, ey=unit_vec(ez||c)
  // ex is vector product of y and z,                 ex=ey||ez
  // If p is parallel to c, or p is anti-parallel to c, vecerror=1
  // if(length(p)==0||length(c)==0)) vecerror=1;

  //basis down(void)
  //   { return basis(ex.down(), ey.down(), ez.down(), name); }
  //basis up(basis *pabas)
  //  { return basis(ex.up(pabas), ey.up(pabas), ez.up(pabas), name); }
  //basis(      basis& pb):ex(pb.ex),ey(pb.ey),ez(pb.ez), name(pb.name) {;}
  //basis(const basis& pb):ex(pb.ex),ey(pb.ey),ez(pb.ez), name(pb.name) {;}
  basis(const basis& pb, const String& pname);
  // the same basis with other name, useful for later turning

  //basis& operator=(basis& fb)
  basis(const vec& pex, const vec& pey, const vec& pez, const String& pname);
  // direct definitions of basis by three perpendicular unit-length vectors

  friend std::ostream& operator<<(std::ostream& file, const basis& b);
  AnyType_copy(basis, basis);
  virtual void print(std::ostream& file, int l) const;
  virtual ~basis(void) {}
};
extern std::ostream& operator<<(std::ostream& file, const basis& b);

class basisReg : public basis, public RegPassivePtr {
 public:
  basisReg(void) : basis() {}
  basisReg(const String& pname) : basis(pname) {}
  basisReg(const vec& p, const String& fname) : basis(p, fname) {}
  basisReg(const vec& p, const vec& c, const String& pname)
      : basis(p, c, pname) {}
  basisReg(const vec& pex, const vec& pey, const vec& pez, const String& pname)
      : basis(pex, pey, pez, pname) {}
  AnyType_copy(basisReg, basis);
};
std::ostream& operator<<(std::ostream& file, const basisReg& v);

//             **** point ****

class point : public absref virt_common_base_pcomma {
 public:
  vec v; 
 private:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
  static absref(absref::* aref);

 public:
  virtual void down(const abssyscoor* fasc);
  virtual void up(const abssyscoor* fasc);
  virtual void shift(const vec& dir) {
    // not defined for vectors, but defined for points
    v += dir;
  }  
  point(void) : v() {} // v is not initialised
  point(const vec& fv) : v(fv) {}
  point(const vfloat& fex, const vfloat& fey, const vfloat& fez)
      : v(fex, fey, fez) {}
  point& operator=(const point& fp) {
    v = fp.v;
    return *this;
  }
  vec operator-(const point& pp) const { return v - pp.v; }
  // creates vector from pp to p
  point operator+(const vec& fv) const { return point(v + fv); }
  friend int operator==(const point& p1, const point& p2) {
    return p1.v == p2.v ? 1 : 0;
  }
  friend int operator!=(const point& p1, const point& p2) {
    return p1.v != p2.v ? 1 : 0;
  }
  friend int apeq(const point& p1, const point& p2, vfloat prec) {
    return apeq(p1.v, p2.v, prec);
  }
  friend int not_apeq(const point& p1, const point& p2, vfloat prec) {
    return not_apeq(p1.v, p2.v, prec);
  }
  friend std::ostream& operator<<(std::ostream& file, const point& p);
  AnyType_copy(point, point);
  virtual void print(std::ostream& file, int l) const;
  virtual ~point() {}

};
std::ostream& operator<<(std::ostream& file, const point& p);

class pointReg : public point, public RegPassivePtr {
 public:
  pointReg(void) : point(), RegPassivePtr() {}
  pointReg(const vec& fv) : point(fv) {}
  pointReg(const vfloat& fex, const vfloat& fey, const vfloat& fez)
      : point(fex, fey, fez) {}
  AnyType_copy(pointReg, point);
  virtual ~pointReg() {}
};
std::ostream& operator<<(std::ostream& file, const pointReg& v);

//             **** system of coordinates ****

//System of coordinates is presented by its center, basis and
//the maternal system of coordinate.
//Take care: c.abas must be equal to abas->ex.abas.
//If asc==NULL and abs(c)==0 than it is primary system of coordinate
//and therefore
//c.abas and abas->ex.abas must be zero,
//baz may be zero or pointer to unit basis.

#define vec_syscoor_index 0
class abssyscoor {
 public:
  String name;
  virtual const point* Gapiv(void) const = 0;
  virtual const basis* Gabas(void) const = 0;
  abssyscoor(void) : name("none") {}
  abssyscoor(char* fname) : name(fname) {}
  abssyscoor(const String& fname) : name(fname) {}
  virtual void print(std::ostream& file, int l) const;

  virtual ~abssyscoor() {}
};
extern std::ostream& operator<<(std::ostream& file, const abssyscoor& s);

class fixsyscoor : public absref, public abssyscoor, public RegPassivePtr {
 public:
  virtual const point* Gapiv(void) const { return &piv; }
  virtual const basis* Gabas(void) const { return &bas; }
  void Ppiv(const point& fpiv);
  void Pbas(const basis& fbas);
  fixsyscoor(void) {} // nominal system
  fixsyscoor(char* fname) : abssyscoor(fname) {} // nominal system
  fixsyscoor(const String& fname) : abssyscoor(fname) {} // nominal system
  fixsyscoor(const point& fpiv, const basis& fbas, const String& fname)
      : abssyscoor(fname), piv(fpiv), bas(fbas) {}
  fixsyscoor(const point* const fapiv, const basis* const fabas,
             const String& fname)
      : abssyscoor(fname),
        piv((fapiv != NULL) ? (*fapiv) : point()),
        bas((fabas != NULL) ? (*fabas) : basis()) {}
  fixsyscoor(const abssyscoor& f)
      : abssyscoor(f),
        piv((f.Gapiv() != NULL) ? (*(f.Gapiv())) : point()),
        bas((f.Gabas() != NULL) ? (*(f.Gabas())) : basis()) {}
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(fixsyscoor);
  virtual ~fixsyscoor() {}

 protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
  static absref(absref::* aref[2]);

 private:
  point piv;
  basis bas;
};
extern std::ostream& operator<<(std::ostream& file, const fixsyscoor& s);

#endif
