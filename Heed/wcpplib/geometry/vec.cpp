#include <stdlib.h>
#include <iomanip>
#ifdef VISUAL_STUDIO
#define _USE_MATH_DEFINES
// see comment in math.h:
/* Define _USE_MATH_DEFINES before including math.h to expose these macro
 * definitions for common math constants.  These are placed under an #ifdef
 * since these commonly-defined names are not part of the C/C++ standards.
 */
#endif
#include <cmath>
#include "wcpplib/geometry/vec.h"
#include "wcpplib/random/ranluxint.h"
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
int vecerror = 0;

macro_copy_body(absref_transmit)
//absref_transmit* absref_transmit::copy(void) const
//{return new absref_transmit(*this);}

void absref_transmit::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "absref_transmit::print(l=" << l << ") qaref=" << qaref
        << " qaref_pointer=" << qaref_pointer
        << " qaref_other=" << qaref_other << "\n";
  file.flush();
}

absref* absref_transmit::get_other(int /*n*/) { return NULL; }

// **** absref ****

void absref::down(const abssyscoor* fasc) {
  if (fasc == NULL) return;  // considered to be unchanged
  ApplyAnyFunctionToVecElements(down(fasc));
}

void absref::up(const abssyscoor* fasc) {
  if (fasc == NULL) return;  // considered to be unchanged
  ApplyAnyFunctionToVecElements(up(fasc));
}

void absref::turn(const vec& dir, vfloat angle) {
  ApplyAnyFunctionToVecElements(turn(dir, angle));
}

void absref::shift(const vec& dir) {
  ApplyAnyFunctionToVecElements(shift(dir));
}

void absref::get_components(ActivePtr<absref_transmit>& aref_tran) {
  aref_tran.put(NULL);
}

/*
void absref::down(const abssyscoor* fasc) {
  if (fasc == NULL) return; // considered to be unchanged
  int qaref;
  absref absref::**aref;
  int qareff;
  absref **areff;
  Garef(qaref, aref, qareff, areff);
  for (int n = 0; n < qaref; ++n) {
    // changes only vectors
    // down is virtual and it is redefined there
    (this->*(aref[n])).down(fasc);
  }
  for (int n=0; n < qareff; ++n) {
    areff[n]->down(fasc);
  }
}

void absref::up(const abssyscoor* fasc) {
  if (fasc == NULL) return; // considered to be unchanged
  int qaref;
  absref absref::**aref;
  int qareff;
  absref **areff;
  Garef(qaref, aref, qareff, areff);
  for (int n = 0; n < qaref; ++n) {
    (this->*(aref[n])).up(fasc);
  }
  for (int n = 0; n < qareff; n++) {
    areff[n]->up(fasc);
  }

}

void absref::turn(const vec& dir, vfloat angle) {
  int qaref;
  absref absref::**aref;
  int qareff;
  absref **areff;
  Garef(qaref, aref, qareff, areff);
  for (int n = 0; n < qaref; ++n) {
    (this->*(aref[n])).turn(dir, angle);
  }
  for (int n = 0; n < qareff; ++n) {
    areff[n]->turn(dir, angle);
  }
}

void absref::shift(const vec& dir) {
  int qaref;
  absref absref::**aref;
  int qareff;
  absref **areff;
  Garef(qaref, aref, qareff, areff);
  for (int n = 0; n < qaref; ++n) {
    (this->*(aref[n])).shift(dir);
  }
  for (int n = 0; n < qareff; ++n) {
    areff[n]->shift(dir);
  }

}

void absref::Garef(int& qaref,  absref absref::**&aref , // fixed memory
                   int& qareff, absref **&areff) {       // free memory
  qaref = 0; qareff = 0; aref = NULL; areff = NULL;
}
*/

// **** vector ****
vfloat cos2vec(const vec& r1, const vec& r2) {
  // cosinus of angle between vectors
  // If one of vectors has zero length, it returns 2.
  pvecerror("vfloat cos2vec(const vec& r1, const vec& r2)");
  vfloat lr1 = length2(r1);
  vfloat lr2 = length2(r2);
  //mcout<<"cos2vec:\n";
  //Iprintn(mcout, lr1);
  //Iprintn(mcout, lr2);
  if (lr1 == 0 || lr2 == 0) {
    vecerror = 1;
    return 0;
  }
  vfloat cs = r1 * r2;
  int sign = 1;
  if (cs < 0) sign = -1;
  cs = cs * cs;
  cs = sign * sqrt(cs / (lr1 * lr2));
  //mcout<<"r1="<<r1<<"r2="<<r2<<"cos="<<cs<<'\n';
  return cs;
  //return r1*r2/(lr1*lr2);
}

vfloat ang2vec(const vec& r1, const vec& r2) {
  // angle between vectors
  // instead of return acos(cos2vec(r1,r2)); which produces NaN on linux at
  // parallel vectors
  vfloat cs = cos2vec(r1, r2);
  if (vecerror != 0) return 0;
  if (cs > 0.707106781187 || cs < -0.707106781187) {  // 1.0/sqrt(2)
    // pass to sin, it will be more exactly
    vfloat sn = sin2vec(r1, r2);
    if (vecerror != 0) return 0;
    if (cs > 0.0)
      return asin(sn);
    else
      return M_PI - asin(sn);
  }
  return acos(cs);
}

vfloat sin2vec(const vec& r1, const vec& r2) {
  // sinus of angle between vectors
  pvecerror("vfloat sin2vec(const vec& r1, const vec& r2)");
  vfloat lr1 = length2(r1);
  vfloat lr2 = length2(r2);
  if (lr1 == 0 || lr2 == 0) {
    vecerror = 1;
    return 0;
  }
  vfloat sn = length(r1 || r2);
  sn = sn * sn;
  sn = sqrt(sn / (lr1 * lr2));
  //mcout<<"r1="<<r1<<"r2="<<r2<<"sin="<<sn<<'\n';
  return sn;
  //return sin(ang2vec(r1,r2));
}

vec project_to_plane(const vec& r, const vec& normal) {
  pvecerror("vec project_to_plane(const vec& r, const vec& normal)");
  vec per(normal || r);
  if (per == dv0) {
    // either one of vectors is 0 or they are parallel
    return dv0;
  }
  vec ax = unit_vec(per || normal);
  vfloat v = ax * r;
  return v * ax;
}

vfloat ang2projvec(const vec& r1, const vec& r2, const vec& normal) {
  pvecerror(
      "vfloat ang2projvec(const vec& r1, const vec& r2, const vec& normal)");
  vec rt1 = project_to_plane(r1, normal);
  vec rt2 = project_to_plane(r2, normal);
  if (rt1 == dv0 || rt2 == dv0) {
    vecerror = 1;
    return 0;
  }
  vfloat tang = ang2vec(rt1, rt2);
  if (tang == 0) return tang;  // projections are parallel
  vec at = rt1 || rt2;
  int i = check_par(at, normal, 0.0001);
  //mcout<<"r1="<<r1<<"r2="<<r2<<"normal="<<normal
  //     <<"rt1="<<rt1<<"rt2="<<rt2<<"\ntang="<<tang
  //     <<"\nat="<<at<<" i="<<i<<'\n';
  if (i == -1) return 2.0 * M_PI - tang;
  return tang;  // it works if angle <= PI
}

vec vec::down_new(const basis* fabas) {
  //pvecerror("vec vec::down_new(void)");
  vec r;
  vec ex = fabas->Gex();
  vec ey = fabas->Gey();
  vec ez = fabas->Gez();
  r.x = x * ex.x + y * ey.x + z * ez.x;
  r.y = x * ex.y + y * ey.y + z * ez.y;
  r.z = x * ex.z + y * ey.z + z * ez.z;
  return r;
}

void vec::down(const basis* fabas) { *this = this->down_new(fabas); }

vec vec::up_new(const basis* fabas_new) {
  // it is assumed that fabas_new is derivative from old
  pvecerrorp("vec vec::up_new((const basis *pbas)");
  vec r;
  //check_econd11(fabas_new , ==NULL, mcerr);
  // not compiled in IRIX, reason is unkown
  if (fabas_new == NULL) {
    funnw.ehdr(mcerr);
    mcerr << "fabas_new==NULL\n";
    spexit(mcerr);
  }
  vec ex = fabas_new->Gex();
  vec ey = fabas_new->Gey();
  vec ez = fabas_new->Gez();
  r.x = x * ex.x + y * ex.y + z * ex.z;
  r.y = x * ey.x + y * ey.y + z * ey.z;
  r.z = x * ez.x + y * ez.y + z * ez.z;
  return r;
}

void vec::up(const basis* fabas_new) { *this = this->up_new(fabas_new); }

vec vec::turn_new(const vec& dir, vfloat angle) {
  pvecerror("vec turn(vec& dir, vfloat& angle)");
  if (length(*this) == 0) return vec(0, 0, 0);
  if (check_par(*this, dir, 0.0) != 0) {
    // parallel vectors are not changed
    return *this;
  }
  vfloat dirlen = length(dir);
  check_econd11a(dirlen, == 0, "cannot turn around zero vector", mcerr);
  vec u = dir / dirlen;  // unit vector
  vec constcomp = u * (*this) * u;
  //vec  perpcomp = (*this) - constcomp;
  //vfloat len=length(perpcomp);
  vec ort1 = unit_vec(u || (*this));
  vec ort2 = ort1 || u;
  vec perpcomp = ort2 * (*this) * ort2;
  vfloat len = length(perpcomp);
  //mcout<<" constcomp="<<constcomp<<" ort1="<<ort1<<" ort2="<<ort2;
  ort1 = sin(angle) * len * ort1;
  ort2 = cos(angle) * len * ort2;
  //mcout<<" constcomp="<<constcomp<<" ort1="<<ort1<<" ort2="<<ort2
  //    <<" len="<<len<<" sin(angle)="<<sin(angle)<<" cos(angle)="<<cos(angle)
  //    <<" angle="<<angle<<'\n';
  return constcomp + ort1 + ort2;

}

void vec::turn(const vec& dir, vfloat angle) {
  *this = this->turn_new(dir, angle);
}

void vec::shift(const vec& /*dir*/) {
  // Not defined for vectors
}

vec vec::down_new(const abssyscoor* fasc) { return down_new(fasc->Gabas()); }
void vec::down(const abssyscoor* fasc) { down(fasc->Gabas()); }
vec vec::up_new(const abssyscoor* fasc) { return up_new(fasc->Gabas()); }
void vec::up(const abssyscoor* fasc) { up(fasc->Gabas()); }

void vec::random_round_vec(void) {
  vfloat phi = M_PI * 2.0 * SRANLUX();
  x = sin(phi);
  y = cos(phi);
  z = 0;
}

void vec::random_conic_vec(double theta) {
  vfloat phi = M_PI * 2.0 * SRANLUX();
  double stheta = sin(theta);
  x = sin(phi) * stheta;
  y = cos(phi) * stheta;
  z = cos(theta);
}

void vec::random_sfer_vec() {
  vfloat cteta = 2.0 * SRANLUX() - 1.0;
  random_round_vec();
  vfloat steta = sqrt(1.0 - cteta * cteta);
  *this = (*this) * steta;
  z = cteta;
}

std::ostream& operator<<(std::ostream& file, const vec& v) {
  Ifile << "vector=" << std::setw(13) << v.x << std::setw(13) << v.y
        << std::setw(13) << v.z;
  file << '\n';
  file.flush();
  return file;
}

vec dex(1, 0, 0);
vec dey(0, 1, 0);
vec dez(0, 0, 1);
vec dv0(0, 0, 0);

std::ostream& operator<<(std::ostream& file, const vecReg& v) {
  Ifile << "vecReg=" << ((vec&)v);
  return file;
}

// **** basis ****
//absref absref::*(basis::aref[3])=
//{(absref absref::*)&basis::ex,
// (absref absref::*)&basis::ey,
// (absref absref::*)&basis::ez};
absref absref::* basis::aref[3] = {
  reinterpret_cast<absref absref::*>(static_cast<vec absref::*>(&basis::ex)),
  reinterpret_cast<absref absref::*>(static_cast<vec absref::*>(&basis::ey)),
  reinterpret_cast<absref absref::*>(static_cast<vec absref::*>(&basis::ez))
};

void basis::get_components(ActivePtr<absref_transmit>& aref_tran) {
  aref_tran.pass(new absref_transmit(3, aref));
}

//void basis::Garef(int& fqaref , absref absref::**&faref, // fixed memory
//	            int& fqareff, absref **&fareff) {      // free memory
//  fqaref=3; fqareff=0; faref=&aref[0]; fareff=NULL;
//}

basis basis::switch_xyz(void) const {
  pvecerror("basis basis::switch_xyz(void)");
  return basis(ez, ex, ey, name);
}

basis::basis(void) : ex(1, 0, 0), ey(0, 1, 0), ez(0, 0, 1) {
  name = "primary_bas";
}

basis::basis(const String& pname) : ex(1, 0, 0), ey(0, 1, 0), ez(0, 0, 1) {
  name = pname;
}

basis::basis(const vec& p, const String& pname) {
  pvecerror("basis::basis(vec &p)");
  name = pname;
  //strcpy(name,pname);
  vec dex(1, 0, 0);
  vec dey(0, 1, 0);
  vec dez(0, 0, 1);
  if (length(p) == 0) {
    vecerror = 1;
    ex = dex;
    ey = dey;
    ez = dez;
  }
  vfloat ca = cos2vec(p, dez);
  if (ca == 1) {
    ex = dex;
    ey = dey;
    ez = dez;
  } else if (ca == -1) {
    ex = -dex;
    ey = -dey;
    ez = -dez;
  } else {
    ez = unit_vec(p);
    ey = unit_vec(ez || dez);
    ex = ey || ez;
  }
}

basis::basis(const vec& p, const vec& c, const String& pname) {
  pvecerror("basis::basis(vec &p, vec &c, char pname[12])");
  name = pname;
  vec dex(1, 0, 0);
  vec dey(0, 1, 0);
  vec dez(0, 0, 1);

  if (length(p) == 0 || length(c) == 0) {
    vecerror = 1;
    ex = dex;
    ey = dey;
    ez = dez;
  }
  vfloat ca = cos2vec(p, c);
  if (ca == 1) {
    vecerror = 1;
    ex = dex;
    ey = dey;
    ez = dez;
  } else if (ca == -1) {
    vecerror = 1;
    ex = dex;
    ey = dey;
    ez = dez;
  } else {
    ez = unit_vec(p);
    ey = unit_vec(ez || c);
    ex = ey || ez;
  }

}

// the same basis with other name, useful for later turning
basis::basis(const basis& pb, const String& pname)
    : ex(pb.ex), ey(pb.ey), ez(pb.ez) {
  name = pname;
}

basis::basis(const vec& pex, const vec& pey, const vec& pez,
             const String& pname) {
  pvecerror("basis::basis(vec &pex, vec &pey, vec &pez, char pname[12])");
  if (!check_perp(pex, pey, vprecision) || !check_perp(pex, pez, vprecision) ||
      !check_perp(pey, pez, vprecision)) {
    mcerr << "ERROR in basis::basis(vec &pex, vec &pey, vec &pez) : \n"
          << "the vectors are not perpendicular\n";
    mcerr << " pex,pey,pez:\n";
    mcerr << pex << pey << pez;
    mcerr << "name=" << pname << '\n';
    spexit(mcerr);
  }
  //if(length(pex)!=vfloat(1.0) ||
  //   length(pey)!=vfloat(1.0) ||
  //   length(pez)!=vfloat(1.0) )
  if (not_apeq(length(pex), vfloat(1.0)) ||
      not_apeq(length(pey), vfloat(1.0)) ||
      not_apeq(length(pez), vfloat(1.0))) {
    mcerr << "ERROR in basis::basis(vec &pex, vec &pey, vec &pez) : \n"
          << "the vectors are not of unit length\n";
    mcerr << " pex,pey,pez:\n";
    mcerr << pex << pey << pez;
    mcerr << "name=" << pname << '\n';
    spexit(mcerr);
  }
  if (not_apeq(pex || pey, pez, vprecision)) {
    mcerr << "ERROR in basis::basis(vec &pex, vec &pey, vec &pez) : \n";
    mcerr << "wrong direction of pez\n";
    mcerr << " pex,pey,pez:\n";
    mcerr << pex << pey << pez;
    mcerr << "name=" << pname << '\n';
    spexit(mcerr);
  }
  name = pname;
  ex = pex;
  ey = pey;
  ez = pez;
}

void basis::print(std::ostream& file, int /*l*/) const { file << (*this); }

std::ostream& operator<<(std::ostream& file, const basis& b) {
  Ifile << "basis: name=" << b.name << '\n';
  indn.n += 2;
  int indnsave = indn.n;
  Ifile << "ex: ";
  indn.n = 0;
  file << b.ex;
  indn.n = indnsave;
  Ifile << "ey: ";
  indn.n = 0;
  file << b.ey;
  indn.n = indnsave;
  Ifile << "ez: ";
  indn.n = 0;
  file << b.ez;
  indn.n = indnsave;
  indn.n -= 2;
  //file << '\n';
  return file;
}

std::ostream& operator<<(std::ostream& file, const basisReg& b) {
  Ifile << "basisReg=" << ((basis&)b);
  return file;
}

// **** point ****

//absref absref::*(point::aref)=(absref absref::*)&point::v;
//absref absref::*(point::aref)=static_cast<absref absref::*>(&point::v);
absref absref::*(point::aref) =
    reinterpret_cast<absref absref::*>(static_cast<vec absref::*>(&point::v));

void point::get_components(ActivePtr<absref_transmit>& aref_tran) {
  aref_tran.pass(new absref_transmit(1, &aref));
}

void point::down(const abssyscoor* fasc) {
  v.down(fasc);
  shift(fasc->Gapiv()->v);
}
void point::up(const abssyscoor* fasc) {
  shift(-fasc->Gapiv()->v);
  v.up(fasc);
}

void point::print(std::ostream& file, int /*l*/) const { file << (*this); }

std::ostream& operator<<(std::ostream& file, const point& p) {
  Ifile << "point:\n";
  indn.n += 2;
  file << p.v;
  indn.n -= 2;
  return file;
}

std::ostream& operator<<(std::ostream& file, const pointReg& b) {
  Ifile << "pointReg=" << ((point&)b);
  return file;
}

// **** system of coordinates ****

void abssyscoor::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "abssyscoor::print(l=" << l << "): name=" << name << '\n';
    if (l > 1) {
      indn.n += 2;
      const point* apiv = Gapiv();
      if (apiv != NULL) {
        Ifile << "piv=" << noindent << (*apiv);
      } else {
        Ifile << "apiv=NULL\n";
      }
      const basis* abas = Gabas();
      if (abas != NULL) {
        Ifile << "bas=" << noindent << (*abas);
      } else {
        Ifile << "abas=NULL\n";
      }
      indn.n -= 2;
    }
    file.flush();
  }
}

std::ostream& operator<<(std::ostream& file, const abssyscoor& f) {
  f.print(file, 2);
  return file;
}

//void fixsyscoor::Papiv(const point* const fapiv)
//{ piv=(fapiv!=NULL) ? (*fapiv) : point() ; }
//void fixsyscoor::Pabas(const basis* const fabas)
//{ bas=(fabas!=NULL) ? (*fabas) : basis() ; }
//fixsyscoor::aref[2]=
//{(absref::*)&fixsyscoor::piv, (absref::*)&fixsyscoor::bas};
//absref absref::*(fixsyscoor::aref[2])=
//{(absref absref::*)&fixsyscoor::piv, (absref absref::*)&fixsyscoor::bas};
absref absref::*(fixsyscoor::aref[2]) = {
  reinterpret_cast<absref absref::*>(
      static_cast<point absref::*>(&fixsyscoor::piv)),
  reinterpret_cast<absref absref::*>(
      static_cast<basis absref::*>(&fixsyscoor::bas))
};

void fixsyscoor::get_components(ActivePtr<absref_transmit>& aref_tran) {
  aref_tran.pass(new absref_transmit(2, aref));
}

void fixsyscoor::Ppiv(const point& fpiv) { piv = fpiv; }
void fixsyscoor::Pbas(const basis& fbas) { bas = fbas; }

void fixsyscoor::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "fixsyscoor::print(l=" << l << ")\n";
    if (l > 1) {
      indn.n += 2;
      RegPassivePtr::print(file, l);
      abssyscoor::print(file, l);
    }
  }
}

std::ostream& operator<<(std::ostream& file, const fixsyscoor& f) {
  Ifile << "fixsyscoor:\n";
  f.RegPassivePtr::print(file, 2);
  f.abssyscoor::print(file, 2);
  return file;
}
