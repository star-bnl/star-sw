#ifndef BOX_H
#define BOX_H
#include <string.h>
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

// ********  box (3-dimensional rectangle (rectangular parallelogram)  *******
// center of coordinate system is meant in the center of the box
class box: public absvol
{public:
  vfloat dx, dy, dz;       // lengths of sides
  vfloat dxh, dyh, dzh;    // half-lengths of sides
  ulsvolume ulsv;
  String name;
  //virtual void Garef(int& fqaref , absref absref::**&faref, //fixed memory
  //	            int& fqareff, absref **&fareff); // free memory
protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
public:      
  box(void);

  box(vfloat fdx, vfloat fdy, vfloat fdz, const String& fname);
  // computes precision by init_prec as a ratio vprecision from mean of
  // dimensions

  box(vfloat fdx, vfloat fdy, vfloat fdz, vfloat fprec, const String& fname);
  // Use user-provided precision

  box(box& fb);
  box(const box& fb);

  virtual ~box()
    { //delete name; 
    }
  //box& operator=(const box& fb);

  void init_prec(void);
  void init_planes(void);

  virtual int check_point_inside(const point& fpt, const vec& dir) const ;

    //virtual int find_embed_vol(const point& fpt, const vec& dir,
    //	                     manip_absvol_treeid* atid) const 

//			     manip_absvol* amvol[pqamvol], int& namvol) const 
    //{return 0;}
  
  //It starts from imbraced manipulators, if any
  //If point in this volume, it returns 1.
  //If there are embraced  volumes, it add some to namvol,
  //otherwise it does not adds namvol==0.


  //virtual int range(trajestep& fts, int s_ext,  
  //		    int& sb, manip_absvol_eid* faeid) const ; 
  // range considering this volume, all embracing volumes
  // sb=1 external volume
  // sb=2 internal volume
  // s_ext=1 inside
  // s_ext=0 outside

  virtual int range_ext(trajestep& fts, int s_ext) const ;
  // range till exit from given volume or to entry only
public:
  macro_copy_header(box);
  //virtual absvol* copy(void) const ;
  virtual void income(gparticle* gp);
  virtual void chname(char *nm) const ;
  virtual void print(ostream& file, int l) const ;
  //virtual int mandatory(void) const ;  
                          // for control surfaces, thin volumes
};

//            *****   manip_box  ********         

class manip_box: public manip_absvol, public box
{public:
  virtual absvol* Gavol(void) const ;
  manip_box(void): manip_absvol(), box() {;}
  //manip_box(const manip_box& f);
  manip_box(const box& f):
    manip_absvol(), box(f) {;}
  macro_copy_header(manip_box);
  //absvol* copy(void) const ;
  virtual void chname(char *nm) const ;
  virtual void print(ostream& file, int l) const ;
  virtual ~manip_box(){;}  

};

//            *****   sh_manip_box  ********         

class sh_manip_box: virtual public sh_manip_absvol, public box
{public:
  virtual absvol* Gavol(void) const ;
protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
public:
  sh_manip_box(void): sh_manip_absvol(), box() {;}
  //manip_box(const manip_box& f);
  sh_manip_box(const box& f):
    sh_manip_absvol(), box(f) {;}
  sh_manip_box(const abssyscoor& fcsys, const box& fbx):
    sh_manip_absvol(fcsys), box(fbx) {;}
  macro_copy_header(sh_manip_box);
  //absvol* copy(void) const ;
  virtual void chname(char *nm) const ;
  virtual void print(ostream& file, int l) const ;
  virtual ~sh_manip_box(){;}  

};
  
#endif
