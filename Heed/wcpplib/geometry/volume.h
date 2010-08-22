#ifndef VOLUME_H
#define VOLUME_H
#include "wcpplib/geometry/vec.h"
#include "wcpplib/geometry/trajestep.h"

//#define TRACE_find_embed_vol     // for debug
//#define TRACE_check_point_inside    // for debug

#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/safetl/AbsList.h"
#include "wcpplib/safetl/AbsPtr.h"

/*
The main definitions related to volume.

Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text 
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

//#define IMPROVED_BOUNDARY  // means ... TREATMENT
// The improvings are introduced (in box.c 14.02.2006, but then discarded.

enum type_obt { Eprivate, Ealien, Eshifted_private, Eshifted_alien };
class gparticle;
class manip_absvol;
class volume;
#define pqamvol 10
//define manip_obt(obt_name, cl_name) 

class manip_absvol;
class absvol;

/* Two little servise classes needed to determine the sequence of inclusions.
The first class contains just the address of the volume and its index
in embracing volume. This is like a node in this list
The second class contains the array of the first classes and determine
all the route.
*/

class manip_absvol_eid  // address and embracing index
{public:
  PassivePtr<manip_absvol> amvol;
  int nembed;  
           // index of this volume met as embraced in previous
           // (in array embed_mvol)
  void print(ostream& file, int l) const ;
  manip_absvol_eid(void);
  /*
  void  down(absref* f) const
    {
      if(amvol->type==Eshifted_private || amvol->type==Eshifted_alien)
	f->down(asc);
    }
  void    up(absref* f) const
    {
      if(amvol->type==Eshifted_private || amvol->type==Eshifted_alien)
	f->up(asc);
    }
  */
};
class manip_absvol_treeid  // tree of identifyers
{public:
  int qeid;  // number of currect volumes
  manip_absvol_eid eid[pqamvol];
  manip_absvol_treeid(void):qeid(0){;}
  const manip_absvol_eid* G_laeid() const  // get last address of manip_absvol_eid
    {
      if(qeid>0) return &eid[qeid-1]; 
      else return 0;
    }
  manip_absvol* G_lamvol() const; // get last address of manipulator

  absvol* G_lavol() const;  // get last address of volume
  //const absvol* G_lavol() const;  // get least address of volume
  friend int operator==(manip_absvol_treeid& tid1, manip_absvol_treeid& tid2);
  friend int operator!=(manip_absvol_treeid& tid1, manip_absvol_treeid& tid2);
  // The following two functions change reference frame of any object 
  // derivative from absref
  void  down_absref(absref* f); 
  void    up_absref(absref* f);

  // The following returns 1 if registered and 0 otherwise
  // Registered means that the route includes this manip_absvol,
  // not necessary the last volume.
  int check_manip_absvol_registered(manip_absvol* amvol);
  int check_absvol_registered(absvol* avol);
  void print(ostream& file, int l) const ;
}; 

int operator==(manip_absvol_treeid& tid1, manip_absvol_treeid& tid2);
inline int operator!=(manip_absvol_treeid& tid1, manip_absvol_treeid& tid2)
{ if(tid1 == tid2) return 0; else return 1; }

/*
Class abstract volume: the principal volume features
Actual chapes should be derivated.
The functions accept and return parameters expressed in the internal
coordinate system inherent to this volume.
For interface with external system please use manip_absvol.
*/
//           ********  absvol  *******
class absvol: virtual public absref, public RegPassivePtr
// public RegPassivePtr is not necessary for general package
// but may be useful in applications
{public:
  vfloat prec;
  virtual ~absvol() {;}
  virtual int check_point_inside(const point& fpt, 
				 const vec& dir) const  =0;
  // If two volumes are exactly adjusted, it may happens that the point
  // belongs to both volumes, to their borders. To avoid this confusion
  // the parameter dir is applyed. 
  // If dir == dv0, and point is exactly on the border,
  // generally behaviour is not specified.
  // If dir != dv0, and point is on the border with precision prec,
  // the exitting volume is ignored.

  virtual int check_point_inside(const abssyscoor& sc,
				 const point& fpt, 
				 const vec& fdir) const;

  virtual int find_embed_vol(const point& fpt, const vec& dir, 
  			     manip_absvol_treeid* atid) 
    const; 
  //virtual int find_embed_vol(const point& fpt, 
  //			     manip_absvol* amvol[pqamvol], 
  //			     int& namvol) const =0;
  // It starts from imbraced manipulators, if any
  // If point in this volume, it returns 1.
  // If there are embraced  volumes, it add some to namvol,
  // otherwise it does not add namvol==0.
  // The embraced volumes should not cross each other,
  // since this function can return only one chain.
  // But the borders can coincide with precision given to embraced volumes.
  // If the point is on the border, it is considered inside volume only if
  // dir is directed inside it.
  // Also algorithm of volume is effective if it interrupts
  // checking after first volume found.

  virtual int range(trajestep& fts, int s_ext,  
  		    int& sb, manip_absvol_eid* faeid) const ; 
  //virtual int range(const point& fpt, const vec& dir, int s_ext,  
  //		    int& sb, vfloat& rng,  point &fpte,
  //		    manip_absvol_eid* faeid) const =0; 
  //virtual int range(const point& fpt, const vec& dir, int s_ext,  
  //		    int& sb, vfloat& rng,  point &fpte,
  //		    manip_absvol** famvol) const =0; 
  // range considering this volume, all embracing volumes
  // sb=0 range restricted by precision reasons.
  // sb=1 crossing border of this volume
  // sb=2 crossing border of embraced volume
  // s_ext=1 inside, and to check all embraced volumes
  // s_ext=0 outside
  // checks only one level in deep. It is assumed that
  // from current volume the particle can pass either outside or
  // to one of embracing volumes.
  // In the last case *faeid is filled by its id.
  // Otherwise  *faeid is filled by 
  // faeid->amvol = NULL; and faeid->nembed = -1;

  // Find cross with current volume ignoring embraced ones
  virtual int range_ext(trajestep& fts, int s_ext) const =0;
  // s_ext=1 exit, now point is inside, but embraced volumes are ingnored.
  // s_ext=0 enter, now point is outside
  //virtual int range_ext(const point& fpt, const vec& dir, int s_ext, 
  //		   vfloat& rng,  point &fptenr) const =0;
  // range till exit from given volume or to entry only

  virtual int range_ext(const abssyscoor& sc,
			trajestep& fts, int s_ext) const;
  // consider that the volume is in system sc relatively current system 
  // in which fts is determined.

  macro_copy_header(absvol);
  //virtual void* copy(void) const;
  //virtual absvol* copy(void) const;
  virtual void income(gparticle* gp) {;}
  virtual void chname(char *nm) const {strcpy(nm,"absvol");}
  virtual void print(ostream& file, int l) const;
  //virtual int mandatory(void) const {return 0;}  
  // for control surfaces, thin volumes
  // now it is not treated.
  // set proper value of prec instead.
  virtual DynLinArr< manip_absvol * > Gamanip_embed(void) const ;
};

class absref_transmit_2fixsyscoor: public absref_transmit
{public:
  fixsyscoor* asys1;
  fixsyscoor* asys2;
  virtual absref* get_other(int n)
  {
    if(n == 0) return asys1;
    if(n == 1) return asys2;
    mcerr<<"absref_transmit_2fixsyscoor::get_other: should never happen\n";
    spexit(mcerr);
    return NULL; // to quiet compiler
  }
  absref_transmit_2fixsyscoor(void): absref_transmit() {;}
  absref_transmit_2fixsyscoor(fixsyscoor* fasys1,
			      fixsyscoor* fasys2):
    absref_transmit(), asys1(fasys1), asys2(fasys2) 
  { qaref_other = 2; }
  macro_copy_total(absref_transmit_2fixsyscoor);
  //virtual absref_transmit_2fixsyscoor* copy(void) const 
  //{ return new absref_transmit_2fixsyscoor(*this); }
  //virtual void print(ostream& file, int l) const ;
  virtual ~absref_transmit_2fixsyscoor() {;}
}; 

class absref_transmit_fixsyscoor: public absref_transmit
{public:
  DynLinArr< fixsyscoor >* asys;
  virtual absref* get_other(int n)
  {
    return &((*asys)[n]);
  }
  absref_transmit_fixsyscoor(void): absref_transmit() {;}
  absref_transmit_fixsyscoor(DynLinArr< fixsyscoor >* fasys):
    absref_transmit(), asys(fasys) { qaref_other = asys->get_qel(); }
  macro_copy_total(absref_transmit_fixsyscoor);
  //virtual absref_transmit_fixsyscoor* copy(void) const 
  //{ return new absref_transmit_fixsyscoor(*this); }
  virtual ~absref_transmit_fixsyscoor() {;}
};
 
class absref_transmit_absvol: public absref_transmit
{public:
  DynLinArr< ActivePtr<absvol> >* avol;
  virtual absref* get_other(int n)
  {
    return (*avol)[n].get();
  }
  absref_transmit_absvol(void): absref_transmit() {;}
  absref_transmit_absvol(DynLinArr< ActivePtr<absvol> >* favol):
    absref_transmit(), avol(favol) { qaref_other = avol->get_qel(); }
  macro_copy_total(absref_transmit_absvol);
  //virtual absref_transmit_absvol* copy(void) const 
  //{ return new absref_transmit_absvol(*this); }
  virtual ~absref_transmit_absvol() {;}
}; 


//          *********  manip_absvol  *********
class manip_absvol: virtual public absref, public RegPassivePtr 
{public:
  virtual absvol* Gavol(void) const = 0 ;

  virtual const abssyscoor* Gasc(void) const {return NULL;}   
          // returns NULL if it is the same system
  virtual int m_check_point_inside(const point& fpt, const vec& dir) const;

  virtual int m_find_embed_vol(const point& fpt, const vec& fdir, 
			     manip_absvol_treeid* atid) const ; 
  // Do the appropriate manipulations with atid and calls avol->find_embed_vol 

  //virtual int find_embed_vol(const point& fpt, 
  //			     manip_absvol* amvol[pqamvol], 
  //			     int& namvol) const ;

  // range considering this volume, all embracing volumes

  // Two the following functions changes syscoor if necessary and
  // calls similar named functions of absvol
  virtual int m_range(trajestep& fts, int s_ext,  
  		    int& sb, manip_absvol_eid* faeid) const ; 
  virtual int m_range_ext(trajestep& fts, int s_ext) const ;
  // s_ext=1 inside, but embraced volumes are ingnored.
  // s_ext=0 outside
  //protected:
  //absref *aref[2];
  //virtual void Garef(int& fqaref , absref absref::**&faref, //fixed memory
  //	            int& fqareff, absref **&fareff); // free memory
  virtual void m_chname(char *nm) const ;
  // The following two functions change reference frame of any object 
  // derivative from absref
  inline void  down_absref(absref* f) const
    {
      const abssyscoor* asc=Gasc();
      if(asc != NULL) f->down(asc);
    }
  inline void    up_absref(absref* f) const
    {
      const abssyscoor* asc=Gasc();
      if(asc != NULL) f->up(asc);
    }
  virtual void m_print(ostream& file, int l) const ;
  //virtual void* copy(void) const; // for ActivePtr, but this is 
  macro_copy_header(manip_absvol);
  //virtual manip_absvol* copy(void) const; // for ActivePtr, but this is 
  // an abstract class
  // so call of this function will terminate the program.
  // User should override it in derived class.
  virtual ~manip_absvol() {;}
};



//          *********  sh_manip_absvol  *********
class sh_manip_absvol: public manip_absvol 
{protected: 
  fixsyscoor csys; 
  //point *acnt;
  //basis* abas;
  //syscoor* asc;
public:
  virtual const abssyscoor* Gasc(void) const ;
  void Psc(const fixsyscoor& fcsys) {csys=fcsys;}

protected:
  //virtual void Garef(int& fqaref , absref absref::**&faref, //fixed memory
  //	            int& fqareff, absref **&fareff); // free memory
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
  //static absref(absref::*aref); initialization is not compiled in Solaris
  //static absref(sh_manip_absvol::*aref);
  absref* aref_ptr[1];
  //static absref *stored_aref[1];
public:
  sh_manip_absvol(void); 
  sh_manip_absvol(sh_manip_absvol& f);
  sh_manip_absvol(const sh_manip_absvol& f);
  sh_manip_absvol(const abssyscoor& f);
  sh_manip_absvol(const point& fc, const basis& fbas, const String& fname);
  virtual ~sh_manip_absvol() {;}


  virtual void m_chname(char *nm) const ; 
  // The following two functions change reference frame of any object 
  // derivative from absref
  virtual void m_print(ostream& file, int l) const ;

  macro_copy_header(sh_manip_absvol);
  //virtual void* copy(void) const; // for ActivePtr, but this is 
  //virtual sh_manip_absvol* copy(void) const; // for ActivePtr, but this is 
  // an abstract class
  // so call of this function will terminate the program.
  // User should override it in derived class.
};

class absref_transmit_2manip: public absref_transmit
{public:
  ActivePtr<manip_absvol>* amvol1;
  ActivePtr<manip_absvol>* amvol2;
  virtual absref* get_other(int n)
  {
    if(n == 0) return amvol1->get();
    if(n == 1) return amvol2->get();
    mcerr<<"absref_transmit_2manip::get_other: should never happen\n";
    spexit(mcerr);
  }
  absref_transmit_2manip(void): absref_transmit() {;}
  absref_transmit_2manip(ActivePtr<manip_absvol>* famvol1,
			 ActivePtr<manip_absvol>* famvol2):
    absref_transmit(), amvol1(famvol1), amvol2(famvol2) 
  { qaref_other = 2; }
  macro_copy_total(absref_transmit_2manip);
  //virtual absref_transmit_2manip* copy(void) const 
  //{ return new absref_transmit_2manip(*this); }
  virtual ~absref_transmit_2manip() {;}
}; 

class absref_transmit_manip: public absref_transmit
{public:
  DynLinArr< ActivePtr<manip_absvol> >* amvol;
  virtual absref* get_other(int n)
  {
    return (*amvol)[n].get();
  }
  absref_transmit_manip(void): absref_transmit() {;}
  absref_transmit_manip(DynLinArr< ActivePtr<manip_absvol> >* famvol):
    absref_transmit(), amvol(famvol) { qaref_other = amvol->get_qel(); }
  macro_copy_total(absref_transmit_manip);
  //virtual absref_transmit_manip* copy(void) const 
  //{ return new absref_transmit_manip(*this); }
  virtual ~absref_transmit_manip() {;}
}; 

//         *************************************


#include "wcpplib/geometry/gparticle.h"

#endif
