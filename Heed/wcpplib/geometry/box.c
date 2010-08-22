//#include <stdlib.h>
//#include <iostream.h>
//#include <iomanip.h>
//#include <math.h>
#include "wcpplib/geometry/box.h"
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

// ********  box (3-dimansional rectangle (rectangular parallelogram)  *******

void box::get_components(ActivePtr<absref_transmit>& aref_tran)
{
  mfunnamep("box::get_components(...)");
  funnw.ehdr(mcerr);
  mcerr<<
  "one should not call this function, since this object can not be modified\n";
  spexit(mcerr);
}

//void box::Garef(int& fqaref , absref absref::**&faref, //fixed memory
//	            int& fqareff, absref **&fareff) // free memory
//{ 
//mfunnamep("box::Garef(...)");
//funnw.ehdr(cerr);
//mcerr<<
//"one should not call this function, sinec this object can not be modified\n";
//spexit(mcerr);
//}

box::box(void): dx(0), dy(0), dz(0), dxh(0), dyh(0), dzh(0), name("none")
{
  mfunname("box::box(void)");
  //mcerr<<"box::box(void) is being inited\n";
  init_prec(); init_planes();
}

box::box(vfloat fdx, vfloat fdy, vfloat fdz, const String& fname)
{ 
  pvecerror("box(vfloat fdx, vfloat fdy, vfloat fdz, const String &fname)");
  dx=abslt(fdx); dy=abslt(fdy); dz=abslt(fdz); 
  dxh=0.5*dx;  dyh=0.5*dy;  dzh=0.5*dz; 
  name=fname;
  //name=new char[strlen(fname)+1]; strcpy(name,fname);
  init_prec(); init_planes();
}

box::box(vfloat fdx, vfloat fdy, vfloat fdz, vfloat fprec, const String& fname)
{ 
  pvecerror("box(vfloat fdx, vfloat fdy, vfloat fdz, const String &fname)");
  dx=abslt(fdx); dy=abslt(fdy); dz=abslt(fdz); 
  dxh=0.5*dx;  dyh=0.5*dy;  dzh=0.5*dz; 
  name=fname;
  //name=new char[strlen(fname)+1]; strcpy(name,fname);
  prec=fprec; init_planes();
}

box::box(box& fb)
{ 
  pvecerror("box(const box& fb)");
  //mcout<<"box::box is called, name="<<fb.name<<"\n";
  dx=fb.dx; dy=fb.dy; dz=fb.dz; 
  dxh=0.5*dx;  dyh=0.5*dy;  dzh=0.5*dz; 
  prec = fb.prec;
  name=fb.name;
  //name=new char[strlen(fb.name)+1]; strcpy(name,fb.name);
  init_planes();
}
box::box(const box& fb)
{ 
  pvecerror("box(const box& fb)");
  //mcout<<"box::box is called, name="<<fb.name<<"\n";
  dx=fb.dx; dy=fb.dy; dz=fb.dz; 
  dxh=0.5*dx;  dyh=0.5*dy;  dzh=0.5*dz; 
  name=fb.name;
  prec = fb.prec;
  //name=new char[strlen(fb.name)+1]; strcpy(name,fb.name);
  init_planes();
}
/*
box& box::operator=(const box& fb)
{ 
  pvecerror("box& operator=(const box& fb)");
  if(this != &fb)
  {
    dx=fb.dx; dy=fb.dy; dz=fb.dz; 
    dxh=0.5*dx;  dyh=0.5*dy;  dzh=0.5*dz;  
    //delete name; name=new char[strlen(fb.name)+1]; strcpy(name,fb.name);
    name=fb.name;
    init_prec(); init_planes();
  }
 return *this;
}
*/
void box::init_prec(void)
{
  prec=(dxh + dyh + dzh)/3.0;
  prec=prec*vprecision;
}
void box::init_planes(void)
{
  mfunname("void box::init_planes(void)");
  //init_prec();
  splane spl[6];
  spl[0]=splane(plane(point( dxh,   0,   0), vec(-1,   0,   0)), 
		                             vec(-1,   0,   0));
  spl[1]=splane(plane(point(-dxh,   0,   0), vec( 1,   0,   0)), 
		                             vec( 1,   0,   0));
  spl[2]=splane(plane(point(   0, dyh,   0), vec(   0,-1,   0)), 
		                             vec(   0,-1,   0));
  spl[3]=splane(plane(point(   0,-dyh,   0), vec(   0, 1,   0)), 
		                             vec(   0, 1,   0));
  spl[4]=splane(plane(point(   0,   0, dzh), vec(   0,   0,-1)), 
		                             vec(   0,   0,-1));
  spl[5]=splane(plane(point(   0,   0,-dzh), vec(   0,   0, 1)), 
		                             vec(   0,   0, 1));
  surface *fsurf[6];
  int n;
  for( n=0; n<6; n++) fsurf[n] = &spl[n];
  //mcout<<"box::init_planes: prec="<<prec<<'\n';
  ulsv.ulsvolume_init(fsurf, 6, "ulsv of box", prec);
  //Iprintn(mcout,ulsv.prec);
}


int box::check_point_inside(const point& fpt, const vec& dir) const 
{ 
  mfunname(
  "virtual int check_point_inside(const point& fpt, const vec& dir)");
#ifdef TRACE_find_embed_vol
  mcout<<"box::check_point_inside: \n";
  print(mcout,1);
  mcout<<"fpt="<<fpt<<"dir="<<dir;
#endif
  if(dir==dv0)
  {  // this is not useful
    if(abslt(fpt.v.x)<=dxh && abslt(fpt.v.y)<=dyh && abslt(fpt.v.z)<=dzh )
      return 1;
    else
      return 0;
  }
  else
  {
    if(abslt(fpt.v.x)<=dxh-prec && 
       abslt(fpt.v.y)<=dyh-prec && 
       abslt(fpt.v.z)<=dzh-prec )
    {
#ifdef TRACE_find_embed_vol
      mcout<<"cond 1, returning 1\n";
#endif
      return 1;
    }
    if(abslt(fpt.v.x) > dxh+prec || 
       abslt(fpt.v.y) > dyh+prec || 
       abslt(fpt.v.z) > dzh+prec )
    {
#ifdef TRACE_find_embed_vol
      if(abslt(fpt.v.x) > dxh+prec) mcout<<"cond 2.1 satisfied\n";
      if(abslt(fpt.v.y) > dyh+prec) mcout<<"cond 2.2 satisfied\n";
      if(abslt(fpt.v.z) > dzh+prec) mcout<<"cond 2.3 satisfied\n";
      mcout<<"cond 2, returning 0\n";
#endif
      return 0;
    }
    // what remains is point belonging to border
#ifdef IMPROVED_BOUNDARY
    // Below we detect cases when particle is exiting, leaving the
    // case when it is entering
    if(abslt(fpt.v.x) > dxh-prec)
    {   
      if(dir.x == 0.0) return 0; 
      if(( fpt.v.x > 0 && dir.x > 0 ) ||
	 ( fpt.v.x < 0 && dir.x < 0 ))
      {
#ifdef TRACE_find_embed_vol
	mcout<<"cond 3, returning 0\n";
#endif
	return 0;  // exitting 
      }
    }
    if(abslt(fpt.v.y) > dyh-prec) 
    {
      if(dir.y == 0.0) return 0; 
      if(( fpt.v.y > 0 && dir.y > 0 ) ||
	 ( fpt.v.y < 0 && dir.y < 0 ))
      { 
#ifdef TRACE_find_embed_vol
	mcout<<"cond 4, returning 0\n";
#endif
	return 0;  // exitting
      } 
    }
    if(abslt(fpt.v.z) > dzh-prec)
    {
      if(dir.z == 0.0) return 0; 
      if(( fpt.v.z > 0 && dir.z > 0 ) ||
	 ( fpt.v.z < 0 && dir.z < 0 )) 
      {
#ifdef TRACE_find_embed_vol
	mcout<<"cond 5, returning 0\n";
#endif
	return 0;  // exitting
      }
    } 
#ifdef TRACE_find_embed_vol
    mcout<<"finish, returning 1\n";
#endif
    
    return 1;
      

#else  // for IMPROVED_BOUNDARY
    // In the old version, which is below,
    // if the track is parallel to a boundary, it was interpreted as
    // signature of being inside volume.
    // The general principal, saying that
    // if particle is located at the boundary and headed 
    // outside the volume,
    // it is considered in the volume to which it is headed,
    // cannot help to choose between two versions.
    // Of course, the box is convex and the particle flying along the border
    // enters the external space sooner or later and it can be regarded as
    // already outside. But the same can be said about the particle, 
    // which is completely indise the volume.
    // So this principle does not work here.
    // The other principle should be applied.
    // If we allow the absolutely thing volumes (control surfaces)
    // we should use the algorithm, which, in particular stops the particle
    // crossing exactly the corner of volume without entering its inside.
    // But this does not allow to choose. So now the old (this) variant 
    // is used, untill other arguments appear.

    if(abslt(fpt.v.x) > dxh-prec && 
       (( fpt.v.x > 0 && dir.x > 0 ) ||
        ( fpt.v.x < 0 && dir.x < 0 )))
    {
#ifdef TRACE_find_embed_vol
      mcout<<"cond 3, returning 0\n";
#endif
      return 0;  // exitting 
    }
    if(abslt(fpt.v.y) > dyh-prec && 
       (( fpt.v.y > 0 && dir.y > 0 ) ||
	( fpt.v.y < 0 && dir.y < 0 ))) 
    {
#ifdef TRACE_find_embed_vol
      mcout<<"cond 4, returning 0\n";
#endif
      return 0;  // exitting
    } 
    if(abslt(fpt.v.z) > dzh-prec && 
       (( fpt.v.z > 0 && dir.z > 0 ) ||
	( fpt.v.z < 0 && dir.z < 0 ))) 
    {
#ifdef TRACE_find_embed_vol
      mcout<<"cond 5, returning 0\n";
#endif
      return 0;  // exitting
    } 
#ifdef TRACE_find_embed_vol
      mcout<<"finish, returning 1\n";
#endif
    return 1;
#endif
  }
}

void box::print(ostream& file, int l) const 
{
  if(l <= 0) return;

  char s[1000];
  chname(s);
  Ifile<<"box::print(l="<<l<<"): "<<s<<'\n';
  indn.n+=2;
  Ifile<<" dx="<<dx<<" dy="<<dy<<" dz="<<dz<<" prec="<<prec<<'\n';
  Ifile<<" dxh="<<dxh<<" dyh="<<dyh<<" dzh="<<dzh<<'\n';
  if(l>=10)
  {
    l--;
    indn.n+=2;
    ulsv.print(file, l);
    indn.n-=2;
  }
  absvol::print(file,l);
  indn.n-=2;
}
/*
int box::range(const point& fpt, const vec& dir, int s_ext,  
	       int& sb, vfloat& rng,  point &fpte, 
	       manip_absvol_eid* faeid) const 
  //		      manip_absvol** famvol) const 
{
  pvecerror("int box::range(const point& fpt, const vec& dir, int s_ext,int& sb, vfloat& rng,  point &fpte, manip_absvol_eid* faeid)");
  faeid->amvol=NULL; faeid->nembed=-1; sb=1; 
  //return range_ext(fpt, dir, s_ext, rng, fpte);
  //mcout<<"int box::range: now calling range_ext\n";
  int s=range_ext(fpt, dir, s_ext, rng, fpte);
  //mcout<<"int box::range:\n";
  //mcout<<fpt<<dir<<fpte;
  //mcout<<"rng="<<rng<<'\n';
  return s;
}
*/

int box::range_ext(trajestep& fts, int s_ext) const 
{
  mfunname("virtual int box::range_ext(trajestep& fts, int s_ext) const");
  //mcout<<"box::range_ext: s_ext="<<s_ext<<" fts="<<fts;
  //print(mcout,1);
  if(s_ext==0)
  {
    if(abslt(fts.currpos.v.x) > dxh+fts.mrange) return 0;
    if(abslt(fts.currpos.v.y) > dyh+fts.mrange) return 0;
    if(abslt(fts.currpos.v.z) > dzh+fts.mrange) return 0;
  }
  else
  {
    if( abslt(fts.currpos.v.x) < dxh-fts.mrange &&
	abslt(fts.currpos.v.y) < dyh-fts.mrange &&
	abslt(fts.currpos.v.z) < dzh-fts.mrange )
    {
      //mcout<<"box::range_ext: fts="<<fts;
      //mcout<<"box::range_ext: returning 0 due to simple check\n";
      return 0;
    }
  }
  //mcout<<"box::range_ext: calling ulsv.range_ext\n";
  return ulsv.range_ext(fts, s_ext);
}
macro_copy_body(box)
//absvol* box::copy(void) const 
//{ return new box(*this); }
void box::income(gparticle* gp){;}
void box::chname(char *nm) const 
#ifdef USE_STLSTRING
{strcpy(nm,"box: "); strcat(nm,name.c_str()); }
#else
{strcpy(nm,"box: "); strcat(nm,name); }
#endif
//int box::mandatory(void) const {return 0;}  

//            *****   manip_box  ********         

absvol*  manip_box::Gavol(void) const {return (box*) this; }

//manip_box::manip_box(const manip_box& f): manip_absvol(f)
//{


macro_copy_body(manip_box)
//absvol* manip_box::copy(void) const 
//{
//  return new manip_box(*this); 
//}
void manip_box::chname(char *nm) const 
#ifdef USE_STLSTRING
{ strcpy(nm,"manip_box: "); strcat(nm,name.c_str()); }
#else
{ strcpy(nm,"manip_box: "); strcat(nm,name); }
#endif

void manip_box::print(ostream& file, int l) const 
{
  if(l>0)
  {
    char s[1000];
    chname(s);
    Ifile<<"manip_box::print(l="<<l<<"): "<<s<<'\n';
    l=l-1;
    if(l>0)
    {
      indn.n+=2;
      //manip_absvol::print(file, l-1); 
      // If to call this^ it calls manip_ulsvolume::print again and loop...
      
      box::print(file, l);
      indn.n-=2;
    }
    file.flush();
  }
}


//            *****   sh_manip_box  ********         

absvol*  sh_manip_box::Gavol(void) const {return (box*) this; }

//manip_box::manip_box(const manip_box& f): manip_absvol(f)
//{

void sh_manip_box::get_components
(ActivePtr<absref_transmit>& aref_tran)
{
  sh_manip_absvol::get_components(aref_tran);
}

macro_copy_body(sh_manip_box)
//absvol* sh_manip_box::copy(void) const 
//{
//  return new sh_manip_box(*this); 
//}
void sh_manip_box::chname(char *nm) const 
#ifdef USE_STLSTRING
{ strcpy(nm,"sh_manip_box: "); strcat(nm,name.c_str()); }
#else
{ strcpy(nm,"sh_manip_box: "); strcat(nm,name); }
#endif

void sh_manip_box::print(ostream& file, int l) const 
{
  if(l>0)
  {
    char s[1000];
    chname(s);
    Ifile<<"sh_manip_box::print(l="<<l<<"): "<<s<<'\n';
    l=l-1;
    if(l>0)
    {
      indn.n+=2;
      csys.print(file, l);
      //manip_absvol::print(file, l-1); 
      // If to call this^ it calls manip_ulsvolume::print again and loop...
      
      box::print(file, l);
      indn.n-=2;
    }
    file.flush();
  }
}




