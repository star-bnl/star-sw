//#include <stdlib.h>
//#include <iostream.h>
//#include <iomanip.h>
//#include <math.h>
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

//             **** surface ****
//             **** splane ****
absref absref::*(splane::aref_splane[2])=
{(absref absref::*)&splane::pn, (absref absref::*)&splane::dir_ins};
//absref absref::*(splane::aref_splane[3])=
//{&splane::piv, &splane::dir, &splane::dir_ins};

void splane::get_components(ActivePtr<absref_transmit>& aref_tran)
{
  aref_tran.pass(new absref_transmit(2, aref_splane));
}


int splane::check_point_inside(const point& fpt, const vec& dir, vfloat fprec)
  const 
{
  mfunname("int splane::check_point_inside(const point& fpt, const vec& dir, vfloat fprec)");
  //mcout<<"int splane::check_point_inside:\n";
  //print(mcout);
  if(dir==dv0)
  {  // this is not useful
    if(fpt==pn.Gpiv()) return 1;
    vec v=fpt-pn.Gpiv();
    if(cos2vec(dir_ins,v)>=-vprecision) return 1;
    else return 0;
  }
  else
  {
    if(pn.check_point_in(fpt, fprec)==1)
    {
      vfloat ca=cos2vec(dir, dir_ins);
      //mcout<<"dir="<<dir<<"dir_ins="<<dir_ins<<"ca="<<ca<<'\n';
      if(ca < 0)  // exitting
	return 0;
      else
	return 1;
    }
    else
    {
      vec v=fpt-pn.Gpiv();
      //mcout<<"cos2vec="<<cos2vec(dir_ins,v)<<'\n';
      if(cos2vec(dir_ins,v) >= 0) return 1;
      else return 0;
    }
  }
}

int splane::check_point_inside1(const point& fpt, int s_ext, vfloat fprec)
  const 
{
  //mcout<<"splane::check_point_inside1:\n";
  if(pn.check_point_in(fpt, fprec)==1)
  {
    //mcout<<"pn.check_point_in(fpt, fprec)=1\n";
    if( s_ext==1)
      return 0;
    else
      return 1;
  }
  else
  {
    //mcout<<"pn.check_point_in(fpt, fprec)!=1\n";
    vec v=fpt-pn.Gpiv();
    //Iprint(mcout, v);
    //mcout<<"cos2vec(dir_ins,v)="<<cos2vec(dir_ins,v)<<'\n';
    //if(cos2vec(dir_ins,v) > 0) 
    //{mcout<<"cos2vec(dir_ins,v) > 0)\n"; return 1;}
    //else {mcout<<"cos2vec(dir_ins,v) <= 0)\n"; return 0; }
    if(cos2vec(dir_ins,v) > 0) return 1;
    else return 0;
  }
}


int splane::range(const trajestep& fts, 
		  vfloat* crange, point* cpt, int* s_ext) const 
//int range(const point& fpt, const vec& dir, vfloat& rng, point& fpte) const 
{
  mfunname("int splane::range(...)");
  /*  it is not corrent
  if(pn.check_point_in(fts.currpos, 0.0)==1)  
  { 
    crange[0]=0.0; cpt[0]=fts.currpos;
    vfloat c=cos2vec(fts.dir, dir_ins);
    if(c > 0)
      s_ext[0]=0;
    else if(c < 0)
      s_ext[0]=1;
    else
      s_ext[0]=2;
    //rng=0; fpte=pn.Gpiv(); 
    return 1; 
  }
  */
  if(fts.s_range_cf==0) // straight line
  {
    point pt = pn.cross(straight(fts.currpos, fts.dir));
    //mcout<<"int splane::range(...), vecerror="<<vecerror<<'\n';
    if(vecerror!=0)
    {
      vecerror=0;
      return 0;
    }
    vfloat rng = length( pt - fts.currpos );
    //mcout<<"check_par( pt-fts.currpos , fts.dir, 0.01 )="
    // <<check_par( pt-fts.currpos , fts.dir, 0.01 )<<'\n';
    if(pt==fts.currpos || check_par( pt-fts.currpos , fts.dir, 0.01 )==1)
      //                                   looks like not matter ^
    { // otherwise the point is behind plane
      if(fts.mrange>=rng) // otherwise it can not reach the plane
      {
	cpt[0]=pt; crange[0]=rng; 
	//if(check_point_inside(fts.currpos, fts.dir, 0.0) == 1)
	vfloat t=cos2vec(fts.dir, dir_ins);
	if(t < 0)
	  s_ext[0]=1;
	else if( t > 0 )
	  s_ext[0]=0;
	else
	  s_ext[0]=2;
	return 1;  
      }
      else
      {
	//mcout<<"fts.mrange<rng\n";
	return 0;
      }
    }
    else
      return 0;
  }
  else
  {
    point pt[2];
    circumf cf(fts.currpos+fts.relcen, 
	       fts.dir||fts.relcen, // if to us, moving against clock
	       length(fts.relcen) );
    int q=cf.cross(pn, pt, 0.0);
    if(q==-1)  // total circle lyes in the plane
    {
      cpt[0]=fts.currpos;
      crange[0]=0.0;
      s_ext[0]=2;
      return 1;
    }
    if(q==0) return 0;
    if(q==1)
    {
      vec r1=-fts.relcen;
      vec r2=pt[0] - cf.Gpiv();
      vfloat angle=ang2projvec(r1, r2, cf.Gdir());
      vfloat rng = cf.Grad()*angle;
      if(fts.mrange>=rng)
      {
	cpt[0]=pt[0]; crange[0]=rng; //s_ext[0]=2;
	vfloat c=cos2vec(dir_ins, fts.relcen);
	if(angle==0.0)  // cross in the current point
	{
	  if(c>0) s_ext[0]=0;
	  else if(c<0) s_ext[0]=1;
	  else s_ext[0]=2;
	}
	else
	{
	  if(c>0) s_ext[0]=1;
	  else if(c<0) s_ext[0]=0;
	  else s_ext[0]=2;
	}
	return 1;  
      }
      else
	return 0;
    }
    if(q==2)
    {
      int qq=0;
      vec r=-fts.relcen;
      vec vcr[2];
      vcr[0]=pt[0] - cf.Gpiv();
      vcr[1]=pt[1] - cf.Gpiv();
      vfloat angle[2];
      angle[0]=ang2projvec(r, vcr[0], cf.Gdir());
      //mcout<<"r="<<r<<"vcr[0]="<<vcr[0]<<"cf.Gdir()="<<cf.Gdir();
      //mcout<<"angle[0]="<<angle[0]<<'\n';
      angle[1]=ang2projvec(r, vcr[1], cf.Gdir());
      //mcout<<"r="<<r<<"vcr[1]="<<vcr[1]<<"cf.Gdir()="<<cf.Gdir();
      //mcout<<"angle[1]="<<angle[1]<<'\n';
      if(angle[0] > angle[1])
      {                         // ordering
	vfloat a=angle[0]; angle[0]=angle[1]; angle[1]=a;
	point p=pt[0]; pt[0]=pt[1]; pt[1]=p;
      }
      vfloat rng;
      rng = cf.Grad()*angle[0];
      if(fts.mrange>=rng)
      {
	// find out what the first point means
	int ins=0;  // 1 if the point inside and exits
	vec td=fts.dir;
	td.turn(cf.Gdir(), angle[0]); // local dir in the crossing point
	vfloat t=cos2vec(td, dir_ins);
	if(t < 0)
	  ins=1;  // means the point was inside and now exiting
	else
	  ins=0;
	/*
	if(angle[0] > 0.0)
	  ins=check_point_inside(fts.currpos, fts.dir, 0.0);
	else
	{
	  vfloat t=cos2vec(fts.dir, dir_ins);
	  if(t < 0)
	    ins=1;  // means the point was inside and now exiting
	  else
	    ins=0;
	}
	*/
	cpt[0]=pt[0]; crange[0]=rng; s_ext[0]=ins; qq++;
	rng = cf.Grad()*angle[1];
	if(fts.mrange>=rng)
	{
	  cpt[1]=pt[1]; crange[1]=rng; s_ext[1]=( ins == 0 ? 1 : 0);   qq++;
	}
      }
      //for(int n=0; n<qq; n++)
      //{
      //	mcout<<"n="<<n<<" cpt="<<cpt[n]
      //	     <<"crange="<<crange[n]<<" s_ext="<<s_ext[n]<<'\n';
      //}
      return qq;
    }
  }
  return 0;  // to calc compiler of IRIX
}

void splane::print(ostream& file, int l) const 
{
  if(l>0)
  {
    Ifile<<"splane:\n";
    indn.n+=2;
    file<<pn;
    Ifile<<"dir_ins: "<<noindent<<dir_ins<<'\n';
    //Ifile<<dir_ins<<'\n';
    //file<<*((plane*)this);
    indn.n-=2;
  }
}



//             **** ulsvolume ****
void ulsvolume::get_components(ActivePtr<absref_transmit>& aref_tran)
{
  int n;
  for( n=0; n<qsurf; n++)
    adrsurf[n]=surf[n].get();
  aref_tran.pass(new absref_transmit(qsurf, (absref**)adrsurf));
}

int ulsvolume::check_point_inside(const point& fpt, const vec& dir) const 
{
  mfunname("ulsvolume::check_point_inside(...)");
  check_econd11(qsurf , <= 0 , mcerr);
  int n;
  //mcout<<"int check_point_inside:\n";
  //mcout<<"fpt="<<fpt<<"dir="<<dir<<"prec="<<prec<<'\n';
  for(n=0; n<qsurf; n++)
  {
    if(!(surf[n].get()-> check_point_inside(fpt, dir, prec)) )
    {
      //mcout<<"int check_point_inside: n="<<n<<" will return 0"<<'\n';
      return 0;
    }
  }
#ifdef TRACE_find_embed_vol
  indn.n++;
  Imcout<<"ulsvolume::check_point_inside: the point is in volume\n";
  Imcout<<"point:"<<fpt;
  print(mcout,0);
  indn.n--;
#endif
  //mcout<<"will return 1\n";
  return 1;
}

//#define DEBUG_ulsvolume_range_ext

int ulsvolume::range_ext(trajestep& fts, int s_ext) const 
{
  mfunnamep("int ulsvolume::range_ext(trajestep& fts, int s_ext) const");
  check_econd11(qsurf , <= 0 , mcerr);
  //trajestep ts(fts);
#ifdef DEBUG_ulsvolume_range_ext
  mcout<<"ulsvolume::range_ext, START, s_ext="<<s_ext<<" qsurf="<<qsurf<<'\n';
  mcout<<fts;
#endif
  vfloat crange[pqcrossurf];
  point cpt[pqcrossurf];
  int fs_ext[pqcrossurf];
  int n, m, nc;
  int s=0;  // sign of crossing
  //mcout<<"ulsvolume::range_ext: starting, s_ext="<<s_ext<<'\n';
  if(s_ext==1)
  {
    for(n=0; n<qsurf; n++)
    {
      int qc=surf[n].get()->range( fts, crange, cpt, fs_ext );
      //if(n==5)
      //{
      //	mcout<<"ulsvolume::range_ext:\n";
      //	surf[n]->print(mcout);
      //	mcout<<fts;
      //	mcout<<"qc="<<qc<<" fs_ext[0]="<<fs_ext[0]<<'\n';
      //}
      //surf[n]->print(mcout, 2);
      //Iprintn(mcout, qc);
      //mcout<<fts;
      for( m=0; m<qc; m++)
      {
	//Iprintn(mcout, fs_ext[m]);
	//Iprintn(mcout, cpt[m]);
	if(fs_ext[m]==1)
	{
	  s=1;
	  // The last minute change, it was 0 somewhy instead of m
	  fts.mrange=crange[m];  // reduce the range 
	  fts.mpoint=cpt[m];
	  break;  // take only the first exit point, it should be closest
	}
	else if(fs_ext[m]==0)
	{
	  if(!(surf[n].get()->check_point_inside(fts.currpos, fts.dir, prec)))
	  {
	    funnw.ehdr(mcerr);
	    mcerr<<"\nshould never happen\n"
		 <<"It may happen if you  call this function with s_ext==1\n"
		 <<"for point outside the volume\n";
	    spexit(mcerr);
	  }
	}
	else if(fs_ext[m]==2)
	  break;            // don't know what to do, safe to ignore
      }
    }
 
    if(s==1)
    {
      fts.s_prec=0;
    }
    return s;
  }
  else   // for if(s_ext==1)
  {
    int ss=0; // sign that there is cross with any of the surfaces
    for(n=0; n<qsurf; n++)
    {
#ifdef DEBUG_ulsvolume_range_ext
      Iprintn(mcout, n);
#endif
      int qc=surf[n].get()->range( fts, crange, cpt, fs_ext );
#ifdef DEBUG_ulsvolume_range_ext
      mcout<<"ulsvolume::range_ext: qc="<<qc<<"\n";
      surf[n]->print(mcout,1);
#endif
      for(nc=0; nc<qc; nc++)  // loop by crossing points
      {
#ifdef DEBUG_ulsvolume_range_ext
	mcout<<"nc="<<nc<<" fs_ext[nc]="<<fs_ext[nc]<<'\n';
#endif
	if(fs_ext[nc]==0)  // thus ignoring exitted surfaces
	{
	  s=1;
	  for(m=0; m<qsurf; m++) // scan other surfaces and verify that
	  {                      // the crossing point is inside
	    if(m!=n)
	    {
	      if(surf[m].get()->
		 check_point_inside1(cpt[nc], fs_ext[nc], prec) == 0)
	      { 
#ifdef DEBUG_ulsvolume_range_ext
		mcout<<"m="<<m<<'\n';
		mcout<<"Since the point is outside of the other surface, "
		     <<"it can not be border of volume\n";
#endif
		s=0; break;
	      }
	    }
	  }
#ifdef DEBUG_ulsvolume_range_ext
	  Iprintn(mcout, s);
#endif
	  if(s==1)
	  {    
#ifdef DEBUG_ulsvolume_range_ext
	    mcout<<"The crossing point is inside all other surfaces, \n"
		 <<"so it is good crossing point\n";
#endif
	    ss=1; fts.mrange=crange[nc]; fts.mpoint=cpt[nc]; 
	    break; // since points are ordered, go to next surface,
	    // may be there is nearer crossing point
	  }
	}
      }
    }
    if (ss==1) {
      fts.s_prec=0;
    }
#ifdef DEBUG_ulsvolume_range_ext
    mcout<<"ulsvolume::range_ext: at the end\n";
    print(mcout,1);
    mcout<<"ss="<<ss<<'\n';
#endif
    return ss;
  }
}
/*
// Old comment, may be not valid, or not at the right place:
// Straight track:
//Two variants of behavior:
//From outside:
//1. For each cross section from right side to check if the crossing point is
// from internal side from each other surfaces
//2. Find the most father point of cross section  for right side
// and to check if it is  from internal side for all other surfaces.

//From inside:
//1. For each cross section from right side to check if the crossing point is
// from internal side from each other surfaces
//2. Find the nearest point of cross section  for right side
//there is no need to check: cross point must exist. 

//I choose number 2. Reason: for outside number 1 the number of checking is
//proportional  number_of_surf**2
*/

ulsvolume::ulsvolume(void) : qsurf(0) {
  name = String("not inited ulsvolume");
}

void ulsvolume::ulsvolume_init(surface *fsurf[pqqsurf], 
			       int fqsurf, 
			       const String& fname, vfloat fprec) {
  prec = fprec;
  name = fname;
  if (qsurf > 0) {
    for (int n = 0; n < qsurf; ++n) surf[n].put(NULL);
  }
  qsurf = fqsurf;
  //mcerr<<"ulsvolume::ulsvolume_init:\n";
  for (int n = 0; n < qsurf; ++n) {
    //mcerr<<"n="<<n<<'\n';
    //fsurf[n]->print(mcerr) ;
    surf[n].put(fsurf[n]);
    //mcerr<<"n="<<n<<'\n';
    //surf[n]->print(mcerr) ;
  } 
}

ulsvolume::ulsvolume(surface *fsurf[pqqsurf], int fqsurf, char* fname,
		     vfloat fprec)
  : qsurf(fqsurf), name(fname) {
  mfunname("ulsvolume::ulsvolume(...)");
  check_econd12(fqsurf , > , pqqsurf , mcerr);
  prec = fprec;
  for (int n = 0; n < qsurf; ++n) surf[n].put(fsurf[n]);
}

ulsvolume::ulsvolume(ulsvolume& f): absref(f), absvol(f), qsurf(f.qsurf), name(f.name) {  
  mfunname("ulsvolume::ulsvolume(...)");
  check_econd12(f.qsurf , > , pqqsurf , mcerr);
  prec = f.prec;
  for (int n = 0; n < qsurf; ++n) surf[n].put(f.surf[n].get());
}
  
ulsvolume::ulsvolume(const ulsvolume& f): absref(f), absvol(f), qsurf(f.qsurf), name(f.name) {  
  mfunname("ulsvolume::ulsvolume(...)");
  check_econd12(f.qsurf , > , pqqsurf , mcerr);
  prec = f.prec;
  for (int n = 0; n < qsurf; ++n) surf[n].put(f.surf[n].get());
}

//ulsvolume& ulsvolume::operator=(const ulsvolume& fv) {
//  //delete name;
//  //for(int n=0; n<qsurf; n++) delete surf[n];
//  ulsvolume_init(((ulsvolume&)fv).surf, fv.qsurf, fv.name, fv.prec);
//  return *this;
//}

macro_copy_body(ulsvolume)

void ulsvolume::print(ostream& file, int l) const {
  char s[1000];
  chname(s);
  Ifile<<"ulsvolume::print(l="<<l<<"): "<<s<<'\n';
  if (l > 0) {
    indn.n+=2;
    Ifile<<"qsurf="<<qsurf<<" prec="<<prec<<'\n';
    for (int n = 0; n < qsurf; ++n) {
      Ifile<<" nsurf="<<n<<'\n';
      surf[n].get()->print(file,l);
    }
    absvol::print(file,l);
    indn.n-=2;
  }
}

manip_ulsvolume::manip_ulsvolume(manip_ulsvolume& f) : 
  absref(f), manip_absvol(f), ulsvolume((ulsvolume&)f) {
}

manip_ulsvolume::manip_ulsvolume(const manip_ulsvolume& f) :
  absref(f), manip_absvol(f), ulsvolume(f) {
}
macro_copy_body(manip_ulsvolume)
/*
ulsvolume* manip_ulsvolume::copy(void) const {
  return new manip_ulsvolume(*this); 
}
*/
void manip_ulsvolume::print(ostream& file, int l) const {
  if (l <= 0) return;
  char s[1000];
  chname(s);
  Ifile<<"manip_ulsvolume::print(l="<<l<<"): "<<s<<'\n';
  l=l-1;
  if (l > 0) {
    indn.n+=2;
    //manip_absvol::print(file, l-1); 
    // If to call this it calls manip_ulsvolume::print again and loop...
    ulsvolume::print(file, l-1);
    indn.n-=2;
  }
}
