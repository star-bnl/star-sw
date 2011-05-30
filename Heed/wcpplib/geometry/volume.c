#include "wcpplib/geometry/volume.h"
//#include <stdlib.h>
//#include <iostream.h>
//#include <iomanip.h>
//#include <math.h>
//#include "geometry/volume.h"
#include "wcpplib/util/emul_new_stand.h"
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

manip_absvol_eid::manip_absvol_eid(void) 
{
  amvol.put(NULL);  nembed=-1; 
}

manip_absvol* manip_absvol_treeid::G_lamvol() const 
         // get last address of manipulator
{
  if(qeid>0) return eid[qeid-1].amvol.get(); 
  else return 0;
}

void  manip_absvol_treeid::down_absref(absref* f)
{ int n; for( n=qeid-1; n>=1; n--) eid[n].amvol->down_absref(f); }

void    manip_absvol_treeid::up_absref(absref* f)
{ int n; for( n=1; n<qeid; n++) eid[n].amvol->up_absref(f); }

int manip_absvol_treeid::check_manip_absvol_registered(manip_absvol* amvol)
{
  int n;
  for( n=0; n<qeid; n++)
    if(eid[n].amvol.get() == amvol) return 1;
  return 0;
}

int manip_absvol_treeid::check_absvol_registered(absvol* avol)
{
  int n;
  for( n=0; n<qeid; n++)
    if(eid[n].amvol->Gavol() == avol) return 1;
  return 0;
}

void manip_absvol_eid::print(ostream& file, int l) const 
{
  if(l>=0)
  {
    char s[1000];
    amvol->m_chname(s);
    Ifile<<s<<", nembed="<<nembed<<'\n';
    file.flush();
  }
} 

absvol* manip_absvol_treeid::G_lavol() const  // get last address of volume
{
  if(qeid>0) return eid[qeid-1].amvol->Gavol(); 
  else return 0;
}

int operator==(manip_absvol_treeid& tid1, manip_absvol_treeid& tid2)
  // checks total route
{
  // first to make fast rejection
  if(tid1.qeid != tid2.qeid) return 0;
  if(tid1.G_laeid()->nembed != tid2.G_laeid()->nembed) return 0;
  if(tid1.G_lamvol() != tid2.G_lamvol()) return 0;
  int n;  // more thoroughly
  for( n=0; n<tid1.qeid-1; n++)  // the last is checked
    if(tid1.eid[n].amvol.get() != tid2.eid[n].amvol.get())
      return 0;
  return 1;
}

void manip_absvol_treeid::print(ostream& file, int l) const 
{
  if(l>=0)
  {
    if(qeid<=0)
      Ifile<<"no volume defined, qeid="<<qeid<<'\n';
    else
    {
      if( l>0 )
      {
	int n;
	for( n=0; n<qeid-1; n++)
	{
	  Ifile<<"n="<<n;
	  eid[n].print(file,0);
	}
      }
      Ifile<<"n="<<qeid-1;
      eid[qeid-1].print(file,0);
    }
    file.flush();
  }
}
//           ********  absvol   *******
// some functions converted from absvst to absvol

DynLinArr< manip_absvol * > absvol::Gamanip_embed(void) const 
{
  //mcout<<"absvst::Gamanip_embed(void)\n";
  return DynLinArr< manip_absvol * >();
}

int absvol::check_point_inside(const abssyscoor& sc,
			       const point& fpt, 
			       const vec& fdir) const
{
  mfunname("absvol::check_point_inside(...)");
#ifdef TRACE_check_point_inside
  mcout<<"absvol::check_point_inside(const abssyscoor& sc,...):\n";
  Iprintn(mcout, sc);
  Iprintn(mcout, fpt);
  Iprintn(mcout, fdir);
#endif
  point pt=fpt;
  vec dir=fdir;
  pt.up(&sc);
  dir.up(&sc);
#ifdef TRACE_check_point_inside
  int s_ret = check_point_inside(pt, dir);
  Iprintn(mcout, s_ret);
  return s_ret;
#else
  return check_point_inside(pt, dir);
#endif
}

int absvol::range_ext(const abssyscoor& sc,
		      trajestep& fts, int s_ext) const
{
  mfunname("absvol::range_ext(...)");
  trajestep ts(fts);
  ts.up(&sc);
  int s=range_ext(ts, s_ext);
  if(s==1)
  {
    ts.down(&sc);
    fts = ts;
  }
  return s;
}



int absvol::find_embed_vol(const point& fpt, const vec& dir, 
			   manip_absvol_treeid* atid) const  
{
  int n;
#ifdef TRACE_find_embed_vol
  indn.n++;
  Imcout<<"absvol::find_embed_vol: entering, atid->qeid="<<atid->qeid<<'\n';
  Imcout<<"absvol::find_embed_vol: embracing volumes:\n";
  for( n=0; n<atid->qeid; n++)
    atid->eid[n].amvol->m_print(mcout,0);
  Imcout<<"absvol::find_embed_vol: this volume:\n";
  print(mcout,0);
  Imcout<<"absvol::find_embed_vol: point:\n";
  Imcout<<fpt;
  Imcout<<"absvol::find_embed_vol: calling check_point_inside\n";
#endif

  int iret = check_point_inside(fpt, dir);

#ifdef TRACE_find_embed_vol
  Imcout<<"absvol::find_embed_vol: after checking:\n";
  //print(mcout,0);
  Imcout<<"absvol::find_embed_vol: return code "<<iret<<'\n';
#endif
  
  if(iret == 0)
  {
#ifdef TRACE_find_embed_vol
    Imcout<<"absvol::find_embed_vol: point is out of volume, exit with 0\n";
    //print(mcout,0);
    indn.n--;
#endif
    return 0;
  }
  //int namvol=0;
  int s=atid->qeid;
  DynLinArr< manip_absvol * > aman=Gamanip_embed();
  if( aman.get_qel() > 0 && atid->qeid >= pqamvol)
  {
    mcerr<<"error in absvol::find_embed_vol: "
	 <<" aman->get_qel() > 0 && atid->qeid == pqamvol\n"
	 <<" atid->qeid="<<atid->qeid<<'\n';
    mcerr<<"increase pqamvol\n";
    exit(1);
  }
  
  for( n=0; n<aman.get_qel(); n++)
  {
#ifdef TRACE_find_embed_vol
    Imcout<<"absvol::find_embed_vol: n="<<n<<" atid->qeid="<<atid->qeid<<'\n';
    Imcout<<"absvol::find_embed_vol:calling of embed_mvol[n].find_embed_vol\n";
     //if(embed_mvol[n].find_embed_vol(fpt, &famvol[fnamvol], namvol));
#endif
    atid->eid[atid->qeid].nembed = n; // for next 
    int i = aman[n]->m_find_embed_vol(fpt, dir, atid);

#ifdef TRACE_find_embed_vol
    Imcout<<"absvol::find_embed_vol: return code is "<<i<<'\n';
#endif
    if(i == 1)
    {
      if(s < atid->qeid)
      {
	//Imcout<<"volume::find_embed_vol: point is in embed, atid->qeid="
	//     <<atid->qeid<<'\n';
	//s=1;
	break;
      }
      else
	Imcout<<"worning in volume::find_embed_vol: contradiction between "
	     <<" i==1 and s == fnamvol\n";
    }
  } //// it is another variant of the same algorithm as in manip_absvol
  //if(s==1)
  //{
  //  fnamvol+=namvol;
  //}
#ifdef TRACE_find_embed_vol
  Imcout<<"absvol::find_embed_vol: exiting, return code is 1\n";
  indn.n--;
#endif
  return 1;
}

int absvol::range(trajestep& fts, int s_ext,  
		  int& sb, manip_absvol_eid* faeid) const  
{
  //vfloat rngh = max_vfloat;
  //point fpteh;
  faeid->amvol.put(NULL);
  faeid->nembed = -1;
  if(s_ext==0) // enter from outside
  {
    sb=1;
    return range_ext(fts, 0);
  }
  int s=range_ext(fts, 1);
  //int s=range_ext(fts, 1, rngh, fpteh);
  //mcout<<"absvol::range: s="<<s<<" fts="<<fts;
  if(s==1)
  {
    sb=1; //rng=rngh; fpte=fpteh;
  }
  else
  {
    sb=0;
  }
  // distance to the border can be more than maximal step
  //else
  //{
  //  mcerr<<"ERROR in volume::range: each volume should be limited\n";
  //  exit(1);
  //}
  int n;
  DynLinArr< manip_absvol * > aman=Gamanip_embed();
  for(n=0; n<aman.get_qel(); n++)
  {
    //mcout<<"volume::range: n="<<n<<'\n';
    if(aman[n]->m_range_ext(fts, 0)==1)
      //if(embed_mvol[n].range_ext(fpt, dir, 0, rngh, fpteh)==1)
    {
      //mcout<<"volume::range: n="<<n<<" fts="<<fts;
      //if(rngh<rng) // should be here always
      //{
	//mcout<<"volume::range: rngh<rng: rngh="<<rngh
	//<<"\nfpteh="<<fpteh<<'\n';
      sb=2; //rng=rngh; fpte=fpteh; //*famvol=&(embed_mvol[n]);
      faeid->amvol.put(aman[n]);
      faeid->nembed = n;
      //}
    }
  }
  //mcout<<"volume::range: fts="<<fts;
  if( sb == 1 || sb == 2 ) return 1;
  else return 0;
}

void absvol::income(gparticle* gp) {;}

void absvol::print(ostream& file, int l) const
{
  if(l>0)
  {
    char s[1000];
    chname(s);
    Ifile<<"absvol::print(l="<<l<<"): name="<<s<<'\n';
    l=l-1;
    if(l>0)
    {
      DynLinArr< manip_absvol * > embed= Gamanip_embed();
      indn.n+=2;
      if(embed.get_qel()>0)
      {
	Ifile<<"The following volumes are embraced, q="<<embed.get_qel()<<'\n';
	indn.n+=2;
	int n;
	for( n=0; n<embed.get_qel(); n++)
	{
	  Ifile<<"n="<<n<<'\n';
	  indn.n+=2;
	  embed[n]->m_print(file, l);
	  indn.n-=2;
	}
	indn.n-=2;
      }
      else
      {
	Ifile<<"None of embraced volumes\n";
      }
      indn.n-=2;
    }
    file.flush();
  }
}

macro_copy_body_not_defined(absvol)
  


//          *********  manip_absvol  *********

int manip_absvol::m_check_point_inside(const point& fpt, 
				     const vec& fdir) const 
{
#ifdef TRACE_check_point_inside
  mcout<<"manip_absvol::m_check_point_inside:\n";
  Iprintn(mcout, fpt);
  Iprintn(mcout, fdir);
#endif
  const abssyscoor* asc=Gasc();
  const absvol *avol=Gavol();
  if(asc != NULL)
  {
#ifdef TRACE_check_point_inside
    mcout<<"asc != NULL\n";
#endif 
    point pt=fpt;
    vec dir=fdir;
    pt.up(asc);
    dir.up(asc);
#ifdef TRACE_check_point_inside
    mcout<<"after transition:\n";
    Iprintn(mcout, pt);
    Iprintn(mcout, dir);
    int s_ret = avol->check_point_inside(pt, dir);
    Iprintn(mcout, s_ret);
    return s_ret;
#else
    return avol->check_point_inside(pt, dir);
#endif
  }
  else
  {
#ifdef TRACE_check_point_inside
    mcout<<"asc == NULL\n";
    int s_ret =  avol->check_point_inside(fpt, fdir);
    Iprintn(mcout, s_ret);
    return s_ret;
#else 
    return avol->check_point_inside(fpt, fdir);
#endif
  }
}

int manip_absvol::m_find_embed_vol(const point& fpt, 
				   const vec& fdir, 
				   manip_absvol_treeid* atid) const  
{
  mfunname("int manip_absvol::m_find_embed_vol(...)");
#ifdef TRACE_find_embed_vol
  indn.n++;
  Imcout<<"manip_absvol::m_find_embed_vol: entering, qeid="<<atid->qeid<<'\n';
  int n;
  Imcout<<"manip_absvol::m_find_embed_vol: embracing volumes:\n";
  for( n=0; n<atid->qeid; n++)
    atid->eid[n].print(mcout,0);
  Imcout<<"manip_absvol::m_find_embed_vol: this volume:\n";
  m_print(mcout,0);
  Imcout<<"manip_absvol::m_find_embed_vol: point:\n";
  Imcout<<fpt;
#endif
  //syscoor* asc=Gasc();
  absvol *avol=Gavol();
  point pt=fpt; up_absref(&pt);
  vec dir=fdir; up_absref(&dir);
  atid->eid[atid->qeid++].amvol.put( (manip_absvol*)this );  // to kill const
  int s = atid->qeid;

#ifdef TRACE_find_embed_vol
  Imcout<<"manip_absvol::m_find_embed_vol: calling avol->find_embed_vol\n";
#endif

  int iret = avol->find_embed_vol(pt, dir, atid);

#ifdef TRACE_find_embed_vol
  Imcout<<"manip_absvol::m_find_embed_vol: returning to this volume:\n";
  m_print(mcout,0);
  Imcout<<"manip_absvol::m_find_embed_vol: return code "<<iret<<'\n';
#endif

  if(iret == 0)
  {
    if(atid->qeid < s)
    {
      mcerr<<"manip_absvol::m_find_embed_vol: should never happen\n";
      exit(1);
    }
    atid->qeid--; 
#ifdef TRACE_find_embed_vol
    Imcout<<"manip_absvol::m_find_embed_vol: after neg inc call atid->qeid="
	 << atid->qeid <<" exiting\n";
    indn.n--;
#endif
    return 0;
  }
  else
  {
    if(atid->qeid < s)
    {
      mcerr<<"manip_absvol::m_find_embed_vol: should never happen\n";
      exit(1);
    }
    //mcout<<"--------------> find_embed_vol:  found in\n";
    //mcout<<"namvol="<<namvol<<'\n';
    //print(mcout,1);
#ifdef TRACE_find_embed_vol
    Imcout<<"manip_absvol::m_find_embed_vol: after pos inc call atid->qeid="
	 <<atid->qeid<<" exiting\n";
    m_print(mcout,0);
    indn.n--;
#endif
    return 1;
  }
}
int manip_absvol::m_range(trajestep& fts, int s_ext,  
  		    int& sb, manip_absvol_eid* faeid) const  
{
  trajestep ts(fts);
  up_absref(&ts);
  //if(type==Eshifted_private || type==Eshifted_alien)
  //ts.up(asc);
  absvol *avol=Gavol();

  int s=avol->range(ts, s_ext, sb, faeid);
  if(s==1)
  {
    down_absref(&ts);
    //if(type==Eshifted_private || type==Eshifted_alien)
    //ts.down(asc);
    fts = ts;
  }
  return s;
}
int manip_absvol::m_range_ext(trajestep& fts, int s_ext) const 
{
  trajestep ts(fts);
  up_absref(&ts);
  //if(type==Eshifted_private || type==Eshifted_alien)
  //ts.up(asc);
  absvol *avol=Gavol();
  int s=avol->range_ext(ts, s_ext);
  if(s==1)
  {
    down_absref(&ts);
    //if(type==Eshifted_private || type==Eshifted_alien)
    //ts.down(asc);
    fts = ts;
    //mcout<<"manip_absvol::range_ext: s_ext="<<s_ext<<" fts="<<fts;
  }
  return s;
}
void manip_absvol::m_chname(char *nm) const 
{ 
  //if(avol==0) strcpy(nm,"empty manip_absvol"); 
  //else 
  //{ 
  strcpy(nm,"mvol->"); Gavol()->chname(&nm[6]); //}
}

void manip_absvol::m_print(ostream& file, int l) const 
{
  if(l>0)
  {
    char s[1000];
    m_chname(s);
    Ifile<<"manip_absvol::m_print(l="<<l<<"): "<<s<<'\n';
    l=l-1;
    if(l>0)
    {
      indn.n+=2;
      const abssyscoor *asys = Gasc();
      if( asys != NULL ) asys->print(file,l-1);
      else mcout<<"manip_absvol::m_print: system==NULL\n";
      absvol* avol=Gavol();
      if(avol!=NULL)avol->print(file,l-1);
      else mcout<<"manip_absvol::m_print: avol==NULL\n";
      indn.n-=2;
    }
    file.flush();
  }
}

macro_copy_body_not_defined(manip_absvol)


//          *********  sh_manip_absvol  *********

//absref absref::*(sh_manip_absvol::aref)=(absref absref::*)&sh_manip_absvol::csys;
//absref absref::*(sh_manip_absvol::aref)=
//  (absref absref::*)
//  (absref manip_absvol::*)
//  (absref sh_manip_absvol::*)
//  &sh_manip_absvol::csys;
//absref absref::*(sh_manip_absvol::aref)=&sh_manip_absvol::csys;
void sh_manip_absvol::get_components(ActivePtr<absref_transmit>& aref_tran)
{
  //aref_tran.pass(new absref_transmit(1, &aref));
  aref_ptr[0] = &csys;
  aref_tran.pass(new absref_transmit(1, aref_ptr));
}

const abssyscoor* sh_manip_absvol::Gasc(void) const {return &csys;}

//void sh_manip_absvol::Garef(
//		    int& fqaref , absref absref::**&faref, //fixed memory
//	            int& fqareff, absref **&fareff) // free memory
//{ 
//  fqaref=1; fqareff=0; faref=&aref; fareff=NULL; 
//}

sh_manip_absvol::sh_manip_absvol(void): csys() 
{
  //mcerr<<"sh_manip_absvol::sh_manip_absvol(void) is done\n";
}
sh_manip_absvol::sh_manip_absvol(sh_manip_absvol& f): csys(f.csys) {;}
sh_manip_absvol::sh_manip_absvol(const sh_manip_absvol& f): csys(f.csys) {;}
sh_manip_absvol::sh_manip_absvol(const abssyscoor& f): csys(f) 
{ ;
  //mcout<<"sh_manip_absvol::sh_manip_absvol is done\n";
}

sh_manip_absvol::sh_manip_absvol(const point& fc, 
				 const basis& fbas, const String& fname):
    csys(fc, fbas, fname) {;}

void sh_manip_absvol::m_chname(char *nm) const 
{ 
  strcpy(nm,"mvol->"); Gavol()->chname(&nm[6]); //}
}

macro_copy_body_not_defined(sh_manip_absvol)

void sh_manip_absvol::m_print(ostream& file, int l) const 
{
  if(l>0)
  {
    char s[1000];
    m_chname(s);
    Ifile<<"sh_manip_absvol::m_print(l="<<l<<"): "<<s<<'\n';
    if(l>1)
    {
      indn.n+=2;
      Ifile<<"csys="<<noindent<<csys;
      absvol* avol=Gavol();
      if(avol!=NULL)avol->print(file,l-1);
      else mcout<<"manip_absvol::m_print: avol==NULL\n";
      indn.n-=2;
    }
    file.flush();
  }
}

//         *************************************



