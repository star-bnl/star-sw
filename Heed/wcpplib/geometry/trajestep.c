#include "wcpplib/geometry/trajestep.h"
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

void trajestep_limit::range(int fs_cf0, vfloat rad,
			    int& fs_cf1, vfloat& mrange)
{
  if(mrange<0 || mrange>max_range)
    mrange=max_range;
  //if(force > 0)
  //{
  // vfloat m=max_prange/force;
  //  if( mrange > m ) mrange = m;
  //}
  fs_cf1 = fs_cf0;
  if(fs_cf1 == 1)
  {
    if( rad >= rad_for_straight ) 
    {
      fs_cf1 = 0;
      if(mrange/rad > max_straight_arange) mrange = rad * max_straight_arange;
    }
    else
    {
      if(mrange/rad > max_circumf_arange) mrange = rad * max_circumf_arange;
    }
  }
}



absref absref::*(trajestep::aref[4])=
{
  (absref absref::*)&trajestep::currpos, 
  (absref absref::*)&trajestep::dir,
  (absref absref::*)&trajestep::relcen, 
  (absref absref::*)&trajestep::mpoint 
};

void trajestep::get_components(ActivePtr<absref_transmit>& aref_tran)
{
  aref_tran.pass(new absref_transmit(4, aref));
}

trajestep::trajestep(trajestep_limit* ftl, 
		     const point& fcurrpos, const vec& fdir, 
		     int fs_cf, const vec& frelcen, 
		     vfloat fmrange, vfloat prec):
  tl(ftl), currpos(fcurrpos), dir(), s_cf(fs_cf), 
  relcen(frelcen), s_prec(1), mrange(fmrange)
{
  pvecerror("trajestep::trajestep(...)");
  if(fdir == dv0)
  {
    dir=dv0; mrange=0;
  }
  else
  {
    dir=unit_vec(fdir);
    if(s_cf==1)
    {
      check_econd11a( check_perp(dir, relcen, prec), !=1, 
		      "dir="<<dir<<"relcen="<<relcen
		      <<"fcurrpos="<<fcurrpos
		      <<"fdir="<<fdir, mcerr);
    }
    tl->range(s_cf, length(relcen), s_range_cf, mrange);
  } 
}

trajestep::trajestep(const trajestep& fts,  // the new object will continue
		     // propagation from the end point of the old one 
		     vfloat mrange) // new range to travel
{
  mfunname("trajestep::trajestep(const trajestep& fts, vfloat mrange)");
  point fpos; vec fdir; vec frelcen;
  fts.Gnextpoint1(fts.mrange, fpos, fdir, frelcen);
  vfloat prec=0.1; // not important here
  *this = trajestep(fts.tl.getver(), fpos, fdir, 
		    fts.s_cf, frelcen, mrange, prec);
}


void trajestep::Gnextpoint(vfloat frange, point& fpos, vec& fdir) const
{
  pvecerror(
    "int trajestep::Gnextpoint(vfloat frange, point& fpos, vec& fdir)");  
  check_econd12(frange, >, mrange, mcerr);
  if(s_range_cf == 0)  // interpolation by straight line
  {
    fpos = currpos + frange*dir;
    if(s_cf == 0) // no curvature
    {
      fdir=dir;
      return; 
    }
    else
    {
      vfloat ang=frange/length(relcen);
      fdir=dir;
      fdir.turn(dir||relcen, ang);
      return;
    }
  }
  else
  {
    vfloat ang=frange/length(relcen);  // angle to turn
    fdir=dir;
    fdir.turn(dir||relcen, ang);       // direction at the end
    vec frelcen = relcen;
    frelcen.turn(dir||relcen, ang);
    fpos=currpos + relcen - frelcen;
    //mcout<<"trajestep::Gnextpoint:\n";
    //Iprint(mcout, relcen);
    //Iprintn(mcout, ang);
    //Iprint(mcout, dir);
    //Iprint(mcout, fdir);
    //Iprint(mcout, frelcen);
    //Iprint(mcout, currpos);
    //Iprint(mcout, fpos);
    //fpos=currpos;
    //point pt(currpos+relcen);
    //basis bs("temp");
    //fixsyscoor sc(&pt, &bs, "temp");
    //fpos.up(&sc);
    //fpos.turn(dir||relcen, ang);
    //fpos.down(&sc);
    return;
  }
}

void trajestep::Gnextpoint1(vfloat frange, point& fpos, 
			    vec& fdir, vec& frelcen) const
{
  pvecerror(
    "int trajestep::Gnextpoint(vfloat frange, point& fpos, vec& fdir, vec& frelcen)");  
  check_econd12(frange, >, mrange, mcerr);
  if(s_range_cf == 0)  // interpolation by straight line
  {
    fpos = currpos + frange*dir;
    if(s_cf == 0) // no curvature
    {
      fdir=dir;
      frelcen = relcen;  // whatever it is
      return; 
    }
    else
    {
      vfloat ang=frange/length(relcen);
      fdir=dir;
      fdir.turn(dir||relcen, ang);
      frelcen = relcen;
      frelcen.turn(dir||relcen, ang);
      return;
    }
  }
  else
  {
    vfloat ang=frange/length(relcen);  // angle to turn
    fdir=dir;
    fdir.turn(dir||relcen, ang);       // direction at the end
    frelcen = relcen;
    frelcen.turn(dir||relcen, ang);
    fpos=currpos + relcen - frelcen;
    //fpos=currpos;
    //point pt(currpos+relcen);
    //basis bs("temp");
    //fixsyscoor sc(&pt, &bs, "temp");
    //fpos.up(&sc);
    //fpos.turn(dir||relcen, ang);
    //fpos.down(&sc);
    return;
  }
}
ostream& operator<<(ostream& file, const trajestep& f)
{
  Ifile<<"trajestep: s_cf="<<f.s_cf<<"\n";
  indn.n+=2;
  Ifile<<"currpos:"<<f.currpos
       <<indn<<"dir="<<f.dir
       <<indn<<"relcen="<<f.relcen
       <<indn<<"s_range_cf="<<f.s_range_cf
       <<" s_prec="<<f.s_prec
       <<" mrange="<<f.mrange<<'\n'
       <<indn<<"mpoint="<<f.mpoint;
  indn.n-=2;
  return file;
}


