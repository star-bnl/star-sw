//#include <stdlib.h>
//#include <iostream.h>
//#include <iomanip.h>
//#include <math.h>
#include "wcpplib/geometry/gparticle.h"
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


void stvpoint::print(ostream& file, int l) const 
{
  if( l >= 0 )
  {
    Ifile<<"stvpoint: sb="<<sb<<" s_ent="<<s_ent
	 <<" prange="<<prange<<" time="<<time<<'\n';
    indn.n+=2;
    Ifile<<"position:\n";
    file<<pt<<ptloc;
    Ifile<<"direction of moving:\n";
    file<<dir<<dirloc;
    Ifile<<"speed="<<speed<<'\n';
    if(tid.qeid<=0)
    { 
      Ifile<<"point is outside universe, tid.qeid="<<tid.qeid<<'\n';
      file.flush();
      indn.n-=2;
      return;
    }
    tid.print(file,1);
    char s[100];
    //amvol[namvol-1]->chname(s);
    //Ifile<<"current volume name "<<s<<'\n';
    if(sb==2)
    {
      next_eid.amvol->m_chname(s);
      Ifile<<"next volume name "<<s<<'\n';
    }
    //if(l>1)
    //{
    // Ifile<<"current volume:\n";
    //  amvol[namvol-1]->print(file,l-1);
    //}
    indn.n-=2;
    file.flush();
  }
}

long gparticle::max_q_zero_step = 100;

gparticle::gparticle(manip_absvol* primvol, const point& pt, 
		     const vec& vel, vfloat time):
  s_life(0), nstep(0),
  total_range_from_origin(0.0), n_zero_step(0), prevpos(), nextpos() 
{
  mfunname("gparticle::gparticle(...)");
  //mcout<<"origin.namvol="<<origin.namvol<<'\n';
  origin.tid.eid[0].nembed=0; // just to clear
  primvol->m_find_embed_vol(pt, vel, &origin.tid);
  origin.pt=pt; 
  if(vel == dv0 )
  {
    origin.dir=dv0; origin.speed=0.0;
  }
  else
  {
    origin.dir=unit_vec(vel); origin.speed=length(vel);
  } 
  origin.ptloc=origin.pt;   origin.tid.up_absref(&origin.ptloc);
  origin.dirloc=origin.dir; origin.tid.up_absref(&origin.dirloc);
  origin.time=time; 
  origin.sb=0; origin.s_ent=1; //origin.next_eid=NULL;
  if(origin.tid.qeid==0) return;
  s_life=1;
  currpos=origin;
  nextpos=currpos; nextpos.s_ent=0;
  //mcout<<"origin.namvol="<<origin.namvol<<'\n';
  //mcout<<"origin=\n"; origin.print(mcout,1);
  //mcout<<"currpos=\n"; currpos.print(mcout,1);
  //nextpos=calc_step_to_bord();
} 

void gparticle::step(void)
{          // make step to nextpos and calculate new step to border
  mfunname("void gparticle::step(void)");
  prevpos=currpos;
  currpos=nextpos;
  curr_relcen = dv0;
  total_range_from_origin += currpos.prange;
  nstep++;
  if(currpos.prange==0)
  {
    n_zero_step++;
    check_econd12a(n_zero_step , > , max_q_zero_step, 
		  "too much zero steps, possible infinite loop\n", mcerr);
  }
  else
    n_zero_step=0;
  physics_after_new_speed();
  if(s_life == 1)
  {
    if(prevpos.tid != currpos.tid)
      //prevpos.namvol != currpos.namvol ||
      //prevpos.amvol[prevpos.namvol-1] != currpos.amvol[currpos.namvol-1])
      change_vol();  // possible correction ( reflection..)
    nextpos=calc_step_to_bord();
  }
}
     
void gparticle::curvature(int& fs_cf, vec& frelcen, vfloat& fmrange, 
			  vfloat prec)
{
  fs_cf=0; frelcen=vec(0,0,0); fmrange=max_vfloat;
  /* The following is for debug
     vec field(0,1,0);
     vfloat rad=10;
     if(length(currpos.dir)>0 && check_par(currpos.dir, field)==0)
     {
     fs_cf=1;
     vfloat coef=sin2vec(currpos.dir,field);
     rad=rad/coef;
     frelcen=unit_vec(currpos.dir||field)*rad;
     fmrange=max_vfloat;
     }
     else
     {
     fs_cf=0; frelcen=vec(0,0,0); fmrange=max_vfloat;
     }
  */
}

stvpoint gparticle::calc_step_to_bord()
{             // calculate next point as step to border
  pvecerror("stvpoint gparticle::calc_step_to_bord()");
  int sb;
  //vfloat rng;
  point pte;
  curr_relcen = dv0;
  if(currpos.sb > 0) // it does not do step, but switch to new volume
  {
    //return switch_new_vol(currpos.amvol, currpos.namvol);
    return switch_new_vol();
  }
  else
  {
    manip_absvol_eid faeid;        //manip_absvol* famvol;
    /*
    if(currpos.s_ent > 0 && 
       currpos.tid.G_lavol()->mandatory()==1) 
      // to find volume again,
      // borders can be common for many volumes,
      // or mandatory volume can be thin.
    {
      //manip_absvol* ffamvol[pqamvol];
      manip_absvol_treeid tidl;
      //int ffnamvol=0;
      currpos.tid.eid[0].amvol->
	find_embed_vol(currpos.pt, currpos.dir, &tidl);
      if(currpos.tid != tidl)
	//currpos.namvol != ffnamvol ||
	//currpos.amvol[currpos.namvol-1] != ffamvol[ffnamvol-1])
      {
	//Imcout<<"calc_step_to_bord: volume is changed again\n";
	return stvpoint(currpos.pt, currpos.dir, currpos.speed, 
			currpos.tid,
			//currpos.amvol, currpos.namvol, // till end of this
			0.0,
			currpos.time , 
			1, 0, *(tidl.G_laeid()) );
      }
    }
    */
    int s_cf; vec relcen; vfloat mrange;
    //mcout<<"gparticle::calc_step_to_bord(): now running the curvature()\n";
    curvature(s_cf, relcen, mrange, gtrajlim.max_straight_arange);
    curr_relcen = relcen;
    if(mrange<=0)
    {   // preserves currpos for modification by physics
      stvpoint temp(currpos);
      temp.s_ent=0;
      return temp;
    }
    //mcout<<"s_cf="<<s_cf<<" relcen="<<relcen
    //	 <<"mrange="<<mrange<<'\n';

    //mcout<<"gparticle::calc_step_to_bord(): now running the trajestep\n";
    currpos.tid.up_absref(&relcen);  // changing to local system
    physics_mrange(mrange);
    trajestep ts(&gtrajlim, currpos.ptloc, currpos.dirloc, 
		 s_cf, relcen, mrange, 
		 currpos.tid.G_laeid()->amvol->Gavol()->prec );
    //s_cf, relcen, mrange, currpos.next_eid.amvol->Gavol()->prec );
    if(ts.mrange<=0)
    {
      stvpoint temp(currpos);
      temp.s_ent=0;
      return temp;
      /*
      return  stvpoint(currpos.pt, currpos.dir, currpos.speed, 
		    currpos.tid, 
		    //currpos.amvol, currpos.namvol, // till end of this
		    ts.mrange,
		    currpos.time, 
		    0, 0, currpos.next_eid );
      */
    }
    //mcout<<"calc_step_to_bord: ts="<<ts;
    //currpos.tid.G_lavol()->print(mcout, 1);

    // Here the range is calculated:
    int i=currpos.tid.G_lavol()->range(ts, 1, sb, &faeid);  
    // 1 means inside the volume and makes
    // the program checking embraced volumes

    // range(currpos.pt, currpos.dir, 
    //	    1, sb, rng, pte, &faeid);
    //mcout<<"calc_step_to_bord: sb="<<sb<<" ts="<<ts;
    if(ts.s_prec==0)  // point is crossed
      return stvpoint(currpos, ts, sb, 0, faeid );
    else
      return stvpoint(currpos, ts, ts.mrange, sb, 0, faeid ); 
    /*
    vec dir;
    ts.Gnextpoint(ts.mrange, pte, dir);
    return stvpoint(pte, dir, currpos.speed, 
		    currpos.tid, 
		    //currpos.amvol, currpos.namvol, // till end of this
		    ts.mrange,
		    currpos.time + ts.mrange / currpos.speed, 
		    sb, 0, faeid );
    */
  }
}

//vfloat gparticle::precision_of_switch=PRECISION_OF_SWITCH;

//stvpoint gparticle::switch_new_vol(manip_absvol* famvol[pqamvol], int fnamvol)
stvpoint gparticle::switch_new_vol(void)
{               // generates next position in new volume
  mfunname("stvpoint gparticle::switch_new_vol(void)");
  /*
  mcout<<"gparticle::switch_new_vol:\n";
  if(currpos.next_eid.amvol==NULL)
  {
    mcout<<"currpos.next_eid.amvol="<<currpos.next_eid.amvol<<'\n';
  }
  else
  {
    currpos.next_eid.amvol->print(mcout,3);
    int i=currpos.next_eid.amvol->check_point_inside(currpos.ptloc, 
						     currpos.dirloc);
    mcout<<"i="<<i<<'\n';
  }
  */
  manip_absvol_treeid tidl;
  manip_absvol_eid eidl;
  //int namvoll=0;
  //manip_absvol* amvoll[pqamvol];
  stvpoint nextp = currpos;
  //vec additional_dist=currpos.dir * nextp.next_eid.amvol->Gavol()->prec;
  //vec additional_dist=currpos.dir * precision_of_switch;
  //nextp.pt.shift(additional_dist);
  //nextp.time=nextp.time+length(additional_dist)/nextp.speed;
  point pth=nextp.pt;
  //if(famvol[fnamvol]->Gavol()->mandatory()==1)
  /*
  if(nextp.sb==2 && nextp.next_eid.amvol->Gavol()->mandatory()==1)
  {
    // Going to embraced volume even if it is too thin
    //for( namvoll=0; namvoll<nextp.namvol; namvoll++)
    //  amvoll[namvoll]=nextp.amvol[namvoll];
    //amvoll[namvoll++]=nextp.next_mvol;
    tidl = nextp.tid;
    tidl.eid[tidl.qeid++] = nextp.next_eid;
    return stvpoint(pth, nextp.dir, nextp.speed, tidl, 
		    0.0, nextp.time, 0 , 1, eidl);
  }
  else
  {
  */
  // search from primary
  // In this case it does not necessary switch to encountered volume
  // namely nextp.next_eid
  // Borders of two volumes may coincide. Thus it should look for
  // the deepest volume at this point.
 mark1:
  nextp.tid.eid[0].amvol->m_find_embed_vol(pth, nextp.dir, &tidl);
  //tidl.print(mcout, 2);
  //mcout<<"namvoll="<<namvoll<<'\n';
  if(tidl.qeid>0)
  {
    int s_e;
    if( tidl == nextp.tid)
    {
      //namvoll==nextp.namvol && amvoll[namvoll-1]==nextp.amvol[namvoll-1] )
      s_e=0; // remains in the same old volume, may be numerical error
      // Will probably repeat attempt to switch at the same steps
      // untill success.
      //mcout<<"stvpoint gparticle::switch_new_vol(void): remains in the same volume\n";
      double curprec = nextp.tid.G_lavol()->prec;
      if(currpos.prange <= curprec)
      { // very bad case, to repeat the trial.
	//mcout<<"trial is repeated\n";
	vec additional_dist = 
	  nextp.dir * curprec;
	//Iprint(mcout, additional_dist);
	pth = pth + additional_dist;
	//Iprint(mcout, pth);
	tidl = manip_absvol_treeid();
	goto mark1;
      }
    }
    else
      s_e=1; // switch to new volume
    return stvpoint(pth, nextp.dir, nextp.speed, 
		    tidl, 0.0, nextp.time, 
		    0 , s_e, eidl);
  }
  else
  {
    s_life=0;
    return stvpoint();
  }
  //}
}
void gparticle::print(ostream& file, int l) const 
{
  if(l >=0 )
  {  
    Ifile<<"gparticle(l="<<l<<"): s_life="<<s_life
	 <<" nstep="<<nstep
	 <<" total_range_from_origin="<<total_range_from_origin
	 <<" n_zero_step="<<n_zero_step<<'\n';
    if(l == 1)
    {
      file.flush();
      return;
    }
    indn.n+=2;
    if(l-5 >=0 )
    {  
      Ifile<<"origin point:\n";
      indn.n+=2;
      origin.print(file,l-2);
      indn.n-=2;
    }
    if(l-4 >=0 )
    {  
      Ifile<<"previous point:\n";
      indn.n+=2;
      prevpos.print(file,l-1);
      indn.n-=2;
    }
    if(l-2 >=0 )
    {  
      Ifile<<"current point:\n";
      indn.n+=2;
      currpos.print(file,l);
      Iprint(file, curr_relcen);
      Ifile<<" length(curr_relcen)="<<length(curr_relcen)<<'\n';
      indn.n-=2;
    }
    if(l-3 >=0 )
    {  
      Ifile<<"next point:\n";
      indn.n+=2;
      nextpos.print(file,l-1);
      indn.n-=2;
    }
    indn.n-=2;
    file.flush();
  }
}
  

    
    
    
	    
	



