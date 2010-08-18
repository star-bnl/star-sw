//#include <stdlib.h>
//#include <iostream.h>
//#include <iomanip.h>
//#include <math.h>
#include "wcpplib/geometry/mparticle.h"
#include "wcpplib/clhep_units/PhysicalConstants.h"
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

mparticle::mparticle(gparticle const & gp, double fmass): 
  gparticle(gp), mass(fmass) 
{
  orig_gamma_1 = lorgamma_1(origin.speed / speed_of_light);
  orig_kin_energy = orig_gamma_1 * mass * pow(speed_of_light, 2);
  prev_gamma_1 = lorgamma_1(prevpos.speed / speed_of_light);
  prev_kin_energy = prev_gamma_1 * mass * pow(speed_of_light, 2);
  curr_gamma_1 = lorgamma_1(currpos.speed / speed_of_light);
  curr_kin_energy = curr_gamma_1 * mass * pow(speed_of_light, 2);
  //next_gamma_1 = lorgamma_1(nextpos.speed / speed_of_light);
  //next_kin_energy = next_gamma_1 * mass * pow(speed_of_light, 2);
}

mparticle::mparticle(gparticle const & gp, double fmass, double gamma_1): 
  gparticle(gp), mass(fmass), orig_gamma_1(gamma_1),
  prev_kin_energy(0.0), prev_gamma_1(0.0), curr_gamma_1(gamma_1) 
  //next_gamma_1(0.0), next_kin_energy(0.0) 
{
  curr_kin_energy = curr_gamma_1 * mass * pow(speed_of_light, 2);
  orig_kin_energy = curr_kin_energy;
  check_consistency(); 
}

mparticle::mparticle(manip_absvol* primvol, const point& pt, 
		     const vec& vel, vfloat time, 
		     double fmass, double gamma_1):
  gparticle(),
  mass(fmass), orig_gamma_1(gamma_1), 
  prev_kin_energy(0.0), prev_gamma_1(0.0), curr_gamma_1(gamma_1) 
{
  mfunname("mparticle::mparticle(...)");
  //mcout<<"origin.namvol="<<origin.namvol<<'\n';
  origin.tid.eid[0].nembed=0; // just to clear
  primvol->m_find_embed_vol(pt, vel, &origin.tid);
  origin.pt=pt; 
  if(vel == dv0 )
  {
    check_econd11(gamma_1 , != 0.0 , mcerr);
    origin.dir=dv0; origin.speed=0.0;
  }
  else
  {
    origin.dir=unit_vec(vel); 
    origin.speed = speed_of_light * lorbetta( gamma_1 );
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
  curr_kin_energy = curr_gamma_1 * mass * pow(speed_of_light, 2);
  orig_kin_energy = curr_kin_energy;
  check_consistency(); 
} 



void mparticle::check_consistency(void) const
{
  mfunname("void mparticle::check_consistency(double speed) const");
  check_econd11(vecerror , != 0 , mcerr);
  double speed;
  speed = speed_of_light * lorbetta( orig_gamma_1 );
  check_econd11a(fabs(speed - origin.speed)/(speed + origin.speed), 
		> 1.0e-10 , (*this), mcerr); 
  speed = speed_of_light * lorbetta( prev_gamma_1 );
  check_econd11a(fabs(speed - prevpos.speed)/(speed + prevpos.speed), 
		> 1.0e-10 , (*this), mcerr); 
  speed = speed_of_light * lorbetta( curr_gamma_1 );
  check_econd11a(fabs(speed - currpos.speed)/(speed + currpos.speed), 
		> 1.0e-10 , (*this), mcerr); 
  //speed = speed_of_light * lorbetta( next_gamma_1 );
  //check_econd12(speed, != , nextpos.speed , mcerr); 
  double kin_ener = orig_gamma_1 * mass * pow( speed_of_light, 2.0);
  if(kin_ener > 1000.0 * DBL_MIN)  // otherwise it is unprecision
  {
    check_econd11a(fabs(orig_kin_energy - kin_ener)/(orig_kin_energy + kin_ener),
		   > 1.0e-9 , "kin_ener="<<kin_ener<<'\n'<<(*this), mcerr); 
  }
  kin_ener = prev_gamma_1 * mass * pow( speed_of_light, 2.0);
  if(kin_ener > 1000.0 * DBL_MIN)
  {
    check_econd11a(fabs(prev_kin_energy - kin_ener)/(prev_kin_energy + kin_ener),
		   > 1.0e-9 , "kin_ener="<<kin_ener<<'\n'<<(*this), mcerr); 
  }
  kin_ener = curr_gamma_1 * mass * pow( speed_of_light, 2.0);
   if(kin_ener > 1000.0 * DBL_MIN)
  {
    check_econd11a(fabs(curr_kin_energy - kin_ener)/(curr_kin_energy + kin_ener),
		   > 1.0e-9 , "kin_ener="<<kin_ener<<'\n'<<(*this), mcerr); 
  }
  //check_econd12(orig_kin_energy , != , 
  //		orig_gamma_1 * mass * pow( speed_of_light, 2.0) , mcerr);
  //check_econd12(prev_kin_energy , != , 
  //		prev_gamma_1 * mass * pow( speed_of_light, 2.0) , mcerr);
  //check_econd12(curr_kin_energy , != , 
  //		curr_gamma_1 * mass * pow( speed_of_light, 2.0) , mcerr);
  //check_econd12(next_kin_energy , != , 
  //		next_gamma_1 * mass * pow( speed_of_light, 2.0) , mcerr);
};

void mparticle::step(void)
{          // make step to nextpos and calculate new step to border
  mfunname("void mparticle::step(void)");
  prevpos         = currpos;
  prev_kin_energy = curr_kin_energy;
  prev_gamma_1    = curr_gamma_1;
  currpos         = nextpos;
  curr_relcen     = dv0;
  //curr_kin_energy = next_kin_energy;
  //curr_gamma_1    = next_gamma_1;
  total_range_from_origin += currpos.prange;
  nstep++;
  if(currpos.prange==0)
  {
    n_zero_step++;
    check_econd12a(n_zero_step , > , max_q_zero_step, 
		  "too much zero steps, possible infinite loop\n";
		   print(mcout,10);, mcerr);
  }
  else
    n_zero_step=0;
  new_speed();  // calculate new current speed, direction and time,
  // basing on the force applied and path travelled.
  // But the path itself is not changed, it is already passed and fixed.
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

void mparticle::curvature(int& fs_cf, vec& frelcen, vfloat& fmrange, 
			  vfloat prec)
{
  pvecerror(
  "void mparticle::curvature(int& fs_cf, vec& frelcen, vfloat& fmrange)");
  vec f;
  vec f_perp_fl;
  int i=force(currpos.pt, f, f_perp_fl, fmrange);
  vec f_perp=currpos.speed * (currpos.dir||f_perp_fl);
  f += f_perp;
  int j;
  //if(i==0 || f==dv0 || currpos.dir==dv0 || check_par(currpos.dir, f)!=0)
  if(i==0 || f==dv0)
  {
    fs_cf=0; frelcen=dv0; //fmrange=max_vfloat;
    if(currpos.dir==dv0) fmrange=0; // to stay in the place
    return;
  }
  if(currpos.dir==dv0)   // starting to move in the direction of force
    currpos.dir=unit_vec(f);
  if((j=check_par(currpos.dir, f, prec))!=0)
  {
    fs_cf=0; frelcen=dv0; //fmrange=max_vfloat;
    if(j==-1)  // deccelerate, search for stop point
    {
      double ran=curr_kin_energy/length(f);
      if(fmrange>ran) fmrange=ran;
    }
  }
  else
  {
    // 
    fs_cf=1;
    vec fn=project_to_plane(f, currpos.dir);  // normal component
    frelcen=unit_vec(fn);
    double len=length(fn); 
    //mcout<<"f="<<f<<"fn="<<fn<<"len="<<len<<'\n';
    //len=len/((curr_gamma_1+1) * mass * currpos.speed * currpos.speed);  
	   // normal acceleration
    //mcout<<"curr_gamma_1="<<curr_gamma_1
    //	 <<" mass="<<mass<<" speed_of_light="<<speed_of_light<<'\n';
    vfloat rad=(currpos.speed * currpos.speed * (curr_gamma_1+1) * mass)/len;
    //mcout<<"len="<<len<<" rad="<<rad<<'\n';
    frelcen*=rad;
  }
  currpos.dirloc = currpos.dir; currpos.tid.up_absref(&currpos.dirloc);
  
}

//double mparticle::speed_of_light; 

/*
void mparticle::new_speed(const point& old_pt, 
			  double old_speed, 
			  double old_kin_energy,
			  const point& new_pt,
			  double& new_speed, 
			  double& new_kin_energy)
*/

void mparticle::new_speed(void)
{
  pvecerror("void mparticle::new_speed(void)");
  //prev_kin_energy=curr_kin_energy;
  //prev_gamma_1=curr_gamma_1;
  if(currpos.prange==0.0)
  {
    //curr_kin_energy=prev_kin_energy;
    //curr_gamma_1=prev_gamma_1;
    // speed is preserved by gparticle
    check_consistency();
    return;
  }    
  vec f1, f2, f_perp1, f_perp2, f_perp_fl1, f_perp_fl2;
  vec f_mean;
  vfloat r1, r2;  // ranges, do not need here
  int i,j;
  i=force(prevpos.pt, f1, f_perp_fl1, r1);
  j=force(currpos.pt, f2, f_perp_fl2, r2);
  check_econd11a(vecerror , != 0 , "position 1, after computing force\n", 
		 mcerr);
  f_perp1=prevpos.speed * (prevpos.dir||f_perp_fl1);
  //f_perp1=currpos.speed * (currpos.dir||f_perp_fl1);  // for debug
  f_perp2=currpos.speed * (currpos.dir||f_perp_fl2);
  // Later f_perp are ignored since they can not do the work;
  f_mean=(f1+f2)/2.0;
  check_econd11a(vecerror , != 0 , "position 2, after computing f_perp\n", 
		 mcerr);
  

  if(i==0 && j==0 || f_mean == dv0) 
  {
    curr_kin_energy=prev_kin_energy;
    curr_gamma_1=prev_gamma_1;
    currpos.speed=prevpos.speed;  // new change
    // speed is preserved by gparticle
    //return;
  }   
  else
  {
    vec r=currpos.pt - prevpos.pt;
    double W=0;  // force * range * cos() = work * cos() ( may be negative )
    if(r != dv0) 
      W=f_mean * r; 
        //W=f1*unit_vec(r) * currpos.prange; 
        // prange should be more exact than difference- no, this is not correct
        // This is work which should lead to increse or decrease the speed
    if(W==0)
    {
      curr_kin_energy=prev_kin_energy;
      curr_gamma_1=prev_gamma_1;
      currpos.speed=prevpos.speed;  // new change
      // speed is preserved by gparticle
      //return;
    }   
    else
    {
      curr_kin_energy = prev_kin_energy + W;
      if(curr_kin_energy<=0)
      {
	curr_kin_energy=0;
	currpos.speed=0;
	curr_gamma_1=0;
	//if(f2==dv0)  // temporary staying. May be field changes...
	//{
	currpos.dir=dv0; 
	//}
	//else
	//{
	//currpos.dir=unit_vec(f2);
	//}
      }
      else
      {
	double resten=mass*pow(speed_of_light, 2);
	curr_gamma_1=curr_kin_energy / resten;
	currpos.speed=speed_of_light * lorbetta( curr_gamma_1 );
	//currpos.speed=speed_of_light * sqrt(1.0 - 1.0 / pow(curr_gamma, 2));
      }
    }
  }
  if(!(i==0 && j==0)) 
  {
    //double f_p_len=
    vec fn1=project_to_plane(f1, prevpos.dir);  // normal component
    //frelcen1=unit_vec(fn1);
    //double len1=length(fn1); 
    vec fn2=project_to_plane(f2, currpos.dir);  // normal component
    check_econd11a(vecerror , != 0 , "position 3, after computing fn2\n", 
		   mcerr);
    vec mean_fn=0.5*(fn1 + fn2); // mean ortogonal component of working force
    //frelcen2=unit_vec(fn2);
    //double len2=length(fn2); 
    double mean_fn_len= length(mean_fn);
    vec fdir=prevpos.dir;
    if(mean_fn_len > 0.0)
    {
      vec relcen=unit_vec(mean_fn);
      double mean_speed=(prevpos.speed + currpos.speed)*0.5;
      vfloat new_rad=(mean_speed * mean_speed * 
		      ((prev_gamma_1+curr_gamma_1)*0.5 + 1) * mass)/
	mean_fn_len;
      if(new_rad > 0.0)
      {
	vfloat ang=currpos.prange/new_rad;  // angle to turn
	fdir.turn(prevpos.dir||relcen, ang);       // direction at the end
      }
    }
    check_econd11a(vecerror , != 0 , "position 4\n", 
		   mcerr);
    vec mean_f_perp_fl = 0.5*( f_perp_fl1 + f_perp_fl2 ); 
    double len_mean_f_perp_fl=length(mean_f_perp_fl);
    f_perp2=currpos.speed * (currpos.dir||f_perp_fl2);
    double mean_f_perp=0.5*(length(f_perp1) + length(f_perp2));
    check_econd11a(vecerror , != 0 , "position 5\n", 
		   mcerr);
    if(len_mean_f_perp_fl > 0.0)
    {
      vec fdir_proj=project_to_plane(prevpos.dir, mean_f_perp_fl);
      if(not_apeq( length(fdir_proj), 0.0 ) == 1)
      {
	check_econd11a(vecerror , != 0 , "position 6\n", 
		       mcerr);
	double length_proj=currpos.prange*cos2vec(prevpos.dir, fdir_proj);
	check_econd11a(vecerror , != 0 , "position 7\n", 
		       mcerr);
	double acc=mean_f_perp/
	  (((prev_gamma_1+curr_gamma_1)*0.5 + 1) * mass);
	double mean_speed=(prevpos.speed + currpos.speed)*0.5;
	double new_rad=pow( mean_speed * length(fdir_proj) , 2.0)/acc;
	double ang=length_proj/new_rad;
	if(new_rad > 0 && ang > 0)
	{
	  fdir.turn(mean_f_perp_fl, -ang);       // direction at the end
	  check_econd11a(vecerror , != 0 , "position 8\n", 
			 mcerr);
	}
      }
    }
    currpos.dir=fdir;
    check_econd11a(vecerror , != 0 , "position 9, after turn\n", 
		   mcerr);

    /*
    double f_perp_len = (length(f_perp1) + length(f_perp2))*0.5;
    // Assumed that direction of turn is stable !!!
    // It is intended mainly for magnetic spectrometers
    // if(f_perp1 != dv0) the particle turns according to it and
    // direction of f_perp2 is ignored.
    // Otherwise the particle is turned according to f_perp2
    if(f_perp_len > 0.0)
    {
      vec frelcen;  // new vector to radious 
      if(length(f_perp1) > 0.0)
	frelcen=unit_vec(f_perp1);
      else
	frelcen=unit_vec(f_perp2);
      double mean_speed=(prevpos.speed + currpos.speed)*0.5;
      vfloat new_rad=(mean_speed * mean_speed * 
		      ((prev_gamma_1+curr_gamma_1)*0.5 + 1) * mass)/f_perp_len;
      if(new_rad > 0.0)
      {
	vfloat ang=currpos.prange/new_rad;  // angle to turn
	vec fdir=prevpos.dir;
	fdir.turn(prevpos.dir||frelcen, ang);       // direction at the end
	//mcout<<"curvature:\n";
	//mcout<<"prevpos="<<noindent; prevpos.print(mcout, 10);
	//mcout<<"currpos="<<noindent; currpos.print(mcout, 10);
	//mcout<<"f_perp1="<<f_perp1;
	//mcout<<"f_perp2="<<f_perp2;
	//mcout<<"prevpos.dir="<<prevpos.dir;
	//mcout<<"currpos.dir="<<currpos.dir;
	//mcout<<"fdir="<<fdir;
	currpos.dir=fdir;
      }
    }
    */
  }
  currpos.dirloc = currpos.dir; currpos.tid.up_absref(&currpos.dirloc);
  currpos.time = prevpos.time +  currpos.prange/
    ((prevpos.speed + currpos.speed)/2.0);
  check_consistency();
}
void mparticle::print(ostream& file, int l) const 
{
  if(l >=0 )
  {  
    Ifile<<"mparticle: mass="<<mass<<" ("
	 <<mass/kg<<" kg, "<<mass*c_squared/GeV<<" GeV)\n";
    Ifile<<"speed_of_light="<<speed_of_light<<" ("
	 <<speed_of_light/(m/second)<<"m/second)\n";
    Ifile<<"orig_kin_energy="<<orig_kin_energy
	 <<" ("<<orig_kin_energy/GeV<<" GeV)"
	 <<" orig_gamma_1="<<orig_gamma_1<<'\n';
    Ifile<<"prev_kin_energy="<<prev_kin_energy
	 <<" ("<<prev_kin_energy/GeV<<" GeV)"
	 <<" prev_gamma_1="<<prev_gamma_1<<'\n';
    Ifile<<"curr_kin_energy="<<curr_kin_energy
	 <<" ("<<curr_kin_energy/GeV<<" GeV)"
	 <<" curr_gamma_1="<<curr_gamma_1<<'\n';
    //check_consistency();  // otherwise at printing from check_consistency()
    // it will fall in loop
    gparticle::print(file,l);
  }
}

ostream& operator<<(ostream& file, const mparticle& f)
{
  (&f)->print(file, 10);
  return file;
}
