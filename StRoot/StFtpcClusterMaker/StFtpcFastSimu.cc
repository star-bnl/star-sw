// $Id: StFtpcFastSimu.cc,v 1.7 2000/01/27 14:48:06 hummler Exp $
//
// $Log: StFtpcFastSimu.cc,v $
// Revision 1.7  2000/01/27 14:48:06  hummler
// correct hit smearing
//
// Revision 1.6  2000/01/27 09:47:18  hummler
// implement raw data reader, remove type ambiguities that bothered kcc
//
// Revision 1.5  2000/01/05 13:23:40  hummler
// make cc5 happy
//
// Revision 1.4  2000/01/03 12:48:57  jcs
// Add CVS Id strings
//

#include "StFtpcFastSimu.hh"
#include <iostream.h>
#include <stdlib.h>
#include <cmath>
#include "math_constants.h"
// random number engines from StarClassLibrary
#include "Random.h"
#include "JamesRandom.h"
#include "RandFlat.h"
StFtpcFastSimu::StFtpcFastSimu(G2T_FTP_HIT_ST* g2t_ftp_hit,
			       int *g2t_ftp_hit_nok,
			       G2T_TRACK_ST* g2t_track,
			       int *g2t_track_nok,
			       G2T_VERTEX_ST* g2t_vertex,
			       FFS_FSPAR_ST* ffs_fspar,
			       FFS_GASPAR_ST* ffs_gaspar,
			       FFS_GEPOINT_ST* ffs_gepoint,
			       int *ffs_gepoint_nok,
			       int ffs_gepoint_maxlen,
			       FCL_FPPOINT_ST* fcl_fppoint,
			       int *fcl_fppoint_nok,
			       int fcl_fppoint_maxlen)
{
  //-----------------------------------------------------------------------
  
  //    check that fppoint and gepoint are large enough to hold g2t_ftp_hit

  if(*g2t_ftp_hit_nok > fcl_fppoint_maxlen ||
     *g2t_ftp_hit_nok > ffs_gepoint_maxlen)
    {
      cerr << "point tables are too small to take all geant hits!!!" << endl;
    }
  else
    {
      //  Read paramenter tables and inititialize  
      ffs_ini(ffs_fspar, ffs_gaspar);
      
      // hh Transfer the usable g2t_ftp_hit-data into fppoint and gepoint
      ffs_hit_rd(g2t_ftp_hit_nok, g2t_ftp_hit, g2t_track_nok,
		 g2t_track, g2t_vertex, ffs_gepoint_nok, ffs_gepoint_maxlen,
		 ffs_gepoint, fcl_fppoint_nok, fcl_fppoint_maxlen,
		 fcl_fppoint);
      
      // jr   mark each hit with a row-number and a individual id
      ffs_tag(ffs_gepoint_nok, ffs_gepoint_maxlen, ffs_gepoint,
	      fcl_fppoint_nok, fcl_fppoint_maxlen, fcl_fppoint);   
      
      //       generate pad response and spatial resolutions
      // mk  in the routine FFS_GEN_PADRES the routine FFS_HIT_SMEAR is called
      ffs_gen_padres(g2t_ftp_hit_nok, g2t_ftp_hit, ffs_gepoint_nok,
		     ffs_gepoint_maxlen, ffs_gepoint, fcl_fppoint_nok, 
		     fcl_fppoint_maxlen, fcl_fppoint);

      // Check for hit-merging

      ffs_merge_tagger(ffs_gepoint_nok, ffs_gepoint_maxlen, ffs_gepoint,
		       fcl_fppoint_nok, fcl_fppoint_maxlen, fcl_fppoint);
    }
}

StFtpcFastSimu::~StFtpcFastSimu()
{
  cout << "StFtpcFastSimu destructed" << endl;
}

int StFtpcFastSimu::ffs_gen_padres(int *g2t_ftp_hit_nok, 
				   G2T_FTP_HIT_ST *g2t_ftp_hit, 
				   int *ffs_gepoint_nok,
				   int ffs_gepoint_maxlen,
				   FFS_GEPOINT_ST *ffs_gepoint,
				   int *fcl_fppoint_nok,
				   int fcl_fppoint_maxlen,
				   FCL_FPPOINT_ST *fcl_fppoint)
  {
    // Local Variables:
    float check1, check2;
    float xi, yi, zi, phi, Rh, Vh, Timeb;
    float sigTimeb, sigPhi, sigma_tr, sigma_tr_hit;
    float sigma_l_hit, sigma_z, sec_width;
    float alpha, lambda;
    float r, pt; 
    float twist_cosine, twist, theta, cross_ang;
    
    // Variables to call ffs_hit_smear
    
    float xo, yo, zo, sigma_x, sigma_y;
    
    // Loop Variables

    int k;

    //------------------------------------------------------------------------
    // Parameters

    const float l=2.0, n_0=92.0, n_eff=11.0;

    //-----------------------------------------------------------------------


    HepJamesRandom engine;
    HepRandom quasiRandom(engine);

    //     loop over tracks

    //mk Check: is s_rad=0 and s_azi=0 then no hit-shifting is wanted. 

    check1 = abs((int)s_rad[0])+abs((int)s_rad[1])+abs((int)s_rad[2])+abs((int)s_rad[3]);
    check2 = abs((int)s_azi[0])+abs((int)s_azi[1])+abs((int)s_azi[2])+abs((int)s_azi[3]);
	
    if(check1==0. && check2 == 0. ) 
      {
           return FALSE;
      }

    //mk end of check

    for(k = 0; k<*fcl_fppoint_nok; k++)
      {
	//            get space point

	xi = fcl_fppoint[k].x;
	yi = fcl_fppoint[k].y;
	zi = fcl_fppoint[k].z;

	//             calculate spatial resolution along the padrow

	//       Azimuthal angle Phi (in radians)
	phi = atan2((double) yi,(double) xi);
	if(phi<0)
	  phi += C_2PI;

	//       Radius of Hit
	Rh = sqrt(xi*xi + yi*yi);

	//       Drift velocity at hit [cm/microsec]
	Vh = Vhm[0] + Vhm[1]*Rh + Vhm[2]*Rh*Rh + Vhm[3]*Rh*Rh*Rh;

	//       Arrival time at Readout-Chambers in microsec    
	Timeb = Tbm[0] + Tbm[1]*Rh + Tbm[2]*Rh*Rh + Tbm[3]*Rh*Rh*Rh;

	// Angle-Determination:
	// Calculate Dip- and Crossing-Angle
	// For low-momenta particles the ionization is sometimes pointlike;
	// then at least one of the momentum-components is 0; therefore set the
	// angles to 0

	if((ffs_gepoint[k].p_g[0]==0.)||
	   (ffs_gepoint[k].p_g[1]==0.)||
	   (ffs_gepoint[k].p_g[2]==0.))
	  {
	    alpha  = 0.;
	    lambda = 0.;
	  }
	else
	  {
	    // twist-angle:
	    r  = sqrt ( sqr(g2t_ftp_hit[k].x[0]) + 
			sqr(g2t_ftp_hit[k].x[1]));
	    pt = sqrt ( sqr(g2t_ftp_hit[k].p[0]) + 
			sqr(g2t_ftp_hit[k].p[1]));
	    twist_cosine=(g2t_ftp_hit[k].p[0]*g2t_ftp_hit[k].x[0]+
			  g2t_ftp_hit[k].p[1]*g2t_ftp_hit[k].x[1])/(r*pt);
	    if ( twist_cosine > 1.0 ) 
	      twist_cosine = 1.0;
	    if ( twist_cosine < -1.0 ) 
	      twist_cosine = -1.0;
	    twist = C_DEG_PER_RAD*acos(twist_cosine);

	    // dip-angle:
            theta = C_DEG_PER_RAD*
atan2((double) (pt*cos(twist*C_RAD_PER_DEG)),
				       (double) ((g2t_ftp_hit[k].x[2]
					 /fabs(g2t_ftp_hit[k].x[2]))*
					g2t_ftp_hit[k].p[2]));

	    // crossing-angle: 
            cross_ang = C_DEG_PER_RAD*
	      atan2((double) (pt*cos(fabs(90.-twist)*C_RAD_PER_DEG)),   
(double) ((g2t_ftp_hit[k].x[2]/fabs(g2t_ftp_hit[k].x[2]))*
		    g2t_ftp_hit[k].p[2]));
	    alpha  = fabs(cross_ang*C_RAD_PER_DEG);
            if(alpha>(C_PI_2))
	      alpha=C_PI-alpha;

	    lambda = fabs(theta*C_RAD_PER_DEG);
            if(lambda>(C_PI_2)) 
	      lambda=C_PI-lambda;

	  }

	//>>>>>>>>>>>>>>> AZIMUTHAL Direction>>>>>>>>>>>>>>>>>>>>>>

	//   Standard deviation in azimuthal-direc. (microns)
	sigPhi = s_azi[0]+s_azi[1]*Rh+s_azi[2]*sqr(Rh)+s_azi[3]*sqr(Rh)*Rh;

	//   Sigma_tr response; including PRF
	sigma_tr = sqrt(sqr(prf_wid)/(l*n_0)
			+sqr(sigPhi)/(l*n_0*sqr(cos(alpha)))
			+(sqr(l*tan(alpha)))/(12.*n_eff));

	//   Reprojection to the point of Origin (=Hitpoint)
	sigma_tr_hit = sigma_tr * (Rh/ra);

	//<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

	//>>>>>>>>>>>>>>> RADIAL Direction>>>>>>>>>>>>>>>>>>>>>>>>>

	//       Standard deviation in r-direc. (microns)
	sigTimeb = s_rad[0] + s_rad[1]*Rh + s_rad[2]*sqr(Rh) + 
	  s_rad[3]*sqr(Rh)*Rh;

	//mk Sigma longitudinal at anode [micron] 
	//mk Include Shaper-width
	sigTimeb = sqrt(sqr(shaper_wid)/(l*n_0) 
			+sqr(sigTimeb)/(l*n_0*sqr(cos(lambda)))      
			+sqr(l*tan(lambda))/(12.*n_eff));

	//   Sigma longitudinal at hit [cm] 
  
	if (slong>=0.)
	  {
	    //  assume fix longitudinal resolution, about 700 microns,  
	    //  as for the TPC
	    cout << "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" << endl;
	    cout << " WARNING! " << endl;
	    cout << " Longitudinal Resolution assumed with fixed value!" << endl;
	    cout << " WARNING!" << endl;
	    cout << "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX " << endl;
	    sigma_l_hit = slong;
	  }
	else
	  {
	    sigma_l_hit = sigTimeb; 
	  }

	//mk Reprojection to the point of Origin (=Hitpoint)
	sigma_l_hit = sigma_l_hit*(Vh/Va); 

	//<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

	//>>>>>>>>>>>>>>> Z Direction>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	//   Sector width
	sec_width = anode_width;
	//   Sigma z
	//                sigma_z = sec_width / sqrt(12.) 
	sigma_z = 0.;

	//<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

	//mk  Va = velocity at readout-ch. in [cm/microsec] 
	//              sigma_l = sigTimeb * Va

	//-> Smearing

	ffs_hit_smear( phi, xi, yi, zi, &xo, &yo, &zo,
		       sigma_l_hit, sigma_tr_hit,&sigma_z,&sigma_x,&sigma_y,
		       quasiRandom);

	fcl_fppoint[k].x = xo;
	fcl_fppoint[k].y = yo;
	fcl_fppoint[k].z = zo;
	fcl_fppoint[k].x_err = sigma_x;
	fcl_fppoint[k].y_err = sigma_y;
	fcl_fppoint[k].z_err = sigma_z;
      }
    return TRUE;
  }

int StFtpcFastSimu::ffs_hit_rd(int *g2t_ftp_hit_nok,
			       G2T_FTP_HIT_ST *g2t_ftp_hit,
                               int *g2t_track_nok, 
			       G2T_TRACK_ST *g2t_track,
                               G2T_VERTEX_ST *g2t_vertex,
                               int *ffs_gepoint_nok,
			       int ffs_gepoint_maxlen,
			       FFS_GEPOINT_ST *ffs_gepoint,
                               int *fcl_fppoint_nok,
			       int fcl_fppoint_maxlen,
			       FCL_FPPOINT_ST *fcl_fppoint)
  {
    int ih, ih_max;
    float phi , dphi; 
    
    //-----------------------------------------------------------------------

    //     set ih_max 
      
    ih_max = *g2t_ftp_hit_nok;

    //     loop over MC hits and extract sector-row information
    
    for(ih= 0; ih<ih_max; ih++)
      {
	// specification variables:
	// changed to new structures (gepoint) 01/29/98 hh
	ffs_gepoint[ih].ge_pid =
	  g2t_track[g2t_ftp_hit[ih].track_p-1].ge_pid;    
	ffs_gepoint[ih].ge_track_p = g2t_ftp_hit[ih].track_p;
	ffs_gepoint[ih].prim_tag = g2t_ftp_hit[ih].track_type;
	if(g2t_track[g2t_ftp_hit[ih].track_p-1].charge < 0.0)
	  {
	    ffs_gepoint[ih].prim_tag *= -1;
	  }
	fcl_fppoint[ih].row = g2t_ftp_hit[ih].volume_id - 100;
	if (fcl_fppoint[ih].row > 10)
	  {
	    fcl_fppoint[ih].row -= 90;
	  }

	// Vertex-Momenta
	ffs_gepoint[ih].p_v[0] =
	  g2t_track[g2t_ftp_hit[ih].track_p-1].p[0];
	ffs_gepoint[ih].p_v[1] =
	  g2t_track[g2t_ftp_hit[ih].track_p-1].p[1];
	ffs_gepoint[ih].p_v[2] =
	  g2t_track[g2t_ftp_hit[ih].track_p-1].p[2];
	
	// Vertex position
	ffs_gepoint[ih].vertex[0] = 
	  g2t_vertex[g2t_track[g2t_ftp_hit[ih].track_p-1].start_vertex_p-1]
	  .ge_x[0];
	ffs_gepoint[ih].vertex[1] = 
	  g2t_vertex[g2t_track[g2t_ftp_hit[ih].track_p-1].start_vertex_p-1]
	  .ge_x[1];
	ffs_gepoint[ih].vertex[2] = 
	  g2t_vertex[g2t_track[g2t_ftp_hit[ih].track_p-1].start_vertex_p-1]
	  .ge_x[2];

	// Secondary production process

	ffs_gepoint[ih].ge_proc = 
	  g2t_vertex[g2t_track[g2t_ftp_hit[ih].track_p-1].start_vertex_p-1]
	  .ge_proc;

	//local momentum
	ffs_gepoint[ih].p_g[0] = g2t_ftp_hit[ih].p[0]; 
	ffs_gepoint[ih].p_g[1] = g2t_ftp_hit[ih].p[1]; 
	ffs_gepoint[ih].p_g[2] = g2t_ftp_hit[ih].p[2]; 

	fcl_fppoint[ih].x = g2t_ftp_hit[ih].x[0]; 
	fcl_fppoint[ih].y = g2t_ftp_hit[ih].x[1]; 
	fcl_fppoint[ih].z = g2t_ftp_hit[ih].x[2]; 
	//sector number 
	phi = atan2((double) fcl_fppoint[ih].y,
                    (double) fcl_fppoint[ih].x);
	if ( phi < 0.0 ) 
	  phi += C_2PI;
	dphi = myModulo((phi-phimin+C_2PI), C_2PI);
	fcl_fppoint[ih].sector = int ( dphi/phisec ) + 1;  
	
	//de/dx
	fcl_fppoint[ih].max_adc = int( 8000000.0 * g2t_ftp_hit[ih].de );
	fcl_fppoint[ih].charge = fcl_fppoint[ih].max_adc * 6;
	// for de/dx simulations, introduce de/dx smearing + adjust factors! hh
	fcl_fppoint[ih].n_pads = 4;
	fcl_fppoint[ih].n_bins = 3;
	// possibly make n_pads, n_bins dependent on exact position, charge
      }

    //     set the row counter
    *ffs_gepoint_nok = ih_max;
    *fcl_fppoint_nok = ih_max;

    return TRUE;
  }

int StFtpcFastSimu::ffs_hit_smear(float phi, 
				  float xi, 
				  float yi, 
				  float zi, 
				  float *xo, 
				  float *yo, 
				  float *zo,
				  float st_dev_l, 
				  float st_dev_t,
				  float *st_dev_z,
				  float *st_dev_x,
				  float *st_dev_y,
				  HepRandom quasiRandom)
  {
    // Local Variables

    float err_pad;		//ERROR ALONG PAD ROW FOR SPACE POINT 
    float err_drft;		//ERROR ALONG DRIFT DIRECTION FOR SPACE POINT
    float smear;
    float err_x, err_y;// err_z;

    //-----------------------------------------------------------------------

    // Evaluate the sigmas in x- and y-direction out of the sigmas in long. 
    // and transverse direction
    
    *st_dev_x = sqr(cos(phi)) * sqr(st_dev_l) + sqr(sin(phi)) * sqr(st_dev_t);
    *st_dev_x = (sqrt(*st_dev_x))/10000.;  // microns -> cm

    *st_dev_y = sqr(sin(phi)) * sqr(st_dev_l) + sqr(cos(phi)) * sqr(st_dev_t);
    *st_dev_y = sqrt (*st_dev_y)/10000.;   // microns -> cm

    smear=(float) quasiRandom.flat()-0.5;
    err_pad = st_dev_t*smear;

    smear=(float) quasiRandom.flat()-0.5;
    err_drft = st_dev_l*smear;

    //        err_Z = st_dev_Z*SMEAR

    // Evaluate hit-shift in x- and y-direction as well as the new points xo and yo

    err_x = cos(phi) * err_drft - sin(phi) * err_pad;
    *xo = xi + (err_x/10000);  

    err_y = sin(phi) * err_drft + cos(phi) * err_pad;
    *yo = yi + (err_y/10000);
    
    //        ZO = ZI + err_Z
    *zo = zi;

    return TRUE;
  }

int StFtpcFastSimu::ffs_ini(FFS_FSPAR_ST *ffs_fspar,   
			    FFS_GASPAR_ST *ffs_gaspar)
  {
    //------   TEMPORARY:  put in parameter table   ------------------
    const float phi_origin=90.;
    const float phi_sector=60.;
    //------   TEMPORARY:  put in parameter table   ------------------
    //-----------------------------------------------------------------------
    // mk
    ri = 8;
    ra = 29.8;
    padrows = 10.;
    // mk
    //     pad response function (prf)
	
    prf_wid = ffs_fspar[0].sprf_0[0];
    shaper_wid = ffs_fspar[0].sprf_0[1];

    //mk Drift-Velocity:
    Vhm[0]  = ffs_gaspar[0].vdrift[0];
    Vhm[1]  = ffs_gaspar[0].vdrift[1];
    Vhm[2]  = ffs_gaspar[0].vdrift[2];
    Vhm[3]  = ffs_gaspar[0].vdrift[3];

    //mk Drift_Time
    Tbm[0] = ffs_gaspar[0].tdrift[0];
    Tbm[1] = ffs_gaspar[0].tdrift[1];
    Tbm[2] = ffs_gaspar[0].tdrift[2];
    Tbm[3] = ffs_gaspar[0].tdrift[3];
    
    //mk Radial Sigma
    s_rad[0] = ffs_gaspar[0].sig_rad[0];
    s_rad[1] = ffs_gaspar[0].sig_rad[1];
    s_rad[2] = ffs_gaspar[0].sig_rad[2];
    s_rad[3] = ffs_gaspar[0].sig_rad[3];

    //mk Azimuthal Sigma
    s_azi[0] = ffs_gaspar[0].sig_azi[0];
    s_azi[1] = ffs_gaspar[0].sig_azi[1];
    s_azi[2] = ffs_gaspar[0].sig_azi[2];
    s_azi[3] = ffs_gaspar[0].sig_azi[3];
    
    cout << "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" << endl;
    cout << "Parametrization for vd, Td, sig_rad and sig_azi:" << endl;
    cout << "vd=" << Vhm[0]<<"+"<<Vhm[1]<<"x+"<<Vhm[2]<<"xx+"
	 <<Vhm[3]<<"xxx" << endl;
    cout << "Td="<<Tbm[0]<<"+"<<Tbm[1]<<"x+"<<Tbm[2]<<"xx+"<<Tbm[3]
	 <<"xxx" << endl;
    cout << "sig_rad="<<s_rad[0]<<"+"<<s_rad[1]<<"x+"<<s_rad[2]
	 <<"xx+"<<s_rad[3]<<"xxx" << endl;
    cout << "sig_azi="<<s_azi[0]<<"+"<<s_azi[1]<<"x+"<<s_azi[2]
	 <<"xx+"<<s_azi[3]<<"xxx" << endl;
    cout << "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" << endl;
    
    // if slong <0, use built-in longitudinal resolution from radial diffusion 
    // if slong >=0, longintudinal resolution is constant and equal to slong
    slong  = ffs_fspar[0].sprf_ta[1];
    anode_width = ffs_fspar[0].padwid[1];

    // Drift velocity at anode (Ranode = ra from/ftpc_params/ ) [cm/microsec]
    Va = Vhm[0] + Vhm[1]*ra + Vhm[2]*sqr(ra) + Vhm[3]*ra*sqr(ra);

    //     phi of sector number 1 origin
    phimin = C_RAD_PER_DEG * phi_origin;

    //     size of one sector in phi
    phisec = C_RAD_PER_DEG * phi_sector;

    //     a cluster is too close to lower sector boundary if it is
    //     not more than 2 pads away (160 pads in 60 degrees, 
    //     2 pads = .75 degrees = 0.013089969 rads)
    sector_phi_min = 0.013089969;

    //     a cluster is too close to upper sector boundary if it is
    //     not more than 3 pads away (160 pads in 60 degrees, 
    //     160 -3 pads = 157 pads = 58.875 degrees = 1.0275626 rads)
    sector_phi_max = 1.0275626;

    return TRUE;
  }

int StFtpcFastSimu::ffs_merge_tagger(int *ffs_gepoint_nok,
				     int ffs_gepoint_maxlen,
				     FFS_GEPOINT_ST *ffs_gepoint,
				     int *fcl_fppoint_nok,
				     int fcl_fppoint_maxlen,
				     FCL_FPPOINT_ST *fcl_fppoint)
  {
    // Local Variables:
    int id_1, id_2, rem_count1, rem_count2, n_gepoints;
    float sig_azi_1, v1, sig_rad_1;
    float dist_rad_in, dist_rad_out;
//     float r2, phi2;
    float delta_azi, delta_r;
    float sigazi[MXMROW], sigrad[MXMROW];
    float r1[MXMROW], phi1[MXMROW];

    // Loop Variables
    int h,i,j,k;

    //-----------------------------------------------------------------------

    k=0;
    for(i=0; i<*fcl_fppoint_nok; i++)
      {
	fcl_fppoint[i].flags = 0;
	
	// azimuthal direction
	r1[i] = sqrt(sqr(fcl_fppoint[i].x) + sqr(fcl_fppoint[i].y));
	phi1[i] = atan2((double) fcl_fppoint[i].y,
                        (double) fcl_fppoint[i].x);
	if ( phi1[i] < 0.0 ) 
	  phi1[i] += C_2PI;
	
	sig_azi_1 = s_azi[0] + s_azi[1]*r1[i] + 
	  s_azi[2]*sqr(r1[i]) + s_azi[3]*sqr(r1[i])*r1[i];
	fcl_fppoint[i].s_phi = sig_azi_1/10000;

	sig_azi_1 = (2.5*sig_azi_1)/10000; // micron -> cm
	sigazi[i] = sig_azi_1*(r1[i]/ra);

	// radial direction
	v1 = Vhm[0]+Vhm[1]*r1[i] + Vhm[2]*sqr(r1[i])+
	  Vhm[3]*sqr(r1[i])*r1[i];
	sig_rad_1 = s_rad[0] + s_rad[1]*r1[i] +  
	  s_rad[2]*sqr(r1[i]) + s_rad[3]*sqr(r1[i])*r1[i];

	fcl_fppoint[i].s_r = sig_rad_1/10000;

	sig_rad_1 = (2.5*sig_rad_1)/10000; // micron -> cm
	sigrad[i] = sig_rad_1*(v1/Va);

      }

    for(h=0;h<SIZE;h++)
      {
	if(nrowmax[h]==0) 
	  continue;
	
        for(i=0;i<nrowmax[h];i++)
	  {
	    id_1 = nrow[h][i]-1;
	    
	    for(j=i+1; j<nrowmax[h]; j++)
	      {
		id_2 = nrow[h][j]-1;
 		if((fcl_fppoint[id_2].flags==1000) || 
 		   (fcl_fppoint[id_2].sector!=fcl_fppoint[id_1].sector))
 		  continue;
		
		delta_azi = fabs(phi1[id_1]-phi1[id_2])
		  *((r1[id_1]+r1[id_2])/2);
		delta_r = fabs(r1[id_1]-r1[id_2]);
		
		if((delta_r < (2 * sigrad[id_1])) &&
		   (delta_azi < (2 * sigazi[id_1])))
		  {
		    // mark clusters as unfolded 
		    if(fcl_fppoint[id_1].flags != 1000)
		      {
			fcl_fppoint[id_1].flags = 1;
		      }
		    fcl_fppoint[id_2].flags = 1;
		  }
		
		if((delta_r<sigrad[id_1]) &&
		   (delta_azi<sigazi[id_1]))
		  {
		    k++;
		    
		    // merge clusters, mark second for removal
		    if(fcl_fppoint[id_1].flags != 1000)
		      {
			fcl_fppoint[id_1].flags = 8;
		      }
		    fcl_fppoint[id_2].flags = 1000;
		    fcl_fppoint[id_1].max_adc=fcl_fppoint[id_1].max_adc+
		      fcl_fppoint[id_2].max_adc / 2;
		      // maxadc adds up somehow, maybe more, maybe less
		    fcl_fppoint[id_1].charge=fcl_fppoint[id_1].charge+
		      fcl_fppoint[id_2].charge;
		    // charge adds up exactly
		    fcl_fppoint[id_1].x=(fcl_fppoint[id_1].x+
					 fcl_fppoint[id_2].x) / 2;
		    fcl_fppoint[id_1].y=(fcl_fppoint[id_1].y+
					 fcl_fppoint[id_2].y) / 2;
		    fcl_fppoint[id_1].z=(fcl_fppoint[id_1].z+
					 fcl_fppoint[id_2].z) / 2;
		    // positions average more or less
		    fcl_fppoint[id_1].s_phi=fcl_fppoint[id_1].s_phi+
		      fcl_fppoint[id_2].s_phi / 2;
		    fcl_fppoint[id_1].s_r=fcl_fppoint[id_1].s_r+
		      fcl_fppoint[id_2].s_r / 2;
		    //widths add up somehow...
		  }
	      }
	  }
      }
    
    rem_count1=0;
    rem_count2=0;

    // now remove merged clusters and those on sector border
    cout << "remove merged and cut-off hits" << endl;
    id_1 = 0;
    id_2 = 0;
    n_gepoints = 0;
      
    dist_rad_in = s_rad[0] + s_rad[1]*ri + s_rad[2]*sqr(ri) + 
      s_rad[3]*ri*ri*ri;
    dist_rad_out = s_rad[0] + s_rad[1]*ra + s_rad[2]*sqr(ra) + 
      s_rad[3]*ra*ra*ra;
    // minimum distance in cm = 2*cluster sigma in microns
    dist_rad_in /= 5000;
    dist_rad_out /= 5000;
      
    while(id_2 < *fcl_fppoint_nok)
      {
	delta_azi = phi1[id_2] 
	  -myModulo(((fcl_fppoint[id_2].sector-1)*phisec+phimin),(C_2PI));
	if (delta_azi<0.0) 
	  delta_azi += C_2PI;

	if((delta_azi < sector_phi_min) || 
           (delta_azi > sector_phi_max) ||
	   (r1[id_2] < ri+dist_rad_in) ||
	   (r1[id_2] > ra-dist_rad_out) ||
           (fcl_fppoint[id_2].flags > 256))
	  {
            if(fcl_fppoint[id_2].flags > 256)
	      {
		rem_count1++;
	      }
            else
	      {
		rem_count2++;
	      }
	  }
	else
	  {
	    if ( id_2 == id_1 )
	      {
		id_1++;
		n_gepoints++;
	      }
	    else
	      {
		fcl_fppoint[id_1].row=fcl_fppoint[id_2].row;
		fcl_fppoint[id_1].sector=fcl_fppoint[id_2].sector;
		fcl_fppoint[id_1].n_pads=fcl_fppoint[id_2].n_pads;
		fcl_fppoint[id_1].n_bins=fcl_fppoint[id_2].n_bins;
		fcl_fppoint[id_1].max_adc=fcl_fppoint[id_2].max_adc;
		fcl_fppoint[id_1].charge=fcl_fppoint[id_2].charge;
		fcl_fppoint[id_1].flags=fcl_fppoint[id_2].flags;
		ffs_gepoint[id_1].ge_track_p=ffs_gepoint[id_2].ge_track_p;
		ffs_gepoint[id_1].ge_pid=ffs_gepoint[id_2].ge_pid;
		ffs_gepoint[id_1].prim_tag=ffs_gepoint[id_2].prim_tag;
		fcl_fppoint[id_1].x=fcl_fppoint[id_2].x;
		fcl_fppoint[id_1].y=fcl_fppoint[id_2].y;
		fcl_fppoint[id_1].z=fcl_fppoint[id_2].z;
		fcl_fppoint[id_1].s_phi=fcl_fppoint[id_2].s_phi;
		fcl_fppoint[id_1].s_r=fcl_fppoint[id_2].s_r;
		ffs_gepoint[id_1].p_v[0]=ffs_gepoint[id_2].p_v[0];
		ffs_gepoint[id_1].p_v[1]=ffs_gepoint[id_2].p_v[1];
		ffs_gepoint[id_1].p_v[2]=ffs_gepoint[id_2].p_v[2];
		ffs_gepoint[id_1].p_g[0]=ffs_gepoint[id_2].p_g[0];
		ffs_gepoint[id_1].p_g[1]=ffs_gepoint[id_2].p_g[1];
		ffs_gepoint[id_1].p_g[2]=ffs_gepoint[id_2].p_g[2];
		ffs_gepoint[id_1].vertex[0]=ffs_gepoint[id_2].vertex[0];
		ffs_gepoint[id_1].vertex[1]=ffs_gepoint[id_2].vertex[1];
		ffs_gepoint[id_1].vertex[2]=ffs_gepoint[id_2].vertex[2];
		ffs_gepoint[id_1].ge_proc=ffs_gepoint[id_2].ge_proc;
		id_1++;
		n_gepoints++;
	      }
	  }
	id_2++;
      }
	
    *ffs_gepoint_nok = n_gepoints;
    *fcl_fppoint_nok = n_gepoints;
      
    cout << "Deleted " << rem_count1 << " merged clusters." << endl;
    cout << "Deleted " << rem_count2 << " clusters on sector limit." << endl;
      
    return TRUE;
  }

int StFtpcFastSimu::ffs_tag(int *ffs_gepoint_nok,
			    int ffs_gepoint_maxlen, 
			    FFS_GEPOINT_ST *ffs_gepoint,
			    int *fcl_fppoint_nok,
			    int fcl_fppoint_maxlen,
			    FCL_FPPOINT_ST *fcl_fppoint)
  {
      int i, k, num_nok;
      //-----------------------------------------------------------------------
      //     Tag hits according to row. Up to maximum row number=20

      //   nrowmax(k) is the #of hits in hitplane k
      //   k is the hitplane-# given by gstar

      for(k=0; k<SIZE; k++)
	{
	  nrowmax[k] = 0;
	}

      num_nok=*fcl_fppoint_nok;
      if (num_nok > MXMROW )
	{
	  cout << "FFS WARNING:  fppoint_h.nok (" << *fcl_fppoint_nok
	       << ") greater than mxmrow ("<< MXMROW<<")"<< endl;
	  cout << "              Setting num_nok =  mxmrow ("
	       << MXMROW <<")" << endl;
	  num_nok=MXMROW;
	}
 
      for(i = 0; i< num_nok; i++)
	{
	  k =  fcl_fppoint[i].row;
	  nrowmax[k-1]++;
	  nrow[k-1][nrowmax[k-1]-1] = i+1;
	}

      return TRUE;

  }
