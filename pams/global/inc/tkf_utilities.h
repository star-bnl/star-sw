#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>


typedef struct	
	{
		int	id;
		int     mc_id;
		int	nhits;
		int	row;
		double	lr;
		double	r;
		double	rphi;
		double	z;
		double	psi;
		double	tanl;
		double	qR;
		
	}	localTrack;

double		tkf_circle_param(localTrack *, double []);
void		tkf_fine_approach(double [], double [], double [], double []);
double		tkf_inv_mass(int , localTrack *, localTrack *, double [], double []);
void		tkf_project_track(double [], double, double [], double []);
void		tkf_track_mom(localTrack *, int, double []);
localTrack	*tkf_update_track_param(double [], double, double [], localTrack *);
double  	tkf_dca_track(double [], double, double [], localTrack *);
void		tkf_v0_impact_param(double [], double [], double [], double *, int *);
int			tkf_vzero_geom(double, double [], double [], double, double, double [], 
						double []);
double		tkf_v0_dca(double [], double [], double, double, double, double, double []);



/************************************************************/			
/*									*/
/*	tkf_circle_param							*/
/*									*/
/*	Input arguments							*/
/*		trk			Helix parameters of track		*/
/*									*/
/*	Output arguments							*/
/*		x[2]		Coordinates of the centre of circle		*/
/*		r			Radius of circle			*/
/*									*/
/*	Functional Description						*/
/*		Returns the x-y coordinates and the radius of the	*/
/*		circle (projected helix) on the bending plane.		*/
/*									*/
/*	FORTRAN code created 12/91 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*									*/
/************************************************************/			

double	tkf_circle_param(localTrack *trk, double xc[2])
{
	int		iflag;
	double	x, y, p[3], a, yt1, yt2, xt1, xt2, rxp, test, q, r;
	

	/*	Find the radius and the (hit) x-y coordinates	*/
	r = 0;
	r = fabs(1.0/(trk->qR));
	if (trk->r == 0)
	{
		trk->r = 0.01;
	}
	x = 0;
	y = 0;
	
	x = trk->r * cos(trk->rphi/trk->r);
	y = trk->r * sin(trk->rphi/trk->r);
	
	/*	Get the momentum components	*/
	
	iflag = 0;
	tkf_track_mom(trk, iflag, p);
	
	/*	Find the two possible solutions	*/
	
	if (p[0] == 0)
	{
		p[0] = 0.01;
	}
	a = p[1]/p[0];
	yt1 = sqrt((r*r)/(a*a+1.0)) + y;
	yt2 = - sqrt((r*r)/(a*a+1.0)) + y;
	xt1 = -a * (yt1 - y) + x;
	xt2 = -a * (yt2 - y) + x;
	
	/*	Decide which one is the right one	*/
	
	rxp = (xt1 - x) * p[1] - (yt1 - y) * p[0];
	test = rxp * trk->qR;
	if (test > 0)
	{
		xc[0] = xt1;
		xc[1] = yt1;
	}
	else				/*	test <= 0	*/
	{
		xc[0] = xt2;
		xc[1] = yt2;
	}
	return r;
}


/************************************************************/			
/*															*/
/*	tkf_fine_approach										*/
/*															*/
/*	Input arguments											*/
/*		xv		3-dimensional position vector of the main	*/
/*				vertex										*/
/*		x		3-dimensional position vector of the		*/
/*				secondary track								*/
/*		p		3-dimensional momentum vector of the		*/
/*				reconstructed secondary particle			*/
/*															*/		
/*	Output arguments										*/
/*		x0		Coordinates of closest approach				*/
/*															*/
/*	Functional Description									*/
/*		Finds the coordinates of the point of closest 		*/
/*		approach from the main vertex. The input vectors	*/
/*		must be calculated at a point close to a vertex.	*/
/*															*/
/*	FORTRAN code created 6/92 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*															*/
/************************************************************/

void	tkf_fine_approach(double xv[3], double x[3], double p[3], double x0[3])
{
	double nom;
	
	nom = (p[1]*p[1] + p[2]*p[2]) * x[0] - (x[1]*p[1] + x[2]*p[2]) * p[0];
	nom = nom + (xv[0]*p[0] + xv[1]*p[1] + xv[2]*p[2]) * p[0];
	x0[0] = nom/(p[0]*p[0] + p[1]*p[1] + p[2]*p[2]);
	
	if (p[0] != 0)
	{
		x0[1] = x[1] + (p[1]/p[0]) * (x0[0] - x[0]);
		x0[2] = x[2] + (p[2]/p[0]) * (x0[0] - x[0]);
	}
	else
	{
		x0[2] = p[1] * (x[2]*p[1] - x[1]*p[2]) / (p[1]*p[1] + p[2]*p[2]);
		if (p[1] == 0)
		{
			x0[1] = x[1];
		}
		else
		{
			x0[1] = -x0[2]*p[2] / p[1];
		}
	}
}

/************************************************************/			
/*															*/
/*	tkf_inv_mass											*/
/*															*/
/*	Input arguments											*/
/*		trk[6]	Helix parameters of the two tracks at the	*/
/*				(secondary) vertex position. If one of 		*/
/*				them is neutral it should be the second		*/
/*				and the first 3 components of trk hold the	*/
/*				momentum vector.							*/
/*		m[2]	Mass hypothesis for the daughter 			*/
/*				particles.									*/
/*		iflag	if 0 then both particles are charged		*/
/*				if not 0 then one particle is neutral		*/
/*															*/		
/*	Output arguments										*/
/*		p[3]		Momentum vector of reconstructed 		*/
/*					particle.								*/
/*		inv_mass	Mass of reconstructed particle			*/
/*															*/
/*	Functional Description									*/
/*		Invariant mass analysis for particle decays into 	*/
/*		two daughter particles with a mass hypothesis.		*/
/*															*/
/*	FORTRAN code created 12/91 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*															*/
/************************************************************/

double	tkf_inv_mass(int iflag, localTrack *trk1, localTrack *trk2, double m[2], 
						double p[3])
{
	double		phi1, phi2;
	double		lambda1, lambda2, cl1, cl2, sl1, sl2;
	double		sph1, sph2, cph1, cph2, pt1, pt2, p1, p2, e1, e2;
	double		px1, px2, py1, py2, pz1, pz2, psq;
	double		mass;
	
	const double		alpha = 0.299792458;
	const double		beta = 5.0;
	
	/*	Extract useful parameters from helix	*/
	
	lambda1 = atan(trk1->tanl);
	cl1 = cos(lambda1);
	sl1 = sin(lambda1);
	phi1 = trk1->psi;
	cph1 = cos(phi1);
	sph1 = sin(phi1);
	pt1 = fabs((beta * alpha) / (1000.0 *(trk1->qR)));
	pz1 = pt1 * trk1->tanl;
	p1 = sqrt(pt1*pt1 + pz1*pz1);
	e1 = sqrt(p1*p1 + m[0]*m[0]);
	px1 = p1 * cl1 * cph1;
	py1 = p1 * cl1 * sph1;
	
	/*	Second track	*/
	
	if (iflag = 0)
	{
		lambda2 = atan(trk2->tanl);
		cl2 = cos(lambda2);
		sl2 = sin(lambda2);
		phi2 = trk2->psi;
		cph2 = cos(phi2);
		sph2 = sin(phi2);
		pt2 = fabs((beta * alpha) / (1000.0 *(trk2->qR)));
		pz2 = pt2 * trk2->tanl;
		p2 = sqrt(pt2*pt2 + pz2*pz2);
		e2 = sqrt(p2*p2 + m[1]*m[1]);
		px2 = p2 * cl2 * cph2;
		py2 = p2 * cl2 * sph2;
	}
	else	/*	if (iflag == 0)	*/
	{
		px2 = trk2->r;
		py2 = trk2->rphi;
		pz2 = trk2->z;
		p2 = sqrt(px2*px2 + py2*py2 + pz2*pz2);
		e2 = sqrt(p2*p2 +m[1]*m[1]);
	}
	
	/*	Calculate the momentum compnents of the primary	*/
	
	p[0] = px1 + px2;
	p[1] = py1 + py2;
	p[2] = pz1 + pz2;
	
	/*	Calculate the invariant mass	*/
	
	psq = p[0]*p[0] + p[1]*p[1] + p[2]*p[2];
	mass = sqrt(pow((e1 + e2), 2) - psq);
	return mass;
}
	
/************************************************************/			
/*															*/
/*	tkf_project_track										*/
/*															*/
/*	Input arguments											*/
/*		xc[2]	Position coordinate in x-y plane of the		*/
/*				center of the circle (charged particle)  	*/
/*		r		Radius of the circle 						*/
/*		xp[2]	x-y coordinate of new point					*/
/*															*/		
/*	Output arguments										*/
/*		x[2]	x-y coordinate of point of closest approach */
/*															*/
/*	Functional Description									*/
/*		Extrapolates a helix and calculates the coordinate 	*/
/*		of the point of the closest approach from a given	*/
/*		point in the beinding plane.						*/
/*															*/
/*	FORTRAN code created 1/92 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*															*/
/************************************************************/

void	tkf_project_track(double xc[2], double r, double xp[2], double x[2])
{
	double	x1[2], x2[2], yy1, yy2, zz1, zz2;
	double	a, b, c;
	
	/*	First find the two possible solutions for closest approach	*/
	
	a = xc[0] - xp[0];
	b = xc[1] - xp[1];
	
	if (b == 0.0)
	{
		x1[1] = xc[1];
		x2[1] = xc[1];
		x1[0] = xc[0] + r;
		x2[0] = xc[0] - r;
	}
	else		/*	if (b != 0)	*/
	{
		c = a / b;
		yy1 = r / sqrt(c*c + 1.0);
		yy2 = -r / sqrt(c*c + 1.0);
		zz1 = c * yy1;
		zz2 = c * yy2;
		x1[0] = zz1 + xc[0];
		x2[0] = zz2 + xc[0];
		x1[1] = yy1 + xc[1];
		x2[1] = yy2 + xc[1];
	}
	
	/*	Choose the right one	*/
	
	a = (xp[0] - xc[0]) * (x1[0] - xc[0]) + (xp[1] - xc[1]) * (x1[1] - xc[1]);
	if (a > 0.)
	{
		x[0] = x1[0];
		x[1] = x1[1];
	}
	else		/*	if (a <= 0)	*/
	{
		x[0] = x2[0];
		x[1] = x2[1];
	}
}

/************************************************************/			
/*															*/
/*	tkf_track_mom											*/
/*															*/
/*	Input arguments											*/
/*		trk[6]	Helix parameters of the track				*/
/*		flag	if iflag = 5 it is a neutral particle and  	*/
/*				trk[6] = 1.0 / momentum of particle			*/
/*															*/		
/*	Output arguments										*/
/*		p[3]	momentum of track	 						*/
/*															*/
/*	Functional Description									*/
/*		Returns the momentum components of a track		 	*/
/*															*/
/*	FORTRAN code created 12/91 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*															*/
/************************************************************/

void	tkf_track_mom(localTrack *trk, int flag, double p[3])
{
	double		phi;
	double		lambda, cl;
	double		sph, cph, pt, ptot;
	
	const double		alpha = 0.299792458;
	const double		beta = 5.0;					/* beta (field) in kGauss */
	
	p[0] = 0;
	p[1] = 0;
	p[2] = 0;
	
	/*	Extract momentum components from helix	*/
	
	lambda = atan(trk->tanl);
	cl = cos(lambda);
	phi = trk->psi;
	cph = cos(phi);
	sph = sin(phi);
	
	/*	Check if it is a neutral particle	*/
	
	if (flag == 5)
	{
		ptot = 1.0 / trk->qR;
		pt = ptot * cl;
		p[2] = pt * trk->tanl;
	}
	else			/* if (flag != 5)	*/
	{
		pt = fabs((beta*alpha) / (1000.0 * (trk->qR)));
		p[2] = pt * trk->tanl;
		ptot = sqrt(pt*pt + p[2]*p[2]);
	}
	
	p[0] = ptot * cl * cph;
	p[1] = ptot * cl * sph;
}

/************************************************************/			
/*															*/
/*	tkf_update_track_param									*/
/*															*/
/*	Input arguments											*/
/*		xc[2]	Position coordinate in x-y plane of the		*/
/*				center of the circle (charged particle)  	*/
/*		r		Radius of the circle 						*/
/*		x[2]	x-y coordinate of new point					*/
/*		trk1	old track parameters						*/
/*															*/		
/*	Output arguments										*/
/*		trk2	Updated track parameters				    */
/*															*/
/*	Functional Description									*/
/*		Extrapolates a helix and calculates the new helix 	*/
/*		parameters at another point.						*/
/*															*/
/*	FORTRAN code created 1/92 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*															*/
/************************************************************/

localTrack	*tkf_update_track_param(double xc[2], double r, double x[2], localTrack *trk1)
{

	double xi,yi,dphi,ds;
    double axb,dz,arg;
    
    localTrack	*trk2;

	/* Calculate the distance ds and the angle between old/new position */

	trk2 = (localTrack	*) malloc(sizeof(localTrack)); 

    xi = trk1->r * cos(trk1->rphi/trk1->r);
    yi = trk1->r * sin(trk1->rphi/trk1->r);
    axb = ( xi-xc[0])*(x[1]-xc[1]) - (yi-xc[1])*(x[0]-xc[0]);
    arg = axb / (r*r);
    if( arg >= 1.0 )
    {
    	arg = 1.0;
    }
    if( arg <= -1.0 )
    {
    	arg = -1.0;
    }
    dphi = asin(arg);
    ds = dphi * r;
    dz = ds * trk1->tanl;

	/* Calculate the new track parameters */
      
    trk2->r = sqrt(x[0]*x[0]+x[1]*x[1]);
    if (x[0] == 0) 
    {
    	x[0] = 0.01;
    }
    trk2->rphi = atan2( x[1],x[0] ) * trk2->r;
    if (trk1->qR == 0.)
    {
    	trk1->qR = 0.001;
    }
    
    if (trk1->qR > 0)
    {
    	trk2->z = trk1->z - dz;
    }
    else
    {
    	trk2->z = trk1->z + dz;
    }
    
    trk2->psi = trk1->psi + dphi;
    trk2->tanl = trk1->tanl;
    trk2->qR = trk1->qR;
    trk2->id = trk1->id;
    trk2->mc_id = trk1->mc_id;
    trk2->nhits = trk1->nhits;
    trk2->row = trk1->row;
    trk2->lr = trk1->lr;

    return trk2;
}


double	tkf_dca_track(double xc[2], double r, double x[2], localTrack *trk1)
{

    double xi,yi,dphi,ds;
    double axb,dz,arg,dca;
    double xx,yy,zz;

	/* Calculate the distance ds and the angle between old/new position */


    xi = trk1->r * cos(trk1->rphi/trk1->r);
    yi = trk1->r * sin(trk1->rphi/trk1->r);
    axb = ( xi-xc[0])*(x[1]-xc[1]) - (yi-xc[1])*(x[0]-xc[0]);
    arg = axb / (r*r);
    if( arg >= 1.0 )
    {
    	arg = 1.0;
    }
    if( arg <= -1.0 )
    {
    	arg = -1.0;
    }
    dphi = asin(arg);
    ds = dphi * r;
    dz = ds * trk1->tanl;

	/* Calculate the new track parameters */
      
    xx = sqrt(x[0]*x[0]+x[1]*x[1]);
    if (x[0] == 0) 
    {
    	x[0] = 0.01;
    }
    yy = atan2( x[1],x[0] ) * xx;
    if (trk1->qR == 0)
    {
    	trk1->qR = 0.001;
    }
    
    if (trk1->qR > 0)
    {
    	zz = trk1->z - dz;
    }
    else
    {
    	zz = trk1->z + dz;
    }
    
    dca = sqrt( xx*xx + zz*zz );
    
    return dca;
}

/************************************************************/			
/*															*/
/*	tkf_v0_impact_param										*/
/*															*/
/*	Input arguments											*/
/*		mv[3]      	3-dimensional main vertex position		*/
/*		x[3]		3-dimensional position vector of the  	*/
/*					secondary vertex						*/
/*		p[3]		3-dimensional momentum vector of the	*/
/*					reconstructed secondary particle.		*/
/*															*/		
/*	Output arguments										*/
/*		r_min		Distance from the main vertex at the	*/
/*					point of closest approach				*/
/*		iflag		if iflag == 2 wrong pointing/false		*/
/*					particle								*/
/*															*/
/*	Functional Description									*/
/*		It checks first if the candidate particle points 	*/
/*		away from the vertex (as it should). Then it finds	*/
/*		the impact parameter of a NEUTRAL reconstructed 	*/
/*		particle from the main vertex.						*/
/*															*/
/*	FORTRAN code created 12/91 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*															*/
/************************************************************/

void	tkf_v0_impact_param(double mv[3],double x[3],double p[3],double *r_min,int *iflag)
{
	double 	check;
      
	/*	Assume a good candidate, reset flag	*/

	*iflag = 0;

	/*	Check first if the candidate flies away from vertex through	*/
	/*	the dot product of position and momentum vector.			*/

	check = (x[0]-mv[0])*p[0] + (x[1]-mv[1])*p[1] + (x[2]-mv[2])*p[2];
	if (check < 0.0)
	{
		*iflag = 2;
	}
	else 		/* if (check < 0)	*/
	{
	
		/* Calculate the impact parameter of the track from vetex	*/
		/* normal way using vector-cross-product.					*/

		*r_min = pow(((x[1]-mv[1])*p[2] - p[1]*(x[2]-mv[2])), 2) + 
			pow(((x[2]-mv[2])*p[0]-p[2]*(x[0]-mv[0])), 2) +
			pow(((x[0]-mv[0])*p[1]-p[0]*(x[1]-mv[1])), 2);
	
		*r_min = (*r_min)/(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
		*r_min = sqrt(*r_min);
	}
}

/************************************************************/			
/*															*/
/*	tkf_vzero_geom											*/
/*															*/
/*	Input arguments											*/
/*		xc1[2]      centre x-y coord. of the first circle	*/
/*		xc2[2]      centre x-y coord. of the second circle  */
/*		r1			radius of first circle					*/
/*		r2			radius of second circle					*/
/*															*/		
/*	Output arguments										*/
/*		x[2]		x-coord. of two possible solutions		*/
/*		y[2]		y-coord. of two possible solutions		*/
/*		iflag		if iflag == 5 no solution, iflag == 3	*/
/*					if only one solution					*/
/*															*/
/*	Functional Description									*/
/*		It finds all possible intersections in the			*/
/*		bending plane of two circles.						*/
/*															*/
/*	FORTRAN code created 12/91 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*															*/
/************************************************************/

int	tkf_vzero_geom(double cut, double xc1[2], double xc2[2], double r1, double r2, 
						double x[2], double y[2])
{
	double	om1, om2, ph1, ph2;
	double	a, b, c, d, dia, dtouch;
	
	int		flag;
      
	/*	Clear variables	*/
	
	x[0] = 0.;
	x[1] = 0.;
	y[0] = 0.;
	y[1] = 0.;
        flag = 0;

	/*	Find the two intersections	*/
	
	a = xc1[0]-xc2[0];
	b = xc1[1]-xc2[1];
	dia = sqrt(a*a + b*b);
	dtouch = dia - r1 - r2;
	c = (r1*r1 - r2*r2 + a*a + b*b)/2.0;
	d = (a*a + b*b)*r1*r1 - c*c;
	
	/*	Check if there is any solution, one or two	*/
	
	if (d < 0.0)
	{
		if (dtouch <= cut)
        {
        	flag = 3;
        	x[0] = xc1[0] + r1*(xc2[0]-xc1[0])/dia;
        	y[0] = xc1[1] + r1*(xc2[1]-xc1[1])/dia;
        	x[1] = x[0];
        	y[1] = y[0];
      	}
      	else
      	{
        	flag = 5;
        }
	}
	else
	{
		if (d == 0.0) 
		{
			flag = 3;
		}
		om1	= ( -b*c+fabs(a*sqrt(d)) ) / (a*a+b*b);
		om2	= ( -b*c-fabs(a*sqrt(d)) ) / (a*a+b*b);

		/*	Find the right pair	*/

		if ( (r1*r1-om1*om1) >= 0.0)
		{
			ph1 = sqrt(r1*r1-om1*om1);
			ph2 = -ph1;
			if ( fabs(pow((ph1+a), 2)+pow((om1+b), 2)-r2*r2) <= fabs(pow((ph2+a), 2)
					+ pow((om1+b), 2) - r2*r2) )
			{
				y[0] = om1+xc1[1];
				x[0] = ph1+xc1[0];
			}
			else
			{
				y[0] = om1+xc1[1];
				x[0] = ph2+xc1[0];
			}	
		}

		/*	Second pair	*/

		if ( (r1*r1-om2*om2) >= 0.0)
		{
			ph1 = sqrt(r1*r1-om2*om2);
			ph2 = -ph1;
			if ( fabs(pow((ph1+a), 2) + pow((om2+b), 2) - r2*r2) <= fabs(pow((ph2+a), 2) + 
					pow((om2+b), 2) - r2*r2) )
			{
				y[1] = om2 + xc1[1];
				x[1] = ph1 + xc1[0];
			}
			else
			{
				y[1] = om2 + xc1[1];
				x[1] = ph2 + xc1[0];
			}	
		}
	}
	return flag;
}

/************************************************************/			
/*															*/
/*	tkf_v0_dca												*/
/*															*/
/*	Input arguments											*/
/*		xn1[3]      Coordinates of first track				*/
/*		xn2[3]      Coordinates of second track				*/
/*															*/		
/*	Output arguments										*/
/*		xv0[3]		Coordinates of seed vertex at point of 	*/
/*					closest approach						*/
/*		v0dca		dca between the two tracks				*/
/*															*/
/*	Functional Description									*/
/*		Calculates the (3D) distance of closest approach	*/
/*		of two tracks (vectors) as well as the 'seed'		*/
/*		vertex coordinates.									*/
/*															*/
/*	FORTRAN code created 7/95 by Spiros Margetis			*/
/*	Converted to C 10/95 by Scott Tooker					*/
/*															*/
/************************************************************/

double	tkf_v0_dca(double xn1[3], double xn2[3], double sxz1, double syz1, double sxz2, 
					double syz2, double xv0[3])
{
	double	x1, x2, y1, y2, z1, z2;
	double	dx, dy, dz;
	double 	a1, a2, a3, c, k, l, m, b, v, A, Bb, Cc, D, E, F;
	double	x, y, z, xp, yp, zp;
	
	double	v0dca;
	
	x1 = xn1[0];
	y1 = xn1[1];
	z1 = xn1[2];
    x2 = xn2[0];
	y2 = xn2[1];
    z2 = xn2[2];
    dx = x1-x2;
    dy = y1-y2;
	dz = z1-z2;
	a1 = syz1-syz2;
    a2 = sxz2-sxz1;
    a3 = sxz1*syz2-syz1*sxz2;

    v0dca = dx*a1 + dy*a2 + dz*a3;
    v0dca = v0dca / sqrt(a1*a1 + a2*a2 + a3*a3);

	if ( (syz1 != 0) && (syz2 != 0) )
	{
		c = sxz1/syz1;
        k = (sxz1*y1)/syz1 - x1;
        l = y1/syz1 - z1;
        b = sxz2/syz2;
        m = (sxz2*y2)/syz2 - x2;
        v = y2/syz2 - z2;
        A = c*a2-a1;
        Bb = b*a2-a1;
        Cc = a2*(k-m);
        D = a2/syz1 - a3;
        E = a2/syz2 - a3;
        F = a2*(l-v);
      
        yp = (A*F - Cc*D)/(D*Bb - A*E);
        y = (Bb*F - Cc*E)/(D*Bb - A*E);
        xv0[1] = (y+yp)/2.0;

        zp = yp/syz2 - v;
        z = y/syz1 - l;
        xv0[2] = (z+zp)/2.0;

        xp = b*yp - m;
        x = c*y  - k;
        xv0[0] = (x+xp)/2.0;
	}
	else
	{
        xv0[0] = 0.001;
        xv0[1] = 0.001;
        xv0[2] = 0.001;
	}
	
	return v0dca;
}
