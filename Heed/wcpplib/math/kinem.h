#ifndef KINEM_H
#define KINEM_H

/*
Some kinematics of particles.
Author I.B.Smirnov, 2003
*/

// Scattering of moving projectile of target in rest,
// cos theta as function of incident and final energy of progectile.

double cos_theta_two_part(double Ep0, double Ep1, double Mp, double Mt, 
			double fspeed_of_light);

void theta_two_part(double Ep0, double Ep1, double Mp, double Mt, 
		    double fspeed_of_light,
		    double& theta_p, double& theta_t);


#endif
