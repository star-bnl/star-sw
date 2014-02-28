#ifndef KINEM_H
#define KINEM_H

/*
Some kinematics of particles.
Author I.B.Smirnov, 2003
*/

namespace Heed {

// Scattering of moving projectile of target in rest,
// cos theta as function of incident and final energy of progectile.

double cos_theta_two_part(double Ep0, double Ep1, double Mp, double Mt);

void theta_two_part(double Ep0, double Ep1, double Mp, double Mt,
                    double& theta_p, double& theta_t);

}

#endif
