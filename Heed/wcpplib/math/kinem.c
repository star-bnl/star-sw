#ifdef VISUAL_STUDIO
#define _USE_MATH_DEFINES
// see comment in math.h:
/* Define _USE_MATH_DEFINES before including math.h to expose these macro
 * definitions for common math constants.  These are placed under an #ifdef
 * since these commonly-defined names are not part of the C/C++ standards.
 */
#endif
#include <math.h>
#include "wcpplib/math/kinem.h"
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"

double cos_theta_two_part(double Ep0, double Ep1, double Mp, double Mt, 
			  double fspeed_of_light)
{
  mfunname("double cos_theta_two_part(...)");
	
  double fsp2 = fspeed_of_light * fspeed_of_light;
  double Mp2 = Mp * Mp * fsp2 * fsp2 ;
  Mt *=  fsp2;
  double d0 = Ep0 * Ep0 - Mp2;
  check_econd11(d0 , <= 0 , mcerr);
  double d1 = Ep1 * Ep1 - Mp2;
  check_econd11(d1 , <= 0 , mcerr);
  double r = (-2.0 * Ep0 * Mt + 2.0 * Ep0 * Ep1 + 2.0 * Mt * Ep1 - 2.0 * Mp2)/
  (2.0 * sqrt(d0) * sqrt(d1));
  return r;
}

void theta_two_part(double Ep0, double Ep1, double Mp, double Mt, 
		    double fspeed_of_light,
		    double& theta_p, double& theta_t)
{
  mfunname("void theta_two_part(...)");

  double fsp2 = fspeed_of_light * fspeed_of_light;
  double Mp2 = Mp * Mp * fsp2 * fsp2 ;
  Mt *=  fsp2;
  double d0 = Ep0 * Ep0 - Mp2;
  check_econd11(d0 , <= 0 , mcerr);
  double d1 = Ep1 * Ep1 - Mp2;
  check_econd11(d1 , <= 0 , mcerr);
  double ctheta = 
    (-2.0 * Ep0 * Mt + 2.0 * Ep0 * Ep1 + 2.0 * Mt * Ep1 - 2.0 * Mp2)/
    (2.0 * sqrt(d0) * sqrt(d1));
  if(ctheta < -1.0) ctheta = -1.0;
  if(ctheta >  1.0) ctheta =  1.0;
  theta_p = acos(ctheta);
  //Iprintn(mcout, theta_p);
  if(theta_p == 0.0)
    theta_t = 0.5 * M_PI;
  else
  {
    double Pp1 = Ep1 * Ep1 - Mp2;
    check_econd11(Pp1 , < 0 , mcerr);
    if(Pp1 != 0.0)
    {
      Pp1 = sqrt(Pp1);
      //Iprintn(mcout, Pp1);
      double d3 = Ep0 + Mt - Ep1;
      double dd1 = d3 * d3 - Mt * Mt;
      check_econd11(dd1 , <= 0 , mcerr);
      double dd2 = sqrt(dd1);
      check_econd11(dd2 , <= 0 , mcerr);
      //Iprintn(mcout, dd2);
      //Iprintn(mcout, sin(theta_p));
      double stheta_t = - Pp1 * (sin(theta_p) / dd2);
      //Iprintn(mcout, stheta_t);
      if(stheta_t < -1.0) stheta_t = -1.0;
      if(stheta_t >  1.0) stheta_t =  1.0;
      theta_t = asin(stheta_t);
      //Iprintn(mcout, theta_t);
    }
    else
    {
      theta_t = 0.5 * M_PI;
    }
  }
}

