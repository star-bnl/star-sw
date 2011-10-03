/***************************************************************************
 *
 * $Id: denlan.c,v 1.1.1.1 1999/09/08 17:34:27 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Aug 1999
 * Description:  Function to fit Landau distribution
 * Adapted from fortran cernlib routine by mclareni
 *
 * $Log: denlan.c,v $
 * Revision 1.1.1.1  1999/09/08 17:34:27  snelling
 * added documentation
 *
 *  
 **************************************************************************/
#include <math.h>

double denlan(double a, double x, double b, double c) {

  const float p1[5] = {0.4259894875E+0, -0.1249762550E+0, 0.3984243700E-1,
		       -0.6298287635E-2, 0.1511162253E-2};
  const float p2[5] = {0.1788541609E+0, 0.1173957403E+0, 0.1488850518E-1,
		       -0.1394989411E-2, 0.1283617211E-3};
  const float p3[5] = {0.1788544503E+0, 0.9359161662E-1, 0.6325387654E-2,
		       0.6611667319E-4, -0.2031049101E-5};
  const float p4[5] = {0.9874054407E+0, 0.1186723273E+3, 0.8492794360E+3,
		       -0.7437792444E+3, 0.4270262186E+3};
  const float p5[5] = {0.1003675074E+1, 0.1675702434E+3, 0.4789711289E+4,
		       0.2121786767E+5, -0.2232494910E+5};
  const float p6[5] = {0.1000827619E+1, 0.6649143136E+3, 0.6297292665E+5,
		       0.4755546998E+6, -0.5743609109E+7};

  const float q1[5] = {1.0, -0.3388260629E+0, 0.9594393323E-1,
		       -0.1608042283E-1, 0.3778942063E-2};
  const float q2[5] = {1.0, 0.7428795082E+0, 0.3153932961E+0,
		       0.6694219548E-1, 0.8790609714E-2};
  const float q3[5] = {1.0, 0.6097809921E+0, 0.2560616665E+0,
		       0.4746722384E-1, 0.6957301675E-2};
  const float q4[5] = {1.0, 0.1068615961E+3, 0.3376496214E+3,
		       0.2016712389E+4, 0.1597063511E+4};
  const float q5[5] = {1.0, 0.1569424537E+3, 0.3745310488E+4,
		       0.9834698876E+4, 0.6692428357E+5};
  const float q6[5] = {1.0, 0.6514101098E+3, 0.5697473333E+5,
		       0.1659174725E+6, -0.2815759939E+7};

  const float a1[3] = {0.4166666667E-1,-0.1996527778E-1, 0.2709538966E-1};
  const float a2[2] = {-0.1845568670E+1,-0.4284640743E+1};

  double landau, u;
  double v = a*x + b;

  if (v < -5.5) {
    u = exp(v + 1.0);
    landau = 0.3989422803 * (exp(-1/u)/sqrt(u))*
      (1+(a1[1]+(a1[2]+a1[3]*u)*u)*u);
    return c * landau;
  }
  else if (v < -1.) {
    u = exp(-v - 1.);
    landau = exp(-u) * sqrt(u) *
      (p1[0] + (p1[1] + (p1[2] + (p1[3] + p1[4] * v) * v) * v) * v) /
      (q1[0] + (q1[1] + (q1[2] + (q1[3] + q1[4] * v) * v) * v) * v);
    return c * landau;
  }
  else if (v < 1.) {
    landau = (p2[0] + (p2[1] + (p2[2] + (p2[3] + p2[4] * v) * v) * v) * v) /
      (q2[0] + (q2[1] + (q2[2] + (q2[3] + q2[4] * v) * v) * v) * v);
    return c * landau;
  }
  else if (v < 5.) {
    landau = (p3[0] + (p3[1] + (p3[2] + (p3[3] + p3[4] * v) * v) * v) * v) /
      (q3[0] + (q3[1] + (q3[2] + (q3[3] + q3[4] * v) * v) * v) * v);
    return c * landau;
  }
  else if (v < 12.) {
    u = 1/v;
    landau = u * u * (p4[0] + (p4[1] + (p4[2] + (p4[3] + p4[4] * u) 
					* u) * u) *u) /
      (q4[0] + (q4[1] + (q4[2] + (q4[3] + q4[4] * u) * u) * u) * u);
    return c * landau;
  }
  else if (v < 50.) {
    u = 1/v;
    landau = u * u * (p5[0] + (p5[1] + (p5[2] + (p5[3] + p5[4] * u) 
					* u) * u) * u) /
      (q5[0] + (q5[1] + (q5[2] + (q5[3] + q5[4] * u) * u) * u) * u);
    return c * landau;
  }
  else if (v < 300.) {
    u = 1/v;
    landau = u * u * (p6[0] + (p6[1] + (p6[2] + (p6[3] + p6[4] * u) 
					* u) * u) * u) /
      (q6[0] + (q6[1] + (q6[2] + (q6[3] + q6[4] * u) * u) * u) * u);
    return c * landau;
  }
  else {
    u = 1 / (v- v * log(v) / (v+1));
    landau = u * u * (1 + (a2[1] + a2[2] * u) * u);
    return c * landau;
  }
}
