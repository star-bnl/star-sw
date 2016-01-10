#include <cmath>
#include <fstream>
#include <iomanip>
#include "wcpplib/stream/findmark.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/math/tline.h"
#include "wcpplib/geometry/vfloat.h"
#include "heed++/code/PhotoAbsCS.h"
#include "heed++/code/EnergyMesh.h"  // From this used only make_log_mesh_ec
                                     /*
2004, I. Smirnov
*/

//#define DEBUG_PRINT_get_escape_particles
//#define DEBUG_ignore_non_standard_channels

//#define ALWAYS_LINEAR_INTERPOLATION  // how the paper was computed

namespace Heed {

int sign_nonlinear_interpolation(double e1, double cs1, double e2, double cs2,
                                 double threshold) {
#ifdef ALWAYS_LINEAR_INTERPOLATION
  return 0;
#else
  // normal case:
  if (cs2 >= cs1 || cs2 <= 0 || e1 < 300.0e-6 || e1 < 1.5 * threshold) {
    //if(cs2 >= cs1 || cs2 <= 0)  // for debug
    return 0;
  } else {
    const double pw = log(cs1 / cs2) / log(e1 / e2);
    //Iprintn(mcout, pw);
    if (pw >= -1.0) {
      // good case for linear interpolation
      return 0;
    } else if (pw < -5.0) {
      // unclear odd case, power would be dangerous
      return 0;
    } else {
      // non-linear interpolation
      return 1;
    }
  }
  return 0;
#endif
}

double my_integr_fun(double xp1, double yp1, double xp2, double yp2,
                     double xmin, double /*xmax*/, double x1, double x2) {
  double res = 0.;
  if (sign_nonlinear_interpolation(xp1, yp1, xp2, yp2, xmin) == 1) {
    res = t_integ_power_2point<double>(xp1, yp1, xp2, yp2, x1, x2);
  } else {
    res = t_integ_straight_2point<double>(xp1, yp1, xp2, yp2, x1, x2, 0, 1);
  }
  return res;
}

double my_val_fun(double xp1, double yp1, double xp2, double yp2, double xmin,
                  double /*xmax*/, double x) {
  double res = 0.;
  if (sign_nonlinear_interpolation(xp1, yp1, xp2, yp2, xmin) == 1) {
    // Non-linear interpolation
    res = t_value_power_2point<double>(xp1, yp1, xp2, yp2, x);
  } else {
    // Linear interpolation
    res = t_value_straight_2point<double>(xp1, yp1, xp2, yp2, x, 1);
  }
  return res;
}

double glin_integ_ar(DynLinArr<double> x, DynLinArr<double> y, long q,
                     double x1, double x2, double threshold) {
  // Fit table by a straight line and integrate the area below it.
  mfunname("double glin_integ_ar(DynLinArr< double > x ...)");

  PointCoorMesh<double, DynLinArr<double> > pcmd(q, &x);
  double s = t_integ_generic_point_ar<
      double, DynLinArr<double>, PointCoorMesh<double, DynLinArr<double> > >(
      pcmd, y, &my_integr_fun, x1, x2, 1, threshold, 0, DBL_MAX);
  /*
  // compare with old version
  double old_res = old_glin_integ_ar(x, y, q, x1, x2, threshold);
  if (!apeq(s, old_res)) {
    Iprint2n(mcout, s, old_res);
    Iprint4n(mcout, q, x1, x2, threshold);
    mcout<<"x,y:\n";
    for (long n = 0; n < q; ++n) {
      Iprint3n(mcout, n, x[n], y[n]);
    }
    spexit(mcerr);
  }
  */
  return s;
}

/*
double glin_val_ar(DynLinArr<double> x, DynLinArr<double> y, long q, double ex,
double threshold) {
  // fit table by a straight line and integrate the area below it.
  mfunname("double glin_val_ar(DynLinArr< double > x ...)");

  PointCoorMesh< double, DynLinArr< double > > pcmd( q, &x);
  double s = t_value_generic_point_ar<double, DynLinArr<double>,
      PointCoorMesh<double, DynLinArr<double> >  >
      (pcmd, y, &my_val_fun, ex
       1, threshold, 0, DBL_MAX);
  return s;
}
*/
/*
double old_glin_integ_ar(DynLinArr< double > x, DynLinArr< double > y,
                         long q, double x1, double x2,
                         double threshold )
// fit table by a straight line and integrate the area below it.
{
  mfunname("double old_glin_integ_ar(DynLinArr< double > x ...)");
  //mcout<<"old_glin: q="<<q<<" x1="<<x1<<" x2="<<x2
  //     <<" threshold="<<threshold<<'\n';
  long i;
  double a,b;
  double s=0;
  if(q<=0)return 0;
  check_econd11(threshold , <= 0.0, mcerr); // to avoid the use for
  // which it is not designed.
  // The table is extrapolated to threshold.
  // Therefore it should have valid nonzero value.
  if( x2 < x1 || x2 < threshold || x1 > x[q-1] ) return 0;
  if(x1 < threshold) x1 = threshold;
  if(x2 > x[q-1]) x2 = x[q-1];
  // to find n1 and n2: indexes of next points after x1 and x2
  long n1,n2;
  for(i=0; i<q; i++ )
    if(x[i] > x1 )
    {        n1=i; break; }
  n2=q-1;
  for(i=n1; i<q; i++ )
    if( x[i] >= x2 )
    {        n2=i; break; }
  //Iprintn(mcout, n2);
  long nr;  // index of current interval
  if( x1 < x[0] )
    //{        xt1=x[0]; nr=0; }
  {
    nr=0;
    if(n2 == 0) n2 = 1;  // new correction
  }
  else
  {        nr=n1-1; }
  double xr1,xr2; // limits of integration at the current step
  xr2=x1;
  //Iprintn(mcout, n2);
  //Iprintn(mcout, q);
  //Iprint3n(mcout, nr, n2, xr2);
  for( ; nr<n2; nr++)
  {

    //for debug:
    check_econd12(nr , > , q-2 , mcerr);

    //if(nr>q-2)
    //{
    //cerr<<"step_integ_ar:wrong nr=",nr,"\n";
    //exit(1);
    //}

    xr1=xr2;
    if( nr < q-1 )
      xr2 = (x2 < x[nr+1]) ? x2 : x[nr+1];
    else
      xr2 = x2;
    if(sign_nonlinear_interpolation(x[nr], y[nr],
                                    x[nr+1], y[nr+1], threshold) == 1)
    {
      double pw = log(y[nr]/y[nr+1]) / log(x[nr]/x[nr+1]);
      check_econd11(pw , >= -1.0 , mcout);
      double k = y[nr] * pow( x[nr] , -pw );
      double t = k/(1+pw)*( pow( xr2 , (pw+1) ) - pow( xr1 , (pw+1) ) );
      check_econd11a(t , < 0.0, "non-linear interpolation\n"
                     <<" x[nr]="<<x[nr]
                     <<" y[nr]="<<y[nr]<<'\n'
                     <<" x[nr+1]="<<x[nr+1]
                     <<" y[nr+1]="<<y[nr+1]<<'\n'
                     <<" threshold="<<threshold<<'\n'
                     <<" pw="<<pw
                     <<" k="<<k
                     <<'\n', mcout);

      //if(nr == 0)
      //{
      //        mcout<<"non-linear interpolation\n"
      //             <<" x[nr]="<<x[nr]
      //             <<" y[nr]="<<y[nr]<<'\n'
      //             <<" x[nr+1]="<<x[nr+1]
      //             <<" y[nr+1]="<<y[nr+1]<<'\n'
      //             <<" threshold="<<threshold<<'\n'
      //             <<" pw="<<pw
      //             <<" k="<<k
      //             <<" t="<<t<<'\n';
      //}
      s += t;
    }
    else
    {
      a = (y[nr+1] - y[nr])/(x[nr+1] - x[nr]);
      b = y[nr];
      if(nr == 0)
      {
        double c1 = a * (xr1 - x[nr]) + b;
        double c2 = a * (xr2 - x[nr]) + b;
        if(c2 < 0.0)   // new correction
        {
          return 0.0;
        }
        if(c1 < 0.0)
        {
          //mcout<<"linear interpolation\n"
          //    <<"c="<<c<<'\n'
          //    <<"x1="<<x1<<" x2="<<x2<<" q="<<q
          //    <<" nr="<<nr<<'\n'
          //    <<" x[nr]="<<x[nr]<<" y[nr]="<<y[nr]<<'\n'
          //    <<" x[nr+1]="<<x[nr+1]<<" y[nr+1]="<<y[nr+1]<<'\n'
          //    <<" xr1,2="<<xr1<<' '<<xr2<<'\n'
          //    <<" a,b = "<<a<<' '<<b<<'\n';
           // find zero and
          // change xr1
          check_econd11(a , == 0.0 , mcerr);
          xr1 = (a * x[nr] - b) / a;
          // for debug:
          double t = 0.5*a*(xr2*xr2 - xr1*xr1) + (b - a*x[nr])*(xr2 - xr1);
          //mcout<<"linear interpolation\n"
          //    <<"xr1="<<xr1<<" a * (xr1- x[nr]) + b="<<a * (xr1- x[nr]) + b
          //    <<" t="<<t<<'\n';
          //exit(0);
        }
      }
      double t = 0.5*a*(xr2*xr2 - xr1*xr1) + (b - a*x[nr])*(xr2 - xr1);
      check_econd11a(t , < 0.0,
                     "linear interpolation\n"
                     <<"x1="<<x1<<" x2="<<x2<<" q="<<q
                     <<" nr="<<nr<<'\n'
                     <<" x[nr]="<<x[nr]<<" y[nr]="<<y[nr]<<'\n'
                     <<" x[nr+1]="<<x[nr+1]<<" y[nr+1]="<<y[nr+1]<<'\n'
                     <<" xr1,2="<<xr1<<' '<<xr2<<'\n'
                     <<" a,b = "<<a<<' '<<b<<'\n' , mcout);
      //if(nr == 0)
      //{
      //        mcout<<"linear interpolation\n"
      //             <<" xr1="<<xr1
      //             <<" xr2="<<xr2<<'\n'
      //             <<" x[nr]="<<x[nr]
      //             <<" y[nr]="<<y[nr]<<'\n'
      //             <<" x[nr+1]="<<x[nr+1]
      //             <<" y[nr+1]="<<y[nr+1]<<'\n'
      //             <<" threshold="<<threshold<<'\n'
      //             <<" a="<<a
      //             <<" b="<<b
      //             <<" t="<<t<<'\n';
      //}
        s+= t;
    }
  }
  return s;
}
*/

PhotoAbsCS::PhotoAbsCS(void) : Z(0), threshold(0.0) { ; }

PhotoAbsCS::PhotoAbsCS(const String& fname, int fZ, double fthreshold)
    : name(fname), Z(fZ), threshold(fthreshold) {
  ;
}

void PhotoAbsCS::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "PhotoAbsCS: name=" << name << " Z = " << Z
          << " threshold = " << threshold << std::endl;
  }
}

OveragePhotoAbsCS::OveragePhotoAbsCS(PhotoAbsCS* apacs, double fwidth,  // MeV
                                     double fstep,                      // MeV
                                     long fmax_q_step)
    : real_pacs(apacs, do_clone),
      width(fwidth),
      max_q_step(fmax_q_step),
      step(fstep) {
  mfunname("OveragePhotoAbsCS::OveragePhotoAbsCS(...)");
  check_econd11(apacs, == NULL, mcerr);
  if (fwidth > 0.0) {
    check_econd11(fstep, >= 0.6 * fwidth, mcerr);  // 0.5 is bad but OK
  }
  /* I do not understand why the access is not allowed below.
     So I call functions
     name =
     apacs->name;
     //real_pacs->name;
     Z = real_pacs->Z;
     threshold = real_pacs->threshold;
  */
  name = real_pacs->get_name();
  Z = real_pacs->get_Z();
  threshold = real_pacs->get_threshold();
}

double OveragePhotoAbsCS::get_CS(double energy) const {
  mfunname("double OveragePhotoAbsCS::get_CS(double energy) const");
  //mcout<<"OveragePhotoAbsCS::get_CS is started\n";
  //mcout<<"OveragePhotoAbsCS::get_CS:\n";
  if (width == 0.0) {
    // for no modification:
    return real_pacs->get_CS(energy);
  } 
  const double w2 = width * 0.5;
  double e1 = energy - w2;
  if (e1 < 0.0) e1 = 0.0;
  const double res = real_pacs->get_integral_CS(e1, energy + w2) / width;
  return res;
}

double OveragePhotoAbsCS::get_integral_CS(double energy1,
                                          double energy2) const {
  mfunname("double OveragePhotoAbsCS::get_integral_CS(double energy1, double "
           "energy2) const");
  //mcout<<"OveragePhotoAbsCS::get_integral_CS is started\n";
  if (width == 0.0 || energy1 >= energy2) {
    // for no modification:
    return real_pacs->get_integral_CS(energy1, energy2);
  } 
  long q = long((energy2 - energy1) / step);
  if (q > max_q_step) {
    return real_pacs->get_integral_CS(energy1, energy2);
  } 
  q++;
  double rstep = (energy2 - energy1) / q;
  double x0 = energy1 + 0.5 * rstep;
  double s = 0.0;
  for (long n = 0; n < q; n++) {
    s += get_CS(x0 + rstep * n);
  }
  s *= rstep;
  return s;

}

void OveragePhotoAbsCS::scale(double fact) {
  mfunname("void OveragePhotoAbsCS::scale(double fact)");
  real_pacs->scale(fact);
}

void OveragePhotoAbsCS::print(std::ostream& file, int l) const {
  mfunname("void PhotoAbsCS::print(std::ostream& file, int l) const");
  Ifile << "OveragePhotoAbsCS: width = " << width << " step=" << step
        << " max_q_step=" << max_q_step << '\n';
  indn.n += 2;
  real_pacs->print(file, l);
  indn.n -= 2;
}

HydrogenPhotoAbsCS::HydrogenPhotoAbsCS(void)
    : PhotoAbsCS("H", 1, 15.43e-6), prefactor(1.) {}

double HydrogenPhotoAbsCS::get_CS(double energy) const {
  if (energy < threshold) {
    return 0.0;
  } else {
    if (energy != DBL_MAX) {
      return 0.5 *  // accounts one atom instead of two
             prefactor * 0.0535 * (pow(100.0e-6 / energy, 3.228));
    } else {
      return 0.0;
    }
  }
}

double HydrogenPhotoAbsCS::get_integral_CS(double energy1,
                                           double energy2) const {
  if (energy2 < threshold) {
    return 0.0;
  } else {
    if (energy1 < threshold) {
      energy1 = threshold;  // local var.
    }
    if (energy2 == DBL_MAX) {
      return 0.5 *  // accounts one atom instead of two
             prefactor * 0.0535 * pow(100.0e-6, 3.228) / 2.228 *
             (1.0 / pow(energy1, 2.228));
    } else {
      return 0.5 *  // accounts one atom instead of two
             prefactor * 0.0535 * pow(100.0e-6, 3.228) / 2.228 *
             (1.0 / pow(energy1, 2.228) - 1.0 / pow(energy2, 2.228));
    }
  }
}

void HydrogenPhotoAbsCS::scale(double fact) { prefactor = fact; }

void HydrogenPhotoAbsCS::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "HydrogenPhotoAbsCS: name=" << name << " Z = " << Z
          << " threshold = " << threshold << std::endl;
  }
}

SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const String& fname, int fZ,
                                             double fthreshold,
                                             const String& ffile_name)
    : PhotoAbsCS(fname, fZ, fthreshold), file_name(ffile_name) {
  mfunnamep("SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(...)");
#ifdef USE_STLSTRING
  std::ifstream file(file_name.c_str());
#else
  std::ifstream file(file_name);
#endif
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  long q = 0;  // number of read values
  ener = DynLinArr<double>(10, 0.0);
  cs = DynLinArr<double>(10, 0.0);
  do {
    file >> ener[q];
    //Iprintn(mcout, file.good());
    //Iprintn(mcout, file.eof());
    //Iprintn(mcout, file.fail());
    //Iprintn(mcout, file.bad());
    //file.clear();  // idiotic thing necessary for the new compiler
    if (!file.good()) break;
    check_econd11(ener[q], < 0.0, mcerr);
    if (q > 0) {
      check_econd12(ener[q], <, ener[q - 1], mcerr);
    }
    ener[q] *= 1.0e-6;  // convertion to MeV
    file >> cs[q];
    if (!file.good()) break;
    check_econd11(cs[q], < 0.0, mcerr);
    q++;
    if (q == ener.get_qel()) {  // increasing the size of arrays
      long q1 = q * 2;
      ener.put_qel(q1);
      cs.put_qel(q1);
    }
  } while (1);
  if (q != ener.get_qel()) {  // reducing the size if necessary
    ener.put_qel(q);
    cs.put_qel(q);
  }

}
SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const String& fname, int fZ,
                                             double fthreshold,
                                             const DynLinArr<double>& fener,
                                             const DynLinArr<double>& fcs)
    : PhotoAbsCS(fname, fZ, fthreshold),
      file_name("none"),
      ener(fener),
      cs(fcs) {
  mfunname("SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(...)");
  check_econd12(ener.get_qel(), !=, cs.get_qel(), mcerr);
}

SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const String& fname, int fZ,
                                             double fthreshold, int l,
                                             double E0, double yw, double ya,
                                             double P, double sigma)
    : PhotoAbsCS(fname, fZ, fthreshold) {
  mfunname("SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS");
  long q = 1000;
  ener = make_log_mesh_ec(2.0e-6, 2.0e-1, q);
  cs.put_qel(q, 0.0);
  long n;
  for (n = 0; n < q; n++) {
    double energy = ener[n];
    if (energy < threshold)
      cs[n] = 0.0;
    else {
      double Q = 5.5 + l - 0.5 * P;
      double y = energy / E0;
      double Fpasc = ((y - 1) * (y - 1) + yw * yw) * pow(y, (-Q)) *
                     pow((1.0 + sqrt(y / ya)), (-P));
      Fpasc = Fpasc * sigma;
      cs[n] = Fpasc;
      //Iprint3n(mcout, energy, E0, threshold);
      //Iprint4n(mcout, Q, y, sigma, Fpasc);
    }
  }
  remove_leading_zeros();
}

SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const SimpleTablePhotoAbsCS& total,
                                             const SimpleTablePhotoAbsCS& part,
                                             double emax_repl) {
  mfunname("SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS (const "
           "SimpleTablePhotoAbsCS& total,...)");

  *this = total;  // to assure that all is preserved

  long qe_i = total.ener.get_qel();

  const DynLinArr<double>& ener_r = part.get_arr_ener();
  const DynLinArr<double>& cs_r = part.get_arr_CS();
  long qe_r = ener_r.get_qel();
  std::vector<double> new_ener;
  std::vector<double> new_cs;
  // first write replacements
  for (long ne = 0; ne < qe_r; ne++) {
    if (ener_r[ne] >= total.get_threshold() && ener_r[ne] <= emax_repl) {
      new_ener.push_back(ener_r[ne]);
      new_cs.push_back(cs_r[ne]);
    }
  }
  for (long ne = 0; ne < qe_i; ne++) {
    if (ener[ne] >= total.get_threshold() && ener[ne] > emax_repl) {
      new_ener.push_back(total.ener[ne]);
      new_cs.push_back(total.cs[ne]);
    }
  }
  const long qe = new_ener.size();
  ener.put_qel(qe);
  cs.put_qel(qe);
  for (long ne = 0; ne < qe; ne++) {
    ener[ne] = new_ener[ne];
    cs[ne] = new_cs[ne];
    //Iprint3n(mcout, ne, ener[ne], cs[ne]);
  }
}

void SimpleTablePhotoAbsCS::remove_leading_zeros(void) {
  long q = ener.get_qel();
  long ne;
  long nez;
  for (ne = 0; ne < q; ne++) {
    if (cs[ne] > 0.0) break;
  }
  if (ne > 0) {
    long qn = q - ne;
    DynLinArr<double> enern(qn);
    DynLinArr<double> csn(qn);
    for (nez = ne; nez < q; nez++) {
      enern[nez - ne] = ener[nez];
      csn[nez - ne] = cs[nez];
    }
    ener = enern;
    cs = csn;
  }
}
void SimpleTablePhotoAbsCS::remove_leading_tiny(double level) {
  long q = ener.get_qel();
  long ne;
  long nez;
  for (ne = 0; ne < q; ne++) {
    if (cs[ne] > level) break;
  }
  if (ne > 0) {
    long qn = q - ne;
    DynLinArr<double> enern(qn);
    DynLinArr<double> csn(qn);
    for (nez = ne; nez < q; nez++) {
      enern[nez - ne] = ener[nez];
      csn[nez - ne] = cs[nez];
    }
    ener = enern;
    cs = csn;
  }
}

double SimpleTablePhotoAbsCS::get_CS(double energy) const {
  mfunname("double SimpleTablePhotoAbsCS::get_CS(double energy) const");
  //mcout<<"SimpleTablePhotoAbsCS::get_CS is started\n";
  //Iprintn(mcout, energy);
  long q = ener.get_qel();
  if (q == 0) return 0.0;
  check_econd11(q, == 1, mcerr);
  //Iprint3n(mcout, threshold,  ener[0], energy);
  if (energy < threshold)
    return 0.0;
  else {
    if (energy <= ener[q - 1]) {
      //mcout<<"Create mesh\n";
      PointCoorMesh<double, const DynLinArr<double> > pcmd(q, &ener);
      double s;
      s = t_value_generic_point_ar<
          double, DynLinArr<double>,
          PointCoorMesh<double, const DynLinArr<double> > >(
          pcmd, cs, &my_val_fun, energy, 1, threshold, 0, DBL_MAX);
      return s;
    } else {
      if (energy == DBL_MAX)
        return 0.0;
      else
        return cs[q - 1] * pow(energy, -2.75) / pow(ener[q - 1], -2.75);
    }

    /*
    long i1, i2;
    long q = ener.get_qel();
    if(energy < ener[0])
    {
      i1 = 0;
      i2 = 1;
      return cs[i1] + (energy - ener[i1])*(cs[i2] - cs[i1])/
        (ener[i2] - ener[i1]);
    }
    else
    {
      if(energy < ener[q-1])
      {
        // now find interval
        i1 = 0;
        i2 = q-1;
        while( i2 - i1 != 1)
        {
          long i3 = (i1 + i2)/2;
          if(energy < ener[i3])
            i2 = i3;
          else
            i1 = i3;
        }
        //mcout<<"i12="<<i1<<' '<<i2
        //     <<" ener="<< ener[i1] <<' '<< ener[i2] <<std::endl;
        // now make interpolation
        // linear is not always good
        //return cs[i1] + (energy -
        //                 ener[i1])*(cs[i2] - cs[i1])/(ener[i2] - ener[i1]);
        //if(cs[i2] >= cs[i1] || ener[i2] < 1.2*ener[i1] ||
        //   cs[i1] == 0 || cs[i2] == 0 )
        if( sign_nonlinear_interpolation(ener[i1], cs[i1],
                                         ener[i2], cs[i2], threshold) == 0 )
        {  // linear case
          //mcout<<"linear\n";
          //Iprintn(mcout, cs[i1] + (energy - ener[i1])*(cs[i2] -
    cs[i1])/(ener[i2] - ener[i1]) );
          return cs[i1] + (energy -
                           ener[i1])*(cs[i2] - cs[i1])/(ener[i2] - ener[i1]);
        }
        else
        {
          //mcout<<"power\n";
          //power
          double pw = log(cs[i1]/cs[i2]) / log(ener[i1]/ener[i2]);
          //Iprintn(mcout, pw);
          return cs[i1] * pow( energy , pw ) / pow( ener[i1] , pw );
        }
      }
      else
      {
        return cs[q-1] * pow( energy , -2.75 ) / pow( ener[q-1] , -2.75 );
      }
    }
    */
  }
}

double SimpleTablePhotoAbsCS::get_integral_CS(double energy1,
                                              double energy2) const {
  mfunname("double SimpleTablePhotoAbsCS::get_integral_CS(...)");

  long q = ener.get_qel();
  if (q == 0) return 0.0;
  check_econd11(q, == 1, mcerr);
  //Iprint2n(mcout, energy2, threshold);
  if (energy2 < threshold) return 0.0;
  if (energy1 < threshold) energy1 = threshold;
  double s = 0.0;
  double energy21 = ener[q - 1];
  if (energy1 < energy21) {
    if (energy21 > energy2) energy21 = energy2;
    check_econd12(energy1, >, energy21, mcerr);
    s = glin_integ_ar(ener, cs, ener.get_qel(), energy1, energy21, threshold);
  }
  //print(mcout, 3);
  //mcout << "energy1="<<energy1
  //      << " energy21="<<energy21
  //      << " ener[q-1]="<<ener[q-1]
  //      << " threshold="<<threshold
  //      << " s="<<s<<'\n';
  check_econd11(s, < 0.0, mcout);
  if (energy2 > ener[q - 1]) {
    // add tail
    if (energy2 == DBL_MAX) {
      if (energy1 < ener[q - 1]) energy1 = ener[q - 1];
      double c =
          cs[q - 1] / (1.75 * pow(ener[q - 1], -2.75)) * pow(energy1, -1.75);
      //check_econd11(c , < 0.0, mcout);
      s += c;
    } else {
      if (energy1 < ener[q - 1]) energy1 = ener[q - 1];
      double c = cs[q - 1] / (1.75 * pow(ener[q - 1], -2.75)) *
                 (pow(energy1, -1.75) - pow(energy2, -1.75));
      //check_econd11(c , < 0.0, mcout);
      s += c;
    }
  }
  return s;
}

void SimpleTablePhotoAbsCS::scale(double fact) {
  mfunnamep("void SimpleTablePhotoAbsCS::scale(double fact)");
  long q = ener.get_qel();
  for (long n = 0; n < q; ++n) {
    cs[n] *= fact;
  }
}

void SimpleTablePhotoAbsCS::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "SimpleTablePhotoAbsCS: name=" << name << " Z = " << Z
          << std::endl;
    Ifile << " threshold = " << threshold << " file_name=" << file_name
          << std::endl;
    if (l > 1) {
      indn.n += 2;
      for (long n = 0; n < ener.get_qel(); ++n) {
        Ifile << "n=" << n << " ener=" << ener[n] << " cs=" << cs[n]
              << std::endl;
      }
      indn.n -= 2;
    }
  }
}

PhenoPhotoAbsCS::PhenoPhotoAbsCS() : PhotoAbsCS("none", 0, 0.0), power(0.0) {}

PhenoPhotoAbsCS::PhenoPhotoAbsCS(const String& fname, int fZ, double fthreshold,
                                 double fpower)
    : PhotoAbsCS(fname, fZ, fthreshold), power(fpower) {
  mfunname("PhenoPhotoAbsCS::PhenoPhotoAbsCS");
  check_econd11a(power, <= 2,
                 " cannot be so, otherwise the integral is infinite", mcerr);
  factor =
      pow(threshold, power - 1.) * Thomas_sum_rule_const_Mb * Z * (power - 1.);
}

double PhenoPhotoAbsCS::get_CS(double energy) const {
  if (energy < threshold || energy == DBL_MAX) return 0.0;
  return factor * (pow(energy, -power));
}

double PhenoPhotoAbsCS::get_integral_CS(double energy1, double energy2) const {
  //Imcout<<"energy1="<<energy1<<" energy2="<<energy2<<'\n';
  if (energy2 < threshold) return 0.0;
  if (energy1 < threshold) energy1 = threshold;
  double s;
  if (energy2 == DBL_MAX) {
    s = factor / (power - 1.) * (1. / pow(energy1, power - 1.));
  } else {
    s = factor / (power - 1.) *
        (1. / pow(energy1, power - 1.) - 1. / pow(energy2, power - 1.));
  }
  return s;
}

void PhenoPhotoAbsCS::scale(double fact) {
  mfunnamep("void PhenoPhotoAbsCS::scale(double fact)");
  factor *= fact;
}

void PhenoPhotoAbsCS::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "PhenoPhotoAbsCS: name=" << name << " Z = " << Z << std::endl;
    Ifile << " threshold = " << threshold << " power=" << power
          << " factor=" << factor << std::endl;
  }
}

/*
// drafts, unused
mesh_for_FitBT =  EnergyMesh(

FitBTPhotoAbsCS::FitBTPhotoAbsCS(void): PhotoAbsCS("none",0, 0.0),
  E0(0.0), yw(0.0), ya(0.0), P(0.0), sigma(0.0)
{;}

FitBTPhotoAbsCS::FitBTPhotoAbsCS
(const String& fname, int fZ, double fthreshold,
 int  fl, double fE0, double fyw, double fya,
 double fP, double fsigma):
  PhotoAbsCS(fname, fZ, fthreshold),
  l(fl), E0(fE0), yw(fyw), ya(fya), P(fP), sigma(fsigma)
{
  mfunname("FitBTPhotoAbsCS::FitBTPhotoAbsCS(...)");
}

double FitBTPhotoAbsCS::get_CS(double energy) const
{
  if(energy < threshold)
    return 0.0;
  else
  {
    if(energy == BDL_MAX)
      return 0.0;
    else
    {
      double Q = 5.5 + fl - 0.5 * fP;
      double y = energy / fE0;
      double Fpasc = ((y - 1) * (y - 1) + yw * yw)
        * pow(y , (-Q) ) * pow((1.0 + sqrt(y / ya)) , (-P));
      Fpasc = Fpasc * sigma0;
      return Fpasc;
    }
}

double PhenoPhotoAbsCS::get_integral_CS(double energy1, double energy2) const
{
  //Imcout<<"energy1="<<energy1<<" energy2="<<energy2<<'\n';
  if(energy2 < threshold)
    return 0.0;
  else
  {
    if(energy1 < threshold) energy1 = threshold;
    double s = factor / (power - 1.0) *
      (1.0/pow (energy1  , power-1.0) - 1.0/pow (energy2  , power-1.0));
    //Imcout<<"s="<<s<<'\n';
    return s;
  }
}
*/

/*
FitBTPhotoAbsCS::FitBTPhotoAbsCS
(const String& fname, int fZ, double fthreshold, double fpower):
  PhotoAbsCS(fname, fZ, 0.0)
{
  mfunname("FitBTPhotoAbsCS::FitBTPhotoAbsCS");

  name = fname;
#ifdef USE_STLSTRING
  std::ifstream BT_file(name.c_str());
#else
  std::ifstream BT_file(name);
#endif
  if( !BT_file )
  {
    funnw.ehdr(mcerr);
    mcerr<<"cannot open file "<<name<<std::endl;
    spexit(mcerr);
  }
  int i = 0;
  while( (i = findmark(BT_file, "$")) == 1 )
  {
    long iZ;
    BT_file >> iZ;
    if(iz == Z)
    {


  funnw.ehdr(mcerr);
  mcerr<<"there is no element Z="<<fZ<<" in file
"<<threshold_file_name<<std::endl;
  spexit(mcerr);
 mark1:

*/

//------------------------------------------------------------------------

void AtomicSecondaryProducts::add_channel(
    double fchannel_prob_dens, const DynLinArr<double>& felectron_energy,
    const DynLinArr<double>& fphoton_energy, int s_all_rest) {
  mfunnamep("void AtomicSecondaryProducts::add_channel(...)");
  check_econd21(fchannel_prob_dens, < 0.0 ||, > 1.0, mcerr);
  long q_old = channel_prob_dens.get_qel();
  long q_new = q_old + 1;
  channel_prob_dens.put_qel(q_new);
  electron_energy.put_qel(q_new);
  photon_energy.put_qel(q_new);
  //Iprintn(mcout, s);
  //check_econd11( s , > 1.0 , mcerr);
  if (s_all_rest == 1) {
    double s = 0.0;
    for (long n = 0; n < q_old; ++n) {
      s += channel_prob_dens[n];
    }
    check_econd21(s, < 0.0 ||, > 1.0, mcerr);
    fchannel_prob_dens = 1.0 - s;
  }
  channel_prob_dens[q_old] = fchannel_prob_dens;
  electron_energy[q_old] = felectron_energy;
  photon_energy[q_old] = fphoton_energy;
  double s = 0.0;
  for (long n = 0; n < q_new; ++n) {
    s += channel_prob_dens[n];
  }
  if (s > 1.0) {
    funnw.ehdr(mcerr);
    mcerr << "s > 1.0, s=" << s << '\n';
    Iprintn(mcerr, q_new);
    for (long n = 0; n < q_new; ++n) {
      mcerr << "n=" << n << " channel_prob_dens[n]=" << channel_prob_dens[n]
            << '\n';
    }
    spexit(mcerr);
  }
}

int AtomicSecondaryProducts::get_channel(
    DynLinArr<double>& felectron_energy,
    DynLinArr<double>& fphoton_energy) const {
  mfunname("int AtomicSecondaryProducts::get_channel(...)");
#ifdef DEBUG_PRINT_get_escape_particles
  mcout << "AtomicSecondaryProducts::get_channel is started\n";
  Iprintn(mcout, channel_prob_dens.get_qel());
#endif
  int ir = 0;
  if (channel_prob_dens.get_qel() > 0) {
    double rn = SRANLUX();
#ifdef DEBUG_PRINT_get_escape_particles
    Iprintn(mcout, rn);
#endif
    if (channel_prob_dens.get_qel() == 1) {
      if (rn < channel_prob_dens[0]) {
        felectron_energy = electron_energy[0];
        fphoton_energy = photon_energy[0];
        ir = 1;
      }
    } else {
      long q = channel_prob_dens.get_qel();
      double s = 0.0;
      for (long n = 0; n < q; ++n) {
        s += channel_prob_dens[n];
        if (rn <= s) {
          felectron_energy = electron_energy[n];
          fphoton_energy = photon_energy[n];
          ir = 1;
#ifdef DEBUG_PRINT_get_escape_particles
          Iprint2n(mcout, n, s);
#endif
          break;
        }
      }
    }
  }
#ifdef DEBUG_PRINT_get_escape_particles
  mcout << "AtomicSecondaryProducts::get_channel is finishing\n";
  Iprintn(mcout, ir);
#endif
  return ir;
}

void AtomicSecondaryProducts::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "AtomicSecondaryProducts(l=" << l << "):\n";
    long q = channel_prob_dens.get_qel();
    Ifile << "number of channels=" << q << '\n';
    indn.n += 2;
    for (long n = 0; n < q; ++n) {
      Ifile << "n_channel=" << n << " probability=" << channel_prob_dens[n]
            << '\n';
      indn.n += 2;
      long qel = electron_energy[n].get_qel();
      Ifile << "number of electrons=" << qel << '\n';
      indn.n += 2;
      for (long nel = 0; nel < qel; ++nel) {
        Ifile << "nel=" << nel << " electron_energy=" << electron_energy[n][nel]
              << '\n';
      }
      indn.n -= 2;
      long qph = photon_energy[n].get_qel();
      Ifile << "number of photons=" << qph << '\n';
      indn.n += 2;
      for (long nph = 0; nph < qph; ++nph) {
        Ifile << "nph=" << nph << " photon_energy=" << photon_energy[n][nph]
              << '\n';
      }
      indn.n -= 2;
      indn.n -= 2;
    }
    indn.n -= 2;
  }
}

AtomPhotoAbsCS::AtomPhotoAbsCS() : name("none"), Z(0), qshell(0) {}

double AtomPhotoAbsCS::get_TICS(double energy,
                                double factual_minimal_threshold) const {
  mfunname("double AtomPhotoAbsCS::get_TICS(double energy, double "
           "factual_minimal_threshold) const");

  if (factual_minimal_threshold <= energy) {
    // All what is absorbed, should ionize
    return get_ACS(energy);
  }
  return 0.0;
}

double AtomPhotoAbsCS::get_integral_TICS(
    double energy1, double energy2, double factual_minimal_threshold) const {
  mfunname("double AtomPhotoAbsCS::get_integral_TICS(double energy1, double "
           "energy2, double factual_minimal_threshold) const");

  if (factual_minimal_threshold <= energy2) {
    // All what is absorbed, should ionize
    if (energy1 < factual_minimal_threshold) {
      energy1 = factual_minimal_threshold;
    }
    return get_integral_ACS(energy1, energy2);
  }
  return 0.0;
}

double AtomPhotoAbsCS::get_TICS(int nshell, double energy,
                                double factual_minimal_threshold) const {
  mfunname("double AtomPhotoAbsCS::get_TICS(int nshell, double energy, double "
           "factual_minimal_threshold) const");

  if (s_ignore_shell[nshell] == 0) {
    if (factual_minimal_threshold <= energy) {
      // All what is absorbed, should ionize
      return get_integral_ACS(nshell, energy);
    }
  }
  return 0.0;
}

double AtomPhotoAbsCS::get_integral_TICS(
    int nshell, double energy1, double energy2,
    double factual_minimal_threshold) const {
  mfunname("double AtomPhotoAbsCS::get_integral_TICS(int nshell, double "
           "energy1, double energy2, double factual_minimal_threshold) const");

  if (s_ignore_shell[nshell] == 0) {
    if (factual_minimal_threshold <= energy1) {
      // All what is absorbed, should ionize
      return get_integral_ACS(nshell, energy1, energy2);
    }
    if (factual_minimal_threshold >= energy2) return 0.0;
    return get_integral_ACS(nshell, factual_minimal_threshold, energy2);
  }
  return 0.0;
}

void AtomPhotoAbsCS::remove_shell(int nshell) {
  mfunname("void AtomPhotoAbsCS::remove_shell(int nshell)");
  check_econd21(nshell, < 0 ||, >= qshell, mcerr);
  s_ignore_shell[nshell] = 1;
}

void AtomPhotoAbsCS::restore_shell(int nshell) {
  mfunname("void AtomPhotoAbsCS::restore_shell(int nshell)");
  check_econd21(nshell, < 0 ||, >= qshell, mcerr);
  s_ignore_shell[nshell] = 0;
}

void AtomPhotoAbsCS::print(std::ostream& file, int l) const {
  mfunnamep("void AtomPhotoAbsCS::print(std::ostream& file, int l) const");
  if (l > 0) {
    Ifile << "AtomPhotoAbsCS(l=" << l << "): name=" << name << " Z = " << Z
          << " qshell = " << qshell << std::endl;
    Iprintn(mcout, asp.get_qel());
    long q = asp.get_qel();
    if (q == 0) {
      q = s_ignore_shell.get_qel();
      indn.n += 2;
      for (long n = 0; n < q; ++n) {
        Ifile << "n=" << n << " s_ignore_shell[n] = " << s_ignore_shell[n]
              << '\n';
      }
      indn.n -= 2;
    } else {
      check_econd12(asp.get_qel(), !=, s_ignore_shell.get_qel(), mcerr);
      indn.n += 2;
      for (long n = 0; n < q; ++n) {
        Ifile << "n=" << n << " s_ignore_shell[n] = " << s_ignore_shell[n]
              << '\n';
        asp[n].print(mcout, l);
      }
      indn.n -= 2;
    }
  }
}

std::ostream& operator<<(std::ostream& file, const AtomPhotoAbsCS& f) {
  f.print(file, 1);
  return file;
}

double AtomPhotoAbsCS::get_I_min(void) const {
  mfunname("double AtomPhotoAbsCS::get_I_min(void) const");
  double st = DBL_MAX;
  for (int n = 0; n < qshell; ++n) {
    // currently the minimal shell is the last,
    // but to avoid this assumption we check all
    if (get_threshold(n) < st) st = get_threshold(n);
  }
  return st;
}

void AtomPhotoAbsCS::get_escape_particles(int nshell, double energy,
                                          DynLinArr<double>& el_energy,
                                          DynLinArr<double>& ph_energy) const {
  mfunname("void AtomPhotoAbsCS::get_escape_particles(...)");
#ifdef DEBUG_PRINT_get_escape_particles
  mcout << "AtomPhotoAbsCS::get_escape_particles is started\n";
  // to find minimal shell
  Iprintn(mcout, nshell);
  Iprintn(mcout, energy);
#endif
  check_econd12(energy, <, 0.5 * get_threshold(nshell), mcerr);
  // In principle, the energy is allowed to be slightly less than threshold
  // due to unprecision of definition of point-wise cross sections.
  // To keep correct norm it is better not to ignore such events.
  // They usually can be treated quite well.
  // The factor 0.5 is put there just as arbitrary check for full stupidity.

  // program is complicated; to make sure that there is no remains
  // from previous runs are allowed to pass, I clear arrays:
  el_energy = DynLinArr<double>(0);
  ph_energy = DynLinArr<double>(0);

  int n_min = 0;
  double st = DBL_MAX;
  for (int n = 0; n < qshell; ++n) {
    // currently the minimal shell is the last,
    // but to avoid this assumption we check all
    if (get_threshold(n) < st) {
      n_min = n;
      st = get_threshold(n);
    }
  }
#ifdef DEBUG_PRINT_get_escape_particles
  Iprintn(mcout, n_min);
#endif
  if (nshell == n_min) {
    // generate delta-electron, here it will be the only one, since the shell
    // is the outmost valent one.
    double en;
    if (energy - get_threshold(n_min) >= 0.0) {
      en = energy - get_threshold(n_min);
    } else {
      en = 0.0;
    }
    el_energy = DynLinArr<double>(1, en);
    ph_energy = DynLinArr<double>(0);
  } else {
    // Energy of photo-electron
    double en = energy - get_threshold(nshell);
    double hdist = 0.0;  // used to preserve the balance of energy
                         // virtual gamma are generated by energy mesh
    // and their energy could be little less than the shell energy.
    // To avoid generation of electrons with negative energy
    // their energy is corrected. The value of correction is hdist.
    // To preserve energy this hdist is then distributed over
    // the other secondary products if they exist.
    if (en < 0.0) {
      hdist = -en;
      en = 0.0;
    }
    int is = 0;
    DynLinArr<double> felectron_energy;
    DynLinArr<double> fphoton_energy;
#ifdef DEBUG_PRINT_get_escape_particles
    Iprint2n(mcout, asp.get_qel(), get_qshell());
#endif
#ifndef DEBUG_ignore_non_standard_channels
    if (asp.get_qel() == get_qshell()) {
      // works only in this case?
      is = asp[nshell].get_channel(felectron_energy, fphoton_energy);
      // Here zero can be if the shell is not included in database
      // or if not standard channel is not choosen by random way.
      // In both cases the standard way should be invoked.
    }
#endif
    int main_n = get_main_shell_number(nshell);
#ifdef DEBUG_PRINT_get_escape_particles
    Iprint2n(mcout, nshell, main_n);
    Iprintn(mcout, is);
    Iprint(mcout, felectron_energy);
    Iprint(mcout, fphoton_energy);
#endif
    //check_econd22( is , == 0 && , main_n , < 0 , mcerr);  // since
    //// there will be neither default channel nor the explicit one
    //if(is == 0 && main_n > 0)
    if (is == 0) {
      // generate default channel
      if (main_n > 0) {
        // first find the principal quantum number of the deepest shell
        int main_n_largest = 0;
        for (int n = 0; n < qshell; ++n) {
          int t = get_main_shell_number(n);
          if (t > main_n_largest) main_n_largest = t;
        }
#ifdef DEBUG_PRINT_get_escape_particles
        Iprintn(mcout, main_n_largest);
#endif
        if (main_n_largest - main_n >= 2) {
          // At least K, l, M shells exists
          // In this case we use more advanced scheme
          // Look for shell with larger main number and with less energy
          int n_choosen = -1;
          double thr = DBL_MAX;  // this will be the least threshold
                                 // among the shells with next principal number
          for (int n = 0; n < qshell; ++n) {
            // currently the minimal shell is the last,
            // but to avoid this assumption we check all
            int main_n_t = get_main_shell_number(n);
            if (main_n_t > 0 && main_n_t == main_n + 1) {
              if (thr > get_threshold(n)) {
                n_choosen = n;
                thr = get_threshold(n);
              }
            }
          }
#ifdef DEBUG_PRINT_get_escape_particles
          Iprint2n(mcout, n_choosen, thr);
#endif
          check_econd11(n_choosen, < 0, mcerr);
          double e_temp = 0.0;
          if ((e_temp = get_threshold(nshell) - hdist -
                        2.0 * get_threshold(n_choosen))
              //if(get_threshold(nshell) - hdist - 2.0*get_threshold(n_choosen)
              > 0.0) {
            el_energy = DynLinArr<double>(4);
            el_energy[0] = en;  // photo-electron
            el_energy[1] = e_temp;
            //el_energy[1] = get_threshold(nshell) - hdist -
            //  2.0*get_threshold(n_choosen);
            // first Auger from choosen shell
            // Then filling two vacancies at the next (choosen) shell
            // from the outermost one
            if ((e_temp = get_threshold(n_choosen) -
                          2.0 * get_threshold(n_min)) > 0.0) {
              el_energy[2] = e_temp;
              //el_energy[2] =
              //  get_threshold(n_choosen) - 2.0 * get_threshold(n_min);
              el_energy[3] = el_energy[2];
              check_econd11(el_energy[2], < 0.0, mcerr);
            } else {  // ignore two last Auger electrons
              el_energy.put_qel(2);
            }
          } else if ((e_temp = get_threshold(nshell) - hdist -
                      get_threshold(n_choosen) - get_threshold(n_min)) > 0.0) {
            el_energy = DynLinArr<double>(3);
            el_energy[0] = en;  // photo-electron
            el_energy[1] = e_temp;
            //el_energy[1] = get_threshold(nshell) - hdist -
            //  get_threshold(n_choosen) - get_threshold(n_min);
            // filling initially ionized level from choosen
            // and emmitance of Auger from outermost.
            check_econd11(el_energy[1], < 0.0, mcerr);
            if ((e_temp = get_threshold(n_choosen) -
                          2.0 * get_threshold(n_min)) > 0.0) {
              el_energy[2] = e_temp;
              //el_energy[2] = get_threshold(n_choosen) - 2.0 *
              //get_threshold(n_min);
            } else {  // ignore this last Auger electron
              el_energy.put_qel(2);
            }
          }
          ph_energy = DynLinArr<double>(0);
        } else  // for if(main_n_largest - main_n >= 2)
            {
          //generate Auger from the outermost shell
          double en1 =
              get_threshold(nshell) - hdist - 2.0 * get_threshold(n_min);
          if (en1 >= 0.0) {
            el_energy = DynLinArr<double>(2);
            el_energy[0] = en;
            el_energy[1] = en1;
          } else {
            el_energy = DynLinArr<double>(1);
            el_energy[0] = en;
          }
          ph_energy = DynLinArr<double>(0);
        }
      } else  // for if(main_n > 0)
          {   // principal numbers are not available, then we generate Auger
              // to outmost shell
        double en1 = get_threshold(nshell) - hdist - 2.0 * get_threshold(n_min);
        if (en1 >= 0.0) {
          el_energy = DynLinArr<double>(2);
          el_energy[0] = en;
          el_energy[1] = en1;
        } else {
          el_energy = DynLinArr<double>(1);
          el_energy[0] = en;
        }
        ph_energy = DynLinArr<double>(0);
      }
    } else  // for if(is == 0),  generate explicit channel
        {
      // Generate photo-electron and
      // just copy all what is proposed by get_channel
      // with corrections by hdist.

      el_energy = DynLinArr<double>(1 + felectron_energy.get_qel());
      el_energy[0] = en;
      long q = felectron_energy.get_qel();
      for (long n = 0; n < q; ++n) {
        check_econd21(felectron_energy[n], < 0 ||, > get_threshold(nshell),
                      mcerr);
        el_energy[1 + n] = felectron_energy[n] - hdist;
        if (el_energy[1 + n] < 0) {
          hdist = -el_energy[1 + n];
          el_energy[1 + n] = 0.0;
        } else {
          hdist = 0.0;
        }
      }
      ph_energy = DynLinArr<double>(fphoton_energy.get_qel());
      q = fphoton_energy.get_qel();
      for (long n = 0; n < q; ++n) {
        check_econd21(fphoton_energy[n], < 0 ||, > get_threshold(nshell),
                      mcerr);
        ph_energy[n] = fphoton_energy[n] - hdist;
        if (ph_energy[n] < 0) {
          hdist = -ph_energy[n];
          ph_energy[n] = 0.0;
        } else {
          hdist = 0.0;
        }
      }
    }
  }
#ifdef DEBUG_PRINT_get_escape_particles
  Iprintn(mcout, ph_energy);
  Iprintn(mcout, el_energy);
  mcout << "AtomPhotoAbsCS::get_escape_particles: exitting\n";
#endif
}

AtomicSecondaryProducts* AtomPhotoAbsCS::get_asp(int nshell) {
  mfunnamep("AtomicSecondaryProducts* AtomPhotoAbsCS::get_asp(int nshell)");
  check_econd21(nshell, < 0 ||, >= qshell, mcerr);
  return &(asp[nshell]);
}

SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(void) : AtomPhotoAbsCS() {}

SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(int fZ, const String& ffile_name)
    : file_name(ffile_name) {
  mfunnamep("SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(int fZ, const String& "
            "ffile_name)");
  check_econd11(fZ, < 1, mcerr);
#ifdef USE_STLSTRING
  std::ifstream file(file_name.c_str());
#else
  std::ifstream file(file_name);
#endif
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  while (findmark(file, "#") == 1) {
    file >> Z;
    if (Z == fZ) {
      file >> qshell;
      check_econd21(qshell, < 1 ||, > 10000, mcerr);
      s_ignore_shell.put_qel(qshell, 0);
      file >> name;
      acs = DynLinArr<ActivePtr<PhotoAbsCS> >(qshell);
      asp = DynLinArr<AtomicSecondaryProducts>(qshell);
      DynLinArr<double> fl(qshell);
      int sZshell = 0;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        double thr = 0.0;
        int Zshell = 0;
        //double fl;
        String shell_name;
        file >> thr;
        check_econd11(thr, <= 0.0, mcerr);
        file >> Zshell;
        check_econd11(Zshell, <= 0, mcerr);
        sZshell += Zshell;
        file >> fl[nshell];
        findmark(file, "!");
        file >> shell_name;
        //PhotoAbsCS* ap = new PhenoPhotoAbsCS(shell_name, Zshell, thr);
        //acs[nshell].pass( ap );
        acs[nshell].pass(new PhenoPhotoAbsCS(shell_name, Zshell, thr * 1.0e-6));
      }
      check_econd12(sZshell, !=, Z, mcerr);

      int n_min = 0;
      double st = DBL_MAX;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        // currently the minimal shell is the last,
        // but to avoid this assumption we check all
        if (get_threshold(nshell) < st) n_min = nshell;
      }
      for (int nshell = 0; nshell < qshell; ++nshell) {
        if (fl[nshell] > 0) {
          check_econd12(nshell, ==, n_min, mcerr);
          DynLinArr<double> felectron_energy(0);
          DynLinArr<double> fphoton_energy(1);
          fphoton_energy[0] = get_threshold(nshell) - get_threshold(n_min);
          asp[nshell].add_channel(fl[nshell], felectron_energy, fphoton_energy);
        }
      }
      return;
    }
  }
  funnw.ehdr(mcerr);
  mcerr << "there is no element Z=" << fZ << " in file " << file_name
        << std::endl;
  spexit(mcerr);
}

SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(int fZ, const PhotoAbsCS& facs) {
  mfunname("SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(int fZ, const "
           "PhotoAbsCS& facs)");
  check_econd11(fZ, <= 0, mcerr);
  check_econd12(fZ, !=, facs.get_Z(), mcerr);
  Z = fZ;
  qshell = 1;
  s_ignore_shell.put_qel(qshell, 0);
  name = facs.get_name();
  acs.put_qel(1);
  acs[0].put(&facs);
}

double SimpleAtomPhotoAbsCS::get_threshold(int nshell) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_threshold(int nshell) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  return acs[nshell]->get_threshold();
}

double SimpleAtomPhotoAbsCS::get_ACS(double energy) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_ACS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n] == 0) {
      s += acs[n]->get_CS(energy);
    }
  }
  return s;
}
double SimpleAtomPhotoAbsCS::get_integral_ACS(double energy1,
                                              double energy2) const {
  mfunnamep(
      "double SimpleAtomPhotoAbsCS::get_integral_ACS(double energy)const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n] == 0) {
      double t;
      s += (t = acs[n]->get_integral_CS(energy1, energy2));
      if (t < 0) {
        funnw.ehdr(mcout);
        mcout << "t < 0\n";
        Iprintn(mcout, t);
        print(mcout, 4);
        spexit(mcout);
      }
    }
    //check_econd11a(t , < 0 , "n="<<n<<'\n',  mcerr);
  }
  return s;
}

double SimpleAtomPhotoAbsCS::get_ACS(int nshell, double energy) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_ACS(int nshell, double energy)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell] == 0) {
    return acs[nshell]->get_CS(energy);
  }
  return 0.0;
}

double SimpleAtomPhotoAbsCS::get_integral_ACS(int nshell, double energy1,
                                              double energy2) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_integral_ACS(int nshell, double "
           "energy1, double energy2)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell] == 0) {
    return acs[nshell]->get_integral_CS(energy1, energy2);
  }
  return 0.0;
}

double SimpleAtomPhotoAbsCS::get_ICS(double energy) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_ICS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n] == 0) {
      s += acs[n]->get_CS(energy);
    }
  }
  return s;
}

double SimpleAtomPhotoAbsCS::get_integral_ICS(double energy1,
                                              double energy2) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_integral_ICS(double energy1, "
           "double energy2) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n] == 0) {
      s += acs[n]->get_integral_CS(energy1, energy2);
    }
  }
  return s;
}

double SimpleAtomPhotoAbsCS::get_ICS(int nshell, double energy) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_ICS(int nshell, double energy)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell] == 0) {
    return acs[nshell]->get_CS(energy);
  }
  return 0.0;
}

double SimpleAtomPhotoAbsCS::get_integral_ICS(int nshell, double energy1,
                                              double energy2) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_integral_ICS(int nshell, double "
           "energy1, double energy2)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell] == 0) {
    return acs[nshell]->get_integral_CS(energy1, energy2);
  }
  return 0.0;
}

void SimpleAtomPhotoAbsCS::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "SimpleAtomPhotoAbsCS(l=" << l << "): name=" << name
          << " Z = " << Z << " qshell = " << qshell
          << " file_name=" << file_name << std::endl;
    l--;
    if (l > 0) {
      indn.n += 2;
      for (int n = 0; n < qshell; ++n) {
        Ifile << "nshell=" << n << std::endl;
        acs[n].print(file, l);
      }
      AtomPhotoAbsCS::print(file, l);
      indn.n -= 2;
    }
  }
}

int SimpleAtomPhotoAbsCS::get_main_shell_number(int nshell) const {
  mfunname("int SimpleAtomPhotoAbsCS::get_main_shell_number(int nshell) const");
  String shell_name = acs[nshell]->get_name();
#ifdef STRSTREAM_AVAILABLE
#ifdef USE_STLSTRING
  istrstream sfile(shell_name.c_str());
#else
  istrstream sfile(shell_name);
#endif
#else
  std::istringstream sfile(shell_name.c_str());
#endif
  int i = -100;
  sfile >> i;
  //Iprintn(mcout, i);
  //check_econd(i <  1  || i > 50 , "i="<<i<<" shell_name="<<shell_name<<'\n' ,
  //                 mcerr);
  if (i < 1 || i > 50) {
    i = -1;
  }
  return i;
}

//----------------------------------------------------------------------

ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const String& fthreshold_file_name,
                                   const String& fsimple_table_file_name,
                                   const String& fname,
                                   double fminimal_threshold)
    : threshold_file_name(fthreshold_file_name),
      simple_table_file_name(fsimple_table_file_name),
      BT_file_name("none"),
      minimal_threshold(fminimal_threshold) {
  mfunnamep("SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(int fZ, const String& "
            "fname, , const String& fthreshold_file_name, const String& "
            "fsimple_table_file_name, double fminimal_threshold)");
  //Imcout<<"ExAtomPhotoAbsCS::ExAtomPhotoAbsCS is run for fZ="<<fZ<<std::endl;
  check_econd11(fZ, < 1, mcerr);
#ifdef USE_STLSTRING
  std::ifstream threshold_file(threshold_file_name.c_str());
#else
  std::ifstream threshold_file(threshold_file_name);
#endif
  if (!threshold_file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << threshold_file_name << std::endl;
    spexit(mcerr);
  }
  DynLinArr<double> thr(0, 0.0);
  DynLinArr<int> Zshell(0, 0);
  DynLinArr<double> fl(0, 0.0);
  DynLinArr<String> shell_name(0);
  while (findmark(threshold_file, "#") == 1) {
    threshold_file >> Z;
    if (Z == fZ) {
      threshold_file >> qshell;
      check_econd21(qshell, < 1 ||, > 10000, mcerr);
      s_ignore_shell.put_qel(qshell, 0);
      //Iprintn(mcout, qshell);
      thr = DynLinArr<double>(qshell, 0.0);
      Zshell = DynLinArr<int>(qshell, 0);
      fl = DynLinArr<double>(qshell, 0.0);
      shell_name = DynLinArr<String>(qshell);
      acs = DynLinArr<ActivePtr<PhotoAbsCS> >(qshell);
      asp = DynLinArr<AtomicSecondaryProducts>(qshell);
      String temp_name;
      threshold_file >> temp_name;
      if (fname == "none")
        name = temp_name;
      else
        name = fname;
      int nshell;
      int sZshell = 0;
      for (nshell = 0; nshell < qshell; nshell++) {
        threshold_file >> thr[nshell];
        check_econd11(thr[nshell], <= 0.0, mcerr);
        thr[nshell] *= 1.0e-6;
        threshold_file >> Zshell[nshell];
        check_econd11(Zshell[nshell], <= 0, mcerr);
        sZshell += Zshell[nshell];
        threshold_file >> fl[nshell];
        findmark(threshold_file, "!");
        threshold_file >> shell_name[nshell];
        //PhotoAbsCS* ap = new PhenoPhotoAbsCS(shell_name, Zshell, thr);
        //acs[nshell].pass( ap );
      }
      check_econd12(sZshell, !=, Z, mcerr);
      int n_min = 0;
      double st = DBL_MAX;
      for (nshell = 0; nshell < qshell;
           nshell++) {  // currently the minimal shell is the last,
                        // but to avoid this assumption
                        // we check all
        if (thr[nshell] < st) {
          n_min = nshell;
          st = thr[nshell];
        }
      }
      for (nshell = 0; nshell < qshell; nshell++) {
        if (fl[nshell] > 0) {
          check_econd12(nshell, ==, n_min, mcerr);
          DynLinArr<double> felectron_energy(0);
          DynLinArr<double> fphoton_energy(1);
          fphoton_energy[0] = thr[nshell] - thr[n_min];
          asp[nshell].add_channel(fl[nshell], felectron_energy, fphoton_energy);
        }
      }
      goto mark1;
    }
  }
  funnw.ehdr(mcerr);
  mcerr << "there is no element Z=" << fZ << " in file " << threshold_file_name
        << std::endl;
  spexit(mcerr);
mark1:
  // Here it reads the PACS as an one shell curve:
  SimpleTablePhotoAbsCS stpacs(name, Z, 0.0, fsimple_table_file_name);
  const DynLinArr<double>& ener = stpacs.get_arr_ener();
  const DynLinArr<double>& CS = stpacs.get_arr_CS();
  //Iprintn(mcout, ener.get_qel());
  DynLinArr<double> left_CS = CS;             // used in sequencial algorithm
  DynLinArr<DynLinArr<double> > SCS(qshell);  // here cs is saved
  int n;
  for (n = 0; n < qshell; n++)
    SCS[n] = DynLinArr<double>(ener.get_qel(), 0.0);
  /*
  if(ener[0] < thr[qshell-1])
  {
    funnw.ehdr(cerr);
    mcerr<<"ener[0] < thr[qshell-1]\n";
    mcerr<<"This possibility is not bad, but it is not implemented \n"
         <<"in the program yet, sorry\n";
    spexit(mcerr);
  }
  */
  int nct = qshell - 1;  //"current" threshold index
  long nce = 0;          // "current" energy index
                         // Let uss ignore values below the lowest threshold
  // It is not clear whether it is right, perhaps this is one of possible ways
  if (ener[0] < thr[qshell - 1]) {
    long ne;
    for (ne = 0; ne < ener.get_qel() && (ener[ne] < thr[qshell - 1] ||
                                         (ener[ne] >= thr[qshell - 1] &&
                                          ne > 1 && CS[ne - 1] <= CS[ne - 2]));
         ne++) {
      if (ne > 0) left_CS[ne - 1] = 0.0;
      nce = ne;
    }
  }
  //Iprintn(mcout, nce);
  //mcout<<"ener[nce]="<<ener[nce]<<" CS[nce]="<<CS[nce]
  //     <<"  CS[nce+1]="<<CS[nce+1]<<std::endl;
  int s_more;
  int nt2 = 0;  // < nt1
  int s_spes = 0;
  do  // Actually this is a loop by the group of thresholds
      {
    //mcout<<"nct="<<nct<<" nce="<<nce<<std::endl;
    // Find all thresholds which are less then the current energy
    int nt;
    s_more = 0;  // sign that there are thresholds more than
                 // the current energy
    for (nt = nct; nt >= 0; nt--) {
      if (s_spes == 0) {
        if (thr[nt] > ener[nce]) {
          s_more = 1;
          break;
        }
      } else {
        if (thr[nt] > ener[nce + 1]) {
          s_more = 1;
          break;
        }
      }
    }
    // nt is now index of the next threshold or -1, if the thresholds are
    // made up.
    int nt1 = nct;
    int nce_next = ener.get_qel();
    nt2 = nt + 1;
    //mcout<<"nt="<<nt<<" nt1="<<nt1<<" nt2="<<nt2<<" s_more="<<s_more<<'\n';
    if (s_more == 1) {
      //if(nt >= 0)  // so if there are other larger thresholds,
      //{        // we should check how far we can pass at this step
      long ne;
      for (ne = nce; ne < ener.get_qel();
           ne++) {  // finding energy larger than the next threshold
        if (thr[nt] <= ener[ne]) {
          nce_next = ne;
          s_spes = 0;
          break;
        } else {  // At the following condition energy could be less then
                  // threshold,
          // but this point will anyway be assotiated with the next shell
          // corresponding to this threshold.
          // This is related to not precise measurement of cross section
          // and not precise knowledge of shell energies.
          // Occurence of this condition is marked by s_spes = 1.
          // At the next passing of this loop the thresholds are compared with
          // the next energy.
          if (ne > 1 && ne < ener.get_qel() - 1 && ne > nce + 2 &&
              thr[nt] <= ener[ne + 1] &&
              (thr[nt] - ener[ne]) / (ener[ne + 1] - ener[ne]) < 0.1 &&
              CS[ne] > CS[ne - 1]) {
            //mcout<<"special condition is satisf.\n";
            nce_next = ne;
            s_spes = 1;
            break;
          }
        }
      }
      if (ne == ener.get_qel())  // threshold is larger then energy mesh
        s_more = 0;              // to finish the loop
    }
    //Iprintn(mcout, nce_next);
    //Iprintn(mcout, ener[nce_next-1]);
    int qt = nt1 - nt2 + 1;   // quantity of the new thresholds
                              //Iprintn(mcout, qt);
    DynLinArr<double> w(qt);  // weights accouring to charges
    int s = 0;                // sum of Z
    for (nt = 0; nt < qt; nt++) {
      int nshell = nct - nt;
      s += Zshell[nshell];
    }
    for (nt = 0; nt < qt; nt++) {
      int nshell = nct - nt;
      w[nt] = double(Zshell[nshell]) / s;
    }
    double save_left_CS = left_CS[nce_next - 1];
    long ne;
    for (ne = 0; ne < nce_next; ne++) {
      for (nt = 0; nt < qt; nt++) {
        int nshell = nct - nt;
        SCS[nshell][ne] = left_CS[ne] * w[nt];
      }
      left_CS[ne] = 0.0;
    }
    for (ne = nce_next; ne < ener.get_qel(); ne++) {
      double extrap_CS =
          save_left_CS * pow(ener[nce_next - 1], 2.75) / pow(ener[ne], 2.75);
      if (extrap_CS > left_CS[ne]) extrap_CS = left_CS[ne];
      for (nt = 0; nt < qt; nt++) {
        int nshell = nct - nt;
        SCS[nshell][ne] = extrap_CS * w[nt];
      }
      left_CS[ne] -= extrap_CS;
    }
    nce = nce_next;
    nct = nt2 - 1;
  } while (s_more != 0);
  // now nt2 will be index of last filled shell
  // Now to fill the shells which are absent in the input table.
  // They will be initialized phenomenologically, basing on the sum rule:
  int ns;
  //Iprintn(mcout, nt2);
  for (ns = 0; ns < nt2; ns++) {
    acs[ns].pass(new PhenoPhotoAbsCS(shell_name[ns], Zshell[ns], thr[ns]));
  }
  // Initialization of input shells:
  for (ns = nt2; ns < qshell; ns++) {
    //acs[ns].pass( new SimpleTablePhotoAbsCS(shell_name[ns], Zshell[ns],
    //                                thr[ns],
    //                                ener, SCS[ns]) );
    SimpleTablePhotoAbsCS* adr = new SimpleTablePhotoAbsCS(
        shell_name[ns], Zshell[ns], thr[ns], ener, SCS[ns]);
    adr->remove_leading_zeros();
    acs[ns].pass(adr);
  }
  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;
  double integ = get_integral_ACS(0.0, DBL_MAX);
  //Iprintn(mcout, integ);
  integ_abs_before_corr = integ;
  double pred_integ = Thomas_sum_rule_const_Mb * Z;
  //Iprintn(mcout, pred_integ);
  if (pred_integ > integ) {
    if (s_add_excitations_to_normalize == 1) {
      // add excitation
      exener[0] =
          low_boundary_of_excitations * acs[qshell - 1]->get_threshold();
      exener[1] = 1.0 * acs[qshell - 1]->get_threshold();
      height_of_excitation = (pred_integ - integ) / (exener[1] - exener[0]);
      if (minimal_threshold > 0.0) {
        if (minimal_threshold > acs[qshell - 1]->get_threshold()) {
          // currently the minimal shell is the last one
          exener[0] += minimal_threshold - acs[qshell - 1]->get_threshold();
          exener[1] += minimal_threshold - acs[qshell - 1]->get_threshold();
        }
      }
    }
    /*
    else
    {
      height_of_excitation = 0.0;
      exener[0] = exener[1] = 0.0;
    }
    */
  } else if (pred_integ < integ) {
    if (s_scale_to_normalize_if_more == 1) {
      const double fact = pred_integ / integ;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        acs[nshell]->scale(fact);
      }
    }
  }

  integ_abs_after_corr = get_integral_ACS(0.0, DBL_MAX);
  integ_ioniz_after_corr = get_integral_ICS(0.0, DBL_MAX);
}

ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const String& fname,
                                   const String& fBT_file_name, int id,
                                   double fminimal_threshold)
    : threshold_file_name("none"),
      simple_table_file_name("none"),
      BT_file_name(fBT_file_name),
      minimal_threshold(fminimal_threshold) {
  mfunnamep("ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const String& fname, "
            "const String& fBT_file_name, int id, double fminimal_threshold)");
  check_econd11(fZ, < 1, mcerr);
  check_econd21(id, < 1 ||, > 2, mcerr);

  name = fname;
#ifdef USE_STLSTRING
  std::ifstream BT_file(BT_file_name.c_str());
#else
  std::ifstream BT_file(BT_file_name);
#endif
  if (!BT_file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << BT_file_name << std::endl;
    spexit(mcerr);
  }
  DynLinArr<double> thresh(0, 0.0);
  DynLinArr<double> fl(0, 0.0);
  Z = fZ;
  int i = findmark(BT_file, "NUCLEAR CHARGE =");
  check_econd11a(i, != 1, "wrong file format", mcerr);
  int Z_from_file;
  BT_file >> Z_from_file;
  check_econd12(Z_from_file, !=, Z, mcerr);
  qshell = 0;
  while ((i = findmark(BT_file, "Z =")) == 1) {
    BT_file >> i;
    check_econd11(i, != Z, mcerr);
    String shellname;
    BT_file >> shellname;
    //Iprintn(mcout, shellname);
    i = findmark(BT_file, "$");
    check_econd11(i, != 1, mcerr);
    long qen;
    BT_file >> qen;
    check_econd11(qen, <= 0, mcerr);
    DynLinArr<double> fener(qen, 0.0);
    DynLinArr<double> fcs(qen, 0.0);
    double thr = 0.0;
    BT_file >> thr;
    check_econd11(thr, <= 0, mcerr);
    thr *= 1.0e-3;  // pass from keV to MeV
    if (id == 2) {
      thresh.increment();
      fl.increment();
      thresh[qshell] = thr;
      BT_file >> fl[qshell];
      check_econd21(fl[qshell], < 0.0 ||, > 1.0, mcerr);
      //Iprintn(mcout, fl[qshell]);
    }
    long nen;
    for (nen = 0; nen < qen; nen++) {
      BT_file >> fener[nen] >> fcs[nen];
      check_econd11(fener[nen], <= 0.0, mcerr);
      check_econd11(fcs[nen], < 0.0, mcerr);
      fener[nen] *= 1.0e-3;  // pass from keV to MeV
    }
    qshell++;
    acs.put_qel(qshell);
    acs[qshell - 1]
        .pass(new SimpleTablePhotoAbsCS(shellname, 0,  // unknown here
                                        thr, fener, fcs));
  }
  if (id == 2) {
    // a copy of similar thing from subroutine above
    int n_min = 0;
    double st = DBL_MAX;
    for (int nshell = 0; nshell < qshell; ++nshell) {
      // currently the minimal shell is the last,
      // but to avoid this assumption we check all
      if (thresh[nshell] < st) {
        n_min = nshell;
        st = thresh[nshell];
      }
    }
    asp = DynLinArr<AtomicSecondaryProducts>(qshell);
    for (int nshell = 0; nshell < qshell; ++nshell) {
      if (fl[nshell] > 0) {
        check_econd12(nshell, ==, n_min, mcerr);
        DynLinArr<double> felectron_energy(0);
        DynLinArr<double> fphoton_energy(1);
        fphoton_energy[0] = thresh[nshell] - thresh[n_min];
        asp[nshell].add_channel(fl[nshell], felectron_energy, fphoton_energy);
      }
    }
  }

  check_econd11(qshell, <= 0, mcerr);
  s_ignore_shell.put_qel(qshell, 0);
  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;
  double integ = get_integral_ACS(0.0, DBL_MAX);
  //Iprintn(mcout, integ);
  integ_abs_before_corr = integ;
  double pred_integ = Thomas_sum_rule_const_Mb * Z;
  //Iprintn(mcout, pred_integ);
  if (pred_integ > integ) {
    if (s_add_excitations_to_normalize == 1) {
      // add excitation
      exener[0] =
          low_boundary_of_excitations * acs[qshell - 1]->get_threshold();
      exener[1] = 1.0 * acs[qshell - 1]->get_threshold();
      height_of_excitation = (pred_integ - integ) / (exener[1] - exener[0]);
      if (minimal_threshold > 0.0) {
        if (minimal_threshold > acs[qshell - 1]->get_threshold()) {
          // currently the minimal shell is the last one
          exener[0] += minimal_threshold - acs[qshell - 1]->get_threshold();
          exener[1] += minimal_threshold - acs[qshell - 1]->get_threshold();
        }
      }
    }
  } else {
    if (s_scale_to_normalize_if_more == 1) {
      const double fact = pred_integ / integ;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        acs[nshell]->scale(fact);
      }
    }
  }
  integ_abs_after_corr = get_integral_ACS(0.0, DBL_MAX);
  integ_ioniz_after_corr = get_integral_ICS(0.0, DBL_MAX);
}

#define READ_FILE_WITH_PRINCIPAL_NUMBERS

ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const String& fname,
                                   const String& fFitBT_file_name,
                                   int id,  // to distinguish it from
                                   // constructor above
                                   int s_no_scale, double fminimal_threshold)
    : threshold_file_name("none"),
      simple_table_file_name("none"),
      BT_file_name(fFitBT_file_name),
      minimal_threshold(fminimal_threshold) {
  mfunnamep(
      "ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const String& fname, const "
      "String& fFitBT_file_name, int id, int id1, double fminimal_threshold)");
  check_econd11(fZ, < 1, mcerr);
  check_econd21(id, < 1 ||, > 2, mcerr);
  Z = fZ;
  name = fname;
#ifdef USE_STLSTRING
  std::ifstream BT_file(fFitBT_file_name.c_str());
#else
  std::ifstream BT_file(fFitBT_file_name);
#endif
  if (!BT_file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << BT_file_name << std::endl;
    spexit(mcerr);
  }
  DynLinArr<double> thresh(0, 0.0);
  DynLinArr<double> fl(0, 0.0);

  int i = 0;
  while ((i = findmark(BT_file, "$")) == 1) {
    long iZ;
    BT_file >> iZ;
    //Iprintn(mcout, iZ);
    if (iZ == Z) {
      BT_file >> qshell;
      //Iprintn(mcout, qshell);
      check_econd11(qshell, <= 0, mcerr);
      check_econd11(qshell, > 1000, mcerr);
      acs.put_qel(qshell);
      if (id == 2) {
        thresh.put_qel(qshell);
        fl.put_qel(qshell);
      }
      for (int nshell = 0; nshell < qshell; ++nshell) {
#ifdef READ_FILE_WITH_PRINCIPAL_NUMBERS
        int n_princ = 0;
#endif
        int l;
        double threshold;
        double E0;
        double yw;
        double ya;
        double P;
        double sigma;
        if (BT_file.eof()) {
          mcerr << "unexpected end of file " << BT_file_name << '\n';
          spexit(mcerr);
        }
        if (!BT_file.good()) {
          mcerr << "bad format of file " << BT_file_name << '\n';
          spexit(mcerr);
        }
#ifdef READ_FILE_WITH_PRINCIPAL_NUMBERS
        BT_file >> n_princ;
        if (!BT_file.good()) {
          mcerr << "bad format of file " << BT_file_name << '\n';
          spexit(mcerr);
        }
        check_econd21(n_princ, < 0 ||, > 10, mcerr);
#endif
        BT_file >> l >> threshold >> E0 >> sigma >> ya >> P >> yw;
        check_econd11(l, < 0, mcerr);
        check_econd11(l, > 20, mcerr);
        threshold *= 1.0e-6;
        E0 *= 1.0e-6;

        check_econd11a(threshold, <= 2.0e-6,
                       "n_princ=" << n_princ << " l=" << l << '\n', mcerr);
        check_econd11(E0, <= 0, mcerr);
        double flu = 0.0;
        if (id == 2) {
          if (BT_file.eof()) {
            mcerr << "unexpected end of file " << BT_file_name << '\n';
            spexit(mcerr);
          }
          if (!BT_file.good()) {
            mcerr << "bad format of file " << BT_file_name << '\n';
            spexit(mcerr);
          }
          BT_file >> flu;
          check_econd11(flu, < 0.0, mcerr);
          check_econd11(flu, > 1.0, mcerr);
          thresh[nshell] = threshold;
          fl[nshell] = flu;
        }
#ifdef READ_FILE_WITH_PRINCIPAL_NUMBERS
        String shellname(long_to_String(n_princ) +  // necessary
                         // for generation escape products
                         String(" shell number ") + long_to_String(nshell));
#else
        String shellname(String("shell number ") + long_to_String(nshell));
#endif
        acs[nshell].pass(
            new SimpleTablePhotoAbsCS(shellname, 0,  // unknown here
                                      threshold, l, E0, yw, ya, P, sigma));
        //Iprintn(mcout, nshell);
        //Iprint3n(mcout, l, threshold, E0);
        //Iprint4n(mcout, yw, ya, P, sigma);
        //acs[nshell]->print(mcout, 5);
      }
      goto mark1;
    }
  }
  funnw.ehdr(mcerr);
  mcerr << "there is no element Z=" << fZ << " in file " << fFitBT_file_name
        << std::endl;
  spexit(mcerr);
mark1:
  if (id == 2) {
    // a copy of similar thing from subroutine above
    int n_min = 0;
    double st = DBL_MAX;
    for (int nshell = 0; nshell < qshell; ++nshell) {
      // currently the minimal shell is the last,
      // but to avoid this assumption we check all
      if (thresh[nshell] < st) {
        n_min = nshell;
        st = thresh[nshell];
      }
    }
    asp = DynLinArr<AtomicSecondaryProducts>(qshell);
    for (int nshell = 0; nshell < qshell; ++nshell) {
      if (fl[nshell] > 0) {
        check_econd12(nshell, ==, n_min, mcerr);
        DynLinArr<double> felectron_energy(0);
        DynLinArr<double> fphoton_energy(1);
        fphoton_energy[0] = thresh[nshell] - thresh[n_min];
        asp[nshell].add_channel(fl[nshell], felectron_energy, fphoton_energy);
      }
    }
  }

  check_econd11(qshell, <= 0, mcerr);
  s_ignore_shell.put_qel(qshell, 0);
  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;
  double integ = get_integral_ACS(0.0, DBL_MAX);
  //Iprintn(mcout, integ);
  integ_abs_before_corr = integ;
  double pred_integ = Thomas_sum_rule_const_Mb * Z;
  //Iprintn(mcout, pred_integ);
  if (pred_integ > integ) {
    if (s_add_excitations_to_normalize == 1) {
      // add excitation
      exener[0] =
          low_boundary_of_excitations * acs[qshell - 1]->get_threshold();
      exener[1] = 1.0 * acs[qshell - 1]->get_threshold();
      height_of_excitation = (pred_integ - integ) / (exener[1] - exener[0]);
      if (minimal_threshold > 0.0) {
        if (minimal_threshold > acs[qshell - 1]->get_threshold()) {
          // currently the minimal shell is the last one
          exener[0] += minimal_threshold - acs[qshell - 1]->get_threshold();
          exener[1] += minimal_threshold - acs[qshell - 1]->get_threshold();
        }
      }
    }
  } else {
    if (s_scale_to_normalize_if_more == 1 && s_no_scale == 0) {
      const double fact = pred_integ / integ;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        acs[nshell]->scale(fact);
      }
    }
  }
  integ_abs_after_corr = get_integral_ACS(0.0, DBL_MAX);
  integ_ioniz_after_corr = get_integral_ICS(0.0, DBL_MAX);
}

ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(
    int fZ, const String& fname,  // just name of this atom
    const String& fFitBT_file_name,
    // const String& fthreshold_file_name, // minimal potential corresponding
    // to:
    const String& fsimple_table_file_name,
    double emax_repl, int id,     // to distinguish it from constructor above
    double fminimal_threshold) {
  mfunname("ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(...)");
  Z = fZ;
  name = fname;
  int s_no_scale = 1;
  *this =
      ExAtomPhotoAbsCS(fZ, fname,  // just name of this atom
                       fFitBT_file_name, id, s_no_scale, fminimal_threshold);

  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;

  double thrmin = DBL_MAX;
  long nsmin = -1;
  // look for minimal shell (usually the last)
  for (long ns = 0; ns < qshell; ++ns) {
    //Iprint2n(mcout, ns, acs[ns]->get_threshold());
    if (thrmin > acs[ns]->get_threshold()) {
      nsmin = ns;
      thrmin = acs[ns]->get_threshold();
    }
  }
  //Iprint3n(mcout, nsmin, acs[nsmin]->get_threshold(), thrmin);
  check_econd11(nsmin, < 0, mcerr);
  check_econd11(nsmin, != qshell - 1, mcerr);  // now it has to be by this way
  ActivePtr<PhotoAbsCS> facs = acs[nsmin];     // copying the valence shell
  PhotoAbsCS* apacs = facs.get();
  //PhotoAbsCS* apacs = acs[nsmin]->getver();
  SimpleTablePhotoAbsCS* first_shell =
      dynamic_cast<SimpleTablePhotoAbsCS*>(apacs);
  //mcout<<"first_shell:\n";
  //first_shell->print(mcout, 2);
  // Strange why the following does not work (because of something being not
  // implemented in commpiler)? - may be due to an error before, which is now
  // corrected.
  // assumes the first is the last
  // SimpleTablePhotoAbsCS* first_shell =
  // dynamic_cast<SimpleTablePhotoAbsCS*>(&(acs[nsmin]->getver()));

  check_econd11(first_shell, == NULL, mcerr);
  /*
#ifdef USE_STLSTRING
  std::ifstream threshold_file(threshold_file_name.c_str());
#else
  std::ifstream threshold_file(threshold_file_name);
#endif
  if( !threshold_file )
  {
    funnw.ehdr(mcerr);
    mcerr<<"cannot open file "<<threshold_file_name<<std::endl;
    spexit(mcerr);
  }
  DynLinArr< double > thr(0, 0.0);
  DynLinArr< int > Zshell(0, 0);
  DynLinArr< double > fl(0, 0.0);
  DynLinArr< String > shell_name(0);
  int n_min=0;
  double thr_min = DBL_MAX;
  while (findmark(threshold_file, "#") == 1) {
    threshold_file >> Z;
    if (Z == fZ) {
      threshold_file >> qshell;
      check_econd21( qshell , < 1 || , > 10000 , mcerr);
      s_ignore_shell.put_qel(qshell, 0);
      //Iprintn(mcout, qshell);
      thr = DynLinArr< double >(qshell, 0.0);
      Zshell = DynLinArr< int >(qshell, 0);
      fl = DynLinArr< double >(qshell, 0.0);
      shell_name = DynLinArr< String >(qshell);
      String temp_name;
      threshold_file >> temp_name;
      if(fname == "none") name = temp_name;
      else name = fname;
      int sZshell = 0;
      for (int nshell = 0; nshell < qshell; nshell++) {
        threshold_file >> thr[nshell];
        check_econd11(thr[nshell] , <= 0.0, mcerr);
        thr[nshell] *= 1.0e-6;
        threshold_file >> Zshell[nshell];
        check_econd11(Zshell[nshell] , <= 0, mcerr);
        sZshell += Zshell[nshell];
        threshold_file >> fl[nshell];
        findmark(threshold_file, "!");
        threshold_file >> shell_name[nshell];
        //PhotoAbsCS* ap = new PhenoPhotoAbsCS(shell_name, Zshell, thr);
        //acs[nshell].pass( ap );
      }
      check_econd12( sZshell , != , Z , mcerr);
      for(nshell=0; nshell<qshell; nshell++) {
        // currently the minimal shell is the last,
        // but to avoid this assumption we check all
        if (thr[nshell] < thr_min) {
          n_min = nshell;
          thr_min = thr[nshell];
        }
      }
      goto mark1;
    }
  }
  funnw.ehdr(mcerr);
  mcerr<<"there is no element Z="<<fZ<<" in file
"<<threshold_file_name<<std::endl;
  spexit(mcerr);
  mark1:
  */

  SimpleTablePhotoAbsCS stpacs(name, Z, 0.0, fsimple_table_file_name);
  stpacs.remove_leading_tiny(1.0e-10);

  // Merging shells:
  acs[nsmin].pass(new SimpleTablePhotoAbsCS(*first_shell, stpacs, emax_repl));

  s_ignore_shell.put_qel(qshell, 0);
  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;
  double integ = get_integral_ACS(0.0, DBL_MAX);
  //Iprintn(mcout, integ);
  integ_abs_before_corr = integ;
  double pred_integ = Thomas_sum_rule_const_Mb * Z;
  //Iprintn(mcout, pred_integ);
  if (pred_integ > integ) {
    if (s_add_excitations_to_normalize == 1) {
      // add excitation
      exener[0] =
          low_boundary_of_excitations * acs[qshell - 1]->get_threshold();
      exener[1] = 1.0 * acs[qshell - 1]->get_threshold();
      height_of_excitation = (pred_integ - integ) / (exener[1] - exener[0]);
      if (minimal_threshold > 0.0) {
        if (minimal_threshold > acs[qshell - 1]->get_threshold()) {
          // currently the minimal shell is the last one
          exener[0] += minimal_threshold - acs[qshell - 1]->get_threshold();
          exener[1] += minimal_threshold - acs[qshell - 1]->get_threshold();
        }
      }
    }
  } else {
    if (s_scale_to_normalize_if_more == 1) {
      const double fact = pred_integ / integ;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        acs[nshell]->scale(fact);
      }
    }
  }
  integ_abs_after_corr = get_integral_ACS(0.0, DBL_MAX);
  integ_ioniz_after_corr = get_integral_ICS(0.0, DBL_MAX);
}

double ExAtomPhotoAbsCS::get_threshold(int nshell) const {
  mfunname("double ExAtomPhotoAbsCS::get_threshold(int nshell) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  double r = acs[nshell]->get_threshold();
  if (minimal_threshold > 0.0) {
    if (r < minimal_threshold) r = minimal_threshold;
  }
  return r;
}

double ExAtomPhotoAbsCS::get_ICS(double energy) const {
  mfunname("double ExAtomPhotoAbsCS::get_ACS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n] == 0) {
      double shift = 0.0;
      const double t = acs[n]->get_threshold();
      if (minimal_threshold > 0.0) {
        if (t < minimal_threshold) shift = minimal_threshold - t;
      }
      //Iprint3n(mcout, n, t, shift);
      s += acs[n]->get_CS(energy - shift);
      //Iprintn(mcout, s);
    }
  }
  return s;
}

double ExAtomPhotoAbsCS::get_integral_ICS(double energy1,
                                          double energy2) const {
  mfunname("double ExAtomPhotoAbsCS::get_integral_ICS(double energy)const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n] == 0) {
      double shift = 0.0;
      const double t = acs[n]->get_threshold();
      if (minimal_threshold > 0.0) {
        if (t < minimal_threshold) shift = minimal_threshold - t;
      }
      s += acs[n]->get_integral_CS(energy1 - shift, energy2 - shift);
    }
  }
  return s;
}

double ExAtomPhotoAbsCS::get_ICS(int nshell, double energy) const {
  mfunname("double ExAtomPhotoAbsCS::get_ICS(int nshell, double energy)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell] == 0) {
    double shift = 0.0;
    const double t = acs[nshell]->get_threshold();
    if (minimal_threshold > 0.0) {
      if (t < minimal_threshold) shift = minimal_threshold - t;
    }
    return acs[nshell]->get_CS(energy - shift);
  }
  return 0.0;
}

double ExAtomPhotoAbsCS::get_integral_ICS(int nshell, double energy1,
                                          double energy2) const {
  mfunname("double ExAtomPhotoAbsCS::get_integral_ICS(int nshell, double "
           "energy1, double energy2)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell] == 0) {
    double shift = 0.0;
    const double t = acs[nshell]->get_threshold();
    if (minimal_threshold > 0.0) {
      if (t < minimal_threshold) shift = minimal_threshold - t;
    }
    return acs[nshell]->get_integral_CS(energy1 - shift, energy2 - shift);
  }
  return 0.0;
}

double ExAtomPhotoAbsCS::get_ACS(double energy) const {
  mfunname("double ExAtomPhotoAbsCS::get_ACS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n] == 0) {
      double shift = 0.0;
      const double t = acs[n]->get_threshold();
      if (minimal_threshold > 0.0) {
        if (t < minimal_threshold) shift = minimal_threshold - t;
      }
      s += acs[n]->get_CS(energy - shift);
    }
  }
  if (energy >= exener[0] && energy <= exener[1]) {
    s += height_of_excitation;
  }
  return s;
}

double ExAtomPhotoAbsCS::get_integral_ACS(double energy1,
                                          double energy2) const {
  mfunname("double ExAtomPhotoAbsCS::get_integral_ACS(double energy1, double "
           "energy2) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n] == 0) {
      double shift = 0.0;
      const double t = acs[n]->get_threshold();
      if (minimal_threshold > 0.0) {
        if (t < minimal_threshold) shift = minimal_threshold - t;
      }
      s += acs[n]->get_integral_CS(energy1 - shift, energy2 - shift);
    }
  }
  //Imcout<<"energy1="<<setw(13)<<energy1
  //        <<"energy2="<<setw(13)<<energy2
  //        <<" s="<<s<<'\n';
  double b[2];
  b[0] = find_max(exener[0], energy1);
  b[1] = find_min(exener[1], energy2);
  //Iprint3n(mcout, b[0], b[1], height_of_excitation);
  if (b[1] >= b[0]) {
    s += height_of_excitation * (b[1] - b[0]);
  }
  //Iprintn(mcout, s);
  return s;
}

double ExAtomPhotoAbsCS::get_ACS(int nshell, double energy) const {
  mfunname("double ExAtomPhotoAbsCS::get_ACS(int nshell, double energy)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell] == 0) {
    double shift = 0.0;
    const double t = acs[nshell]->get_threshold();
    if (minimal_threshold > 0.0) {
      if (t < minimal_threshold) shift = minimal_threshold - t;
    }
    double s = acs[nshell]->get_CS(energy - shift);
    if (nshell == qshell - 1 && energy >= exener[0] && energy <= exener[1]) {
      s += height_of_excitation;
    }
    return s;
  }
  return 0.0;
}

double ExAtomPhotoAbsCS::get_integral_ACS(int nshell, double energy1,
                                          double energy2) const {
  mfunname("double ExAtomPhotoAbsCS::get_integral_ACS(int nshell, double "
           "energy1, double energy2)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell] == 0) {
    double shift = 0.0;
    const double t = acs[nshell]->get_threshold();
    if (minimal_threshold > 0.0) {
      if (t < minimal_threshold) shift = minimal_threshold - t;
    }
    double s = acs[nshell]->get_integral_CS(energy1 - shift, energy2 - shift);
    //Imcout<<"energy1="<<setw(13)<<energy1<<" s="<<s<<'\n';
    if (nshell == qshell - 1) {
      double b[2];
      b[0] = find_max(exener[0], energy1);
      b[1] = find_min(exener[1], energy2);
      if (b[1] >= b[0]) {
        s += height_of_excitation * (b[1] - b[0]);
      }
    }
    return s;
  }
  return 0.0;
}

void ExAtomPhotoAbsCS::print(std::ostream& file, int l) const {
  if (l > 0) {
    Ifile << "ExAtomPhotoAbsCS(l=" << l << "): name=" << name << " Z = " << Z
          << " qshell = " << qshell << std::endl;
    indn.n += 2;
    Ifile << "threshold_file_name=" << threshold_file_name << '\n';
    Ifile << "simple_table_file_name=" << simple_table_file_name << '\n';
    Ifile << "BT_file_name=" << BT_file_name << std::endl;
    Ifile << "Thomas_sum_rule_const_Mb * Z = " << Thomas_sum_rule_const_Mb* Z
          << '\n';
    Ifile << "integ_abs_before_corr        = " << integ_abs_before_corr << '\n';
    Ifile << "integ_abs_after_corr         = " << integ_abs_after_corr << '\n';
    Ifile << "integ_ioniz_after_corr       = " << integ_ioniz_after_corr
          << '\n';
    Ifile << "height_of_excitation=" << height_of_excitation
          << " exener=" << exener[0] << ' ' << exener[1] << '\n';
    Iprintn(file, minimal_threshold);
    Ifile << "integrals by shells:\n";
    Ifile << "nshell, int(acs), int(ics)\n";
    for (long n = 0; n < qshell; n++) {
      double ainteg = get_integral_ACS(n, 0.0, DBL_MAX);
      double iinteg = get_integral_ICS(n, 0.0, DBL_MAX);
      Ifile << n << "    " << ainteg << "    " << iinteg << '\n';
    }

    if (l > 1) {
      l--;
      indn.n += 2;
      for (long n = 0; n < qshell; ++n) {
        Ifile << "nshell=" << n << std::endl;
        acs[n].print(file, l);
      }
      AtomPhotoAbsCS::print(file, l);
      indn.n -= 2;
    }
    indn.n -= 2;
  }
}

int ExAtomPhotoAbsCS::get_main_shell_number(int nshell) const {
  mfunname("int ExAtomPhotoAbsCS::get_main_shell_number(int nshell) const");
  String shell_name = acs[nshell]->get_name();
#ifdef STRSTREAM_AVAILABLE
#ifdef USE_STLSTRING
  istrstream sfile(shell_name.c_str());
#else
  istrstream sfile(shell_name);
#endif
#else
  std::istringstream sfile(shell_name.c_str());
#endif
  int i = -1;
  sfile >> i;
  //Iprintn(mcout, i);
  //check_econd(i <  1  || i > 50 , "i="<<i<<" shell_name="<<shell_name<<'\n' ,
  //                 mcerr);
  if (i < 1 || i > 50) {
    i = -1;
  }
  return i;
}

void ExAtomPhotoAbsCS::replace_shells_by_overage(double fwidth,  // MeV
                                                 double fstep,
                                                 long fmax_q_step) {
  mfunname("void ExAtomPhotoAbsCS::replace_shells_by_overage(...)");
  for (long n = 0; n < qshell; n++) {
    //Iprintn(mcout, n);
    //mcout<<"----------------before replacement:\n";
    //acs[n]->print(mcout, 10);
    PhotoAbsCS* a =
        new OveragePhotoAbsCS(acs[n].getver(), fwidth, fstep, fmax_q_step);
    //mcout<<"----------------after replacement:\n";
    //acs[n]->print(mcout, 10);
    acs[n].pass(a);
  }
}

//---------------------------------------------------------

MolecPhotoAbsCS::MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom, int fqatom,
                                 double fW, double fF)
    : W(fW), F(fF) {
  qatom = fqatom;
  qatom_ps.put_qel(1, qatom);
  atom.put_qel(1, PassivePtr<const AtomPhotoAbsCS>(&fatom));
  if (W == 0.0) {
    W = coef_I_to_W * atom[0]->get_I_min();
  }
}

MolecPhotoAbsCS::MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom1, int fqatom_ps1,
                                 const AtomPhotoAbsCS& fatom2, int fqatom_ps2,
                                 double fW, double fF)
    : W(fW), F(fF) {
  qatom = fqatom_ps1 + fqatom_ps2;
  qatom_ps.put_qel(2);
  atom.put_qel(2);
  qatom_ps[0] = fqatom_ps1;
  qatom_ps[1] = fqatom_ps2;
  atom[0] = fatom1;
  atom[1] = fatom2;
  if (W == 0.0) {
#ifdef CALC_W_USING_CHARGES
    W = coef_I_to_W * (qatom_ps[0] * atom[0]->get_Z() * atom[0]->get_I_min() +
                       qatom_ps[1] * atom[1]->get_Z() * atom[1]->get_I_min()) /
        (qatom_ps[0] * atom[0]->get_Z() + qatom_ps[1] * atom[1]->get_Z());
#else
    W = coef_I_to_W * (qatom_ps[0] * atom[0]->get_I_min() +
                       qatom_ps[1] * atom[1]->get_I_min()) / qatom;
#endif
  }
}

MolecPhotoAbsCS::MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom1, int fqatom_ps1,
                                 const AtomPhotoAbsCS& fatom2, int fqatom_ps2,
                                 const AtomPhotoAbsCS& fatom3, int fqatom_ps3,
                                 double fW, double fF)
    : W(fW), F(fF) {
  qatom = fqatom_ps1 + fqatom_ps2 + fqatom_ps3;
  qatom_ps.put_qel(3);
  atom.put_qel(3);
  qatom_ps[0] = fqatom_ps1;
  qatom_ps[1] = fqatom_ps2;
  qatom_ps[2] = fqatom_ps3;
  atom[0] = fatom1;
  atom[1] = fatom2;
  atom[2] = fatom3;
  if (W == 0.0) {
#ifdef CALC_W_USING_CHARGES
    W = coef_I_to_W * (qatom_ps[0] * atom[0]->get_Z() * atom[0]->get_I_min() +
                       qatom_ps[1] * atom[1]->get_Z() * atom[1]->get_I_min() +
                       qatom_ps[2] * atom[2]->get_Z() * atom[2]->get_I_min()) /
        (qatom_ps[0] * atom[0]->get_Z() + qatom_ps[1] * atom[1]->get_Z() +
         qatom_ps[2] * atom[2]->get_Z());
#else
    W = coef_I_to_W * (qatom_ps[0] * atom[0]->get_I_min() +
                       qatom_ps[1] * atom[1]->get_I_min() +
                       qatom_ps[2] * atom[2]->get_I_min()) / qatom;
#endif
  }
}

double MolecPhotoAbsCS::get_ACS(double energy) const {
  mfunname("double MolecPhotoAbsCS::get_ACS(double energy) const");
  const long q = qatom_ps.get_qel();
  double s = 0.0;
  for (long n = 0; n < q; n++) {
    s += qatom_ps[n] * atom[n]->get_ACS(energy);
  }
  return s;
}

double MolecPhotoAbsCS::get_integral_ACS(double energy1, double energy2) const {
  mfunname("double MolecPhotoAbsCS::get_integral_ACS(double energy1, double "
           "energy2) const");
  const long q = qatom_ps.get_qel();
  double s = 0.0;
  for (long n = 0; n < q; n++) {
    s += qatom_ps[n] * atom[n]->get_integral_ACS(energy1, energy2);
  }
  return s;
}

double MolecPhotoAbsCS::get_ICS(double energy) const {
  mfunname("double MolecPhotoAbsCS::get_ICS(double energy) const");
  const long q = qatom_ps.get_qel();
  double s = 0.0;
  for (long n = 0; n < q; n++) {
    s += qatom_ps[n] * atom[n]->get_ICS(energy);
  }
  return s;
}

double MolecPhotoAbsCS::get_integral_ICS(double energy1, double energy2) const {
  mfunname("double MolecPhotoAbsCS::get_integral_ICS(double energy1, double "
           "energy2) const");
  const long q = qatom_ps.get_qel();
  double s = 0.0;
  for (long n = 0; n < q; n++) {
    s += qatom_ps[n] * atom[n]->get_integral_ICS(energy1, energy2);
  }
  return s;
}

int MolecPhotoAbsCS::get_total_Z() const {
  mfunname("int MolecPhotoAbsCS::get_total_Z() const");
  int s = 0;
  const long q = qatom_ps.get_qel();
  for (long n = 0; n < q; n++) {
    s += atom[n]->get_Z();
  }
  return s;
}

void MolecPhotoAbsCS::print(std::ostream& file, int l) const {
  Ifile << "MolecPhotoAbsCS (l=" << l << "):\n";
  Iprintn(file, qatom);
  Iprintn(file, W);
  Iprintn(file, F);
  const long q = qatom_ps.get_qel();
  Ifile << "number of sorts of atoms is " << q << '\n';
  indn.n += 2;
  for (long n = 0; n < q; n++) {
    Ifile << "n=" << n << " qatom_ps[n]=" << qatom_ps[n] << " atom:\n";
    atom[n]->print(file, l);
  }
  indn.n -= 2;
}

std::ostream& operator<<(std::ostream& file, const MolecPhotoAbsCS& f) {
  f.print(file, 1);
  return file;
}

/*
PhotoAbsorptionCS::PhotoAbsorptionCS(void):sqh(0) {};

PhotoAbsorptionCS::PhotoAbsorptionCS(const String& ffile_name, PassivePtr<
EnergyMesh > fenergy_mesh) {
  mfunname("PhotoAbsorptionCS::PhotoAbsorptionCS(const String& ffile_name,
PassivePtr< EnergyMesh > fenergy_mesh)");

  std::ifstream file(ffile_name);
  if (!file) {
    mcerr<<" can not open file "<<ffile_name<<'\n';
    spexit(mcerr);
  }
  file>>z;
  check_econd12(z , < 1 || , >= max_poss_atom_z , mcerr);
  file>>qsh;
  check_econd12(qsh , < 1 || , >= z , mcerr);
  String atname;
  file >> atname;
  shell_energy = DinLinArr< double >(qsh, 0.0);
  fluorescence_yield = DinLinArr< double >(qsh, 0.0);
  z_shell = DinLinArr< int >(qsh, 0);

  for (int nsh = qsh-1; nsh >= 0; nsh--) {
    file >> shell_energy[nsh] >> z_shell[nsh] >> fluorescence_yield[nsh];
    check_econd11(shell_energy[nsh] , >  0.0 , mcerr);
    check_econd12(z_shell[nsh] , < 1 || , >= z , mcerr);
    check_econd12(fluorescence_yield[nsh] , < 0.0 || , > 1.0 , mcerr);
    int ci;  // now pass comments
    while( !((ci=getc(fli))=='\n') ) {
      check_econd11( ci , == EOF , mcerr);
    }
  }
  long qen;
  file >> qen;
  check_econd11(qen , < 1 ,  mcerr);
  DinLinArr< double >en(qen, 0.0);
  total_cs = DinLinArr< double >(qen, 0.0);
  shell_cs = DinArr< double >(qsh, qen, 0.0);
  for (long nen = 0; nen<qen; ++nen) {
    file>>en[nen];
    if(nen > 0) {
      check_econd11(en[nen] , < en[nen-1] ,  mcerr);
    }
    file>>total_cs[nen];
    check_econd11(total_cs[nen] , < 0.0 ,  mcerr);
    double s = 0;
    for (nsh = 0; nsh < qsh; nsh++) {
      file>>shell_cs[nsh][nen];
      check_econd11(shell_cs[nen] , < 0.0 ,  mcerr);
      s+=shell_cs[nsh][nen];
    }
    s = fabs((total_cs[nen] - s)/(total_cs[nen] + s));
    check_econd11(s , > 0.001 ,  mcerr);  // precision of printing
  }
  *fenergy_mesh = EnergyMesh(en);
}
*/

}
