/****************************************************************************
 * $Id: StHbtYlm.cxx,v 1.1 2013/01/18 14:46:02 yyang Exp $
 * **************************************************************************
 * Description: Part of StHbtCorrFctnDirectYlm utility
 *				Provide ultilities for complex and matrix process.
 *
 ***************************************************************************/

#include "StHbtYlm.h"
#include <gsl/gsl_sf.h>
#define LRANGE 10

std::complex<double> StHbtYlm::Ceiphi(double phi)
{
	return ( std::complex<double>( cos(phi), sin(phi) ) );
}

double StHbtYlm::Legendre(int ell, double ctheta)
{
	return ( gsl_sf_legendre_Pl(ell, ctheta) );
}

std::complex<double> StHbtYlm::Ylm(int ell, int m, double theta, double phi)
{
	double				 ctheta;
	std::complex<double> answer;
	std::complex<double> ci(0.0, 1.0);
	ctheta = cos(theta);
	answer = gsl_sf_legendre_sphPlm(ell, abs(m), ctheta) * Ceiphi(m * phi);
	if (m < 0) {
		if (abs(m) % 2) answer *= -1.0;
	}
	return (answer);
}

std::complex<double> StHbtYlm::Ylm(int ell, int m, double x, double y, double z)
{
	std::complex<double> answer;
	double				 ctheta, phi;
	double				 r = sqrt(x * x + y * y + z * z);
	if ( r < 1e-10 || fabs(z) < 1e-10 ) ctheta = 0.0;
	else ctheta = z / r;
	phi = atan2(y, x);
	answer = gsl_sf_legendre_sphPlm(ell, abs(m), ctheta) * Ceiphi(m * phi);
	if (m < 0) {
		if (abs(m) % 2) answer *= -1.0;
	}
	return (answer);
}

void StHbtYlm::YlmUpToL(int lmax, double x, double y, double z, std::complex<double>* ylms)
{
	std::complex<double> answer;
	double				 ctheta, phi;
	int					 lcur = 0;
	double				 lpol;

	double coss[LRANGE];
	double sins[LRANGE];

	double r = sqrt(x * x + y * y + z * z);
	if ( r < 1e-10 || fabs(z) < 1e-10 ) ctheta = 0.0;
	else ctheta = z / r;
	phi = atan2(y, x);

	for (int iter = 1; iter <= lmax; iter++) {
		coss[iter - 1] = cos(iter * phi);
		sins[iter - 1] = sin(iter * phi);
	}
	ylms[lcur++] = gsl_sf_legendre_sphPlm(0, 0, ctheta) * std::complex<double>(1, 0);

	for (int il = 1; il <= lmax; il++) {
		for (int im = 0; im <= il; im++) {
			lpol = gsl_sf_legendre_sphPlm(il, im, ctheta);
			if (im) {
				ylms[lcur + il + im] = lpol * std::complex<double>(coss[im - 1], sins[im - 1]);
				if (im % 2)
					ylms[lcur + il - im] = -lpol* std::complex<double>(coss[im - 1], -sins[im - 1]);
				else
					ylms[lcur + il - im] = lpol * std::complex<double>(coss[im - 1], -sins[im - 1]);
			}
			else {
				ylms[lcur + il] = lpol * std::complex<double>(1, 0);
			}
		}
		lcur += 2 * il + 1;
	}
}

void StHbtYlm::YlmUpToL(int lmax, double ctheta, double phi, std::complex<double>* ylms)
{
	int	   lcur = 0;
	double lpol;

	double coss[LRANGE];
	double sins[LRANGE];

	for (int iter = 1; iter <= lmax; iter++) {
		coss[iter - 1] = cos(iter * phi);
		sins[iter - 1] = sin(iter * phi);
	}
	ylms[lcur++] = gsl_sf_legendre_sphPlm(0, 0, ctheta) * std::complex<double>(1, 0);

	for (int il = 1; il <= lmax; il++) {
		for (int im = 0; im <= il; im++) {
			lpol = gsl_sf_legendre_sphPlm(il, im, ctheta);
			if (im) {
				ylms[lcur + il + im] = lpol * std::complex<double>(coss[im - 1], sins[im - 1]);
				if (im % 2)
					ylms[lcur + il - im] = -lpol* std::complex<double>(coss[im - 1], -sins[im - 1]);
				else
					ylms[lcur + il - im] = lpol * std::complex<double>(coss[im - 1], -sins[im - 1]);
			}
			else {
				ylms[lcur + il] = lpol * std::complex<double>(1, 0);
			}
		}
		lcur += 2 * il + 1;
	}
}

double StHbtYlm::ReYlm(int ell, int m, double theta, double phi)
{
	return ( real( StHbtYlm::Ylm(ell, m, theta, phi) ) );
}

double StHbtYlm::ImYlm(int ell, int m, double theta, double phi)
{
	return ( imag( StHbtYlm::Ylm(ell, m, theta, phi) ) );
}

double StHbtYlm::ReYlm(int ell, int m, double x, double y, double z)
{
	return ( real( StHbtYlm::Ylm(ell, m, x, y, z) ) );
}

double StHbtYlm::ImYlm(int ell, int m, double x, double y, double z)
{
	return ( imag( StHbtYlm::Ylm(ell, m, x, y, z) ) );
}

/*************************************************************************
 * $Log: StHbtYlm.cxx,v $
 * Revision 1.1  2013/01/18 14:46:02  yyang
 * Add ultilities for SHD of CF
 *
 ************************************************************************/
