/****************************************************************************
 * $Id: StHbtYlm.h,v 1.1 2013/01/18 14:46:02 yyang Exp $
 * **************************************************************************
 * Description: Part of StHbtCorrFctnDirectYlm utility
 *				Provide ultilities for complex and matrix process.
 *
 ***************************************************************************/

#ifndef StHbtYlm_hh
#define StHbtYlm_hh
#include <cstdlib>
#include <cmath>
#include <complex>

namespace StHbtYlm
{
	std::complex<double> Ceiphi(double phi);
	double Legendre(int ell, double ctheta);
	std::complex<double> Ylm(int ell, int m, double theta, double phi);

	double Legendre(int ell, double ctheta);

	std::complex <double> Ylm(int ell, int m, double x, double y, double z);

	void YlmUpToL(int lmax, double x, double y, double z, std::complex<double>* ylms);
	void YlmUpToL(int lmax, double ctheta, double phi, std::complex<double>* ylms);

	double ReYlm(int ell, int m, double theta, double phi);
	double ReYlm(int ell, int m, double x, double y, double z);
	double ImYlm(int ell, int m, double theta, double phi);
	double ImYlm(int ell, int m, double x, double y, double z);

}

#endif

/*************************************************************************
 * $Log: StHbtYlm.h,v $
 * Revision 1.1  2013/01/18 14:46:02  yyang
 * Add ultilities for SHD of CF
 *
 ************************************************************************/
