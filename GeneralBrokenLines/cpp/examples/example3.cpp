/*
 * example1.cpp
 *
 *  Created on: Aug 24, 2011
 *      Author: kleinwrt
 */

/** \file
 *  Example application.
 *
 *  \author Claus Kleinwort, DESY, 2011 (Claus.Kleinwort@desy.de)
 *
 *  \copyright
 *  Copyright (c) 2011 - 2017 Deutsches Elektronen-Synchroton,
 *  Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version. \n\n
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details. \n\n
 *  You should have received a copy of the GNU Library General Public
 *  License along with this program (see the file COPYING.LIB for more
 *  details); if not, write to the Free Software Foundation, Inc.,
 *  675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <time.h>
#include "example1.h"
#include "GblTrajectory.h"

using namespace gbl;
using namespace Eigen;

Matrix5d gblSimpleJacobian3(double ds, double cosl, double bfac) {
	/// Simple jacobian: quadratic in arc length difference
	/**
	 * \param [in] ds    (3D) arc-length
	 * \param [in] cosl  cos(lambda)
	 * \param [in] bfac  Bz*c
	 * \return jacobian
	 */
	Matrix5d jac;
	jac.setIdentity();
	jac(1, 0) = -bfac * ds * cosl;
	jac(3, 0) = -0.5 * bfac * ds * ds * cosl;
	jac(3, 1) = ds;
	jac(4, 2) = ds;
	return jac;
}

double unrm3() {
	///  unit normal distribution, Box-Muller method, polar form
	static double unrm2 = 0.0;
	static bool cached = false;
	if (!cached) {
		double x, y, r;
		do {
			x = 2.0 * rand() / RAND_MAX - 1;
			y = 2.0 * rand() / RAND_MAX - 1;
			r = x * x + y * y;
		} while (r == 0.0 || r > 1.0);
		// (x,y) in unit circle
		double d = sqrt(-2.0 * log(r) / r);
		double unrm1 = x * d;
		unrm2 = y * d;
		cached = true;
		return unrm1;
	} else {
		cached = false;
		return unrm2;
	}
}

void example3() {
	/// Simple example.
	/**
	 * Create points on initial trajectory, create trajectory from points,
	 * fit and write trajectory to MP-II binary file,
	 * get track parameter corrections and covariance matrix at points.
	 *
	 * Equidistant measurement layers and thin scatterers, propagation
	 * with simple jacobian (quadratic in arc length differences).
	 * Curvilinear system (U,V,T) as local coordinate system.
	 *
	 * This example simulates and refits tracks in a system of planar detectors
	 * with 2D measurements in a constant magnet field in Z direction using
	 * the curvilinear system as local system and (Q/P, slopes, offsets) as
	 * local track parameters. The true track parameters are
	 * randomly smeared with respect to a (constant and straight) reference
	 * trajectory with direction (lambda, phi) and are used (only) for the
	 * on-the-fly simulation of the measurements and scatterers. The predictions
	 * from the reference trajectory are therefore always zero and the residuals
	 * needed (by addMeasurement) are equal to the measurements.
	 *
	 * This variant "measures" the scattering (variance) in layer 4 by adding two local
	 * parameters to describe the multiple scattering angle (without Chi2 bias).
	 */

//MP	MilleBinary mille; // for producing MillePede-II binary file
	unsigned int nTry = 1000; //: number of tries
	unsigned int nLayer = 10; //: number of detector layers
	std::cout << " Gbltst-eigen $Rev: 127 $ " << nTry << ", " << nLayer
			<< std::endl;

	srand(4711);

	clock_t startTime = clock();
// track direction
	double sinLambda = 0.3;
	double cosLambda = sqrt(1.0 - sinLambda * sinLambda);
	double sinPhi = 0.;
	double cosPhi = sqrt(1.0 - sinPhi * sinPhi);
// tDir = (cosLambda * cosPhi, cosLambda * sinPhi, sinLambda)
// U = Z x T / |Z x T|, V = T x U
	Matrix<double, 2, 3> uvDir;
	uvDir(0, 0) = -sinPhi;
	uvDir(0, 1) = cosPhi;
	uvDir(0, 2) = 0.;
	uvDir(1, 0) = -sinLambda * cosPhi;
	uvDir(1, 1) = -sinLambda * sinPhi;
	uvDir(1, 2) = cosLambda;
// measurement resolution
	Vector2d measErr;
	measErr << 0.001, 0.001;
	Vector2d measPrec; // (independent) precisions
	measPrec << 1.0 / (measErr(0) * measErr(0)), 1.0 / (measErr(1) * measErr(1));
	Matrix2d measInvCov; // inverse covariance matrix
	measInvCov.setZero();
	measInvCov(0, 0) = measPrec[0];
	measInvCov(1, 1) = measPrec[1];
// scattering error
	Vector2d scatErr;
	scatErr << 0.003, 0.003;
	Vector2d scatPrec;
	scatPrec << 1.0 / (scatErr(0) * scatErr(0)), 1.0 / (scatErr(1) * scatErr(1));
// (RMS of) CurviLinear track parameters (Q/P, slopes, offsets)
	Vector5d clPar;
	Vector5d clErr;
	clErr << 0.001, -0.1, 0.2, -0.15, 0.25;
	Matrix5d clCov, clSeed;
	unsigned int seedLabel = 99999;
// additional parameters
	Vector2d scatVar;
	scatVar << 0., 0.; // measured scattering variance (in layer 4)

	double bfac = 0.2998; // Bz*c for Bz=1
	double step = 1.5 / cosLambda; // constant steps in RPhi

	double Chi2Sum = 0.;
	int NdfSum = 0;
	double LostSum = 0.;
	int numFit = 0;

	for (unsigned int iTry = 1; iTry <= nTry; ++iTry) {
		// curvilinear track parameters
		for (unsigned int i = 0; i < 5; ++i) {
			clPar[i] = clErr[i] * unrm3();
		}
		clCov.setZero();
		for (unsigned int i = 0; i < 5; ++i) {
			clCov(i, i) = 1.0 * (clErr[i] * clErr[i]);
		}
//		std::cout << " Try " << iTry << ":" << clPar << std::endl;
// arclength
		double s = 0.;
		double sScat = 0.;
		Matrix5d jacPointToPoint;
		jacPointToPoint.setIdentity();
// create list of points
		std::vector<GblPoint> listOfPoints;
		listOfPoints.reserve(2 * nLayer);

		for (unsigned int iLayer = 0; iLayer < nLayer; ++iLayer) {
//			std::cout << " Layer " << iLayer << ", " << s << std::endl;
//     measurement directions
			double sinStereo = (iLayer % 2 == 0) ? 0. : 0.1;
			double cosStereo = sqrt(1.0 - sinStereo * sinStereo);
			Matrix<double, 3, 2> mDirT;
			mDirT.setZero();
			mDirT(1, 0) = cosStereo;
			mDirT(2, 0) = sinStereo;
			mDirT(1, 1) = -sinStereo;
			mDirT(2, 1) = cosStereo;
// projection measurement to local (curvilinear uv) directions (duv/dm)
			Matrix2d proM2l = uvDir * mDirT;
// projection local (uv) to measurement directions (dm/duv)
			Matrix2d proL2m = proM2l.inverse();
			// point with (independent) measurements (in measurement system)
			GblPoint pointMeas(jacPointToPoint);
			// measurement - prediction in measurement system with error
			Vector2d meas = proL2m * clPar.tail(2);
			for (unsigned int i = 0; i < 2; ++i) {
				meas[i] += measErr[i] * unrm3();
			}
			pointMeas.addMeasurement(proL2m, meas, measPrec);
			/* point with (correlated) measurements (in local system)
			 GblPoint point(jacPointToPoint);
			 // measurement - prediction in local system with error
			 Vector2d meas;
			 for (unsigned int i = 0; i < 2; ++i) {
			 meas[i] = measErr[i] * unrm3();
			 }
			 meas = proM2l * meas + clPar.tail<2>();
			 Matrix2d localInvCov = proL2m.adjoint() * measInvCov * proL2m;
			 point.addMeasurement(meas, localInvCov); */

			// additional local parameters?
			if (iLayer > 4) {
				//std::cout << " scat " << sScat << " " << s << std::endl;
				Matrix2d addDer = proL2m * (s - sScat);
				pointMeas.addLocals(addDer);
			}

// add point to trajectory
			listOfPoints.push_back(pointMeas);
			unsigned int iLabel = listOfPoints.size();
			if (iLabel == seedLabel) {
				clSeed = clCov.inverse();
			}
// propagate to scatterer
			jacPointToPoint = gblSimpleJacobian3(step, cosLambda, bfac);
			//jac2 = gblSimpleJacobian2(step, cosLambda, bfac);
			clPar = jacPointToPoint * clPar;
			clCov = jacPointToPoint * clCov * jacPointToPoint.adjoint();
			s += step;
			if (iLayer < nLayer - 1) {
				Vector2d scat(0., 0.);
				// point with scatterer
				GblPoint pointScat(jacPointToPoint);
				if (iLayer != 4) {
					pointScat.addScatterer(scat, scatPrec);
				} else {
					// measure scattering (with 2 local parameters, no scatterer)
					sScat = s;
				}
				listOfPoints.push_back(pointScat);
				iLabel = listOfPoints.size();
				if (iLabel == seedLabel) {
					clSeed = clCov.inverse();
				}
				// scatter a little
				for (unsigned int i = 0; i < 2; ++i) {
					clPar[i + 1] += scatErr[i] * unrm3();
					clCov(i + 1, i + 1) += scatErr[i] * scatErr[i];
				}
				// propagate to next measurement layer
				clPar = jacPointToPoint * clPar;
				clCov = jacPointToPoint * clCov * jacPointToPoint.adjoint();
				s += step;
			}
		}
//
		// create trajectory
		GblTrajectory traj(listOfPoints);
		//GblTrajectory traj(listOfPoints, seedLabel, clSeed); // with external seed
		//traj.printPoints();

		if (not traj.isValid()) {
			std::cout << " Invalid GblTrajectory -> skip" << std::endl;
			continue;
		}
// fit trajectory
		double Chi2;
		int Ndf;
		double lostWeight;
		traj.fit(Chi2, Ndf, lostWeight);
//		std::cout << " Fit: " << Chi2 << ", " << Ndf << ", " << lostWeight << std::endl;
		VectorXd aCorrection(7);
		MatrixXd aCovariance(7, 7);
		traj.getResults(10, aCorrection, aCovariance);
		// measured scattering parameters
		scatVar(0) += aCorrection(5) * aCorrection(5);
		scatVar(1) += aCorrection(6) * aCorrection(6);
		// contribution from fit
		scatVar(0) -= aCovariance(5, 5);
		scatVar(1) -= aCovariance(6, 6);
		/* look at residuals
		 for (unsigned int label = 1; label <= listOfPoints.size(); ++label) {
		 unsigned int numData = 0;
		 std::cout << " measResults, label " << label << std::endl;
		 VectorXd residuals(2), measErr(2), resErr(2), downWeights(2);
		 traj.getMeasResults(label, numData, residuals, measErr, resErr,
		 downWeights);
		 std::cout << " measResults, numData " << numData << std::endl;
		 for (unsigned int i = 0; i < numData; ++i) {
		 std::cout << " measResults " << label << " " << i << " "
		 << residuals[i] << " " << measErr[i] << " " << resErr[i]
		 << std::endl;
		 }
		 } */
// debug printout
		//traj.printTrajectory(1);
		//traj.printPoints(1);
		//traj.printData();
		Chi2Sum += Chi2;
		NdfSum += Ndf;
		LostSum += lostWeight;
		numFit++;
	}

	clock_t endTime = clock();
	double diff = endTime - startTime;
	double cps = CLOCKS_PER_SEC;
	std::cout << " Time elapsed " << diff / cps << " s" << std::endl;
	std::cout << " Chi2/Ndf = " << Chi2Sum / NdfSum << std::endl;
	std::cout << " Tracks fitted " << numFit << std::endl;
	std::cout << " ScatErr4 " << sqrt(scatVar[0] / numFit) << " "
			<< sqrt(scatVar[1] / numFit) << std::endl;
}

