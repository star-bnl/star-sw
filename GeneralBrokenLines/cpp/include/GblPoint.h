/*
 * GblPoint.h
 *
 *  Created on: Aug 18, 2011
 *      Author: kleinwrt
 */

/** \file
 *  GblPoint definition.
 *
 *  \author Claus Kleinwort, DESY, 2011 (Claus.Kleinwort@desy.de)
 *  \author Gregor Mittag, DESY, 2017 (templates and other optimizations)
 *
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

#ifndef GBLPOINT_H_
#define GBLPOINT_H_

#include<iostream>
#include<vector>
#include<math.h>
#include <stdexcept>
#ifdef GBL_EIGEN_SUPPORT_ROOT
#include "TVectorD.h"
#include "TMatrixD.h"
#include "TMatrixDSym.h"
#include "TMatrixDSymEigen.h"
#endif

#include "Eigen/Dense"

namespace gbl {

typedef Eigen::Matrix<double, 5, 1> Vector5d;
typedef Eigen::Matrix<double, 2, 3> Matrix23d;
typedef Eigen::Matrix<double, 2, 5> Matrix25d;
typedef Eigen::Matrix<double, 2, 7> Matrix27d;
typedef Eigen::Matrix<double, 3, 2> Matrix32d;
typedef Eigen::Matrix<double, 5, 5> Matrix5d;

/// Point on trajectory
/**
 * User supplied point on (initial) trajectory.
 *
 * Must have jacobian for propagation from previous point. May have:
 *
 *   -# Measurement (1D - 5D)
 *   -# Scatterer (thin, 2D kinks)
 *   -# Additional local parameters (with derivatives). Fitted together with track parameters.
 *   -# Additional global parameters (with labels and derivatives). Not fitted, only passed
 *      on to (binary) file for fitting with Millepede-II.
 */
class GblPoint {
public:
	GblPoint(const Matrix5d &aJacobian);
	GblPoint(const GblPoint&) = default;
	GblPoint& operator=(const GblPoint&) = default;
	GblPoint(GblPoint&&) = default;
	GblPoint& operator=(GblPoint&&) = default;
	virtual ~GblPoint();
#ifdef GBL_EIGEN_SUPPORT_ROOT
	// input via ROOT
	GblPoint(const TMatrixD &aJacobian);
	void addMeasurement(const TMatrixD &aProjection, const TVectorD &aResiduals,
			const TVectorD &aPrecision, double minPrecision = 0.);
	void addMeasurement(const TMatrixD &aProjection, const TVectorD &aResiduals,
			const TMatrixDSym &aPrecision, double minPrecision = 0.);
	void addMeasurement(const TVectorD &aResiduals, const TVectorD &aPrecision,
			double minPrecision = 0.);
	void addMeasurement(const TVectorD &aResiduals,
			const TMatrixDSym &aPrecision, double minPrecision = 0.);
	void addScatterer(const TVectorD &aResiduals, const TVectorD &aPrecision);
	void addScatterer(const TVectorD &aResiduals,
			const TMatrixDSym &aPrecision);
	void addLocals(const TMatrixD &aDerivatives);
	void addGlobals(const std::vector<int> &aLabels,
			const TMatrixD &aDerivatives);
#endif
	// input via Eigen

	/// Add a measurement to a point.
	/**
	 * Add measurement (in measurement system) with arbitrary precision (inverse covariance) matrix.
	 * Will be diagonalized.
	 * ((up to) 2D: position, 4D: slope+position, 5D: curvature+slope+position)
	 * \tparam Projection  Projection matrix
	 * \tparam Residuals   Residuals vector
	 * \tparam Precision   Precision matrix or vector (with diagonal)
	 * \param [in] aProjection Projection from local to measurement system (derivative of measurement vs local parameters)
	 * \param [in] aResiduals Measurement residuals
	 * \param [in] aPrecision Measurement precision (matrix)
	 * \param [in] minPrecision Minimal precision to accept measurement
	 */
	template<typename Projection, typename Residuals, typename Precision,
			typename std::enable_if<(Precision::ColsAtCompileTime != 1)>::type* =
					nullptr>
	void addMeasurement(const Eigen::MatrixBase<Projection>& aProjection,
			const Eigen::MatrixBase<Residuals>& aResiduals,
			const Eigen::MatrixBase<Precision>& aPrecision,
			double minPrecision = 0.);

	/// Add a measurement to a point.
	/**
	 * Add measurement (in measurement system) with diagonal precision (inverse covariance) matrix.
	 * ((up to) 2D: position, 4D: slope+position, 5D: curvature+slope+position)
	 * \tparam Projection  Projection matrix
	 * \tparam Residuals   Residuals vector
	 * \tparam Precision   Precision matrix or vector (with diagonal)
	 * \param [in] aProjection Projection from local to measurement system (derivative of measurement vs local parameters)
	 * \param [in] aResiduals Measurement residuals
	 * \param [in] aPrecision Measurement precision (vector with diagonal)
	 * \param [in] minPrecision Minimal precision to accept measurement
	 */
	template<typename Projection, typename Residuals, typename Precision,
			typename std::enable_if<(Precision::ColsAtCompileTime == 1)>::type* =
					nullptr>
	void addMeasurement(const Eigen::MatrixBase<Projection>& aProjection,
			const Eigen::MatrixBase<Residuals>& aResiduals,
			const Eigen::MatrixBase<Precision>& aPrecision,
			double minPrecision = 0.);

	/// Add a measurement to a point.
	/**
	 * Add measurement in local system with arbitrary precision (inverse covariance) matrix.
	 * Will be diagonalized.
	 * ((up to) 2D: position, 4D: slope+position, 5D: curvature+slope+position)
	 * \tparam Residuals   Residuals vector
	 * \tparam Precision   Precision matrix or vector (with diagonal)
	 * \param [in] aResiduals Measurement residuals
	 * \param [in] aPrecision Measurement precision (matrix)
	 * \param [in] minPrecision Minimal precision to accept measurement
	 */
	template<typename Residuals, typename Precision, typename std::enable_if<
			(Precision::ColsAtCompileTime != 1)>::type* = nullptr>
	void addMeasurement(const Eigen::MatrixBase<Residuals>& aResiduals,
			const Eigen::MatrixBase<Precision>& aPrecision,
			double minPrecision = 0.);

	/// Add a measurement to a point.
	/**
	 * Add measurement in local system with diagonal precision (inverse covariance) matrix.
	 * ((up to) 2D: position, 4D: slope+position, 5D: curvature+slope+position)
	 * \tparam Residuals   Residuals vector
	 * \tparam Precision   Precision matrix or vector (with diagonal)
	 * \param [in] aResiduals Measurement residuals
	 * \param [in] aPrecision Measurement precision (vector with diagonal)
	 * \param [in] minPrecision Minimal precision to accept measurement
	 */
	template<typename Residuals, typename Precision, typename std::enable_if<
			(Precision::ColsAtCompileTime == 1)>::type* = nullptr>
	void addMeasurement(const Eigen::MatrixBase<Residuals>& aResiduals,
			const Eigen::MatrixBase<Precision>& aPrecision,
			double minPrecision = 0.);

	/// Add a (thin) scatterer to a point.
	/**
	 * Add scatterer with arbitrary precision (inverse covariance) matrix.
	 * Will be diagonalized. Changes local track direction.
	 *
	 * The precision matrix for the local slopes is defined by the
	 * angular scattering error theta_0 and the scalar products c_1, c_2 of the
	 * offset directions in the local frame with the track direction:
	 *
	 *            (1 - c_1*c_1 - c_2*c_2)   |  1 - c_1*c_1     - c_1*c_2  |
	 *       P =  ----------------------- * |                             |
	 *                theta_0*theta_0       |    - c_1*c_2   1 - c_2*c_2  |
	 *
	 * \tparam Precision   Precision matrix or vector (with diagonal)
	 * \param [in] aResiduals Scatterer residuals
	 * \param [in] aPrecision Scatterer precision (full matrix)
	 */
	template<typename Precision, typename std::enable_if<
			(Precision::ColsAtCompileTime == 2)>::type* = nullptr>
	void addScatterer(const Eigen::Vector2d &aResiduals,
			const Eigen::MatrixBase<Precision>& aPrecision);

	/// Add a (thin) scatterer to a point.
	/**
	 * Add scatterer with diagonal precision (inverse covariance) matrix.
	 * Changes local track direction.
	 *
	 * The precision matrix for the local slopes is defined by the
	 * angular scattering error theta_0 and the scalar products c_1, c_2 of the
	 * offset directions in the local frame with the track direction:
	 *
	 *            (1 - c_1*c_1 - c_2*c_2)   |  1 - c_1*c_1     - c_1*c_2  |
	 *       P =  ----------------------- * |                             |
	 *                theta_0*theta_0       |    - c_1*c_2   1 - c_2*c_2  |
	 *
	 * \tparam Precision   Precision matrix or vector (with diagonal)
	 * \param [in] aResiduals Scatterer residuals
	 * \param [in] aPrecision Scatterer precision (vector with diagonal)
	 */
	template<typename Precision, typename std::enable_if<
			(Precision::ColsAtCompileTime == 1)>::type* = nullptr>
	void addScatterer(const Eigen::Vector2d &aResiduals,
			const Eigen::MatrixBase<Precision>& aPrecision);

	/// Add local derivatives to a point.
	/**
	 * Point needs to have a measurement.
	 * \tparam Derivative  Derivatives matrix
	 * \param [in] aDerivatives Local derivatives (matrix)
	 */
	template<typename Derivative>
	void addLocals(const Eigen::MatrixBase<Derivative>& aDerivatives);
	template<typename Derivative>

	/// Add global derivatives to a point.
	/**
	 * Point needs to have a measurement.
	 * \tparam Derivative  Derivatives matrix
	 * \param [in] aLabels Global derivatives labels
	 * \param [in] aDerivatives Global derivatives (matrix)
	 */
	void addGlobals(const std::vector<int> &aLabels,
			const Eigen::MatrixBase<Derivative>& aDerivatives);
	//
	unsigned int hasMeasurement() const;
	double getMeasPrecMin() const;
	void getMeasurement(Matrix5d &aProjection, Vector5d &aResiduals,
			Vector5d &aPrecision) const;
	void getMeasTransformation(Eigen::MatrixXd &aTransformation) const;
	bool hasScatterer() const;
	void getScatterer(Eigen::Matrix2d &aTransformation,
			Eigen::Vector2d &aResiduals, Eigen::Vector2d &aPrecision) const;
	void getScatTransformation(Eigen::Matrix2d &aTransformation) const;
	unsigned int getNumLocals() const;
	const Eigen::MatrixXd& getLocalDerivatives() const;
	unsigned int getNumGlobals() const;
	void getGlobalLabels(std::vector<int> &aLabels) const;
	void getGlobalDerivatives(Eigen::MatrixXd &aDerivatives) const;
	void getGlobalLabelsAndDerivatives(unsigned int aRow,
			std::vector<int> &aLabels, std::vector<double> &aDerivatives) const;
	unsigned int getLabel() const;
	int getOffset() const;
	const Matrix5d& getP2pJacobian() const;
	void getDerivatives(int aDirection, Eigen::Matrix2d &matW,
			Eigen::Matrix2d &matWJ, Eigen::Vector2d &vecWd) const;
	void printPoint(unsigned int level = 0) const;

private:
	friend class GblTrajectory; // to have the following setters private
	void setLabel(unsigned int aLabel);
	void setOffset(int anOffset);
	void addPrevJacobian(const Matrix5d &aJac);
	void addNextJacobian(const Matrix5d &aJac);

	unsigned int theLabel; ///< Label identifying point
	int theOffset; ///< Offset number at point if not negative (else interpolation needed)
	Matrix5d p2pJacobian; ///< Point-to-point jacobian from previous point
	Matrix5d prevJacobian; ///< Jacobian to previous scatterer (or first measurement)
	Matrix5d nextJacobian; ///< Jacobian to next scatterer (or last measurement)
	unsigned int measDim; ///< Dimension of measurement (1-5), 0 indicates absence of measurement
	double measPrecMin; ///< Minimal measurement precision (for usage)
	Matrix5d measProjection; ///< Projection from measurement to local system

	Vector5d measResiduals; ///< Measurement residuals
	Vector5d measPrecision; ///< Measurement precision (diagonal of inverse covariance matrix)
	bool transFlag; ///< Transformation exists?
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic,
			Eigen::ColMajor /* default */, 5, 5> measTransformation; ///< Transformation of diagonalization (of meas. precision matrix)
	bool scatFlag; ///< Scatterer present?
	Eigen::Matrix2d scatTransformation; ///< Transformation of diagonalization (of scat. precision matrix)
	Eigen::Vector2d scatResiduals; ///< Scattering residuals (initial kinks if iterating)
	Eigen::Vector2d scatPrecision; ///< Scattering precision (diagonal of inverse covariance matrix)
	Eigen::MatrixXd localDerivatives; ///< Derivatives of measurement vs additional local (fit) parameters
	std::vector<int> globalLabels; ///< Labels of global (MP-II) derivatives
	Eigen::MatrixXd globalDerivatives; ///< Derivatives of measurement vs additional global (MP-II) parameters
};

template<typename Projection, typename Residuals, typename Precision,
		typename std::enable_if<(Precision::ColsAtCompileTime != 1)>::type*>
void GblPoint::addMeasurement(const Eigen::MatrixBase<Projection>& aProjection,
		const Eigen::MatrixBase<Residuals>& aResiduals,
		const Eigen::MatrixBase<Precision>& aPrecision, double minPrecision) {
	static_assert(static_cast<int>(Residuals::ColsAtCompileTime) == 1, "addMeasurement: cols(Residuals) must be 1 (vector)");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) <= 5 or static_cast<int>(Residuals::RowsAtCompileTime) == Eigen::Dynamic, "addMeasurement: rows(Residuals) must be 1-5 or dynamic");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) == static_cast<int>(Precision::RowsAtCompileTime), "addMeasurement: rows(Residuals) and rows(Precision) must be equal");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) == static_cast<int>(Projection::RowsAtCompileTime), "addMeasurement: rows(Residuals) and rows(Projection) must be equal");
	static_assert(static_cast<int>(Precision::RowsAtCompileTime) == static_cast<int>(Precision::ColsAtCompileTime), "addMeasurement: rows(Precision) and cols(Precision) must be equal");
	static_assert(static_cast<int>(Projection::RowsAtCompileTime) == static_cast<int>(Projection::ColsAtCompileTime), "addMeasurement: rows(Projection) and cols(Projection) must be equal");
	measDim = aResiduals.rows();
	measPrecMin = minPrecision;
	// arbitrary precision matrix
	Eigen::SelfAdjointEigenSolver<typename Precision::PlainObject> measEigen {
			aPrecision };
	measTransformation = measEigen.eigenvectors().transpose();
	transFlag = true;
	measResiduals.tail(measDim) = measTransformation * aResiduals;
	measPrecision.tail(measDim) = measEigen.eigenvalues();
	measProjection.bottomRightCorner(measDim, measDim) = measTransformation
			* aProjection;
}

template<typename Projection, typename Residuals, typename Precision,
		typename std::enable_if<(Precision::ColsAtCompileTime == 1)>::type*>
void GblPoint::addMeasurement(const Eigen::MatrixBase<Projection>& aProjection,
		const Eigen::MatrixBase<Residuals>& aResiduals,
		const Eigen::MatrixBase<Precision>& aPrecision, double minPrecision) {
	static_assert(static_cast<int>(Residuals::ColsAtCompileTime) == 1, "addMeasurement: cols(Residuals) must be 1 (vector)");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) <= 5 or static_cast<int>(Residuals::RowsAtCompileTime) == Eigen::Dynamic, "addMeasurement: rows(Residuals) must be 1-5 or dynamic");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) == static_cast<int>(Precision::RowsAtCompileTime), "addMeasurement: rows(Residuals) and rows(Precision) must be equal");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) == static_cast<int>(Projection::RowsAtCompileTime), "addMeasurement: rows(Residuals) and rows(Projection) must be equal");
	static_assert(static_cast<int>(Projection::RowsAtCompileTime) == static_cast<int>(Projection::ColsAtCompileTime), "addMeasurement: rows(Projection) and cols(Projection) must be equal");
	measDim = aResiduals.rows();
	measPrecMin = minPrecision;
	// diagonal precision matrix
	measResiduals.tail(measDim) = aResiduals;
	measPrecision.tail(measDim) = aPrecision;
	measProjection.bottomRightCorner(measDim, measDim) = aProjection;
}

template<typename Residuals, typename Precision, typename std::enable_if<
		(Precision::ColsAtCompileTime != 1)>::type*>
void GblPoint::addMeasurement(const Eigen::MatrixBase<Residuals>& aResiduals,
		const Eigen::MatrixBase<Precision>& aPrecision, double minPrecision) {
	static_assert(static_cast<int>(Residuals::ColsAtCompileTime) == 1, "addMeasurement: cols(Residuals) must be 1 (vector)");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) <= 5 or static_cast<int>(Residuals::RowsAtCompileTime) == Eigen::Dynamic, "addMeasurement: rows(Residuals) must be 1-5 or dynamic");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) == static_cast<int>(Precision::RowsAtCompileTime), "addMeasurement: rows(Residuals) and rows(Precision) must be equal");
	measDim = aResiduals.rows();
	measPrecMin = minPrecision;
	// arbitrary precision matrix
	Eigen::SelfAdjointEigenSolver<typename Precision::PlainObject> measEigen {
			aPrecision };
	measTransformation = measEigen.eigenvectors().transpose();
	transFlag = true;
	measResiduals.tail(measDim) = measTransformation * aResiduals;
	measPrecision.tail(measDim) = measEigen.eigenvalues();
	measProjection.bottomRightCorner(measDim, measDim) = measTransformation;
}

template<typename Residuals, typename Precision, typename std::enable_if<
		(Precision::ColsAtCompileTime == 1)>::type*>
void GblPoint::addMeasurement(const Eigen::MatrixBase<Residuals>& aResiduals,
		const Eigen::MatrixBase<Precision>& aPrecision, double minPrecision) {
	static_assert(static_cast<int>(Residuals::ColsAtCompileTime) == 1, "addMeasurement: cols(Residuals) must be 1 (vector)");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) <= 5 or static_cast<int>(Residuals::RowsAtCompileTime) == Eigen::Dynamic, "addMeasurement: rows(Residuals) must be 1-5 or dynamic");
	static_assert(static_cast<int>(Residuals::RowsAtCompileTime) == static_cast<int>(Precision::RowsAtCompileTime), "addMeasurement: rows(Residuals) and rows(Precision) must be equal");
	measDim = aResiduals.rows();
	measPrecMin = minPrecision;
	// diagonal precision matrix
	measResiduals.tail(measDim) = aResiduals;
	measPrecision.tail(measDim) = aPrecision;
	measProjection.setIdentity();
}

template<typename Precision, typename std::enable_if<
		(Precision::ColsAtCompileTime == 2)>::type*>
void GblPoint::addScatterer(const Eigen::Vector2d &aResiduals,
		const Eigen::MatrixBase<Precision>& aPrecision) {
	static_assert(static_cast<int>(Precision::RowsAtCompileTime) == 2 or static_cast<int>(Precision::RowsAtCompileTime) == Eigen::Dynamic, "addScatterer: rows(Precision) must be 2 or dynamic");
	scatFlag = true;
	// arbitrary precision matrix
	Eigen::SelfAdjointEigenSolver<typename Precision::PlainObject> scatEigen {
			aPrecision };
	scatTransformation = scatEigen.eigenvectors();
	scatTransformation.transposeInPlace();
	scatResiduals = scatTransformation * aResiduals;
	scatPrecision = scatEigen.eigenvalues();
}

template<typename Precision, typename std::enable_if<
		(Precision::ColsAtCompileTime == 1)>::type*>
void GblPoint::addScatterer(const Eigen::Vector2d &aResiduals,
		const Eigen::MatrixBase<Precision>& aPrecision) {
	static_assert(static_cast<int>(Precision::RowsAtCompileTime) == 2 or static_cast<int>(Precision::RowsAtCompileTime) == Eigen::Dynamic, "addScatterer: rows(Precision) must be 2 or dynamic");
	scatFlag = true;
	scatResiduals = aResiduals;
	scatPrecision = aPrecision;
	scatTransformation.setIdentity();
}

template<typename Derivative>
void GblPoint::addLocals(const Eigen::MatrixBase<Derivative>& aDerivatives) {
	if (measDim) {
		localDerivatives.resize(aDerivatives.rows(), aDerivatives.cols());
		if (transFlag) {
			localDerivatives = measTransformation * aDerivatives;
		} else {
			localDerivatives = aDerivatives;
		}
	}
}

template<typename Derivative>
void GblPoint::addGlobals(const std::vector<int> &aLabels,
		const Eigen::MatrixBase<Derivative>& aDerivatives) {
	if (measDim) {
		globalLabels = aLabels;
		globalDerivatives.resize(aDerivatives.rows(), aDerivatives.cols());
		if (transFlag) {
			globalDerivatives = measTransformation * aDerivatives;
		} else {
			globalDerivatives = aDerivatives;
		}

	}
}

}
#endif /* GBLPOINT_H_ */
