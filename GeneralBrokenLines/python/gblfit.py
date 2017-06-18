'''
Track fit with general broken lines.

Created on Jul 27, 2011

@author: kleinwrt
'''

## \file
# GBL objects
#
# \author Claus Kleinwort, DESY, 2011 (Claus.Kleinwort@desy.de)
#
#  \copyright
#  Copyright (c) 2011 - 2016 Deutsches Elektronen-Synchroton,
#  Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
#  This library is free software; you can redistribute it and/or modify
#  it under the terms of the GNU Library General Public License as
#  published by the Free Software Foundation; either version 2 of the
#  License, or (at your option) any later version. \n\n
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Library General Public License for more details. \n\n
#  You should have received a copy of the GNU Library General Public
#  License along with this program (see the file COPYING.LIB for more
#  details); if not, write to the Free Software Foundation, Inc.,
#  675 Mass Ave, Cambridge, MA 02139, USA.

import numpy as np
import math
from gblnum import BorderedBandMatrix
from mille import MilleRecord

## User supplied point on (initial) trajectory.
#
#  Must have jacobians for propagation to previous or next point with offsets (first, last
#  point, points with scatterer). May have:
#  
#    1. Measurement (1D or 2D)
#    2. Scatterer (thin, 2D kinks)
#    3. Additional local parameters (with derivatives). Fitted together with track parameters.
#    4. Additional global parameters (with labels and derivatives). Not fitted, only passed
#       on to (binary) file for fitting with Millepede-II. 
#       
class GblPoint(object):

  ## Create new point.
  #
  #  @param aJacobian jacobian from previous point; matrix(float)
  #
  def __init__(self, aJacobian):
    ## label for referencing point (0,1,..,number(points)-1); int
    self.__label = 0
    ##  >=0: offset number at point, <0: offset number at next point with offset; int
    self.__offset = 0
    ## Point-to-point jacobian from previous point; matrix(float)
    self.__p2pJacobian = aJacobian
    ## jacobians for propagation to previous or next point with offsets; pair(matrix(float))
    self.__jacobians = [ [], [] ]
    ## measurement at point: projection (dm/du), residuals (to initial trajectory), precision; list(matrix(float))
    self.__measurement = None
    ## dimension of measurement (2D, 4D or 5D); int
    self.__measDim = 0
    ## transformation (to eigen-vectors of precision matrix); matrix(float)
    self.__measTransformation = None    
    ## scatterer at point: (transformation or None,) initial kinks, precision (inverse covariance matrix); list(matrix(float))
    self.__scatterer = None
    ## local derivatives; matrix(float)
    self.__localDerivatives = None
    ## global labels; matrix(int)
    self.__globalLabels = None
    ## global derivatives; matrix(float)
    self.__globalDerivatives = None
      
# for extension to retrieval of residuals, pulls    
#    self.__dataMeas = [0, 0]
# for extension to retrieval of residuals, pulls    
#    self.__dataScat = [0, 0]

  ## Add a mesurement to a point.
  # 
  #  Add measurement with arbitrary precision (inverse covariance) matrix.
  #  Will be diagonalized.
  # 
  #  @param aMeasurement measurement (projection (or None), residuals, precision
  #                       (diagonal of or full matrix)); list(matrix(float))
  #
  def addMeasurement(self, aMeasurement):
    self.__measurement = aMeasurement
    self.__measDim = aMeasurement[1].shape[0]
    if (aMeasurement[2].ndim == 2):  # full precision matrix, need to diagonalize
      eigenVal, eigenVec = np.linalg.eigh(aMeasurement[2])
      self.__measTransformation = eigenVec.T
#     transform measurement
      if (aMeasurement[0] is None):
        self.__measurement[0] = self.__measTransformation
      else:
        self.__measurement[0] = np.dot(self.__measTransformation, aMeasurement[0])
      self.__measurement[1] = np.dot(self.__measTransformation, aMeasurement[1])
      self.__measurement[2] = eigenVal

  ## Check point for a measurement.
  #
  # @return flag; bool
  #
  def hasMeasurement(self):

    return (self.__measurement is not None)

  ## Retrieve measurement of a point.
  #  
  #  @return measurement (projection residuals, precision); list(matrix(float))
  #
  def getMeasurement(self):
    return self.__measurement
 
  ## Retrieve dimension of measurement of a point.
  #  
  #  @return measurement dimension (2, 4 or 5); int
  #
  def getMeasDim(self):
    return self.__measDim

  ## Add a (thin) scatterer to a point.
  #  
  #  Add scatterer with arbitrary precision (inverse covariance) matrix.
  #  Will be diagonalized. Changes local track direction.
  #
  #  The precision matrix for the local slopes is defined by the
  #  angular scattering error theta_0 and the scalar products c_1, c_2 of the
  #  offset directions in the local frame with the track direction:
  #
  #             (1 - c_1*c_1 - c_2*c_2)   |  1 - c_1*c_1     - c_1*c_2  |
  #        P =  ~~~~~~~~~~~~~~~~~~~~~~~ * |                             |
  #                 theta_0*theta_0       |    - c_1*c_2   1 - c_2*c_2  |
  #      
  #  @param aScatterer scatterer (kinks, precision (diagonal of or full matrix)); list(matrix(float))     
  # 
  def addScatterer(self, aScatterer):
    self.__scatterer = [ None ] + aScatterer
    if (aScatterer[1].ndim == 2):  # full precision matrix, need to diagonalize
      eigenVal, eigenVec = np.linalg.eigh(aScatterer[1])
      scatTransformation = eigenVec.T
#     transform measurement
      self.__scatterer[0] = scatTransformation
      self.__scatterer[1] = np.dot(scatTransformation, aScatterer[0])
      self.__scatterer[2] = eigenVal
 
  ## Check point for a scatterer.
  #
  # @return flag; bool
  #  
  def hasScatterer(self):
    return (self.__scatterer is not None)

  ## Retrieve scatterer of a point.
  #  
  #  @return scatterer (kinks, precision); list(matrix(float))   
  # 
  def getScatterer(self):
    return self.__scatterer

  ## Add local derivatives.
  #  
  #  @param derivatives local derivatives; matrix(float)   
  #   
  def addLocals(self, derivatives):
    if (self.__measDim > 0):
      if (self.__measTransformation is None):
        self.__localDerivatives = derivatives
      else:
        self.__localDerivatives = np.dot(self.__measTransformation, derivatives)        

  ## Add global derivatives.
  #  
  #  @param labels global labels; matrix(int)
  #  @param derivatives global derivatives; matrix(float)
  #
  def addGlobals(self, labels, derivatives):
    if (self.__measDim > 0):
      self.__globalLabels = labels
      if (self.__measTransformation is None):
        self.__globalDerivatives = derivatives
      else:
        self.__globalDerivatives = np.dot(self.__measTransformation, derivatives) 

  ## Get number of local derivatives.
  #  
  #  @return Number of local derivatives at point; int
  #
  def getNumLocals(self):
    if (self.__localDerivatives is not None):
      return self.__localDerivatives.shape[1]
    else:
      return 0

  ## Get local derivatives.
  #  
  #  @return local derivatives; matrix(float)  
  #
  def getLocalDerivatives(self):
    return self.__localDerivatives

  ## Get global labels.
  #  
  #  @return global labels; matrix(int)   
  def getGlobalLabels(self):
    return self.__globalLabels
 
  ## Get global derivatives.
  #  
  #  @return global derivatives; matrix(float)     
  # 
  def getGlobalDerivatives(self):
    return self.__globalDerivatives
 
  ## Define label of a point.
  #  
  #  @param aLabel label; int   
  #
  def setLabel(self, aLabel):
    self.__label = aLabel

  ## Retrieve label of a point.
  #  
  #  @return label; int
  #
  def getLabel(self):
    return self.__label

  ## Define offset of a point and references to previous and next point with offset.
  #  
  #  @param anOffset offset number (at point (>=0) or at next point with offset (<0)); int
  # 
  def setOffset(self, anOffset):
    self.__offset = anOffset

  ## Get offset of a point.
  #  
  #  @return offset number (at point (>=0) or at next point with offset (<0)); int
  #
  def getOffset(self):
    return self.__offset
  
#  def setDataMeas(self, aIndex, aData):
# for extension to retrieval of residuals, pulls    
#    self.__dataMeas[aIndex] = aData

#  def setDataScat(self, aIndex, aData):
# for extension to retrieval of residuals, pulls    
#    self.__dataScat[aIndex] = aData
 
  ## Retrieve jacobian of a point.
  #  
  #  @return point-to-point jacobian (from previous point); matrix(float) 
  #
  def getP2pJacobian(self):
    return self.__p2pJacobian         

  ## Add jacobian to previous offset.
  #  
  #  @param aJacobian jacobian for propagation to previous point with offsets; matrix(float) 
  #
  def addPrevJacobian(self, aJacobian):
    self.__jacobians[0] = np.linalg.inv(aJacobian)

  ## Add jacobian to next offset.
  #  
  #  @param aJacobian jacobian for propagation to next point with offsets; matrix(float)
  #
  def addNextJacobian(self, aJacobian):
    self.__jacobians[1] = aJacobian
 
  ## Get derivatives for locally linearized track model (backward or forward propagation).
  #  
  #  @param index 0 (previous) or 1 (next point with offsets); int
  #  @return derivatives; list(matrix(float))   
  # 
  def getDerivatives(self, index):
    aJacobian = self.__jacobians[index]
    matJ = aJacobian[3:5, 3:5]  # J
    matS = aJacobian[3:5, 1:3]  # S 
    vecD = aJacobian[3:5, 0:1]  # d
    if (index < 1):
      matS = -matS
    matW = np.linalg.inv(matS)  # W = +/- S^-1
    return matW, np.dot(matW, matJ), np.dot(matW, vecD)  # W, W*J, W*d

  ## Print point. 
  def printPoint(self):
    print " point ", self.__label, self.__offset 

#------------------------------------------------------------------------------ 

## Data (block) containing value, precision and derivatives for measurements, kinks and external seed.
#  
#  Created from attributes of GblPoints, used to construct linear equation system for track fit.
#
class GblData(object):

  ## Create new data.
  #  
  #  @param aLabel label of corresponding point; int
  #  @param aType  type of data; int
  #  @param aValue value; float
  #  @param aPrec precision; float
  #
  def __init__(self, aLabel=0, aType=0, aValue=0., aPrec=0.):
    ## label of corresponding point; int
    self.__label = aLabel
    ## type of data (0: none, 1: internal measurement, 2: internal kink, 3: external seed, 4: external measurement); int
    self.__type = aType
    ## value (residual or kink); float
    self.__value = aValue
    ## precision (diagonal element of inverse covariance matrix); float
    self.__precision = aPrec
    ## down weighting method; int
    self.__dwMethod = 0
    ## down weighting factor (M-estimators); float
    self.__downWeight = 1.
    ## prediction (for value from fit); float
    self.__prediction = 0.
    ## labels of fit parameters (with non zero derivative); list(int)
    self.__parameters = []
    ## derivatives (prediction vs fit parameters); list(float)
    self.__derivatives = []
    ## labels of global parameters; list(int)
    self.__globalLabels = []
    ## derivatives (prediction vs global parameters); list(float)
    self.__globalDerivatives = []

#    self.__predictionVar = 0.
  
  ##Add derivatives to data (block) from measurements and kinks. Generate lists of labels.
  #  
  #  @param iRow row of measurement (vector); int
  #  @param labDer labels of derivatives vs curvature and band parameters (offsets); vector(int)
  #  @param matDer derivatives vs curvature and band parameters (offsets); matrix(float)
  #  @param derLocal derivatives vs local parameters; list(float)
  #  @param labGlobal labels of global parameters; list(int)
  #  @param derGlobal derivatives vs global parameters; list(float)  
  #            
  def addDerivatives(self, iRow, labDer, matDer, \
                     derLocal=None, labGlobal=None, derGlobal=None):
    if (derLocal is not None):
      for i in range(derLocal.shape[1]):  # local derivatives
        if (derLocal[iRow, i] != 0.):
          self.__derivatives.append(derLocal[iRow, i])
          self.__parameters.append(i + 1)
        
    for i in range(len(labDer)):  # curvature, offset derivatives
      if (labDer[i] != 0 and matDer[iRow, i] != 0.):
        self.__derivatives.append(matDer[iRow , i])
        self.__parameters.append(labDer[i])

    if (derGlobal is not None):  
      for i in range(derGlobal.shape[1]):  # global derivatives
        if (derGlobal[iRow, i] != 0.):
          self.__globalLabels.append(labGlobal[iRow, i])
          self.__globalDerivatives.append(derGlobal[iRow, i])

  ## Add derivatives to data (block) from external seed. Generate lists of labels.
  #  
  #  @param indexExt labels from exteranl seed; list(int)
  #  @param derExt derivatives from exteranl seed; list(float)
  #
  def addExtDerivatives(self, indexExt, derExt):
    for i in range(len(derExt)):  # external derivatives
      if (derExt[i] != 0.):
        self.__derivatives.append(derExt[i])
        self.__parameters.append(indexExt[i])  

  ## Calculate compressed matrix and right hand side from data.
  #  
  #  @return indices, compressed right hand side and matrix; list
  #
  def getMatrices(self):  
    aVector = np.array([ self.__derivatives ])
    aMatrix = np.dot(aVector.T, aVector)
    aValue = self.__value
    aWeight = self.__precision * self.__downWeight
    return self.__parameters, aValue * aVector * aWeight, aMatrix * aWeight

  ## Calculate prediction for data from fit.
  # 
  #  @param aVector values of fit parameters; vector(float)    
  #
  def setPrediction(self, aVector):
    self.__prediction = 0.
    for i in range(len(self.__parameters)):
      self.__prediction += self.__derivatives[i] * aVector[ self.__parameters[i] - 1 ]

#  def setPredictionVariance(self, aMatrix):
#    '''Calculate variance of prediction for data from fit.'''
# var(residual) = 1./precision**2 - var(prediction)
#    aBlockMatrix = aMatrix.getBlockMatrix(self.__parameters)
#    self.__predictionVar = np.dot(self.__derivatives.T, \
#                             np.dot(aBlockMatrix, self.__derivatives))

  ## Outlier down weighting with M-estimators.
  #  
  #  @param aMethod method (1=Tukey, 2=Huber, 3=Cauchy); int
  #  @return weight (0..1); float  
  #          
  def setDownWeighting(self, aMethod): 
    self.__dwMethod = aMethod
    scaledResidual = abs(self.__value - self.__prediction) * math.sqrt(self.__precision)   
    if (aMethod == 1):  # Tukey
      if (scaledResidual < 4.6851):
        aWeight = (1.0 - (scaledResidual / 4.6851) ** 2) ** 2
      else:
        aWeight = 0.
    elif (aMethod == 2):  # Huber
      if (scaledResidual < 1.345):
        aWeight = 1.
      else:
        aWeight = 1.345 / scaledResidual
    elif (aMethod == 3):  # Cauchy
      aWeight = 1.0 / (1.0 + (scaledResidual / 2.3849) ** 2)      
    self.__downWeight = aWeight
    return aWeight

  ## Calculate Chi2 (contribution) from data.
  # 
  # For down-weighting with M-estimators the corresponding objective function is used.
  # 
  #  @return Chi2; float   
  #     
  def getChi2(self):
    scaledResidual = abs(self.__value - self.__prediction) * math.sqrt(self.__precision)
    Chi2 = scaledResidual ** 2
    if (self.__dwMethod == 1):  # Tukey
      if (scaledResidual < 4.6851):
        Chi2 = 4.6851 ** 2 / 3. * (1. - (1. - (scaledResidual / 4.6851) ** 2) ** 3)
      else:  
        Chi2 = 4.6851 ** 2 / 3.
    elif (self.__dwMethod == 2):  # Huber
      if (scaledResidual > 1.345):
        Chi2 = 1.345 * (2.*scaledResidual - 1.345) 
    elif (self.__dwMethod == 3):  # Cauchy
      Chi2 = math.log(1. + (scaledResidual / 2.3849) ** 2) * 2.3849 ** 2     
    return Chi2
  
  ## Get Label.
  #
  #  @return label; int   
  #
  def getLabel(self):
    return self.__label

  ## Get type.
  #
  #  @return type; int   
  #
  def getType(self):
    return self.__type
      
  ## Get data for residual (and errors).
  #  
  #  @return data components; list 
  #
  def getResidual(self):
    return self.__value - self.__prediction, 1.0 / self.__precision, self.__downWeight, self.__parameters, self.__derivatives

  ## Get data components (for copying to MP binaty record)
  #  
  #  @return data components; list 
  #
  def toRecord(self):
    return self.__value, self.__precision, self.__parameters, self.__derivatives, \
            self.__globalLabels, self.__globalDerivatives

  ## Set data components (from MP binaty record)
  # 
  #  @param dataList data components; list
  #
  def fromRecord(self, dataList):
    self.__value, self.__precision, self.__parameters, self.__derivatives, \
            self.__globalLabels, self.__globalDerivatives = dataList
 
  ## Analyze labels of fit parameters to determine number of parameters and
  #  border size with given maximal band width.
  #  
  #  @param maxBand maximal band width; int   
  #  @return number of parameters and border size (from this data); pair(int)  
  #           
  def analyzeData(self, maxBand):
    maxPar = self.__parameters[-1]
    maxBor = 0
    for i in self.__parameters:
      if (i < maxPar - maxBand):
        maxBor = i
    return maxPar, maxBor

  ## Print data.      
  def printData(self):
    print " measurement at label ", self.__label, " with type ", self.__type, " : ", self.__value, self.__precision
    print " param ", self.__parameters
    print " deriv ", self.__derivatives
    print " global labels ", self.__globalLabels
    print " global deriv  ", self.__globalDerivatives
 
#------------------------------------------------------------------------------ 

## \mainpage General information
#
#  \section intro_sec Introduction
#  
#  For a track with an initial trajectory from a prefit of the
#  (2D, 4D or 5D) measurements (internal seed) or an external 
#  prediction (external seed) the description of multiple scattering
#  is added by offsets in a local system. Along the initial
#  trajectory points are defined with can describe a measurement
#  or a (thin) scatterer or both. The refit provides corrections
#  to the local track parameters (in the local system) and the 
#  corresponding covariance matrix at any of those points.
#  Non-diagonal covariance matrices will be diagonalized internally.
#  Outliers can be down-weighted by use of M-estimators.
#  At one point the measurements can be omitted from the refit
#  to calculate unbiased residuals.
#
#  A position measurement is in a plane defined by two directions.
#  Along one direction the measurement precision may be zero,
#  defining a 1D measurement in the other direction.
#
#  The broken lines trajectory is defined by (2D) offsets at the 
#  first and last point and all points with a scatterer. The
#  prediction for a measurement is obtained by interpolation of
#  the enclosing offsets and for triplets of adjacent offsets
#  kink angles are determined. This requires for all points the
#  jacobians for propagation to the previous and next offset.
#
#  Additional local or global parameters can be added and the
#  trajectories can be written to special binary files for
#  calibration and alignment with Millepede-II.
#  (V. Blobel, NIM A, 566 (2006), pp. 5-13).
#
#  The conventions for the coordinate systems follow:
#  Derivation of Jacobians for the propagation of covariance
#  matrices of track parameters in homogeneous magnetic fields
#  A. Strandlie, W. Wittek, NIM A, 566 (2006) 687-698.
#  
#  The source code is available at the DESY SVN server, see:
#  https://www.wiki.terascale.de/index.php/GeneralBrokenLines
#
#  \section seq_sec Calling sequence:
#    -# Create trajectory:\n
#            <tt>traj = \ref gblfit.GblTrajectory "GblTrajectory()" </tt>
#    -# For all points on initial trajectory 
#        - Create point (supply jacobian from previous point):\n
#            <tt>point = \ref gblfit.GblPoint "GblPoint(jacobian)"</tt>
#        - Optionally add measurement to point:\n    
#            <tt>point.addMeasurement(..)</tt>
#        - Optionally additional local or global parameters for measurement:\n 
#            <tt>point.addLocals(..)</tt> \n
#            <tt>point.addGlobals(..)</tt>
#        - Optionally add scatterer to point:\n    
#            <tt>point.addScatterer(..)</tt>
#        - Add point (ordered by arc length) to trajectory, get label of point:\n 
#            <tt>label = traj.addPoint(point)</tt>
#    -# Optionally add external seed:\n
#            <tt>traj.addExternalSeed(..)</tt>
#    -# Fit trajectory (potentially several times with different options), get Chi2, Ndf (and weight lost by M-estimators):\n
#            <tt>[..] = traj.fit()</tt>
#    -# For any point on inital trajectory
#        - Get corrections and covariance matrix for track parameters:\n
#            <tt>[..] = traj.getResults(label) </tt>
#        - Optionally get residuals with errors for measurements:\n
#            <tt>[..] = traj.getMeasResults(label) </tt>
#        - Optionally get residuals with errors for scatterers:\n
#            <tt>[..] = traj.getScatResults(label) </tt>
#    -# Optionally write trajectory to MP binary file:\n
#            <tt>traj.milleOut(..)</tt>
#            
#  Alternatively trajectories can by read from MP binary files and fitted. 
#  As the points on the initial trajectory are not stored in this files results at
#  points (corrections, covariance matrix) are not available and omission of
#  measurements from a point is not possible.
#  
#  \section ref_sec References:  
#    - V. Blobel, C. Kleinwort, F. Meier,
#      Fast alignment of a complex tracking detector using advanced track models,
#      Computer Phys. Communications (2011), doi:10.1016/j.cpc.2011.03.017
#    - C. Kleinwort, General Broken Lines as advanced track fitting method,
#      NIM A, 673 (2012), 107-110, doi:10.1016/j.nima.2012.01.024
# 

## General Broken Lines Trajectory.   
#      
class GblTrajectory(object):
 
  ## Create new trajectory.
  #  
  #  @param hasCurv flag for curvature; bool
  #  @param aDim active offset components (0:u_1, 1:u_2); list(int) 
  #
  def __init__(self, hasCurv=True, aDim=[0, 1]):
    ## number of points on trajectory; int
    self.__numPoints = 0
    ## number of (points with) offsets on trajectory; int
    self.__numOffsets = 0
    ## 'curvature' is fit parameter (=1); int
    self.__numCurvature = (1 if hasCurv else 0)
    ## number fit parameters; int
    self.__numParameters = 0
    ## number of local parameters; int
    self.__numLocals = 0
    ## label of point with external seed; int 
    self.__externalPoint = 0
    ## active components of offsets (both ([0,1]) or single ([0] or [1]); list(int)
    self.__dimensions = aDim
    ## points on trajectory; list(GblPoint)
    self.__points = [] 
    ## data (blocks) of trajectory; list(GblData)
    self.__data = []
    ## external seed (for local, fit parameters); matrix(float)
    self.__externalSeed = None
    ## mapping points to data blocks from measurements; list(int)
    self.__measDataIndex = []
    ## mapping points to data blocks from scatterers; list(int)
    self.__scatDataIndex = []
    ## label of point with measurements skipped in fit (for unbiased residuals)
    self.__skippedMeasLabel = 0
    
  ## Add point to trajectory. Points have to be ordered in arc length.
  #  
  #  @param point point to be added; GblPoint
  #  @return label of added point (1..number(points)); int   
  # 
  def addPoint(self, point):
    self.__numPoints += 1
    label = self.__numPoints
    point.setLabel(label)
    self.__points.append(point)
    self.__numLocals = max(self.__numLocals, point.getNumLocals())
    return label

  ## Get number of points on trajectory.
  #  
  #  @return number of points; int   
  # 
  def getNumPoints(self):
    return self.__numPoints
 
  ## Add external seed to trajectory.
  #  
  #  @param aLabel label of point with external seed; int 
  #  @param aSeed seed (precision matrix of track parameters at point); matrix(float)  
  #
  def addExternalSeed(self, aLabel, aSeed):
    self.__externalPoint = aLabel
    self.__externalSeed = aSeed

  ## Print points of trajectory.
  def printPoints(self):
    print "GblPoints"
    for p in self.__points:
      p.printPoint() 

  ## Print data of trajectory.          
  def printData(self):
    print "GblData blocks"
    for d in self.__data:
      d.printData() 

  ## Get data of trajectory.          
  def getData(self):
    return self.__data
 
  ## Write (data blocks of) trajectory to MP (binary) file (as *float* or *double* values).
  #  
  #  @param aFile MP file
  #  @param doublePrec flag for storage in as *double* values
  #     
  def milleOut(self, aFile, doublePrec=False):
    rec = MilleRecord(doublePrec)
#   data measurements and kinks        
    for aData in self.__data:       
      rec.addData(aData.toRecord())
                    
    rec.writeRecord(aFile)

  ## Read (data blocks of) trajectory from MP (binary) file.
  #  
  #  @param aFile MP file
  #
  def milleIn(self, aFile):
    rec = MilleRecord()
    rec.readRecord(aFile)
    mPar = 0
    mBor = 0
    mBand = 3 * len(self.__dimensions) - 1  # max band width
    while (rec.moreData()):
      aTag = rec.specialDataTag()
      if (aTag < 0):
# get data
        aData = GblData()
        aData.fromRecord(rec.getData())
        self.__data.append(aData)
        nPar, nBor = aData.analyzeData(mBand)
        mPar = max(mPar, nPar)
        mBor = max(mBor, nBor)
        
    self.__numParameters = mPar
    self.__numLocals = mBor - self.__numCurvature
 
  ## Get jacobian for transformation from fit to track parameters at point.
  #  
  #  @param aLabel (signed) label of point; int
  #  @return labels of required fit parameters and jacobian; list  
  #
  def __getJacobian(self, aLabel):
    aDim = self.__dimensions
    nDim = len(aDim)
    anIndex = abs(aLabel) - 1
#   check consistency of (index, direction)    
    if (aLabel > 0):
      nJacobian = 1
      if (anIndex >= self.__numPoints - 1):
        anIndex = self.__numPoints - 1
        nJacobian = 0
    else:
      nJacobian = 0
      if (anIndex <= 0):
        anIndex = 0
        nJacobian = 1
# Jacobian broken lines (q/p,..,u_i,u_i+1..) to local (q/p,u',u) parameters   
    nCurv = self.__numCurvature
    nLocals = self.__numLocals   
    nBorder = nCurv + nLocals
    nParBRL = nBorder + 2 * nDim
    nParLoc = nLocals + 5
    aJacobian = np.zeros((nParLoc, nParBRL))
    aPoint = self.__points[anIndex]
    anIndex = []
    labDer, matDer = self.__getFitToLocalJacobian(aPoint, 5, nJacobian)
#   from local parameters
    for i in range(nLocals):
      aJacobian[i + 5, i] = 1.0;
      anIndex.append(i + 1);
  
#   from trajectory parameters
    iCol = nLocals;
    for i in range(5):
      if (labDer[i] > 0):
        anIndex.append(labDer[i]);
        for j in range(5):
          aJacobian[j, iCol] = matDer[j, i];
        iCol += 1

    return anIndex, aJacobian

  ## Get (part of) jacobian for transformation from (trajectory) fit to track parameters at point.
  #
  #  Jacobian broken lines (q/p,..,u_i,u_i+1..) to local (q/p,u',u) parameters.
  #
  #  @param aPoint point to use; GblPoint
  #  @param measDim dimension of 'measurement' (2, 4 or 5); int
  #  @param nJacobian direction (0: to previous offset, 1: to next offset); int
  #  @return labels for fit parameters with non zero derivatives, 
  #           corresponding transformation matrix; list(vector(int), matrix(float))    
  #
  def __getFitToLocalJacobian(self, aPoint, measDim, nJacobian=1):
    aDim = self.__dimensions
    nDim = len(aDim)
    nCurv = self.__numCurvature
    nLocals = self.__numLocals      
    nOffset = aPoint.getOffset() 
    anIndex = [0, 0, 0, 0, 0]
    aJacobian = np.zeros((measDim, 5))
    labOffset = measDim - 2
    labSlope = measDim - 4
    labCurv = measDim - 5
    
    if (nOffset < 0):  # need interpolation
      prevW, prevWJ, prevWd = aPoint.getDerivatives(0)  # W-, W- * J-, W- * d-
      nextW, nextWJ, nextWd = aPoint.getDerivatives(1)  # W+, W+ * J+, W+ * d+
      sumWJ = prevWJ + nextWJ
      matN = np.linalg.inv(sumWJ)  # N = (W- * J- + W+ * J+)^-1 
#     local offset
      if (labOffset >= 0):
#       derivatives for u_int      
        prevNW = np.dot(matN, prevW)  # N * W-
        nextNW = np.dot(matN, nextW)  # N * W+
        prevNd = np.dot(matN, prevWd)  # N * W- * d-
        nextNd = np.dot(matN, nextWd)  # N * W+ * d+
        iOff = nDim * (-nOffset - 1) + nLocals + nCurv + 1  # first offset ('i' in u_i)
        if (nCurv > 0):
          aJacobian[labOffset:measDim, 0:1] = -prevNd - nextNd  # from curvature
          anIndex[0] = nLocals + 1
        aJacobian[labOffset:measDim, 1:3] = prevNW
        aJacobian[labOffset:measDim, 3:5] = nextNW
        for i in range(nDim):
          anIndex[1 + aDim[i]] = iOff + i
          anIndex[3 + aDim[i]] = iOff + nDim + i
#     local slope 
      if (labSlope >= 0):
#       derivatives for u'_int
        prevWPN = np.dot(nextWJ, prevNW)  # W+ * J+ * N * W-
        nextWPN = np.dot(prevWJ, nextNW)  # W- * J- * N * W+
        prevWNd = np.dot(nextWJ, prevNd)  # W+ * J+ * N * W- * d-
        nextWNd = np.dot(prevWJ, nextNd)  # W- * J- * N * W+ * d+
        if (nCurv > 0):
          aJacobian[labSlope:labOffset, 0:1] = prevWNd - nextWNd  # from curvature
        aJacobian[labSlope:labOffset, 1:3] = -prevWPN
        aJacobian[labSlope:labOffset, 3:5] = nextWPN

    else:  # at point               
#     anIndex must be sorted
#     forward : iOff2 = iOff1 + nDim, index1 = 1, index2 = 3
#     backward: iOff2 = iOff1 - nDim, index1 = 3, index2 = 1
      iOff1 = nDim * nOffset + nCurv + nLocals + 1  # first offset ('i' in u_i)
      index1 = 3 - 2 * nJacobian  # index of first offset
      iOff2 = iOff1 + nDim * (nJacobian * 2 - 1)  # second offset ('i' in u_i)
      index2 = 1 + 2 * nJacobian  # index of second offset
#     local offset
      if (labOffset >= 0):
        aJacobian[labOffset, index1] = 1.0  # from 1st Offset
        aJacobian[labOffset + 1, index1 + 1] = 1.0
        for i in range(nDim):
          anIndex[index1 + aDim[i]] = iOff1 + i
#     local slope and curvature
      if (labSlope >= 0):
        matW, matWJ, vecWd = aPoint.getDerivatives(nJacobian)  # W, W * J, W * d
        sign = 2 * nJacobian - 1
        if (nCurv > 0):
          aJacobian[labSlope:labOffset, 0:1] = -sign * vecWd  # from curvature
          anIndex[0] = nLocals + 1
        aJacobian[labSlope:labOffset, index1:index1 + 2] = -sign * matWJ
        aJacobian[labSlope:labOffset, index2:index2 + 2] = sign * matW
        for i in range(nDim):
          anIndex[index2 + aDim[i]] = iOff2 + i  
          
#   local curvature
    if (labCurv >= 0):
      if (nCurv > 0):
        aJacobian[labCurv, labCurv] = 1.0  
                    
    return anIndex, aJacobian    

  ## Get jacobian for transformation from (trajectory) fit to kink parameters at point.
  #
  #  Jacobian broken lines (q/p,..,u_i-1,u_i,u_i+1..) to kink (du') parameters.
  #
  #  @param aPoint point to use; GblPoint
  #  @return labels for fit parameters with non zero derivatives, 
  #           corresponding transformation matrix; list(vector(int), matrix(float))
  #   
  def __getFitToKinkJacobian(self, aPoint):              
    aDim = self.__dimensions
    nDim = len(aDim)
    nCurv = self.__numCurvature
    nLocals = self.__numLocals        
    nOffset = aPoint.getOffset() 
    anIndex = [0, 0, 0, 0, 0, 0, 0]
    aJacobian = np.zeros((2, 7))    

    prevW, prevWJ, prevWd = aPoint.getDerivatives(0)  # W-, W- * J-, W- * d-
    nextW, nextWJ, nextWd = aPoint.getDerivatives(1)  # W+, W+ * J+, W+ * d+
    sumWJ = prevWJ + nextWJ  # W- * J- + W+ * J+
    sumWd = prevWd + nextWd  # W+ * d+ + W- * d-
    iOff = (nOffset - 1) * nDim + nCurv + nLocals + 1  # first offset ('i' in u_i)

#   local offset
    if (nCurv > 0):
      aJacobian[:, 0:1] = -sumWd  # from curvature
      anIndex[0] = nLocals + 1      
    aJacobian[:, 1:3] = prevW  # from 1st Offset
    aJacobian[:, 3:5] = -sumWJ  # from 2nd Offset
    aJacobian[:, 5:7] = nextW  # from 3rd Offset
    for i in range(nDim):
      anIndex[1 + aDim[i]] = iOff + i
      anIndex[3 + aDim[i]] = iOff + nDim + i
      anIndex[5 + aDim[i]] = iOff + nDim * 2 + i
        
    return anIndex, aJacobian    

  ## Get residual and errors from data block.
  #  
  #  @param aData  data block
  #  @param used    flag for usage of data block in fit; bool
  #  @return residual, error of measurement and residual and down-weighting factor; list
  #
  def __getResAndErr(self, aData, used=True):
    aResidual, aMeasVar, aDownWeight, indLocal, derLocal = self.__data[aData].getResidual()
    aVec = np.array(derLocal)  # compressed vector
    aMat = self.__matrix.getBlockMatrix(indLocal)  # compressed matrix     
    aFitVar = np.dot(aVec, np.dot(aMat, aVec.T))  # variance from track fit
    aMeasError = math.sqrt(aMeasVar)  # error of measurement
    if used:
      aResError = math.sqrt(aMeasVar - aFitVar) if aFitVar < aMeasVar else 0.  # error of biased residual
    else:
      aResError = math.sqrt(aMeasVar + aFitVar)  # error of unbiased residual
    return aResidual, aMeasError, aResError, aDownWeight 
  
  ## Get results (corrections, covarinace matrix) at point in forward or backward direction.
  #  
  #  The point is identified by its label (1..number(points)), the sign distinguishes the 
  #  backward (facing previous point) and forward 'side' (facing next point). 
  #  For scatterers the track direction may change in between.
  #   
  #  @param aLabel signed label of point (<0 backward, >0 forward); int
  #  @return correction vector, covarinace matrix for track parameters; list
  #
  def getResults(self, aLabel):
    anIndex, aJacobian = self.__getJacobian(aLabel)
    nParBRL = len(anIndex)
    aVec = np.empty(nParBRL)
    for i in range(nParBRL):
      aVec[i] = self.__vector[anIndex[i] - 1]  # compressed vector
    aMat = self.__matrix.getBlockMatrix(anIndex)  # compressed matrix
    locPar = np.dot(aJacobian, aVec) 
    locCov = np.dot(aJacobian, np.dot(aMat, aJacobian.T))
    return  locPar, locCov 
  
  ## Get residuals at point from measurement.
  #
  # Get (diagonalized) residual, error of measurement and residual and down-weighting factor for measurement at point
  #  
  #  @param aLabel  label of point; int
  #  @return vectors with residuals, errors of maeansurements and residuals, down-weighting factors; list
  #
  def getMeasResults(self, aLabel):
    firstData = self.__measDataIndex[aLabel - 1]  # first data block with measurement
    numData = self.__measDataIndex[aLabel] - firstData  # number of data blocks
    if numData <= 0:
      return 0, None, None, None, None
    #
    aResiduals = np.empty(numData)
    aMeasErr = np.empty(numData)
    aResErr = np.empty(numData)
    aDownWeight = np.empty(numData)
    for i in range(numData):
      aResiduals[i], aMeasErr[i], aResErr[i], aDownWeight[i] = self.__getResAndErr(firstData + i, (aLabel <> self.__skippedMeasLabel))
    return numData, aResiduals, aMeasErr, aResErr, aDownWeight
 
  ## Get residuals at point from scatterer.
  #
  # Get (diagonalized) residual, error of measurement and residual and down-weighting factor for scatterering kinks at point
  #  
  #  @param aLabel  label of point; int
  #  @return vectors with residuals, errors of maeansurements and residuals, down-weighting factors; list
  #
  def getScatResults(self, aLabel):
    firstData = self.__scatDataIndex[aLabel - 1]  # first data block with measurement
    numData = self.__scatDataIndex[aLabel] - firstData  # number of data blocks
    if numData <= 0:
      return 0, None, None, None, None
    #
    aResiduals = np.empty(numData)
    aMeasErr = np.empty(numData)
    aResErr = np.empty(numData)
    aDownWeight = np.empty(numData)
    for i in range(numData):
      aResiduals[i], aMeasErr[i], aResErr[i], aDownWeight[i] = self.__getResAndErr(firstData + i)
    return numData, aResiduals, aMeasErr, aResErr, aDownWeight
 
  ## Get residuals from data of trajectory. 
  #
  #  @param iData  index of data block; int
  #         
  def getResidual(self, iData):
    return self.__getResAndErr(iData)   
         
  ## Perform fit of trajectory.
  #  
  #  @param optionList M-estimators to be used (one iteration per character); string
  #  @param aLabel label of point where to skip measurements (for unbiased residuals)
  #  @return Chi2, Ndf, loss of weight from fit ([0., -1, 0.] if fit failed); list
  #
  def fit(self, optionList="", aLabel=0):

    ## Define offsets from list of points.
    def defineOffsets():
# set labels for previous/next offsets
#     first point is offset    
      self.__points[0].setOffset(0)
      nOffsets = 1
#     intermediate scatterers are offsets    
      for aPoint in self.__points[1:-1]:
        if (aPoint.hasScatterer()):
          aPoint.setOffset(nOffsets)
          nOffsets += 1
        else:  
          aPoint.setOffset(-nOffsets)
#     last point is offset    
      self.__points[-1].setOffset(nOffsets)
      self.__numOffsets = nOffsets + 1
      self.__numParameters = self.__numOffsets * len(self.__dimensions) \
                           + self.__numCurvature + self.__numLocals       

    ## Calculate Jacobians to previous/next scatterer from point to point ones.
    def calcJacobians():
      scatJacobian = np.empty((5, 5)) 
#     forward propagation (all)
      lastPoint = 0;
      numStep = 0;
      for iPoint in range(1, self.__numPoints):
        if (numStep == 0):
          scatJacobian = self.__points[iPoint].getP2pJacobian()
        else: 
          scatJacobian = np.dot(self.__points[iPoint].getP2pJacobian(), scatJacobian)
        numStep += 1
        self.__points[iPoint].addPrevJacobian(scatJacobian)  # aPoint -> previous scatterer
        if (self.__points[iPoint].getOffset() >= 0):
          self.__points[lastPoint].addNextJacobian(scatJacobian)  # lastPoint -> next scatterer
          numStep = 0;
          lastPoint = iPoint
#     backward propagation (without scatterers)
      for iPoint in range(self.__numPoints - 1, 0, -1):
        if (self.__points[iPoint].getOffset() >= 0):
          scatJacobian = self.__points[iPoint].getP2pJacobian()
          continue  # skip offsets
        self.__points[iPoint].addNextJacobian(scatJacobian)  # iPoint -> next scatterer
        scatJacobian = np.dot(scatJacobian, self.__points[iPoint].getP2pJacobian())

    ## Prepare fit; generate data from points.     
    def prepare():
      aDim = self.__dimensions
# measurements
      self.__measDataIndex.append(len(self.__data))  # offset
      for aPoint in self.__points:
        if (aPoint.hasMeasurement()):
          nLabel = aPoint.getLabel()
          measDim = aPoint.getMeasDim()
          localDer = aPoint.getLocalDerivatives()
          globalLab = aPoint.getGlobalLabels()
          globalDer = aPoint.getGlobalDerivatives()
          matP, aMeas, aPrec = aPoint.getMeasurement()
          nJacobian = 1 if aPoint.getOffset() < self.__numOffsets - 1 else 0  # last point needs backward propagation
          labDer, matDer = self.__getFitToLocalJacobian(aPoint, measDim, nJacobian)
          matPDer = matDer if matP is None else np.dot(matP, matDer)
          for i in range(measDim):
            if (aPrec[i] > 0.):
              aData = GblData(nLabel, 1, aMeas[i], aPrec[i])
              aData.addDerivatives(i, labDer, matPDer, localDer, \
                                   globalLab, globalDer)
              self.__data.append(aData)
        self.__measDataIndex.append(len(self.__data))
#                aPoint.setDataMeas(i, len(self.__data)) 
# pseudo measurements from kinks
      self.__scatDataIndex.append(len(self.__data))  # offset
      self.__scatDataIndex.append(len(self.__data))  # first point
      for aPoint in self.__points[1:-1]:
        if (aPoint.hasScatterer()):
          nLabel = aPoint.getLabel()        
          matT, aMeas, aPrec = aPoint.getScatterer()
          labDer, matDer = self.__getFitToKinkJacobian(aPoint)
          matTDer = matDer if matT is None else np.dot(matT, matDer)
          for i in aDim:
            if (aPrec[i] > 0.):
              aData = GblData(nLabel, 2, aMeas[i], aPrec[i])
              aData.addDerivatives(i, labDer, matTDer)
              self.__data.append(aData)
        self.__scatDataIndex.append(len(self.__data))
#              aPoint.setDataScat(i, len(self.__data)) 
      self.__scatDataIndex.append(len(self.__data))  # last point
#     external seed
      if (self.__externalPoint != 0):
        externalIndex, aJacobian = self.__getJacobian(self.__externalPoint)
        eigenVal, eigenVec = np.linalg.eigh(self.__externalSeed)
        aMatrix = np.dot(eigenVec.T, aJacobian)
        for i in range(len(eigenVec)):
          if (eigenVal[i] > 0.):
            externalDerivatives = []
            for j in range(len(externalIndex)):
              externalDerivatives.append(aMatrix[i, j])
            aData = GblData(self.__externalPoint, 3, 0., eigenVal[i])
            aData.addExtDerivatives(externalIndex, externalDerivatives)
            self.__data.append(aData)
        self.__measDataIndex.append(len(self.__data))

    ## Build linear equation system from data.
    def buildLinearEquationSystem():
      nBorder = self.__numCurvature + self.__numLocals
      self.__matrix = BorderedBandMatrix(self.__numParameters, nBorder)
      self.__vector = np.zeros(self.__numParameters)
      for aData in self.__data:
        # skipped (internal) measurement?
        if aData.getLabel() == self.__skippedMeasLabel and aData.getType() == 1:
          continue 
        index, aVector, aMatrix = aData.getMatrices()
        for i in range(len(index)):
          self.__vector[ index[i] - 1 ] += aVector[0, i]  # update vector
        self.__matrix.addBlockMatrix(index, aMatrix)  # update matrix

    ## Down weight (data) outliers.
    #  
    #  @param aMethod M-estimator; int
    #  @return loss of weight (sum(1-down_weighting)); float
    #
    def downWeight(aMethod):
      aLoss = 0.
      for aData in self.__data: 
        aLoss += (1. - aData.setDownWeighting(aMethod))
      return aLoss
 
    ## Calculate predictions.
    def predict():
      for aData in self.__data: 
        aData.setPrediction(self.__vector)
    
    if (self.__data == []):  # generate data from points   
      defineOffsets()    
      calcJacobians()                      
      prepare()

    # skip measurements from point?
    self.__skippedMeasLabel = aLabel

    buildLinearEquationSystem()  # create linear equations system from data
#
    try:
      aMethod = 0
      lostWeight = 0.
      self.__vector = self.__matrix.solveAndInvertBorderedBand(self.__vector)
      predict()
      
      for o in optionList:  # down weighting iterations    
        try:
          aMethod = "THC".index(o.upper()) + 1
          lostWeight = downWeight(aMethod)
          buildLinearEquationSystem()
          self.__vector = self.__matrix.solveAndInvertBorderedBand(self.__vector)
          predict()
        except ValueError:
          pass                  
             
      Ndf = -self.__numParameters 
      Chi2 = 0.
      for aData in self.__data: 
        # skipped (internal) measurement?
        if aData.getLabel() == self.__skippedMeasLabel and aData.getType() == 1:
          continue 
        Chi2 += aData.getChi2()
        Ndf += 1
      Chi2 /= [1.0, 0.8737, 0.9326, 0.8228 ][aMethod]  
      return Chi2, Ndf, lostWeight
    
    except (ZeroDivisionError, np.linalg.linalg.LinAlgError):
      return  0., -1, 0.
    
