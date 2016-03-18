'''
Simple Test Program for General Broken Lines.

Created on Jul 27, 2011

@author: kleinwrt
'''

## \file
# examples
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
import time
from gblfit import GblPoint, GblTrajectory
#

## Create points on initial trajectory, create trajectory from points,
#  fit and write trajectory to MP-II binary file,
#  get track parameter corrections and covariance matrix at points.
#  
#  Equidistant measurement layers and thin scatterers, propagation 
#  with simple jacobian (quadratic in arc length differences).
#  Curvilinear system (T,U,V) or measurement system (I,J,K) as local
#  coordinate system and (q/p, slopes, offsets) as local track parameters.
#  
#  This example simulates and refits tracks in a system of planar detectors
#  with 2D measurements in a constant magnet field in Z direction using
#  the curvilinear system as local system. The true track parameters are
#  randomly smeared with respect to a (constant and straight) reference
#  trajectory with direction (lambda, phi) and are used (only) for the
#  on-the-fly simulation of the measurements and scatterers. The predictions
#  from the reference trajectory are therefore always zero and the residuals
#  needed (by addMeasurement) are equal to the measurements.
#
def example1():

#
  np.random.seed(47117)

  nTry = 1000  #: number of tries
  nLayer = 5  #: number of detector layers
  print " Gbltst $Rev: 116 $ ", nTry, nLayer
  start = time.clock()
# track direction
  sinLambda = 0.3
  cosLambda = math.sqrt(1.0 - sinLambda ** 2)
  sinPhi = 0.
  cosPhi = math.sqrt(1.0 - sinPhi ** 2)
# Curvilinear system: track direction T, U = Z x T / |Z x T|, V = T x U
  tuvDir = np.array([[cosLambda * cosPhi, cosLambda * sinPhi, sinLambda], \
                     [-sinPhi, cosPhi, 0.], \
                     [-sinLambda * cosPhi, -sinLambda * sinPhi, cosLambda]])
# measurement resolution
  measErr = np.array([ 0.001, 0.001])  # 10 mu
  measPrec = 1.0 / measErr ** 2
# scattering error
  scatErr = 0.001  # 1 mread
# RMS of CurviLinear track parameters (Q/P, slopes, offsets)
  clErr = np.array([0.001, -0.1, 0.2, -0.15, 0.25])
  # precision matrix for external seed (in local system)
  locSeed = None
  seedLabel = 0  # label of point with seed
  if seedLabel != 0:
    print " external seed at label ", seedLabel
#
  bfac = 0.2998  # Bz*c for Bz=1
  step = 1.5 / cosLambda  # constant steps in RPhi
#
  Chi2Sum = 0.
  NdfSum = 0
  LostSum = 0.
#
  binaryFile = open("milleBinaryISN.dat", "wb")
#
  for iTry in range(nTry):
# generate (CurviLinear) track parameters 
    clNorm = np.random.normal(0., 1., 5)  
    clPar = clErr * clNorm
    # covariance matrix
    clCov = np.eye(5)
    for i in range(5):
      clCov[i, i] = clErr[i] ** 2
# arclength
    s = 0.
# point-to-point jacobian (from previous point)    
    jacPointToPoint = np.eye(5)
# additional (local or global) derivatives    
    addDer = np.array([[1.0], [0.0]])
    labGlobal = np.array([[4711], [4711]])
# create trajectory
    traj = GblTrajectory(bfac != 0.)

# at previous point: transformation from local to curvilinear system 
    oldL2c = np.eye(5)
   
    for iLayer in range(nLayer):
      #print " layer ", iLayer
# measurement directions (J,K) from stereo angle
      sinStereo = (0. if iLayer % 2 == 0 else 0.1) 
      cosStereo = math.sqrt(1.0 - sinStereo ** 2)    
# measurement system: I, J ,K    
      ijkDir = np.array([[1., 0., 0.], \
                         [0., cosStereo, sinStereo], \
                         [0., -sinStereo, cosStereo]])
# local system: measurement or curvilinear 
      #local = gblMeasSystem(ijkDir, tuvDir)
      local = gblCurviSystem(ijkDir, tuvDir)
# projections
      proL2m = local.getTransLocalToMeas()
      proL2c = local.getTransLocalToCurvi()
      proC2l = np.linalg.inv(proL2c)
# projection curvilinear to measurement directions
      proC2m = local.getTransCurviToMeas()
# measurement - prediction in measurement system with error
      measNorm = np.random.normal(0., 1., 2)  
      meas = np.dot(proC2m, clPar[3:5]) + measErr * measNorm
# jacobian is calculated in curvilinear system and transformed
      jac = np.dot(proC2l, np.dot(jacPointToPoint, oldL2c))
# point with (independent) measurements (in measurement system)
      point = GblPoint(jac)
      point.addMeasurement([proL2m, meas, measPrec])
# additional local parameters?
#      point.addLocals(addDer)
# additional global parameters?
      point.addGlobals(labGlobal, addDer)
      addDer = -addDer  # locDer flips sign every measurement      
# add point to trajectory      
      iLabel = traj.addPoint(point)
      if iLabel == abs(seedLabel):
        clSeed = np.linalg.inv(clCov)
        locSeed = np.dot(proL2c, np.dot(clSeed, proL2c.T))
# propagate to scatterer
      jacPointToPoint = gblSimpleJacobian(step, cosLambda, bfac)
      clPar = np.dot(jacPointToPoint, clPar)
      clCov = np.dot(jacPointToPoint, np.dot(clCov, jacPointToPoint.T))
      s += step
      if (iLayer < nLayer - 1):
        scat = np.array([0., 0.])
# point with scatterer
        jac = np.dot(proC2l, np.dot(jacPointToPoint, proL2c))
        point = GblPoint(jac)
        scatP = local.getScatPrecision(scatErr)
        point.addScatterer([scat, scatP])
        iLabel = traj.addPoint(point)
        if iLabel == abs(seedLabel):
          clSeed = np.linalg.inv(clCov)
          locSeed = np.dot(proL2c, np.dot(clSeed, proL2c.T))

# scatter a little    
        scatNorm = np.random.normal(0., 1., 2)  
        clPar[1:3] = clPar[1:3] + scatErr * scatNorm
# propagate to next measurement layer    
        clPar = np.dot(jacPointToPoint, clPar)
        clCov = np.dot(jacPointToPoint, np.dot(clCov, jacPointToPoint.T))
        s += step
      oldL2c = proL2c
 
# add external seed
    if locSeed is not None:    
      traj.addExternalSeed(seedLabel, locSeed)
# dump trajectory
#    traj.dump()
  
# fit trajectory
    Chi2, Ndf, Lost = traj.fit()
    print " Record, Chi2, Ndf, Lost", iTry, Chi2, Ndf, Lost
# write to MP binary file    
#    traj.milleOut(binaryFile)
# sum up    
    Chi2Sum += Chi2
    NdfSum += Ndf
    LostSum += Lost
# get corrections and covariance matrix at points 
    if (iTry == 0):
      for i in range(1, nLayer + 1):      
        locPar, locCov = traj.getResults(-i)
        print " >Point ", i
        print " locPar ", locPar
        #print " locCov ", locCov      
        locPar, locCov = traj.getResults(i)
        print " Point> ", i
        print " locPar ", locPar
        #print " locCov ", locCov
# check residuals        
      for i in range(traj.getNumPoints()):
        numData, aResiduals, aMeasErr, aResErr, aDownWeight = traj.getMeasResults(i + 1)
        for j in range(numData):
          print " measRes " , i, j, aResiduals[j], aMeasErr[j], aResErr[j], aDownWeight[j]   
        numData, aResiduals, aMeasErr, aResErr, aDownWeight = traj.getScatResults(i + 1)
        for j in range(numData):
          print " scatRes " , i, j, aResiduals[j], aMeasErr[j], aResErr[j], aDownWeight[j]   
#
  end = time.clock()
  print " Time [s] ", end - start
  print " Chi2Sum/NdfSum ", Chi2Sum / NdfSum
  print " LostSum/nTry ", LostSum / nTry

## Read trajectory from MP-II binary file and refit.
#
def example2():
#  
  binaryFile = open("milleBinaryISN.dat", "rb")
  nRec = 0
  maxRec = 10  #: maximum number of records to read
  Chi2Sum = 0.
  NdfSum = 0
  LostSum = 0.
  start = time.clock()
  
  try:
    while(nRec < maxRec):
# create trajectory
      traj = GblTrajectory(0)
# read from file      
      traj.milleIn(binaryFile)  # get data blocks from file
      nRec += 1
# fit trajectory      
      Chi2, Ndf, Lost = traj.fit()
      print " Record, Chi2, Ndf, Lost", nRec, Chi2, Ndf, Lost
# sum up      
      Chi2Sum += Chi2
      NdfSum += Ndf
      LostSum += Lost 
         
  except EOFError:
    pass    
  
  print " records read ", nRec
  end = time.clock()
  print " Time [s] ", end - start
  print " Chi2Sum/NdfSum ", Chi2Sum / NdfSum
  print " LostSum/nTry ", LostSum / nRec
 
## Simple jacobian.
#  
#  Simple jacobian for (q/p, slopes, offsets) in curvilinear system
#  quadratic in arc length difference.
#
#  @param ds arc length difference; float
#  @param cosl cos(lambda); float
#  @param bfac Bz*c; float
#  @return jacobian to move by 'ds' on trajectory matrix(float)
#
def gblSimpleJacobian(ds, cosl, bfac):
  jac = np.eye(5)
  jac[1, 0] = -bfac * ds * cosl
  jac[3, 0] = -0.5 * bfac * ds * ds * cosl
  jac[3, 1] = ds
  jac[4, 2] = ds  
  return jac     

## Measurement system as local system
#
# In general the precision matrix for multiple scattering is not diagonal
#
class gblMeasSystem(object):
 
  ## Construct local system
  #
  #  @param  measDir (directions of) measurement system
  #  @param  curviDir (directions of) curvilinear system
  #  
  def __init__(self, measDir, curviDir):
    ## products (T,U,V) * (I,J,K)
    self.__prod = np.dot(curviDir, measDir.T)

  ## Transformation from local to measurement system 
  #
  #  @return None (systems are identical)
  #   
  def getTransLocalToMeas(self):
    return None

  ## Transformation of (q/p, slopes, offsets) from local to curvilinear system
  #
  #  @return Transformation for track parameters; 5*5 matrix
  #   
  def getTransLocalToCurvi(self):
    meas2crv = np.zeros((5, 5))
    meas2crv[0, 0] = 1.
    meas2crv[1:3, 1:3] = self.__prod[1:3, 1:3] * self.__prod[0, 0]  # (U,V)*(J,K) * T*I
    meas2crv[3:5, 3:5] = self.__prod[1:3, 1:3]  # (U,V)*(J,K)
    return meas2crv
 
  ## Transformation from curvilinear to measurement system 
  #
  #  @return Transformation for offsets; 2*2 matrix
  #   
  def getTransCurviToMeas(self):
    return np.linalg.inv(self.__prod[1:3, 1:3])  #((U,V)*(J,K))^-1
 
  ## Scattering precision matrix in local system  
  #
  #  @return Precision; 2*2 matrix
  #   
  def getScatPrecision(self, scatErr):
    c1 = self.__prod[0, 1]  # T*J
    c2 = self.__prod[0, 2]  # T*K
    fac = (1 - c1 * c1 - c2 * c2) / (scatErr * scatErr)
    scatP = np.empty((2, 2))
    scatP[0, 0] = fac * (1 - c1 * c1)
    scatP[0, 1] = fac * (-c1 * c2)
    scatP[1, 0] = fac * (-c1 * c2)
    scatP[1, 1] = fac * (1 - c2 * c2)
    return scatP       
 
## Curvilinear system as local system
#
# The precision matrix for multiple scattering (for slopes)
# is diagonal (multiple of unit matrix).
#
class gblCurviSystem(object):

  ## Construct local system
  #
  #  @param  measDir (directions of) measurement system
  #  @param  curviDir (directions of) curvilinear system
  #    
  def __init__(self, measDir, curviDir):
    ## projection curvilinear to measurement system: ((U,V)*(J,K))^-1
    self.__c2m = np.linalg.inv(np.dot(curviDir[1:3, 1:3], measDir[1:3, 1:3].T))

  ## Transformation from local to measurement system 
  #   
  #  @return Transformation for offsets; 2*2 matrix
  #   
  def getTransLocalToMeas(self):
    return self.__c2m  #((U,V)*(J,K))^-1

  ## Transformation of (q/p, slopes, offsets) from local to curvilinear system
  #
  #  @return Transformation for track parameters; 5*5 matrix
  # 
  def getTransLocalToCurvi(self):
    return np.eye(5)  # unit matrix 
 
  ## Transformation from curvilinear to measurement system 
  #   
  #  @return Transformation for offsets; 2*2 matrix
  #   
  def getTransCurviToMeas(self):
    return self.__c2m  #((U,V)*(J,K))^-1
 
  ## Scattering precision matrix in local system  
  #
  #  @return Precision diagonal; vector(2)
  #
  def getScatPrecision(self, scatErr):
    return np.array([1., 1.]) / (scatErr * scatErr)  # diagonal only     
        
# create points on initial trajectory, create trajectory from points,
# fit and write trajectory to MP-II binary file
# get track parameter corrections and covariance matrix at points
example1()
# read trajectory from MP-II binary file and refit
#example2()
