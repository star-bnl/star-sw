#ifndef StiKalmanTrackNode_H
#define StiKalmanTrackNode_H 1
#include <iostream.h>
#include <stdlib.h>
#include "Exception.h"
#include "StiTrackNode.h"

class StiHit;
class StiDetector;

class StiKalmanTrackNode : public StiTrackNode 
{
    /* 
     * A work class used to handle Kalman filter information while
     * constructing track nodes.
     */
    
public:
    
    void reset();
    void set(int   depth,
	     StiHit * hit,
	     const double alpha,
	     const double xRef,
	     const double xx[5], 
	     const double cc[15], 
	     const double dEdx,
	     const double chi2);
    void setState(const StiKalmanTrackNode * node);
    void get(double& alpha,
	     double& xRef,
	     double x[5], 
	     double cc[15], 
	     double& dEdx,
	     double& chi2);
    void getMomentum(double p[3], double e[6]=0) const;
    double getTanL() const;
    double getPt() const;
    void getGlobalMomentum(double p[3], double e[6]=0) const;
    void setAsCopyOf(const StiKalmanTrackNode * node);
    int  propagate(StiKalmanTrackNode *p, StiDetector * tDet);	//throw (Exception);
    void propagate(double x, 
		   double x0,   
		   double rho); //throw (Exception); // mass hypothesis
    double evaluateChi2(); //throw ( Exception);
    void updateNode(); //throw (Exception);
    void extendToVertex(); //throw (Exception);
    void rotate(double alpha); //throw ( Exception);
    
    double fAlpha;          // rotation angle
    double fX;              // X-coordinate of this track (reference plane)
    double fP0;             // Y-coordinate of a track
    double fP1;             // Z-coordinate of a track
    double fP2;             // C*x0
    double fP3;             // track curvature==C
    double fP4;             // tangent of the track momentum dip angle
    double fC00;                         // covariance
    double fC10, fC11;                   // matrix
    double fC20, fC21, fC22;             // of the
    double fC30, fC31, fC32, fC33;       // track
    double fC40, fC41, fC42, fC43, fC44; // parameters
    double fdEdx;           // dE/dx 
    double fChi2;
    
    // static methods
    static void    setFieldConstant(double f) { kField = f;};
    static double  getFieldConstant()         { return kField;}; 
    static void    setMassHypothesis(double m);
    static double  getMassHypothesis(); 
    static void    setElossCalculated(bool option);
    static void    setMCSCalculated(bool option);
    static bool StiKalmanTrackNode::getElossCalculated();
    static bool StiKalmanTrackNode::getMCSCalculated();  
    static double  unitCharge;
    
    friend ostream& operator<<(ostream& os, const StiKalmanTrackNode& n);
    
    static bool  recurse;
    
protected:   
    
    static double kField;
    static bool   elossCalculated;
    static bool   mcsCalculated;
    static double massHypothesis;
};

#endif
