//StiCircleCalculator.cxx
//M.L. Miller (Yale Software)
//10/01

//Std
#include <math.h>
#include <Stiostream.h>

//Scl
#include "StThreeVectorF.hh"
#include "StThreeVector.hh"

//Sti
#include "StiCircleCalculator.h"

StiCircleCalculator::StiCircleCalculator()
    : mMessenger( *(Messenger::instance(MessageType::kSeedFinderMessage))),
      mRadius(-1.), mXCenter(0.), mYCenter(0.), mProbableH(0.)
{
}

StiCircleCalculator::~StiCircleCalculator()
{
}

void StiCircleCalculator::calculate(const StThreeVector<double>& pt1,
				    const StThreeVector<double>& pt2,
				    const StThreeVector<double>& pt3)
{
    //mMessenger <<"StiCircleCalculator::calculate()"<<endl;
    //mMessenger <<"pt1: "<<pt1<<" pt2: "<<pt2<<" pt3: "<<pt3<<endl;

    mRadius=-1.;
    mXCenter=0.;
    mYCenter=0.;
    mProbableH=0.;
    
    calculateRadius(pt1, pt2, pt3);
    calculateCenter(pt1, pt2, pt3);
    calculateProbableH(pt1, pt2, pt3);
    
    return;
}

void StiCircleCalculator::calculateRadius(const StThreeVector<double>& pt1,
					  const StThreeVector<double>& pt2,
					  const StThreeVector<double>& pt3)
{
    //  For an inscribed triangle
    //  a/sin(A) = 2r
    //                               ^
    //          p2 (v2)            y |
    //             #                 |   # denotes given point
    //            / \                |
    //           / C \               |
    //        a /     \ b             ------>
    //         /       \                    x
    //        /         \  
    //  p1   / B       A \  p3
    // (v1) #-------------# (v3)
    //            c

    //mMessenger <<"StiCircleCalculator::calculateRadius()"<<endl;
    //mMessenger <<"pt1: "<<pt1<<" pt2: "<<pt2<<" pt3: "<<pt3<<endl;

    StThreeVector<double> a = pt2-pt1;
    StThreeVector<double> c = pt3-pt1;
    StThreeVector<double> b = pt3-pt2;

    double B = acos((a*c)/(abs(a)*abs(c)));
    
    mRadius = abs(b)/(2*sin(B));
}

void StiCircleCalculator::calculateCenter(const StThreeVector<double>& pt1,
					 const StThreeVector<double>& pt2,
					 const StThreeVector<double>& pt3)
{
    //              o (xc, yc) 
    //  p1         /|          # denotes given points
    //   #       /  |          o denotes origin of the circle
    //    \    /    |
    //    '\ /      |        The point where two bisectors intersect
    //     /\       |        is the center of a circle on which the
    //   /  ,\ p2   |        points lie.
    //        #-----------#
    //           "  |  "   p3
    //              |
    //              |
    //            c

    // !!!CAUTION: Only for Bz right now!!!!
    //

    //mMessenger <<"StiCircleCalculator::calculateCenter()"<<endl;
    //mMessenger <<"pt1: "<<pt1<<" pt2: "<<pt2<<" pt3: "<<pt3<<endl;
        
    // construct the line of bisector 1 (between v1 and v2)
    StThreeVector<double> k1 = pt1-pt2;    
    StThreeVector<double> mid1 = pt2+(k1.unit()*abs(k1)/2);	// midpoint of v1.v2
    double theta1 = atan(k1.y()/k1.x());
    
    // The bisector is a line rotated at PI/2 going thru mid1
    // and given by:
    // y = tan(theta1+M_PI/2)(x-mid1.x())+mid1.y();
    StThreeVector<double> k2 = pt2-pt3;
    
    StThreeVector<double> mid2 = pt3+(k2.unit()*abs(k2)/2);    // midpoint of v2.v3
    double theta2 = atan(k2.y()/k2.x());
    
    // The bisector is a line rotated at PI/2 going thru mid2
    // and given by:
    // y = tan(theta2+M_PI/2)(x-mid2.x())+mid2.y();
    
    // They intersect at (xc,yc)
    double m1 = tan(theta1+M_PI/2);
    double m2 = tan(theta2+M_PI/2);
    
    mXCenter = (m1*mid1.x()-m2*mid2.x()+mid2.y()-mid1.y())/(m1-m2);
    mYCenter = m1*(mXCenter-mid1.x())+mid1.y();
}

void StiCircleCalculator::calculateProbableH(const StThreeVector<double>& pt1,
					     const StThreeVector<double>& pt2,
					     const StThreeVector<double>& pt3)
{
    //*********************************************
    // Given 3 pixels: v1; v2; v3
    //
    //**** ASSUME: v1 < v2 < v3
    //
    //  ^
    // y|
    //  |   v3 x
    //  |      |     v2
    //  |      |   x
    //  |   k1 |Þ /   where Þ = angle theta between :
    //  |      | / k2           vectors v1 and v2.
    //  |      |/
    //  |   v1 x      If they are unit vectors:
    //  |
    //  |-----------------------------> x
    //
    //   k1 = ( x' )    k2 = ( x )
    //        ( y' )         { y )
    //
    //                xy' -x'y
    //   Þ = arcsin (----------)
    //                x^2 + y^2
    //
    // The sign of h is given by:
    //
    //        h = signOf(-Þ)
    //
    
    StThreeVector<double> k1 = (pt3-pt1).unit();
    StThreeVector<double> k2 = (pt2-pt1).unit();
    
    // this is the original (how I thought it should be done:
    // actually should be the negative of this
    double beta = asin((k1.x()*k2.y() - k2.x()*k1.y())/(k1.perp2()));

    // inverse
    // beta = asin((k2.x()*k1.y() - k1.x()*k2.y())/(k1.perp2()));
    // probable h:
    mProbableH = (beta>0) ? -1. : 1.;
}
