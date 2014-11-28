/***************************************************************************
 *
 * $Id: vecTest2.cc,v 1.2 2003/09/02 17:59:38 perev Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: vecTest2.cc,v $
 * Revision 1.2  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/02/17 12:44:04  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:55  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StGlobals.hh"
#include "StThreeVector.hh"

int main()
{
    cout << "This program tests the rotation and angular setting functions" << endl;
    cout << "-------------------------------------------------------------" << endl;

    StThreeVector<float>  vec1(3,4,5);
    StThreeVector<double> vec2(vec1);
	
    PR(vec1);
    PR(vec2);
    cout << endl;

    PR(vec1.mag());
    PR(vec1.theta());
    PR(vec1.phi());

    cout << "\nsetMagnitude(100) " << endl;
    vec1.setMagnitude(100);
    PR(vec1);
    PR(vec1.mag());
    PR(vec1.theta());
    PR(vec1.phi());
    PR(vec2);
    
    cout << "\nsetPhi(M_PI) " << endl;
    vec2.setPhi(M_PI);
    PR(vec2);
    PR(vec2.mag());
    PR(vec2.theta());
    PR(vec2.phi());
    
    cout << "\nsetTheta(M_PI) " << endl;
    vec2.setTheta(M_PI);
    PR(vec2);
    PR(vec2.mag());
    PR(vec2.theta());
    PR(vec2.phi());

    cout << "\nsetMagnitude(10) " << endl;
    vec1.setMagnitude(10);
    PR(vec1);
    StThreeVector<double> vec3(vec1.orthogonal());
    PR(vec3);
    PR(vec1*vec3);
    cout << endl;
    
    StThreeVector<double> vec4(0,1,0);
    PR(vec4);
    StThreeVector<float> vec5(1,1,0);
    PR(vec4.angle(vec5));
    cout << endl;
    
    double angle = M_PI/2;

    vec4.rotateX(angle);
    PR(vec4);
    vec4.rotateY(angle);
    PR(vec4);
    vec4.rotateZ(angle);
    PR(vec4);
    cout << endl;

    PR(vec5);
    vec5.rotateX(angle);
    PR(vec5);
    vec5.rotateY(angle);
    PR(vec5);
    vec5.rotateZ(angle);
    PR(vec5);

    return 0;

 
}
