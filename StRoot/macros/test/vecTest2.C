#include "Riostream.h"
#include "StThreeVector.hh"
#define PR(x) cout << (#x) << " = " << (x) << endl;

void vecTest2()
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

}
