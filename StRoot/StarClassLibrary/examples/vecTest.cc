/***************************************************************************
 *
 * $Id: vecTest.cc,v 1.4 2003/09/02 17:59:38 perev Exp $
 *
 * Author: Thomas Ullrich, April 1998
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: vecTest.cc,v $
 * Revision 1.4  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  1999/12/21 15:15:01  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/06/04 18:04:57  ullrich
 * Added testing of new features
 *
 * Revision 1.1  1999/02/17 12:44:04  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:54  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StGlobals.hh"
#include "StLorentzVector.hh"

int main()
{
	cout << "This program tests the StThreeVector and StLorentzVector classes" << endl;
	cout << "----------------------------------------------------------------" << endl;

	cout << "\nTesting StThreeVector:" << endl;

	StThreeVector<float>  vec1;
	StThreeVector<float>  vec2(1,2,3);
	StThreeVector<double> vec3(vec2);
#ifdef ST_NO_TEMPLATE_DEF_ARGS
	StThreeVector<double> vec4 = vec3;
	StThreeVector<double> vec5(vec4);
#else	
	StThreeVector<>       vec4 = vec3;
	StThreeVector<>       vec5(vec4);
#endif
	double *a = new double[3];
	a[0] = a[1] = a[2] = 42;
	float *b = new float[3];
	b[0] = b[1] = b[2] = 99;
	StThreeVector<double> vec6(a);
	StThreeVector<double> vec7(b);
	    
	PR(vec1);
	PR(vec2);
	PR(vec3);
	PR(vec4);
	PR(vec6);
        vec6[2] = 17;
	PR(vec6);
	PR(vec7);
        vec7[2] = 11;
	PR(vec7);
	
	
	PR(vec5 = vec4);
	PR(vec5 = vec1);
	
	PR(vec1 += vec2);
	PR(vec1 -= vec4);
	PR(vec4 *= 3.14);
	PR(vec4 /= 1.77);
    
	PR(vec4 == vec2);
	PR(vec1 != vec2);

	PR(vec1 = vec2+vec4);
	PR(vec2 = vec1/2.33);
	vec3.setX(3.32);
	vec3.setZ(0.087);
	PR(vec3);
        PR(vec4 = vec3-0.24*vec4);

	PR(vec1*vec2);
	PR(vec1.dot(vec2));
	PR(vec1.cross(vec2));
	PR(vec1.angle(vec4));

	PR(abs(vec2));
	PR(vec2.mag());
	PR(vec2.mag2());

	PR(vec3.unit());
	PR(abs(vec3.unit()));

	PR(vec1.theta());
	PR(vec1.cosTheta());
	PR(vec1.phi());
	PR(vec1.perp());
	PR(vec1.perp2());
	PR(vec1.pseudoRapidity());

	PR(vec4[0]);
	PR(vec4(1));

	cout << "Testing exception handling." << endl;
	cout << "Note that depending on your platform" << endl;
	cout << "the resulting actions might differ." << endl;
#ifndef ST_NO_EXCEPTIONS
	try {
	    PR(vec4[3]);
	}
	catch (out_of_range &e) {
  	    cout << "Caught exception: " << e.what() << endl;
  	}
#else
	PR(vec4[3]);
#endif
	cout << "Done" << endl;
	
	cout << "\nTesting StLorentzVector:" << endl;
	
	StLorentzVector<double> lvec1;
 	StLorentzVector<float>  lvec2(1,2,3,40);
 	StLorentzVector<double> lvec3(lvec2);
#ifdef ST_NO_TEMPLATE_DEF_ARGS
 	StLorentzVector<double> lvec4 = lvec3;
        StLorentzVector<double> lvec5(vec3, 190.2);
        StLorentzVector<double> lvec6;
#else
 	StLorentzVector<>       lvec4 = lvec3;
        StLorentzVector<>       lvec5(vec3, 190.2);
        StLorentzVector<>       lvec6;
#endif
	StLorentzVector<double> lvec7(lvec5);
	StLorentzVector<double> lvec8(lvec5);
	
 	PR(lvec1);
 	PR(lvec2);
 	PR(lvec3);
 	PR(lvec4);
 	PR(lvec5);
	PR(lvec7);
        lvec7[3] = 17;
	PR(lvec7);
	PR(lvec8);
        lvec8[3] = 11;
	PR(lvec8);
	
 	PR(lvec6 = lvec5);
 	PR(lvec6 = lvec2);
	
 	PR(lvec2.x());
 	PR(lvec2.y());
 	PR(lvec2.z());
 	PR(lvec2.t());
 	PR(lvec2.e());
	
 	PR(lvec1 += lvec2);
 	PR(lvec1 -= lvec4);
 	PR(lvec4 *= 3.14);
 	PR(lvec3 /= 1.72);
	
 	PR(lvec2 == lvec4);
 	PR(lvec1 != lvec2);
	
 	PR(lvec1 = lvec2+lvec4);
 	PR(lvec2 = lvec1/2.33);
 	lvec3.setX(3.32);
 	lvec3.setZ(0.087);
 	PR(lvec3);
        PR(lvec4 = lvec3-0.24*lvec4);

 	lvec2.setVect(vec2);
  	PR(lvec1);
  	PR(lvec2);
 	PR(lvec1*lvec2);

 	PR(abs(lvec2));
 	PR(lvec2.m());
 	PR(lvec2.m2());
 	PR(lvec2.mt());
 	PR(lvec2.mt2());

 	PR(lvec3.vect().unit());

 	PR(lvec1.theta());
 	PR(lvec1.cosTheta());
 	PR(lvec1.phi());
 	PR(lvec1.perp());
 	PR(lvec1.perp2());
 	PR(lvec1.pseudoRapidity());
 	PR(lvec1.rapidity());

 	PR(lvec4[0]);
 	PR(lvec4(1));

	PR(lvec1.plus());
	PR(lvec1.minus());

	cout << "\nConstruct a 4-vector of a pion:" << endl;
	StThreeVector<double> pionMomentum(100, 200, 300);
	cout << "with momentum: " << pionMomentum << endl;
	cout << "Energy should be: " << pionMomentum.massHypothesis(139) << endl;;
	
	StLorentzVector<double> pion(pionMomentum, pionMomentum.massHypothesis(139));
	cout << "pion is: " << pion << endl;
	double mass = ::sqrt(sqr(pion.e()) - pion.vect().mag2());
	cout << "Mass is: (" << pion.e() << ")^2 - (" << pion.vect().mag() << ")^2 = " <<  mass << endl;

	
	cout << "\nTesting exception handling." << endl;
 	cout << "Note that depending on your platform" << endl;
 	cout << "the resulting actions might differ." << endl;
#ifndef ST_NO_EXCEPTIONS
 	try {
 	    PR(lvec4[4]);
 	}
  	catch (out_of_range &e) {
 	    cout << "Caught exception: " << e.what() << endl;
 	}
#else
	PR(lvec4[4]);	
#endif	
 	cout << "Done" << endl;
 	cout << "Test finished" << endl;
	
	return 0;
}
