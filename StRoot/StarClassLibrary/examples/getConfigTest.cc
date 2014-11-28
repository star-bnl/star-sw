/***************************************************************************
 *
 * $Id: getConfigTest.cc,v 1.4 2003/09/02 17:59:37 perev Exp $
 *
 * Author: Thomas Ullrich, May 1998
 ***************************************************************************
 *
 * Description:  Example/test program for StGetConfigValue utility
 *
 ***************************************************************************
 *
 * $Log: getConfigTest.cc,v $
 * Revision 1.4  2003/09/02 17:59:37  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  1999/12/21 15:14:41  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/05/19 21:14:42  ullrich
 * Corrected path of input file
 *
 * Revision 1.1  1999/02/17 12:43:56  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:41  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StGlobals.hh"
#include "StGetConfigValue.hh"
#include "StThreeVector.hh"
#include <vector>
#include <unistd.h>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

int main()
{
    const char* filename = "example.conf";

    if (access(filename, R_OK)) {
	cerr << "Cannot access file '" << filename << "'" << endl;
	return 1;
    }
    
    double singleValue = 10;
    StGetConfigValue(filename, "singleValue", singleValue);
    cout << "singleValue = " << singleValue << endl;

    float *manyValues = new float[10];
    StGetConfigValue(filename, "manyValues", manyValues, 10);
    cout << "manyValues = ";
    for (int i=0; i<10; i++) cout << manyValues[i] << ' ';
    cout << endl;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
    vector<double, allocator<double> > vec(10,0);
#else
#ifdef GNU_GCC
    vector<double> vec(10);
#else
    vector<double> vec(10,0);
#endif // GNU_GCC  %$#&
#endif
    StGetConfigValue(filename, "vec", vec, 5);
    cout << "vec = ";
    for (int k=0; k<10; k++) cout << vec[k] << ' ';
    cout << endl;

    StThreeVector<double> vec3;
    StGetConfigValue(filename, "vec3", vec3);
    cout << "vec3 = " << vec3 << endl;
    
#if !defined(__SUNPRO_CC)
    string anyName("default");
    StGetConfigValue(filename, "anyName", anyName);
    cout << "anyName = " << anyName.c_str() << endl;
#endif
    
    float xfoo = 3.14;
    StGetConfigValue(filename, "xfoo", xfoo);
    cout << "xfoo = " << xfoo << endl;

    return 0;
}
