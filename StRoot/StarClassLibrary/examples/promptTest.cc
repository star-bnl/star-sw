/***************************************************************************
 *
 * $Id: promptTest.cc,v 1.3 2000/02/02 19:05:18 ullrich Exp $
 *
 * Author: Thomas Ullrich, April 1998
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: promptTest.cc,v $
 * Revision 1.3  2000/02/02 19:05:18  ullrich
 * Changed macros for CC5/CC4.2 compatibility
 *
 * Revision 1.2  1999/12/21 15:14:56  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/02/17 12:44:01  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:49  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StPrompt.hh"
#include "StThreeVector.hh"
#include <string>
#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif

int main()
{
	StThreeVector<float>  vec2(1,2,3);
	StPrompt("Enter new 3-vector", vec2);
	cout << "new value: " << vec2 << endl;

	string filename("var.cc");
	char   othername[16] = "short.c";
	double var = 1/3.;
	
	bool answer = true;
	while (answer) {
	    StPrompt("Enter file name", filename);
	    cout << "new value: " << filename.c_str() << endl;
	    StPrompt("Enter other name", othername, 16);
	    cout << "new value: " << othername << endl;

	    StPrompt("any number", var);
	    cout << "new value: " << var << endl;

#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500  
	    StBoolPrompt("more questions", answer);
#else
	    StPrompt("more questions", answer);
#endif
	}
	
	StPrompt();
	cout << "bye" << endl;
	return 0;
}
