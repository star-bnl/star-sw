//StFastLineFitter_ex.cxx

#include "Sti/StiFastLineFitter.h"
#include "Stiostream.h"
#include <cmath>

//This is meant as an example of how to use StFastLineFitter_ex.cxx
int main()
{
    StFastLineFitter myFitter;
    myFitter.clear();
    
    double slope = 3.;
    double intercept = 7.;
    for (double x=1.; x<=10.; ++x) {
	double y=slope*x + intercept;
	double weight = ::sqrt(y);
	myFitter.addPoint(x, y, weight);
    }
    bool rc = myFitter.fit();
    if (myFitter.rc()!=0) {
	cout <<"ERROR:\tFit failed."<<endl;
    }	
    myFitter.print();
    
    return 1;
}
