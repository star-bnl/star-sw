//CombinationIterator_ex.cxx

//This file is an example of how to use the combination iterator.

#include "Stiostream.h"
using std::cout;
using std::endl;

#include <vector>
using std::vector;

#include <algorithm>
using std::copy;

using std::ostream_iterator;

#include <time.h>

#include "CombinationIterator.h"

int main()
{
    cout <<"Hello World!"<<endl;
    
    vector<double> vec1;
    vector<double> vec2;
    vector<double> vec3;    
    
    for (double i=1.; i<=3.; ++i) {
	vec1.push_back(i);
	vec2.push_back(10.*i);
	vec3.push_back(100.*i);
    }

    CombinationIterator<double> myIt;
    myIt.push_back(vec1.begin(), vec1.end());
    myIt.push_back(vec2.begin(), vec2.end());
    myIt.push_back(vec3.begin(), vec3.end());

    clock_t start = clock();
    for( ; myIt!=myIt.end(); ++myIt) {
	vector<double> combo = *myIt;
	cout <<"\n\t --- Next Combination --- "<<endl;
	copy((*myIt).begin(), (*myIt).end(),
	     ostream_iterator<double>(cout, " "));
	cout <<endl;
    }

    clock_t stop = clock();
    double time = (stop-start)/CLOCKS_PER_SEC;
    
    cout <<"\n\nElapsed Time:\t"<<time<<" seconds"<<endl;
    
    return 1;
}
