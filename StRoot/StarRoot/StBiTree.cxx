#include "StBiTree.h"
#include <iostream>
using namespace std;
void testclass() {
   StBiTree< vector<float> > a;
   cout << "Empty:" << a.IsEmpty() << endl;
   vector<float> b(3);
   b[0] =4; b[1]=3; b[2]=2;
   a.push_back(b);
   // a.begin();
   b[0] =1; b[1]=3; b[2]=8;
   a.push_back(b);
   b[0] =8; b[1]=9; b[2]=10;
   cout << "Empty:" << a.IsEmpty() << endl;
   a.push_back(b);
   a.Print(5);
   a.PrintData(5);
   cout << __FUNCTION__ << "  depth="<< a.Depth()<< endl;
}