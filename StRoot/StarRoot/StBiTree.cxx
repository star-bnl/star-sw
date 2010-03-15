#include "StBiTree.h"
#include <iostream>
#include <cmath>
#include "StDraw3D.h"
#include "TLine.h"
using namespace std;

float line( float x, float A, float B, float C) 
   {
      float y = abs(B)>0.0001 ? -(A*x + C)/B : 1;
      cout << __FUNCTION__ << " x ="<< x << "  y=" << y << endl;
      return  y;
   }
void testclass() {

   StDraw3D *dsp = new StDraw3D(0);
   StBiTree< vector<float> > a3;
   cout << "Empty:" << a3.IsEmpty() << endl;
   vector<float> b(3);
   b[0] =4; b[1]=3; b[2]=2;
   a3.push_back(b);
   a3.begin();
   b[0] =1; b[1]=3; b[2]=8;
   a3.push_back(b);
   b[0] =8; b[1]=9; b[2]=10;
   cout << "Empty:" << a3.IsEmpty() << endl;
   a3.push_back(b);
   a3.Print(5);
   a3.PrintData(5);
   
   StBiTree< vector<float> > a2;
   cout << "Empty:" << a2.IsEmpty() << endl;
   vector<float> b2(2);
   b2[0] =4; b2[1]=5;
   dsp->Point(b2[0],b2[1],0,kVtx);
   a2.push_back(b2);
   // a.begin();
   b2[0] =1; b2[1]=3; 
   dsp->Point(b2[0],b2[1],0,kVtx);
   a2.push_back(b2);
   b2[0] =8; b2[1]=9;
   dsp->Point(b2[0],b2[1],0,kVtx);
   cout << "Empty:" << a2.IsEmpty() << endl;
   a2.push_back(b2);
   a2.Print(5);
   a2.PrintData(5);
   dsp->Point(0,line(0.1,7,6,33.75),0,kBlue,3);
   dsp->Point(1,line(1,7,6,33.75),0,kBlue,3);
   dsp->Point(0,line(0.1,-3,-2,-7.75),0,kBlue,3);
   dsp->Point(1,line(1,-3,-2,-7.75),0,kBlue,3);

//   TLine *l= new TLine();
//   gEventDisplay->Line(b2[0],b2[1],0,kVtx);
//   l->DrawLine(0,line(0.1,7,6,33.75),0.8,line(0.8,7,6,33.75));
//   l->DrawLine(0,line(0.1,-3,-2,-7.75),0.8,line(0.8,-3,-2,-7.75));
//   m2->Draw();


}