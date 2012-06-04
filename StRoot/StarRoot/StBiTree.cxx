#include "StBiTree.h"
#include <iostream>
#include <cmath>
#include "StDraw3D.h"
#include "TLine.h"
#include "TRandom.h"
using namespace std;


float line( float x, float A, float B, float C) 
   {
      float y = abs(B)>0.0001 ? -(A*x + C)/B : 1;
      cerr << __FUNCTION__ << " x ="<< x << "  y=" << y << endl;
      return  y;
   }
float line( float x, const vector <float> &v) 
{
    return line(x,v[0],v[1],v[2]);
}

namespace {
void plotplane(StDraw3D &dsp, const vector< float> plane, const vector < float > &point )
{
   
}
}
void testclass() {

   StDraw3D *dsp = new StDraw3D(0);
//   dsp->SetBkColor(kWhite);
   typedef StBiTree< vector<float> > Node3d;
   TRandom r;
#ifdef test3d
   Node3d a3;
   cout << "Empty:" << a3.IsEmpty() << endl;
   vector<float> b(3);
   for (int i=0; i<10; ++i) {
      b[0] =r.Rndm(); b[1]=r.Rndm(); b[2]=r.Rndm();
      a3.push_back(b);
      dsp->Point(b[0],b[1],b[2],kWhite,7,1);

   }   
   // find neibours
   b[0]= 0.5 ; b[1]= 0.5; b[2]=0.5;
   dsp->Point(b[0],b[1],b[2],kVtx);
   Node3d &l = *(a3.find(b));
   dsp->Point(l.Data()[0],l.Data()[1],l.Data()[2],kVtx);

   Node3d *next =  l.Parent();
   int nCounter = 0;
   while (next){
      Node3d *thisParent = next->Parent();
      if (thisParent) {
         Node3d *uncle = thisParent->Branch(next->WhereAmI() == Node3d::kLeft ? Node3d::kRight : Node3d::kLeft);
         Node3d &cousin = *uncle->find(b);
         dsp->Point(cousin.Data()[0],cousin.Data()[1],cousin.Data()[2],kUsedHit);
         dsp->Line(b[0],b[1],b[2],cousin.Data()[0],cousin.Data()[1],cousin.Data()[2],kBlue,0,3);
         dsp->Line(l.Data()[0],l.Data()[1],l.Data()[2],cousin.Data()[0],cousin.Data()[1],cousin.Data()[2],kRed,0,3);
         nCounter++;
         cout << nCounter << ". x =" << cousin.Data()[0]
         if (cousin.Data().size() >= 2 ) << "; y =" << cousin.Data()[1]
         if (cousin.Data().size() >= 3 ) cout << "; z =" << cousin.Data()[2]
         coud << endl;
      }
      next= thisParent;
   }
#else
   Node3d a2;
   vector<float> b(2);
#if 0
   b[0] =   0; b[1]= 0;   a2.push_back(b); dsp->Point(b[0],b[1],0,kWhite,7,1);
   b[0] =  -1; b[1]=-1;   a2.push_back(b); dsp->Point(b[0],b[1],0,kWhite,7,1);
   b[0] =-0.5; b[1]=-1;   a2.push_back(b); dsp->Point(b[0],b[1],0,kWhite,7,1);
   b[0] =+0.5; b[1]=-1;   a2.push_back(b); dsp->Point(b[0],b[1],0,kWhite,7,1);
   b[0] =+1.0; b[1]=-1;   a2.push_back(b); dsp->Point(b[0],b[1],0,kWhite,7,1);
   a2.Print();
   a2.PrintData();
#else
   for (int i=0; i<25; ++i) {
      b[0] =r.Rndm(); b[1]=r.Rndm();
      Node3d *p = a2.push_back(b)->Parent();
      dsp->Point(b[0],b[1],0,kYellow,3,1);
      if ( p ) 
      {
         float x = b[0];
         vector<float> pl = p->Data();
         cerr << " x0=" << x-0.3 << " y0=" << line(x-0.3,pl) 
            << " x1=" << x+0.3 << " y1=" << line(x+0.3,pl) 
            << endl;
         TObject *view1 = dsp->Line(x+1,line(x+1,pl),0.0,x-1,line(x-1,pl),0.0,kRed);
 
         vector<float> connection =  p->Left()->Data();
         connection.push_back(0);
         connection.push_back( p->Right()->Data()[0]);
         connection.push_back( p->Right()->Data()[1]);
         connection.push_back(0);
         TObject *view2 = dsp->Line(connection,kBlue);
         dsp->Update(true);
         //dsp->Animate();
         //delete view1; delete view2;
      }
   }   
#endif
   dsp->Update(true);
   dsp->SetDrawOption("{view:all}");
   // find neibours
   b[0]= 0.8 ; b[1]= 0.8;
   dsp->Point(b[0],b[1],0,kVtx);
   Node3d &l = *(a2.find(b));
   dsp->Point(l.Data()[0],l.Data()[1],0,kVtx);
   cout << " Depth = " <<  l.Depth() << endl;
   b = l.Data();
   Node3d *next =  l.Parent();
   int nCounter = 0;
   while (next){
      Node3d *thisParent = next->Parent();
      if (thisParent) {
         Node3d *uncle = thisParent->Branch(next->WhereAmI() == Node3d::kLeft ? Node3d::kRight : Node3d::kLeft);
         Node3d &cousin = *uncle->find(b);
         dsp->Point(cousin.Data()[0],cousin.Data()[1],0,kBlue,4,6);
         dsp->Point(cousin.Data()[0],cousin.Data()[1],0,kBlue,3,2);
         dsp->Point(cousin.Data()[0],cousin.Data()[1],0.01,kBlue,3,2);
         dsp->Point(cousin.Data()[0],cousin.Data()[1],-0.01,kBlue,3,2);
         dsp->Update(true);
     //    dsp->Line(b[0],b[1],0,cousin.Data()[0],cousin.Data()[1],0,kBlue,0,3);
         nCounter++;
        cout << nCounter << ". x =" << cousin.Data()[0]
         << "; y =" << cousin.Data()[1]
         << endl;
      }
      next= thisParent;
   }
#endif
//   TLine *l= new TLine();
//   gEventDisplay->Line(b2[0],b2[1],0,kVtx);
//   l->DrawLine(0,line(0.1,7,6,33.75),0.8,line(0.8,7,6,33.75));
//   l->DrawLine(0,line(0.1,-3,-2,-7.75),0.8,line(0.8,-3,-2,-7.75));
//   m2->Draw();


}