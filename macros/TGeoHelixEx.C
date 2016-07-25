#include "TCanvas.h"
#include "TPolyMarker3D.h"
#include "TPointSet3D.h"
#include "TRandom3.h"
#include "TGeoHelix.h"
#include "TStyle.h"

#include "TStopwatch.h"

TCanvas*       g_canvas;
TPolyMarker3D* g_pm;
TPointSet3D*   g_ps;
TRandom3 g_rnd;

void gltracks(const Int_t N=500)
{
  // Fake N-tracks with points.
  //
  // Use flat curvature/z-step distribution.
  // Step by 1cm until R_max, Z_max is reached or number of points
  // exceeds N_max_points

  const Int_t    N_max_points = 1000;
  const Double_t R_max        = 2;
  const Double_t Z_max        = 3;

  const Double_t R_max_sqr    = R_max*R_max;

  Int_t N_all = 0;


  g_pm = new TPolyMarker3D(200*N);
  g_ps = new TPointSet3D(200*N);

  for(int i=0; i<N; ++i) {

    Double_t curv  =  2*g_rnd.Rndm();
    Double_t zstep = 10*g_rnd.Rndm();
    Double_t phi   = TMath::TwoPi()*g_rnd.Rndm();
    
    TGeoHelix h(curv, zstep, g_rnd.Rndm() > 0.5 ? 1 : -1);
    h.SetField(0, 0, 1);
    h.InitPoint(0.1*(2*g_rnd.Rndm() - 1),
		0.1*(2*g_rnd.Rndm() - 1),
		0.1*(2*g_rnd.Rndm() - 1));
    h.InitDirection(TMath::Cos(phi)/curv,
		    TMath::Sin(phi)/curv,
		    (g_rnd.Rndm() > 0.5 ? 1 : -1)*zstep/curv,
		    kFALSE);

    const Double_t* p = h.GetCurrentPoint();
    for(int j=0; j<N_max_points; ++j) {
      g_pm->SetNextPoint(p[0], p[1], p[2]);
      g_ps->SetNextPoint(p[0], p[1], p[2]);
      ++N_all;

      h.Step(0.01);
      if(TMath::Abs(p[2]) > Z_max || p[0]*p[0]+p[1]*p[1] > R_max_sqr)
	break;
    }
  }
  
  printf("Created %d tracks, number of all points %d.\n", N, N_all);

  g_canvas = new TCanvas;
  g_canvas->Divide(2, 1);
  
  g_canvas->cd(1);
  gPad->GetViewer3D("ogl"); // Force creation of OpenGL viewer
  g_pm->Draw("ogl");
  
  g_canvas->cd(2); 
  gPad->GetViewer3D("ogl");
  g_ps->Draw("ogl");

  printf("Call update() to test refresh speeds.\n");
}

void TGeoHelixEx() //update()
{
  TStopwatch t;

  t.Start();
  g_pm->SetMarkerColor(Color_t(1 + 6*g_rnd.Rndm()));
  g_canvas->cd(1);
  gPad->Modified();
  gPad->Update();
  t.Stop();
  printf("PolyMarker %lf\n", t.RealTime());

  t.Start();
  g_ps->SetMarkerColor(Color_t(1 + 6*g_rnd.Rndm()));
  g_canvas->cd(2);
  gPad->Modified();
  gPad->Update();
  t.Stop();
  printf("PointSet   %lf\n", t.RealTime());
}
