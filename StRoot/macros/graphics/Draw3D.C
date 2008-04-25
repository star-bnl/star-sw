#ifndef __CINT__
#  include "StEventHelper.h"
#  include "StTrack.h"
#  include "StMeasuredPoint.h"
#  include "TPolyMarker3D.h"
#  include "TPolyLine3D.h"
#  include "TSystem.h"
#  include "TROOT.h"
#  include "TEnv.h"
#  include "StCheckQtEnv.h"
#  include "TStyle.h"
#else
  class StTrack; 
  class StMeasuredPoint;
#endif  
//static 
      int ___draw_3d_init = 0;
// static 
      const Color_t colorDefault = Color_t(-1);
// static 
      const Style_t styDefault   = Style_t(-1);
// static 
      const Size_t  sizDefault   = Size_t (-1);
// static 
      Color_t bkColor = kBlack;
//___________________________________________________
void Draw3DDraw(TObject *o)
{
   if (o) {
      o->Draw();
      if (gPad->GetFillColor() != bkColor)
      gPad->SetFillColor(bkColor);
   }
}
//___________________________________________________
void Draw3DBkColor(Color_t newBkColor)
{
    bkColor = newBkColor;
}

//___________________________________________________
int Draw3DInit(){
   if (___draw_3d_init) return 1;
   ___draw_3d_init = 1;
   gROOT->Macro("Load.C");
   if (!StCheckQtEnv::SetQtEnv(false)) {
      // define the background image
      const char *backShape = "$STAR/StRoot/macros/graphics/StarTPC.iv";
      printf(" Setting the background shape to be 	%s\n", backShape);
      gEnv->SetValue("Gui.InventorBackgroundShape",backShape);
   }
   return 1;
}
//___________________________________________________
TObject *Draw3DPoints(int n, const float *xyz, Color_t col=colorDefault,Style_t sty=styDefault,Size_t siz=sizDefault)
{
   if (!___draw_3d_init) Draw3DInit();
   TPolyMarker3D *plMk  = new TPolyMarker3D(n,(Float_t*)xyz);
   if (col != colorDefault) plMk->SetMarkerColor(col);
   if (sty != styDefault)   plMk->SetMarkerStyle(sty);
   if (siz != sizDefault)   plMk->SetMarkerSize(siz);
   Draw3DDraw(plMk);
   return plMk;
}

//___________________________________________________
TObject *Draw3DPoint(float x, float y, float z, Color_t col=colorDefault,Style_t sty=styDefault,Size_t siz=sizDefault)
{
   if (!___draw_3d_init) Draw3DInit();
   TPolyMarker3D *plMk  = new TPolyMarker3D();
   plMk->SetNextPoint(x,y,z);
   if (col != colorDefault) plMk->SetMarkerColor(col);
   if (sty != styDefault)   plMk->SetMarkerStyle(sty);
   if (siz != sizDefault)   plMk->SetMarkerSize(siz);
   Draw3DDraw(plMk);
   return plMk;
}

//___________________________________________________
TObject *Draw3D(int n,  const float *xyz)
{
    if (!___draw_3d_init) Draw3DInit();
    return Draw3DPoints(n,xyz,kBlue,5);
}


//___________________________________________________
TObject *Draw3DLine(int n,  const float *xyz, Color_t col=colorDefault,Style_t sty=styDefault,Size_t siz=sizDefault)
{
   if (!___draw_3d_init) Draw3DInit();
   TPolyLine3D *plLine  = new TPolyLine3D(n,(Float_t*)xyz);
   if (col != colorDefault) plLine->SetLineColor(col);
   if (sty != styDefault)   plLine->SetLineStyle(sty);
   if (siz != sizDefault)   plLine->SetLineWidth(Width_t(siz));
   Draw3DDraw(plLine);
   return plLine;
}

//___________________________________________________
void Draw3D(){
   if (!___draw_3d_init) Draw3DInit();
   float xyz[] = { 189.195,       27.951,       123.966
                 ,187.195,       28.6187,       122.89
                 ,181.195       ,30.6788       ,119.556
                 ,179.195       ,31.3387       ,118.454
                 ,177.195       ,32.0065       ,117.328
                 ,175.195       ,32.6132       ,116.26
                 ,173.195       ,33.2385       ,115.146
                 ,171.195       ,33.8552       ,114.016
                 ,169.195       ,34.3924       ,112.964
         };

   int sizeXYZ = sizeof(xyz)/sizeof(float)/3;
   const float *fff = (const float *)&xyz[0];
   fprintf(stderr," %d %p\n", sizeXYZ,fff);
   Draw3D(sizeXYZ,fff);
   Draw3DLine(sizeXYZ,fff);
}

//___________________________________________________
TObject *Draw3D(const StTrack &track, Color_t col=colorDefault,Style_t sty=styDefault,Size_t siz=sizDefault)
{
   StTrackPoints trPnt(&track);
   return Draw3DLine(trPnt.GetN(),trPnt.GetP(),col,sty,siz);
}

//___________________________________________________
TObject *Draw3D(const StMeasuredPoint &hit, Color_t col=colorDefault,Style_t sty=styDefault,Size_t siz=sizDefault)
{
   // Draw the StMeasuredPoint, StHit, StVertex with the graphical attribute provided
   const StThreeVectorF& position = hit.position();
   return Draw3DPoint(position.x(),position.y(),position.z(),col,sty,siz);
}
//___________________________________________________
TObject *Draw3DInnOut(const StTrack &track, Bool_t in, Color_t col=colorDefault,Style_t sty=styDefault,Size_t siz=sizDefault)
{
   StInnOutPoints trInOut(&track,in);
   return Draw3DPoints(trInOut.GetN(),trInOut.GetP(),col,sty,siz);
}
