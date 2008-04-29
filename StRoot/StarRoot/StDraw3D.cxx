#  include "StDraw3D.h"
#  include "TVirtualPad.h"
#  include "TPolyMarker3D.h"
#  include "TPolyLine3D.h"
#  include "TSystem.h"
#  include "TROOT.h"
#  include "TEnv.h"
#  include "StCheckQtEnv.h"
#  include "TStyle.h"

int StDraw3D::fgDraw_3d_init = 0;

static Color_t colorDefault = Color_t(-1);
static Style_t styDefault   = Style_t(-1);
static Size_t  sizDefault   = Size_t (-1);

Color_t StDraw3D::fgColorDefault = Color_t(-1);
Style_t StDraw3D::fgStyDefault   = Style_t(-1);
Size_t  StDraw3D::fgSizDefault   = Size_t (-1);
Color_t StDraw3D::fgBkColor      = kBlack;
//___________________________________________________
StDraw3D::StDraw3D(TVirtualPad *pad): fPad(pad)
{
   static const Style_t UHitSty = 4; static const Size_t UHitSiz = 0.35; static const Color_t UHitCol=kBlue;
   static const Style_t NHitSty = 1; static const Size_t NHitSiz = 1.00; static const Color_t NHitCol=kGreen;
   static const Style_t TrakSty = 1; static const Size_t TrakSiz = 1.00; static const Color_t TrakCol=kRed;
   static const Style_t VertSty = 5; static const Size_t VertSiz = 3.50; static const Color_t VertCol=kYellow;
   AddStyle(kVtx,         VertCol,VertSty,VertSiz);
   AddStyle(kPrimaryTrack,TrakCol,TrakSty,TrakSiz);
   AddStyle(kGlobalTrack, TrakCol,TrakSty,TrakSiz);
   AddStyle(kUsedHit,     UHitCol,UHitSty,UHitSiz);
   AddStyle(kUnusedHit,   NHitCol,NHitSty,NHitSiz);
}

//___________________________________________________
TObject *StDraw3D::Draw(TObject *o)
{
   // Draw the 3d object 
   // and set the new background color if needed
   if (o) {
      o->Draw();
      if (gPad->GetFillColor() != fBkColor)
      gPad->SetFillColor(fBkColor);
   }
   return o;
}
//___________________________________________________
void StDraw3D::SetBkColor(Color_t newBkColor)
{
   // Set the canvas background color;
   fBkColor = newBkColor;
}
//___________________________________________________
const StDraw3DStyle &StDraw3D::AddStyle(EDraw3DStyle type,Color_t col,Style_t sty,Size_t siz)
{
   fStyles.insert(std::pair<EDraw3DStyle,StDraw3DStyle>(type,StDraw3DStyle(type,col,sty,siz)));
   return Style(type);
}

//___________________________________________________
const StDraw3DStyle &StDraw3D::Style(EDraw3DStyle type)
{
    return fStyles[type];
}

//___________________________________________________
int StDraw3D::Draw3DInit(){
   // check the correct env and load the plugins
   if (fgDraw_3d_init) return 1;
   fgDraw_3d_init = 1;
   // gROOT->Macro("Load.C");
   if (!StCheckQtEnv::SetQtEnv(false)) {
      // define the background image
      const char *backShape = "$STAR/StRoot/macros/graphics/StarTPC.iv";
      printf(" Setting the background shape to be 	%s\n", backShape);
      gEnv->SetValue("Gui.InventorBackgroundShape",backShape);
   }
   return 1;
}
//___________________________________________________
TObject *StDraw3D::Points(int n, const float *xyz, Color_t col,Style_t sty,Size_t siz)
{
   if (!fgDraw_3d_init) Draw3DInit();
   TPolyMarker3D *plMk  = new TPolyMarker3D(n,(Float_t*)xyz);
   if (col != colorDefault) plMk->SetMarkerColor(col);
   if (sty != styDefault)   plMk->SetMarkerStyle(sty);
   if (siz != sizDefault)   plMk->SetMarkerSize(siz);
   return Draw(plMk);
}
//___________________________________________________
TObject *StDraw3D::Points(int n, const float *xyz, EDraw3DStyle sty)
{
  const StDraw3DStyle &style =  Style(sty);
  return Points(n, xyz, style.Col(),style.Sty(),style.Siz());
}
//___________________________________________________
TObject *StDraw3D::Point(float x, float y, float z, Color_t col,Style_t sty,Size_t siz)
{
   if (!fgDraw_3d_init) Draw3DInit();
   TPolyMarker3D *plMk  = new TPolyMarker3D();
   plMk->SetNextPoint(x,y,z);
   if (col != colorDefault) plMk->SetMarkerColor(col);
   if (sty != styDefault)   plMk->SetMarkerStyle(sty);
   if (siz != sizDefault)   plMk->SetMarkerSize(siz);
   return Draw(plMk);
}

//___________________________________________________
TObject *StDraw3D::Point(float x, float y, float z, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Point(x,y, z, style.Col(),style.Sty(),style.Siz());
}

//___________________________________________________
TObject *StDraw3D::Draw3D(int n,  const float *xyz)
{
   if (!fgDraw_3d_init) Draw3DInit();
   return Points(n,xyz,kVtx);
}

//___________________________________________________
TObject *StDraw3D::Line(int n,  const float *xyz, Color_t col,Style_t sty,Size_t siz)
{
   if (!fgDraw_3d_init) Draw3DInit();
   TPolyLine3D *plLine  = new TPolyLine3D(n,(Float_t*)xyz);
   if (col != colorDefault) plLine->SetLineColor(col);
   if (sty != styDefault)   plLine->SetLineStyle(sty);
   if (siz != sizDefault)   plLine->SetLineWidth(Width_t(siz));
   return Draw(plLine);
}

//___________________________________________________
TObject *StDraw3D::Line(int n,  const float *xyz,EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Line(n,xyz,  style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
void StDraw3D::Draw3DTest(){
   if (!fgDraw_3d_init) Draw3DInit();
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
   Line(sizeXYZ,fff,kPrimaryTrack);
}
#if 0
//___________________________________________________
TObject *Draw3D(const StTrack &track, Color_t col,Style_t sty,Size_t siz)
{
   StTrackPoints trPnt(&track);
   return Draw3DLine(trPnt.GetN(),trPnt.GetP(),col,sty,siz);
}

//___________________________________________________
TObject *Draw3D(const StMeasuredPoint &hit, Color_t col,Style_t sty,Size_t siz)
{
   // Draw the StMeasuredPoint, StHit, StVertex with the graphical attribute provided
   const StThreeVectorF& position = hit.position();
   return Draw3DPoint(position.x(),position.y(),position.z(),col,sty,siz);
}
//___________________________________________________
TObject *Draw3DInnOut(const StTrack &track, Bool_t in, Color_t col,Style_t styt,Size_t siz)
{
   StInnOutPoints trInOut(&track,in);
   return Draw3DPoints(trInOut.GetN(),trInOut.GetP(),col,sty,siz);
}
#endif
