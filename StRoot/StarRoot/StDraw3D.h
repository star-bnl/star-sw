#ifndef STAR_StDraw3D
#define STAR_StDraw3D
// $Id: StDraw3D.h,v 1.20 2008/05/15 22:11:17 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008

#include "TObject.h"
#include "Gtypes.h"
#include "TString.h"
#include <map>

  //
  //  Class StDraw3D - to draw the 3D primitives like 3D points and 3D lines
  //  decoratated with the STAR detector geometry
  //
  //  It provides the simple way to visualize the event 
  //  primitives in 3D quickly against of the STAR detector 
  //  geometry.
  //
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DClass.png">end_html
  
class TVirtualPad;
class TVirtualViewer3D;

enum EDraw3DStyle {kVtx,kPrimaryTrack,kGlobalTrack,kUsedHit,kUnusedHit,kTrackBegin,kTrackEnd,kUser};

class StDraw3DStyle {
   private:
       EDraw3DStyle  fType;
       Color_t fColor;
       Style_t fSty;
       Size_t  fSiz;
   public:
     StDraw3DStyle(EDraw3DStyle typ=kUsedHit
          ,  Color_t col = Color_t(-1)
          ,  Style_t sty = Style_t(-1)
          ,  Size_t  siz = Size_t (-1))
      : fType(typ),fColor(col),fSty(sty),fSiz(siz) {}
     Color_t      Col()  const { return fColor;}
     Style_t      Sty()  const { return fSty;  }
     Size_t       Siz()  const { return fSiz;  }
     EDraw3DStyle Type() const { return fType; }
     Color_t      &Col()  { return fColor;}
     Style_t      &Sty()  { return fSty;  }
     Size_t       &Siz()  { return fSiz;  }
     EDraw3DStyle &Type() { return fType; }
     void         SetCol(Color_t col) { Col() = col;}
     void         SetSty(Style_t sty) { Sty() = sty;}
     void         SetSiz(Size_t  siz) { Siz() = siz;}
     void         SetType(Color_t col, Style_t sty, Size_t  siz)
                  { SetCol(col); SetSiz(siz); SetSty(sty);  }
     
 //    operator Color_t ()  const { return Col();}
 //    operator Style_t ()  const { return Sty();}
 //    operator Size_t  ()  const { return Siz();}
};


class view_3D;

class StDraw3D : public TObject
{
   private:
       static int fgDraw_3d_init;
       std::map<EDraw3DStyle,StDraw3DStyle> fStyles;
       TVirtualPad *fPad;
       Color_t  fBkColor; // background color
       TVirtualViewer3D *fViewer; 
       view_3D *fView;
       TString fDetectorName;
       StDraw3D *fMaster;

       static Color_t fgColorDefault;
       static Style_t fgStyDefault;
       static Size_t  fgSizDefault;
       static Color_t fgBkColor;
       static Int_t   fDrawCanvasCounter;
       TVirtualPad *InitPad();
       void SetMaster(StDraw3D *master);
       void InitViewer();
       void Redraw();

public:
   StDraw3D(TVirtualPad *pad = 0, const char *detectorName="TPC");
   virtual ~StDraw3D();
   virtual const StDraw3DStyle &AddStyle(EDraw3DStyle type,Color_t col,Style_t sty,Size_t siz);
   TVirtualPad *Pad() const;
   TVirtualViewer3D *Viewer() const;
   virtual void  Clear(Option_t *opt="");
   virtual TObject *Draw(TObject *o);
   virtual const TString &DetectorNames() const;
   virtual void  SetDetectors(const char*nameDetectors);
   virtual void  AddDetectors(const char*nameDetectors);
   virtual void  Draw(Option_t *option="") {TObject::Draw(option);}
   virtual const StDraw3DStyle &Style(EDraw3DStyle type);
   virtual void  SetBkColor(Color_t newBkColor);

   virtual TObject *Draw3D(int n,  const float *xyz);

   virtual TObject *Points(int n, const float *xyz
         ,  EDraw3DStyle sty);

   virtual TObject *Points(int n, const float *xyz
         ,  Color_t col= Color_t(-1)
         ,  Style_t sty= Style_t(-1)
         ,  Size_t siz = Size_t (-1));

   virtual TObject *Point(float x, float y, float z
         ,  Color_t col= Color_t(-1)
         ,  Style_t sty= Style_t(-1)
         ,  Size_t siz = Size_t (-1));

   virtual TObject *Point(float x, float y, float z
         ,  EDraw3DStyle sty);

   virtual TObject *Line(int n,  const float *xyz
         ,  Color_t col= Color_t(-1)
         ,  Style_t sty= Style_t(-1)
         ,  Size_t siz = Size_t (-1));

   virtual TObject *Line(int n,  const float *xyz
         ,  EDraw3DStyle sty);
   virtual void Joint(StDraw3D *dsp);
   virtual void SetModel(TObject *model);
   virtual void SetComment(const char *cmnt);
   virtual void AddComment(const char *cmnt);
   virtual void Update();
   virtual void Modified();

    void Draw3DTest();

    ClassDef(StDraw3D,0);
};
#endif
