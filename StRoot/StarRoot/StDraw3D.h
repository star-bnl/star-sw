#ifndef STAR_StDraw3D
#define STAR_StDraw3D

#include "TObject.h"
#include "Gtypes.h"
#include <map>
  //
  // class StDraw3D - to draw the 3D primitives like 3D points and 3D lines
  // decoratated with the STAR detector geometry
  //
class TVirtualPad;

enum EDraw3DStyle {kVtx,kPrimaryTrack,kGlobalTrack,kUsedHit,kUnusedHit,kUser};

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
 //    operator Color_t ()  const { return Col();}
 //    operator Style_t ()  const { return Sty();}
 //    operator Size_t  ()  const { return Siz();}
};

class StDraw3D : public TObject
{
   private:
       static int fgDraw_3d_init;
       std::map<EDraw3DStyle,StDraw3DStyle> fStyles;
       TVirtualPad *fPad;
       Color_t  fBkColor; // background color
       static Color_t fgColorDefault;
       static Style_t fgStyDefault;
       static Size_t  fgSizDefault;
       static Color_t fgBkColor;

public:
   StDraw3D(TVirtualPad *pad = 0);
   virtual ~StDraw3D(){;}
   static int Draw3DInit();
   virtual const StDraw3DStyle &AddStyle(EDraw3DStyle type,Color_t col,Style_t sty,Size_t siz);

   virtual TObject *Draw(TObject *o);
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

    void Draw3DTest();

    ClassDef(StDraw3D,0);
};
#endif
