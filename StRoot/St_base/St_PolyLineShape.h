#ifndef STAR_St_PolyLineShape
#define STAR_St_PolyLineShape

#include <TShape.h>
#include <TAttMarker.h>

class TPoints3DABC;
class TVirtualPad;

enum EShapeTypes { kNULL=0, kSphere, kBrik};

class St_PolyLineShape : public TShape, public TAttMarker {
 protected:
   Bool_t        m_PointFlag;   // Flag whether we should paint "points" (option "P")
   Bool_t        m_LineFlag;    // Flag whether we should connect the points with "line" (option "L")
   EShapeTypes   m_ShapeType;   // shape of the segment connections
   TShape       *m_Shape;       // shape for draw each segment of the polylins
   TShape       *m_Connection;  // shape to represent the each "end" of the polyline
   TPoints3DABC   *m_Points;    // PolyLine itself
   Float_t       m_WidthFactor; // factor to calculate the the tube diameters 
   Bool_t        m_HasDrawn;    // flag to avoid multiply plots
   Bool_t        m_Smooth;      // Make smooth connections
   Size3D       *m_SizeX3D;     //! the X3D buffer sizes


protected:
   virtual void  Create();
   virtual Size3D *CreateX3DSize(Bool_t marker=kFALSE);
   virtual void  SetConnection(TShape *connection){ m_Connection = connection;}
   virtual Int_t PointDistancetoPrimitive(Int_t px, Int_t py);

public:
   St_PolyLineShape();
   St_PolyLineShape(TPoints3DABC *points,Option_t* option="P");
   virtual ~St_PolyLineShape();
   virtual Int_t        DistancetoPrimitive(Int_t px, Int_t py);
   virtual void         Draw(Option_t *opt="");
   virtual void         ExecuteEvent(Int_t event, Int_t px, Int_t py);
   virtual TShape      *GetConnection() const { return m_Connection;}
   virtual Color_t      GetColorAttribute() const;
   virtual Width_t      GetSizeAttribute()  const;
   virtual Style_t      GetStyleAttribute() const;
   virtual TPoints3DABC *GetMarker() const { return m_Points;}
   virtual TPoints3DABC *GetPoints() const { return m_Points;}
   virtual TShape      *GetShape() const { return m_Shape;}
   virtual Bool_t       GetSmooth() const { return m_Smooth;}
   virtual Float_t      GetWidthFactor() const { return m_WidthFactor;}
   virtual void         PaintNode(Float_t *start,Float_t *end,Option_t *option);
   virtual void         Paint(Option_t *opt);
   virtual void         Paint3d(Option_t *opt);
   virtual void         PaintX3DLine(Option_t *opt="");
   virtual void         PaintX3DMarker(Option_t *opt="");
   static Double_t     *gyrot(Double_t *dirc, Double_t cosang,Double_t sinang, Double_t trans[3][3]);
   virtual void         PaintPoints(Int_t n, Float_t *p=0, Option_t *opt="");
   virtual void         PaintPolyMarker(Int_t n, Float_t *p=0, Marker_t m=0, Option_t *opt="");
   static Float_t       Product(Float_t *v1, Float_t *v2,Int_t ndim=3);
   static Double_t      Product(Double_t *v1, Double_t *v2,Int_t ndim=3);
   virtual Color_t      SetColorAttribute(Color_t color);
   virtual Width_t      SetSizeAttribute(Width_t size);
   virtual Int_t        SetConnection(EShapeTypes connection=kBrik);
   virtual void         SetShape(TShape *shape);
   virtual void         SetSmooth(Bool_t smooth=kTRUE){ m_Smooth=smooth;}
   virtual Style_t      SetStyleAttribute(Style_t style);
   virtual void         SetWidthFactor(Float_t fact=1.0){m_WidthFactor = fact;} //*MENU 
   virtual Int_t        Size() const;
   virtual void         Sizeof3D() const;
   ClassDef(St_PolyLineShape,0)
};

#endif
