#ifndef STAR_St_PolyLineShape
#define STAR_St_PolyLineShape

#include <TShape.h>

//#define MyInputPoints TPolyLine3D
//#define MyInputPoints St_PolyLine3D

class TPoints3DABC;

// class St_PolyLine3D;
// class TPolyMarker3D;
// typedef St_PolyLine3D  MyInputPoints;

class TVirtualPad;

enum EShapeTypes { kNULL=0, kSphere, kBrik};

class St_PolyLineShape : public TShape  {
 protected:
   EShapeTypes   m_ShapeType;   // shape of the segment connections
   TShape       *m_Shape;       // shape for draw each segment of the polylins
   TShape       *m_Connection;  // shep to represent the each "end" of the polyline
//   MyInputPoints  *m_Points;        // PolyLine itself
//   St_PolyLine3D *m_Points;        // PolyLine itself
   TPoints3DABC   *m_Points;        // PolyLine itself
   Float_t       m_WidthFactor; // factor to calculate the the tube diameters 
   Bool_t        m_HasDrawn;    // flag to avoid multiply plots
   Bool_t        m_Smooth;      // Make smooth connections


protected:
   virtual void  Create();
   virtual void  SetConnection(TShape *connection){ m_Connection = connection;}

public:
   St_PolyLineShape();
   St_PolyLineShape(TPoints3DABC *points,Option_t* option="");
   virtual ~St_PolyLineShape();
   static  void Axis(TVirtualPad *p=0, Float_t width=0.5);
   virtual Int_t        DistancetoPrimitive(Int_t px, Int_t py);
   virtual void         Draw(Option_t *opt="");
   virtual void         ExecuteEvent(Int_t event, Int_t px, Int_t py);
   virtual TShape      *GetConnection(){ return m_Connection;}
//   virtual MyInputPoints *GetLine(){ return m_Points;}
   virtual TPoints3DABC *GetMarker(){ return m_Points;}
   virtual TPoints3DABC *GetPoints(){ return m_Points;}
   virtual TShape      *GetShape(){ return m_Shape;}
   virtual Bool_t       GetSmooth(){ return m_Smooth;}
   virtual Float_t      GetWidthFactor(){ return m_WidthFactor;}
   virtual void         PaintNode(Float_t *start,Float_t *end,Option_t *option);
   virtual void         Paint(Option_t *opt);
   virtual void         Paint3d(Option_t *opt);
   static Double_t     *gyrot(Double_t *dirc, Double_t cosang,Double_t sinang, Double_t trans[3][3]);
   static Float_t       Product(Float_t *v1, Float_t *v2,Int_t ndim=3);
   static Double_t      Product(Double_t *v1, Double_t *v2,Int_t ndim=3);
   virtual Int_t        SetConnection(EShapeTypes connection=kBrik);
   virtual void         SetShape(TShape *shape);
   virtual void         SetSmooth(Bool_t smooth=kTRUE){ m_Smooth=smooth;}
   virtual void         SetWidthFactor(Float_t fact=1.0){m_WidthFactor = fact;} //*MENU 
   virtual void         Sizeof3D() const;
   ClassDef(St_PolyLineShape,0)
};

#endif
