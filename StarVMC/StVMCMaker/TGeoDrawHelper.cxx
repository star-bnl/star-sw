// Author: Valeri Fine   19/01/2004
/****************************************************************************
** $Id: TGeoDrawHelper.cxx,v 1.4 2015/08/04 21:03:36 jwebb Exp $
**
** Copyright (C) 2004 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.      
**
*****************************************************************************/

#include "TGeoDrawHelper.h"
#include "THashList.h"
#include "TGeometry.h"
#include "TMap.h"
#include "TRotMatrix.h"

#include "TGeoVolume.h"
#include "TVolume.h"

#include "TGeoNode.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"

#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoPara.h"
#include "TGeoTube.h"
#include "TGeoCone.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoEltu.h"
#include "TGeoArb8.h"
#include "TGeoTrd1.h"
#include "TGeoTrd2.h"
#include "TGeoSphere.h"

#include "TBRIK.h"
#include "TPARA.h"
#include "TTUBE.h"
#include "TCTUB.h"
#include "TTUBS.h"
#include "TCONE.h"
#include "TCONS.h"
#include "TPCON.h"
#include "TPGON.h"
#include "TELTU.h"
#include "TTRD1.h"
#include "TTRD2.h"
#include "TTRAP.h"
#include "TSPHE.h"
#include "TGTRA.h"

//_____________________________________________________________________________
static inline Bool_t CompareMatrix(const double *pa,const TRotMatrix &b)
{  double *pb=((TRotMatrix *)&b)->GetMatrix();
   for (int i=0; i<9; i++)  if (pa[i]!=pb[i]) return kFALSE;
   return kTRUE;
}
//_____________________________________________________________________________
static inline TRotMatrix *GetMatrix(const double *rotation)
{  
   THashList *list = gGeometry->GetListOfMatrices();   
   TIter nextmatrix(list);
   TRotMatrix *matrix=0;
   // no optimization !!! while ((matrix=(TRotMatrix *) nextmatrix()) && !CompareMatrix(rotation,*matrix) ) {}   
   if (!matrix) {
      char mname[20];
      int n=list->GetSize(); sprintf(mname,"matrix%d",n+1);
      //  we have to transponce the matrix 
      Double_t mx[9];
      mx[0] = rotation[0]; mx[1] = rotation[3]; mx[2] = rotation[6];
      mx[3] = rotation[1]; mx[4] = rotation[4]; mx[5] = rotation[7];
      mx[6] = rotation[2]; mx[7] = rotation[5]; mx[8] = rotation[8];
      matrix = new TRotMatrix(mname,mname,mx); 
   }
   return matrix;
}

//______________________________________________________________________________
//
//  Missing class TARB8 
//______________________________________________________________________________
class TARB8 : public TBRIK {

protected:
   Double_t              fXY[8][2];    // list of vertices

public:
   TARB8(){;}
   TARB8(const char *name, const char *title, const char *material,  Double_t dz, Double_t *vertices=0)
   : TBRIK(name, title,material,0,0,dz) { if (vertices) memcpy(fXY,vertices,sizeof(fXY)); }
   virtual ~TARB8(){;}
#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,3)  
   virtual void Paint(Option_t *){} 
   virtual void    SetPoints(Double_t *buff) {

      //*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*Create ARB8 points*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      //*-*                            ==================

      Float_t  dz;

      dz  = TBRIK::fDz;
      static const int x = 0;
      static const int y = 1;
      int i = 0;
      if (buff) {
         buff[ 0] = fXY[i][x];  buff[ 1] = fXY[i++][y];  buff[ 2] = -dz;
         buff[ 3] = fXY[i][x];  buff[ 4] = fXY[i++][y];  buff[ 5] = -dz;
         buff[ 6] = fXY[i][x];  buff[ 7] = fXY[i++][y];  buff[ 8] = -dz;
         buff[ 9] = fXY[i][x];  buff[10] = fXY[i++][y];  buff[11] = -dz;

         buff[12] = fXY[i][x];  buff[13] = fXY[i++][y];  buff[14] =  dz;
         buff[15] = fXY[i][x];  buff[16] = fXY[i++][y];  buff[17] =  dz;
         buff[18] = fXY[i][x];  buff[19] = fXY[i++][y];  buff[20] =  dz;
         buff[21] = fXY[i][x];  buff[22] = fXY[i++][y];  buff[23] =  dz;
      }
   }
#endif    
   virtual void    SetPoints(Double_t *buff) const {

      //*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*Create ARB8 points*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      //*-*                            ==================

      Float_t  dz;

      dz  = TBRIK::fDz;
      static const int x = 0;
      static const int y = 1;
      int i = 0;
      if (buff) {
         buff[ 0] = fXY[i][x];  buff[ 1] = fXY[i++][y];  buff[ 2] = -dz;
         buff[ 3] = fXY[i][x];  buff[ 4] = fXY[i++][y];  buff[ 5] = -dz;
         buff[ 6] = fXY[i][x];  buff[ 7] = fXY[i++][y];  buff[ 8] = -dz;
         buff[ 9] = fXY[i][x];  buff[10] = fXY[i++][y];  buff[11] = -dz;

         buff[12] = fXY[i][x];  buff[13] = fXY[i++][y];  buff[14] =  dz;
         buff[15] = fXY[i][x];  buff[16] = fXY[i++][y];  buff[17] =  dz;
         buff[18] = fXY[i][x];  buff[19] = fXY[i++][y];  buff[20] =  dz;
         buff[21] = fXY[i][x];  buff[22] = fXY[i++][y];  buff[23] =  dz;
      }
   }
};
//______________________________________________________________________________


// ClassImp(TGeoDrawHelper)

//______________________________________________________________________________
TGeoDrawHelper::TGeoDrawHelper(TGeoVolume *geoVolume) 
{
   // Create the TVolume reprersentation of given TGeoVolume
   // if geoVolume = 0 (default) use gGeoManager->GetTopVolume instead.
   fGeoVolume = geoVolume;
   if (gGeoManager) {
      if (!fGeoVolume) fGeoVolume = gGeoManager->GetTopVolume();
      fVolume = MakeVolume(fGeoVolume);
   }
}
//______________________________________________________________________________
TGeoDrawHelper::~TGeoDrawHelper()
{ }

//______________________________________________________________________________
Int_t TGeoDrawHelper::DistancetoPrimitive(Int_t px, Int_t py)
{ return  fVolume ? fVolume->DistancetoPrimitive(px, py) : 999999; }
//______________________________________________________________________________
void TGeoDrawHelper::Draw(Option_t *option)
{ 
   if (fVolume)  {
      fVolume->SetBit(kCanDelete);
      fVolume->Draw(option);
   }
}
//______________________________________________________________________________
const char *TGeoDrawHelper::GetName() const
{ 
   return fVolume ? fVolume->GetName():GetName(); 
}
//______________________________________________________________________________
const char *TGeoDrawHelper::GetIconName() const
{ 
   return fVolume ? fVolume->GetIconName(): GetIconName(); 
}
//______________________________________________________________________________
void  TGeoDrawHelper::Paint(Option_t *option)
{ 
  if (fVolume) fVolume->Paint(option); 
}

//______________________________________________________________________________
TVolume *TGeoDrawHelper::MakeVolume( TGeoVolume *top, std::map<TGeoVolume *,TVolume *> *volumeMap) {
   static int depth = 0;
   std::map<TGeoVolume *,TVolume *> *toFlag = volumeMap;
   if (!toFlag) depth = 0;
   TVolume *topVolume = 0; 
   TObjArray *nodes = 0;
   if (top )  {
       // if (!top->IsVolumeMulti()) 
      TShape *thisShape = MakeShape(top->GetShape());
      TString title = top->GetName();
      TString name = TString(title, 4);
      topVolume = new TVolume(name,title,thisShape); 
      topVolume->SetLineColor(top->GetLineColor()); topVolume->SetLineStyle(top->GetLineStyle());
      topVolume->SetLineWidth(top->GetLineWidth()); topVolume->SetFillColor(top->GetLineColor());

      if (!top->IsVisible()) topVolume->SetVisibility(TVolume::kThisUnvisible);
      if ( (nodes = top->GetNodes()) ) {
         TIter next(nodes);
         TGeoNode *geoNode;
         TGeoVolume *geoVolume = 0;
         Int_t nNodes = top->GetNdaughters();
         Int_t i = 0;
         for (i = 0; (i < nNodes)  ; i++) {
            geoNode   = top->GetNode(i);
            geoVolume = geoNode ? geoNode->GetVolume(): 0;
            if (geoVolume) {
               TVolume *nextVolume = 0;
               if  (!volumeMap) {
                  volumeMap = new std::map<TGeoVolume *,TVolume *>; // (1000);
                  depth = 1;
               }  else  {
                  std::map <TGeoVolume *,TVolume *> :: const_iterator volumeFinder;
                  volumeFinder = volumeMap->find(geoVolume);
                  if (volumeFinder != volumeMap->end( ) ) 
                     nextVolume = volumeFinder->second;
               }
               depth++;
               if (!nextVolume) {
                  typedef std::pair <TGeoVolume *, TVolume *> VOLUME_PAIR;
                  gGeoManager->CdDown(i);
                  nextVolume= MakeVolume(geoVolume,volumeMap);
                  gGeoManager->CdUp();
                  volumeMap->insert(VOLUME_PAIR(geoVolume,nextVolume));
               }
               --depth;
               TGeoMatrix *geoMatrix       = geoNode->GetMatrix();
               const Double_t   *trans     = geoMatrix->GetTranslation();
               const Double_t   *rotation  = geoMatrix->GetRotationMatrix();
               TRotMatrix *matrix          = geoMatrix->IsIdentity() ? 0 : GetMatrix(rotation);
               topVolume->Add(nextVolume, trans[0],trans[1],trans[2],matrix, geoNode->GetNumber());
   }  }  }  }
   if (!toFlag)  delete volumeMap;
   return topVolume;
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoBBox &shp) {
   return
      new TBRIK(shp.GetName()
         ,"BRIK","void",shp.GetDX(),shp.GetDY(),shp.GetDZ());
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoPara &shp) {
   return
      new TPARA(shp.GetName(),"PARA","void",
            shp.GetX(),shp.GetY(),shp.GetZ()
           ,shp.GetAlpha(),shp.GetTheta(),shp.GetPhi());  
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoTube &shp) {
   return
      new TTUBE(shp.GetName(),"TUBE","void",
               shp.GetRmin(),shp.GetRmax(),shp.GetDz());   
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoTubeSeg &shp) {
   return
      new TTUBS(shp.GetName(),"TUBS","void",
           shp.GetRmin(),shp.GetRmax(),shp.GetDz(),shp.GetPhi1(),shp.GetPhi2());     
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoCone &shp) {
   return
      new TCONE(shp.GetName(),"CONE","void",
           shp.GetDz(),shp.GetRmin1(),shp.GetRmax1(),shp.GetRmin2(),shp.GetRmax2());               
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoConeSeg &shp) {
   return
      new TCONS(shp.GetName(),"CONE","void",
            shp.GetDz()
           ,shp.GetRmin1(),shp.GetRmax1()
           ,shp.GetRmin2(),shp.GetRmax2()
           ,shp.GetPhi1 (),shp.GetPhi2 () );               
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoPcon &shp) {
   TPCON *t = 
      new TPCON(shp.GetName(),"PCON","void"
          ,shp.GetPhi1(),shp.GetDphi(),shp.GetNz());
      for (Int_t i=0; i<shp.GetNz(); i++) 
         t->DefineSection(i,shp.GetZ(i),shp.GetRmin(i),shp.GetRmax(i));
      return t;
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoPgon &shp) {
   TPGON *t = 
      new TPGON(shp.GetName(),"PGON","void"
          ,shp.GetPhi1(),shp.GetDphi(),shp.GetNedges(),shp.GetNz());
      for (Int_t i=0; i<shp.GetNz(); i++) 
         t->DefineSection(i,shp.GetZ(i),shp.GetRmin(i),shp.GetRmax(i));
      return t;
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoEltu &shp) {
   return
      new TELTU(shp.GetName(),"ELTU","void",
           shp.GetA(),shp.GetB(),shp.GetDz());  
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoTrap &shp) {
   return
      new TTRAP(shp.GetName(),"TRAP","void",
         shp.GetDz(),shp.GetTheta(),shp.GetPhi()
         ,shp.GetH1(),shp.GetBl1(),shp.GetTl1(),shp.GetAlpha1()
         ,shp.GetH2(),shp.GetBl2(),shp.GetTl2(),shp.GetAlpha2() );  
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoCtub &shp) {
   return
      new TCTUB(shp.GetName(),"CTUB","void",
         shp.GetRmin(),shp.GetRmax(),shp.GetDz(),shp.GetPhi1(),shp.GetPhi2()
        ,shp.GetNlow()[0], shp.GetNlow()[1], shp.GetNlow()[2]
        ,shp.GetNhigh()[0],shp.GetNhigh()[1],shp.GetNhigh()[2] );
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoTrd1 &shp) {
   return
      new TTRD1(shp.GetName(),"TRD1","void",
         shp.GetDx1(),shp.GetDx2(),shp.GetDy(),shp.GetDz());   
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoTrd2 &shp) {
   return
      new TTRD2(shp.GetName(),"TRD2","void",
         shp.GetDx1(),shp.GetDx2(),shp.GetDy1(),shp.GetDy2(),shp.GetDz());   
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoSphere &shp) {
   return
      new TSPHE(shp.GetName(),"TSPHE","void",
         shp.GetRmin(),shp.GetRmax()
        ,shp.GetTheta1(),shp.GetTheta2()
        ,shp.GetPhi1(),shp.GetPhi2());   
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoGtra &shp) {
   return
      new TGTRA(shp.GetName(),"TGTRA","void",
          shp.GetDz(),shp.GetTheta(),shp.GetPhi()
         ,shp.GetTwistAngle()
         ,shp.GetH1(),shp.GetBl1(),shp.GetTl1(),shp.GetAlpha1()
         ,shp.GetH2(),shp.GetBl2(),shp.GetTl2(),shp.GetAlpha2() );  
}

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoArb8 &shp) {
   return
      new TARB8(shp.GetName(),"TARB8","void",
          shp.GetDz(),((TGeoArb8 &)shp).GetVertices() );  
}

//________________________________________________________________
//
//________________________________________________________________
#define GEOSHAPE(shapeCode) \
   if ( shape->IsA() == (_NAME2_(T,shapeCode)::Class() ) ) { t=MakeShape(*(_NAME2_(T,shapeCode *))shape); }


// #define GEOSHAPE(shapeCode) if (shape->TestShapeBit(_NAME2_(TGeoShape::k,shapeCode))) { t=MakeShape(*(_NAME2_(T,shapeCode *))shape); }
//   fprintf(stderr," \n + shape %s \n",shape->GetName()); }
//  case    : t=MakeShape(*(_NAME2_(T,shapeCode *))shape); fprintf(stderr," \n + shape %s \n",shape->GetName()); break         

//________________________________________________________________
TShape *TGeoDrawHelper::MakeShape(const TGeoShape *shape) {
   TShape *t = 0;
   if (shape) {


      if ( shape->TestShapeBit(TGeoShape::kGeoBad           ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoBad \n",shape->GetName()); }          
      if ( shape->TestShapeBit(TGeoShape::kGeoRSeg          ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoRSeg \n",shape->GetName()); }         
      if ( shape->TestShapeBit(TGeoShape::kGeoPhiSeg        ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoPhiSeg\n",shape->GetName()); }        
      if ( shape->TestShapeBit(TGeoShape::kGeoThetaSeg      ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoThetaSeg \n",shape->GetName()); }      
      if ( shape->TestShapeBit(TGeoShape::kGeoVisX          ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoVisX \n",shape->GetName()); }    
      if ( shape->TestShapeBit(TGeoShape::kGeoVisY          ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoVisY \n",shape->GetName()); }    
      if ( shape->TestShapeBit(TGeoShape::kGeoVisZ          ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoVisZ\n",shape->GetName()); }    
      if ( shape->TestShapeBit(TGeoShape::kGeoRunTimeShape  ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoRunTimeShape\n",shape->GetName()); }
      if ( shape->TestShapeBit(TGeoShape::kGeoInvalidShape  ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoInvalidShape\n",shape->GetName()); }
      if ( shape->TestShapeBit(TGeoShape::kGeoTorus         ) ) { printf(" This shape has not been implemented yet %s TGeoShape::kGeoTorus\n",shape->GetName()); }

      if ( shape->TestShapeBit(TGeoShape::kGeoBox           ) ) { t=MakeShape(*(TGeoBBox *)shape); }
      GEOSHAPE(GeoPara);
//      if ( shape->TestShapeBit(TGeoShape::kGeoSph           ) ) { t=MakeShape(*(TGeoSphere *)shape); }
      GEOSHAPE(GeoSphere);
      GEOSHAPE(GeoTube);
      GEOSHAPE(GeoTubeSeg);
      GEOSHAPE(GeoCone);
      GEOSHAPE(GeoConeSeg);
      GEOSHAPE(GeoPcon);
      GEOSHAPE(GeoPgon);

      // if (shape->TestShapeBit(TGeoShape::kGeoArb8           ) ) { printf(" This shape has not been implemented yet %s\n",shape->GetName()); }
      GEOSHAPE(GeoArb8);
      GEOSHAPE(GeoEltu);
      GEOSHAPE(GeoTrap);
      GEOSHAPE(GeoCtub);
      GEOSHAPE(GeoTrd1);
      GEOSHAPE(GeoTrd2);

      if ( shape->TestShapeBit(TGeoShape::kGeoComb          ) ) { printf(" This shape has not been implemented yet %s\n",shape->GetName()); }
      // if ( shape->TestShapeBit(TGeoShape::kGeoClosedShape   ) ) { printf(" This shape has not been implemented yet %s\n",shape->GetName()); }
      GEOSHAPE(GeoGtra);


      ////      case HYPE: t=new THYPE(nick,"HYPE","void",
      ////                       shp.Get0],shp.Get1],shp.Get2],shp.Get3]);                    break;
      //        case GTRA: t=new TGTRA(nick,"GTRA","void",
      //                         shp.Get0],shp.Get1],shp.Get2],shp.Get3],shp.Get4],shp.Get5],
      //                         shp.Get6],shp.Get7],shp.Get8],shp.Get9],shp.Get10],shp.Get11]);        break;
      //        default:   t=new TBRIK(nick,"BRIK","void",
      //                         shp.Get0],shp.Get1],shp.Get2]);                         break;
   }
   if (!t) t= new TBRIK("DEFAULT","BRIK","void",10,10,10);
   return t;
}
