// @(#)root/gtgl:$Name:  $:$Id: TBoundBoxEstimator.h,v 1.5 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      31/05/05

#ifndef ROOT_TBoundBoxEstimator
#define ROOT_TBoundBoxEstimator

/****************************************************************************
** TBoundBoxEstimator
**
** Copyright (C) 2005 by Valeri Fine. Brookhaven National Laboratory. All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TShape3DPolygonView.h"

class TBoundBoxEstimator {
protected:
   Coord3D  fMasterCurrentBoxMin;
   Coord3D  fMasterCurrentBoxMax;

   Coord3D  fLocalCurrentBoxMin;
   Coord3D  fLocalCurrentBoxMax;

   std::vector<TMatrixD> fCurrentTrasformation;
   std::vector<Coord3D> fCurrentTranslation;
   Bool_t   fMasterInit;
   Bool_t   fLocalInit;
public:
   static TMatrixD fgUnitMatrixD;
public:
   TBoundBoxEstimator():fMasterInit(kTRUE),fLocalInit(kTRUE) {;}

   inline void AdjustMaster() {
      for (int i=0;i<3;i++) {
         if ( fMasterInit || (fLocalCurrentBoxMax[i] > fMasterCurrentBoxMax[i]) )    
            fMasterCurrentBoxMax[i] = fLocalCurrentBoxMax[i];
         if ( fMasterInit || (fLocalCurrentBoxMin[i] < fMasterCurrentBoxMin[i]) )   
            fMasterCurrentBoxMin[i] = fLocalCurrentBoxMin[i];
      }
      if (fMasterInit) fMasterInit = kFALSE;
   }
   inline void AdjustLocal(Double_t *point) {
      for (int i=0;i<3;i++) {
         if ( fLocalInit || (point[i] > fLocalCurrentBoxMax[i]) )    
            fLocalCurrentBoxMax[i] = point[i];
         if ( fLocalInit || (point[i] < fLocalCurrentBoxMin[i]) )  
            fLocalCurrentBoxMin[i] = point[i];
      }
      if (fLocalInit) fLocalInit = kFALSE;
   }
   inline void Local2Master() 
   {
        // Convert the current local box in tot he master one in place
        Coord3D &translation = fCurrentTranslation.back();
        fLocalCurrentBoxMin +=   translation;
        fLocalCurrentBoxMax +=   translation;

        TMatrixD &currentMatrix = fCurrentTrasformation.back();
        fLocalCurrentBoxMin *= currentMatrix; 
        fLocalCurrentBoxMax *= currentMatrix; 

        fLocalInit = kTRUE;
   }
   const Coord3D &GetMaxBounds() const { return fMasterCurrentBoxMax;}
   const Coord3D &GetMinBounds() const { return fMasterCurrentBoxMin;}
   Int_t Push( const Double_t *traslation,const Double_t *rotation) 
   {
      if (!fLocalInit) {
         if (fCurrentTrasformation.size() > 0) Local2Master();
         AdjustMaster();
      }
      if (rotation)
         fCurrentTrasformation.push_back(TMatrixD(3,3,rotation));
      else {
         TMatrixD unit(3,3); unit.UnitMatrix();
         fCurrentTrasformation.push_back(unit);
      }
      fCurrentTranslation.push_back(Coord3D(traslation));
      return fCurrentTrasformation.size();
   }
   Int_t Pop() 
   { 
      if (fCurrentTrasformation.size() > 0) {
         fCurrentTrasformation.pop_back();
         fCurrentTranslation.pop_back();
      }
      return fCurrentTrasformation.size();
   }
};
#endif
