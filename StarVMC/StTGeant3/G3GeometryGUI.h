#ifndef G3GEOMETRYGUI_H
#define G3GEOMETRYGUI_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: G3GeometryGUI.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $ */


#include "TClonesArray.h"
#include "StTGeant3.h"

class G3GuiGeomMain;
class G3DrawVolume;
class TRotMatrix;

class G3GeometryGUI : public TObject {
 public:
    G3GeometryGUI(const char* opt = "");
    virtual ~G3GeometryGUI(){}
   private:
    G3GuiGeomMain *fPanel;      // the main gui panel
    Int_t          fNstack;      // number of volumes
    TClonesArray   *fVolumes;    // array of volumes  
    Int_t          fNMaterials;  // number of materials and media
    TClonesArray   *fMaterials;  // array of materials
    TClonesArray   *fMedia;      // array of materials
    TObjArray      *fRotations;  // Rotation Matrices
 private:
    G3GeometryGUI(const G3GeometryGUI& geo): TObject(geo) {}
    G3GeometryGUI & operator=(const G3GeometryGUI&) 
    {return *this;}
    
    ClassDef(G3GeometryGUI,1)  // Steering class for the G3 GUI
};



#endif
