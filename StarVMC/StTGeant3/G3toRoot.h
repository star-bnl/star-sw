#ifndef G3toRoot_H
#define G3toRoot_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: G3toRoot.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $ */

#include <TObject.h>
#include <StTGeant3.h>

class TClonesArray;
class TObjArray;
class TGPicture;
class TFolder;
class TGeometry;

class G3Volume;
class G3Node;


class G3toRoot : public TObject 
{
 public:
    G3toRoot();
    virtual ~G3toRoot(){}
    virtual void G32root();
    void ConvertToRootShapes(TFolder *item=0, G3Node** node=0,
			     Int_t nNodes=1);
    // Setters
    virtual void SetExpandDivisions(Int_t flag = 1)
	{fExpand = flag;}
    // Getters
    TFolder*      GetTopFolder() {return fTopFolder;}
    TClonesArray* GetMaterials() {return fMaterials;}    
    TClonesArray* GetMedia()     {return fMedia;}
    char*         TopName()      {return fTopName;}
 private:
    void ExpandDivisions(G3Node* node=0);
    void ReadGeometryTree();
    void ReadMaterials();    
    void ReadRotations();
    TFolder* AddVolume(TObject * obj, TFolder *parent, const char* name);
    virtual G3Volume* Volume(Int_t id);
    Int_t Medium(Int_t idvol);
    Int_t Material(Int_t idvol);
    Float_t Cut(Int_t imed, Int_t icut);
    // Return number of children for volume idvol
    Int_t NChildren(Int_t idvol);
    // Return child number idc of volume idvol
    Int_t Child(Int_t idvol, Int_t idc);
    G3toRoot &operator=(const G3toRoot &) {return *this;}
 private:
    TClonesArray   *fVolumes;    //! array of volumes  
    TClonesArray   *fMaterials;  //! array of materials
    TClonesArray   *fMedia;      //! array of materials
    TObjArray      *fRotations;  //! Rotation Matrices
    // Zebra bank related information	
    Int_t*     fZlq;              //! pointer to Zebra bank lq
    Float_t*   fZq;               //! pointer to Zebra bank q
    Int_t*     fZiq;              //! pointer to Zebra bank iq
    Gclink_t*  fGclink;           //! pointer to Geant common block 
    Gcnum_t*   fGcnum;            //! pointer to Geant common block
    // List Tree
    TFolder*   fTopFolder;        //! Folder structure containing volumes
    TGeometry* fGeometry;         //  Pointer to geometry
    Int_t      fExpand;           //  Flag for division expansion
    char*      fTopName;          //! Name of top-volume 
    ClassDef(G3toRoot,1) // Class responsible for geometry conversion for the G3 GUI 
};

#endif








