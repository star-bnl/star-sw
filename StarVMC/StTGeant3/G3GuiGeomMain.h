#ifndef G3GUIGEOMMAIN_H
#define G3GUIGEOMMAIN_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: G3GuiGeomMain.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $ */

#include "TClonesArray.h"
#include "TGFrame.h"
#include "TGListTree.h"
#include "TGComboBox.h"


class TGTab;
class TGMenuBar;
class TGPopupMenu;
class TGTextBuffer;
class TGTextEntry;
class TGLabel;
class TGTextButton;
class G3Node;
class TObjArray;
class TFolder;

class G3Material;
class G3Medium;
class G3GuiGeomDialog;
class G3Volume;

class G3GuiGeomMain : public TGMainFrame {
 public:
    G3GuiGeomMain(const TGWindow *p, UInt_t w, UInt_t h);
    virtual ~G3GuiGeomMain();
    // Destroy the main window
    virtual void CloseWindow();
    // Add item to ListTree
    virtual TGListTreeItem *
	AddItem(TObject *obj, TGListTreeItem* parent,
		const char* name,
		const TGPicture* open, const TGPicture* closed);
    // Add Material to ComboBox
    virtual void AddMaterial(G3Material *Material, Int_t i);
    // Add Medium to ComboBox
    virtual void AddMedium(G3Medium *Medium, Int_t i);
    // Process messages from this window
    virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
    // Update widgets
    virtual void Update();
    // Update ComboBoxes
    virtual void UpdateCombo();
    virtual void UpdateListBox();
    // Relate objects to ComboEntries
    // Currently ComboBox Entries are strings only, hence we need this construction
    virtual void SetMaterialComboEntries(TClonesArray *entries);
    virtual void SetMediaComboEntries(TClonesArray *entries);
    virtual void AddFoldersRecursively(TFolder* folder=0, TGListTreeItem* parent=NULL);
    virtual void Plot();
    virtual void CloseDialog() {fDialog = 0;}
private:
    TGTab              *fTab;           // Contains Tab entries: volumes, materials..
    TGCanvas           *fCanvasWindow;  // Canvas window for list tree
    TGCompositeFrame   *fF2, *fF21, *fF3, *fF31, *fF4, *fF5;      // Frames for combos
    TGCompositeFrame   *fF6, *fF61, *fF62, *fF63;                 // Frames for combos
    TGListTree         *fLt;                                      // Volumes list tree
    TGMenuBar          *fMenuBar;                                 // Menu bar: File, Draw Control ...
    TGPopupMenu        *fMenuFile, *fMenuTest, *fMenuHelp;        // Pop-up menus
    TGLayoutHints      *fMenuBarItemLayout, *fMenuBarHelpLayout,  // Lay-out hints
	               *fMenuBarLayout, fLTab;                    // Lay-out hints
    TGLayoutHints      *fL2;                                      // Lay-out hints
    G3GuiGeomDialog   *fDialog;                                  //! no output please
    TGComboBox         *fMaterialCombo;                           // Material  combo box
    TGComboBox         *fMechanismCombo;                          // Mechanism combo box
    TGComboBox         *fMediaCombo, *fParticleCombo;             // Media and particle combo boxes
    TGListBox          *fProcessLB, *fCutsLB;                     // List boxes for cuts and processes
    TClonesArray       *fComboMaterialEntries;                    // List of materials
    TClonesArray       *fComboMediaEntries;                       // List of media
    TGHorizontalFrame  *fHframe[6],*fHframeM[8];                  // sub frames 
    TGTextBuffer       *fTbh[6], *fTbhM[8], *fTbh61, *fTbh62, *fTbh63;  // text frames
    TGTextEntry        *fTeh[6], *fTehM[8], *fTeh61, *fTeh62, *fTeh63;  // text entries
    TGLabel            *fLabel[6], *fLabelM[8], *fSLabel61;             // labels
    TGTextButton       *fPlotButton;                                    // Plot-Button
    Float_t            fEmin;         // minimum energy for de/dx plot
    Float_t            fEmax;         // maximum energy for de/dx plot
    Int_t              fNbins;        // number of bins for de/dx plot
  G3GuiGeomMain(const G3GuiGeomMain &gm) 
    : TGMainFrame((const TGMainFrame&)gm) {}
  virtual G3GuiGeomMain & operator=(const G3GuiGeomMain &) {return *this;}
    

    ClassDef(G3GuiGeomMain,1)  // Main Widgets for the G3 GUI
};

R__EXTERN G3Material    *gCurrentMaterial;
R__EXTERN G3Medium      *gCurrentMedium;
R__EXTERN G3GuiGeomMain *gMainWindow;

#endif
