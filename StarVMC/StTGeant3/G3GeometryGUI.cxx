/* *************************************************************************
 * Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 *                                                                        *
 * Author: The ALICE Off-line Project.                                    *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

/*
$Log: G3GeometryGUI.cxx,v $
Revision 1.1.1.1  2004/07/17 20:02:55  perev
STAR version of Geant321 TGeant3 etc

Revision 1.3  2004/01/28 08:17:52  brun
Reintroduce the Geant3 graphics classes (thanks Andreas Morsch)

Revision 1.1.1.1  2002/07/24 15:56:26  rdm
initial import into CVS

*/


//
// Steering class for the G3 GUI
// Author: Andreas Morsch
// andreas.morsch@cern.ch
//


#include "G3GeometryGUI.h"
#include "G3Volume.h"
#include "G3Material.h"
#include "G3Medium.h"
#include "G3GuiGeomMain.h"
#include "G3toRoot.h"

#include <TArrayF.h>
#include <TRotMatrix.h>
#include <TGeometry.h>
#include <TFile.h>
#include <TFolder.h>

G3Volume      *gCurrentVolume   = new G3Volume("NULL");
G3Material    *gCurrentMaterial = new G3Material();
G3Medium      *gCurrentMedium   = new G3Medium();
G3GuiGeomMain *gMainWindow      = 0;

ClassImp(G3GeometryGUI)

    G3GeometryGUI::G3GeometryGUI(const char* opt)
{
    char tmp[20];
    strcpy(tmp, opt);

// Constructor
    fPanel     = new G3GuiGeomMain(gClient->GetRoot(), 500, 500);
//  Store local copy of zebra bank entries
    G3toRoot* geometry = new G3toRoot();
    if (strcmp(tmp, "expand") == 0) geometry->SetExpandDivisions();
    geometry->G32root();
    char* topName = geometry->TopName();
    G3Volume* top = (G3Volume*) 
	(geometry->GetTopFolder()->FindObject(topName));
    gCurrentVolume = top;
//
//  Mediate between g3 Geometry and GUI
    fPanel->SetMaterialComboEntries(geometry->GetMaterials());
    fPanel->SetMediaComboEntries(geometry->GetMedia());
    fPanel->AddFoldersRecursively(geometry->GetTopFolder());
    fPanel->Update();
}

void G3GeometryGUI::Streamer(TBuffer &)
{
// Dummy Streamer
;
}





