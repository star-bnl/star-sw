class StarAgmlViewer;
StarAgmlViewer *view;

void          MakeViewerScene(TEveWindowSlot* slot, TEveViewer*& v, TEveScene*& s);
TEveCaloLego* MakeCaloLego(TEveCaloData* data, TEveWindowSlot* slot);
TEveCalo2D*   MakeCalo2D(TEveCalo3D* calo3d, TEveWindowSlot* slot,TEveProjection::EPType_e t);
TEveCalo3D*   MakeCalo3D(TEveCaloData* data, TEveWindowSlot* slot);

const Double_t RADIUS      = 364.90;   // cm  MAGP rmax
const Double_t HALF_LENGTH = 715.00/2; // cm  MAGP length/2

TObjectSet *set = 0;

void viewAgml( const Char_t *tag = "y2014", 
	       const Char_t *top = "IDSM", 
	       const Char_t *filename = 0,
	       Double_t thresh=0.01 )

{
  TString _chain = Form("r%s agml sti gstar",tag);

  TEveManager::Create()

  gROOT->LoadMacro("bfc.C");
  bfc(-1, _chain );

  gSystem->Load("libEve.so");
  gSystem->Load("StarAgmlViewer.so");

  view = new StarAgmlViewer();
  chain->Init();

  if ( filename ) TGeoManager::Import(filename); 
    
  // Generate histogram
  StarAgmlChecker checker( gGeoManager );
  set = (TObjectSet *) checker.MaterialPlot( top, 50, -2.50, +2.50 );
			  
  TBrowser *b = new TBrowser();
  b->Add(set);
  // Retrieve histogram from set
  //  TH2F *radlen = (TH2F *)set->GetObject();

  return;

  // Create a TEve calo histo
  Int_t colors[] = { 
    kRed,
    kMagenta,
    kBlue,
    kOrange,
    kAzure,
    kMagenta,
    kGreen,
    kPink,
    kTeal,
    kYellow,
  };
  Int_t sizeofcolors = sizeof(colors)/sizeof(Int_t);
		    

  TEveCaloDataHist* data = new TEveCaloDataHist(); {
    //  data -> AddHistogram( radlen );
    //  data -> RefSliceInfo(0).Setup("RADLEN", thresh, kRed, 30);

    Int_t nn = set -> GetListSize();

    for ( Int_t islice=0;islice<nn;islice++ )
      {
	TObjectSet *obj = (TObjectSet *)set->At(islice);
	if ( !obj ) continue;

	TH2F *radlen = (TH2F *)obj->GetObject();
	if ( !radlen) continue;

	Int_t color = colors[ islice%sizeofcolors ] - islice/sizeofcolors;

	data -> AddHistogram( radlen );
	data -> RefSliceInfo( islice ).Setup( radlen->GetName(), thresh, color, 30 );

	data->GetEtaBins()->SetTitleFont(120);
	data->GetEtaBins()->SetTitle("h");
	data->GetPhiBins()->SetTitleFont(120);
	data->GetPhiBins()->SetTitle("f");

      }
	
    data->IncDenyDestroy();
    gEve->AddToListTree(data, kFALSE);

  }

  ////////////////////////////////////////////////////////////////////////////////////////////

  // Add calorimeter lego plot in 2nd tab
  TEveWindowSlot* slot = TEveWindow::CreateWindowInTab(gEve->GetBrowser()->GetTabRight());
  TEveCaloLego* lego = MakeCaloLego(data, slot);


  // 3rd tab perform projections
  slot = TEveWindow::CreateWindowInTab(gEve->GetBrowser()->GetTabRight());

  /// Creates a packed window
  TEveWindowPack* packH = slot->MakePack(); 
  packH->SetElementName("Projections");
  packH->SetHorizontal();
  packH->SetShowTitleBar(kFALSE);
  
  /// New slot in the window at left
  slot = packH->NewSlot();

  /// Will also be packed
  TEveWindowPack* pack0 = slot->MakePack();
  pack0->SetShowTitleBar(kFALSE);

  /// Left top and left bottom slots
  TEveWindowSlot*  slotLeftTop   = pack0->NewSlot();
  TEveWindowSlot* slotLeftBottom = pack0->NewSlot();

  /// New slot at right
  slot = packH->NewSlot();

  /// Will also be packed
  TEveWindowPack* pack1 = slot->MakePack();
  pack1->SetShowTitleBar(kFALSE);

  /// Right top and right bottom slots
  TEveWindowSlot* slotRightTop    = pack1->NewSlot();
  TEveWindowSlot* slotRightBottom = pack1->NewSlot();

  // viewers ans scenes in second tab
  TEveCalo3D* calo3d = MakeCalo3D(data, slotRightTop); {
    MakeCalo2D(calo3d, slotLeftTop, TEveProjection::kPT_RPhi);
    MakeCalo2D(calo3d, slotLeftBottom, TEveProjection::kPT_RhoZ);
  }
  TEveCaloLego* lego = MakeCaloLego(data, slotRightBottom);

  //////////////////////////

  gEve->GetBrowser()->GetTabRight()->SetTab(1);
  gEve->Redraw3D(kTRUE);

    


}


///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////


void MakeViewerScene(TEveWindowSlot* slot, TEveViewer*& v, TEveScene*& s)
{
   // Create a scene and a viewer in the given slot.

   v = new TEveViewer("Viewer");
   v->SpawnGLViewer(gEve->GetEditor());
   slot->ReplaceWindow(v);
   gEve->GetViewers()->AddElement(v);
   s = gEve->SpawnNewScene("Scene");
   v->AddScene(s);
}

///////////////////////////////////////////////////////////////////////////////////////////////

TEveCaloLego* MakeCaloLego(TEveCaloData* data, TEveWindowSlot* slot)
{
   // Eta-phi lego view.

   TEveViewer* v;
   TEveScene* s;
   if (slot)
   {
      TEveViewer* v; TEveScene* s;
      MakeViewerScene(slot, v, s);
   } else {
      v = gEve->GetDefaultViewer();
      s = gEve->GetEventScene();
   }
   v->SetElementName("Viewer - Lego");
   s->SetElementName("Scene - Lego");

   gStyle->SetPalette(1, 0);
   TEveCaloLego* lego = new TEveCaloLego(data);
   s->AddElement(lego);

   // By the default lego extends is (1x1x1). Resize it to put in 'natural' 
   // coordinates, so that y extend in 2*Pi and set height of lego two times
   //  smaller than y extend to have better view in 3D perspective.
   lego->InitMainTrans();
   lego->RefMainTrans().SetScale(TMath::TwoPi(), TMath::TwoPi(), TMath::Pi());

   // draws scales and axis on borders of window
   TGLViewer* glv = v->GetGLViewer();
   TEveCaloLegoOverlay* overlay = new TEveCaloLegoOverlay();
   glv->AddOverlayElement(overlay);
   overlay->SetCaloLego(lego);

   // set event handler to move from perspective to orthographic view.
   glv->SetCurrentCamera(TGLViewer::kCameraOrthoXOY);
   glv->SetEventHandler
      (new TEveLegoEventHandler(glv->GetGLWidget(), glv, lego));
   gEve->AddToListTree(lego, kTRUE);

   return lego;
}
//______________________________________________________________________________
TEveCalo3D* MakeCalo3D(TEveCaloData* data, TEveWindowSlot* slot)
{
   // 3D catersian view.

   TEveViewer* v; TEveScene* s;
   MakeViewerScene(slot, v, s);
   v->SetElementName("Viewer - 3D");
   s->SetElementName("Scene - 3D");

   TEveCalo3D* calo3d = new TEveCalo3D(data);
   calo3d->SetBarrelRadius(RADIUS);
   calo3d->SetEndCapPos   (HALF_LENGTH);
   s->AddElement(calo3d);

   //   add_jet(calo3d, "JetCone Lojz",  1.4,  1.0, 0.4, 0.2);
   //   add_jet(calo3d, "JetCone Mici", -2.0, -2.1, 0.2, 0.4);

   return calo3d;
}

//______________________________________________________________________________
TEveCalo2D* MakeCalo2D(TEveCalo3D* calo3d, TEveWindowSlot* slot,
                       TEveProjection::EPType_e t)
{
   // Projected calorimeter.

   TEveViewer* v; TEveScene* s;
   MakeViewerScene(slot, v, s);
   v->SetElementName("Viewer - 2D");
   s->SetElementName("Scene - 2D");

   TEveProjectionManager* mng = new TEveProjectionManager();
   mng->SetProjection(t);

   TEveProjectionAxes* axes = new TEveProjectionAxes(mng);
   s->AddElement(axes);
   TEveCalo2D* calo2d = (TEveCalo2D*) mng->ImportElements(calo3d);
   s->AddElement(calo2d);

   v->GetGLViewer()->SetCurrentCamera(TGLViewer::kCameraOrthoXOY);

   gEve->AddToListTree(mng, kTRUE);
   gEve->AddToListTree(calo2d, kTRUE);

   return calo2d;
}
