{{
/* This is a very simple example macro using the StMuDstMaker to open
 * a MuDst file and print out some event and charged track information
 */
  gROOT->Reset();
  gROOT->Macro("loadMuDst.C");
  TString muDstFile = "/star/data15/reco/ppProduction2008/ReversedFullField/P08ie/2008/046/9046031/st_physics_adc_9046031_raw_2070002.MuDst.root";
  mudst_mk=new StMuDstMaker(0,0,muDstFile.Data());
  mudst_mk->Init();
  
  Int_t n_evt=1;
//  for (Int_t i_evt=0; i_evt<n_evt; i_evt++) 
  {
    mudst_mk->Make();
    if (mudst_mk->muDst()==0) {
      cout << "No event" << endl;
      continue;
    }
    StMuEvent *event=mudst_mk->muDst()->event();
    cout << "Event: " << event->eventId() << endl;
    StThreeVectorF vtx_pos = event->primaryVertexPosition();
    cout << "Vertex at " << vtx_pos.x() << " " << vtx_pos.y() << " " << vtx_pos.z() << endl;
    StuDraw3DMuEvent::Display()->Point(vtx_pos.x(),vtx_pos.y(),vtx_pos.z(),kVtx);
    StuDraw3DMuEvent::Display()->Tracks();
    StuDraw3DMuEvent::Display()->Tracks(primary);
    cout << "Rendering . . . ." << endl;
    StuDraw3DMuEvent::Display()->Update();
  }

}}
