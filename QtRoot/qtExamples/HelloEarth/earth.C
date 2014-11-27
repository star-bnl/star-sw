void earth()
{
  // Draw the globus using "worldmap.bmp" texture file 
  if ( gSystem->AccessPathName("earth.iv") ) {
    // Create the Inventor file if needed
    ofstream out("earth.iv");
    out << "#Inventor V2.1 ascii" << endl;
    out << " Texture2 { filename \"worldmap.bmp\" " << endl;
    out <<"  model REPLACE } "<< endl;
    out << "Sphere {   radius 10 }" << endl;
  }
  TCanvas *c = new TCanvas;  c->SetFillColor(kBlack);
  TVirtualViewer3D *viewer =0;
  if  (viewer = TVirtualViewer3D::Viewer3D(c,"oiv")) {
         gEnv->SetValue("Gui.InventorBackgroundShape","earth.iv");
         gEnv->SetValue("Gui.SnapShotFileCounter","100");

         viewer->BeginScene();  viewer->EndScene();
  }
}
