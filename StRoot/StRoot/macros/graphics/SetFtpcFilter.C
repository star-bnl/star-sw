{
  // Get a pointer to "displayMAker from chain
  StEventDisplayMaker *ds = (StEventDisplayMaker *)chain->Maker("EventDisplay");

  // Print the list of the defaults tables to be drawn
  ds->PrintNames();

  // Create the custom filter and attach it to "EventDisplay"
  StFtpcTrackFilter *f = new StFtpcTrackFilter();
  ds->SetFilter(f,StEventDisplayMaker::kTptTrack);
  ds->SetFilter(f,StEventDisplayMaker::kTable);

  // Edit the list of tables to be drawn
  // ds->RemoveName("dst/point(id_track,position[0]:position[1]:charge)");
  ds->RemoveName("dst/primtrk");
  ds->AddName("dst/globtrk");
  ds->PrintNames();

  // Redraw the last event
   cout << " if you want to remove hit points type:" << endl
        << "\t ds->RemoveName(\"dst/point(id_track,position[0]:position[1]:charge)\");" << endl 
        << "then:" << "\t ds->ReDraw()" << endl;
  ds->ReDraw();
}
