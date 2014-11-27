void markers(int lineWidth=0)
{
   // Display the table of markers with their numbers.
   TMarker *marker = new TMarker();
   marker->SetMarkerSize(3);
   TText *text = new TText();
   text->SetTextFont(62);
   text->SetTextAlign(22);
   text->SetTextSize(0.1);
   char atext[] = "       ";
   Double_t x = 0;
   Double_t dx = 1/12.0;
   for (Int_t i=1;i<12;i++) {
      x += dx;
      sprintf(atext,"%d",i);
      marker->SetMarkerStyle(i+ 1000*lineWidth);
      marker->DrawMarker(x,.35);
      text->DrawText(x,.17,atext);
      sprintf(atext,"%d",i+19);
      marker->SetMarkerStyle(i+19 + 1000*lineWidth);
      marker->DrawMarker(x,.8);
      text->DrawText(x,.62,atext);
   }
   delete marker;
   delete text;
}
