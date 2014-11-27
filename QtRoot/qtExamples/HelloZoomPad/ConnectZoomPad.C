void ConnectZoomPad (const char *canvas ="c1")
{
   //
   // To see the output of this macro, 
   // Execute:
   //  1. execute $ROOTSYS/tutorials/hsimple.C at once to create hsimple.root file
   //
   //     > root $ROOTSYS/tutorials/hsimple.C
   //
   //  2. Execute $ROOTSYS/tutorials/h1draw.C to create 3 TPad's "c1" TCanvas
   //     root[2] .x $ROOTSYS/tutorials/h1draw.C to create 3 Tpad's "c1" TCanvas
   //
   //  3. Execute this macro to create the TQtZoomPadWidget 
   //     and connect the TCanvas""Selected" ROOT signal to the TQtZoomPadWidget::Selected ROOT slot
   //
   //  4. Click over any TPad with the middle mouse button to popup the zoomer widgets;
   //
   //  Attention:  One can not zoom the TPad's with the temporary TH1 object copy
   //  ---------   created and drawn with the TObject::DrawCopy method.
   //              No workaround has been found yet. Consider using TObject::Draw.
   //
   //------------------------
   //
   // Create the Pad zoomer widget
   TQtZoomPadWidget *zoomer = new TQtZoomPadWidget();
   TCanvas *c = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(canvas);
   if (c) zoomer->Connect(c);
   else   fprintf(stderr," The TCanvas object <%s> has not been found\n", canvas);
 }
