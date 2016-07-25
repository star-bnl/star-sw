void C2Html(TPad *c1 = 0, Double_t scale = 20) {
  if (! c1) c1 = gPad;
  /*  TQtZoomPadWidget *zummer = new TQtZoomPadWidget(c1);
  zummer->resize(900,650);
  TQtCanvas2Html d(c1,scale,"./",zummer);
  */
  TQtCanvas2Html d(c1,900,650);

}
