{
  // This macro prints out the sizes of the sekected 3d pad
  if (gPad) {
    TView *view = gPad->GetView(); 
    Float_t min[3],max[3];
    view->GetRange(min,max);
    for (int i=0;i<3; i++) printf("%d.  min = %f, max = %f \n", i, min[i],max[i]);
  }
  else
    printf(" No 3 view has been created yet\n");
}
