// $Id: Get3DSize.C,v 1.4 1999/05/21 15:58:44 kathy Exp $
// $Log: Get3DSize.C,v $
// Revision 1.4  1999/05/21 15:58:44  kathy
// fix name of owner on Get3DSize
//
// Revision 1.3  1999/05/21 15:33:50  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: This macro prints out the sizes of the sekected 3d pad
//=======================================================================
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
