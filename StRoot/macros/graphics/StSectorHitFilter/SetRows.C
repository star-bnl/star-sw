void SetRows(){

  // Author: Valerie Fine (fine@bnl.gov) 19 July 2000 BNL
  // $Id: SetRows.C,v 1.1 2000/07/19 21:53:23 fine Exp $
  // This function defines the list of the (sector*100+padrow)
  // to be present by StEventDusplayMaker with StSectorHitFilter

 Int_t sec[] = {138,140,142,144};
 ((StSectorHitFilter *)trackFilter)->SetSecRow(sec,sizeof(sec)/sizeof(Float_t));
  if (dsMaker) dsMaker->ReDraw();
}

