void sector()
{
 St_Node *hall = (St_Node *)chain->DataSet("HALL");
 St_Node *sector = (St_Node *)hall->FindByName("TPSS");
 TList *pos = sector->GetListOfPositions();
 St_NodePosition *padPos = 0;
 Int_t i = 0;
 TIter next(pos);
 Bool_t outter = kFALSE;
 while (padPos = (St_NodePosition *)next() )  {
   Int_t padRowNumber =  padPos->GetId();
   if (padRowNumber  <= 39 && ((padRowNumber %3)-2) )  continue;
   if (padRowNumber  <= 39) {
           padRowNumber /= 3;
           padRowNumber++;
   }
   else  {
     if (!outter) {cout << "outter sectors:" << endl; outter = kTRUE;}
     padRowNumber -= 39-13;     
   }
   cout << padRowNumber << ".  x = " << padPos->GetX()<< endl;
 }
}
