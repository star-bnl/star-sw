// $Id: Example_DateTime.C,v 1.1 1999/12/07 22:00:30 kathy Exp $
// $Log: Example_DateTime.C,v $
// Revision 1.1  1999/12/07 22:00:30  kathy
// new example macro showing how to get date and time so I won't have to figure it out again
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//    Shows how to get the date and time 
//=======================================================================

void Example_DateTime()
{

 TDatime now;
 Int_t ndate=now.GetDate();
 Int_t ntime=now.GetTime();
 cout << " DATE = "  << ndate << " TIME = " << ntime << endl;

 TDatime kathy;
 const Char_t *amy = kathy.AsString();
 cout << " Date & Time: " << amy << endl;


}




