#include "LASERINO_TABLE.h"
void ConverL() {
  ofstream &out = cout;
  //  out.open("laserino.h", ios::out);
  out << "static LASERINO_t Bundles[14][6] = " << endl;
  //  cout << " ===== Bundles ======= " << endl;
  LASERINO_t Bundles[14][6];
  for (Int_t r = 0; r < 14; r++) {
    Int_t sector = Locals[r][0][0].Sector;
    Int_t s = (sector-1)/2;
    for (Int_t b = 0; b < 6; b++) {
      Int_t m = 3;
      Bundles[r][b] = Locals[r][b][m];
      Bundles[r][b].dX = Bundles[r][b].ddX = Bundles[r][b].dY = Bundles[r][b].ddY = 0;
      
      Bundles[r][b].DeltaPhi =  - 1.e-3*(dPhiC[s].Angle+dPhiC2[s].Angle);
      if (r == 0 && b == 0) out << " {{{";
      else {
	if (b == 0) out << "  {{";
	else        out << "   {";
      }
      //      Bundles[r][b].Print();
      Bundles[r][b].Write(out);
      if (r == 13 && b == 5) out << "}}};";
      else {
	if (b == 5) out << "}},";
	else        out << "},";
      }
      out << endl;
    }
  }
  //  cout << " ===== Mirrors ======= " << endl;
  LASERINO_t Mirrors[7];
  out << "static LASERINO_t Mirrors[14][6][7] = " << endl;
  for (Int_t r = 0; r < 14; r++) {
    for (Int_t b = 0; b < 6; b++) {
      Double_t phi0 = 0;
      for (Int_t m = 0; m < 3; m++) phi0 += Locals[r][b][m].DeltaPhi;
      Double_t phiM = -phi0;
      for (Int_t m = 0; m < 7; m++) {
	Mirrors[m]    =  Locals[r][b][m];
	Mirrors[m].X -=  Bundles[r][b].X;
	Mirrors[m].Y -=  Bundles[r][b].Y;
	Mirrors[m].Z -=  Bundles[r][b].Z;
	Mirrors[m].ThetaZ -=  Bundles[r][b].ThetaZ;
	Mirrors[m].dX -=  Bundles[r][b].dX;
	Mirrors[m].dY -=  Bundles[r][b].dY;
	Mirrors[m].DeltaPhi = phiM;
	phiM += Locals[r][b][m].DeltaPhi;
	//	Mirrors[m].Print();
      }
      //      cout << " ------------- " << endl;
      if ( Mirrors[0].Z > Mirrors[1].Z) { Mirrors[0].Status += 100;  Mirrors[1].Status += 100;/* cout << "Z  1 > 2" << endl;  Mirrors[0].Print();  Mirrors[1].Print(); */}
      if ( Mirrors[0].Z > Mirrors[4].Z) { Mirrors[0].Status += 100;  Mirrors[3].Status += 100;/* cout << "Z  1 > 4" << endl;  Mirrors[0].Print();  Mirrors[3].Print(); */}
      if ( Mirrors[6].Z > Mirrors[3].Z) { Mirrors[6].Status += 100;  Mirrors[3].Status += 100;/* cout << "Z  7 > 4" << endl;  Mirrors[6].Print();  Mirrors[3].Print(); */}
      if ( Mirrors[6].Z > Mirrors[5].Z) { Mirrors[6].Status += 100;  Mirrors[5].Status += 100;/* cout << "Z  7 > 6" << endl;  Mirrors[6].Print();  Mirrors[5].Print(); */}
				                                          		    			      
      if ( Mirrors[1].Z > Mirrors[2].Z) { Mirrors[1].Status += 100;  Mirrors[4].Status += 100;/* cout << "Z  2 > 3" << endl;  Mirrors[1].Print();  Mirrors[4].Print(); */}
      if ( Mirrors[3].Z > Mirrors[2].Z) { Mirrors[3].Status += 100;  Mirrors[2].Status += 100;/* cout << "Z  4 > 3" << endl;  Mirrors[3].Print();  Mirrors[2].Print(); */}
      if ( Mirrors[3].Z > Mirrors[4].Z) { Mirrors[3].Status += 100;  Mirrors[4].Status += 100;/* cout << "Z  4 > 5" << endl;  Mirrors[3].Print();  Mirrors[4].Print(); */}
      if ( Mirrors[5].Z > Mirrors[4].Z) { Mirrors[5].Status += 100;  Mirrors[4].Status += 100;/* cout << "Z  6 > 5" << endl;  Mirrors[5].Print();  Mirrors[4].Print(); */}
				                                          		    			      
      if ( Mirrors[0].X > Mirrors[1].X) { Mirrors[0].Status +=   1;  Mirrors[1].Status +=   1;/* cout << "X  1 > 2" << endl;  Mirrors[0].Print();  Mirrors[1].Print(); */}
      if ( Mirrors[0].X > Mirrors[3].X) { Mirrors[0].Status +=   1;  Mirrors[3].Status +=   1;/* cout << "X  1 > 4" << endl;  Mirrors[0].Print();  Mirrors[3].Print(); */}
      if ( Mirrors[0].X > Mirrors[5].X) { Mirrors[0].Status +=   1;  Mirrors[5].Status +=   1;/* cout << "X  1 > 6" << endl;  Mirrors[0].Print();  Mirrors[5].Print(); */}
				                                          		    			      
      if ( Mirrors[6].X > Mirrors[1].X) { Mirrors[6].Status +=   1;  Mirrors[1].Status +=   1;/* cout << "X  7 > 2" << endl;  Mirrors[6].Print();  Mirrors[1].Print(); */}
      if ( Mirrors[6].X > Mirrors[3].X) { Mirrors[6].Status +=   1;  Mirrors[3].Status +=   1;/* cout << "X  7 > 4" << endl;  Mirrors[6].Print();  Mirrors[3].Print(); */}
      if ( Mirrors[6].X > Mirrors[5].X) { Mirrors[6].Status +=   1;  Mirrors[5].Status +=   1;/* cout << "X  7 > 6" << endl;  Mirrors[6].Print();  Mirrors[5].Print(); */}
				                                          		    			      
      if ( Mirrors[1].X > Mirrors[2].X) { Mirrors[0].Status +=   1;  Mirrors[2].Status +=   1;/* cout << "X  1 > 3" << endl;  Mirrors[0].Print();  Mirrors[2].Print(); */}
      if ( Mirrors[3].X > Mirrors[2].X) { Mirrors[3].Status +=   1;  Mirrors[2].Status +=   1;/* cout << "X  4 > 3" << endl;  Mirrors[3].Print();  Mirrors[2].Print(); */}
      if ( Mirrors[5].X > Mirrors[2].X) { Mirrors[5].Status +=   1;  Mirrors[2].Status +=   1;/* cout << "X  6 > 3" << endl;  Mirrors[5].Print();  Mirrors[2].Print(); */}
				                                          		    			      
      if ( Mirrors[1].X > Mirrors[4].X) { Mirrors[0].Status +=   1;  Mirrors[4].Status +=   1;/* cout << "X  1 > 5" << endl;  Mirrors[0].Print();  Mirrors[4].Print(); */}
      if ( Mirrors[3].X > Mirrors[4].X) { Mirrors[3].Status +=   1;  Mirrors[4].Status +=   1;/* cout << "X  4 > 5" << endl;  Mirrors[3].Print();  Mirrors[4].Print(); */}
      if ( Mirrors[5].X > Mirrors[4].X) { Mirrors[5].Status +=   1;  Mirrors[4].Status +=   1;/* cout << "X  6 > 5" << endl;  Mirrors[5].Print();  Mirrors[4].Print(); */}
				                                          		    			      
      if ( Mirrors[1].Y > Mirrors[0].Y) { Mirrors[1].Status +=  10;  Mirrors[0].Status +=  10;/* cout << "Y  2 > 1" << endl;  Mirrors[1].Print();  Mirrors[0].Print(); */}
      if ( Mirrors[1].Y > Mirrors[2].Y) { Mirrors[1].Status +=  10;  Mirrors[2].Status +=  10;/* cout << "Y  2 > 3" << endl;  Mirrors[1].Print();  Mirrors[2].Print(); */}
				                                          		    			      
      if ( Mirrors[0].Y > Mirrors[3].Y) { Mirrors[0].Status +=  10;  Mirrors[4].Status +=  10;/* cout << "Y  1 > 4" << endl;  Mirrors[0].Print();  Mirrors[4].Print(); */}
      if ( Mirrors[2].Y > Mirrors[3].Y) { Mirrors[2].Status +=  10;  Mirrors[4].Status +=  10;/* cout << "Y  3 > 4" << endl;  Mirrors[2].Print();  Mirrors[4].Print(); */}
      if ( Mirrors[3].Y > Mirrors[4].Y) { Mirrors[3].Status +=  10;  Mirrors[4].Status +=  10;/* cout << "Y  4 > 5" << endl;  Mirrors[3].Print();  Mirrors[4].Print(); */}
      if ( Mirrors[3].Y > Mirrors[6].Y) { Mirrors[3].Status +=  10;  Mirrors[6].Status +=  10;/* cout << "Y  4 > 7" << endl;  Mirrors[3].Print();  Mirrors[6].Print(); */}
      if ( Mirrors[4].Y > Mirrors[5].Y) { Mirrors[4].Status +=  10;  Mirrors[5].Status +=  10;/* cout << "Y  5 > 6" << endl;  Mirrors[4].Print();  Mirrors[5].Print(); */}
      if ( Mirrors[6].Y > Mirrors[5].Y) { Mirrors[6].Status +=  10;  Mirrors[5].Status +=  10;/* cout << "Y  7 > 6" << endl;  Mirrors[6].Print();  Mirrors[5].Print(); */}
      for (Int_t m = 0; m < 7; m++) {
	if (r == 0 && b == 0 && m == 0) out << " {{{{";
	else {
	  if (b == 0 && m == 0)         out << "  {{{";
	  else        {
	    if (m == 0)                 out << "   {{"; 
	    else                        out << "    {";
	  }
	}
	//      Bundles[r][b].Print();
	Mirrors[m].Write(out);
	if (r == 13 && b == 5 && m == 6) out << "}}}};";
	else {
	  if          (b == 5 && m == 6) out << "}}},";
	  else {
	    if                  (m == 6) out << "}}";
	    else                         out << "},";
	  }
	}
	out << endl;
      }
      //      cout << " ------------- " << endl;
    }
  }
  //  out.close();
}
