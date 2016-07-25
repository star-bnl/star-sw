void xbins() {
 Float_t ePhiList[13] = {  0.0000, 0.5236, 1.0472, 1.5708, 2.0944, 2.6180, 3.1416,
				  3.6652, 4.1888, 4.7124, 5.2360, 5.7596, 6.2832  } ;  // 13 planes of phi - so can wrap around
 
 Float_t xbins[14];
 Float_t dx = 0;
 Float_t  x = -999;
 for (Int_t i = 0; i <= 13; i++) {
   if (i == 0)       xbins[i] = ePhiList[i] - 0.5*(ePhiList[i+1] - ePhiList[i]);
   else if (i == 13) xbins[i] = ePhiList[i-1] + 0.5*(ePhiList[i-1] - ePhiList[i-2]);
   else              xbins[i] = 0.5*(ePhiList[i-1] + ePhiList[i]);
   if (i > 0) {dx = 0.5*(xbins[i] - xbins[i-1]); x = 0.5*(xbins[i] + xbins[i-1]);}
   cout << "xbins[" << i << "] = " << xbins[i] << "\tdx = " << dx << "\tx = " << x <<  endl;
 }
}
