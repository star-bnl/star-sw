void pythiaTuneA(TPythia6* pyth){

   pyth->SetPARP(67,4.0);
   pyth->SetMSTP(81,1.0);
    //  pyth->SetMSTP(82,4.0);  <-- this one causes probles 
    //   with many lines of output like below ...  never finishes event 1
    // -->  Advisory warning: maximum violated by  1.464E+00 in event       1
    // -->  XSEC(96,1) increased to  1.467E+03
   pyth->SetPARP(83,0.5);
   pyth->SetPARP(84,0.4);
   pyth->SetPARP(89,1800.0);
   pyth->SetPARP(90,0.25);

   pyth->SetPARP(82,2.0);
   pyth->SetPARP(85,0.9);
   pyth->SetPARP(86,0.95);


}

//-------------------------------------------------------
void pythiaTuneB(TPythia6* pyth){

  //<-- same a tune a
   pyth->SetPARP(67,4.0);
   pyth->SetMSTP(81,1.0);
    //  pyth->SetMSTP(82,4.0);  <-- this one causes probles 
    //   with many lines of output like below ...  never finishes event 1
    // -->  Advisory warning: maximum violated by  1.464E+00 in event       1
    // -->  XSEC(96,1) increased to  1.467E+03
   pyth->SetPARP(83,0.5);
   pyth->SetPARP(84,0.4);
   pyth->SetPARP(89,1800.0);
   pyth->SetPARP(90,0.25);
   //<-- end same as tune a

   pyth->SetPARP(82,1.9);
   pyth->SetPARP(85,1.0);
   pyth->SetPARP(86,1.0);
}

//-------------------------------------------------------
void pythiaTunes(TPythia6* pyth, int itune){

  //
  // simple preset listing of tunes that we're not going to 
  // spend the effort to expose to the gui.
  //

  if(!itune) return;
  if(itune==1) return pythiaTuneA(pyth);
  if(itune==2) return pythiaTuneB(pyth);

}


//
// --------------- ok, here's the code to call -----------------
//
TPythia6* getPythia(const char* rframe, 
                    const char* cproj, 
                    const char* ctarg, 
                    float rts, 
                    int itune){
  TPythia6* retVal=new TPythia6();
  retVal->Initialize(rframe,cproj,ctarg,rts);

  retVal->SetMRPY(2,0); 

  pythiaTunes(retVal,itune);
  return retVal;
}

  
