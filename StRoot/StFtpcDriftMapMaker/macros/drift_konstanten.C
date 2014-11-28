void drift_konstanten(Float_t Vc)
{
  Float_t RA=30.05;
  Float_t RI=7.73;

  cout<<"Calculate the values needed for the creation of the drift map tables:"<<endl;

  cout<<"RA = "<<RA<<endl;
  cout<<"RI = "<<RI<<endl;

  Float_t inv_konst=1/log(RA/RI);
  //cout<<inv_konst<<endl;
 
  cout<<"Vc = "<<Vc<<endl;

  Float_t rtimesE=(inv_konst*Vc)*1000;

  cout<<"radiusTimesField = "<<rtimesE<<endl;
  //cout<<(rtimesE/RI-rtimesE/RA)/(RA-RI)<<endl;
  //Float_t eprocm=(rtimesE/RI-rtimesE/RA)/(RA-RI)/10;
  //cout<<eprocm<<endl;
  cout<<"minimumDriftField ="<<int (rtimesE/RA)<<endl;
  cout<<"maximumDriftField ="<<rtimesE/RI<<endl;
  //cout<<"minimumDriftField ="<<rtimesE/RI<<endl;
  cout<<"numberOfEFieldBinsUsed = "<<fabs((rtimesE/RA)-(rtimesE/RI))<<endl;
}
