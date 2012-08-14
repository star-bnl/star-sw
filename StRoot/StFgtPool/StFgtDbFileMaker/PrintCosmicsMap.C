#include <iostream>
#include <fstream>
#include <string>
using namespace std;

void PrintCosmicsMap()
{

  ifstream f;
  f.open("fgtMapDump.csv");
  if (!f) return;
  
  Int_t electId,geoId,rdo,arm,apv,chan,disk,quad,strip,stat;
  Double_t ord,low,high,ped,sigped;
  string layer,geo,;
  string line;
  Int_t newEid,mapping[51280];
  
  for (int xx = 0;xx<51280; xx++) mapping[xx]=-1;

  getline(f,line);
  cout<<line<<endl;
  getline(f,line);
  cout<<line<<endl;

  Int_t ii = 0;
  while ((f.is_open())&&(ii<26879))
    {

      
      f>>electId>>geoId>>rdo>>arm>>apv>>chan>>disk>>quad>>layer>>strip>>ord>>low>>high>>geo>>stat>>ped>>sigped;
      ii++;
      

      //find electronic id of old location 
      //eId = channel + 128 * ( apv + 20 * (arm + 6 * ( rdo - 1) ) );
      //1B short was at RDO 2,ARM 0, APV 5-9  which ranges from 125*128 - 130*128  (16000-16639)
      //         now at RDO 1,ARM 0, APV 5-9  which ranges from   5*128 -   10*128    (640-1279)
      //1C short was at RDO 2,ARM 0, APV 17-21  which ranges from 135*128 - 140*128  (17280-17919)
      //         now at RDO 1,ARM 0, APV 17-21  which ranges from  15*128 -  20*128  ( 1920-2559)
      //1D short was at RDO 1,ARM 0, APV 5-9  which ranges from  5*128 - 10*128  (640-1279)
      //         now at RDO 1,ARM 1, APV 5-9  which ranges from  25*128 - 30*128 (3200-3839)


      // if (electId%1000==0) cout<< electId<<endl;

      if ((electId>=16000)&&(electId<=16639))//disk 1Bshort
	{

	  if (apv > 11) apv = apv-2;
	  newEid = chan + 128 * ( apv + 20 * (arm + 6 * ( 1 - 1) ) );//set rdo = 1
	  //cout<<electId<<", "<<geoId<<", "<<rdo<<", "<<arm<<", "<<apv<<", "<<chan<<", "<<disk<<", "<<quad<<", "<<layer<<", "<<strip<<", "<<ord<<", "<<low<<", "<<high<<", "<<geo<<", "<<stat<<", "<<ped<<", "<<sigped<<", "<<endl;
	  //cout<<newEid<<endl;
	  mapping[newEid]=geoId;
	}
      
      if ((electId<=17919)&&(electId>=17280))//disk 1Cshort
	{

	  if (apv > 11) apv = apv-2;
	  newEid = chan + 128 * ( apv + 20 * (arm + 6 * ( 1 - 1) ) );//set rdo = 1
	  //cout<<electId<<", "<<geoId<<", "<<rdo<<", "<<arm<<", "<<apv<<", "<<chan<<", "<<disk<<", "<<quad<<", "<<layer<<", "<<strip<<", "<<ord<<", "<<low<<", "<<high<<", "<<geo<<", "<<stat<<", "<<ped<<", "<<sigped<<", "<<endl;
	  //cout<<newEid<<endl;
	  mapping[newEid]=geoId;
	}

      if ((electId<=1279)&&(electId>=640))//disk 1Dshort
	{

	  if (apv > 11) apv = apv-2;
	  newEid = chan + 128 * ( apv + 20 * (1 + 6 * ( rdo - 1) ) );//set rdo = 1
	  //cout<<electId<<", "<<geoId<<", "<<rdo<<", "<<arm<<", "<<apv<<", "<<chan<<", "<<disk<<", "<<quad<<", "<<layer<<", "<<strip<<", "<<ord<<", "<<low<<", "<<high<<", "<<geo<<", "<<stat<<", "<<ped<<", "<<sigped<<", "<<endl;
	  //cout<<newEid<<endl;
	  mapping[newEid]=geoId;
	}

    }


  for (int xx = 0;xx<51280; xx++) cout<<xx<<" "<<mapping[xx]<<endl;;
 
  f.close();
}
