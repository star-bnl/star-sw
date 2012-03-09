#include "StFgtDb.h"
#include <fstream>
using namespace std;

void
StFgtDb::printFgtDumpCSV1(TString fname, int myDate, int myTime) {

  ofstream fd;
  fd.open(fname.Data());

  if (fd.is_open())
    {

      cout<<"Saving FGT output in "<<fname.Data()<<endl;
      
      fd <<"#  FGT mapping, timeStamp "<< myDate<<" "<<myTime<<"\n";
      fd<<"# electId,geoID,   RDO(1;2),ARM(0-4),APV(0-9;12-21),chan(0-127),  disk(1-6),quad(A-D),layer(P;R),strip(P:0-719;R0-279+400-679), ordinate(rad;cm),lowSpan(cm;rad),upSpan(cm;rad), geoName, stat,ped(ADC),sigPed(ADC)\n";
      
      int nTry=0, nMap=0;
      for (int rdo=1;rdo<=2;rdo++){// 2 RDOs numbered 1,2
	for (int arm=0;arm<6;arm++){//6 arms numbered from 0
	  for (int apv=0;apv<=21;apv++){//24 APVs numbered 0-23 but in real life APV# 10,11,22,23 are unused so 0-19 in determining electronic Id
	    if ((apv==10)||(apv==11)) continue;	
	  
	    for (int channel=0;channel<128;channel++){//128 channels numbered from 0
	      
	      nTry++;
	      int geoId=StFgtDb::getGeoIdFromElecCoord(rdo, arm, apv, channel);
	      if (geoId<0) continue;
	      nMap++;
	      Short_t disk,quad,strip; Char_t layer;
	      Double_t  ordinate,  lowerSpan,  upperSpan;
	      StFgtGeom::decodeGeoId(geoId,disk,quad,layer,strip);
	      StFgtGeom::getPhysicalCoordinate(geoId,disk,quad,layer,ordinate,  lowerSpan,  upperSpan);
	      
	      double  ped=StFgtDb::getPedestalFromElecCoord(rdo,arm,apv,channel);
	      double  pedSig=StFgtDb::getPedestalSigmaFromElecCoord(rdo,arm,apv,channel);
	      Short_t stat=StFgtDb::getStatusFromElecCoord(rdo,arm,apv,channel);	      
	      int     electId = StFgtGeom::getElectIdFromElecCoord(rdo,arm,apv,channel);
	      std::string  geoName=StFgtDb::getGeoNameFromElecCoord(rdo,arm,apv,channel);	 
	      
	      
	      fd<<electId<<" "<<geoId<<"  "<<rdo<<"  "<<arm<<"  "<<apv<<"  "<<channel<<"  "<<disk+1<<"  "<<quad+'A'<<"  "<<layer<<"  "<<strip<<"  "<<ordinate<<"  "<<lowerSpan<<"  "<<upperSpan<<"  "<<geoName.data()<<"  "<<stat<<"  "<<ped<<"  "<<pedSig<<"\n";
	    
	    }
	  }
	}
      }
    

      fd<<"#  FGT mapping end, nTry= "<<nTry<<" nMap="<< nMap<<" from StFgtDb\n";
      fd.close();
      
    }
  else
   {
     cout << "Error opening file";
   }
  
}
