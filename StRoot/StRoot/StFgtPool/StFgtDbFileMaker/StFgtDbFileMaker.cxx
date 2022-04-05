//*-- Author : Renee Fatemi (UKY)


#include "StFgtDbFileMaker.h"
#include "TDataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "StFgtUtil/geometry/StFgtGeom.h"

ClassImp(StFgtDbFileMaker)


StFgtDbFileMaker::StFgtDbFileMaker(const char *name):StMaker(name){

}


StFgtDbFileMaker::~StFgtDbFileMaker(){

}


Int_t StFgtDbFileMaker::Init(){
  

  //initialize geoIds in array to -1
  for (Int_t i = 0;i< 51200;i++){
    mapping[i]=-1; 
  }
  return StMaker::Init();
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StFgtDbFileMaker::Make(){

  //printBigFgtGeomMap();
  printRealDbMappingFile();
 

 return kStOK;
}

Int_t StFgtDbFileMaker::Finish(){
   for (Int_t i = 0;i< 51200;i++){
     cout<<i<<"   "<< mapping[i]<<endl; 
   }
 return kStOk;
}

void StFgtDbFileMaker::printBigFgtGeomMap()
{

 for (rdo=1;rdo<3;rdo++){// 2 RDOs numbered 1,2
    for (arm=0;arm<6;arm++){//6 arms numbered from 0
      for (apv=0;apv<24;apv++){//24 APVs numbered 0-23 but in real life APV# 10,11,22,23 are unused so 0-19 in determining electronic Id
	for (channel=0;channel<128;channel++){//128 channels numbered from 0

	  if ((apv==10)||(apv==11)||(apv==22)||(apv==23)) continue;

	  apvMod = apv;
	  if (apv > 11) apvMod = apv-2;
	  
	  geoId = geom->getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);
	  geom->decodeGeoId(geoId,disk,quad,layer,strip);
	  electId = getElectId(rdo,arm,apvMod,channel);
	  cout<<" geoId="<<geoId<<" electId="<<electId<<
	    " rdo="<<rdo<<" arm="<<arm<<" apv="<<apv<<" apvMod="<<apvMod<<" channel="<<channel<<
	    " disk="<<disk<<" quad="<<quad<<" layer="<<layer<<" strip="<<strip<<endl;
	}
      }
    }
  }

}


void StFgtDbFileMaker::printIdealDbMappingFile(){

  for (rdo=1;rdo<3;rdo++){// 2 RDOs numbered 1,2
    for (arm=0;arm<6;arm++){//6 arms numbered from 0
      for (apv=0;apv<24;apv++){//24 APVs numbered 0-23 but in real life APV# 10,11,22,23 are unused so 0-19 in determining electronic Id
	for (channel=0;channel<128;channel++){//128 channels numbered from 0
	  
	  if ((apv==10)||(apv==11)||(apv==22)||(apv==23)) continue;

	  apvMod = apv;
	  if (apv > 11) apvMod = apv-2;
	  
	  geoId = geom->getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);
	  geom->decodeGeoId(geoId,disk,quad,layer,strip);
	  electId = getElectId(rdo,arm,apvMod,channel);
	  cout<<electId<<"  "<<geoId<<endl;

	}
      }
    }
  }
}

void StFgtDbFileMaker::printRealDbMappingFile(){

  //in principle we could use the file provided by Ben Buck here:
  //http://drupal.star.bnl.gov/STAR/system/files/APV%20Connection%20Map%20Simplified-DAQ%20perspectave_0.txt
  //however this file has a different number of sig figs and is stored in mm instead of cm (as in StFgtGeom). So the input 
  //file above was opened in a spreadsheet and modified to have the same sigfig and units as those in StFgtGeom::mStrips

  ifstream f;
  f.open("NewMappingFileMark2.txt");
  if (!f) return;
   
  Int_t apv,ch,bs;
  Double_t r,phi;
  Char_t layer;
  Int_t strip,stripId;
  Int_t rdo,arm,group;
  Int_t electronicID,geoId;

  
  while (f>>ch>>apv>>bs>>r>>phi)
    {

      //read in file and check it is correct
      if (0) cout<<" ch = "<<ch<<" apv = "<<apv<<" bs="<<bs<<" r="<<r<<" phi="<<phi<<endl;
       

      if (phi == -1) layer = 'R';
      if (r == -1) layer = 'P';
      
      //loop over disk and quad to get strips (from StFgtGeom:mStrips[]) that correspond to this r,phi position
      //Phi is unique but there are two identical r positions for each quadrant. 
      //Fortunately the r value is unique for APV group 0-4 and 5-9. 
      //GeoId is then constructed from disk,quad, layer and strip
      //this geoId is then associated with the APV+ch that corresponds to the r,phi position from the original file
      //The electronic ID is calculated by combining APV+ch with the RDO+ARM+APV_GROUP as defined in Ben's file:
      //https://docs.google.com/spreadsheet/ccc?key=0Al3dLXZXg8e8dDBSSXplYndKWmNyQ2FkSmppYTNjR3c#gid=1 

      for (int disk= 0; disk< 6; ++disk)
	{
	  for (int quad = 0; quad<4; ++quad)
	    {	      	    

	      if (apv < 5) 
		{
		  stripId = -1;	
		  if (layer == 'P') stripId = searchPhiStripId(phi);
		  if (layer == 'R') stripId = searchRStripId_HighPhi(r);
		  
		  if (disk == 0)//disk1
		    {
		      
		      if (quad == 3) //this assumes D == 3!
			{
			  rdo=2;
			  arm=0;
			  group = 1;
			}
		      
		      
		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=1;
			  arm=0;
			  group = 0;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=1;
			  arm=0;
			  group = 1;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  rdo=2;
			  arm=0;
			  group = 0;
			}

		    }

		  //disk 2
		  if (disk == 1)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=1;
			  arm=1;
			  group = 0;
		     
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=1;
			  arm=1;
			  group = 1;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }


		  //disk 3
		  if (disk == 2)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=2;
			  arm=1;
			  group = 1;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=1;
			  arm=2;
			  group = 0;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }

		  //disk 4
		  if (disk == 3)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=2;
			  arm=2;
			  group = 0;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=2;
			  arm=2;
			  group = 1;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }


		  //disk 5
		  if (disk == 4)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=1;
			  arm=3;
			  group = 1;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=2;
			  arm=3;
			  group = 0;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }


		  //disk 6
		  if (disk == 5)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=1;
			  arm=4;
			  group = 0;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=1;
			  arm=4;
			  group = 1;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }

		}
	      else
		{


		  stripId = -1;	
		  if (layer == 'P') stripId = searchPhiStripId(phi);
		  if (layer == 'R') stripId = searchRStripId_LowPhi(r);
		  
		  if (disk == 0)//disk1
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  rdo = 1;
			  arm = 0;
			  group = 0;
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=1;
			  arm=0;
			  group = 1;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=2;
			  arm=0;
			  group = 0;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  rdo=2;
			  arm=0;
			  group = 1;
			}

		    }

		  //disk 2
		  if (disk == 1)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=1;
			  arm=1;
			  group = 1;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=2;
			  arm=1;
			  group = 0;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }


		  //disk 3
		  if (disk == 2)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=1;
			  arm=2;
			  group = 0;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=1;
			  arm=2;
			  group = 1;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }

		  //disk 4
		  if (disk == 3)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=2;
			  arm=2;
			  group = 1;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=1;
			  arm=3;
			  group = 0;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }


		  //disk 5
		  if (disk == 4)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=2;
			  arm=3;
			  group = 0;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=2;
			  arm=3;
			  group = 1;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }


		  //disk 6
		  if (disk == 5)
		    {

		      if (quad == 3) //this assumes D == 3!
			{
			  continue;//no detector here
			}


		      if (quad == 0) //this assumes A == 0!
			{
			  rdo=1;
			  arm=4;
			  group = 1;
			}


		      if (quad == 1) //this assumes B == 1!
			{
			  rdo=2;
			  arm=4;
			  group = 0;
			}


		      if (quad == 2) //this assumes C == 1!
			{
			  continue;//no detector here
			}

		    }
		}
	      

	      //strip = stripId % StFgtGeom::kNumFgtStripsPerLayer;
	      strip = stripId % kFgtNumStrips;
	      geoId= StFgtGeom::encodeGeoId(disk, quad,layer,strip);

	      Int_t real_apv = group*12 + apv;		
	      electronicID = StFgtGeom::encodeElectronicId(rdo,arm, real_apv,ch);

	      mapping[electronicID]=geoId;
	      
	      if (1){
		cout<<" rdo="<<rdo<<" arm="<<arm<<" apv ="<<apv<<" ch="<<ch<<" electId="<<
		  electronicID<<" disk="<<disk<<" quad="<<quad<<" layer="<<layer<<" strip="<<
		  strip<<" stripID="<<stripId<<" geoID="<<geoId<<endl; 
	      }
 
	    }
	}
    }
}



Int_t StFgtDbFileMaker::searchRStripId_HighPhi(Double_t r)
{

  Int_t ii;
  
  assert(r!=0);
  
  for (ii = 0; ii < 280; ++ii)
    {
      if (r == StFgtGeom::mStrips[ii].ordinate) {
	//cout<<" r="<<r<<" =?  "<<StFgtGeom::mStrips[ii].ordinate<<endl;
	break;
      }
    }
   
  
  return ii;
}

Int_t StFgtDbFileMaker::searchRStripId_LowPhi(Double_t r)
{

  Int_t ii;
  
  assert(r!=0);
  
  //kFgtNumStrips*2 used to be kNumStrips
  for (ii = 400; ii <  (kFgtNumStrips*2); ++ii)
    {
      if (r == StFgtGeom::mStrips[ii].ordinate) {
	//cout<<" r="<<r<<" =?  "<<StFgtGeom::mStrips[ii].ordinate<<endl;
	break;
      }
    }
   
  
  return ii;
}


Int_t StFgtDbFileMaker::searchPhiStripId(Double_t p)
{

  Int_t ii;
  
  assert(p!=-1);

  //kFgtNumStrips*2 used to be kNumStrips before change of consts and put into StFgtConsts.  
  for (ii = 0; ii < (2*kFgtNumStrips); ++ii)
    {
      if (p == StFgtGeom::mStrips[ii].ordinate){
	//cout<<" p="<<p<<" =?  "<<StFgtGeom::mStrips[ii].ordinate<<endl;
	break;
      }
    }
  
  return ii;
}


Int_t StFgtDbFileMaker::getElectId(Int_t rdo, Int_t arm, Int_t apvMod, Int_t channel){

  Int_t eID = -1;
  if ((rdo > 0)&&( rdo < 3)){
    if ((arm >= 0)&&(arm < 6)){
      if ((apvMod >= 0)&&(apvMod < 20)){
	if ((channel >= 0)&&(channel < 128)){
	  
	  eID =  channel + (128 * (apvMod + (20 * (arm + (6 * (rdo - 1))))));
	}
      }
    }
  }

  return eID;
}
