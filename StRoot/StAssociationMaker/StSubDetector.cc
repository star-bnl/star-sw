



#include "StSubDetector.hh"
#include "StThreeVector.hh"
#include "StDevice.hh"
#include "StEvent/StTpcHit.hh"
#include "StMcEvent/StMcTpcHit.hh"




//__________________________________



int TpcLocalTransform(StThreeVector<float>& xgl, int& iSector, int& iPadrow, float& xlocal){
    // this STINKS - Hopefully this is a placeholder for when we have a "real" database!
    // This tries to follow the code found in
    // $STAR/pams/tpc/tpg/tpc_global_to_local.F
    
    // try using StThreeVector member functions
    float phi = xgl.phi();
  if (phi<0.0) phi+=6.21318530718;
  iSector =(int) ((phi+0.2617993878)/0.5235987756);  
  if (iSector==12) iSector=0;
  float phi_rot;
  float ff;
  if (xgl.z()>0){
      // Do nothing to the z local.
      iSector=15-iSector;
      if (iSector>12) iSector-=12;
      phi_rot = ((float)iSector) * 0.5235987756;
    ff = -1.0;
  }
  else {
    iSector+=9;
    if (iSector<=12) iSector+=12;
    // Do nothing to the z local.
    phi_rot = ((float)(24-iSector)) * 0.5235987756;
    ff = 1.0;
  }

  // subtract "1" from sector, so we start from zero all the time...
  iSector--;

  

  // don't use the rotation function of StThreeVector, because
  // it looks like the rotation that works is backwards??  Whatever...
  xlocal = ff*(xgl.x()*cos(phi_rot)-xgl.y()*sin(phi_rot));
  float ylocal = xgl.x()*sin(phi_rot)+xgl.y()*cos(phi_rot);

  // now find padrow-- first try padrows 1-8 (0-7 if we start from 0)
  float radmax = 62.4;
  float spacing= 4.8;
  for (iPadrow=0; iPadrow<8; iPadrow++, radmax+=spacing){
      if (ylocal<radmax) { break;  }
  }
  if (iPadrow>7) {           // OK then try rows 9-13 (8-12 if we start from 0)
      radmax = 100.0;
      spacing = 5.2;
      for (iPadrow=8; iPadrow<13; iPadrow++, radmax+=spacing){
	  if (ylocal<radmax) { break; }
      }
  }
  if (iPadrow>12){         // finally, try the outer sector (14-45, or 13-44 starting from 0)
      radmax = 128.2;
      spacing = 2.0;
      for (iPadrow=13; iPadrow<45; iPadrow++, radmax+=spacing){
	  if (ylocal<radmax) { break; }
      }
  }
  
  if (iPadrow>=45) {
      cout << "The Vector is " << xgl << endl;
      cout << "The Sector is " << iSector << endl;
      cout << "The xlocal is " << xlocal << endl;
      cout << "The ylocal is " << ylocal << endl;
      
      cout << "Transformation failed :( " << endl;
    return -1;
  }
  return 0;
}


//__________________________________

//__________________________________
StSubDetector::StSubDetector(const unsigned int nDevices, const unsigned int nRows){
    cout << "StSubDetector::StSubDetector() CONSTRUCTOR ---------------- " << endl;
    for (unsigned int iDevice=0; iDevice<nDevices; iDevice++){
	StDevice* dev = new StDevice(nRows);
	mDevices.push_back(dev);
    };
    mNDevices = nDevices;
}

//__________________________________

StSubDetector::~StSubDetector(){
  for (unsigned int iDevice=0; iDevice<mDevices.size(); iDevice++){
    delete mDevices[iDevice];
  };
}

//__________________________________
void StSubDetector::addHit(const StTpcHit* hit){
  StThreeVector<float> xglbvec = hit->position();
  int iSector;
  int iPadrow;
  float xLocal;
  if (TpcLocalTransform(xglbvec,iSector,iPadrow,xLocal)!=0){
    cout << "Transformation failed!!!"<<endl;
  }
  
  float zGlobal = xglbvec.z();
  mDevices[iSector]->row(iPadrow)->addHit(hit, xLocal, zGlobal);
}

//__________________________________
void StSubDetector::addHit(const StMcTpcHit* hit){
  StThreeVector<float> xglbvec = hit->position();
  int iSector;
  int iPadrow;
  float xLocal;
  if (TpcLocalTransform(xglbvec,iSector,iPadrow,xLocal)!=0){
    cout << "Transformation failed!!!"<<endl;
  }
  
  float zGlobal = xglbvec.z();
  mDevices[iSector]->row(iPadrow)->addHit(hit, xLocal, zGlobal);
}

