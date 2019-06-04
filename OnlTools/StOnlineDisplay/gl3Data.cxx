// $Id: gl3Data.cxx,v 1.5 2019/06/04 12:37:28 jml Exp $

#include "gl3Data.h"

#include <iostream>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <qmessagebox.h>
#include "RTS/EventTracker/gl3LMVertexFinder.h"

Gl3Data::Gl3Data(QObject* parent, const char* name)
  : QObject(parent, name) {
  datafile = NULL;
  isFileOpen = false;
  strcpy(filename,"");
  transformer = NULL;
  bemcCalibration = NULL;
  eemcCalibration = NULL;
  event = NULL;
  tracker = NULL;
  bfield = -0.5;
  currentTrack = -1;
  mTPCTrackStepSize = 10;
  mTPCTrackStopRadius = 198;
  retrack = false; //true; //false;
  currentBEmcTower = -1;
  currentEEmcTower = -1;
  emcTowerAdcCut = 0;
}

Gl3Data::~Gl3Data() {
  if (evbuff) {
    free(evbuff);
  }
  if (event) {
    delete event;
  }
  if (transformer) {
    delete transformer;
  }
}

void Gl3Data::setFileName(char *name) {
  // just to avoid buffer overflows...
  strncpy(filename,name,511);
  filename[511]=0;
}

void Gl3Data::Init() {
  int status = init();
  if (status < 0) {
    // signal failure to parent...
    emit InitFailed(status);
  } else {
    // signal success to parent...
    emit InitSuccess();
  } 
}

void Gl3Data::ReadNextEvent(daqReader *rdr, char *mem) {

  LOG(NOTE, "gl3Data: ReadNextEvent");
  
  LOG(NOTE, "Read from evpReader: seq=%d token=%d",rdr->seq, rdr->token);
  int ret = event->readFromEvpReader(rdr);
  LOG(NOTE, "Done with evpReader");

  // delete evp;

  // Emits...
  // cout << "event.readL3Data: " << event->readL3Data(l3p) << endl;
  cout << "TrgWord: 0x" << hex << event->getTrgWord() << endl;
  cout << "nHits: " << dec << event->getNHits() << endl;
  cout << "nTracks: " << event->getNTracks() << endl;
  currentTrack = -1;

  emit(BuildNewTpcClusterList(this));
  emit(BuildNewTpcTrackList(this));
  emit(BuildNewBEmcTowerList(this));
  emit(BuildNewEEmcTowerList(this));
  emit(NewNumberOfTracks(event->getNTracks()));
  emit(NewNumberOfHits(event->getNHits()));
  // dummy L3 decision...
  emit(NewL3Decision(TRUE));
  // end emits...

  if(ret < 0) {
    printf("Bad read..\n");
    emit OpenFileFailed(-1, filename);
  }

  else emit OpenFileSuccess();
}

void Gl3Data::CloseFile() {
  closeFile();
}

int Gl3Data::init() {
  float bfield = 0.5;

  // initialize GL3 framework
  maxEventBuffer = 10000000;
  evbuff = (char *)malloc(maxEventBuffer);
  if (!evbuff) {
    return -1;
  }

  if (transformer) {
    cout << "oops" << endl;
  }
  transformer = new l3CoordinateTransformer();
  //transformer->Set_parameters_by_hand(0.581, 200.668, 201.138);
  transformer->Set_parameters_by_hand(0.581, 200.668-12.8, 201.138-12.8);
  transformer->LoadTPCLookupTable("/RTS/conf/L3/map.bin");
  // transformer->Print_parameters();
  
  gl3LMVertexFinder *lmv = new gl3LMVertexFinder();
  lmv->setParameters(10,3,250.0,10.0,3.0,3.0);

  bemcCalibration = new l3EmcCalibration(4800);
  bemcCalibration->loadTextMap("/RTS/conf/L3/emcmap.txt");
  
  eemcCalibration = new l3EmcCalibration(720);
  eemcCalibration->loadTextMap("/RTS/conf/L3/eemcmap.txt");

  LOG("JEFF", "gl3Data...");
  tracker = new FtfSl3(transformer, NULL);
  tracker->setup();
  tracker->para.bField = fabs(bfield);
  tracker->para.bFieldPolarity = (int)(bfield/fabs(bfield));
  tracker->reset();
  event = new gl3Event(transformer,bemcCalibration, eemcCalibration);
  event->setup();
  event->setBField(bfield);
  event->setHitProcessing(2);
  //event->setup(transformer,emcCalibration);
  event->setHitProcessing(2);
  event->setLMVertexFinder(lmv);
  event->setVertexFinderMethod(3);
  //event->init();
  //event->setHitProcessing(2);
  //event->setCoordinateTransformer(transformer);  

  return 0;
}

bool Gl3Data::openFile() {
  if (isFileOpen) {
    closeFile();
  }
  datafile = fopen(filename, "r");
  if (datafile == NULL) {
    return false;
  }
  isFileOpen = true;
  return true;

}

bool Gl3Data::closeFile() {
  if (isFileOpen) {
    fclose(datafile);
    isFileOpen = false;
  }
  return true;
}

void Gl3Data::resetTpcHits() {
  currentHit = -1;
}

int Gl3Data::getNextTpcHit() {
  currentHit++;
  gl3Hit *hit = getCurrentHit();

  //if(hit) {
  //hit->print();
  // }

  if (!hit) {
    return -1;
  }
  return 1;
}

int Gl3Data::getTpcHitPos(float *x, float *y, float *z) {
  gl3Hit *hit = getCurrentHit();
  if (!hit) {
    return -1;
  }
  (*x) = hit->getX();
  (*y) = hit->getY();
  (*z) = hit->getZ();
  return 1;
}

int Gl3Data::getTpcHitColor(float *r, float *g, float *b) {
  gl3Hit *hit = getCurrentHit();
  if (!hit) {
    return -1;
  }
  val2col(hit->getCharge(),0.0,700.0,r,g,b);
  return 1;
}

gl3Hit *Gl3Data::getCurrentHit() {
  if (currentHit >= event->getNHits()) {
    return NULL;
  }
  gl3Hit *hit = event->getHit(currentHit);
  return hit;
}

gl3Track *Gl3Data::getFirstTrack() {
  currentTrack = 0;
  return getCurrentTrack();
}

gl3Track *Gl3Data::getNextTrack() {
  currentTrack++;
  return getCurrentTrack();
}

gl3Track *Gl3Data::getCurrentTrack() {
  if (currentTrack >= event->getNTracks()) {
    return NULL;
  }
  gl3Track *track = event->getTrack(currentTrack);
  return track;
} 

void Gl3Data::resetTpcTracks() {
  currentTrack = -1;
}

void Gl3Data::resetBEmcTower() {
  currentBEmcTower = -1;
}

void Gl3Data::resetEEmcTower() {
  currentEEmcTower = -1;
}

int Gl3Data::getNextBEmcTower(float *eta, float *phi, int type) {
  *eta = 0;
  *phi = 0;
  float adc = -100;
  const int nTowers = 4800;
  do {
    currentBEmcTower++;
    if (currentBEmcTower >= nTowers) {
      return -1;
    }
    if (type == 0) {
      getBEmcTowerAdc(&adc);
    } else {
      if (type == 1) {
	getBEmcTowerEnergy(&adc);
      }
    }
    if ((adc == -100) || (adc > 0)) {
      // debug message... - could be removed later
      //cout << "Reading EMC tower " << currentBEmcTower << " - ";
      *eta = event->emc.getBarrelTower(currentBEmcTower)
	  ->getTowerInfo()->getEta();
      *phi = event->emc.getBarrelTower(currentBEmcTower)
	  ->getTowerInfo()->getPhi();
      //cout << "eta=" << *eta << ", phi=" << *phi;
      //cout << " adc=" << adc << endl;
    }

    // cut is for the moment only on tower adc
    // this need to be changed in the next version
    // let's make sure we have the tower adc and not the energy...
    getBEmcTowerAdc(&adc);
  } while (adc < emcTowerAdcCut);
  return 0;
}

int Gl3Data::getNextEEmcTower(float *eta1, float *eta2,  float *phi, int type) {
  *eta1 = 0;
  *eta2 = 0;
  *phi = 0;
  float adc = -100;
  const int nTowers = 720;
  do {
    currentEEmcTower++;
    if (currentEEmcTower >= nTowers) {
      return -1;
    }
    if (type == 0) {
      getEEmcTowerAdc(&adc);
    } else {
      if (type == 1) {
	getEEmcTowerEnergy(&adc);
      }
    }
    //    if ((adc == -100) || (adc > 0)) {
    // debug message... - could be removed later
    //cout << "Reading EMC tower " << currentEEmcTower << " - ";
    *eta1 = event->emc.getEndcapTower(currentEEmcTower)
	->getTowerInfo()->getEtaMax();
    *eta2 = event->emc.getEndcapTower(currentEEmcTower)
	->getTowerInfo()->getEtaMin();
    *phi = event->emc.getEndcapTower(currentEEmcTower)
	->getTowerInfo()->getPhi();
    //cout << "eta=" << *eta << ", phi=" << *phi;
    //cout << " adc=" << adc << endl;
    //    }

    // cut is for the moment only on tower adc
    // this need to be changed in the next version
    // let's make sure we have the tower adc and not the energy...
    getEEmcTowerAdc(&adc);
  } while (adc < emcTowerAdcCut);
  return 0;
}

int Gl3Data::getBEmcTowerAdc(float *adc) {
  if (currentBEmcTower >= 0) {
    *adc = event->emc.getBarrelTower(currentBEmcTower)->getADC();
    return 0;
  }
  return -1;
}

int Gl3Data::getEEmcTowerAdc(float *adc) {
  if (currentEEmcTower >= 0) {
    *adc = event->emc.getEndcapTower(currentEEmcTower)->getADC();
    return 0;
  }
  return -1;
}

int Gl3Data::getEEmcTowerEnergy(float *energy) {
  if (currentEEmcTower >= 0) {
    *energy = event->emc.getEndcapTower(currentEEmcTower)->getEnergy();
    return 0;
  }
  return -1;
}

int Gl3Data::getBEmcTowerEnergy(float *energy) {
  if (currentBEmcTower >= 0) {
    *energy = event->emc.getBarrelTower(currentBEmcTower)->getEnergy();
    return 0;
  }
  return -1;
}

void Gl3Data::getBEmcTowerColor(float *r, float *g, float *b, int type) {
  float adc = 0;
  if (type == 0) {
    getBEmcTowerAdc(&adc);
    val2col(adc,30,150,r,g,b);
    return;
  }
  if (type == 1) {
    getBEmcTowerEnergy(&adc);
    val2col(adc,0.1,3,r,g,b);
    return;
  }
  val2col(adc,0,1,r,g,b);
}

void Gl3Data::getEEmcTowerColor(float *r, float *g, float *b, int type) {
  float adc = 0;
  if (type == 0) {
    getEEmcTowerAdc(&adc);
    val2col(adc,0,150,r,g,b);
    return;
  }
  if (type == 1) {
    getEEmcTowerEnergy(&adc);
    val2col(adc,0.1,3,r,g,b);
    return;
  }
  val2col(adc,0,1,r,g,b);
}

int Gl3Data::getNextTpcTrack() {
  gl3Track *track;
  while (((track = getNextTrack()) != NULL) &&
	 accept(track) == false) { /* nop */};
  if (!track) {
    return -1;
  }
  return 1;
}

void Gl3Data::resetTpcTrackPos() {
  mCurrentRadius = -1000;
  gl3Track *track = getCurrentTrack();

  if (track) {
    char innerRow = track->innerMostRow;
    float innerRadius = transformer->GetRadialDistanceAtRow((int)innerRow);
    mCurrentRadius = innerRadius - mTPCTrackStepSize;
    char outerRow = track->outerMostRow;
    float outerRadius = transformer->GetRadialDistanceAtRow((int)outerRow);
    mTPCTrackStopRadius = min(outerRadius,(float)198.0);
  }
}

int Gl3Data::getNextTpcTrackPos(float *x, float *y, float *z) {
  if (mCurrentRadius < -100) {
    return -1;
  }
  mCurrentRadius += mTPCTrackStepSize;
  if (mCurrentRadius >= mTPCTrackStopRadius+mTPCTrackStepSize-1) {
    return -2;
  }
  if (mCurrentRadius > mTPCTrackStopRadius) {
    mCurrentRadius = mTPCTrackStopRadius;
  }
  gl3Track *track = getCurrentTrack();
  if (!track) {
    return -3;
  }
  Ftf3DHit pos = track->extraRadius(mCurrentRadius);
  if ((pos.z > 223.0) ||
      (pos.z < -223.0)) {
    return -4;
  }
  if ((pos.x == 0.) &&
      (pos.y == 0.) &&
      (pos.z == 0.)) {
    return -5;
  }
  (*x) = pos.x;
  (*y) = pos.y;
  (*z) = pos.z;
  return 1;
}

void Gl3Data::getTrackColor(float *r, float *g, float *b) {
  float dedx = 0;
  gl3Track *track = getCurrentTrack();
  if (track) {
    dedx = track->dedx;
  }
  val2col(dedx,0.0,6.0e-06,r,g,b);
}

void Gl3Data::val2col(float val, float min, float max, 
		    float *newR, float *newG, float *newB)
{
  float R=0.0, G=0.0, B=0.0;
  float colval=0;

  //RCGYB
  if(val<=max)
  {
   if(max>0 || val<min)
   {
    colval=(val-min)/max;
    float colvaltimes4=colval*4.00;
    if(colval<0.25)
    {
     //cyan
     B=G=colvaltimes4;
     //blue
     B+=1.00-colvaltimes4;
    }
    else
    {
     if(colval<0.5)
     {
      //cyan
      B=G=1.00-(colvaltimes4-1.00);
      //green
      G+=(colvaltimes4-1.00);
     }
     else
     {
      if(colval<0.75)
      {
       //yellow       
       G=R=(colvaltimes4-2.00);
       //green
       G+=1.00-(colvaltimes4-2.00);
      }
      else
      {
       //yellow
       G=R=1.00-(colvaltimes4-3.00);
       //red
       R+=(colvaltimes4-3.00);
      }
     }
    }
   }
   else
   {
    // if max<=0 OR val<min chose white
    R=1.0; G=1.0; B=1.0;
   }
  }
  else
  {
   // if val>max chose RED
   R=1.0;
  }
 
  //printf("cv:%f  R:%f G:%f B:%f \n",colval, R,G,B);
  
  /*
  use only with RGB
  //printf("M:%f Y:%f C:%f \n",R+G,G+B,R+B);
  // M,Y,C
  //glColor3f(R+G, G+B, R+B);
  //
  */
  
  //RGB
  //glColor3f(R, G, B);
  (*newR) = R;
  (*newG) = G;
  (*newB) = B;

}

void Gl3Data::retrackEvent() {
  /* retracking not implemented... */
  /* we need it for cluster info */
  int maxBytes = 9800000;
  char *buffer = new char[maxBytes]; 
  struct L3_P *oldL3p = (struct L3_P *)evbuff;

  L3_P *gl3Header = (L3_P *) buffer;
  int nBytesHeader = sizeof(L3_P);
  char *trackDataPointer = buffer + nBytesHeader;
  memset(buffer,0,nBytesHeader * sizeof(char));
  char *endTrackBuffer = buffer + maxBytes;

  memcpy(gl3Header->bh.bank_type,CHAR_L3_P, 8);
  gl3Header->bh.bank_id = 1;
  gl3Header->bh.format_ver = DAQ_RAW_FORMAT_VERSION;
  gl3Header->bh.byte_order = DAQ_RAW_FORMAT_ORDER;
  gl3Header->bh.format_number = oldL3p->bh.format_number;
  gl3Header->bh.token = 1;
  gl3Header->bh.w9 = DAQ_RAW_FORMAT_WORD9;
  gl3Header->bh.crc = 0;      //don't know yet....

  const int firstSector = 1;
  const int lastSector = 24;

  for (int i = firstSector-1; i < lastSector; i = i+1) {
    struct L3_P *l3p = (struct L3_P *)evbuff;
    if (l3p->sector[i].len) {
      L3_SECP *l3secp = 
	(struct L3_SECP *) ((char *) l3p + l3p->sector[i].off*4);
      trackUnit(i,l3secp,&trackDataPointer,&endTrackBuffer,gl3Header);
    }
  }

  /* event is now retracked, but we need to copy also trg and emc data... */

  // copy trigger data...
  if (gl3Header->bh.format_number >= 5) {
    char *oldTrgData = (((char *) oldL3p) + oldL3p->trig.off*4);
    int oldTrgSize = oldL3p->trig.len;
    
    
    gl3Header->trig.off = (trackDataPointer - (char *)gl3Header)/4;
    gl3Header->trig.len = oldTrgSize;
    if (oldTrgSize > 0) {
      memcpy(trackDataPointer,oldTrgData,oldTrgSize*4);
      trackDataPointer += oldTrgSize*4;
    }
  }
  
  // copy emc data...
  if (gl3Header->bh.format_number >= 7) {
    char *oldEmcData;
    int oldEmcSize;
    for (int i = 0; i < 6; i++) {
      oldEmcData = (((char *) oldL3p) + oldL3p->emc[i].off*4);
      oldEmcSize = oldL3p->emc[i].len;
      gl3Header->emc[i].off = (trackDataPointer - (char *)gl3Header)/4;
      if (oldEmcSize > 0) {
	memcpy(trackDataPointer,oldEmcData,oldEmcSize*4);
	trackDataPointer += oldEmcSize*4;
      }
    }
  }

  // and now set everything else to null
  gl3Header->summary_data.len = 0;
  gl3Header->summary_data.off = 0;
  for (int i = 0; i < 5; i++) {
    gl3Header->svt[i].len = 0;
    gl3Header->svt[i].off = 0;
  }
  for (int i = 0; i < 2; i++) {
    gl3Header->ftpc[i].len = 0;
    gl3Header->ftpc[i].off = 0;
  }
  gl3Header->reserved1.len = 0;
  gl3Header->reserved1.off = 0;


  delete evbuff;
  maxEventBuffer = maxBytes;
  evbuff = buffer;
}

int Gl3Data::trackUnit(int iSector,
		       L3_SECP *l3secp,
		       char **trackDataPointer,
		       char **endTrackBuffer,
		       L3_P *gl3Header) {
  if (l3secp->clusterp.len) {
    L3_SECP *sectorHeader =  (L3_SECP *) (*trackDataPointer);
    (*trackDataPointer) += sizeof(L3_SECP);
    memcpy (sectorHeader->bh.bank_type, CHAR_L3_SECP, 8);
    sectorHeader->bh.length = sizeof (struct L3_SECP) / 4;
    sectorHeader->bh.bank_id = iSector + 1;
    sectorHeader->bh.format_ver = DAQ_RAW_FORMAT_VERSION;
    sectorHeader->bh.byte_order = DAQ_RAW_FORMAT_ORDER;
    sectorHeader->bh.format_number = 1;
    sectorHeader->bh.token = 999;
    sectorHeader->bh.w9 = DAQ_RAW_FORMAT_WORD9;
    sectorHeader->bh.crc = 0; //don't know yet....

    struct TPCSECLP *tpcseclp =
      (struct TPCSECLP *) ((char *) l3secp + l3secp->clusterp.off * 4);
    tracker->readSector((bankHeader*)tpcseclp);
    tracker->processSector();
    int nBytes = tracker->fillTracks(l3secp->clusterp.len * 4,
				     (*trackDataPointer), 1);
    sectorHeader->trackp.off = ((*trackDataPointer) - 
				(char *) sectorHeader) / 4;
    sectorHeader->trackp.len = nBytes / 4;
    (*trackDataPointer) += nBytes;

    nBytes = tracker->fillHits ((*endTrackBuffer) - (*trackDataPointer),
			       (*trackDataPointer), 1);
    sectorHeader->sl3clusterp.off =
      ((*trackDataPointer) - (char *) sectorHeader) / 4;
    sectorHeader->sl3clusterp.len = nBytes / 4;
    (*trackDataPointer) += nBytes;

    gl3Header->bh.token = 1;
    gl3Header->sector[iSector].len =
      ((*trackDataPointer) - (char *) (sectorHeader)) / 4;
    gl3Header->sector[iSector].off = 
      ((char *) (sectorHeader) - (char *)gl3Header) / 4;
  }
  return 0;
}


void Gl3Data::setBField(float field) {
  bfield = field;
  if (event) {
    event->setBField(bfield);
  }
  if (tracker) {
    tracker->para.bField = bfield;
    tracker->para.bFieldPolarity = (int)(bfield/fabs(bfield));
    tracker->reset();
  }
  if (isFileOpen) {
    emit BuildNewTpcTrackList(this);
  }
}

void Gl3Data::setEmcTowerAdcCut(float adc) {
  emcTowerAdcCut = adc; 
  if (isFileOpen) {
    emit BuildNewBEmcTowerList(this);
    emit BuildNewEEmcTowerList(this);
  } 
}
