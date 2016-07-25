#include "TString.h"
#include "Riostream.h"
void LecroyMap() {
  //                  io mo*ch
  TString tpcVoltages[2][12][8];
  TString V[24][8];
  for (Int_t io = 0; io < 2; io++) {
    for (Int_t k = 0; k < 96; k++) {
      Int_t channel = k%8;
      Int_t module  = k/8;
      Int_t sec  = 1 + 2*module + channel/4; // sector(module,channel);
      Int_t socket  = channel%4 + 4*io + 1;
      Int_t l    = 8*(sec-1)+socket-1;
      tpcVoltages[io][module][channel] = Form("sector %02i socket %02i",sec,socket);
      V[sec-1][socket-1] = Form("io %i, channel %02i, module %i",io,channel,module);
    }
  }
  for (Int_t sec = 1; sec <= 24; sec++) {
    for (Int_t socket = 1; socket <= 8; socket++) {
      cout << "sec/socket " << sec << " / " << socket << " = " << V[sec-1][socket-1].Data() << endl;
    }
  }
  for (Int_t io = 0; io < 2; io++) {
    for (Int_t module = 0; module < 12; module++) {
      for (Int_t channel = 0; channel < 8; channel++) {
	cout << "io/module/channel " << io << " / " << module << " / " << channel  << " => " << tpcVoltages[io][module][channel].Data() << endl;
      }
    }  
  }
}
