#include "Riostream.h"
#include "Rtypes.h"
static Int_t _debugAsk = 1;
//________________________________________________________________________________
Bool_t Ask() {
  static Bool_t fAsk = kTRUE;
  char symbol;
  if (fAsk){
    std::cout << "ask (enter - next, r - don't ask, q - quit) >";
    do{
      std::cin.get(symbol);
      if (symbol == 'r') {
	_debugAsk = 0;
        fAsk = kFALSE;
      } else if (symbol == 'q') return kTRUE;
    } while (symbol != '\n');
    std::cout << std::endl;
  }
  return kFALSE;
}
