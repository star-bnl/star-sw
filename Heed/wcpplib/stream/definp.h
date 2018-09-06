#ifndef DEFINP_H
#define DEFINP_H

#include <vector>
#include <iostream>
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"

namespace Heed {

/* The function below looks for string in cin input stream,
read a value of a corresponding type and return it.
They may be dangerous because they do not require a separator in front of
string.
Therefore the longer name which has the same ending can be erroneously taken
instead of the looked one.
The rewind is not made. Therefore the names should appear in order.
The returning the read value is useful because such a functions can be used
for initialization of global variables.
*/
int definp_int(const std::string& str = std::string());

// New experimental routines

long set_position(const std::string& word, std::istream& istrm, int s_rewind,
                  int s_req_sep);

// Below "endpar" means that this class invelops a few parameters,
// which are typical for functions reading something from stream.
// Previously these parameters appeared at the end of parameter list.
// From this this notation has evolved.
class definp_endpar {
 public:
  std::istream* istrm;  // exclusion from rules.
  // Rationale: actually here should be derivatives from istream.
  // If to declare here derivative from istream and RegPassivePtr,
  // the standard derivatives from istream will not be appropriate instances.
  int s_rewind;
  int s_req_sep;  // recognized separators are '\n' and ' '
  int s_print;
  int s_short;  // if 1, assumes that the names are not output and not
                // searched for at reading.
  definp_endpar()
      : istrm(NULL), s_rewind(0), s_req_sep(0), s_print(0), s_short(0) {}
  definp_endpar(std::istream* fistrm, int fs_rewind, int fs_req_sep,
                int fs_print = 1, int fs_short = 0)
      : istrm(fistrm),
        s_rewind(fs_rewind),
        s_req_sep(fs_req_sep),
        s_print(fs_print),
        s_short(fs_short) {}
};

template <class T>
void definp_any_par(T& inp, const std::string& word, const definp_endpar& dep,
                    int fs_short = 0) {
  mfunnamep("template< class T > definp_any_par(...)");
  check_econd11a(dep.istrm->good(), != 1,
                 "before input of variable named " << word << '\n', mcerr);
  // mcout<<"definp_any_par:\n";
  // Iprint2n(mcout, word, dep.s_short);
  if (fs_short == 0)
    if (dep.s_short == 0)
      set_position(word, *dep.istrm, dep.s_rewind, dep.s_req_sep);
  (*(dep.istrm)) >> inp;
  if (dep.s_print == 1)
    // Iprintn(mcout, dep.s_print);
    Imcout << "definp_any_par: " << word << ' ' << inp << '\n';
  // Iprintn(mcout, inp);
  check_econd11a(dep.istrm->good(), != 1,
                 "after input of variable named "
                     << word << "\nwhose input value is " << inp << '\n',
                 mcerr);
}

//#define DEFINPPAREND definp_stream, s_rewind, s_req_sep, s_print
#define DEFINPPAREND dep
#define DEFINPAP(name) definp_any_par(name, std::string(#name "="), DEFINPPAREND)

/*
With these macros the programmer can reduce the volume of program inputting
many parameters to a minimum. For example, to input variable "varname"
it needs just to say
        DEFINPAP(varname);
provided that the following declarations (with not necessarily these values)
appear somewhere before:
    definp_endpar dep(&std::cin, 0, 0, 0);
    //std::istream& definp_stream = std::cin;
        //int s_rewind = 1;
    //int s_req_sep = 1;
        //int s_print = 1;
        int varname = 0;
*/

}

#endif
