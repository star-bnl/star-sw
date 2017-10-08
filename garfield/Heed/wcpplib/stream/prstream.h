#ifndef PRSTREAM_H
#define PRSTREAM_H
/*
This is the main file which determines the output matters:
default streams and indentation.

There are two default streams in C++: cout and cerr.
In the program we often need to use stream for regular output and
the same or another stream for emergency or exstraordinary cases,
exceptions, error, etc. These logical streams can be realized not only to
default tty, but to files. Perhaps there are many ways to control this,
but the simplest one is the use symbolic stream
notations mcout amnd mcerr (my cout and my cerr) throughout the program,
and to bound them with real streams through trivial macro-driven replacements,
as done below.

The practice shows that whatever advanced debugger and proficient skills
the programmer has in his computer, sooner or later
he will encode the print of all members of each significant
not trivial class of his program in readable and understandable form.
At least it is so in numerical calculations.
Each such printing is usually controlled by a key determining
the level of details. At large level the user expects to see
the output of all structured components of the current object.
Then the initial call of object->print(stream, key)
triggers similar calls of print of components, usually with less
key of details, component->print(stream, key-1).
It is very important to allow the reader of such listing
to distinguish visually the output from main object,
from components of the main object, components of components, etc.
Also it is useful to emphasis the name of classes,
the printing of sequences of similar elements
such as elements of arrays performed in loops. The user may want
to stress any other structures appearing in his classes.
This is possible by inclusion of indentation with which
the objects are printed. There should be some number of blanks
established which should be printed prior to content of
any line printed from any object. Any object should be allowed to
add additional blanks and required to remove the additions at the
end of its output.

It appered that it is not trivial to arrange such system that makes this
and is completely safe from any misuse. Despite of all the power of C++
it appears to be not possible without significant intrusion in internal
functioning of streams. There was some discussion in a news-group which
does not point to any appropriate solution. Therefore here this is done
by means which could be crititized in some respects by the lovers of
object-oriented approach, but it has the pronounced advantages that
it is compartible with any streams, it is convenuent enough for practice,
it really works, and it really exists.

There is a class indentation and the global object of this
class called shortly "indn". This object keeps the current number of blanks
needed to insert in output listing  before each line.
This object is not tied to certain stream. Therefore this current
number will be valid for any stream.

The indentation is invoked if you print
  Imcout<<something     // indentation is invoked
instead of
  mcout<<something      // indentation is not invoked
Also you can use Ifile instead of file.
Also indentation in 2 sequencial lines will be made by
  Imcout<<something_in_1_line<<'\n'<<indn<<something_in_next_line<<'\n';
To change the number of blanks you put
  indn.n+=2;  // or 1, or 4, or what you want.
And don't forget to restore the previous value by
  indn.n-=2;
If you output the composite class with redefined operator<<
and this latter operator uses indentation, particularly it
starts from Ifile<<...,
and you want this class continues the line, for example:
  Ifile<<"name_of_object="<<object;
then you probably don't want to see additional blanks between
"name_of_object=" and the first line of the object itself.
Then you want to prohibit the indentation when printing the first line of
the object. To provide this you can print:
  Ifile<<"name_of_object="<<noindent<<object;
The directive "noindent" turns off the request for indentation
immidiately after it, but switched off later. Thus indentation will be
automatically turned on after when the first file<<indn is met and skipped.
The rest of the object will be printed with correct indentation despite of
"noindent" statement.

In addition, at the bottom of this file a few useful Iprint-like macro are
defined. The idea is to print not only variable, but start from its name
and "=". Thus instead of
  Imcout << "my_favorite_variable=" << my_favorite_variable << '\n';
you can use the shorter
  Iprintn(mcout, my_favorite_variable);

  Iprint - just print a variable without saying << '\n' at the end.
  Iprintn - print a variable and pass to next line.

Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

#include <iostream>

#define USE_DEFAULT_STREAMS
// otherwise everything will be flushed to a file
// If the macro OPEN_LOGFILE_EXPLICITLY (see below) is NOT defined,
// the file name is fixed as "prstream_log.out" (see below).
// Otherwise the file can be opened with any name by lines like those:
//	HelperForMcout::get_ofstream().clear();
//      HelperForMcout::get_ofstream().open("logfile.out");
//      if(!HelperForMcout::get_ofstream())
//      {
//	      cout<<"cannot open file\n";
//      }
//
// If user wants this file to contain all the protocol from the full program,
// this file prstream.h should be included in the main program
// (which is ordinary practice).

#ifdef USE_DEFAULT_STREAMS

//#ifndef mcout
#define mcout std::cout /* change to ordinary default C++ stream */
//#endif
#define mcerr std::cerr

#else

#define OPEN_LOGFILE_EXPLICITLY

#include <fstream>

class HelperForMcout {
  // and also for mcerr. Used for switching them to file
 public:
  static long count;  // will be common for all modules

  static std::ofstream& get_ofstream(void) {
    static std::ofstream prstream_output_file;
    return prstream_output_file;
  }

  HelperForMcout(void) {
#ifndef OPEN_LOGFILE_EXPLICITLY
    if (count++ == 0) {
      get_ofstream().open("prstream_log.out");
    }
#else
    count++;
#endif
  }
  ~HelperForMcout(void) {
    if (--count == 0) {
      get_ofstream().flush();
      get_ofstream().close();
    }
  }
};

namespace {
HelperForMcout __helper_for_mcout;
}  // thanks to
   // unnamed namespace the object __helper_for_mcout will be different for
   // all modules, but the counter will be nevertheless common.

#define mcout HelperForMcout::get_ofstream()
#define mcerr HelperForMcout::get_ofstream()

#endif

namespace Heed {

class indentation {
 public:
  int n;  // current number of blanks to print
  int s_not;
  indentation(void) { n = 0; }
};

extern indentation indn;

inline std::ostream& operator<<(std::ostream& file, indentation& ind) {
  if (ind.s_not == 1)
    ind.s_not = 0;
  else
    for (int n = 0; n < ind.n; n++) file << ' ';
  return file;
}

std::ostream& noindent(std::ostream& f);
std::ostream& yesindent(std::ostream& f);

#define Ifile file << indn
#define Imcout mcout << indn
#define Iprint(file, name) \
  file << indn << #name << "=" << noindent << name << yesindent;
#define Iprintf(file, name)                                        \
  {                                                                \
    file << indn << #name << "=" << noindent << name << yesindent; \
    file.flush();                                                  \
  }
#define Iprintn(file, name) \
  file << indn << #name << "=" << noindent << name << '\n' << yesindent;
#define Iprintnf(file, name)                                               \
  {                                                                        \
    file << indn << #name << "=" << noindent << name << '\n' << yesindent; \
    file.flush();                                                          \
  }
#define Iprintan(file, name, addition)                                        \
  file << indn << #name << "=" << noindent << name << ' ' << addition << '\n' \
       << yesindent;
// addition is convenient as notation of units

#define Iprint2(file, name1, name2)                                           \
  file << indn << #name1 << "=" << noindent << name1 << ", " << #name2 << "=" \
       << noindent << name2 << yesindent;
#define Iprint2n(file, name1, name2)                                          \
  file << indn << #name1 << "=" << noindent << name1 << ", " << #name2 << "=" \
       << noindent << name2 << '\n' << yesindent;
#define Iprint2nf(file, name1, name2)                                    \
  {                                                                      \
    file << indn << #name1 << "=" << noindent << name1 << ", " << #name2 \
         << "=" << noindent << name2 << '\n' << yesindent;               \
    file.flush();                                                        \
  }
#define Iprint3(file, name1, name2, name3)                                    \
  file << indn << #name1 << "=" << noindent << name1 << ", " << #name2 << "=" \
       << noindent << name2 << ", " << #name3 << "=" << noindent << name3     \
       << yesindent;
#define Iprint3n(file, name1, name2, name3)                                   \
  file << indn << #name1 << "=" << noindent << name1 << ", " << #name2 << "=" \
       << noindent << name2 << ", " << #name3 << "=" << noindent << name3     \
       << '\n' << yesindent;
#define Iprint3nf(file, name1, name2, name3)                              \
  {                                                                       \
    file << indn << #name1 << "=" << noindent << name1 << ", " << #name2  \
         << "=" << noindent << name2 << ", " << #name3 << "=" << noindent \
         << name3 << '\n' << yesindent;                                   \
    file.flush();                                                         \
  }
#define Iprint4n(file, name1, name2, name3, name4)                            \
  file << indn << #name1 << "=" << noindent << name1 << ", " << #name2 << "=" \
       << noindent << name2 << ", " << #name3 << "=" << noindent << name3     \
       << ", " << #name4 << "=" << noindent << name4 << '\n' << yesindent;

extern int s_short_output;  // sign which allows to make output shorter
// simultaneously for all classes. Useful for writing "persistence classes"
// by standard <</>> operators.
// If instead of this one tries to use special functions like
// class_name::short_write,
// he finds an obstacle that such functions are absent for inbuilt types.
}

#endif
