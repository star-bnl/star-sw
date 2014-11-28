#ifndef COMMONS_CONFIG_H

#define COMMONS_CONFIG_H
#define CERNLIB_ATLAS
#define CERNLIB_AGEOLD

#include <commons/machine.h>

#ifdef ATLAS_TYPE
#ifndef CERNLIB_TYPE
#define CERNLIB_TYPE
#endif
#ifndef CERNLIB_ATLAS
#define CERNLIB_ATLAS
#endif
#ifndef CERNLIB_ATLSIM
#define CERNLIB_ATLSIM
#endif
#ifndef CERNLIB_HADRON 
#define CERNLIB_HADRON 
#endif
#ifndef CERNLIB_GCALOR 
#define CERNLIB_GCALOR 
#endif
#ifndef CERNLIB_BSLASH 
#define CERNLIB_BSLASH 
#endif
#ifndef CERNLIB_NONEWL 
#define CERNLIB_NONEWL 
#endif
#ifndef CERNLIB_DZDOC 
#define CERNLIB_DZDOC 
#endif
#ifndef CERNLIB_MOTIF 
#define CERNLIB_MOTIF 
#endif
#ifndef CERNLIB_GLOC
#define CERNLIB_GLOC
#endif
#ifndef CERNLIB_HIGZ 
#define CERNLIB_HIGZ 
#endif
#ifndef CERNLIB_SHL 
#define CERNLIB_SHL 
#endif                    
#ifndef CERNLIB_CG 
#define CERNLIB_CG 
#endif

#endif

#endif /* COMMONS_CONFIG_H */
