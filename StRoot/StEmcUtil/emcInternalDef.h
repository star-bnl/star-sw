/* 
  Copy from pams/emc/inc/emc_def.h for convenience.
*/
#define MAXDET 8

#define BEMC  1         
#define BPRS  2
#define BSMDE 3         
#define BSMDP 4          
	       		
#define EEMC  5         
#define EPRS  6
#define ESMDE 7         
#define ESMDP 8
#if 0
/**********************************************************************************
Detector Number:

BEMC  1   barrel EMC tower    
BPRS  2   barrel EMC pre-shower
BSMDE 3   barrel SMD eta strip
BSMDP 4   barrel SMD phi strip
                            
EEMC  5   endcap EMC tower    
EPRS  6   endcap EMC pre-shower
ESMDE 7   endcap SMD eta strip
ESMDP 8   endcap SMD phi strip        
        
Descriptions:

B*              module    module#, 1-60 at Z>0 and 61-120 at Z<0
BEMC, BPRS      eta       eta tower#, 1 at |eta|=0.0 and 20 at |eta|=1.0
                sub       phi tower# within a module, 1 or 2
BSMDE           eta       eta strip#, 1-75 for narrow strip 76-150 for wide strip
                sub       not valid
BSMDP           eta       eta patch#, 1-10
                sub       phi strip#, 1-15

E*              module    module#, 1-12 at west end and 13-24 at east end(?!)
EEMC, EPRS      eta       eta tower#, 1 at |eta|=1.0 and 12 at |eta|=2.0
                sub       phi tower# within a module, 1-5
ESMDE, ESMDP    eta       Don't know yet
                sub       Don't know yet
                
Ranges Summary:
det     det#    module          eta_bin         sub
BEMC    1       1-120           1-20            1,2
BPRS    2       1-120           1-20            1,2
BSMDE   3       1-120           1-150           not valid
BSMDP   4       1-120           1-10            1-15
EEMC    5       1-24            1-12            1-5 
EPRS    6       1-24            1-12            1-5 
ESMDE   7       1-24            ?               ?  
ESMDP   8       1-24            ?               ?  

See also STAR note #0229 for geometry and numbering scheme
**********************************************************************************/
#endif

