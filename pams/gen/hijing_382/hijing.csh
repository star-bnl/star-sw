#! /usr/local/bin/tcsh -f
# first argument is a serial number
# for output file: evgen.<serialNumber>.nt 
if (! $?STAR) source ${GROUP_DIR}/.starver DEV
set iset = $1
if (! $iset) set iset = 999

set maxEvts = 100
set energy  = 200.
set beamAZ  = ( 2 1 )
set targAZ  = ( 197 79 ) #gold

set inp = /tmp/$iset.$$.hijev.inp
if (-e $inp) rm $inp
cat <<EOF > $inp
'  ====================================================== '   
'  =====         Hijing Control file                ===== '   
'  ====================================================== ' 
' Events                          '  ${maxEvts}
' Frame/Energy                    '  'CMS'  ${energy}              
' Projectile  type/A/Z            '  'A'  ${beamAZ}              
' Target      type/Z/Z            '  'A'  ${targAZ}         
' Impact parameter min/max (fm)   '  0.   20.                  
' Jet quenching (1=yes/0=no)      '  0                        
' Hard scattering/pt jet          '  0   2.0                   
' Max # jets per nucleon (D=10)   '  10                        
' Set ihpr2(11) and ihpr2(12)     '  1   0
' Set ihpr2(21) and ihpr2(18)     '  1   1
' set B production                '  1.5  
' istat=1 old istat>1 new         '  2                                    
EOF

$STAR_BIN/hijing_382 -run $iset -inp $inp
rm $inp
exit                                    
