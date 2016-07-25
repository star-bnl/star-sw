alias h 'history'
#alias cd        'cd \!*; echo "(wd now: `pwd`)"'
alias up        'cd ..'
alias copy      'cp -iv'
alias h                 'history \!* | more'
alias la                /bin/ls -aCF
alias ll                /bin/ls -laC
alias dy                'cd | cd '
alias ldir              /bin/ls -ld
alias dir               /bin/ls -laF
alias cop                cp
alias del                rm
alias ren                mv
alias cls                clear
alias pwd               'echo $cwd'
alias ty                 more
alias sho               '(set echo;set; alias; setenv; limit; umask; ps -ux)|& more;jobs;set noecho'
#
#alias vt300 "set term = vt300"
#               X terminal
switch (`uname`)
case "Linux":
        alias psf "ps ux"
        breaksw
default:
        alias psf " ps -ef | grep $USER  | grep -v 'ps -ef'"
endsw
alias decdb "/usr/bin/decladebug"
alias quota 'fs listquota'
alias  cvsyf "cvs -z 9 -d fisyak@reines.ucdavis.edu:/afs/cern.ch/cms/cmsim/repository"
alias  cvsd0 "cvs -z 9 -d cvsuser@d0chb.fnal.gov:/cvsroot/d0cvs"
alias print    'lpr -Php5si1_paper'
alias pritrans 'lpr -Php5si1_trans'
alias prilarge 'lpr -Php5si1_large'
alias pricolp  'lpr -Pstarcol_paper'
alias pritras  'lpr -Pstarcol_trans'
alias disp "setenv DISPLAY bergman.star.bnl.gov:0.0; source $HOME/.aliases"
alias dispa "setenv DISPLAY 137.138.203.236:0.0; source $HOME/.aliases"
alias dispw "setenv DISPLAY welles.star.bnl.gov:0.0; source $HOME/.aliases"
alias dispt "setenv DISPLAY startpc5.star.bnl.gov:0.0; source $HOME/.aliases"
alias dispy "setenv DISPLAY yuri.usatlas.bnl.gov:0.0; source $HOME/.aliases"
if ( $?DISPLAY == 0) exit
alias Xterm "xterm -T `uname -n` -n `uname -n` -display $DISPLAY -fn fixed  -bg black -fg white  -sb -sl 4000 -geometry 132x64 &"
alias xw "   xterm -T `uname -n` -n `uname -n` -display $DISPLAY -fn 10x20  -bg black -fg white -cr red -ms blue -sb -sl 4000 -geometry 132x48 &" 
alias star 'setenv GROUP_DIR /afs/rhic/star/group; source $GROUP_DIR/group_env.csh'
alias atlas 'source ~/.atlas'
alias SetupRoot 'source /afs/rhic/usatlas/scripts/ROOT_env.csh'
alias bu "busers all | awk '{if ( "\$4" > 0 ) print}' "
alias art "busers all | grep -e '\W[1-9]' -e 'USER'"
alias Cons 'cons NoSalt=yes'
setenv TERM_OPTIONS "-fn fixed  -bg black -fg white  -sb -sl 4000 -geometry 132x64"

