#!/opt/star/bin/perl -w
#
# $Id: parseXmlConfig.pl,v 1.2 2001/02/16 22:11:38 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: parses an Xml-Configuration description & loads into memory
#
#****************************************************************************
# 
# $Log: parseXmlConfig.pl,v $
# Revision 1.2  2001/02/16 22:11:38  porter
# modified for new low-level table structures
#
# Revision 1.1  2000/04/28 14:08:22  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use XML::Parser;

sub parse_Config {

  my %args = (
              DEBUG => '',
              fileName => '',
              @_,
              );

  if($args{DEBUG}){
  print "In parseXmlConfig with input file=".$args{fileName}, "\n";
}

  $filename = $args{fileName};


%colormap = (
             directory => "navy",
             file => "forestgreen"
             );


$fullpath = '';
my @currentElement = ();
$content='';
$p1 = new XML::Parser(Handlers => {Start => \&handle_start,
                                     End   => \&handle_end,
                                   Char  => \&handle_char});

#  $content='';

  $depth = 0;
  $dec=0;
  $node=0;
  @currentParent=();
  $thisParent=0;
  
$p1->parsefile($filename);

  return ;
}

######################
sub handle_start {
#    local ($expat, $element) = @_;
    my ($expat, $element) = @_;
#    print "c-start ",$element,"\n";
    $element =~ s/[\s\n]*//g;    
    $currentElement = $element;
    if($currentElement eq 'dbNode'){ 
        $depth++;
#        print $node," c-start check ",$element," depth=",$depth,"\n";
      if($dec<1){
           
#        print $node," c-start check ",$element," depth=",$depth,"\n";
              if($#currentParent<$depth){$#currentParent=$depth;}
              $currentParent[$depth]=$node;
     
    }
        $dec=0;
    }
    if($depth>-1){$thisParent=$currentParent[$depth];}

    $endKey=0;
        $startKey=1;
      
}

######################
sub handle_end {
#    local ($expat, $element) = @_;
    my ($expat, $element) = @_;
    $currentElement = $element;
#    print "c-end ",$element,"\n";
    if($currentElement eq 'dbNode'){
        $depth--;
        $dec++;
#        print "Depth=",$depth," ",$node," c-end check ",$element," ",$currentParent[$depth],"\n";
        $thisParent=$currentParent[$depth];
    }
    $endKey=1;
        $startKey=0;
      
}

######################
sub handle_char {
# Here's where the variables are loaded Keyed by "currentElement"
#   local ($expat, $element) = @_;
   my ($expat, $element) = @_;

#       print "Element -1 = ",$element," \n";
   if(!($currentElement eq 'comment')){
#       print "Element 0 = ",$element;
     $element =~ s/[\s\n]*//g;
#       print " Element 1 = ",$element, "\n";
     }
    if ( $element ne '') {
        if ( $currentElement eq 'StarDataBase' ) {
            $dbName=$element;
        }
        if ( $currentElement eq 'dbNode' && $startKey){
            $#nodeName++;
#           print "Current element = ",$#nodeName," ",$element," ",$node," parent= ",$thisParent,"\n";
            $node=$#nodeDepth=$#nodeVersion=$#nodeParent=$#actionWord=$#nodeComment=$#nodeName;
            $nodeParent[$node]=$thisParent;
            $nodeName[$node]=$element;
            $nodeVersion[$node]="default";
            $actionWord[$node]="add";
            $commentFlag=0; 
           $nodeComment[$node]='';

        }

        if ( $currentElement eq 'version' && $startKey){
            $nodeVersion[$node] = $element;
        }
        if ( $currentElement eq 'action' ){
#            print "In action ",$nodeName[$node]," ",$node,"\n";
            if($element eq 'delete'){
#            print "In delete action ",$nodeName[$node]," ",$node,"\n";
                 #--- assume 'add' unless it is really 'delete' ----#
                $actionWord[$node]=$element;
            }
        }
        if ( $currentElement eq 'comment' && !$commentFlag) {
            $commentFlag=1;
            $nodeComment[$node]=$element;
          #  print "ElementComment = ",$element,"\n";
        }
                
  
    }
}

1;
















