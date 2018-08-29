import sys

_isExporting = True
def setExport( flag ):
    global _isExporting
    _isExporting = flag

##
# \module IOHandler
# \author Jason C. Webb
#
# Provides an IOHandler class for attaching output to various files and/or streams
#


class IOHandler:

    def __init__(self,newline=False,indentBy=3):
        self.table = {
            'out' : sys.stdout,
            'err' : sys.stderr
            } # Dictionary
        self.newline=newline
        self.nleft = 0
        self.indentBy=indentBy

    """
    __call__ method accepts text and a unit designator known to the handler.  The
    text will be written to the referenced unit.  Additional units may be added
    using the connect method.
    """
    def __call__(self,text,unit='out',keepCase=False):

        # Wrap text in comments if we are in another language's export block
        if _isExporting == False:
            text = '/* Export block\n'+text+'\n*/'

        # Do not output blank lines
        if text.strip()==';':
            return

        
        # Replacement of fortran-style comments... may fail for split lines
        mytext=''
        text = text + ' '
        for i,c in enumerate(text):

            if c=='!' and not text[i+1]=='=':
                mytext += ';//'
            else:
                mytext += c

                
        text=mytext

        # Replacement of mortran-style comments
        #   TO BE DONE

        

        brackets = { '{':0, '}':0 }
        for c in text:
            try: brackets[c] += 1
            except KeyError: pass

        nright=brackets['}'] # 
        nleft =brackets['{']

        nnet=nleft-nright

        # Shift on close bracket
        #if ( nright ): self.nleft-=1
        if ( nnet < 0 ): self.nleft+=nnet

        left=''
        for i in range(0,self.nleft):
            left += self.indentBy*" "
        
        self.table[unit].write(left+text);        
        if ( self.newline ):
            self.table[unit].write('\n')

        # Shift next time on open bracket
        #if ( nleft ): self.nleft+=1
        if ( nnet > 0 ): self.nleft+=nnet

    """
    Connects a new output file to a unit specifier.
    """
    def connect(self,unit,file):
        self.table[ unit ] = file;

    def unit(self,name):
        try:
            return self.table[name]
        except KeyError:
            return None

    def write(self,text):
        self(text)
        
    def prepend(self,text,unit):
        self.table[unit].write(text,prepend=True)
            
class IOBuffer(IOHandler):
    def __init__(self,file=sys.stdout,newline=False,indentBy=3):
        self.data = []
        self.file = file
        IOHandler.__init__(self,newline,indentBy)
    def write(self,line,prepend=False):
        if prepend: 
            self.data.reverse()
        self.data.append(line)
        if prepend: 
            self.data.reverse()
    def dump(self):
        for line in self.data:
            #self.file.write( str(self) + ': ' + line )
            if len(line.strip()):
                self.file.write( line )
                self.file.write( '\n' )
    def setFile(self,file):
        self.file=file
        
if __name__ == '__main__':
    io = IOHandler();

    io("This is a test of the io handler\n")
    io.connect("test", file=open('test.dat','w'))
    io(unit="test",text="This is a second test of the io handler\n")

    b = IOBuffer()
    io.connect('buf',b)
    io(unit='buf',text='{')
    io(unit='buf',text='I am the very model\n')
    io(unit='buf',text='of a modern major general\n')
    io(unit='buf',text='I have information...\n')
    io(unit='buf',text='}')
    io.unit('buf').dump()
    
    
    
