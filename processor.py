"""
Top level cates.

macroname, after backslash
group, in {}
hash, use hashtable
"""

class Token:
    """Class for token."""

    def __init__(self, text, cat, start=-1, end=-1):
        """Set text and cataloge."""
        self.text = text
        self.cat = cat
        self.expandText = ''
        self.start = 0  # start position in document
        self.end = 0  # end position in document
    
    def __eq__(self, other):
        return (self.text == other.text) and (self.cat == other.cat)

    def __str__(self):
        """Print."""
        return "Token('" + self.text + "', " + self.cat + ", " + self.expandText + ")"
    
    def copy(self):
        nt = Token(None, None)
        nt.text = str(self.text)
        nt.cat = str(self.cat)
        nt.expandText = str(self.expandText)
        nt.start = self.start
        nt.end = self.end
        return nt

class Macro:
    """Class for macros."""

    def __init__(self, name, body, nneg=-1, npos=-1):
        self.name = name
        self.nneg = nneg
        self.npos = npos
        self.body = body
        if nneg < 0:
            self.detect_nneg()
        if npos < 0:
            self.detect_npos()

    def __eq__(self, other):
        return self.name == other.name
    
    def __str__(self) -> str:
        res = "Macro("
        res += self.name+', '
        res += self.body+')'
        return res
    
    def detect_npos(self):
        npos = 0
        i = 1
        if len(self.body) == 0:
            self.npos = npos
            return None
        while True:
            placer = '#{0:d}'.format(i)
            if placer in self.body:
                npos += 1
                i += 1
            else:
                break
        self.npos = npos
    
    def detect_nneg(self):
        i = 1
        nneg = 0
        if len(self.body) == 0:
            self.nneg = nneg
            return None
        while True:
            placer = '#-{0:d}'.format(i)
            if placer in self.body:
                nneg += 1
                i += 1
            else:
                break
        self.nneg = nneg

    def expand(self, negargs, posargs):
        nneg, npos = len(negargs), len(posargs)
        if (nneg == self.nneg) and (npos == self.npos):
            res = self.body[:]
            if len(res) == 0:
                return True, res
            placer = '#0'
            if placer in res:
                res = res.replace(placer, self.name)
            for i in range(1, nneg+1):
                placer = '#-{0:d}'.format(i)
                res = res.replace(placer, negargs[i-1])
            for i in range(1, npos+1):
                placer = '#{0:d}'.format(i)
                res = res.replace(placer, posargs[i-1])
            return True, res
        else:
            return False, "not enough args"

class Processor:
    """Class for processor."""

    globalMacros = {
        '@callpy': Macro('@callpy', '', npos=1),
        '@endcallpy': Macro('@endcallpy', ''),
        '@hash': Macro('@hash', '\\@hash{#1}', npos=1),
        '@newMacro': Macro('@newMacro', '', nneg=0, npos=2),
    }
    tokens = {
        'letter':list('abcdefghijklmnopqrstuvwxyz'
                    +'ABCDEFGHIJKLMNOPQRSTUVWXYZ@'),
        'groupstart': ['{'],
        'groupend': ['}'],
        'leader': ['\\'],
        'textend': [chr(0)],
        'space' : [' ', '\t'],
        'nl' : ['\n', '\r'],
    }

    def __init__(self, text, scripts=None, verbose=0):
        """Init processer."""
        if ord(text[-1]) == 0:
            self.input = text
        else:
            self.input = text + chr(0)
        self.text = str(self.input)
        self._verbose = verbose
        self.tokenList = []
        self.scripts = scripts
        self.n = 0
        self.macros = {k: v for k, v in Processor.globalMacros.items()}
        self._maxMacroLength = 0
        for key in self.macros.keys():
            self._updateMaxMacroLength(key)
        self._maxTokenLength = 1
        self._updateTokenLength()
        self._lastMacroIndex = -1
        self.result = ''
        self.hashTable = {}
        self.invalidArgCat=['textend', 'space', 'leader', 'nl']

    def _updateMaxMacroLength(self, macroName):
        """Update when new macro defined."""
        nl = len(macroName)
        if nl > self._maxMacroLength:
            self._maxTokenLength = nl
    
    def _updateTokenLength(self):
        self._maxTokenLength = 1
        for key in Processor.tokens.keys():
            for t in Processor.tokens[key]:
                nl = len(t)
                if nl > self._maxTokenLength:
                    self._maxTokenLength = nl
    
    def process(self):
        macro = None
        nneg, npos = -1, -1
        negstart, posend = -1, -1
        negargs, posargs = [], []
        while True:
            if self._verbose == 1:
                print('=========')
                print(self.text)
                print('++++++')
                print(self.hashTable)
                print(self._lastMacroIndex)
            if self._lastMacroIndex > -1:
                success, finished, message, posend = self._checkpos(macro.name, npos)
                if not success:
                    raise ValueError(message)
                if not finished:
                    shouldContinue = self._scaner()
                    if not shouldContinue:
                        raise ValueError('macro \\'+macro.name+' need '+str(npos)+' positive arguments')
                    continue
                negargs, posargs = self._macroStack(negstart, posend)
                # if macro.name == '@newMacro':
                #     print('negargs ', negargs)
                #     print('posargs', posargs)
                #     exit(0)
                success, ntoken = self.macroExpander(
                    macro, 
                    self.tokenList[self._lastMacroIndex], 
                    negargs, posargs)
                if not success:
                    raise ValueError(ntoken.expandText)
                del self.tokenList[negstart:posend+1]
                if len(ntoken.expandText) > 0:
                    self.tokenList.append(ntoken)
                buffer = self.combiner()
                if buffer != self.text[:self.n]:
                    self.text = buffer + self.text[self.n:]
                    self.tokenList = []
                    self.n = 0
                self._lastMacroIndex = -1
                npos, nneg = -1, -1
                negstart, posend = -1, -1
                negargs, posargs = [], []
                macro = None
                continue
            shouldContinue = self._scaner()
            if not shouldContinue:
                break
            lastToken = self.tokenList[-1]
            if lastToken.cat == 'macroname':
                # print(lastToken)
                self._lastMacroIndex = len(self.tokenList)-1
                # print(self._lastMacroIndex)
                macro = self.macros[lastToken.text]
                nneg, npos = macro.nneg, macro.npos
                success, message, negstart = self._checkneg(macro.name, nneg)
                if not success:
                    raise ValueError(message)
                continue
        self.result = self.combiner(asout=True)
                
    def _checkneg(self, name, nneg):
        if nneg == 0:
            return True, None, self._lastMacroIndex
        if nneg > len(self.tokenList)-1:
            return False, 'macro \\'+name+' need '+str(nneg)+' negative arguments', -1
        i = 1
        canuse, index = 0, 0
        while canuse < nneg:
            if i > self._lastMacroIndex:
                break
            index = self._lastMacroIndex-i
            if self.tokenList[index].cat not in self.invalidArgCat:
                canuse += 1
            i += 1
        if canuse == nneg:
            return True, None, index
        else:
            return False, 'macro \\'+name+' need '+str(nneg)+' negative arguments', -1
    
    def _checkpos(self, name, npos):
        if npos == 0:
            return True, True, None, self._lastMacroIndex
        nafter = len(self.tokenList)-1-self._lastMacroIndex
        if (nafter < npos) and (self.tokenList[-1].cat == 'textend'):
            return False, False, 'macro \\'+name+' need '+str(npos)+' positive arguments', -1
        i = 1
        canuse, index = 0, 0
        while canuse < npos:
            index = self._lastMacroIndex+i
            if index > len(self.tokenList)-1:
                break
            if self.tokenList[index].cat not in self.invalidArgCat:
                canuse += 1
            i += 1
        if canuse != npos:
            if self.tokenList[-1].cat == 'textend':
                return False, False, 'macro \\'+name+' need '+str(npos)+' positive arguments', -1
            return True, False, None, -1
        return True, True, None, index

    def _macroStack(self, negstart, posend):
        negargs = list()
        posargs = list()
        if negstart == posend:
            return negargs, posargs
        i = 1
        index = self._lastMacroIndex
        while index > negstart:
            index = self._lastMacroIndex-i
            if self.tokenList[index].cat not in self.invalidArgCat:
                negargs.append(self.tokenList[index].text)
            i += 1
        i = 1
        index = self._lastMacroIndex
        while index < posend:
            index = self._lastMacroIndex+i
            if self.tokenList[index].cat not in self.invalidArgCat:
                posargs.append(self.tokenList[index].text)
            i += 1
        return negargs, posargs

    def combiner(self, asout=False):
        res = ''
        for t in self.tokenList:
            if t.cat == 'macroname':
                res += t.expandText
                continue
            if t.cat == 'hash':
                if not asout:
                    res += t.expandText
                else:
                    hashcode = int(t.expandText[7:-1])
                    res += self.hashTable[hashcode]
                continue
            if t.cat == 'textend':
                if not asout:
                    res += chr(0)
                break
            res += t.text
        return res

    def macroExpander(self, macro, token, negargs, posargs):
        macroname = macro.name
        ntoken = token.copy()
        needint, success, res = self.intInterprater(macroname, ntoken, negargs, posargs)
        if needint:
            ntoken.expandText = res
            return success, ntoken
        success, res = macro.expand(negargs, posargs)
        ntoken.expandText = res
        return success, ntoken

    def callpy(self, funcname, arg):
        func = None
        interactive = False
        # print('funcname', funcname)
        if funcname[0] == '@':
            interactive = True
            funcname = funcname[1:]
        # print('funcname', funcname)
        for key in self.scripts.keys():
            try:
                func = getattr(self.scripts[key], funcname)
                break
            except AttributeError:
                continue
        if func is None:
            return False, None
        try:
            if interactive:
                expanding = func(self, arg)
            else:
                expanding = func(arg)
        except Exception as e:
            return False, str(e)
        if expanding is None:
            return True, ''
        else:
            return True, expanding
    
    def addMacro(self, macroname, body, nneg=-1, npos=-1):
        if macroname[0] == '\\':
            macroname = macroname[1:]
        if macroname in self.macros.keys():
            return False, 'Macro \\' + macroname + 'already defined'
        for c in macroname:
            if c not in Processor.tokens['letter']:
                return False, "invalid character '"+c+"' in macro name"
        self.macros[macroname] = Macro(macroname, body, nneg=nneg, npos=npos)
        self._updateMaxMacroLength(macroname)
        return True, ''

    def removeMacro(self, macroname):
        if '@' in macroname:
            return False, "Macro \\"+macroname+" can not be removed"
        if macroname not in self.macros.keys():
            return False, "Macro \\"+macroname+" not defined"
        del self.macros[macroname]
        return True, None

    def _scaner(self):
        shouldContinue, ntoken, nn = self.getNextToken()
        if not shouldContinue:
            return False
        if ntoken.cat == 'leader':
            success, macroname, nn = self.stackWhileCat('letter')
            if success:
                if macroname not in self.macros.keys():
                    raise ValueError('macro \\' + macroname + ' not defined!')
                del self.tokenList[-1]
                self.n = nn
                ntoken = Token(macroname, 'macroname')
                self.tokenList.append(ntoken)
                return True
            else:
                raise ValueError('can not get macro name')
        if ntoken.cat == 'groupstart':
            success, groupedText, nn = self.stackUntilLevelByCat()
            if not success:
                raise ValueError('group not ended')
            del self.tokenList[-1]
            ntoken = Token(groupedText, 'group')
            self.tokenList.append(ntoken)
            self.n = nn
            return True
        return True

    def intInterprater(self, macroname, token, negargs, posargs):
        if macroname == '@callpy':
            success, arg, nn = self.stackUntilText('\\@endcallpy')
            if not success:
                return True, False, '\\@callpy should end with \\@endcallpy'
            self.n = nn
            funcname = posargs[0]
            success, expanding = self.callpy(funcname, arg)
            if not success:
                if expanding is None:
                    return True, False, "can not call function '"+funcname+"'"
                return True, False, "error message from script: "+expanding
            if isinstance(expanding, str):
                return True, True, expanding
            if isinstance(expanding, tuple):
                hashcode, variable, place = expanding
                if ((not isinstance(hashcode, int)) or 
                    ((not isinstance(variable, str)) 
                    and (variable is not None)) or
                    (not isinstance(place, bool))):
                    return True, False, 'function should return a str or tuple[int, str, bool]'
                if hashcode in self.hashTable.keys():
                    return True, False, 'hash code '+str(hashcode)+' is protected'
                if hashcode not in self.hashTable.keys():
                    self.hashTable[hashcode] = variable
                res = variable
                if not place:
                    res = '\\@hash{'+str(hashcode)+"}"
                return True, True, res
        elif macroname == '@endcallpy':
            return True, False, '\\@endcallpy should use with \\@callpy'
        elif macroname == '@hash':
            try:
                hashcode = int(posargs[0])
            except:
                return True, False, "\\@hash should take integer number as argument"
            if ((hashcode in self.hashTable.keys()) or
                (hashcode in self.hashTable.keys())):
                token.cat = 'hash'
                res = '\\@hash{' + posargs[0] + '}'
                return True, True, res
            else:
                return True, False, 'hash code '+posargs[0]+' not in table'
        elif macroname == '@newMacro':
            name, body = str(posargs[0]), str(posargs[1])
            success, res = self.addMacro(name, body)
            return True, success, res
        else:
            return False, None, None

    def getNextToken(self, n=-1, changeState=True):
        if n < 0:
            n = self.n
        tl = self._maxTokenLength
        while tl > 0:
            c = self.text[n:n+tl]
            for key in Processor.tokens.keys():
                if c in Processor.tokens[key]:
                    if key == 'textend':
                        return False, Token(c, key), len(self.text)-1
                    ntoken = Token(c, key)
                    n += tl
                    if changeState:
                        self.tokenList.append(ntoken)
                        self.n = n
                    return True, ntoken, n
            tl -= 1
        c = self.text[n]
        ntoken = Token(c, 'others')
        if changeState:
            self.tokenList.append(ntoken)
            self.n = n+1
        return True, ntoken, n+1

    def stackUntilCat(self, tokencat, n=-1):
        """
        Stack text until arrive a given catalogue of token.

        Parameters:
        tokencat: string, catalogue of token to stop.
        n: int, optional, start position in self.text. 
            If negative, use self.n
        Return:
        string, stacked text.
        """
        if n < 0:
            n = self.n
        res = ''
        while True:
            shouldContinue, token, n = self.getNextToken(n=n, changeState=False)
            if not shouldContinue:
                return False, res, n-1
            if token.cat == tokencat:
                return True, res, n-1
            res += token.text

    def stackWhileCat(self, tokencat, n=-1):
        """
        Stack text while a given catalogue of token.

        Parameters:
        tokencat: string, catalogue of token to continue.
        n: int, optional, start position in self.text. 
            If negative, use self.n
        Return:
        string, stacked text.
        """
        if n < 0:
            n = self.n
        res = ''
        while True:
            _, token, n = self.getNextToken(n=n, changeState=False)
            if token.cat != tokencat:
                return True, res, n-1
            res += token.text
    
    def stackUntilLevelByCat(self,
                        inc='groupstart', dec='groupend',
                        endLevel=0, startLevel=1, n=-1):
        if n < 0:
            n = self.n
        res = ''
        level = startLevel
        while True:
            shouldContinue, token, n = self.getNextToken(n=n, changeState=False)
            if not shouldContinue:
                if level == endLevel:
                    return True, res, n
                else:
                    return False, None, n
            if token.cat == inc:
                level += 1
            if token.cat == dec:
                level -= 1
            if level != endLevel:
                res += token.text
            else:
                return True, res, n

    def stackUntilText(self, stopText, n=-1):
        """Stack text until text."""
        if n < 0:
            n = self.n
        res = ''
        tl = len(stopText)
        maxl = len(self.text)
        while True:
            if n+tl >= maxl:
                return False, res, maxl
            ntext = self.text[n:n+tl]
            if ntext == stopText:
                return True, res, n+tl
            res += self.text[n]
            n += 1


if __name__ == '__main__':
    file = open('core.tlpp', 'r')
    text = file.read()
    print(text)
    print('=============')
    coremodel = SCRIPT = __import__('TLPPcore')
    scripts = {'core': coremodel}
    processer = Processor(text, scripts=scripts, verbose=0)
    processer.process()
    print('++++++++++++')
    print(processer.result)
    # for t in processer.tokenList:
    #     print(t)
    exit(0)
    file = open('./test.txt', 'r')
    test = file.read()
    file.close()
    print('++++ input ++++')
    print(test)
    print('++++ end input ++++')
    test += chr(0)
    processer = Processor(test, verbose=1)
    isexpand = processer.process()
    print('isexpand =', isexpand)
    tokendisp = [str(t) for t in processer.tokenList]
    print('++++ result tokens ++++')
    for d in tokendisp:
        print(d)
    print('++++ end result tokens ++++')
    print('++++ exist macros ++++')
    print(processer.macros)
    print('++++ end exist macros ++++')
    print('++++ final result ++++')
    print(processer.result)
    print('++++ end final result ++++')
