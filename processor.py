"""
3 cates.

macroname, after backslash
group, in {}
space, whitespace ' ', while not considered as macro arguments
others, all others
textend, end for whole text
quote, quotation
"""


class Token:
    """Class for token."""

    def __init__(self, text, cat, start, end):
        """Set text and cataloge."""
        self.text = text
        self.cat = cat
        self.expandText = ''
        self.start = 0  # start position in document
        self.end = 0  # end position in document

    def __str__(self):
        """Print."""
        return "Token('" + self.text + "', " + self.cat + ", " + self.expandText + ")"


class Processor:
    """Class for processor."""

    globalMacros = {
        # name : (number of args befor, number of args after,
            #         body(None if builtin),
            #         isbuiltin)
            'newMacro': (0, 2, None, True),
            'quote': (0, 0, None, True),
            'endquote': (0, 0, None, True),
            'quotation': (0, 1, None, True),
            'nl': (0, 0, None, True),
            'newLineCharOn': (0, 0, None, True),
            'newLineCharOff': (0, 0, None, True),
            'renewMacro': (0, 2, None, True),
            'include': (0, 1, None, True),
            'input': (0, 1, None, True),
            'ignore': (0, 0, None, True),
            'endignore': (0, 0, None, True),
            'callpy': (0, 1, None, True),
            'pyarg': (0, 0, None, True),
            'endpyarg': (0, 0, None, True),
            'dnl': (0, 0, None, True)
    }
    def __init__(self, text, exTokens=None, ignorLineBreak=False, 
                verbose=0, script=None):
        """Init processer."""
        if ord(text[-1]) == 0:
            self.text = text
        else:
            self.text = text + chr(0)
        self.verbose = verbose
        if exTokens is None:
            self.tokens = []
        else:
            self.tokens = exTokens
        self.ignorLineBreak = ignorLineBreak
        self.script = script
        self.n = 0
        self.macros = {k: v for k, v in Processor.globalMacros.items()}
        self.maxMacroLength = 0
        for key in self.macros.keys():
            self.updateMaxMacroLength(key)
        self.preargsBuffer = []
        self.afterargsBuffer = []
        self.preargsChecked = False
        self.lastMacroIndex = -1
        self.preremoves = 0
        self.afterremoves = 0
        self.result = ''
        self.nlhash = hash('\n')
        self.quotationTable = {}
        self.quotationTable[self.nlhash] = '\n'
        self.exthashcode = []

    def scan(self):
        """Scan text and tokeniz text."""
        # if the text has been expanded at least once
        isexpand = False
        lastmacro = None
        isfinished = False
        buffer = ''
        while True:
            if self.verbose == 1:
                print('---- start ----')
                print('text')
                print(self.text)
                print('n = '+str(self.n))
                print('len of text = '+str(len(self.text)))
                print('---- end ----')
            shouldContinue = self.getNextToken(
                ignoreLineBreak=self.ignorLineBreak)
            if not shouldContinue:
                # when detected the EOF, stop
                break
            if len(self.tokens) == 0:
                # when start with new line
                continue
            lastToken = self.tokens[-1]
            ntokens = len(self.tokens)
            if self.lastMacroIndex == -1:
                # start deal with a macro
                if lastToken.cat == 'macroname':
                    self.lastMacroIndex = ntokens - 1
                    lastmacro = self.tokens[-1]
                    isfinished = self.macroChecker(lastToken,
                                                   self.lastMacroIndex)
            else:
                isfinished = self.macroChecker(lastmacro,
                                               self.lastMacroIndex)
            if isfinished:
                hasExpand = self.expandMacro(
                        lastmacro,
                        self.preargsBuffer,
                        self.afterargsBuffer)
                if not isexpand:
                    isexpand = hasExpand
                # ++++ remove tokens, reset state ++++
                self.lastMacroIndex = -1
                isfinished = False
                if self.afterremoves != 0:
                    del self.tokens[-self.afterremoves:]
                if self.preremoves != 0:
                    del self.tokens[-self.preremoves-1:-1]
                if len(self.tokens[-1].expandText) == 0:
                    del self.tokens[-1]
                    hasExpand = False
                self.afterargsBuffer = []
                self.preargsBuffer = []
                self.preargsChecked = False
                self.afterremoves = 0
                self.preremoves = 0
                # ++++ end remove tokens ++++
                # ++++ expand recursive ++
                if hasExpand:
                    buffer = self.combiner(asout=False)
                    self.tokens = []
                    self.text = buffer + self.text[self.n:]
                    self.n = 0
                    isexpand = False
                # ++++ end expand recursive ++++
        # recombine to text
        self.result = self.combiner(asout=True)
        return isexpand

    def combiner(self, asout=False):
        """Combine tokens to text."""
        buffer = ''
        for token in self.tokens:
            if token.cat == 'others':
                buffer += token.text
                continue
            if token.cat == 'group':
                buffer += token.text
                continue
            if token.cat == 'macroname':
                buffer += token.expandText
                continue
            if token.cat == 'quote':
                if asout:
                    textHash = token.expandText[11:-1]
                    textHash = int(textHash)
                    buffer += self.quotationTable[textHash]
                else:
                    buffer += token.expandText
                continue
            if token.cat == 'space':
                buffer += ' '
                continue
        return buffer

    def macroChecker(self, macro, tokenIndex):
        """
        Check macro.

        Return is finished or need more tokens.
        Given how many tokens need to remove from
        token list.
        """
        ntokens = len(self.tokens)
        name = macro.text
        nargb, narga, body, isbuiltin = self.macros[name]
        # check args befor the macro call
        if not self.preargsChecked:
            if nargb != 0:
                i, n = 1, 1
                while n <= nargb:
                    nn = tokenIndex - i
                    if nn < 0:
                        raise ValueError("'" + name +
                                         "' did not get enough pre-args")
                    if self.tokens[nn].cat == 'space':
                        i += 1
                        continue
                    else:
                        self.preargsBuffer.append(self.tokens[nn])
                        n += 1
                        i += 1
                self.preremoves = i - 1
            else:
                self.preremoves = 0
            self.preargsChecked = True
        # check args after the macro call
        if narga != 0:
            i, n = 1, 1
            while n <= narga:
                nn = tokenIndex + i
                if nn > ntokens - 1:
                    # False for need to get more tokens
                    return False
                if self.tokens[nn].cat == 'space':
                    i += 1
                    continue
                elif self.tokens[nn].cat == 'textend':
                    raise ValueError("'" + name +
                                     "' did not get enough after-args")
                else:
                    if n > len(self.afterargsBuffer):
                        self.afterargsBuffer.append(self.tokens[nn])
                    i += 1
                    n += 1
            self.afterremoves = i - 1
            return True
        else:
            self.afterremoves = 0
            return True

    def expandMacro(self, macro, preargs, afterargs):
        """
        Expand single macro.

        Return if text has changed and add expanding text to token.
        """
        macroname = macro.text
        nargb, narga, body, isbuiltin = self.macros[macroname]
        if not isbuiltin:
            res = body[:]
            res = res.replace('#0', macroname)
            if nargb != 0:
                for i, barg in enumerate(preargs):
                    placer = "#-{0:d}".format(i+1)
                    res = res.replace(placer, barg.text)
            if narga != 0:
                for i, aarg in enumerate(afterargs):
                    placer = "#{0:d}".format(i+1)
                    res = res.replace(placer, aarg.text)
            macro.expandText = res[:]
            # True for expanded, false for not. Help for recursive expanding
            return True
        if isbuiltin:
            isexpand = self.buildinMacro(macro, preargs, afterargs)
            return isexpand

    def buildinMacro(self, macro, preargs, afterargs):
        """Call function for builtin macros."""
        name = macro.text
        if name == 'newMacro':
            self.newMacro(macro, preargs, afterargs)
            return True
        if name == 'renewMacro':
            self.renewMacro(macro, preargs, afterargs)
            return True
        if name == 'quotation':
            self.quotation(macro, preargs, afterargs)
            return False
        if name == 'quote':
            self.quote(macro, preargs, afterargs)
            return True
        if name == 'endquote':
            self.endquote(macro, preargs, afterargs)
            return True
        if name == 'ignore':
            self.ignore(macro, preargs, afterargs)
            return True
        if name == 'endignore':
            self.endignore(macro, preargs, afterargs)
            return True
        if name == 'nl':
            self.linebreak(macro, preargs, afterargs)
            return False
        if name == 'newLineCharOn':
            self.newLineCharOn(macro, preargs, afterargs)
            return True
        if name == 'newLineCharOff':
            self.newLineCharOff(macro, preargs, afterargs)
            return True
        if name == 'include':
            self.include(macro, preargs, afterargs)
            return True
        if name == 'input':
            self.input(macro, preargs, afterargs)
            return True
        if name == 'callpy':
            self.callpy(macro, preargs, afterargs)
            return True
        if name == 'pyarg':
            raise ValueError('\\pyarg should call after the \\callpy')
        if name == 'endpyarg':
            raise ValueError('no \\pyarg to end!')
        if name == 'dnl':
            self.dnl(macro, preargs, afterargs)
            return True

# =================== builtin macro functions =======================
    def newMacro(self, macro, preargs, afterargs):
        """Define new macro."""
        name, body = afterargs[0].text, afterargs[1].text
        if name[0] == '\\':
            name = name[1:]
        if name in self.macros.keys():
            raise ValueError("macro '" + name + "' already defined.")
        if (('\\' in name) or
            (' ' in name) or ('{' in name) or
            ('}' in name) or ('\n' in name)):
            raise ValueError("macro name can not contain '\', ' ', '{' and '}'")
        macro.expandText = ''
        npargs, naargs = 0, 0
        # detect number of args
        i = 1
        while True:
            pplacer = "#-{0:d}".format(i)
            i += 1
            if pplacer in body:
                npargs += 1
            else:
                break
        i = 1
        while True:
            aplacer = "#{0:d}".format(i)
            i += 1
            if aplacer in body:
                naargs += 1
            else:
                break
        self.macros[name] = (npargs, naargs, body, False)
        self.updateMaxMacroLength(name)

    def renewMacro(self, macro, preargs, afterargs):
        """Redefine a macro."""
        name = afterargs[0].text
        if name[0] == '\\':
            name = name[1:]
        if name not in self.macros.keys():
            raise ValueError("macro '" + name + "' not defined.")
        if self.macros[name][-1]:
            raise ValueError("can not redefine builtin macros.")
        del self.macros[name]
        self.newMacro(macro, preargs, afterargs)

    def quotation(self, macro, preargs, afterargs):
        """Internal quotetion."""
        body = afterargs[0]
        try:
            bodyhash = int(body.text)
        except ValueError:
            raise ValueError("\\quotation macro can not be used!")
        if bodyhash not in self.quotationTable.keys():
            raise ValueError("\\quotation macro can not be used!")
        macro.expandText = '\\quotation{' + body.text + '}'
        macro.cat = 'quote'
    
    def quote(self, macro, preargs, afterargs):
        """Start quotation."""
        n = self.n
        buffer = ''
        for i in range(n, len(self.text), 1):
            if self.text[i:i+9] == '\\endquote':
                buffer = self.text[n:i]
                break
        else:
            raise ValueError("\\quote not end!")
        bhash = hash(buffer)
        self.quotationTable[bhash] = buffer
        self.n = i+9
        macro.cat = 'quote'
        macro.expandText = '\\quotation{' + str(bhash) + '}'
    
    def endquote(self, macro, preargs, afterargs):
        raise ValueError("no \\quote to end!")

    def ignore(self,macro, preargs, afterargs):
        """Start ignore."""
        n = self.n
        for i in range(n, len(self.text), 1):
            if self.text[i:i+10] == '\\endignore':
                break
        else:
            raise ValueError("\\ignore not end!")
        self.n = i+10
        macro.expandText = ''
    
    def endignore(self, macro, preargs, afterargs):
        raise ValueError("no \\ignore to end!")

    def linebreak(self, macro, preargs, afterargs):
        """Line breaker."""
        macro.cat = 'quote'
        macro.text = '\n'
        macro.expandText = '\\quotation{' + '\n' + '}'

    def newLineCharOn(self, macro, preargs, afterargs):
        """Enable new line charactor."""
        self.ignorLineBreak = False
        macro.expandText = ''

    def newLineCharOff(self, macro, preargs, afterargs):
        """Disable new line charactor."""
        self.ignorLineBreak = True
        macro.expandText = ''
    
    def include(self, macro, preargs, afterargs):
        """Include other file."""
        file = open(afterargs[0].text, 'r')
        fileText = file.read()
        file.close()
        macro.expandText = fileText
    
    def input(self, macro, preargs, afterargs):
        """Input other file."""
        file = open(afterargs[0].text, 'r')
        fileText = file.read()
        file.close()
        subprocessor = Processor(fileText)
        _ = subprocessor.scan()
        res = subprocessor.result[:]
        macro.expandText = res

    def dnl(self, macro, preargs, afterargs):
        if self.text[self.n] == '\n':
            self.n += 1
        macro.expandText = ''

    def callpy(self, macro, preargs, afterargs):
        pyarg = ''
        n = self.n
        if self.text[n:n+6] == '\\pyarg':
            n = n+6
            for i in range(n, len(self.text), 1):
                if self.text[i:i+9] == '\\endpyarg':
                    pyarg = self.text[n:i]
                    self.n = i + 9
                    break
            else:
                raise ValueError("\\pyarg not end!")
        else:
            raise ValueError('need argument')
        funcname = afterargs[0].text
        funcname = funcname.replace(' ', '')
        func = getattr(self.script, funcname)
        expanding = func(pyarg)
        if isinstance(expanding, str):
            macro.expandText = expanding
        elif isinstance(expanding, tuple):
            hashcode, expandText, ifplace = expanding
            if hashcode in self.quotationTable.keys():
                if hashcode not in self.exthashcode:
                    raise ValueError(
                        'hashcode ' + hashcode + 'already used!')
            if hashcode not in self.exthashcode:
                self.exthashcode.append(hashcode)
                
            self.quotationTable[hashcode] = expandText
            if ifplace:
                macro.cat = 'quote'
                macro.expandText = '\\quotation{' + str(hashcode) + '}'
            else:
                macro.expandText = ''
        else:
            raise ValueError(
                '\\pycal can only take function return '+
                'a string or (hashcode(integer), string, Bool)')

# ================= end builtin macro functions =============

    def updateMaxMacroLength(self, macroName):
        """Update when new macro defined."""
        nl = len(macroName)
        if nl > self.maxMacroLength:
            self.maxMacroLength = nl

    def getNextToken(self, ignoreLineBreak=True):
        """Get next token."""
        n = self.n
        c = self.text[n]
        if c == '\\':
            # for macro
            n = n + 1
            buffer = ''
            for i in range(self.maxMacroLength, 0, -1):
                buffer = self.text[n:n+i]
                if buffer in self.macros.keys():
                    self.tokens.append(Token(buffer, 'macroname', self.n, n+i))
                    self.n = n + i
                    break
            else:
                raise ValueError("'" + self.text[n:n+self.maxMacroLength] + "' is undefined")
            return True
        if c == '{':
            # for group
            level = 1
            buffer = ''
            while True:
                n = n + 1
                try:
                    cc = self.text[n]
                except IndexError:
                    raise ValueError("'{' not closed")
                if cc == '}':
                    level -= 1
                if cc == '{':
                    level += 1
                buffer += cc
                if level == 0:
                    buffer = buffer[:-1]
                    self.tokens.append(Token(buffer, 'group', self.n, n+1))
                    self.n = n + 1
                    break
            return True
        if ord(c) == 0:
            # for end of the whole document
            self.tokens.append(Token(c, 'textend', self.n, self.n))
            return False
        if c == ' ':
            # for space
            self.n += 1
            self.tokens.append(Token(c, 'space', self.n, self.n))
            return True
        if c == '\n':
            if ignoreLineBreak:
                self.n += 1
                return True
            else:
                nltoken = Token('quotation', 'quote', self.n, self.n)
                nltoken.expandText = '\\quotation{' + str(self.nlhash) + '}'
                self.tokens.append(nltoken)
                self.n += 1
                return True
        # for any others
        self.tokens.append(Token(c, 'others', self.n, self.n))
        self.n += 1
        return True


if __name__ == '__main__':
    file = open('./test.txt', 'r')
    test = file.read()
    file.close()
    print('++++ input ++++')
    print(test)
    print('++++ end input ++++')
    test += chr(0)
    processer = Processor(test, verbose=1)
    isexpand = processer.scan()
    print('isexpand =', isexpand)
    tokendisp = [str(t) for t in processer.tokens]
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
