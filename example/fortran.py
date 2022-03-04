
class Variable:
    def __init__(self, name, vartype, pars=None):
        self.name = name
        self.vartype = vartype
        self.pars = pars
    
    def __str__(self):
        out = 'Variable object at '+str(id(self)) + '\n'
        out += 'name = ' + self.name +'\n'
        out += 'type = ' + self.vartype + '\n'
        out += 'parameters:\n'
        out += str(self.pars) + '\n'
        return out

    def declheader(self):
        out = ''
        if self.vartype == 'vector':
            start = self.pars['begin']
            start = int(start)
            size = self.pars['size']
            size = int(size)
            end = int(start + size -1)
            out += self.pars['type'] + ','
            out += 'dimension('
            out += str(start) + ':'
            out += str(end) + ')'
        else:
            out += self.vartype
        out = out.upper()
        return out


class Subrouten:
    def __init__(self, name):
        self.name = name
        self.varin = []
        self.varout = []
        self.varinout = []
        self.varaux = []
        self.headerHash = None

    def getheader(self):
        header = 'SUBROUTINE ' + self.name + '('
        indecl = {}
        outdecl = {}
        inoutdecl = {}
        auxdecl = {}
        hasin, hasout, hasinout, hasaux = False, False, False, False
        if len(self.varin) > 0:
            hasin = True
            for var in self.varin:
                header += var.name + ', '
                declheader = var.declheader()
                if declheader not in indecl:
                    indecl[declheader] = []
                indecl[declheader].append(var.name)
            header = header[:-2]
        if len(self.varinout) > 0:
            hasinout = True
            for var in self.varinout:
                header += var.name + ', '
                declheader = var.declheader()
                if declheader not in inoutdecl:
                    inoutdecl[declheader] = []
                inoutdecl[declheader].append(var.name)
            header = header[:-2]
        if len(self.varout) > 0:
            hasout = True
            for var in self.varout:
                header += var.name + ', '
                declheader = var.declheader()
                if declheader not in outdecl:
                    outdecl[declheader] = []
                outdecl[declheader].append(var.name)
            header = header[:-2]
        header += ')\n'
        header += 'IMPLICIT NONE\n'
        if len(self.varaux) > 0:
            hasaux = True
            for var in self.varaux:
                declheader = var.declheader()
                if declheader not in auxdecl:
                    auxdecl[declheader] = []
                auxdecl[declheader].append(var.name)
        if hasin:
            for declheader in indecl.keys():
                header += declheader
                header += ','
                header += 'INTENT(IN) :: '
                for v in indecl[declheader]:
                    header += v + ', '
                header = header[:-2]
                header += '\n'
        if hasout:
            for declheader in outdecl.keys():
                header += declheader
                header += ','
                header += 'INTENT(OUT) :: '
                for v in outdecl[declheader]:
                    header += v + ', '
                header = header[:-2]
                header += '\n'
        if hasinout:
            for declheader in inoutdecl.keys():
                header += declheader
                header += ','
                header += 'INTENT(INOUT) :: '
                # print(declheader)
                for v in inoutdecl[declheader]:
                    header += v + ', '
                header = header[:-2]
                header += '\n'
        if hasaux:
            for declheader in auxdecl.keys():
                header += declheader
                header += ' :: '
                for v in auxdecl[declheader]:
                    header += v + ', '
                header = header[:-2]
                header += '\n'
        return header


class GlobalState:
    def __init__(self) -> None:
        self.insubroute = False
        self.infunction = False
        self.subroutines = []
        self.functions = []
        self.currentSubroutine = None
        self.currentLoop = None

state = GlobalState()
"""
\input{a, b, c, d \in \real; e \in \vector{real,4}}
\output{a, b, c, d \in \real; e \in \vector{real,4}}
"""
def getArg(arg):
    arg = arg.replace(' ', '')
    arg = arg.replace('\n', '')
    arglist = arg.split(';')
    out = []
    for decl in arglist:
        vv = decl.split('\\in')
        names, vartype = vv[0], vv[1][1:]
        if len(vartype) == 0:
            vartype = 'integer'
        names = names.split(',')
        if vartype[0:6] != 'vector':
            # print(vartype)
            for name in names:
                out.append(Variable(name, vartype))
        else:
            pars = vartype[7:-1].split(',')
            vectype = pars[0]
            vecsize = pars[1]
            parameter = {
                'type': vectype,
                'size': vecsize,
                'begin': '1'
            }
            if len(pars) == 3:
                vecbegin = pars[2]
                parameter['begin'] = vecbegin
            for name in names:
                out.append(Variable(name, 'vector', parameter))
    return out

"""
\subroutine{name}
body

\subroutine{swip}
\inout{a,b\in\real}
\aux{t\in\real}
\subbody
t = a
a = b
b = t
\endsubroutine
"""
def startsubroutine(arg: str):
    if state.insubroute:
        raise ValueError("Already in subrouting define")
    state.insubroute = True
    arg = arg.strip()
    arg = arg.replace(' ', '_')
    arg = arg.replace('\n', '')
    subrouineobj = Subrouten(arg)
    state.subroutines.append(
        subrouineobj
    )
    state.currentSubroutine = state.subroutines[-1]
    header = 'SUBROUTINE '+arg
    hashcode = hash(header)
    state.currentSubroutine.headerhash = hashcode
    return (hashcode, header, True)

def inputvar(arg: str):
    if not state.insubroute:
        raise ValueError("sub routine not defined")
    varlist = getArg(arg)
    state.currentSubroutine.varin = varlist
    return ''

def outputvar(arg):
    if not state.insubroute:
        raise ValueError("sub routine not defined")
    varlist = getArg(arg)
    state.currentSubroutine.varout = varlist
    return ''

def inoutvar(arg):
    if not state.insubroute:
        raise ValueError("sub routine not defined")
    varlist = getArg(arg)
    state.currentSubroutine.varinout = varlist
    return ''

def auxvar(arg):
    if not state.insubroute:
        raise ValueError("sub routine not defined")
    varlist = getArg(arg)
    state.currentSubroutine.varaux = varlist
    return ''

def updateheader(arg):
    if not state.insubroute:
        raise ValueError("sub routine not defined")
    header = state.currentSubroutine.getheader() + '\n'
    hashcode = state.currentSubroutine.headerhash
    return (hashcode, header, False)

def endsubroutine(arg):
    if not state.insubroute:
        raise ValueError("sub routine not defined")
    name = state.currentSubroutine.name
    state.insubroute = False
    state.currentSubroutine = None
    return '\nEND SUBROUTINE ' + name

def sequence(arg: str):
    arg = arg.replace(' ', '')
    arg = arg.replace('\n', '')
    arglist = arg.split(',')
    if len(arglist) < 3:
        raise ValueError(
            'Not enough information to create the sequence')
    first, second, end = arglist[0], arglist[1], arglist[-1]
    step = str(int(second) - int(first))
    return first + ':' + end + ':' + step

def until(arg):
    out = 'DO\n'
    out += 'IF ( ' + arg + ' ) THEN\n'
    out += 'EXIT\n'
    out += 'END IF\n'
    return out

def whiledo(arg):
    out = 'DO '
    out += 'WHILE ( ' + arg + ' )\n'
    return out

def fordo(arg: str):
    arg = arg.replace(' ', '')
    arg = arg.replace('\n', '')
    arglist = arg.split(',')
    itr, start, end = arglist[0], arglist[1], arglist[2]
    step = '1'
    if len(arglist) == 4:
        step = arglist[3]
    out = 'DO ' + itr + ' = ' + start + ',' + end + ',' + step
    return out



if __name__ == '__main__':
    arg = 'a, b, c, d \\in \\real; e,f,g \\in \\vector{real,4}'
    out = getArg(arg)
    print(len(out))
    for var in out:
        print(var)