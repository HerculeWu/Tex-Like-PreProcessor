from processor import Processor
import argparse
import sys
import os
sys.path.append(os.getcwd())
parser = argparse.ArgumentParser(description='Tex-Like PreProcessor')
parser.add_argument(
    '--input', '-i',
    type=str, 
    help='input file', 
    required=True)
parser.add_argument(
    '--output', '-o',
    type=str, 
    help='output file', 
    required=True)
parser.add_argument(
    '--load', '-l',
    type=str, 
    help='load file for macro definitions')
parser.add_argument(
    '--script', '-s',
    type=str, 
    help='load python script')
parser.add_argument(
    '--verbose', '-v',
    type=int, default=0,
    help='verbose 0(off, default) or 1(on)')
args = parser.parse_args()

ifilename = args.input
ofilename = args.output
lfilename = args.load
script = args.script
verbose = args.verbose
SCRIPT = None
if script is not None:
    if script[-3:] == '.py':
        script = script[:-3]
    SCRIPT = __import__(script)
if lfilename is not None:
    lfile = open(lfilename, 'r')
    configs = lfile.read()
    lfile.close()
    loadProcessor = Processor(configs, verbose=verbose, script=SCRIPT)
    _ = loadProcessor.scan()
    Processor.globalMacros = {k: v for k, v in loadProcessor.macros.items()}
ifile = open(ifilename, 'r')
itext = ifile.read()
ifile.close()
processor = Processor(itext, verbose=verbose, script=SCRIPT)
_ = processor.scan()
ofile = open(ofilename, 'w+')
ofile.write(processor.result)
ofile.close()

